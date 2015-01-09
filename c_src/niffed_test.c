//
// Nif term driver
//

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>

#include "erl_driver.h"
#include "erl_nif.h"

#include "niffed.h"

// Hack to handle R15 driver used with pre R15 driver
#if ERL_DRV_EXTENDED_MAJOR_VERSION == 1
typedef int  ErlDrvSizeT;
typedef int  ErlDrvSSizeT;
#endif

typedef struct {
    ErlDrvPort   port;
    ErlNifEnv*   env;
    ErlNifEntry* nif;
} drv_data_t;

ErlDrvEntry niffed_test_entry;

#define PORT_CONTROL_BINARY

#define CTL_LOOKUP_NIF   0
#define CTL_LOOKUP_ATOM  1
#define CTL_CALL     2
    

#define DBG(...) printf(__VA_ARGS__)
// #define DBG(...)

static int        niffed_test_init(void);
static void       niffed_test_finish(void);
static void       niffed_test_stop(ErlDrvData);
static void       niffed_test_output(ErlDrvData, char*, ErlDrvSizeT);
static void       niffed_test_outputv(ErlDrvData, ErlIOVec*);
static void       niffed_test_ready_input(ErlDrvData, ErlDrvEvent);
static void       niffed_test_ready_output(ErlDrvData data, ErlDrvEvent event);
static ErlDrvData niffed_test_start(ErlDrvPort, char* command);
static ErlDrvSSizeT niffed_test_ctl(ErlDrvData,unsigned int,char*,ErlDrvSizeT,char**,ErlDrvSizeT);
static void       niffed_test_timeout(ErlDrvData);

/* general control reply function */
static ErlDrvSSizeT ctl_reply(int rep, void* buf, ErlDrvSizeT len,
			      char** rbuf, ErlDrvSizeT rsize)
{
    char* ptr;

    if ((len+1) > rsize) {
#ifdef PORT_CONTROL_BINARY
	ErlDrvBinary* bin = driver_alloc_binary(len+1);
	if (bin == NULL) 
	    return -1;
	ptr = bin->orig_bytes;	
	*rbuf = (char*) bin;
#else
	if ((ptr = driver_alloc(len+1)) == NULL)
	    return -1;
	*rbuf = ptr;
#endif
    }
    else
	ptr = *rbuf;
    *ptr++ = rep;
    memcpy(ptr, buf, len);
    return len+1;
}


static int niffed_test_init(void)
{
    // FIXME: build hash for nif_term_funcs 
    // atom(val)+arity  => index
    return 0;
}

static void       niffed_test_finish(void)
{
    // fprintf(stderr, "niffed_test_finish called!!!\r\n");
}

static void       niffed_test_stop(ErlDrvData d)
{
    drv_data_t* dptr = (drv_data_t*) d;

    // fprintf(stderr, "niffed_test_stop called!!!\r\n");
    if (dptr) {
	driver_free(dptr);
    }
}

static void       niffed_test_output(ErlDrvData d, char* buf, ErlDrvSizeT len)
{
    (void) d;
    (void) buf;
    (void) len;
    // fprintf(stderr, "niffed_test_output called!!!\r\n");
}

static void       niffed_test_outputv(ErlDrvData d, ErlIOVec* iov)
{
    (void) d;
    (void) iov;
    // fprintf(stderr, "niffed_test_outputv called!!!\r\n");
}

// netlink socket triggered process data
static void niffed_test_ready_input(ErlDrvData d, ErlDrvEvent event)
{
    (void) d;
    (void) event;
}

static void niffed_test_ready_output(ErlDrvData d, ErlDrvEvent event)
{
    (void) d;
    (void) event;
}

static ErlDrvSSizeT niffed_test_ctl(ErlDrvData d,unsigned int cmd,char* buf,
				    ErlDrvSizeT len,char** rbuf,
				    ErlDrvSizeT rlen)
{
    drv_data_t* dptr = (drv_data_t*) d;
    intptr_t fixbuf[1024];  // rewrite buffer
    intptr_t* ptr;
    ERL_NIF_TERM term;

    DBG("command = %d len=%lu\r\n", cmd, len);

    switch(cmd) {
    case CTL_LOOKUP_ATOM: {  // translate atom string
	ErlDrvTermData value;
	if (niffed_lookup_atom(buf, len, &value))
	    return ctl_reply(1, &value, sizeof(value), rbuf, rlen);
	return ctl_reply(0, "", 0, rbuf, rlen);	
    }

    case CTL_LOOKUP_NIF: {  // lookup function/arity return index
	ErlDrvTermData value;
	if (niffed_lookup_nif(buf+1,buf[0],buf[buf[0]+1],dptr->nif,&value))
	    return ctl_reply(1, &value, sizeof(value), rbuf, rlen);
	return ctl_reply(0, "", 0, rbuf, rlen);
    }	

    case CTL_CALL: {  // call
	ERL_NIF_TERM argv[256];
	int index;
	int argc;
	int i;
	// <index> <argc> <arg1>,...<argn> data
	// index and args are encoded as small integers
	index = (((ERL_NIF_TERM*)buf)[0]) >> 4;
	argc  = (((ERL_NIF_TERM*)buf)[1]) >> 4;

	DBG("index: %d\r\n", index);
	DBG("argc:  %d\r\n", argc);

	if ((index < 0) || (index >= dptr->nif->num_of_funcs) ||
	    (argc < 0) || (argc > 255)) {
	    return ctl_reply(0, "", 0, rbuf, rlen);
	}
	ptr = niffed_offset((intptr_t*) buf, len / sizeof(intptr_t), argc+2,
			    fixbuf, sizeof(fixbuf) / sizeof(intptr_t));
	DBG("pointers done: %p\r\n", ptr);
	// Now store arguments in a nif environment!
	enif_clear_env(dptr->env);
	for (i = 0; i < argc; i++)
	    argv[i] = enif_make_copy(dptr->env, ((ERL_NIF_TERM*)ptr)[2+i]);
	// call nif 
	term = (*dptr->nif->funcs[index].fptr)(dptr->env, argc, argv);
	printf("term: ");
	niffed_print(dptr->env, term);
	printf("\r\n");
	if (ptr != fixbuf)
	    driver_free(ptr);
	if (term == 0)
	    return ctl_reply(0, "", 0, rbuf, rlen);
	else
	    return niffed_copy(dptr->env, term, rbuf, rlen);
    }

    default:
	return ctl_reply(0, "", 0, rbuf, rlen);
    }
}

static void niffed_test_timeout(ErlDrvData d)
{
    (void) d;
}

// access to the nif C code - linked with this code (a driver)
// extern ErlNifEntry* niffed_test_nif_init(TWinDynNifCallbacks* callbacks);
extern ErlNifEntry* nif_init(void);

static ErlDrvData niffed_test_start(ErlDrvPort port, char* command)
{
    (void) command;
    drv_data_t* dptr;

    // Setup
    if (!(dptr = driver_alloc(sizeof(drv_data_t))))
	return ERL_DRV_ERROR_ERRNO;

    memset(dptr, 0, sizeof(drv_data_t));
    dptr->port = port;
    dptr->env  = enif_alloc_env();
    dptr->nif  = nif_init();

    printf("nif_init returned %p\r\n", dptr->nif);

#ifdef PORT_CONTROL_BINARY
    set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);
#endif

    return (ErlDrvData) dptr;
}



DRIVER_INIT(nl_drv)
{
    ErlDrvEntry* ptr = &niffed_test_entry;

    ptr->driver_name = "niffed_test";
    ptr->init  = niffed_test_init;
    ptr->start = niffed_test_start;
    ptr->stop  = niffed_test_stop;
    ptr->output = niffed_test_output;
    ptr->ready_input  = niffed_test_ready_input;
    ptr->ready_output = niffed_test_ready_output;
    ptr->finish = niffed_test_finish;
    ptr->control = niffed_test_ctl;
    ptr->timeout = niffed_test_timeout;
    ptr->outputv = niffed_test_outputv;
    ptr->ready_async = 0;
    ptr->flush = 0;
    ptr->call = 0;
    ptr->event = 0;
    ptr->extended_marker = ERL_DRV_EXTENDED_MARKER;
    ptr->major_version = ERL_DRV_EXTENDED_MAJOR_VERSION;
    ptr->minor_version = ERL_DRV_EXTENDED_MINOR_VERSION;
    ptr->driver_flags = ERL_DRV_FLAG_USE_PORT_LOCKING;
    ptr->process_exit = 0;
    ptr->stop_select = 0;  // add me

    return (ErlDrvEntry*) ptr;
}
