//
// API for driver to convert control calls into 
// nif calls
//

#include <stdio.h>
#include <memory.h>
#include "niffed.h"

#define DBG(...) printf(__VA_ARGS__)
// #define DBG(...)

#define TAG_PRIMARY_HEADER      0x0
#define TAG_PRIMARY_LIST        0x1
#define TAG_PRIMARY_BOXED       0x2
#define TAG_PRIMARY_IMMED       0x3

#define PORT_CONTROL_BINARY

// scan "NIF" term data and offset all pointers

intptr_t* niffed_offset(intptr_t* src, size_t n, int dstart,intptr_t* dst, size_t m)
{
    intptr_t* src_end = src + n;
    intptr_t* dst0 = dst + dstart;  // data area
    intptr_t* res  = dst;
    intptr_t  val;
    int arity;
    int i = 0;

    if (n > m) {
	dst = driver_alloc(sizeof(intptr_t)*n);
	dst0 = dst + dstart; // first element is term or pointer
	res  = dst;
	DBG("allocated dst0 = %p\r\n", dst0);
    }
    else 
	DBG("fixed dst0 = %p\r\n", dst0);
    while(src < src_end) {
	val = *src++;
	DBG("src[%d] = %lx/%ld ", i, val&~3, val&3);
	switch(val & 0x3) {
	case TAG_PRIMARY_HEADER:
	    arity = val >> 6;  // arity
	    DBG("%p, dst[%d] = arity=%d, %lx/%ld\r\n", dst, i++, arity,
		val>>2,val&3);
	    if ((val & 0x3f) != 0) { // not tuple
		*dst++ = val;
		while(arity > 0) {
		    arity--;
		    val = *src++;
		    DBG("src[%d] = %lx/%ld ", i, (*src)&~3,(*src)&3);
		    DBG("%p, dst[%d] = %lx/%ld\r\n", dst, i++, val&~3,val&3);
		    *dst++ = val;
		}
	    }
	    else {  // tuple do element by element
		*dst++ = val;
	    }
	    break;
	case TAG_PRIMARY_LIST:
	case TAG_PRIMARY_BOXED:
	    val = (intptr_t)(dst0+(val>>2)) | (val&3);
	    DBG("%p, dst[%d] = %lx/%ld\r\n", dst, i++, val&~3,val&3);
	    *dst++ = val;
	    break;
	case TAG_PRIMARY_IMMED:
	    DBG("%p, dst[%d] = %lx/%ld\r\n", dst, i++, val>>2,val&3);
	    *dst++ = val;
	    break;
	}
    }
    return res;
}

static int enif_is_list_cell(ErlNifEnv* env, ERL_NIF_TERM term)
{
    return enif_is_list(env, term) && !enif_is_empty_list(env, term);
}

void niffed_print(ErlNifEnv* env, ERL_NIF_TERM term)
{
    if (term == 0)
	printf("BADARG");
    else if (enif_is_number(env, term)) {
	double d;
	long l;
	if (enif_get_double(env, term, &d)) {
	    printf("%f", d);
	}
	else if (enif_get_long(env, term, &l)) {
	    printf("%ld", l);
	}
	else {
	    printf("BIGNUM - not supported by nif");
	}
    }
    else if (enif_is_atom(env, term)) {
	char string[256];
	enif_get_atom(env, term, string, sizeof(string), ERL_NIF_LATIN1);
	printf("%s", string);  // fixme quote when needed
    }
    else if (enif_is_pid(env, term)) {
	ErlNifPid pterm;
	if (enif_get_local_pid(env, term, &pterm)) {
	    printf("<0.%lu.0>", pterm.pid>>4);
	}
	else {
	    printf("<x.y.z>");
	}
    }
    else if (enif_is_port(env, term)) {
	printf("#Port<x.yt>");
    }
    else if (enif_is_empty_list(env, term)) {
	printf("[]");
    }
    else if (enif_is_binary(env, term)) {
	ErlNifBinary bin;
	int i;
	enif_inspect_binary(env, term, &bin);
	if (bin.size > 0) {
	    printf("<<%d", bin.data[0]);
	    for (i = 1; i<bin.size; i++)
		printf(",%d", bin.data[i]);
	    printf(">");
	}
	else
	    printf("<<>>");
    }
    else if (enif_is_list(env, term)) {
	ERL_NIF_TERM head,tail;
	printf("[");
	do {
	    enif_get_list_cell(env, term, &head, &tail);
	    niffed_print(env, head);
	    if (enif_is_list_cell(env, tail))
		printf(",");
	    else if (!enif_is_empty_list(env, tail)) {
		printf("|");
		niffed_print(env, tail);
	    }
	    term = tail;
	} while(enif_is_list_cell(env, term));
	printf("]");
    }
    else if (enif_is_tuple(env, term)) {
	const ERL_NIF_TERM* array;
	int i, arity;
	enif_get_tuple(env, term, &arity, &array);
	printf("{");
	for (i = 0; i < arity; i++) {
	    niffed_print(env, array[i]);
	    if (i < arity-1)
		printf(",");
	}
	printf("}");
    }
    else if (enif_is_binary(env, term)) {  // note! not bitstring
	int i;
	ErlNifBinary bin;
	enif_inspect_binary(env, term, &bin);
	printf("<<");
	for (i = 0; i < bin.size; i++) {
	    printf("%d", bin.data[i]);
	    if (i < bin.size-1)
		printf(",");
	}
	printf(">>");
    }
    else {
	printf("UNKNOWN: BITSTRING?");
    }
}

int niffed_lookup_atom(char* name, int len, ErlDrvTermData* value)
{
    char string[256];
    if (len < 256) {
	memcpy(string, name, len);
	string[len] = '\0';
	*value = driver_mk_atom(string);
	return 1;
    }
    return 0;
}

int niffed_lookup_nif(char* name, int len, int arity, 
		      ErlNifEntry* nif, ErlDrvTermData* value)
{
    int i;
    for (i = 0; i < nif->num_of_funcs; i++) {
	int n;
	if ((nif->funcs[i].arity == arity) &&
	    ((n = strlen(nif->funcs[i].name)) == len) &&
	    (memcmp(name,nif->funcs[i].name,n) == 0)) {
	    *value = i;
	    return 1;
	}
    }
    return 0;
}


static ErlDrvSSizeT copy_reply(int rep, void* buf, ErlDrvSizeT len,
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
//
// Copy obj to control output
//
int niffed_copy(ErlNifEnv* env, ERL_NIF_TERM obj, char** rbuf, ErlDrvSizeT rsize)
{
    // test max 24 bit integer as special mark !
    // create spcial copy object = {[<mark>],obj} 
    ERL_NIF_TERM mark = enif_make_int(env, (int)((intptr_t) rbuf & 0xffffff));
    ERL_NIF_TERM mark_list = enif_make_list1(env, mark);
    ERL_NIF_TERM obj1 = enif_make_tuple2(env, mark_list, obj);
    ERL_NIF_TERM copy;
    ERL_NIF_TERM* ptr;
    ERL_NIF_TERM* rptr;


    copy = enif_make_copy(env, obj1);

    // copy should look something like
    //     +----------------+
    // ->  | 2   | ARITYVAL |
    //     +----------------+
    //     |     |  LIST    |  -+
    //     +----------------+   |
    //     |     OBJ        |   |
    //     +----------------+   |
    //     | object data    |   |
    //     |      ...       |   |
    //     +----------------+   |
    //     | <mark> |  INT  | <-+
    //     +----------------+
    //     |        |  NIL  |
    //     +----------------+

    // determine the size of the object
    ptr = (ERL_NIF_TERM*) (copy & ~3);
    rptr = ptr;
// optimise a bit later on
//    if (*ptr == obj) // object is constant
//	return copy_reply(1, &obj, sizeof(obj), rbuf, rsize);
    while(*ptr != mark)
	ptr++;
    // do not return last cell
    printf("niffed_copy: %lu\r\n", ptr-rptr);
    return copy_reply(1,rptr,sizeof(ERL_NIF_TERM)*(ptr-rptr),rbuf,rsize);
}
