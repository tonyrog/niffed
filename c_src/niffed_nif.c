//
// A standalone module access from a driver
//
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "erl_nif.h"

static ERL_NIF_TERM info(ErlNifEnv* env,int argc,const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM hello(ErlNifEnv* env,int argc,const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM goodbye(ErlNifEnv* env,int argc,const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM echo(ErlNifEnv* env,int argc,const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM reverse(ErlNifEnv* env,int argc,const ERL_NIF_TERM argv[]);

ErlNifFunc niffed_test_funcs[] = {
    { "info",    0, info    },
    { "hello",   1, hello   },
    { "goodbye", 1, goodbye },
    { "hello",   2, hello   },
    { "echo",    1, echo    },
    { "reverse", 1, reverse }
};

static ERL_NIF_TERM info(ErlNifEnv* env,int argc,const ERL_NIF_TERM argv[])
{
    printf("info: niffed_test_funcs=%p\r\n", niffed_test_funcs);
    printf("info: info=%p\r\n", niffed_test_funcs[0].fptr);
    printf("info: env=%p\r\n", env);
    return enif_make_int(env, 0);
}


static ERL_NIF_TERM hello(ErlNifEnv* env,int argc,const ERL_NIF_TERM argv[])
{
    if (argc == 1) {
	long l;
	if (enif_get_long(env, argv[0], &l))
	    return enif_make_long(env, l + 1);
    }
    else if (argc == 2) {
	long l1, l2;
	if (enif_get_long(env, argv[0], &l1) &&
	    enif_get_long(env, argv[1], &l2))
	    return enif_make_long(env, l1 + l2 + 1);
    }
    return enif_make_badarg(env);
}


static ERL_NIF_TERM goodbye(ErlNifEnv* env,int argc,const ERL_NIF_TERM argv[])
{
    if (enif_is_atom(env, argv[0])) {
	char string[256];
	int i, j, len;
	enif_get_atom(env, argv[0], string, sizeof(string), ERL_NIF_LATIN1);
	len = strlen(string);
	i = 0;
	j = len-1;
	while(i < j) {
	    char t = string[i];
	    string[i] = string[j];
	    string[j] = t;
	    i++;
	    j--;
	}
	return enif_make_atom_len(env, string, len);
    }
    return enif_make_badarg(env);
}

static ERL_NIF_TERM echo(ErlNifEnv* env,int argc,const ERL_NIF_TERM argv[])
{
    return argv[0];
}

static ERL_NIF_TERM make_nil(ErlNifEnv* env)
{
    return enif_make_list(env, 0);
}

static ERL_NIF_TERM reverse(ErlNifEnv* env,int argc,const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM list = argv[0];
    ERL_NIF_TERM res = make_nil(env);
    ERL_NIF_TERM hd, tl;

    while(enif_get_list_cell(env, list, &hd, &tl)) {
	res = enif_make_list_cell(env, hd, res);
	list = tl;
    }
    if (enif_is_empty_list(env, list))
	return res;
    return enif_make_badarg(env);
}


ERL_NIF_INIT(niffed_test, niffed_test_funcs, 
             0, 0, 0, 0)
