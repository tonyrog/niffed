//
// A standalone module access from a driver
//
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "erl_nif.h"

static ERL_NIF_TERM hello(ErlNifEnv* env,int argc,const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM goodbye(ErlNifEnv* env,int argc,const ERL_NIF_TERM argv[]);

ErlNifFunc niffed_test_funcs[] = {
    { "hello",   1, hello   },
    { "goodbye", 1, goodbye },
    { "hello",   2, hello   }
};

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

ERL_NIF_INIT(niffed_test, niffed_test_funcs, 
             0, 0, 0, 0)
