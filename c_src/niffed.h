#ifndef __NIFFED_H__
#define __NIFFED_H__

#include <stdlib.h>

#include "erl_driver.h"
#include "erl_nif.h"

extern intptr_t* niffed_offset(intptr_t* src, size_t n, int dstart,intptr_t* dst, size_t m);
extern void niffed_print(ErlNifEnv* env, ERL_NIF_TERM term);
extern int niffed_lookup_atom(char* name, int len, ErlDrvTermData* value);
extern int niffed_lookup_nif(char* name, int len, int arity, 
			     ErlNifEntry* nif, ErlDrvTermData* value);
extern int niffed_copy(ErlNifEnv* env, ERL_NIF_TERM obj, char** rbuf, ErlDrvSizeT rsize);

#endif
