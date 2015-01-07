#ifndef __NIFFED_H__
#define __NIFFED_H__

#include <stdlib.h>
#include <stdint.h>

#include "erl_driver.h"
#include "erl_nif.h"

extern intptr_t* niffed_offset(intptr_t* src, size_t n, int dstart,intptr_t* dst, size_t m);
extern void niffed_print(ErlNifEnv* env, ERL_NIF_TERM term);
extern ErlDrvTermData niffed_atom(char* name, int len);

#endif
