// Copyright (c) 2014 Darach Ennis < darach at gmail dot com >.
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to permit
// persons to whom the Software is furnished to do so, subject to the
// following conditions:  
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
// OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
// NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
// DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
// OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
// USE OR OTHER DEALINGS IN THE SOFTWARE.

#include <stdint.h>
#include "erl_nif.h"
#include "jch.h"

static ERL_NIF_TERM jch_chash(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    uint64_t key;
    uint32_t num_buckets;
    unsigned int switch_len;
    char algorithm[11];
    int32_t chash;

    if (argc != 3 ||
        !enif_get_uint64(env, argv[0], &key) ||
        !enif_get_uint(env, argv[1], &num_buckets) ||
        !enif_get_atom_length(env, argv[2], &switch_len, ERL_NIF_LATIN1) ||
        switch_len > 10 ||      // orig | xorshift64
        !enif_get_atom(env, argv[2], algorithm, switch_len + 1, ERL_NIF_LATIN1) ||
        num_buckets < 1)
    {
        return enif_make_badarg(env);
    }

    switch (*algorithm) {
    case 'o':                   // orig
        chash = _jch_chash_orig(key,num_buckets);
        break;
    case 'x':                   // xorshift64
        chash = _jch_chash(key,num_buckets);
        break;
    default:
        return enif_make_badarg(env);
    }

    return enif_make_int(env,chash);
}

static void init(ErlNifEnv* env)
{
}

static int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    init(env);
    return 0;
}

static int on_upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info)
{
    init(env);
    return 0;
}

static void on_unload(ErlNifEnv* env, void* priv_data)
{
    // nothing to clean up
}

static ErlNifFunc nif_funcs[] = {
    {"ch", 3, jch_chash}
};

ERL_NIF_INIT(jch, nif_funcs, &on_load, NULL, &on_upgrade, &on_unload)
