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
#include <limits.h>
#include "math.h"

// a reasonably fast, good period, low memory use, xorshift64* based prng
double lcg_next(uint64_t* x)
{
    *x ^= *x >> 12; // a
    *x ^= *x << 25; // b
    *x ^= *x >> 27; // c
    return (double)(*x * 2685821657736338717ULL) / ULLONG_MAX;
}

// jump consistent hash
int32_t _jch_chash(uint64_t key, uint32_t num_buckets)
{
    uint64_t seed = key; int32_t b = -1; int32_t j = 0;

    do {
        b = j;
        double r = lcg_next(&seed);
        j = floor( (b + 1)/r );
    } while(j < num_buckets);

    return b;
}


// Implementation from original article, compatible with
// most implementations in other languages.
int32_t _jch_chash_orig(uint64_t key, uint32_t num_buckets) {
  int64_t b = -1, j = 0;

  while (j < num_buckets) {
    b = j;
    key = key * 2862933555777941757ULL + 1;
    j = (b + 1) * ((double)(1LL << 31) / (double)((key >> 33) + 1));
  }

  return (int32_t)b;
}
