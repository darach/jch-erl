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
inline double lcg_next(uint64_t* x)
{
    *x ^= *x >> 12; // a
    *x ^= *x << 25; // b
    *x ^= *x >> 27; // c
    return (double)(*x * 2685821657736338717LL) / ULONG_MAX;
}

// jump consistent hash
int _jch_chash(uint64_t key, unsigned int num_buckets)
{
    uint64_t seed = key; int b = -1; int j = 0;

    do {
        b = j;
        double r = lcg_next(&seed);
        j = floor( (b + 1)/r );
    } while(j < num_buckets);

    return b;
}
