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
#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include "jch.h"

int main(int argc, char** argv)
{
    if (argc != 3 || atol(argv[1]) < 1L || atoi(argv[2]) < 1)
    {
        fprintf(stderr, "usage:\n\t%s <iterations> <num_buckets>\n\n", argv[0]);
        fprintf(stderr, "example:\n\t%s 10000000 1024\n\n", argv[0]);
        return 1;
    }

    unsigned long n = atol(argv[1]);
    uint32_t b = atoi(argv[2]);

    struct timeval s, e;
    gettimeofday(&s,NULL);
    for (uint64_t i = 0; i < n; i++)
    {
        _jch_chash(i, b);
    }
    gettimeofday(&e,NULL);

    double es = (e.tv_sec - s.tv_sec) + ((double)(e.tv_usec - s.tv_usec)/1e6); 
    double en = es * 1e9;
    double hps = (es >= 1.) ? (double)n / es : (double)n * es;
    double nsh = en / n;
    printf("ch/perf: n: %lu b: %u elapsed: %f (%f hps, %f nsh).\n", n, b, es, hps, nsh);

    return 0;
}
