**jch-erl**  [![Build Status](https://travis-ci.org/darach/jch-erl.png)](https://travis-ci.org/darach/jch-erl)

Jump Consistent Hashing Library

NIF wrapper for Jump Consistent Hash algorithm by John Lamping and Eric Veach
developed at Google, Inc. Paper: ["A Fast, Minimal Memory, Consistent Hash Algorithm](http://arxiv.org/ftp/arxiv/papers/1406/1406.2294.pdf).

This implementation uses the xorshift64\* pseudo-random number generator rather than
the linear congruential generator in the paper as it is reasonably fast but, more importantly,
memory efficient.

### Usage

A single function ```jch:ch/2``` is offered. Simply pass in a 64 bit long (or less) integer
key argument followed by the desired bucket or continuum partition size. The function returns the
partition allocated for the key.

Performance is very stable as bucket size increases and distribrution across the
ring is stable (standard deviation for a reasonable sample size is typically <5%
relative to the mean.

### Example
```bash
% ERL_LIBS=deps erl +sfwi 1 +scl false -pa ebin
Erlang R16B02 (erts-5.10.3) [source] [64-bit] [smp:8:8] [async-threads:10] [hipe] [kernel-poll:false] [dtrace]

Eshell V5.10.3  (abort with ^G)
1> jch:ch(1,128).
29
2> jch:ch(13,128).
121
3> jch:ch(13,128).
121
4> jch:ch(trunc(math:pow(2,64))-1,128).
78
5> jch:ch(trunc(math:pow(2,64)),128). %% off by 1 mate
** exception error: bad argument
     in function  jch:ch/2
        called as jch:ch(18446744073709551616,128)
6> %% off by 1 mate
6>
```

Enjoy!

