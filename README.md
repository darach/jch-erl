**jch-erl** [![Build Status](https://travis-ci.org/darach/jch-erl.svg?branch=master)](https://travis-ci.org/darach/jch-erl)

Jump Consistent Hashing Library

NIF wrapper for Jump Consistent Hash algorithm by John Lamping and Eric Veach
developed at Google, Inc. Paper: ["A Fast, Minimal Memory, Consistent Hash Algorithm](http://arxiv.org/ftp/arxiv/papers/1406/1406.2294.pdf).

This library have 2 implementations: the xorshift64\* pseudo-random number generator and
the linear congruential random generator as in the paper. First one is reasonably fast but, more importantly,
memory efficient. Second one is compatible with implementations in other languages.

Performance results (via Travis CI) for

* Erlang/OTP 18.2 - TODO
* Erlang/OTP 17.1 - https://travis-ci.org/darach/jch-erl/jobs/50712346
* Erlang/OTP 17.0 - https://travis-ci.org/darach/jch-erl/jobs/50712347
* Erlang/OTP R16BB03 - https://travis-ci.org/darach/jch-erl/jobs/50712348
* Erlang/OTP R16B02 - https://travis-ci.org/darach/jch-erl/jobs/50712349
* Erlang/OTP R16B01 - https://travis-ci.org/darach/jch-erl/jobs/50712350
* Note. Some tests were disabled as they were too strenuous for travis, resulting in build failure! (Ha ha!)

### Usage

Two functions `jch:ch/2` and `jch:ch/3` are offered. Simply pass in a 64 bit long (or less) integer
key argument followed by the desired bucket or continuum partition size. The function returns the
partition allocated for the key. Optional 3'rd argument is atom `orig` to use compatible algorithm
or atom `xorshift64` to use more eficient algorithm.

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
4> jch:ch(13, 128, orig).
56
5> jch:ch(trunc(math:pow(2,64))-1,128).
78
6> jch:ch(trunc(math:pow(2,64)),128). %% off by 1 mate
** exception error: bad argument
     in function  jch:ch/2
        called as jch:ch(18446744073709551616,128)
7> %% off by 1 mate
7>
```

Enjoy!

