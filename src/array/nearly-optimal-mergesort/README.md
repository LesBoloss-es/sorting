Nearly-Optimal Mergesort
========================

This directory contains an OCaml reimplementation of the algorithms described in
Nearly-Optimal Mergesort (Munro & Wild 2018). The goal is not to be OCaml-like
but to be the closest possible to their implementations.

Naming Conventions
------------------

The naming conventions for this paper are the same as that of Timsort: a "run"
is either a maximal weakly increasing or maximal strictly decreasing region. The
fact that we consider only strictly decreasing runs allows to reverse them on
detection without loosing the stability of the algorithm.

Algorithms from the Paper
-------------------------

We copy here the descriptions of the algorithms as found in their paper. For a
practical implementation, it is better to look at the GitHub of the authors:
https://github.com/sebawild/nearly-optimal-mergesort-code/blob/master/src/wildinter/net/mergesort/

### PeekSort

```
PeekSort(A[l..r], e, s)

 1  if e == r or s == l then return
 2  m := l + floor((r-l) / 2)
 3  if m <= e
 4    PeekSort(A[e+1..r], e+1, s)
 5    Merge(A[l..e], A[e+1..r])
 6  else if m >= s
 7    PeekSort(A[l..s-1], e, s-1)
 8    Merge(A[l..s-1], A[s..r])
 9  else
10    i := ExtendRunLeft(A[m], l);  j := ExtendRunRight(A[m], r);
11    if i == l and j == r return
12    if m - i < j - m
13      PeekSort(A[l..i-1], e, i-1)
14      PeekSort(A[i..r], j, s)
15      Merge(A[l..i-1], A[i..r])
16    else
17      PeekSort(A[l..j], e, i)
18      PeekSort(A[j+1..r], j+1, s)
19      Merge(A[l..j], A[j+1..r]) *)
```
