---
jupyter:
  jupytext:
    text_representation:
      extension: .md
      format_name: markdown
      format_version: '1.1'
      jupytext_version: 1.16.6
  kernelspec:
    display_name: Logtalk
    language: logtalk
    name: logtalk_kernel
---

<!--
________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
SPDX-FileCopyrightText: 1998-2025 Paulo Moura <pmoura@logtalk.org>  
SPDX-License-Identifier: Apache-2.0

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
________________________________________________________________________
-->

# coinduction

This example requires a recent version of CxProlog, ECLiPSE, SICStus Prolog,
SWI-Prolog, or YAP as the backend Prolog compiler.

Note that Logtalk support for coinduction is still experimental. The two major
issues are lack of robust Prolog support for cyclic terms and lack of support
for tabling of cyclic terms. The first issue prevents using of some backend
Prolog compilers. The second issue may prevent some coinductive predicates to
behave as (theoretically) expected.

This folder contains several examples of coinductive predicates, adapted from
the coinduction papers and from the Feliks Kluzniak's DRA meta-interpreter.

The unit tests are currently disabled when using CxProlog or ECLiPSE as the
backend compiler. Although some tests pass, all tests calling `bagof/3` don't
terminate until all available memory is exhausted due to the fragile support
for rational terms in these two Prolog compilers.

For more information see:

```text
@inproceedings{1778186,
	author = {Gupta, Gopal and Bansal, Ajay and Min, Richard and Simon, Luke and Mallya, Ajay},
	title = {Coinductive logic programming and its applications},
	booktitle = {Proceedings of the 23rd International Conference on Logic programming (ICLP)},
	year = {2007},
	isbn = {3-540-74608-0, 978-3-540-74608-9},
	pages = {27--44},
	location = {Porto, Portugal},
	publisher = {Springer-Verlag},
	address = {Berlin, Heidelberg},
}

@inproceedings{
	author = {Neda Saeedloei and Gopal Gupta},
	title = {Verifying Complex Continuous Real-Time Systems with Coinductive CLP(R)},
	booktitle = {Proceedings of the 19th Workshop on Logic-based methods in Programming Environments (WLPE)},
	year = {2009},
	location = {Pasadena, California, USA}
}

@inproceedings{saeedloei_et_al:LIPIcs:2010:2599,
	author ={Neda Saeedloei and Gopal Gupta},
	title = {Timed Definite Clause Omega-Grammars},
	booktitle ={Technical Communications of the 26th International Conference on Logic Programming},
	pages = {212--221},
	series = {Leibniz International Proceedings in Informatics (LIPIcs)},
	isbn = {978-3-939897-17-0},
	issn = {1868-8969},
	year = {2010},
	volume = {7},
	editor = {Manuel Hermenegildo and Torsten Schaub},
	publisher = {Schloss Dagstuhl--Leibniz-Zentrum fuer Informatik},
	address = {Dagstuhl, Germany},
	URL = {http://drops.dagstuhl.de/opus/volltexte/2010/2599},
	doi = {http://dx.doi.org/10.4230/LIPIcs.ICLP.2010.212},
	annote = {Keywords: Constraint Logic Programming over reals, Co-induction, Context-Free Grammars, Omega-Grammars}
}

@inproceedings{AnconaSAC12,
	author = {Ancona, D.},
	title = {Regular corecursion in {P}rolog},
	booktitle = {A{CM} {S}ymposium on {A}pplied {C}omputing ({SAC} 2012)},
	ftp = {ftp://ftp.disi.unige.it/person/AnconaD/AnconaSAC12.pdf},
	keywords = {coinduction,corecursion},
	year = 2012
}

@inproceedings{pmoura13b,
	author = {Paulo Moura},
	title = "{A Portable and Efficient Implementation of Coinductive Logic Programming}",
	booktitle = {Proceedings of the Fifteenth International Symposium on Practical Aspects of Declarative Languages},
	editor = "Kostis Sagonas",
	series = "Lecture Notes in Computer Science",
	pages = {77-92},
	volume = "7752",
	year = {2013},
	publisher = "Springer-Verlag",
	address = "Berlin Heidelberg",
	doi = {10.1007/978-3-642-45284-0_6},
	url = {http://dx.doi.org/10.1007/978-3-642-45284-0_6}
}
```

Start by loading the example:

```logtalk
logtalk_load(coinduction(loader)).
```

An elementary coinductive predicate:

```logtalk
simple::p.
```

<!--
true ;
false.
-->

Similar:

```logtalk
simple::p(hoho).
```

<!--
true ;
false.
-->

```logtalk
simple::p(hoho, X).
```

<!--
X = hoho ;
false.
-->

The following goal is true for any cyclic list containing only ones:

```logtalk
L = [1| L], binary::p(L).
```

<!--
L = [1|L] ;
false.
-->

Same for any cyclic list containing only zeros:

```logtalk
L = [0| L], binary::p(L).
```

<!--
L = [0|L] ;
false.
-->

Or a repetition of a pattern of ones and zeros:

```logtalk
L = [1,0,1| L], binary::p(L).
```

<!--
L = [1, 0, 1|L] ;
false.
-->

But not all solutions can be returned:

```logtalk
%%table
binary::p(X).
```

<!--
X = [0|X] ;
X = [1|X] ;
false.
-->

Infinite streams example:

```logtalk
%%table
streams::nat_stream([0, s(0), s(s(0))| T]).
```

<!--
T = [s(s(0))|T] ;
T = [s(0), s(s(0))|T] ;
T = [0, s(0), s(s(0))|T] ;
false.
-->

```logtalk
X = [0, 1, 1, 0| X], streams::bit_stream(X).
```

<!--
X = [0, 1, 1, 0|X] ;
false.
-->

Filtering odd numbers from a list:

```logtalk
L = [0, s(0), s(s(0))| L], filter::filter(L, F).
```

<!--
L = [0, s(0), s(s(0))|L], F = [0, s(s(0))|F] ;
false.
-->

Using the Sieve of Eratosthenes to find prime numbers:

```logtalk
sieve::primes(20, P).
```

<!--
P = [2, 3|_S1], % where
    _S1 = [5, 7, 11, 13, 17, 19, 2, 3|_S1] ;
false.
-->

List membership example:

```logtalk
X = [1, 2, 3| X], lists::comember(2, X).
```

<!--
X = [1, 2, 3|X] ;
false.
-->

```logtalk
X = [1, 2, 3, 1, 2, 3], lists::comember(2, X).
```

<!--
false.

```logtalk
%%table
X = [1, 2, 3| X], lists::comember(Y, X).
```

<!--
X = [1, 2, 3|X], Y = 1 ;
X = [1, 2, 3|X], Y = 2 ;
X = [1, 2, 3|X], Y = 3 ;
false.
-->

```logtalk
X = [0, s(0), s(s(0))], lists::comember(s(0), X).
```

<!--
false.
-->

```logtalk
X = [0, s(0), s(s(0))| X], lists::comember(s(0), X).
```

<!--
X = [0, s(0), s(s(0))|X] ;
false.
-->

List append example:

```logtalk
Y = [4,5,6| Y], lists::append([1,2,3], Y, Z).
```

<!--
Y = [4, 5, 6|Y], Z = [1, 2, 3, 4, 5, 6|Y].
-->

```logtalk
X = [1,2,3| X], Y = [3,4| Y], lists::append(X, Y, Z).
```

<!--
X = [1, 2, 3|X],
Y = [3, 4|Y],
Z = [1|_S1], % where
    _S1 = [2, 3, 1|_S1] ;
false.
-->

```logtalk
%%table
Z = [1,2| Z], lists::append(X, Y, Z).
```

<!--
Z = Y, Y = [1, 2|Y], X = [] ;
Z = [1, 2|Z], X = [1], Y = [2|Z] ;
Z = X, X = [1, 2|X] ;
false.
-->

List non-membership example:

```logtalk
X = [1,2,3], lists::absent(2, X).
```

<!--
false.
-->

```logtalk
X = [1,2,3], lists::absent(4, X).
```

<!--
false.
-->

```logtalk
X = [1,2,3| X], lists::absent(4, X).
```

<!--
X = [1, 2, 3|X] ;
false.
-->

```logtalk
X = [1,2,3| X], lists::absent(2, X).
```

<!--
false.
-->

Sorting example:

```logtalk
X = [1-2,2-3,1-4|X], sorting::keysort(X, L).
```

<!--
X = [1-2, 2-3, 1-4|X],
L = [1-2|_S1], % where
    _S1 = [1-2|_S1] .
-->

```logtalk
X = [1-2,2-3|Y], Y = [1-4|Y], sorting::keysort(X, L).
```

<!--
X = [1-2, 2-3|_S1], % where
    _S1 = [1-4|_S1],
Y = [1-4|_S1],
L = [1-2|_S2], % where
    _S2 = [1-4|_S2] .
-->

Omega-automaton example:

```logtalk
%%table
automaton::automaton(s0, X).
```

<!--
X = [a, b, c, d|X] ;
X = [a, b, e|X] ;
false.
-->

Module 4 counter example:

```logtalk
counter::verify.
```

<!--
true.
-->

Nested automata example:

```logtalk
%%table
nested::state(s0, X), lists::absent(s2, X).
```

<!--
X = [s0|_S1], % where
    _S1 = [s1|_S1] ;
X = [s0, s3|X] ;
false.
-->

Timed automata example:

```logtalk
%%table
train::driver(s0, s0, s0, X, R).
```

<!--
X = [approach, lower|_S1], % where
    _S1 = [down, in, out, exit, raise, approach, up, lower|_S1],
R = [ (approach, 0), (lower, 1.0)|_S2], % where
    _S2 = [ (down, _G4969), (in, _G4975), (out, _G4981), (exit, _G4987), (raise, _G4993), (approach, _G4999), (up, _G5005), (lower, 1.0)|_S2],
{_G5024>0.0, _G5033= ... + ... + _G5049+_G5046+_G5043-_G5040+_G5024, _G5040> -1.0, _G5040<0.0, _G5078= ... + ... + _G5046+_G5043, _G5093>0.0, _G5005= ... - ..., ... = ..., ..., ...} ;
X = [approach|_S1], % where
    _S1 = [lower, down, in, out, exit, raise, up, approach|_S1],
R = [ (approach, 0)|_S2], % where
    _S2 = [ (lower, 1.0), (down, _G4919), (in, _G4925), (out, _G4931), (exit, _G4937), (raise, _G4943), (up, _G4949), (approach, 0)|_S2],
{_G4965>0.0, _G4974=_G4925+_G4990+_G4987+_G4984+_G4981+_G4965, _G4995=_G4925+_G4990+_G4987+_G4984, _G4981>1.0, _G4981<2.0, _G4949= ... + ... + _G4981, _G5046= ... + ..., ... > ..., ..., ...} ;
false.
-->

Timed automata coroutining example:

```logtalk
cotrain::comain(A, B, C).
```

<!--
A = [approach, in, out, exit|A], B = [approach, exit|B], C = [lower, raise|C] ;
false.
-->

```logtalk
cotrain::test_max(M, N, R).
```

<!--
R = [ (approach, 0), (lower, 1.0), (down, _G3563), (in, _G3569), (out, _G3575), (exit, _G3581), (raise, _G3587), (up, _G3593)],
{_G3600>0.0, M= ... + ... + _G3625+_G3622+_G3619+_G3616+_G3600, _G3635>0.0, N= ... + ... + _G3619+_G3616-_G3635, ... - ... - _G3616+_G3635< -0.0, _G3697>0.0, _G3706= ... + ..., ... > ..., ..., ...} ;
false.
-->

Finding the cyclic paths in graphs:

```logtalk
%%table
cp1::path(a, Path).
```

<!--
Path = [a, b|_S1], % where
    _S1 = [b|_S1] ;
Path = [a, b, c, d|_S1], % where
    _S1 = [d|_S1] ;
Path = [a|_S1], % where
    _S1 = [b, c, a|_S1] ;
false.
-->

```logtalk
%%table
cp2::path(a, Path).
```

<!--
Path = [a|_S1], % where
    _S1 = [b, c, a|_S1] ;
Path = [a|_S1], % where
    _S1 = [b, c, d, a|_S1] ;
false.
-->

```logtalk
cp3::path(a, Path, 3).
Path = [a|_S1], % where
    _S1 = [b, c, a|_S1] ;
false.
-->

Testing for bipartite graphs (vertex adjacency lists must be ordered):

```logtalk
A = v(a, [F]), B = v(b, [F, G]), C = v(c, [H, I]), D = v(d, [G]), E = v(e, [F, I]), F = v(f, [A, B]), G = v(g, [B, D]), H = v(h, [C]), I = v(i, [C, E]), graph::bipartite(A).
```

<!--
A = v(a, [_S1]), % where
    _S1 = v(f, [v(a, [_S1]), v(b, [_S1, _S2])]),
    _S2 = v(g, [v(b, [_S1, _S2]), v(d, [_S2])]),
F = v(f, [v(a, [_S1]), v(b, [_S1, _S2])]),
B = v(b, [_S1, _S2]),
G = v(g, [v(b, [_S1, _S2]), v(d, [_S2])]),
C = _S3, % where
    _S3 = v(c, [v(h, [_S3]), _S4]),
    _S4 = v(i, [_S3, v(e, [_S1, _S4])]),
H = v(h, [_S3]),
I = v(i, [_S3, v(e, [_S1, _S4])]),
D = v(d, [_S2]),
E = v(e, [_S1, _S4]) ;
false.
-->
