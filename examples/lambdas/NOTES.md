---
jupyter:
  jupytext:
    text_representation:
      extension: .md
      format_name: markdown
      format_version: '1.3'
      jupytext_version: 1.16.7
  kernelspec:
    display_name: Logtalk
    language: logtalk
    name: logtalk_kernel
---

<!--
________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>  
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

# lambdas

This folder contains examples of using Logtalk support for lambda
expressions. Some examples and test queries are collected from public
discussions on lambda expressions on the SWI-Prolog mailing list and
on the `comp.lang.prolog` discussion group.

The `lambda_warnings` file illustrates lint checks for lambda expressions.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the example:

```logtalk
logtalk_load(lambdas(loader)).
```

Some sample queries using the `call/N` built-in methods (note that these
methods are private, hence the use of the context-switching `(<<)/2` control
construct):

```logtalk
logtalk << call([X,Y]>>(Y is X*X), 5, R).
```

<!--
R = 25
-->

An example using currying:

```logtalk
logtalk << call([Z]>>(call([X,Y]>>(Y is X*X), 5, R), Z is R*R), T).
```

<!--
T = 625.
-->

Some sample queries using the `metapredicates` library predicates.

Checking that all list elements are greater than 3:

```logtalk
meta::map([X]>>(X>3),[4,5,9]).
```

<!--
true.
-->

Exchanging the key and value in a list of pairs:

```logtalk
meta::map([A-B,B-A]>>true, [1-a,2-b,3-c], Zs).
```

<!--
Zs = [a-1,b-2,c-3].
-->

Exchanging the key and value in a list of pairs using currying:

```logtalk
meta::map([A-B]>>([B-A]>>true), [1-a,2-b,3-c], Zs).
```

<!--
Zs = [a-1,b-2,c-3].
-->

Computing the distances from points to origin:

```logtalk
Points = [(1,4),(2,5),(8,3)], meta::map([(X,Y),Z]>>(Z is sqrt(X*X + Y*Y)), Points, Distances).
```

<!--
Distances = [4.1231056256176606,5.3851648071345037,8.5440037453175304], Points = [(1,4),(2,5),(8,3)].
-->

Computing the distances from points to origin using currying:

```logtalk
Points = [(1,4),(2,5),(8,3)], meta::map([(X,Y)]>>([Z]>>(Z is sqrt(X*X + Y*Y))), Points, Distances).
```

<!--
Distances = [4.1231056256176606,5.3851648071345037,8.5440037453175304], Points = [(1,4),(2,5),(8,3)].
-->

Computing the sum of squares of point coordinates:

```logtalk
meta::map([(X,Y),Z]>>(Z is X*X + Y*Y), [(1,2),(3,4),(5,6)], Result).
```

<!--
Result = [5,25,61].
-->

Computing the sum of squares of point coordinates using currying:

```logtalk
meta::map([(X,Y)]>>([Z]>>(Z is X*X + Y*Y)), [(1,2),(3,4),(5,6)], Result).
```

<!--
Result = [5,25,61].
-->

Mapping a list of lists of lists:

```logtalk
Xsss = [[[1,2,3],[4]],[[5]]], meta::map(meta::map(meta::map([X,Y]>>(Y is X+3))), Xsss, Ysss).
```

<!--
Xsss = [[[1,2,3],[4]],[[5]]], Ysss = [[[4,5,6],[7]],[[8]]].
-->

Computing a list of lists from a list:

```logtalk
meta::map([X,[X]]>>true,[1,2],Ys).
```

<!--
Ys = [[1],[2]].
-->

Computing a list from a list of lists:

```logtalk
meta::map([X,[X]]>>true,Xs,[[1],[2]]).
```

<!--
Xs = [1,2].
-->

In the next example, we use currying to "declare" the variable L as a local variable:

```logtalk
meta::map([N]>>({L}/[M]>>(list::length(L, N), list::length([1|L], M))), [999,123],R).
```

<!--
R = [1000,124].
-->

Some sample queries using lambda expressions as goals (not closures):

```logtalk
logtalk<<([]>>true), logtalk<<({}/true), logtalk<<({}/[]>>true), logtalk<<({X}/true).
```

<!--
true.
-->

Some error cases:

```logtalk
catch(logtalk << ({X}/[X]>>true), Error, true).
```

<!--
Error = error(representation_error(lambda_parameters),{_282}/[_282]>>true,logtalk).
-->

```logtalk
catch(meta::map([X,Y,Z]>>char_code(X), [a,b,c], R), Error, true).
```

<!--
uncaught exception: error(representation_error(lambda_parameters),[_278,_280,_282]>>char_code(_278),meta)
-->

Examples on simplifying `setof/3` and similar predicates usage:

```logtalk
%%table
countries::currencies_wrong(Currencies).
```

<!--
Currencies = [pound_sterling] ;
Currencies = [dinar] ;
Currencies = [ringgit] ;
Currencies = [euro] ;
Currencies = [euro] ;
Currencies = [dinar].
-->

```logtalk
countries::currencies_no_lambda(Currencies).
```

<!--
Currencies = [dinar, euro, pound_sterling, ringgit].
-->

```logtalk
countries::currencies_lambda(Currencies).
```

<!--
Currencies = [dinar, euro, pound_sterling, ringgit].
-->

Example of using a custom implementation of fold left in disguise. Sum of the first 9 natural numbers:

```logtalk
sigma::sum([X,Y]>>(Y is X), 0, 9, R).
```

<!--
R = 45.
-->

Sum of the squares of the first 9 natural numbers:

```logtalk
sigma::sum([X,Y]>>(Y is X*X), 0, 9, R).
```

<!--
R = 285.
-->

Remember that closures are called in the context of the _sender_ when
used as arguments to meta-predicates:

```logtalk
sigma::sum([X,Y]>>(sigma::sum([W,Z]>>(Z is W), X, 9, Y)), 0, 9, R).
```

<!--
R = 330.
-->

Use the `fold_left/4` meta-predicate to calculate Fibonacci numbers. Nth Fibonacci number:

```logtalk
meta::fold_left([N1-[F1,F2],_,N2-[F2,F3]]>>(F3 is F1+F2, N2 is N1+1), 0-[0,1], _, 10-[F, _]).
```

<!--
F = 55.
-->

Sequence of Fibonacci numbers:

```logtalk
meta::fold_left([N1-[F1,F2],_,N2-[F2,F3]]>>(F3 is F1+F2, N2 is N1+1), 0-[0,1], _, N-[F, _]).
```

<!--
F = 0
N = 0 ? ;
F = 1
N = 1 ? ;
F = 1
N = 2 ? ;
F = 2
N = 3 ? ;
F = 3
N = 4 ? ;
F = 5
N = 5 ?
...
-->

Miscellaneous tests of using lambda expressions:

```logtalk
misc::common_prefix([1], Xs, Ys).
```

<!--
Xs = [],  Ys = [] ? ;
Xs = [A], Ys = [[1|A]] ? ;
Xs = [A,B], Ys = [[1|A],[1|B]] ? ;
Xs = [A,B,C], Ys = [[1|A],[1|B],[1|C]] ?
...
-->

```logtalk
misc::call_n.
```

<!--
This test should print f(x,y) in all lines:
f(x,y)
f(x,y)
f(x,y)
f(x,y)
f(x,y)
f(x,y)

true.
-->

```logtalk
misc::local.
```

<!--
true.
-->

The following lambda benchmarks are so far only available when using
SWI-Prolog, Trealla Prolog, XSB, XVM, or YAP as the Logtalk backend
compilers:

```logtalk
lambda_benchmarks::bench1.
```

<!--
Using map/2 with a closure for testing less(0, X) with X in [1..100000]: 
% 600,004 inferences, 0.091 CPU in 0.095 seconds (95% CPU, 6618178 Lips)
Using map/2 with a lambda for testing less(0, X) with X in [1..100000]:  
% 2,300,030 inferences, 0.479 CPU in 0.481 seconds (100% CPU, 4801322 Lips)

true.
-->

The second benchmarks is based on code posted by Jan Wielemaker in
the SWI-Prolog mailing list:

```logtalk
lambda_benchmarks::bench2.
```

<!--
Adding 1 to every integer in the list [1..100000] using a local add1/2 predicate:
% 100,002 inferences, 0.015 CPU in 0.016 seconds (93% CPU, 6702547 Lips)
Adding 1 to every integer in the list [1..100000] using map/3 with the integer::plus/3 predicate:
% 700,004 inferences, 0.133 CPU in 0.134 seconds (99% CPU, 5257456 Lips)
Adding 1 to every integer in the list [1..100000] using map/3 with a lambda argument with a is/2 goal:
% 1,800,030 inferences, 0.439 CPU in 0.442 seconds (99% CPU, 4100651 Lips)

true.
-->

Learn about lint checks for lambdas expressions:

```logtalk
logtalk_load(lambda_warnings).
```

<!--
*     Unclassified variable A in lambda expression: [B,C]>>f(B,C,A)
*       while compiling object lambda_warnings
*       in file .../examples/lambdas/lambda_warnings.lgt between lines 31-33
*     
*     Variable A have dual role in lambda expression: {A}/[B,C,A]>>f(B,C,A)
*       while compiling object lambda_warnings
*       in file .../examples/lambdas/lambda_warnings.lgt between lines 36-37
*     
% [ .../examples/lambdas/lambda_warnings.lgt loaded ]
% 2 compilation warnings

true.
-->

Some examples with constraints using GNU Prolog as the backend compiler:

```logtalk
Xss = [[1,2],[3]], meta::map(meta::map([X,Y,Z]>>(X+Y#=Z)), Xss, Yss, Zss).
```

<!--
Xss = [[1,2],[3]],
Yss = [[_#3(0..268435454),_#54(0..268435453)],[_#105(0..268435452)]],
Zss = [[_#22(1..268435455),_#73(2..268435455)],[_#124(3..268435455)]].

(1 ms) yes
-->

```logtalk
Xss= [[1,2],[3]], meta::map(meta::map({Y}/[X,Z]>>(X+Y#=Z)), Xss, Zss).
```

<!--
Xss = [[1,2],[3]],
Y = _#3(0..268435452),
Zss = [[_#22(1..268435453),_#66(2..268435454)],[_#110(3..268435455)]].

(1 ms) yes
-->

```logtalk
meta::map({Y}/[X,Z]>>(Z#=X+Y), Xs, Ys).
```

<!--
Xs = [] Ys = [] ? ;
Xs = [_#3(0..268435455)], Y = _#22(0..268435455), Ys = [_#41(0..268435455)] ? ;
Xs = [_#3(0..268435455),_#96(0..268435455)], Y = _#22(0..268435455), Ys = [_#41(0..268435455),_#115(0..268435455)] ? ;
...
-->

Some examples with constraints using SWI-Prolog or YAP as the backend compiler:

```logtalk
use_module(library(clpfd)).
```

```logtalk
Xs = [A,B], meta::map({Y}/[X,Z]>>(clpfd:(X+Y #= Z)), Xs, Zs).
```

<!--
Xs = [A, B], Zs = [_G1114, _G1117], A+Y#=_G1114, B+Y#=_G1117.
-->

```logtalk
meta::map({Z}/[X,Y]>>(clpfd:(Z#=X+Y)), Xs, Ys).
```

<!--
Xs = [], Ys = [] ;
Xs = [_G1369], Ys = [_G1378], _G1369+_G1378#=Z ;
Xs = [_G1579, _G1582], Ys = [_G1591, _G1594], _G1582+_G1594#=Z, _G1579+_G1591#=Z ;
Xs = [_G1789, _G1792, _G1795], Ys = [_G1804, _G1807, _G1810], _G1795+_G1810#=Z, _G1792+_G1807#=Z, _G1789+_G1804#=Z ;
...
-->

Some examples with constraints using B-Prolog as the backend compiler:

```logtalk
Xss= [[1,2],[3]], meta::map(meta::map([X,Y,Z]>>(X+Y#=Z)), Xss, Yss, Zss).
```

<!--
Xss = [[1,2],[3]],
Yss = [[_01acd0:[-268435455..268435455],_0348d0:[-268435455..268435455]],[_04e5dc:[-268435455..268435455]]],
Zss = [[_01ac9c:[-268435455..268435455],_03489c:[-268435455..268435455]],[_04e5a8:[-268435455..268435455]]].
-->

```logtalk
Xss= [[1,2],[3]], meta::map(meta::map({Y}/[X,Z]>>(X+Y#=Z)), Xss, Zss).
```

<!--
Xss = [[1,2],[3]],
Zss = [[_01aca4:[-268435455..268435455],_0348cc:[-268435455..268435455]],[_04e5c4:[-268435455..268435455]]].
-->

Some examples with constraints using SICStus Prolog as the backend compiler:

```logtalk
use_module(library(clpfd)).
```

```logtalk
Xss= [[1,2],[3]], meta::map(meta::map([X,Y,Z]>>(clpfd:(X+Y#=Z))), Xss, Yss, Zss).
```

<!--
Xss = [[1,2],[3]],
Yss = [[_A,_B],[_C]],
Zss = [[_D,_E],[_F]],
_D in inf..sup,
_A in inf..sup,
_E in inf..sup,
_B in inf..sup,
_F in inf..sup,
_C in inf..sup ?
-->

```logtalk
Xs = [A,B], meta::map({Y}/[X,Z]>>(clpfd:(X+Y #= Z)), Xs, Zs).
```

<!--
Xs = [A,B],
Zs = [_A,_B],
A in inf..sup,
Y in inf..sup,
_A in inf..sup,
B in inf..sup,
_B in inf..sup ?
-->
