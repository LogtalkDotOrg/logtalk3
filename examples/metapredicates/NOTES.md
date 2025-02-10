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

# metapredicates

This example shows the use of meta-predicates in Logtalk. Meta-predicates are
predicates whose head includes arguments that will be called as goals in the
body of the predicate definition.

This example defines the following objects:

- `company`  
	usage example of the `map_reduce/5` meta-predicate

- `fibonacci`  
	example of calculating Fibonacci numbers using the `fold_left/4`
	meta-predicate

- `sort(_)`  
	this is a parametric object containing a method that implements the
	quicksort sorting algorithm; the parameter is interpreted as the type
	of the elements being sorted

- `tracer`  
	this object implements a meta-predicate that is used by `sort(_)` to 
	trace the sorting algorithm steps

- `metapreds`, `descendant`, and `test`  
	objects used for illustrating the use of closures as meta-arguments

- `predicates`  
	object defining some predicates for testing meta-predicates defined 
	in the Logtalk library

- `grammar`  
	object illustrating using `phrase/2` as a meta-predicate meta-argument

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the example and the required library files:

```logtalk
logtalk_load(metapredicates(loader)).
```

Try the simple meta-predicate definitions from the "goals.lgt" file:

```logtalk
simple_client::test_whatever.
```

<!--
true.
-->

```logtalk
simple_client::test_whatever_all.
```

<!--
Hello world!

true.
-->

Alternatively:

```logtalk
simple_client_alt::test_whatever.
```

<!--
true.
-->

```logtalk
simple_client_alt::test_whatever_all.
```

<!--
Hello world!

true.
-->

Note that _user_ is a pseudo-object representing the Prolog database;
therefore, the integer comparisons are performed using the standard
Prolog built-in predicates:

```logtalk
sort(user)::sort([3, 1, 4, 2, 9], Sorted).
```

<!--
call: partition([1,4,2,9],3,_358,_359)
exit: partition([1,4,2,9],3,[1,2],[4,9])
call: sort([1,2],_740)
call: partition([2],1,_967,_968)
exit: partition([2],1,[],[2])
call: sort([],_1300)
exit: sort([],[])
call: sort([2],_1539)
call: partition([],2,_1765,_1766)
exit: partition([],2,[],[])
call: sort([],_2093)
exit: sort([],[])
call: sort([],_2332)
exit: sort([],[])
exit: sort([2],[2])
exit: sort([1,2],[1,2])
call: sort([4,9],_2831)
call: partition([9],4,_3058,_3059)
exit: partition([9],4,[],[9])
call: sort([],_3391)
exit: sort([],[])
call: sort([9],_3630)
call: partition([],9,_3856,_3857)
exit: partition([],9,[],[])
call: sort([],_4184)
exit: sort([],[])
call: sort([],_4423)
exit: sort([],[])
exit: sort([9],[9])
exit: sort([4,9],[4,9])

Sorted = [1,2,3,4,9].
-->

Test the `setof/3` wrapper:

```logtalk
wrappers_client::p(L).
```

<!--
L = [1, 2, 3].
-->

```logtalk
wrappers_client::q(L).
```

<!--
L = [1, 2, 3].
-->

Call the meta-predicate `apply/2` directly:

```logtalk
metapreds::test_this.
```

<!--
1, metapreds

true.
-->

Send an `apply/2` message to self:

```logtalk
descendant::test_self.
```

<!--
2, descendant

true.
-->

Send an `apply/2` message from another object:

```logtalk
test::test_obj.
```

<!--
3, test

true.
-->

Use the `partition/4` meta-predicate with a predicate defined in the
pseudo-object `user`:

```logtalk
meta::partition(even_integer, [1,2,3,4,5], Included, Excluded).
```

<!--
Included = [2, 4], Excluded = [1, 3, 5].
-->

Get a visual illustration of the differences between left and right folds:

```logtalk
folds::left(Left).
```

<!--
Left = '(((((((((0+1)+2)+3)+4)+5)+6)+7)+8)+9)'.
-->

```logtalk
folds::right(Right).
```

<!--
Right = '(1+(2+(3+(4+(5+(6+(7+(8+(9+0)))))))))'.
-->

Walk a list using left and right folds (note the lambda parameters order):

```logtalk
meta::fold_left([Y, X, [X|Y]]>>true, [], [1,2,3,4,5,6,7,8,9], R).
```

<!--
R = [9, 8, 7, 6, 5, 4, 3, 2, 1]
-->

```logtalk
meta::fold_right([X, Y, [X|Y]]>>true, [], [1,2,3,4,5,6,7,8,9], R).
```

<!--
R = [1, 2, 3, 4, 5, 6, 7, 8, 9].
-->

We can compose left folds:

```logtalk
meta::fold_left([X,Y,Z]>>(Z is X + Y), 0, [1,2,3], Sum).
```

<!--
Sum = 6.
-->

```logtalk
meta::fold_left(meta::fold_left([X,Y,Z]>>(Z is X + Y)), 0, [[1,2,3],[4,5,6]], Sum).
```

<!--
Sum = 21.
-->

Use the `fold_left/4` meta-predicate with a predicate defined in the
pseudo-object _user_:

```logtalk
meta::fold_left(sum_squares, 0, [1,2,3], Result).
```

<!--
Result = 34.
-->

Use the `fold_left/4` and `fold_right/4` meta-predicates with the `atom_concat/3`
Prolog built-in predicate:

```logtalk
meta::fold_left(atom_concat, 'PREFIX', [abc,def,ghi], Result).
```

<!--
Result = 'PREFIXabcdefghi'.
-->

```logtalk
meta::fold_right(atom_concat, 'SUFIX', [abc,def,ghi], Result).
```

<!--
Result = abcdefghiSUFIX.
-->

Use the `fold_left/4` meta-predicate with predicates defined in the object
`predicates`:

```logtalk
meta::fold_left(predicates::sum, 0, [1,2,3,4,5], Result).
```

<!--
Result = 15.
-->

```logtalk
meta::fold_left(predicates::product, 1, [1,2,3,4,5], Result).
```

<!--
Result = 120.
-->

```logtalk
meta::fold_left(predicates::tuple, (0,0), [(1,2), (3,4), (6,4)], Result).
```

<!--
Result = (10, 10).
-->

Alternatives to `fold_left/4` and `fold_right/4` when there isn't an initial value:

```logtalk
meta::fold_left_1([X,Y,Z]>>(Z is X+Y), [1,2,3,4,5], R).
```

<!--
R = 15.
-->

```logtalk
meta::fold_right_1([X,Y,Z]>>(Z is X-Y), [1,2,3,4,5], R).
```

<!--
R = 3.
-->

```logtalk
meta::fold_right_1([X,Y,Z]>>(Z is X*Y), [1,2,3,4,5], R).
```

<!--
R = 120.
-->

Use the `scan_left/4` and `scan_right/4` meta-predicates with predicates
defined in the object `predicates`:

```logtalk
meta::scan_left(sum_squares, 0, [1,2,3], Result).
```

<!--
Result = [0, 1, 5, 34].
-->

```logtalk
meta::scan_right(predicates::sum, 5, [1,2,3,4], Result).
```

<!--
Result = [15, 14, 12, 9, 5].
-->

Compute a sequence of factorial numbers using a scan of natural numbers
using multiplication:

```logtalk
meta::scan_left([X,Y,Z]>>(Z is X*Y), 1, [1,2,3,4,5,6], Result).
```

<!--
Result = [1, 1, 2, 6, 24, 120, 720].
-->

Use the `map/2-3` meta-predicates with some Prolog built-in predicates.

Check that all list elements are integers:

```logtalk
meta::map(integer, [1,2,3,4,5]).
```

<!--
true.
-->

Map characters to character codes:

```logtalk
meta::map(char_code, [a,b,c,d,e], Codes).
```

<!--
Codes = [97, 98, 99, 100, 101].
-->

Use the `fold_left/4` meta-predicate to calculate Fibonacci numbers:

```logtalk
fibonacci::nth(10, Fib).
```

<!--
Fib = 55.
-->

The first six Fibonacci numbers:

```logtalk
%%table
integer::between(0, 5, Nth), fibonacci::nth(Nth, Fib).
```

<!--
Fib = 0, Nth = 0 ? ;
Fib = 1, Nth = 1 ? ;
Fib = 1, Nth = 2 ? ;
Fib = 2, Nth = 3 ? ;
Fib = 3, Nth = 4 ? ;
Fib = 5, Nth = 5 ? 
...
-->

Use the `map_reduce/5` meta-predicate to give bad news to a company employees:

```logtalk
company::(company(C1), get_salary(company(C1),S1)).
```

<!--
C1 = [topdept(name('Human Resources'),manager(name('Lisa'),salary(123456)),[]),topdept(name('Development'),manager(name('Anders'),salary(43210)),[subdept(name('Visual Basic'),manager(name('Amanda'),salary(8888)),[]),subdept(name('Visual C#'),manager(name('Erik'),salary(4444)),[])])]
S1 = 179998
-->

```logtalk
company::(company(C1), cut_salary(company(C1), C2), get_salary(C2, S2)).
```

<!--
C1 = [topdept(name('Human Resources'),manager(name('Lisa'),salary(123456)),[]),topdept(name('Development'),manager(name('Anders'),salary(43210)),[subdept(name('Visual Basic'),manager(name('Amanda'),salary(8888)),[]),subdept(name('Visual C#'),manager(name('Erik'),salary(4444)),[])])]
C2 = company([topdept(name('Human Resources'),manager(name('Lisa'),salary(61728)),[]),topdept(name('Development'),manager(name('Anders'),salary(21605)),[subdept(name('Visual Basic'),manager(name('Amanda'),salary(4444)),[]),subdept(name('Visual C#'),manager(name('Erik'),salary(2222)),[])])])
S2 = 89999
-->
