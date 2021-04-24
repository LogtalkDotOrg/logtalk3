________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>  
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


`edcg`
======

This library provides a Logtalk port of the Peter Van Roy's extended DCG
implementation. For full documentation on EDCGs, see:

https://www.info.ucl.ac.be/%7Epvr/edcg.html

This Logtalk version defines a hook object, `edcg`. Source files defining
EDCGs must be compiled using the compiler option `hook(edcg)`:

	| ?- logtalk_load(source, [hook(edcg)]).

Alternatively, the following directive can be added at the beginning of the
source file containing the EDCGs:

	:- set_logtalk_flag(hook, edcg).

The hook object automatically adds the EDCGs `-->>` infix operator scoped
to the source file.

This port has simplified by copying and then modifying Michael Hendricks's
`edcg` repo at:

https://github.com/mndrix/edcg

A notable difference is that Michael's version declares Peter's original
predicates for declaring accumulators and predicates using the hidden
arguments as multifile predicates. But this is risky as two independent
EDCGs may use e.g. the same accumulator names and introduce conflicts.
The Logtalk version uses instead the `edcg` hook object internal state
to temporarily save those predicates in order to parse the corresponding
EDCGs.


API documentation
-----------------

Open the [../../docs/library_index.html#edcg](../../docs/library_index.html#edcg)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` utility file:

	| ?- logtalk_load(edcg(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file of the `edcgs`
example:

	| ?- logtalk_load(edcgs(tester)).


Usage
-----

Follows the usage documentation written by Michael Hendricks (with a
contribution from Peter Ludemann), used here with permission, with the
necessary changes for the Logtalk port.

```logtalk

% Declare accumulators
acc_info(adder, X, In, Out, integer::plus(X,In,Out)).

% Declare predicates using these hidden arguments
pred_info(len,0,[adder,dcg]).
pred_info(increment,0,[adder]).

increment -->>
    [1]:adder.  % add one to the accumulator


len(Xs,N) :-
    len(0,N,Xs,[]).

len -->>
    [_],  % 'dcg' accumulator has an element
    !,
    increment,  % increment the 'adder' accumulator
    len.
len -->>
    [].
```

Introduction
------------

DCG notation gives us a single, hidden accumulator.  Extended DCG notation (implemented by this library) lets predicates have arbitrarily many hidden accumulators. As demonstrated by the Synopsis above, those accumulators can be implemented with arbitrary goals (like `integer::plus/3`).

Benefits of this library:

  * avoid tedium and errors from manually threading accumulators through your predicates
  * add or remove accumulators with a single declaration
  * change accumulator implementation with a single declaration (ex, switching from ordsets to rbtrees)

Syntax
------

Extended DCG syntax is very similar to DCG notation.  An EDCG is created with clauses whose neck is the `-->>` operator.  The following syntax is supported inside an EDCG clause:

  * `{Goal}` - don't expand any hidden arguments of `Goal`
  * `Goal` - expand all hidden arguments of Goal that are also in the head. Those hidden arguments not in the head are given default values.
  * `Goal:L` - If `Goal` has no hidden arguments then force the expansion of all arguments in `L` in the order given. If `Goal` has hidden arguments then expand all of them, using the contents of `L` to override the expansion. `L` is either a term of the form `Acc`, `Acc(Left,Right)`, `Pass`, `Pass(Value)`, or a list of such terms. When present, the arguments `Left`, `Right`, and `Value` override the default values of arguments not in the head.
  * `List:Acc` - Accumulate a list of terms in the accumulator `Acc`
  * `List` - Accumulate a list of terms in the accumulator `dcg`
  * `X/Acc` - Unify `X` with the left term for the accumulator `Acc`
  * `Acc/X` - Unify `X` with the right term for the accumulator `Acc`
  * `X/Acc/Y` - Unify `X` with the left and `Y` with the right term for the accumulator `Acc`
  * `insert(X,Y):Acc` - Insert the arguments `X` and `Y` into the chain implementing the accumulator `Acc`. This is useful when the value of the accumulator changes radically because `X` and `Y` may be the arguments of an arbitrary relation
  * `insert(X,Y)` - Insert the arguments `X` and `Y` into the chain implementing the accumulator `dcg`. This inserts the difference list X-Y into the accumulated list

Declaration of Predicates
-------------------------

Predicates are declared with facts of the following form:

```logtalk
pred_info(Name, Arity, List).
```

The predicate `Name/Arity` has the hidden parameters given in `List`. The parameters are added in the order given by `List` and their names must be atoms.

Declaration of Accumulators
---------------------------

Accumulators are declared with facts in one of two forms. The short form is:

```logtalk
acc_info(Acc, Term, Left, Right, Joiner).
```

The long form is:

```logtalk
acc_info(Acc, Term, Left, Right, Joiner, LStart, RStart).
```

In most cases the short form gives sufficient information. It declares the accumulator `Acc`, which must be an atom, along with the accumulating function, `Joiner`, and its arguments `Term`, the term to be accumulated, and `Left` & `Right`, the variables used in chaining.

The long form of `acc_info` is useful in more complex programs. It contains two additional arguments, `LStart` and `RStart`, that are used to give default starting values for an accumulator occurring in a body goal that does not occur in the head. The starting values are given to the unused accumulator to ensure that it will execute correctly even though its value is not used. Care is needed to give correct values for `LStart` and `RStart`. For DCG-like list accumulation both may remain unbound.

Two conventions are used for the two variables used in chaining depending on which direction the accumulation is done. For forward accumulation, `Left` is the input and `Right` is the output. For reverse accumulation, `Right` is the input and `Left` is the output.

Declaration of Passed Arguments
-------------------------------

Passed arguments are conceptually the same as accumulators with `=/2` as the joiner function.  Passed arguments are declared as facts in one of two forms. The short form is:

```logtalk
pass_info(Pass).
```

The long form is:

```logtalk
pass_info(Pass, PStart).
```

In most cases the short form is sufficient. It declares a passed argument `Pass`, that must be an atom. The long form also contains the starting value `PStart` that is used to give a default value for a passed argument in a body goal that does not occur in the head. Most of the time this situation does not occur.

Additional documentation
------------------------

Peter Van Roy's page: [Declarative Programming with State](https://www.info.ucl.ac.be/~pvr/edcg.html)

Technical Report UCB/CSD-90-583 [Extended DCG Notation: A Tool for Applicative Programming in Prolog](https://www2.eecs.berkeley.edu/Pubs/TechRpts/1990/5471.html) by Peter Van Roy

  * The Tech Report's PDF is [here](https://www2.eecs.berkeley.edu/Pubs/TechRpts/1990/CSD-90-583.pdf)

A short [Wikipedia article](https://en.wikipedia.org/wiki/Definite_clause_grammar#Extensions) on DCGs and extensions.
