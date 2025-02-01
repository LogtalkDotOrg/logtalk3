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

# logic

This folder contains an object which implements a translator from 
first-order predicate logic propositions to conjunctive normal form 
and to clausal form. The translator code is partially based on code 
published in the book "Programming in Prolog" by W. F. Clocksin and 
C. S. Mellish.

The following operators are used for representing logic connectives:

- negation: `~`
- disjunction: `v`
- conjunction: `&`
- implication: `=>`
- equivalence: `<=>`

Quantifiers are represented using the following notation:

- universal: `all(X, P)`
- existential: `exists(X, P)`

The two main object predicates are `translate/2` and `step_by_step/2`.
The first predicate, `translate/2`, translate a logic proposition to 
a list of clauses. The second predicate, `step_by_step/2`, performs 
the same translations as `translate/2` but also prints the results 
of each conversion step.

Start by loading the example:

```logtalk
logtalk_load(logic(loader)).
```

Translate a single logic proposition:

```logtalk
translator::translate((p v ~q) => (r & k), Cs).
```

<!--
r :- p.
k :- p.
q; r :- .
q; k :- .

Cs = [cl([r],[p]),cl([k],[p]),cl([q,r],[]),cl([q,k],[])].
-->

Translate a single logic proposition printing each translation step:

```logtalk
translator::step_by_step((p v ~q) => (r & k), Cs).
```

<!--
Processing proposition: p v ~q=>r&k

  1. Remove implications: ~ (p v ~q) v r&k
  2. Distribute negation: ~p&q v r&k
  3. Remove existential quantifiers: ~p&q v r&k
  4. Convert to prenex normal form: ~p&q v r&k
  5. Remove universal quantifiers: ~p&q v r&k
  6. Convert to conjunctive normal form: (~p v r)&(~p v k)&((q v r)&(q v k))
  7. Convert to clauses: [cl([r],[p]),cl([k],[p]),cl([q,r],[]),cl([q,k],[])]

Clauses in Prolog-like notation:
r :- p.
k :- p.
q; r :- .
q; k :- .

Cs = [cl([r],[p]),cl([k],[p]),cl([q,r],[]),cl([q,k],[])].
-->

Translate a single logic proposition printing each translation step:

```logtalk
translator::step_by_step(all(X, exists(Y, p(X) v ~q(X) => r(X, Y))), Cs).
```

<!--
Processing proposition: all(X, exists(Y, p(X)v~q(X)=>r(X, Y)))

  1. Remove implications: all(X, exists(Y, ~ (p(X)v~q(X))v r(X, Y)))
  2. Distribute negation: all(X, exists(Y, ~p(X)&q(X)v r(X, Y)))
  3. Remove existential quantifiers: all(X, ~p(X)&q(X)v r(X, f1(X)))
  4. Convert to prenex normal form: all(X, ~p(X)&q(X)v r(X, f1(X)))
  5. Remove universal quantifiers: ~p(X)&q(X)v r(X, f1(X))
  6. Convert to conjunctive normal form: (~p(X)v r(X, f1(X)))& (q(X)v r(X, f1(X)))
  7. Convert to clauses: [cl([r(X, f1(X))], [p(X)]), cl([q(X), r(X, f1(X))], [])]

Clauses in Prolog-like notation:
r(X, f1(X)) :- p(X).
q(X); r(X, f1(X)) :- .

X = X, Y = f1(X), Cs = [cl([r(X, f1(X))], [p(X)]), cl([q(X), r(X, f1(X))], [])].
-->

Translate a single logic proposition printing each translation step:

```logtalk
translator::step_by_step(all(X, men(X) => mortal(X)), Cs).
```

<!--
Processing proposition: all(X, men(X)=>mortal(X))

  1. Remove implications: all(X, ~men(X)v mortal(X))
  2. Distribute negation: all(X, ~men(X)v mortal(X))
  3. Remove existential quantifiers: all(X, ~men(X)v mortal(X))
  4. Convert to prenex normal form: all(X, ~men(X)v mortal(X))
  5. Remove universal quantifiers: ~men(X)v mortal(X)
  6. Convert to conjunctive normal form: ~men(X)v mortal(X)
  7. Convert to clauses: [cl([mortal(X)], [men(X)])]

Clauses in Prolog-like notation:
mortal(X) :- men(X).

X = X, Cs = [cl([mortal(X)], [men(X)])].
-->
