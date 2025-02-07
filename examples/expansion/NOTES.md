---
jupyter:
  jupytext:
    text_representation:
      extension: .md
      format_name: markdown
      format_version: '1.3'
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

# expansion

The `expansion.lgt` source file illustrates how the scope of the term and
goal-expansion predicates affects the results of the expansion mechanisms.

The `hooks.lgt` and `raw.lgt` source files illustrate how to define and
combine hook objects.

The `pipeline.lgt` source file uses the `hook_pipeline/1` library object,
which illustrates how to define a hook object that applies a pipeline of
hook objects.

For more details, please see the comments in the source files.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the example:

```logtalk
logtalk_load(expansion(loader)).
```

Simple tests of the `expand_term/2` and `expand_goal/2` built-in methods.

Sending the messages `expand_term/2` (`expand_goal/2`) to an object only
result in the use of the clauses defined for the `term_expansion/2` 
(`goal_expansion/2`) hook predicate if this predicate is public:

```logtalk
exp_public::expand_term(8, Term).
```

<!--
Term = eight.
-->

```logtalk
exp_public::expand_goal(write(Term), EGoal).
```

<!--
EGoal = write_term(Term, [quoted(true)]).
-->

The clauses for the `term_expansion/2` (`goal_expansion/2`) hook predicate
will not be used if they are not within the scope of the sender (in this
case, the pseudo-object `user`) as in the following cases:

```logtalk
exp_protected::expand_term(8, Term).
```

<!--
Term = 8.
-->

```logtalk
exp_protected::expand_goal(write(Term), EGoal).
```

<!--
EGoal = write(Term).
-->

```logtalk
exp_private::expand_term(8, Term).
```

<!--
Term = 8.
-->

```logtalk
exp_private::expand_goal(write(Term), EGoal).
``` 

<!--
EGoal = write(Term).
-->

The following queries perform similar tests but with the calls to the
`expand_term/2` (`expand_goal/2`) built-in method being made from within 
the prototypes:

```logtalk
desc_public::test_term_expansion(8, Term).
```

<!--
Term = eight.
-->

```logtalk
desc_public::test_goal_expansion(write(Term), EGoal).
```

<!--
EGoal = write_term(Term, [quoted(true)])
```

<!--
true.
-->

```logtalk
desc_protected::test_term_expansion(8, Term).
```

<!--
Term = eight.
-->

```logtalk
desc_protected::test_goal_expansion(write(Term), EGoal).
```

<!--
EGoal = write_term(Term, [quoted(true)]).
-->

```logtalk
desc_private::test_term_expansion(8, Term).
```

<!--
Term = 8.
-->

```logtalk
desc_private::test_goal_expansion(write(Term), EGoal).
```

<!--
EGoal = write(Term).
-->

Simple tests of hook objects:

```logtalk
cooked << (aa, bb, cc).
```

<!--
true.
-->

```logtalk
cooked << (ha, hb, hc).
```

<!--
true.
-->

```logtalk
cooked << p.
```

<!--
true.
-->

```logtalk
piped<<a(key-value).
```

<!--
true.
-->

```logtalk
piped<<b(key-value).
```

<!--
true.
-->

```logtalk
piped<<c(key-value).
```

<!--
true.
-->
