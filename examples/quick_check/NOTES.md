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

# quick_check

For a description of this example, please see the comments in the 
`quick_check.lgt` and `tests.lgt` source files.

For more information on QuickCheck-based testing see the documentation of
the `lgtunit` tool.

See also the `testing` and ``tests_dsl` examples.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the sample properties used to use QuickCheck interactively
at the top-level interpreter:

```logtalk
logtalk_load(quick_check(loader)).
```

Ensure that QuickCheck warnings are printed:

```logtalk
set_logtalk_flag(report, warnings).
```

Check that reversing a list twice gives back the same list:

```logtalk
lgtunit::quick_check(reverse_2_twice_prop(+list)).
```

<!--
% 100 random tests passed
true.
-->

Check the `same_length_2_prop/1` property by running the default number of
random tests:

```logtalk
lgtunit::quick_check(same_length_2_prop(+list)).
```

<!--
% 100 random tests passed
true.
-->

Check the `same_length_3_prop/1` property by running 25 random tests:

```logtalk
lgtunit::quick_check(same_length_3_prop(+list), [n(25)]).
```

<!--
% 25 random tests passed
true.
-->

Try a broken property, `broken_nth1_3_prop/1`, to check returning of a
counter-example that falsifies the property:

```logtalk
lgtunit::quick_check(broken_nth1_3_prop(+list), [n(25)]).
```

<!--
*     quick check test failure (at test 1 after 0 shrinks):
*       broken_nth1_3_prop([])
false.
-->

The alternative `lgtunit::quick_check/3` predicate returns results in
reified form:

```logtalk
lgtunit::quick_check(same_length_2_prop(+list), Result, []).
```

<!--
Result = passed.
-->

```logtalk
lgtunit::quick_check(broken_nth1_3_prop(+list), Result, [n(250)]).
```

<!--
Result = failed(broken_nth1_3_prop([])).
-->

Defining a predicate specifying a property of the predicate that we want
to test is not always necessary; for example assuming that the predicate
`length/2` is defined in `user`:

```logtalk
lgtunit::quick_check(length(+list, -integer)).
```

<!--
% 100 random tests passed
true.
-->

The template of the `quick_check/1-3` predicates can also use the
`(::)/2` and `(<<)/2` control constructs:

```logtalk
lgtunit::quick_check(list::length(+list, -integer)).
```

<!--
% 100 random tests passed
true.
-->

```logtalk
lgtunit::quick_check(list<<length(+list, -integer)).
```

<!--
% 100 random tests passed
true.
-->

When using a backend Prolog system supporting modules, the template
argument of the `quick_check/1-3` predicates can be an explicitly
module-qualified template; for example, assuming a system providing 
a `lists` module defining the de facto standard `length/2` predicate:

```logtalk
use_module(library(lists)).
```

<!--
true.
-->

```logtalk
lgtunit::quick_check(lists:length(+list, -integer)).
```

<!--
% 100 random tests passed
true.
-->
