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

# self_vs_this

This example illustrates the differences between *self* and *this* and
also the execution-context built-in methods that allow to access the
values of *self*, *this*, and *sender*.

For an in-depth discussion of these concepts, see the corresponding
Handbook glossary entries, the section on "Predicates", and the
reference pages on the the execution-context built-in methods.

For an additional example using parametric objects, see the `sicstus`
example.

Start by loading the example:

```logtalk
logtalk_load(self_vs_this(loader)).
```

Illustrate the differences between _self_ and _this_:

```logtalk
aircraft::context.
```

<!--
Running context/0 predicate definition in object "aircraft":
  self: aircraft
  this: aircraft
  sender: user

Running context/0 predicate definition in object "transport":
  self: aircraft
  this: transport
  sender: user

Running context/0 predicate definition in object "thing":
  self: aircraft
  this: thing
  sender: user

true.
-->
