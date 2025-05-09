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

Design pattern:
	Singleton

Description:
	"Ensure a class has one instance, and provide a global point
	of access to it."

This pattern can be used with both classes and prototypes.

Given Logtalk supports for prototypes, implementing this pattern is
trivial. Instead of using a class and write code to ensure a single
class instance, we can simply use a prototype, which is its own global
point of access. As applications can use a mix of prototypes and
classes, there is nothing to be gained in implementing this pattern
using classes.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the design pattern sample implementation:

```logtalk
logtalk_load(design_patterns('creational/singleton/loader')).
```

As the singleton is implemented using a prototype, we access the
singleton predicates directly:

```logtalk
singleton::foo(X).
```

<!--
X = bar.
-->
