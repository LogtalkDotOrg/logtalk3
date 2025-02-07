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

# patching

This folder contains an example that shows how to use a complementing
category to patch broken object predicate declarations and definitions 
(without modifying its source code), thus providing functionality similar
to Objective-C categories.

The complemented objects must be compiled with the flag `complements` set
to `allow` (its default value is usually `deny`). This solution was adapted
to improve performance of applications that don't make use complementing
categories and to provide a solution for preventing the use of categories
to break object encapsulation. Note that the `complements` flag can be set
on a per-object basis by using the `set_logtalk_flag/2` directive.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the example:

```logtalk
logtalk_load(patching(loader)).
```

<!--
true.
-->

Find categories that complement objects:

```logtalk
complements_object(Category, Object).
```

<!--
Category = patch, Object = broken.
-->

Verify the patch in the `patch` category for the predicate `is_proper_list/1`
defined in the object `broken`:

```logtalk
broken::is_proper_list([1,2,3]).
```

<!--
true.
-->

```logtalk
broken::is_proper_list(_).
```

<!--
false.
-->

```logtalk
broken::is_proper_list([a,b,c|_]).
```

<!--
false.
-->

Verify the patch in the `patch` category for the wrong scope of the `last/3`
predicate declared in the object `broken`:

```logtalk
broken::last(_, _, _).
```

<!--
    permission_error(access, private_predicate, last/3),
    broken::last(_, _, _),
    user
-->

Verify the patch in the `patch` category for the wrong scope of the `last/3`
predicate declared in the object `broken`:

```logtalk
broken::nextto(2, 3, [1,2,3]).
```

<!--
true.
-->
