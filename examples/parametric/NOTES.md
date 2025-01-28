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

# parametric

This folder contains some examples of parametric objects and categories.
Object and category parameters are logical variables that are shared with
all the entity predicates. This example illustrates accessing parameters
using the `parameter/1` and `this/1` built-in methods. Parameters can also
be accessed using _parameter variables_ as illustrated by the `parvars`
example.

Start by loading the example:

```logtalk
logtalk_load(parametric(loader)).
```

Some queries using the list parametric object:

```logtalk
%%table
[1, 2, 3]::member(X).
```

<!--
X = 1 ;
X = 2 ;
X = 3 ;
false.
```

```logtalk
[1, 2, 3]::last(X).
```

<!--
X = 3.
-->

```logtalk
[1, 2, 3]::nextto(2, Y).
```

<!--
Y = 3.
-->

```logtalk
[]::member(X).
```

<!--
false.
-->

Some queries using the time and date parametric objects:

```logtalk
date(Year, Month, Day)::today.
```

<!--
Year = 2000, Month = 8, Day = 15.
-->

```logtalk
date(Year, _, _)::today, \+ date(Year, _, _)::leap_year.
```

<!--
Year = 2002.
-->

```logtalk
time(Hours, Mins, Secs)::now.
```

<!--
Hours = 13, Mins = 52, Secs = 42.
-->

```logtalk
rectangle(W, H, X, Y)::init, rectangle(W, H, X, Y)::move(3, 4, NR), NR::position(X2, Y2).
```

<!--
W = 2, H = 1, X = 0, Y = 0, NR = rectangle(2, 1, 3, 4), X2 = 3, Y2 = 4.
-->

Some queries with parametric objects that define "setter"
methods that return updated object identifiers:

```logtalk
person(sally, 20)::grow_older(NewId).
```

<!--
NewId = person(sally, 21).
-->

```logtalk
employee(sally, 21, 1200)::give_raise(250, NewId).
```

<!--
NewId = employee(sally, 21, 1450).
-->

Some queries with parametric categories:

```logtalk
speech(winter, wedding)::advice.
```

<!--
Clothes: [pants, sleeves, heavy]
Speech:  [happy, jokes]

true.
-->
