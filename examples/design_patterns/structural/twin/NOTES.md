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

Design pattern:
	Twin

Description:
	This design pattern provides an alternative for the use of
	multiple inheritance, based on defining two (or more) tightly
	coupled instances of the parent class whose protocol we want
	to inherit, hence the pattern name. Each instance refers to the
	other and is responsible for its parent protocol, forwarding to
	its twin other messages.

This pattern can be used with both classes and prototypes.

This pattern is not described in the GoF book. See e.g. the following
Wikipedia page for details and references:

https://en.wikipedia.org/wiki/Twin_pattern

Logtalk supports multiple inheritance (and also multiple instantiation).
But its support for categories often allows better solutions. This pattern
provides a third alternative that can be preferable in some cases. When
compared with a category-based solution, a key difference is that, with
categories, each category protocol adds to the protocol of the object
(importing the categories) while with this pattern each object must use
message forwarding to its twin object (and thus one level of indirection).
Note that this pattern implies a circular reference between the twin
objects, which prevents some static binding optimizations.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the design pattern sample implementation:

```logtalk
logtalk_load(design_patterns('structural/twin/loader')).
```

Each object can answer its messages and the messages for its twin object,
thanks to message forwarding:

```logtalk
a_date::year(Year).
```

<!--
Year = 2018.
-->

```logtalk
a_date::hour(Hour).
```

<!--
Hour = 11.
-->

```logtalk
a_time::minutes(Minutes).
```

<!--
Minutes = 27.
-->

```logtalk
a_time::day(Day).
```

<!--
Day = 13.
-->

We can also create twins dynamically:

```logtalk
time::new(twin_time, twin_date, [hour(12),minutes(37),seconds(17)]).
```

<!--
true.
-->

```logtalk
date::new(twin_date, twin_time, [year(2003),month(9),day(23)]).
```

<!--
true.
-->

```logtalk
twin_time::year(Year).
```

<!--
Year = 2003.
-->

```logtalk
twin_date::seconds(Seconds).
```

<!--
Seconds = 17.
-->
