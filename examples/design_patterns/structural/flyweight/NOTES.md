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

Design pattern:
	Flyweight

Description:
	"Use sharing to support a large number of fine-grained objects
	efficiently."

This pattern can be used with both classes and prototypes.

Flyweight objects store invariant state that can be shared between several
objects, thus allowing more efficient representation of large number of
objects that share that state. This allow those objects to only store the
variant parts of the state.

Note that any object in Logtalk, independently of the role it plays (e.g.
prototype or class), can also act as a flyweight object for its descendants.
Static predicates that should be shared can simply be defined in the flyweight
object and inherited by its descendants. Any dynamic predicates can be handled
(i.e., asserted, accessed, and retracted) in either the flyweight object, if
shared, or in *self*, if it may differ for each descendant.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the design pattern sample implementation:

```logtalk
logtalk_load(design_patterns('structural/flyweight/loader')).
```

Add cheese brands and respective cost to the flyweight object using any of
the shops:

```logtalk
shop1::(stock_cheese(white, 1.25), stock_cheese(blue, 3.75)).
```

<!--
true.
-->

All shops now have white and blue cheese at the same cost:

```logtalk
shop2::cheese(Brand, Cost).
```

<!--
Brand = white,
Cost = 1.25 ;
Brand = blue,
Cost = 3.75

true.
-->

Both shops can sell cheese but the units sold are stored per-shop:

```logtalk
shop1::sell_cheese(blue, 3), shop2::sell_cheese(blue, 8).
```

<!--
true.
-->

Check cheese units sold and income for each shop:

```logtalk
shop1::(total_units_sold(Units), total_income(Income)).
```

<!--
Total = 3, Income = 11.25.
-->

```logtalk
shop2::(total_units_sold(Units), total_income(Income)).
```

<!--
Total = 8, Income = 30.0.
-->
