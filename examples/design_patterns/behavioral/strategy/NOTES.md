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
	Strategy

Description:
	"Define a family of algorithms, encapsulate each one, and make them
	interchangeable. Strategy lets the algorithm vary independently of
	from clients that use it."

This pattern can be used with both classes and prototypes.

The sample implementation uses prototypes and dynamic object predicates
to allow the strategy to be changed dynamically at runtime.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the design pattern sample implementation:

```logtalk
logtalk_load(design_patterns('behavioral/strategy/loader')).
```

Create a first customer:

```logtalk
customer::new(first, normal_strategy).
```

<!--
true.
-->

Normal billing:

```logtalk
first::add(1.0, 1).
```

<!--
true.
-->

Start Happy Hour:

```logtalk
first::(set_strategy(happy_hour_strategy), add(1.0, 2)).
```

<!--
true.
-->

Create a second customer:

```logtalk
customer::new(second, happy_hour_strategy), second::add(0.8, 1).
```

<!--
true.
-->

First customer pays:

```logtalk
first::print_bill.
```

<!--
Total due: 2.0

true.
-->

End Happy Hour:

```logtalk
second::(set_strategy(normal_strategy), add(1.3, 2), add(2.5, 1), print_bill).
```

<!--
Total due: 5.5

true.
-->
