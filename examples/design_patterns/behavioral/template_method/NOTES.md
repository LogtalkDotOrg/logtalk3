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
	Template Method

Description:
	"Define the skeleton of an algorithm in an operation, deferring
	some steps to subclasses. Template Method lets subclasses redefine
	certain steps of an algorithm without changing the algorithm's
	structure."

This pattern can be used with both classes and prototypes.

The key for implementing this pattern is the *message to self* control
construct, `(::)/1`. It allows a predicate implementing an algorithm to
call the predicates implementing the algorithm steps defined in descendant
objects. It also allows easy definition of default definitions for the
algorithm steps that can be inherited, redefined, or specialized in the
descendant objects. The predicates implementing the algorithm steps are
often declared as protected.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the design pattern sample implementation:

```logtalk
logtalk_load(design_patterns('behavioral/template_method/loader')).
```

Print the steps for a car trip:

```logtalk
travel_by_car::journey.
```

<!--
Rent a car
Put book, pajamas, and toothbrush in luggage
Drive car to destination
Put book and pajamas in nightstand and toothbrush in bathroom

true.
-->

Print the steps for a airplane trip:

```logtalk
travel_by_airplane::journey.
```

<!--
Make a flight reservation
Put book, pajamas, and toothbrush in luggage
Check luggage weight against airline policy
Go to the airport and board the airplane
Put book and pajamas in nightstand and toothbrush in bathroom

true.
-->
