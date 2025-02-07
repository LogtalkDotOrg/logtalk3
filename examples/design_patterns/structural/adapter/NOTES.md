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
	Adaptor

Description:
	"Convert the interface of a class into another interface clients
	expect. Adapter lets classes work together that couldn't otherwise
	because of incompatible interfaces."

This pattern can be used with both classes and prototypes.

This pattern is usually implemented using either multiple inheritance
or using object composition. Both solutions are supported in Logtalk.
We illustrate in our sample implementation a solution that mimics
composition by using parametric objects. We could also have used in
alternative dynamic predicates to hold the same information passed
using parameters.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the design pattern sample implementation:

```logtalk
logtalk_load(design_patterns('structural/adapter/loader')).
```

Connect rechargers to phones.

Lightning charger:

```logtalk
iphone_recharger(iphone)::connect.
```

<!--
Lightning connected
Recharge Started
  Recharge 25%
  Recharge 50%
  Recharge 75%
Recharge Finished

true.
-->

Chain two chargers:

```logtalk
iphone_micro_usb_recharger(iphone)::connect.
```

<!--
MicroUsb connected
Lightning connected
Recharge Started
  Recharge 25%
  Recharge 50%
  Recharge 75%
Recharge Finished

true.
-->

MicroUsb charger:

```logtalk
android_recharger(android)::connect.
```

<!--
MicroUsb connected
Recharge Started
  Recharge 20%
  Recharge 40%
  Recharge 60%
  Recharge 80%
Recharge Finished

true.
-->
