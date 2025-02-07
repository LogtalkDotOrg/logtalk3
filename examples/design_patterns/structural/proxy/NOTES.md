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
	Proxy

Description:
	"Provide a surrogate or placeholder for another object to control
	access to it."

This pattern can be used with both classes and prototypes.

Proxy objects are simple to implement. We can have the proxy object
implementing a protocol, shared with the real object, that specifies
only the predicates that the proxy object redefines. This allows the
using the `forward/1` handler to delegate all other messages to the
real object. When the proxy object is expected to implement the full
protocol of the real object, an alternative solution is for the proxy
object to inherit privately from the real object and overriding (or
specializing) only those predicates that motivated the use of a proxy.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the design pattern sample implementation:

```logtalk
logtalk_load(design_patterns('structural/proxy/loader')).
```

First, lets create a old enough driver, no other than Mr. Magoo itself
(pedestrians be aware!):

```logtalk
driver::new(magoo, 'Mr. Magoo', 87).
```

<!--
true.
-->

In some of the cartoons, Mr. Magoo appears to be driving a 1929 Packard
Model 645 touring car; we can define the car as a static object in the
source file or create it dynamically:

```logtalk
create_object(packard_645, [extends(car)], [], []).
```

<!--
true.
-->

Then we check, using the proxy object, that the driver can actually
drive the car:

```logtalk
car_proxy(packard_645, magoo)::drive.
```

<!--
Car has been driven!

true.
-->

Now, lets create a driver that is just too young, one of the children
that Mr. Magoo sometimes babysits:

```logtalk
driver::new(wheeler, 'Wheeler', 7).
```

<!--
true.
-->

Then we check, using the proxy object, that the child is effectively not
allowed to drive the car:

```logtalk
car_proxy(packard_645, wheeler)::drive.
```

<!--
Sorry, Wheeler is too young to drive!

true.
-->
