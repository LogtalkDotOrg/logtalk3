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
	Bridge

Description:
	"Decouple an abstraction from its implementation so that the two
	can vary independently."

This pattern can be used with both classes and prototypes.

In this sample implementation, we use classes for representing the
abstraction part of the pattern and prototypes for the implementation
part. As discussed in the pattern description, the instances in the
abstraction hierarchy use object composition to refer to and delegate
operations to the implementation hierarchy objects.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the design pattern sample implementation:

```logtalk
logtalk_load(design_patterns('structural/bridge/loader')).
```

Draw the static instance of circle:

```logtalk
a_circle::draw.
```

<!--
API2: circle at 1.7:11.3 with radius 2.1

true.
-->

Create a couple of dynamic circles, resize them by 25%, and draw them:

```logtalk
circle::new(Circle1, [drawing_api(drawing_api_1), x(1.0), y(2.0), radius(3.0)]),
Circle1::(resize(25), draw).
```

<!--
API1: circle at 1.0:2.0 with radius 3.75

Circle1 = o1.
-->

```logtalk
circle::new(Circle2, [drawing_api(drawing_api_2), x(5.0), y(7.0), radius(11.0)]),
Circle2::(resize(25), draw).
```

<!--
API2: circle at 5.0:7.0 with radius 13.75

Circle2 = o2.
-->
