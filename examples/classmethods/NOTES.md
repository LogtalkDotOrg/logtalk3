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

# classmethods

This example illustrates how to define "class methods" as found on class-
based object-oriented programming languages such as Java or Objective-C.
Logtalk classes are objects. Therefore, class methods are simply instance
methods defined in the class of the class, i.e. in its meta-class.

This example defines the following objects:

- `circle`  
	class representing common features of geometric circles
	such as radius and position

- `metacircle`  
	meta-class of class circle defining "class methods" for
	creating instances and calculating areas

- `c42`  
	static instance of class `circle`

Start by loading the example:

```logtalk
logtalk_load(classmethods(loader)).
```

Use the _class method_ `area/2` to calculate the are of a circle given its radius:

```logtalk
circle::area(1.0, Area).
```

<!--
Area = 3.14159.
-->

Ask our static instance, `c42`, its area:

```logtalk
c42::area(Area).
```

<!--
Area = 24.6301.
-->


Create a dynamic instance of class circle using the _class method_ `new/4`:

```logtalk
circle::new(1.2, 7.9, 2.0, Circle).
```

<!--
Circle = o1.
-->


Ask the circle `o1` its area:

```logtalk
o1::area(Area).
```

<!--
Area = 4.52389.
-->
