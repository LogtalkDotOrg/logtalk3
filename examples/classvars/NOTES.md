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

# classvars

This folder contains an example that shows how to implement class variables
as defined in Smalltalk. The name shared instance variables is however much
more accurate. In systems like Logtalk, which enables the use of explicit
metaclasses, true class variables are just the class (as an object) own
instance variables!

This example defines a root class, `root` and three instances, `instance1`,
`instance2`, and `instance3`. The root class defines a shared instance variable
(using a dynamic predicate) and the setter and getter methods which implement
the variable sharing behavior.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the example:

```logtalk
logtalk_load(classvars(loader)).
```

Get the value of the class variable for each instance:

```logtalk
instance1::cv(Value1), instance2::cv(Value2), instance3::cv(Value3).
```

<!--
Value1 = Value2, Value2 = Value3, Value3 = 0.
-->

Change the value of the class variable via instance1:

```logtalk
instance1::set_cv(1).
```

<!--
true.
-->

Get the value of the class variable for the other instances:

```logtalk
instance2::cv(Value2), instance3::cv(Value3).
```

<!--
Value2 = 1, Value3 = 1.
-->
