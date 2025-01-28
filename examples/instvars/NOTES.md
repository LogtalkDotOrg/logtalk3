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

# instvars

This folder contains an example that shows how to implement instance
variables as defined in class-based object-oriented programming languages.

This example defines a root class, `root`, and three instances, `instance1`, 
`instance2`, and `instance3`. The root class defines an instance variable 
(using a dynamic predicate) and the corresponding setter and getter methods.
The root class is used to store a default value for the instance variable.

Start by loading the example:

```logtalk
logtalk_load(instvars(loader)).
```

Get the value of the instance variable for each instance (the default value,
stored in the instances class, is returned):

```logtalk
instance1::ivar(Value1), instance2::ivar(Value2), instance3::ivar(Value3).
```

<!--
Value1 = 0, Value2 = 0, Value3 = 0.
-->

Change the value of the instance variable for `instance1`:

```logtalk
instance1::set_ivar(1).
```

Get the value of the instance variable for each instance:

```logtalk
instance1::ivar(Value1), instance2::ivar(Value2), instance3::ivar(Value3).
```

<!--
Value1 = 1, Value2 = 0, Value3 = 0.
-->
