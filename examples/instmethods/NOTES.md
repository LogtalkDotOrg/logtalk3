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

# instmethods

This folder contains an example of instance defined methods. When using 
classes and instances, methods must be declared in a class but the method 
definitions may be stored in the instances, either overriding or specializing 
the class definitions.

This example defines the following objects:

- `root`  
	root class defining a method named `method/0`

- `instance1`  
	simple instance of root inheriting `method/0`

- `instance2`  
	instance of root overriding the inherited method `method/0`

- `instance3`  
	instance of root specializing the inherited method `method/0`

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the example:

```logtalk
logtalk_load(instmethods(loader)).
```

The `instance1` definition for method `method/0` is found in its class:

```logtalk
instance1::method.
```

<!--
This is the default definition for the method, stored in class root.

true.
-->

The `instance2` instance overrides the definition of method `method/0`:

```logtalk
instance2::method.
```

<!--
This is an overriding definition stored in the instance2 instance itself.

true.
-->

The `instance3` instance specializes the definition of method `method/0`:

```logtalk
instance3::method.
```

<!--
This is a specializing definition stored in the instance3 instance itself.
It makes a super call to execute the default definition:

This is the default definition for the method, stored in class root.

true.
-->
