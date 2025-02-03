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

# roots

This folder contains rewritten versions of some objects provided  with
previous, 1.x versions, of Logtalk. They are intended to  help the
conversion of applications from Logtalk 1.x to 2.x and to support most
of the other examples provided with the current Logtalk distribution.

Short description of each example entity:

- `class`  
	default metaclass for all classes
- `classp`  
	protocol of class `class`

- `abstract_class`  
	default metaclass for all abstract classes
- `abstract_classp`  
	protocol of class `abstract_class`

- `object`  
	root class for class-based hierarchies
- `objectp`  
	protocol of class `object`

- `initialization`  
	category defining methods for object initialization

- `proto`  
	root prototype for prototype-based hierarchies
- `protop`  
	protocol for prototype `proto`

- `nil`  
	object used to represent a void reference

Please note that the entities above are just example definitions. There is 
nothing fundamental about any of them; they can and should be replaced by 
definitions better fitted to the requirements of specific applications.

For a simpler version of this example, see the `reflection` example. If
you are not familiar with the concept of metaclass used in this example,
see the `metaclasses` example first.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the example and the required library files:

```logtalk
logtalk_load(roots(loader)).
```

Some queries dealing with instance/class hierarchies:

```logtalk
abstract_class::ancestors(Ancestors).
```

<!--
Ancestors = [class, abstract_class, object].
-->

```logtalk
class::ancestors(Ancestors).
```

<!--
Ancestors = [class, abstract_class, object].
-->

```logtalk
object::ancestors(Ancestors).
```

<!--
Ancestors = [class, abstract_class, object].
-->

```logtalk
class::instances(Instances).
```

<!--
Instances = [object, abstract_class, class].
-->

```logtalk
class::superclass(Super).
```

<!--
Super = abstract_class ;
Super = object ;
false.
-->
