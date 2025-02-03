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

# inheritance

This folder contains examples of public, protected, and private inheritance,
for both prototype-based and class-based hierarchies.

This example defines a category named `predicates` which specifies three 
predicates, one public, one protected, and one private. This category is 
imported by the root objects: `parent` for the prototype hierarchy and 
`root` for the class hierarchy. Each root object have a set of three 
descendants, each one using one of the inheritance types.

The two object hierarchies are organized as follows:

```text
parent
	prototype1				% public inheritance
		descendant1
	prototype2				% protected inheritance
		descendant2
	prototype3				% private inheritance
		descendant3

root
	subclass1				% public inheritance
		instance1
	subclass2				% protected inheritance
		instance2
	subclass3				% private inheritance
		instance3
```

A second category named `interface`, imported by all objects except the 
sub-class instances, allows us to query the objects about their interfaces.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the example:

```logtalk
logtalk_load(inheritance(loader)).
```

`parent` interface:

```logtalk
parent::interface.
```

<!--
public/0 - public
protected/0 - protected
private/0 - private

true.
-->

`prototype1` extends `public::parent`:

```logtalk
prototype1::interface.
```

<!--
public/0 - public
protected/0 - protected

true.
-->

`prototype2` extends `protected::parent`:

```logtalk
prototype2::interface.
```

<!--
public/0 - protected
protected/0 - protected

true.
-->

The `prototype3` prototype extends `private::parent`:

```logtalk
prototype3::interface.
```

<!--
public/0 - private
protected/0 - private

true.
-->

The `descendant1` prototype extends `public::prototype1`:

```logtalk
descendant1::interface.
```

<!--
public/0 - public
protected/0 - protected

true.
-->

The `descendant2` prototype extends `public::prototype2`:

```logtalk
descendant2::interface.
```

<!--
public/0 - protected
protected/0 - protected

true.
-->

The `descendant3` prototype extends `public::prototype3`

```logtalk
descendant3::interface.
```

<!--
true.
-->

Object (root of the inheritance graph) interface:

```logtalk
root::interface.
```

<!--
public/0 - public
protected/0 - protected
private/0 - private

true.
-->

`instance1` instantiates `subclass1` that specializes `public::root`:

```logtalk
instance1::interface.
```

<!--
protected/0 - protected
public/0 - public

true.
-->

`instance2` instantiates `subclass2` that specializes `protected::root`:

```logtalk
instance2::interface.
```

<!--
protected/0 - protected
public/0 - protected

true.
-->

`instance3` instantiates `subclass3` that specializes `private::root`:

```logtalk
instance3::interface.
```

<!--
true.
-->
