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

# reflection

Run this example with no other examples loaded at the same time.

This folder contains an example that shows how to implement a reflective
class-based system. There are three main classes:

- `object`  
	root of the inheritance graph
- `class`  
	default metaclass for all instantiable classes
- `abstract_class`  
	default metaclass for all abstract classes

Each class inherit all the methods form the other two classes and from 
itself (without any inheritance loops of course ;-).

You can find more sophisticated versions of these classes in the `roots`
example. If you are not familiar with the concept of metaclass used in
this example, see the `metaclasses` example first.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the example:

```logtalk
logtalk_load(reflection(loader)).
```

Print the (public and protected) interface of each class:

```logtalk
object::print, abstract_class::print, class::print.
```

<!--
Object: object

  interface:
    new/1
    delete/1
    instances/1
    metaclass/0
    abstract_class/0
    strict_instance/0
    print/0

Object: abstract_class

  interface:
    new/1
    delete/1
    instances/1
    metaclass/0
    abstract_class/0
    strict_instance/0
    print/0

Object: class

  interface:
    new/1
    delete/1
    instances/1
    metaclass/0
    abstract_class/0
    strict_instance/0
    print/0

true.
-->

The object `Class` is the metaclass of all classes:

```logtalk
class::instances(Instances), class::metaclass.
```

<!--
Instances = [class,abstract_class,object].
-->

Create an abstract class, check it and print its interface:

```logtalk
abstract_class::new(ac), ac::abstract_class, ac::print.
```

<!--
Object: ac

  interface:
    metaclass/0
    abstract_class/0
    strict_instance/0
    print/0

true.
-->

Try to create an instance of the abstract class:

```logtalk
catch(ac::new(i), Error, true).
```

<!--
Error = error(existence_error(predicate_declaration,new(i)),ac::new(i),user).
-->

Create a new instantiable class and print its interface:

```logtalk
class::new(c), c::print.
```

<!--
Object: c

  interface:
    new/1
    delete/1
    instances/1
    metaclass/0
    abstract_class/0
    strict_instance/0
    print/0

true.
-->

Create an instance of the new class:

```logtalk
c::new(i), c::instances(Instances).
```

<!--
Instances = [i].
-->

Because `c` does not declare any predicates, its instances have no interface:

```logtalk
\+ i::current_predicate(_).
```

<!--
true.
-->

Create an instance of object, root of the inheritance graph, and print its interface:

```logtalk
object::new(j), j::print.
```

<!--
Object: j

  interface:
    strict_instance/0
    print/0

true.
-->

Delete the dynamic objects that we created:

```logtalk
c::delete(i), class::delete(c), abstract_class::delete(ac), object::delete(j).
```

<!--
true.
-->
