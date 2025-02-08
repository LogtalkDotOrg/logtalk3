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

# dynpred

This folder contains examples of using the built-in database handling
methods with object and categories. Two object hierarchies are provided,
one prototype-based, and the other class-based, in order to illustrate
the differences between asserting predicates in a class and in a prototype:

The following objects are defined:

- `root`  
	root of the prototype hierarchy; declares and defines a public,
	dynamic predicate
- `descendant`  
	simple prototype extending the root prototype

- `class`  
	root of the class hierarchy; declares and defines a public predicate
- `metaclass`  
	class metaclass
- `instance`  
	simple instance of class class

- `prototype`  
	simple prototype used to illustrate how the scope of asserted 
	predicates depends on the target object (this, self, or an explicit 
	object)

In addition, the file `categories.lgt` illustrates how to define category
predicates that handle dynamic predicates in the context of _this_ and in
the context of _self_.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the example:

```logtalk
logtalk_load(dynpred(loader)).
```

Sending to descendant the message `p/1`, returns the definition in root:

```logtalk
descendant::p(Value).
```

<!--
Value = root.
-->

Asserting a local definition for `p/1` in descendant overrides the inherited 
definition:

```logtalk
descendant::(assertz(p(descendant)), p(Value)).
```

<!--
Value = descendant.
-->

If we retract the local definition, again the definition inherited from root
will be used:

```logtalk
descendant::(retractall(p(_)), p(Value)).
```

<!--
Value = root.
-->

The object `class` does not understand the message `p1/1` (the predicate is
declared only for the `class` descendant instances):

```logtalk
catch(class::p1(X), Error, true).
```

<!--
Error = error(existence_error(predicate_declaration,p1/1),logtalk(class::p1(_948),c(user,user,r(user,class,[],[])))).
-->

The same message is valid for the `class` instances:

```logtalk
instance::p1(X).
```

<!--
X = class.
-->

If we assert a clause for a new predicate, `p2/1`, in `class`
(a side-effect being a dynamic declaration of the predicate):

```logtalk
class::assertz(p2(class)).
```

<!--
true.
-->

The new predicate, like p1/1, is not available for `class`:

```logtalk
catch(class::p2(Value), Error, true).
```

<!--
catch(instance::p2(_), Error, true).
-->

But is available for the `class` instances, the same way as `p1/1`:

```logtalk
instance::p2(X).
```

<!--
X = class.
-->

If we change our mind and abolish the new predicate:

```logtalk
class::abolish(p2/1).
```

<!--
true.
-->

Then `instance` will no longer accept the `p2/1` message:

```logtalk
catch(instance::p2(_), Error, true).
```

<!--
Error = error(existence_error(predicate_declaration,p2/1),logtalk(instance::p2(_1718),c(user,user,r(user,instance,[],[])))).
-->

Using a prototype, assert three new predicates (the method `object_assert/0`
asserts the predicate `public_predicate/0` from outside the prototype; the 
method `self_assert/0` asserts the predicate `protected_predicate/0` in _self_; 
the method `this_assert/0` asserts the predicate `private_predicate/0` in _this_):

```logtalk
prototype::(object_assert, self_assert, this_assert).
```

<!--
true
-->

Check the resulting scope of each predicate:

```logtalk
prototype::dynamic_predicates.
```

<!--
public_predicate/0 - public
protected_predicate/0 - protected
private_predicate/0 - private
true.
-->
