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

# self_vs_super

This example illustrates the differences between a message to *self* and
a *super* call when calling an inherited meta-predicate. See the comments
in the code for details.

For an in-depth discussion of these concepts, see the corresponding
Handbook glossary entries, the section on "Predicates", and the
reference pages on the the execution-context built-in methods.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the example:

```logtalk
logtalk_load(self_vs_super(loader)).
```

Illustrate the differences between sending a message to _self_ and
making a _super_ call to call an inherited meta-predicate by using
a `foo/1` predicate defined in `user`:

```logtalk
foo(X).
```

<!--
X = 1 ? ;
X = 2 ? ;
X = 3.
-->

The _super_ calls preserve _sender_ and therefore the `foo/1` predicate
is called by the meta-predicate in the context of `user`:

```logtalk
proto::meta_super(foo, X).
```

<!--
Execution context for the parent object meta/2 meta-predicate:
  self: proto
  this: parent
  sender: user

X = 1 ? ;
X = 2 ? ;
X = 3.
-->

Messages to _self_ reset _sender_ and therefore the `foo/1` predicate
is called in the context of `proto`, hence the existence error:

```logtalk
catch(proto::meta_self(foo, X), Error, true).
```

<!--
Execution context for the parent object meta/2 meta-predicate:
  self: proto
  this: parent
  sender: proto

Error = error(existence_error(procedure,foo/1),logtalk(call(foo(_307)),c(proto,proto,r(user,proto,c(user,user,r(user,proto,[],[])),[])))).
-->
