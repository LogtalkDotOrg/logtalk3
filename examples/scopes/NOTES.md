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

# scopes

This simple programming example illustrates predicate scope semantics.
See the comments in the `scopes.lgt` file for detailed explanations.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the example:

```logtalk
logtalk_load(scopes(loader)).
```

We can always send a message for a public predicate:

```logtalk
prototype::foo(Foo).
```

<!--
Foo = 1.
-->

But we cannot send messages for protected predicates:

```logtalk
prototype::bar(_).
```

<!--
error(permission_error(access,protected_predicate,bar/1),logtalk(prototype::bar(_),c(user,user,r(user,prototype,[],[]))))
-->

Or for private predicates:

```logtalk
prototype::baz(_).
```

<!--
uncaught exception: error(permission_error(access,private_predicate,baz/1),logtalk(prototype::baz(_),c(user,user,r(user,prototype,[],[]))))
-->

Same for local predicates not declared by a scope directive; note the
different exception term compared with the two previous queries:

```logtalk
prototype::local(_).
```

<!--
error(existence_error(predicate_declaration,local/1),logtalk(prototype::local(_),c(user,user,r(user,prototype,[],[]))))
-->

From the object holding the scope directives, we can always access any
predicate redefinition in the descendants (note that the `p_foo/1`, `p_bar/1`,
and `p_baz/1` predicates are declared public, defined in the _prototype_
object, and inherited by the _descendant_ object):

```logtalk
descendant::p_foo(Foo).
```

<!--
Foo = 2.
-->

```logtalk
descendant::p_bar(Bar).
```

<!--
Bar = 2.
-->

```logtalk
descendant::p_baz(Baz).
```

<!--
Baz = 2.
-->

From descendant objects, we can access inherited predicate definitions for
public and protected predicates:

```logtalk
descendant::d_foo(Foo).
```

<!--
Foo = 1.
-->

```logtalk
descendant::d_bar(Bar).
```

<!--
Bar = 1.
-->
