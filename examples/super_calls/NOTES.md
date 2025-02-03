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

# super_calls

This simple programming example illustrates that "super" calls (the (^^)/1
control construct) preserve the value of "self" when calling an inherited
definition. This allows any `::/1` goal in the called inherited definition
to work as expected in the correct context. This example also illustrates
that "super" calls require dynamic binding when the called predicate is
dynamic.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the example:

```logtalk
logtalk_load(super_calls(loader)).
```

The predicate `get_local/1` calls the `local/1` predicate in _self_,
i.e. in the object that receives the `get_local/1` message:

```logtalk
parent::get_local(Local).
```

<!--
Local = parent.
-->

```logtalk
prototype::get_local(Local).
```

<!--
Local = prototype.
-->

The `prototype::correct/1` predicate makes a _super_ call, which preserves
_self_, to the inherited `get_local/1` predicate:

```logtalk
prototype::correct(Local).
```

<!--
Local = prototype.
-->

The `prototype::wrong/1` predicate sends a message to the parent, which
(necessarily) resets _self_ to the message receiver:

```logtalk
prototype::wrong(Local).
```

<!--
Local = parent.
-->

The _super_ calls force dynamic binding when the called predicate is dynamic:

```logtalk
bottom::value(Value).
```

<!--
Value = parent.
-->

```logtalk
middle::assertz(d(middle)).
```

<!--
true.
-->

```logtalk
bottom::value(Value).
```

<!--
Value = middle.
-->

```logtalk
middle::retractall(d(_)).
```

<!--
true.
-->

```logtalk
bottom::value(Value).
```

<!--
Value = parent.
-->
