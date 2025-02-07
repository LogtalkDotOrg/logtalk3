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

# self_messages

This simple programming example illustrates the semantics of messages
to _self_ (using the `::/1` control construct), i.e. to the object that
received the message being processed.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the example:

```logtalk
logtalk_load(self_messages(loader)).
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

The `get_default/1` predicate also calls the `default/1` predicate in
_self_ but predicate is only defined in the _parent_ object; its
definition is therefore inherited by the _prototype_ object:

```logtalk
parent::get_default(Default).
```

<!--
Default = parent.
-->

```logtalk
prototype::get_default(Local).
```

<!--
Default = parent.
-->

The `get_undefined/1` predicate also calls the `undefined/1` predicate in
_self_ but the predicate is only declared and not defined in either
object making the messages fail as per closed world assumption:

```logtalk
parent::get_undefined(_).
```

<!--
false.
-->

```logtalk
prototype::get_undefined(_).
```

<!--
false.
-->
