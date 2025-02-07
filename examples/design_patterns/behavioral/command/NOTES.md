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

Design pattern:
	Command

Description:
	"Encapsulate a request as an object, thereby letting you parameterize
	clients with different requests, queue or log requests, and support
	undoable operations."

This pattern can be used with both classes and prototypes.

The simple way to implement this design pattern in Logtalk is to represent
commands as parametric object identifiers with the parametric objects
implementing the command protocol.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the design pattern sample implementation:

```logtalk
logtalk_load(design_patterns('behavioral/command/loader')).
```

First, test the basic functionality for light:

```logtalk
light(l1)::turn_on.
```

<!--
The light(l1) is on
true.
-->

Commands are represented using parametric object identifiers:

```logtalk
flip_up_command(light(l1))::execute.
```

<!--
The light(l1) is on
true.
-->

Use the client/invoker object to store_and_execute commands:

```logtalk
switch::(
		store_and_execute(flip_up_command(light(kitchen))),
		store_and_execute(flip_up_command(coffee_maker))
	).
```

<!--
The light(kitchen) is on
The coffee_maker is on
true.
-->

Show the commands history:

```logtalk
switch::history.
```

<!--
flip_up_command(light(kitchen))
flip_up_command(coffee_maker)
true.
-->

Reply the commands:

```logtalk
switch::reply_history.
```

<!--
The light(kitchen) is on
The coffee_maker is on
true.
-->
