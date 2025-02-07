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
	Facade

Description:
	"Provide a unified interface to a set of interfaces in a subsystem.
	Facade defines a higher-level interface that makes the subsystem
	easier to use."

This pattern can be used with both classes and prototypes. As a facade
object delegates the simplified client requests to the objects that
represent the abstracted subsystems, these subsystems can be implemented
using prototypes, classes, or a mix of both.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the design pattern sample implementation:

```logtalk
logtalk_load(design_patterns('structural/facade/loader')).
```

Use the simplified interface provided by the facade to start the computer:

```logtalk
computer_facade(cpu, memory, ssd)::start.
```

<!--
Freezing processor.
Loading from 0x00 data: Some data from sector 100 with size 1024
Jumping to: 0x00
Executing.

true.
-->

Or, in alternative:

```logtalk
computer_facade::start.
```

<!--
Freezing processor.
Loading from 0x00 data: Some data from sector 100 with size 1024
Jumping to: 0x00
Executing.

true.
-->
