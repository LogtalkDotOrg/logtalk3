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

Design pattern:
	Threading State

Description:
	Definite Clause Grammars (DCGs) can be used to thread state
	whenever we have a sequence of calls where each call takes
	an input state and an output state as arguments with the
	output state of each call being passed to the next call as
	its input state. DCGs notation allows abstracting the state
	arguments and thus simplifying the code.

The sample implementation converts a floating-point number into an
integer number using a sequence of operations represented using
Definite Clause Grammar rules.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the design pattern sample implementation:

```logtalk
logtalk_load(design_patterns('logic/threading_state/loader')).
```

Convert a float into an integer:

```logtalk
states::convert(12.34, Integer).
```

<!--
Integer = 76.
-->

Confirm the result:

```logtalk
Integer is round((12.34 * 12.34) /2.0).
```

<!--
Integer = 76.
-->
