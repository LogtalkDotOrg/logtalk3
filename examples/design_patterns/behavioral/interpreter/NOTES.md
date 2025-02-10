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
	Interpreter

Description:
	"Given a language, define a representation for its grammar along
	with an interpreter that uses the representation to interpret
	sentences in the language."

This pattern can be used with both classes and prototypes.

Logtalk support for Definite Clause Grammars (DCGs) allows straightforward
representation of grammars and thus implementation of this pattern. Our
sample code makes use of tabling (to deal with left-recursion in the
original example) and thus can only be run with B-Prolog, SWI-Prolog, XSB,
or YAP backend Prolog compilers.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the design pattern sample implementation:

```logtalk
logtalk_load(design_patterns('behavioral/interpreter/loader')).
```

Parse and evaluate an expression in Reverse Polish notation (RPN):

```logtalk
interpreter::eval("a b + c a - -", [a=5, b=3, c=1], Value).
```

<!--
Value = 12.
-->
