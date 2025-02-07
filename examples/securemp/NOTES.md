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

# securemp

This folder contains a set of source files for testing Logtalk secure
implementation of meta-predicates. For full details on this example 
and on the safety rules used by Logtalk when compiling and executing
meta-predicates, please see the paper:

```text
@inproceedings{pmoura09b,
	author = {Paulo Moura},
	title = "{Secure Implementation of Meta-predicates}",
	booktitle = {Proceedings of the Eleventh International Symposium on Practical Aspects of Declarative Languages},
	editor = "Andy Gill and Terrance Swift",
	series = "Lecture Notes in Computer Science",
	volume = "5418",
	month = jan,
	year = {2009},
	pages = {269--283},
	publisher = "Springer-Verlag",
	address = "Berlin Heidelberg",
}
```

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Simply load the example and check the compilation and runtime error
messages:

```logtalk
logtalk_load(securemp(loader)).
```
