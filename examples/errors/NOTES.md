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

# errors

This folder contains examples of objects, categories, and protocols containing
errors and code issues that will trigger Logtalk compiler warnings and errors.
The goal of this example is to help the user to get acquainted  to the Logtalk
compiler warning and error reporting. Open the source files in a text editor
to better understand how the compiler deals with common programming errors.

Note that the warnings and errors that you will get while compiling your
source files depend on your compiler flags (defined explicitly as parameters
for the `logtalk_compile/2` or `logtalk_load/2` built-in predicates or by
default in your Prolog adapter file and possibly overridden in your settings
file). See the Handbook section on writing and running applications for a
detailed description of the flags.

The `tutor` tool provides explanations and fix suggestions for compiler
warnings and errors messages. New users are advised to load it at startup
(e.g., from a settings file) for a more friendly experience.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

To augment compiler warnings and errors messages with explanations and
fix suggestions, start by loading the `tutor` tool:

```logtalk
logtalk_load(tutor(loader)).
```

The entities on this example are divided in two sets. The first set 
results on compiler warning messages, while the second set results in
compiler error messages. To load each sets of entities, compile and
load the corresponding loader files.

Illustrate common compiler errors:

```logtalk
logtalk_load(errors(warnings_loader)).
```

Illustrate common compiler warnings:

```logtalk
logtalk_load(errors(errors_loader)).
```
