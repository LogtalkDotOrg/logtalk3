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

# threads - blackboard

This folder contains a simple multi-threading example illustrating how
to use the Logtalk built-in predicates `threaded_wait/1` and `threaded_notify/1`
for synchronizing threads using shared resources. The example consists of
two persons, a student and a teacher, sharing a blackboard chalk and eraser.

This example should preferably be run from the top-level instead of as
a notebook to observe the threads output.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the example and the required library files:

```logtalk
logtalk_load(blackboard(loader)).
```

Start the producer and the consumer, each one running in its own thread:

```logtalk
threaded_ignore(student::run(10)), threaded_ignore(teacher::run(4)).
```

<!--
teacher is writing...
student is writing...
student is writing...
student is writing...
student is writing...
teacher is writing...
teacher is writing...
teacher is writing...
student is writing...
student is writing...
student is writing...
student is writing...
student is writing...
student is writing...
-->
