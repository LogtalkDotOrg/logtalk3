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

# logs

This folder contains an example of using a category to define a simple 
logging support for objects. This example illustrates how to define in 
a category a set of predicates that handle a dynamic predicate in the 
context of "this".

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the example:

```logtalk
logtalk_load(logs(loader)).
```

The object log is automatically initialized when the object is loaded:

```logtalk
object::print_log.
```

<!--
2008/7/17-18:15:38 - start

true.
-->

Add a new entry to the object log:

```logtalk
object::add_log_entry('something interesting happens').
```

<!--
true.
-->

Check current object log:

```logtalk
object::print_log.
```

<!--
2008/7/17-18:15:38 - start
2008/7/17-18:18:10 - something interesting happens

true.
-->

In alternative, enumerate all log entries using backtracking:

```logtalk
%%table
object::log_entry(Date, Entry).
```

<!--
Date = 2008/7/17-8:15:38, Entry = start ;
Date = 2008/7/17-8:18:0, Entry = 'something interesting happens'.
-->
