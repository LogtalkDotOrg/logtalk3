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

# msglog

This example illustrates how to use Logtalk event-driven programming support
for implementing a simple message logger for messages sent from the command-
line (i.e., from the pseudo-object `user`). If you need more than one message 
logger, just create a new prototype as an extension of the object `msglog`.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the example and the required library files:

```logtalk
logtalk_load(msglog(loader)).
```

Start recording user messages:

```logtalk
msglog::record.
```

<!--
true.
-->

Send some messages:

```logtalk
%%table
list::member(X, [1, 2, 3]).
```

<!--
X = 1 ;
X = 2 ;
X = 3 ;
false.
-->

```logtalk
character::is_alpha(p).
```

<!--
true.
-->

```logtalk
%%table
integer::between(1, 4, N).
```

<!--
N = 1 ;
N = 2 ;
N = 3 ;
N = 4 ;
false.
-->

Stop recording and print message log:

```logtalk
msglog::(stop, print).
```

<!--
list::member(X, [1, 2, 3]).
character::is_alpha(p).
integer::between(1, 4, N).

true.
-->
