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

# threads - buffer

This folder contains a simple multi-threading example illustrating how
to use the Logtalk built-in `threaded_wait/1` and `threaded_notify/1`
predicates for synchronizing threads writing to and reading from a buffer
that can only contain a fixed number of items at the same time. 

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the example and the required library files:

```logtalk
logtalk_load(buffer(loader)).
```

Start the producer and the consumer, each one running in its own thread,
using a buffer capable of holding at most 7 items, with the consumer being
slower than the producer (float values in seconds must be used):

```logtalk
threaded_ignore(producer(7,1.2)::run(25)), threaded_ignore(consumer(7,3.7)::run(25)).
```

<!--
produced item 0 (1/7 items in the buffer)
consumed item 0 (0/7 items in the buffer)
produced item 1 (1/7 items in the buffer)
produced item 2 (2/7 items in the buffer)
produced item 3 (3/7 items in the buffer)
produced item 4 (4/7 items in the buffer)
consumed item 1 (3/7 items in the buffer)
produced item 5 (4/7 items in the buffer)
produced item 6 (5/7 items in the buffer)
produced item 7 (6/7 items in the buffer)
consumed item 2 (5/7 items in the buffer)
produced item 8 (6/7 items in the buffer)
consumed item 3 (5/7 items in the buffer)
produced item 9 (6/7 items in the buffer)
consumed item 4 (5/7 items in the buffer)
produced item 10 (6/7 items in the buffer)
consumed item 5 (5/7 items in the buffer)
produced item 11 (6/7 items in the buffer)
produced item 12 (7/7 items in the buffer)
consumed item 6 (6/7 items in the buffer)
produced item 13 (7/7 items in the buffer)
consumed item 7 (6/7 items in the buffer)
produced item 14 (7/7 items in the buffer)
consumed item 8 (6/7 items in the buffer)
produced item 15 (7/7 items in the buffer)
consumed item 9 (6/7 items in the buffer)
produced item 16 (7/7 items in the buffer)
consumed item 10 (6/7 items in the buffer)
produced item 17 (7/7 items in the buffer)
consumed item 11 (6/7 items in the buffer)
produced item 18 (7/7 items in the buffer)
consumed item 12 (6/7 items in the buffer)
produced item 19 (7/7 items in the buffer)
consumed item 13 (6/7 items in the buffer)
produced item 20 (7/7 items in the buffer)
consumed item 14 (6/7 items in the buffer)
produced item 21 (7/7 items in the buffer)
consumed item 15 (6/7 items in the buffer)
produced item 22 (7/7 items in the buffer)
consumed item 16 (6/7 items in the buffer)
produced item 23 (7/7 items in the buffer)
consumed item 17 (6/7 items in the buffer)
produced item 24 (7/7 items in the buffer)
consumed item 18 (6/7 items in the buffer)
consumed item 19 (5/7 items in the buffer)
consumed item 20 (4/7 items in the buffer)
consumed item 21 (3/7 items in the buffer)
consumed item 22 (2/7 items in the buffer)
consumed item 23 (1/7 items in the buffer)
consumed item 24 (0/7 items in the buffer)
-->
