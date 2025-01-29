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

# ack

This folder contains an implementation of the Ackermann function (general
recursive function). For a description of this function see e.g.

	https://en.wikipedia.org/wiki/Ackermann_function

Print Logtalk, Prolog backend, and kernel versions:

```{code-cell}
%versions
```

Start by loading the example:

```{code-cell}
logtalk_load(ack(loader)).
```

Sample queries:

```{code-cell}
ack::ack(2, 4, V).
```
<!--
V = 11.
-->

```{code-cell}
ack::ack(3, 3, V).
```
<!--
V = 61.
-->

```{code-cell}
ack::ack(3, 4, V).
```
<!--
V = 125.
-->

Sample queries for backends implementing the time/1 predicate (e.g.,
SWI-Prolog, Trealla Prolog, XVM, or YAP; the adapter files for these
systems may ensure that a (::)/2 goal in the argument of the `time/1`
predicate is compiled prior to calling it):

```{code-cell}
% auto-load the predicate in the case of SWI-Prolog
time(true).
```
<!--
true.
-->

```{code-cell}
time(ack::ack(2, 4, V)).
```
<!--
% 98 inferences, 0.00 CPU in 0.00 seconds (0% CPU, Infinite Lips)
V = 11.
-->

```{code-cell}
time(ack::ack(3, 3, V)).
```
<!--
% 2,451 inferences, 0.00 CPU in 0.00 seconds (0% CPU, Infinite Lips)
V = 61.
-->

```{code-cell}
time(ack::ack(3, 4, V)).
```
<!--
% 10,326 inferences, 0.00 CPU in 0.00 seconds (0% CPU, Infinite Lips)
V = 125.
-->
