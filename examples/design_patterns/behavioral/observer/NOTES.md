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
SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>  
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
	Observer

Description:
	"Define a one-to-many dependency between objects so that when one
	object changes state, all its dependents are notified and updated
	automatically."

This pattern can be used with both classes and prototypes.

Given Logtalk support for events, this design pattern is trivial to
implement. Any number of objects can play the role of observer and any
number of objects can be subjects. Events are managed by the runtime,
avoiding the coupling typical of solutions using a Smalltalk-like
dependents mechanism (which is the most typical implementation in
other programming languages): subjects don't need to be aware of
observers. The observer registry is global and subject predicates
don't need to be modified to notify observers. A possible limitation
is that events are only generated for public messages, i.e. for
messages using the `(::)/2` control construct. Messages to self, which
use the `(::)/1` control construct, don't generate events. This
restriction exists to ensure that events cannot be used to break
encapsulation. When this limitation is an issue, note that the
Logtalk standard library includes an implementation of the Smalltalk
dependents mechanism that can be used in alternative.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the design pattern sample implementation:

```logtalk
logtalk_load(design_patterns('behavioral/observer/loader')).
```

Trigger an observed event:

```logtalk
subject::ping(Reply).
```

<!--
user is asking observer:  ping(_992)
observer replied to user: ping(pong)
Reply = pong

true.
-->
