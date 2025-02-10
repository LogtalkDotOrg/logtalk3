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

# engines - pmq

This folder contains an implementation of a message priority queue using
a perpetual threaded engine holding the priority queue. At any moment, we
ask for a list of the pending messages ordered by priority.

A variant is also provided that splits top messages from normal messages
into separate queues. In this case, asking for a list of the pending
messages returns a list with top messages before the normal messages but
keeping the message sent order otherwise.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the example and the required libraries:

```logtalk
logtalk_load(pmq(loader)).
```

<!--
true.
-->

Send some messages to the priority queue:

```logtalk
pmq::(send(13-let), send(5-out), send(11-the), send(17-who), send(7-dogs)).
```

<!--
true
-->

Retrieve the current messages sorted by priority:

```logtalk
pmq::messages(Messages).
```

<!--
Messages = [who, let, the, dogs, out].
-->

After retrieving the messages the priority queue is empty until new messages
are received:

```logtalk
pmq::messages(Messages).
```

<!--
Messages = [].
-->

Send the next batch of messages:

```logtalk
pmq::(send(8-fun), send(11-have), send(3-':-)')).
```

Retrieve the current messages sorted by priority:

```logtalk
pmq::messages(Messages).
```

<!--
Messages = [have, fun, ':-)'].
-->

Same messages but to an alternative implementation that splits top messages
from normal messages instead of sorting all messages by priority:

```logtalk
split::(send(13-let), send(5-out), send(11-the), send(17-who), send(7-dogs)).
```

<!--
true.
-->

```logtalk
split::messages(List).
```

<!--
List = [let, the, who, out, dogs].
-->

```logtalk
split::messages(List).
```

<!--
List = [].
-->

```logtalk
split::(send(8-fun), send(11-have), send(3-':-)')).
```

<!--
true.
-->

```logtalk
split::messages(List).
```

<!--
List = [have, fun, ':-)'].
-->
