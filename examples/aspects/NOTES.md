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

# aspects

This example illustrates how to implement aspects (as in Aspect-Oriented
Programming) using the event-driven programming support and hot patching
using categories. The example reuses the bank money transfer AOP example
as found in e.g.

https://en.wikipedia.org/wiki/Aspect-oriented_programming

The following entities are defined:

- `bank` (object defining a bank with a transfer predicate)
- `accounts` (defining balance, withdraw, and deposit predicates)
- `security` (hot patching category limiting maximum amount per transfer)
- `logging` (event monitor object logging transfers)

Note that we could also have defined `security` as a monitor object with a
`before/3` event handle that would make a transfer fail when the maximum
amount is exceeded. The downside of this alternative is that we should not
rely in the calling order of event handlers and thus the check could be
applied before or after the logging message that a transfer is being
attempted.

Start by loading the example:

```logtalk
logtalk_load(aspects(loader)).
```

Transfer 100 from john account to jane account

```logtalk
bank::transfer(john, jane, 100).
```

<!--
Attempting transfer:
  From:   john
  To:     jane
  Amount: 100
Transfer successful.
true.
-->

Try to transfer 400 from jane account to john account

```logtalk
bank::transfer(jane, john, 400).
```

<!--
Attempting transfer:
  From:   jane
  To:     john
  Amount: 400
Maximum transfer amount (200) exceeded!
false.
-->
