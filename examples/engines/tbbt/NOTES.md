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

# engines - tbbt

This example implements the rock, paper, scissors, lizard, Spock game played
in the "The Big Bang Theory" sitcom using one threaded engine per player. See:

	http://www.samkass.com/theories/RPSSL.html

Currently this example requires ECLiPSe or SWI-Prolog. It should run also on
XSB and YAP if and when these systems fix the bugs in their multi-threading
support.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Load the example:

```logtalk
logtalk_load(tbbt(loader)).
```

Explain the game of rock, paper, scissors, lizard, spock:

```logtalk
game::explain.
```

Play a game:

```logtalk
game::play.
```
