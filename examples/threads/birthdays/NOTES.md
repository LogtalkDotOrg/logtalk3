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

# threads - birthdays

This folder contains a simple multi-threading example with agents and
their birthdays, illustrating the use of the built-in predicates
`threaded_ignore/1`, `threaded_once/1`, and `threaded_exit/1`.

When using XVM as the backend, this example must be run from the top-level
interpreter.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Load the example:

```logtalk
logtalk_load(birthdays(loader)).
```

Create two new agents, Paul and Nathalie:

```logtalk
agent::(new(paul, 40, male), new(nathalie, 32, female)).
```

<!--
true.
-->

Make them friends:

```logtalk
paul::new_friend(nathalie).
```

<!--
true.
-->

Turn event support on for messages sent at the command-line
(i.e., for messages sent from the pseudo-object `user`):

```logtalk
set_logtalk_flag(events, allow).
```

<!--
true.
-->

It's birthday for Nathalie:

```logtalk
nathalie::birthday.
```

<!--
Happy birthday from paul!
Thanks! Here, have a slice of cake, paul.
Thanks for the cake nathalie!
Say goodbye to your 32's!

true.
-->

Ask Nathalie her age:

```logtalk
nathalie::age(Age).
```

<!--
Age = 33.
-->

A year goes by...

```logtalk
nathalie::birthday.
```

<!--
Happy birthday from paul!
Thanks! Here, have a slice of cake, paul.
Thanks for the cake nathalie!
Say goodbye to your 33's!

true.
-->

Turn event support off:

```logtalk
set_logtalk_flag(events, deny).
```

<!--
true.
-->
