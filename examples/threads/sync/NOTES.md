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

# threads - sync

This folder contains two simple multi-threading examples illustrating the 
use of the `synchronized/1` predicate directive to cope with methods that 
have side effects.

The `slow_print` example was originally coded for a Rosetta Code contribution
available at:

https://rosettacode.org/wiki/Mutex#Logtalk

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the example:

```logtalk
logtalk_load(sync(loader)).
```

Slow print text:

```logtalk
slow_print::start.
```

<!--
abc
123
abc
123
abc
123
abc
123
abc
...
-->

Send three asynchronous messages whose corresponding methods perform output operations:

```logtalk
threaded_ignore(nasty1::io(alpha)), threaded_ignore(nasty1::io(digit)), threaded_ignore(nasty1::io(alpha)).
```

<!--
a0ab1bc2c3ddefef45gg6hh7ii8jkjk9
llmmnnopopqqrrsstztzyyxxwwuv
uv

true.
-->

Send three asynchronous messages whose corresponding methods perform database updates
(this may or may not work, most likely will throw an exception):

```logtalk
threaded_ignore(nasty1::update_db(_)), threaded_ignore(nasty1::update_db(_)), threaded_ignore(nasty1::update_db(_)).
```

<!--
false.
-->

The best solution is to declare predicates that need to be thread synchronized as `synchronized`,
as exemplified in object `nasty2`:

```logtalk
threaded_ignore(nasty2::io(alpha)), threaded_ignore(nasty2::io(digit)), threaded_ignore(nasty2::io(alpha)).
```

<!--
abcdefghijklmnopqrstzyxwuv
0123456789
abcdefghijklmnopqrstzyxwuv

true.
-->

```logtalk
threaded_call(nasty2::update_db(_)), threaded_call(nasty2::update_db(_)), threaded_call(nasty2::update_db(_)).
```

<!--
true.
-->

```logtalk
threaded_exit(nasty2::update_db(X)), threaded_exit(nasty2::update_db(Y)), threaded_exit(nasty2::update_db(Z)).
```

<!--
X = 1
Y = 2
Z = 3 

true.
-->
