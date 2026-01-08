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

# elephants

This is a simple example illustrating the concept of _prototypes_ using
elephants. A similar example is often found in knowledge representation
discussions.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the example:

```logtalk
logtalk_load(elephants(loader)).
```

Prototypes are neither instances or classes but either standalone objects
as `clyde`, our prototypical but concrete elephant:

```logtalk
clyde::number_of_legs(N).
```

<!--
N = 4.
-->

```logtalk
clyde::color(C).
```

<!--
C = grey.
-->

Or objects that are derived from other prototypes as `fred`, which is
like `clyde` except in his color:

```logtalk
fred::number_of_legs(N).
```

<!--
N = 4.
-->

```logtalk
fred::color(C).
```

<!--
C = white.
-->
