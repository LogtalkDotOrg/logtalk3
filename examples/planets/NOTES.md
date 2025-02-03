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

# planets

This is a simple example illustrating the basics of defining and using
protocols and categories.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the example:

```logtalk
logtalk_load(planets(loader)).
```

Compute the weight of the `m1` object on Earth:

```logtalk
earth::weight(m1, W1).
```

<!--
W1 = 29.40.
-->

Compare with the weight of the `m1` object on Mars:

```logtalk
mars::weight(m1, W1).
```

<!--
W1 = 11.16.
-->

Compute the weight of the `m2` object on Earth:

```logtalk
earth::weight(m2, W2).
```

<!--
W2 = 39.20.
-->

Compare with the weight of the `m2` object on Jupiter:

```logtalk
jupiter::weight(m2, W2).
```

<!--
W2 = 92.48.
-->
