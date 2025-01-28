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

# laptops

This example illustrates defining an object as a composition of other
objects in order to contrast with category-based composition. For a
detailed description, please see the comments in the `laptops.lgt`
source file.

Start by loading the example:

```logtalk
logtalk_load(laptops(loader)).
```

Describe the basic laptop:

```logtalk
basic::describe.
```

<!--
CPU: i5
Memory: 8 GB
Display: 1440 x 900 pixels
Keyboard: qwerty
true.
-->

Describe the business laptop:

```logtalk
business::describe.
```

<!--
CPU: i7
Memory: 16 GB
Display: 2560 x 1600 pixels
Keyboard: qwerty
true.
-->

Create a custom laptop and describe it:

```logtalk
custom::new(faster, thirteen, dvorak, Laptop).
```

<!--
Laptop = o1.
-->

```logtalk
o1::describe.
```

<!--
CPU: i7
Memory: 16 GB
Display: 1440 x 900 pixels
Keyboard: dvorak
true.
-->
