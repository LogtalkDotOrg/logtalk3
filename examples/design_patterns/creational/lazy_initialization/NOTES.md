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

Design pattern:
	Lazy initialization

Description:
	Delay creation of a required instance (accessed using a key)
	until the instance is requested.

This pattern can be used with both classes and prototypes. The description
above is from the Wikipedia page on this pattern:

https://en.wikipedia.org/wiki/Lazy_initialization

This pattern is not described in the GoF book. See the Wikipedia page for
details and references.

The sample implementation uses classes.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the design pattern sample implementation:

```logtalk
logtalk_load(design_patterns('creational/lazy_initialization/loader')).
```

Access/create an instance for key "apple":

```logtalk
fruit::new(apple, Instance).
```

<!--
Instance = o1.
-->

Access/create an instance for key "banana":

```logtalk
fruit::new(banana, Instance).
```

<!--
Instance = o2.
-->

Access/create an instance for key "apple":

```logtalk
fruit::new(apple, Instance).
```

<!--
Instance = o1.
-->

Show all initialized keys:

```logtalk
fruit::show_all.
```

<!--
apple
banana

true.
-->
