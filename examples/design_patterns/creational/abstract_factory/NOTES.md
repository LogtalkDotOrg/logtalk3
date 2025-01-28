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
	Abstract Factory

Description:
	"Provide an interface for creating families of related or dependent
	objects without specifying their concrete classes."

This pattern can be used with both classes and prototypes.

The sample implementation uses prototypes for simplicity with categories
playing a similar role to abstract classes.

Start by loading the design pattern sample implementations:

```logtalk
logtalk_load(design_patterns('creational/abstract_factory/loader')).
```

Create buttons with different appearances:

```logtalk
factory(linux)::create_button(Button), Button::paint.
```

<!--
Render a button in a Linux style
Button = o1

true.
-->

```logtalk
factory(macos)::create_button(Button), Button::paint.
```

<!--
Render a button in a macOS style
Button = o2

true.
-->

```logtalk
factory(windows)::create_button(Button), Button::paint.
```

<!--
Render a button in a Windows style
Button = o3

true.
-->
