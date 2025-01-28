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
	Mediator

Description:
	"Defines an object that encapsulates how a set of objects interact.
	Mediator promotes loose coupling by keeping objects from referring
	to each other explicitly, and it lets you vary their interaction
	independently."

This pattern can be used with both classes and prototypes.

Start by loading the design pattern sample implementations:

```logtalk
logtalk_load(design_patterns('behavioral/mediator/loader')).
```

First, click the "book" button:

```logtalk
book_button::click.
```

<!--
book_button disabled
view_button enabled
search_button enabled
booking...

true.
-->

Next, click the "view" button:

```logtalk
view_button::click.
```

<!--
book_button enabled
view_button disabled
search_button enabled
viewing...

true.
-->

Finally, click the "search" button:

```logtalk
search_button::click.
```

<!--
book_button enabled
view_button enabled
search_button disabled
searching...

true.
-->
