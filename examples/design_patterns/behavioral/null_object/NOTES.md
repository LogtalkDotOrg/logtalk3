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
	Null object

Description:
	"Instead of using a null reference to convey absence of an object
	(for instance, a non-existent customer), one uses an object which
	implements the expected interface, but whose method body is empty.
	The advantage of this approach over a working default implementation
	is that a null object is very predictable and has no side effects:
	it does nothing."

This pattern can be used with both classes and prototypes. The description
above is from the Wikipedia page on this pattern:

https://en.wikipedia.org/wiki/Null_object_pattern

This pattern is not described in the GoF book but is found in books by
Martin Fowler and Joshua Kerievsky. See the Wikipedia page for details
and references.

Logtalk doesn't provide a "null" built-in object by design. When the
equivalent to a null object is necessary, one can be defined as e.g.
defined in this pattern. Note that Logtalk provides a library
implementation of "optionals", which are often a better solution.

Start by loading the design pattern sample implementations:

```logtalk
logtalk_load(design_patterns('behavioral/null_object/loader')).
```

Non null objects reply to the predicates with their own intrinsic answers.
For example, a dog:

```logtalk
dog::make_sound.
```

<!--
Woof...

true.
-->

Or a cat:

```logtalk
cat::make_sound.
```

<!--
Meowww...

true.
-->

The null object implements the same interface but does nothing.

```logtalk
null_animal::make_sound.
```

<!--
true.
-->
