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

# my_types

This folder contains an example of defining new types for type-testing
and type-checking purposes (what else?) using the user-extensible `type`
library object.

Load the example:

```logtalk
logtalk_load(my_types(loader)).
```

<!--
true.
-->

Type-check temperature values in different units. I.e., that the value is in the valid range.

Celsius:

```logtalk
type::check(temperature(celsius), 38.7).
```

<!--
true.
-->

Fahrenheit:

```logtalk
type::check(temperature(fahrenheit), 101.2).
```

<!--
true.
-->

Kelvin (valid value):

```logtalk
type::check(temperature(kelvin), 307.4).
```

<!--
true.
-->

Kelvin (invalid value):

```logtalk
type::valid(temperature(kelvin), -12.1).
```

<!--
false.
-->

The temperature type definition requires a float value:

```logtalk
catch(type::check(temperature(celsius), 38), Error, true).
```

<!--
Error = type_error(float, 38).
-->

The Kelvin scale starts at 0.0:

```logtalk
catch(type::check(temperature(kelvin), -12.1, my_error_context), Error, true).
```

<!--
Error = error(domain_error(property(float, [A]>>(A>=0.0)), -12.1), my_error_context).
-->
