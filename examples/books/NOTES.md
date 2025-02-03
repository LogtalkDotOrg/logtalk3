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

# books

This example illustrates how to use the optional terms library to decouple
data acquisition, which must be able to represent optional values in the
data, from data processing, which decides how to handle those values and
their absence. The use of optionals terms avoids the often problematic
solution of using special values to represent the absence of optionals
values. The optional terms library predicates allow handling the optional
terms without the need to use if-then-else or cut control constructs.

For a description of this example, please see the comments in the 
`books.lgt` source file.

See also the `missing_data `and `cascade` examples.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the example:

```logtalk
logtalk_load(books(loader)).
```

Print a list of books that have extras:

```logtalk
data_processing::print.
```

<!--
The Philosopher's Stone
  with free quidditch_set at 278 gr
The Chamber of Secrets
  with free map
The Prisoner of Azkaban
The Goblet of Fire
The Order of the Phoenix
The Half-Blood Prince
  with free audio_cd
The Deathly Hallows
  with free horcrux_set at 123 gr
true.
-->

Print variant using kilograms instead of grams by mapping optionals:

```logtalk
data_processing::print_kg.
```

<!--
The Philosopher's Stone
  with free quidditch_set at 0.278 kg
The Chamber of Secrets
  with free map
The Prisoner of Azkaban
The Goblet of Fire
The Order of the Phoenix
The Half-Blood Prince
  with free audio_cd
The Deathly Hallows
  with free horcrux_set at 0.123 kg
true.
-->

Print a list of extras with declared weight:

```logtalk
data_processing::print_heavy_extras.
```

<!--
quidditch_set at 278 gr
horcrux_set at 123 gr

true.
-->

Get a list of book titles that have extras:

```logtalk
data_processing::books_with_extras(Titles).
```

<!--
Titles = ['The Philosopher\'s Stone', 'The Chamber of Secrets', 'The Half-Blood Prince', 'The Deathly Hallows'].
-->
