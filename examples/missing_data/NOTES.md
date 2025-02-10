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

# missing_data

This example illustrates how to use the expected terms library to decouple
data acquisition, which must be able to succeed when unexpected events
happen, from data processing, which decides how to handle those events.

For more details about this example, please see the comments in the 
`expecteds.lgt` source file.

See also the `cascade` and `books` examples.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the example:

```logtalk
logtalk_load(missing_data(loader)).
```

In the first data processing example, we provide defaults for the missing data:

```logtalk
data_processing::print.
```

<!--
gomez
  father: john doe
  mother: jane doe

pubert
  father: gomez
  mother: morticia

pugsley
  father: gomez
  mother: morticia

morticia
  father: john doe
  mother: jane doe

wednesday
  father: gomez
  mother: morticia

true.
-->

In the second data processing example, we simply skip missing data:

```logtalk
data_processing::print_complete.
```

<!--
pubert
  father: gomez
  mother: morticia

pugsley
  father: gomez
  mother: morticia

wednesday
  father: gomez
  mother: morticia

true.
-->

In the third data processing example, we throw an error on missing data:

```logtalk
catch(data_processing::check, Error, true).
```

<!--
Error = missing_father-gomez.
-->
