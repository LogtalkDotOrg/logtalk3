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

# persistency

This example illustrates a very simple solution for persisting an object
dynamic state across sessions. It uses a plain Prolog file for saving the
state and an include/1 directive to automatically restore the saved state
when the object is loaded. The saved state file is created if it doesn't
exist by the `loader.lgt` file before loading the example itself. See also
the `serialization` example.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Load the example:

```logtalk
logtalk_load(persistency(loader)).
```

<!--
true.
-->

No initial state:

```logtalk
persistency::state(S).
no

Add some terms:

```logtalk
persistency::add(a).
```

<!--
true.
-->

```logtalk
persistency::add(b).
```

<!--
true.
-->

Enumerate current terms:

```logtalk
persistency::state(S).
```

<!--
S = a ;
S = b ;
false.
-->

Save the terms to permanent storage:

```logtalk
persistency::save.
```

<!--
true.
-->

Halt the process:

```logtalk
halt.
```

Restart Logtalk and reload:

```logtalk
logtalk_load(persistency(loader)).
```

<!--
true.
-->

Enumerate current terms:

```logtalk
%%table
persistency::state(S).
```

<!--
S = a ;
S = b ;
false.
-->

Add another term:

```logtalk
persistency::add(c).
```

<!--
true.
-->

Save the terms to permanent storage:

```logtalk
persistency::save.
```

<!--
true.
-->

Halt the process:

```logtalk
halt.

Restart Logtalk and reload:

```logtalk
logtalk_load(persistency(loader)).
```

<!--
true.
-->

Enumerate current terms:

```logtalk
%%table
persistency::state(S).
```

<!--
S = a ;
S = b ;
S = c ;
false.
-->
