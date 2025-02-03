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

# jpl

Running all the examples and the benchmarks requires XVM (with its
`jni` plug-in installed) or SWI-Prolog (with the JPL library installed).
YAP most likely would be usable when the old JPL library bundled with it
is updated. Some examples (but not the benchmarks) can also be used with
JIProlog.

This folder contains GUI examples, most of them adapted from the JPL library
distributed with SWI-Prolog and YAP, of calling Java from Logtalk. It uses
a lightweight abstraction included in the Logtalk library for calling Java.

Adaptations of the JColorChooser and JOptionPane dialog examples and the
JTable example from the JPL distribution are included.

When running the GUI examples on the macOS Terminal application, you may
get a Java error saying that the AWT cannot be started. In alternative, try
to run the example from within the SWI-Prolog macOS application instead
of using the shell integration script. This issue is due to a macOS Java
issue that's orthogonal to both SWI-Prolog/YAP and Logtalk.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Start by loading the example:

```logtalk
logtalk_load(jpl(loader)).
```

Get the Java version by using the getProperty/1 static method:

```logtalk
java('java.lang.System', Version)::getProperty('java.version').
```

<!--
Version = '1.7.0_51'.
-->

Get the value of the "pi" constant by accessing the 'PI' class field:

```logtalk
java('java.lang.Math')::get_field('PI', Pi).
```

<!--
Pi = 3.141592653589793.
-->

Create a new instance of ArrayList and add some elements to it:

```logtalk
java('java.util.ArrayList')::new(ArrayList), java(ArrayList)::(add('Paulo'), add('Carlos'), add('Helena')).
```

<!--
true.
-->

Round-trip conversion:

```logtalk
java::terms_to_array([x,[1,a,7,[y,z]],k,[],3.14,foo(bar)], Array), java::array_to_terms(Array, List).
```

<!--
Array = @'J#00000140321793636416', List = [x, [1, a, 7, [y, z]], k, [], 3.14, foo(bar)].
-->

Run some benchmarks comparing plain JPL calls with calls to Logtalk's thin
abstraction layer:

```logtalk
benchmarks::run.
```

<!--
...
-->

Some GUI examples, adapted from the JPL distribution examples (skip if running as a notebook)

`JColorChooser` dialog example:

```logtalk
(current_object(jupyter) -> true; color_chooser::color(Color)).
```

<!--
Color = @'J#00000140727398998200'.
-->

`JTable` example:

```logtalk
(current_object(jupyter) -> true; flags_table::display).
```

<!--
true.
-->

`JList` dialog example:

```logtalk
(current_object(jupyter) -> true; jlist::display).
```

<!--
true.
-->

`JOptionPane` dialog example:

```logtalk
(current_object(jupyter) -> true; text_entry::text(Text)).
```

<!--
Text = ... .
-->
