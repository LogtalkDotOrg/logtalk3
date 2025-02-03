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

# clustering

This example is only supported when using XVM (with its `jni` plug-in
installed), SWI-Prolog, or YAP as the backend compiler.

This is a simple example of using a Java library for performing clustering
of a set of numbers. It uses the Apache Commons Math Java library, which
you **must** download from:

https://commons.apache.org/proper/commons-math/

After downloading the library archive, decompress it, and copy the main
JAR (for version 3.6.1, this would be the `commons-math3-3.6.1.jar` file)
to this example `jars` folder before running the example.

Be sure to have download the required JAR files before attempting to
run this example; see the `NOTES.txt` file for details

Start by setting the Java `CLASSPATH` environment variable. Three backend
Prolog systems are supported: XVM, SWI-Prolog, and YAP. There's a Bash
script file that sets the `CLASSPATH` environment variable when sourced:

	$ cd "$LOGTALKUSER/examples/clustering"
	$ . set_classpath.sh

Similar for Windows using the `set_classpath.ps1` PowerShell script.

Second, start Logtalk and load the example:

```logtalk
logtalk_load(clustering(loader)).
```

Create 4 clusters from a list of float values:

```logtalk
clustering::clusters([1.0,1.5,1.8,3.5,3.6,4.0,4.2], 4, 10000, Clusters).
```

<!--
Clusters = [[3.5, 3.6], [1.5, 1.8], [4.0, 4.2], [1.0]].
-->
