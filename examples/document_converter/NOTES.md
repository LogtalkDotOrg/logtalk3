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

# document_converter

This example is only supported when using XVM (with its `jni` plug-in
installed), SWI-Prolog, or YAP as the backend compiler.

This is a simple example of using a Java library for converting documents
to text using the Apache Tika Java library, which you **must** download
from:

https://tika.apache.org

After downloading the Tika JAR file (for version 2.8.0, this would be the
`tika-app-2.8.0.jar` file), copy it to this example `jars` folder before
running the example.

Be sure to have download the required JAR files before attempting to
run this example.

Start by setting the Java `CLASSPATH` environment variable. Three backend
Prolog systems are supported: XVM, SWI-Prolog, and YAP. There's a Bash
script file that sets the `CLASSPATH` environment variable when sourced:

	$ cd "$LOGTALKUSER/examples/document_converter"
	$ . set_classpath_swi.sh

Second, start Logtalk and load the example:

```logtalk
logtalk_load(document_converter(loader)).
```

Convert a "sample.pdf" document to a "sample.txt" file:

```logtalk
document::convert('sample.pdf', 'sample.txt').
```

<!--
true.
-->

Get the text contents of a "sample.pdf" document:

```logtalk
document::contents('sample.pdf', Contents).
```

<!--
Contents = ... .
-->
