________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
SPDX-FileCopyrightText: 1998-2023 Paulo Moura <pmoura@logtalk.org>  
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


To load this example and for sample queries, please see the `SCRIPT.txt`
file. This example is only supported when using Arriba/LVM (with its `jni`
plug-in installed), SWI-Prolog, or YAP as the backend compiler.

This is a simple example of using a Java library for performing clustering
of a set of numbers. It uses the Apache Commons Math Java library, which
you **must** download from:

https://commons.apache.org/proper/commons-math/

After downloading the library archive, decompress it, and copy the main
JAR (for version 3.6.1, this would be the `commons-math3-3.6.1.jar` file)
to this example `jars` folder before running the example.
