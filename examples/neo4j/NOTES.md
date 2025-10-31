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

# neo4j

This example is only supported when using XVM (with its `jni` plug-in
installed), SWI-Prolog, or YAP as the backend compiler.

There are two individual examples. The first is a simple example of using
Neo4j based on the `HelloWorldExample` found at:

https://neo4j.com/docs/java-manual/current/get-started/

The second example is more elaborated and based on Neo4j example found at:

https://console.neo4j.org

After instaling Neo4j (5.x or later version), you must edit the `neo4j.conf`
file to run this example:

Disable authentication:

```text
dbms.security.auth_enabled=false
```

Configure the Bolt connector:

```text
server.bolt.enabled=true
server.bolt.tls_level=DISABLED
server.bolt.listen_address=:7687
server.bolt.advertised_address=:7687
```

You may also need to disable some Neo4j system properties:

```text
#wrapper.java.additional=-Djava.awt.headless=true
#wrapper.java.additional.4=-Dneo4j.ext.udc.source=homebrew
```

Or, depending on the Neo4j version:

```text
#server.jvm.additional=-Djava.awt.headless=true-Dunsupported.dbms.udc.source=homebrew
```

Save the edit file and start (or restart) Neo4j:

```text
$ neo4j start
```

When running this example on Windows, check first the Neo4j installation
directory path used in the `set_classpath_*.ps1` scripts and adjust it if
required.

For the default location of the `neo4j.conf` file, see:

https://neo4j.com/docs/operations-manual/current/configuration/file-locations/

## Running from a terminal

When running this example from the terminal (i.e., not as a notebook),
start by setting the Java `CLASSPATH` environment variable. Three backend
Prolog systems are supported: SWI-Prolog, XVM, and YAP. There's a Bash
script file that sets the `CLASSPATH` environment variable when sourced:

```text
$ cd "$LOGTALKUSER/examples/document_converter"
$ . set_classpath.sh
```

## Running as a notebook

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Set the required environment variables (edit for the location of the Neo4j JAR files in your system):

```logtalk
os::directory_files('/usr/local/Cellar/neo4j/5.26.1/libexec/lib', _JARs, [paths(absolute), extensions(['.jar'])]),
atomic_list_concat(_JARs, ':', _CLASSPATH),
setenv('CLASSPATH', _CLASSPATH).
```

Load the example:

```logtalk
logtalk_load(neo4j(loader)).
```

Print a greeting message (if not running as a notebook):

```logtalk
(current_object(jupyter) -> true; hello_world('bolt://localhost:7687', 'neo4j', 'password')::print_greeting('Hello world!')).
```

<!--
Hello world!, from node 0

true.
-->

Find the people Neo knows in the Matrix:

```logtalk
matrix('bolt://localhost:7687', 'neo4j', 'password')::neo_knows(Who).
```

<!--
Who = ['Agent Smith', 'Cypher', 'Morpheus', 'Trinity'].
-->
