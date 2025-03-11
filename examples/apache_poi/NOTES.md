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

# apache_poi

This example is only supported when using XVM (with its `jni` plug-in
installed), SWI-Prolog, or YAP as the backend compiler.

This is a simple example of using a Java library for converting Excel
spreadsheets to text using the Apache POI Java library, which you
**must** download from:

https://poi.apache.org

Assuming POI 5.4.0, the following JAR files are required:

- `commons-codec-1.18.0.jar`
- `commons-collections4-4.4.jar`
- `commons-compress-1.27.1.jar`
- `commons-io-2.18.0.jar`
- `commons-math3-3.6.1.jar`
- `curvesapi-1.6.0.jar`
- `log4j-api-2.24.3.jar`
- `log4j-core-2.24.3.jar`
- `poi-5.4.0.jar`
- `poi-excelant-5.4.0.jar`
- `poi-ooxml-5.4.0.jar`
- `poi-ooxml-full-5.4.0.jar`
- `poi-ooxml-schemas-4.1.2.jar`
- `poi-scratchpad-5.4.0.jar`
- `SparseBitSet-1.3.jar`
- `xmlbeans-5.3.0.jar`

Copy all the JAR files to this example `jars` folder before running the
example.

## Running from a terminal

When running this example from the terminal (i.e., not as a notebook),
start by setting the Java `CLASSPATH` environment variable. Three backend
Prolog systems are supported: XVM, SWI-Prolog, and YAP. There's a Bash
script file that sets the `CLASSPATH` environment variable when sourced:

```text
$ cd "$LOGTALKUSER/examples/apache_poi"
$ . set_classpath.sh
```

Similar for Windows using the `set_classpath.ps1` PowerShell script.

## Running as a notebook

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Set the required environment variables (edit if using a different Apache POI version):

```logtalk
setenv('CLASSPATH', './jars/commons-codec-1.18.0.jar:./jars/commons-collections4-4.4.jar:./jars/commons-compress-1.27.1.jar:./jars/commons-io-2.18.0.jar:./jars/commons-math3-3.6.1.jar:./jars/curvesapi-1.6.0.jar:./jars/log4j-api-2.24.3.jar:./jars/log4j-core-2.24.3.jar:./jars/poi-5.4.0.jar:./jars/poi-excelant-5.4.0.jar:./jars/poi-ooxml-5.4.0.jar:./jars/poi-ooxml-full-5.4.0.jar:./jars/poi-ooxml-schemas-4.1.2.jar:./jars/poi-scratchpad-5.4.0.jar:./jars/SparseBitSet-1.3.jar:./jars/xmlbeans-5.3.0.jar').
```

Load the example:

```logtalk
logtalk_load(apache_poi(loader)).
```

Read a `sample.xlsx` spreadsheet to a `user` object `db/4` predicate:

```logtalk
spreadsheet::load('test_files/sample.xlsx', user, db).
```

<!--
true.
-->

List all clauses of the `db/4` predicate representing the spreadsheet sheets and number of rows:

```logtalk
%%table
db(SheetName, SheetIndex, FirstRow, LastRow).
```

<!--
Contents = ... .
-->

List all clauses of the `db/5` predicate representing the spreadsheet sheet contents:

```logtalk
%%table
db(SheetIndex, RowIndex, ColumnIndex, CellType, CellValue).
```

<!--
...
-->

Define a `capitals` object with `capitals/4-5` predicates:

```logtalk
%%file capitals.lgt

:- object(capitals).

    :- public([
		capitals/4,
		capitals/5
	]).

	% continents
	capitals(europe, 0, 0, 3).
	capitals(africa, 1, 0, 3).
	% europe
	capitals(0, 0, 0, string, 'Country').
	capitals(0, 0, 1, string, 'Capital').
	capitals(0, 1, 0, string, 'Portugal').
	capitals(0, 1, 1, string, 'Lisboa').
	capitals(0, 2, 0, string, 'Spain').
	capitals(0, 2, 1, string, 'Madrid').
	capitals(0, 3, 0, string, 'France').
	capitals(0, 3, 1, string, 'Paris').
	% africa
	capitals(1, 0, 0, string, 'Country').
	capitals(1, 0, 1, string, 'Capital').
	capitals(1, 1, 0, string, 'Angola').
 	capitals(1, 1, 1, string, 'Luanda').
	capitals(1, 2, 0, string, 'Egypt').
	capitals(1, 2, 1, string, 'Cairo').
	capitals(1, 3, 0, string, 'Cape Verde').
	capitals(1, 3, 1, string, 'Praia').

:- end_object.
```

<!--
true.
-->

Save the `capitals/4-5` predicates to a `capitals.xls` spreadsheet:

```logtalk
spreadsheet::save(capitals, capitals, 'capitals.xls').
```

<!--
true.
-->
