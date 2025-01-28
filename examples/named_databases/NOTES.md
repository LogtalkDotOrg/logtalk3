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

# named_databases

This example implements "named databases", providing a prototype implementation
for a *portable* API based on Lean Prolog implementation of this functionality.
The goal is to provide the fastest possible *loading time* for encapsulated
data. The API supports loading data from a file into a named database, saving
a named database to a file, together with a set of predicates derived from the
standard Prolog built-in database predicates. The performance advantage of
using a Prolog module over a Logtalk object is that, for the supported backend
compilers, the module file is compiled in one step without requiring generating
and compiling an intermediate file as (must be) done by the Logtalk compiler
when loading a source file (for portability across compiler backends).

Named databases are currently implemented for a subset of the Prolog systems
supported by Logtalk: ECLiPSe, SICStus Prolog, SWI-Prolog, and YAP.

Caveat: most Prolog systems cannot list clauses for static code. Some systems,
such as SWI-Prolog, do allow listing of static code by default but can be set
to prevent it. One important consequence is that, although it is possible to
*load* files with static predicates into named databases, saving them will
only save the dynamic predicates. I.e. named databases loading static resources
should be regarded as read-only databases.

A hook object is provided for optimizing calls to the named database API
predicates within object and categories.

Being a programming example, there isn't currently any error checking on
the API predicate arguments.

API description:

- `db_create(Database)`  
	Creates a new named database

- `db_dynamic(Database, Predicate)`  
	Declares a new dynamic predicate

- `db_abolish(Database, Predicate)`  
	Abolishes a dynamic predicate

- `db_asserta(Database, Clause)`  
	Asserts a clause for a dynamic predicate

- `db_assertz(Database, Clause)`  
	Asserts a clause for a dynamic predicate

- `db_retract(Database, Clause)`  
	Retracts a matching clause for a dynamic predicate

- `db_retractall(Database, Head)`  
	Retracts all clauses for a dynamic predicate with a matching head

- `db_clause(Database, Head, Body)`  
	Retrieves clauses for dynamic predicates in the named database

- `db_call(Database, Goal)`  
	Proves a goal using the predicate clauses in the named database

- `db_once(Database, Goal)`  
	Proves a goal once using the predicate clauses in the named database

- `db_listing(Database)`  
	Lists all dynamic predicates in the named database

- `db_load(Database, File)`  
	Loads a Prolog file into a named database

- `db_save(Database, File)`  
	Saves all dynamic predicates to a file

- `db_clear(Database)`  
	Abolishes all dynamic predicates

Start by loading the example:

```logtalk
logtalk_load(named_databases(loader)).
```

Create a new named database:

```logtalk
db_create(my_db).
```

<!--
true.
-->

Add some facts to the new database:

```logtalk
db_dynamic(my_db, foo/1).
```

<!--
true.
-->

```logtalk
db_assertz(my_db, foo(1)), db_assertz(my_db, foo(2)), db_assertz(my_db, foo(3)).
```

<!--
true.
-->

Prove goals using the named database:

```logtalk
%%table
db_call(my_db, foo(X)).
```

<!--
X = 1 ;
X = 2 ;
X = 3.
-->

```logtalk
db_once(my_db, foo(X)).
```

<!--
X = 1.
-->

Save all dynamic predicates in the database to a file:

```logtalk
db_save(my_db, 'my_db.pl').
```

<!--
true.
-->

Clear the named database:

```logtalk
db_clear(my_db).
```

<!--
true.
-->

Load the saved file into a different database:

```logtalk
db_create(foo_db), db_load(foo_db, 'my_db.pl').
```

<!--
true.
-->

Check that the saved facts are there by retracting them one by one:

```logtalk
%%table
db_retract(foo_db, foo(X)).
```

<!--
X = 1 ;
X = 2 ;
X = 3.
-->
