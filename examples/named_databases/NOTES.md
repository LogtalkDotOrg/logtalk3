________________________________________________________________________

This file is part of Logtalk <http://logtalk.org/>  
Copyright 1998-2016 Paulo Moura <pmoura@logtalk.org>

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
file.

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

Named databases are currently implemented for a subset of the Prolog
systems supported by Logtalk: ECLiPSe, Lean Prolog, SICStus Prolog,
SWI-Prolog, and YAP.

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
