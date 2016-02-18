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


To load this tool and for sample queries, please see the `SCRIPT.txt` file.

This is a prototype tool to help port a plain Prolog application to Logtalk.
The tool takes a list of Prolog files, loads and wraps the code in each file
using an object wrapper, and advises on missing directives to be added to
those objects by using the compiler lint checker and the reflection API. The
user is then expected to copy and pasted the printed advise into the Prolog
files and run them again. After a few, usually just one, iterations there
will be no more missing directives and the user can proceed to test the
wrapper objects. Assuming that the tests are successful, the user can then
add the object opening and closing directives to the Prolog files, change
their extension to a Logtalk source code file extension, and recompile to
check for additional issues.

Current limitations:

- The tool cannot deal with syntax errors in the Prolog files. These usually
occur when using a backend Prolog compiler different from the one used to
run the original plain Prolog code.

- There isn't yet any support for dealing with meta-predicates and advise on
missing meta-predicate directives.


All source files are formatted using tabs (the recommended setting is a
tab width equivalent to 4 spaces).
