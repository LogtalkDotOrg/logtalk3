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


This is a prototype tool to help port a plain Prolog application to Logtalk.
The tool takes a directory of Prolog files or a list of Prolog files, loads
and wraps the code in each file using an object wrapper, and advises on missing
directives to be added to those objects by using the compiler lint checker and
the reflection API. The user can then either save the generated wrapper objects
or copy and pasted the printed advise into the Prolog files (updating them to
Logtalk files by adding the object opening and closing directives to the Prolog
files). The wrapper objects can then be loaded for testing.

For the tool API, consult the `../../docs/wrapper_0.html` file.

This tool can be loaded using the query:

	| ?- logtalk_load(wrapper(loader)).

Typical workflow:

	| ?- wrapper::rdirectory(root_directory_of_prolog_code).
	...
	| ?- wrapper::save.
	...

The API predicates also accept a set of options for customization:

- `prolog_extensions(Extensions)`  
	list of file name extensions used to recognize Prolog source code files (default is `['.pl']`)
- `logtalk_extension(Extension)`  
	Logtalk default file name extension for the generated wrapper files (default is `'.lgt'`)
- `exclude_files(Files)`  
	list of Prolog source files names to exclude (default is `[]`)
- `exclude_directories(Files)`  
	list of sub-directory names to exclude (default is `[]`)

Current limitations:

- The tool cannot deal with syntax errors in the Prolog files. These usually
occur when using a backend Prolog compiler different from the one used to
run the original plain Prolog code.

- The tool assumes that all files to be wrapped have different names (even if
found in different directories). If that is not the case, the name conflicts
must be manually solved before using the tool.

- There isn't yet any support for dealing with meta-predicates and advise on
missing meta-predicate directives.


All source files are formatted using tabs (the recommended setting is a
tab width equivalent to 4 spaces).
