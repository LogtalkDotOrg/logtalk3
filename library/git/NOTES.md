________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>

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


`git`
=====

This library provides access to a git project current branch and latest
commit data (e.g. commit hash).


API documentation
-----------------

Open the [../../docs/library_index.html#git](../../docs/library_index.html#git)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

	| ?- logtalk_load(git(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(git(tester)).


Usage
-----

All predicates take as first argument a directory, which should be either a
git repository directory or a sub-directory of a git repository directory.
The main predicate is `commit_log/3`. It provides access to the `git log`
command when called with the `--oneline -n 1 --pretty=format:` options. By
passing as second argument the desired format, it returns an atom with the
formatted output. For example:

	| ?- git::commit_log('/Users/pmoura/logtalk3', '%h%n%B', Output),
	     write(Output), nl.

	eccaa1a2a
	Update SVG diagrams

	Output = 'eccaa1a2a\nUpdate SVG diagrams\n'
	yes

See e.g. the official documentation on `git log` pretty formats for details:

https://git-scm.com/docs/pretty-formats

Convenient predicates are also provided for common used formats such as the
commit author and the commit hash. For example:

	| ?- git::commit_author('/Users/pmoura/Documents/Logtalk/logtalk3', Author).

	Author = 'Paulo Moura'
	yes

	| ?- git::commit_hash('/Users/pmoura/Documents/Logtalk/logtalk3', Hash).
	
	Hash = eccaa1a2a9495fef441915bbace84e0a4b0394a2
	yes

It's also possible to get the name of the current local branch. For example:

	| ?- git::branch('/Users/pmoura/Documents/Logtalk/logtalk3', Branch).
	
	Branch = master
	yes
