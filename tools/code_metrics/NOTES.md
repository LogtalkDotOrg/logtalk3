________________________________________________________________________

This file is part of Logtalk <http://logtalk.org/>  
Copyright 2017 Ebrahim Azarisooreh <ebrahim.azarisooreh@gmail.com> and  
Paulo Moura <pmoura@logtalk.org>

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


Overview
--------

The purpose of this tool is to assess qualities of source code that may
predict negative aspects such as complexity, error-proneness, and overall
maintainability. It is meant to be extensible via the addition of metric
objects into the `metrics` sub-directory. Currently, three metrics are
provided: "Number of Clauses", "Depth of Inheritance", and "Coupling Score".
The method of scoring was strongly influenced by the metrics rating system
in Microsoft Visual Studio and aims to eventually emulate the functionality
of a maintainability index score.

For usage examples, see the `SCRIPT.txt` file.


Loading
-------

This tool can be loaded using the query:

	| ?- logtalk_load(code_metrics(loader)).


Third-party tools
-----------------

`cloc` is an open-source command-line program that counts blank lines, comment
lines, and lines of source code in many programming languages including Logtalk.
It is available at https://github.com/AlDanial/cloc

`ohcount` is an open-source command-line program that counts blank lines, comment
lines, and lines of source code in many programming languages including Logtalk.
It is available at https://github.com/blackducksoftware/ohcount
