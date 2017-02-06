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
objects into the `metric/` sub-directory. At the time of writing (2017/1/11),
this metrics tool comes with three metrics out of the box: `Number of Clauses`,
`Depth of Inheritance`, and `Coupling Score`. The method of scoring was
strongly influenced by the metrics rating system in Microsoft Visual Studio and
aims to eventually emulate the functionality of a maintainability index score.
Please see `SCRIPT.txt` for common examples of usage.


Loading
-------

This tool can be loaded using the query:

	| ?- logtalk_load(code_metrics(loader)).


Issues
------

It's currently unknown whether the first argument for `item_score/2` should
support any non-entities, hence the name was changed from `entity_score/2`
to `item_score/2`. Therefore, the initial version of this tool may introduce
some mental ambiguity. This issue should be resolved as the tool matures.
