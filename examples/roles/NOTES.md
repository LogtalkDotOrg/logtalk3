________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
Copyright 1998-2019 Paulo Moura <pmoura@logtalk.org>

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

Logtalk provides _objects_, _protocols_, and _categories_ as first-class
entities. Relations between entities define _patterns of code reuse_ and
the roles played by the entities. For example, when an object instantiates
another object, the first object plays the role of an instance and the
second object plays the role of a class. An extends relation between two
objects implies that both objects play the role of prototypes, with one
of them extending the other, its parent prototype.

This simple example illustrates the different roles an object can play.
See the comments in the `roles.lgt` file for detailed explanations.
