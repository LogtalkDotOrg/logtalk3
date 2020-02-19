________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
Copyright 2020 Michael T. Richter and Paulo Moura <pmoura@logtalk.org>

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

This example illustrates how to use SWI-Prolog Pengines from objects. Two
versions are provided. The first version, `dumper`, simply writes all the
pengine answers to the current output. The second version, `engines`, uses
a threaded engine to provide an interface to the pengine in order to access
the answers on demand. This shows how an application can ask a pengine to
compute answers and then go do something else until it needs to access the
answers. This example started from a port of the pengines example found at:

https://www.swi-prolog.org/pldoc/man?section=pengine-examples

The minimal `pengine_server` Prolog module code is also based on the pengines
documentation, available at:

https://www.swi-prolog.org/pldoc/doc_for?object=section(%27packages/pengines.html%27)
