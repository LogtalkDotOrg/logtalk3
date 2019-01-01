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


Design pattern:
	Multiton

Description:
	Generalization of the Singleton design pattern where a fixed number
	of named instances of a class is managed.

This pattern can be used with both classes and prototypes. The description
above is from the Wikipedia page on this pattern:

https://en.wikipedia.org/wiki/Multiton_pattern

This pattern is not described in the GoF book. See the Wikipedia page for
details and references.

The sample implementation uses classes. To simplify, it assumes a fixed
(at compile time) set of named instances. An alternative would be allow
the definition of the set of named instances at runtime by extending the
protocol of the multiton with the necessary predicates. Also to simplify,
the sample code uses the named instance identifiers as the access keys.
This could also be changed to use instead a dictionary.
