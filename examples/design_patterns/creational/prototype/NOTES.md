________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>  
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


Design pattern:
	Prototype

Description:
	"Specify the kinds of objects to create using a prototypical
	instance, and create new objects by copying this prototype."

This pattern can be used with both classes and prototypes.

Logtalk doesn't provide a built-in object clone functionality that would
help in implementing this design pattern. One of the main issues in
providing object cloning support is the depth of the copy when an object
contains references to other objects. Some languages provide a single
cloning primitive while other support both shallow and deep copies (e.g.
Java's clone method performs a shallow copy).

The sample implementation uses classes. The cloning operation creates a
new instance with the same state as the currently selected prototype.
This is a relatively expensive operation do to requiring retrieving the
state of the prototype for copying. More efficient solutions can be
implemented in most cases by e.g. having customized instance creation
predicate definitions.
