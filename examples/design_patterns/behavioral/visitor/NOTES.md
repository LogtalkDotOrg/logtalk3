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


Design pattern:
	Visitor

Description:
	"Represent an operation to be performed on the elements of an
	object structure. Visitor lets you define a new operation without
	changing the classes of the elements on which it operates."

This pattern can be used with both classes and prototypes.

This design pattern can be trivially implemented by defining a suitable
meta-predicate that walks the structure and applies a user defined
closure to each element. When the object defining the structure doesn't
provide such a meta-predicate, a complementing category can define and
add it to the object. Both cases are illustrated in the sample code.
