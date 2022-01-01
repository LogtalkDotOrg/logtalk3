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
	Template Method

Description:
	"Define the skeleton of an algorithm in an operation, deferring
	some steps to subclasses. Template Method lets subclasses redefine
	certain steps of an algorithm without changing the algorithm's
	structure."

This pattern can be used with both classes and prototypes.

The key for implementing this pattern is the *message to self* control
construct, `::/1`. It allows a predicate implementing an algorithm to
call the predicates implementing the algorithm steps defined in descendant
objects. It also allows easy definition of default definitions for the
algorithm steps that can be inherited, redefined, or specialized in the
descendant objects. The predicates implementing the algorithm steps are
often declared as protected.
