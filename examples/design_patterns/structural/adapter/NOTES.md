________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>  
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
	Adaptor

Description:
	"Convert the interface of a class into another interface clients
	expect. Adapter lets classes work together that couldn't otherwise
	because of incompatible interfaces."

This pattern can be used with both classes and prototypes.

This pattern is usually implemented using either multiple inheritance
or using object composition. Both solutions are supported in Logtalk.
We illustrate in our sample implementation a solution that mimics
composition by using parametric objects. We could also have used in
alternative dynamic predicates to hold the same information passed
using parameters.
