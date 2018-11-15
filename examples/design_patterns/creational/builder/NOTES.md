________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>

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
	Builder

Description:
	"Separate the construction of a complex object from its
	representation so that the same construction process can
	create different representations."

This pattern can be used with both classes and prototypes.

Two sample implementations are provided, both using prototypes. The first
one uses parametric objects for a more declarative solution. The second
one uses objects with dynamic state to represent builders and products.
