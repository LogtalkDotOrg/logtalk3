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
	Singleton

Description:
	"Ensure a class has one instance, and provide a global point
	of access to it."

This pattern can be used with both classes and prototypes.

Given Logtalk supports for prototypes, implementing this pattern is
trivial. Instead of using a class and write code to ensure a single
class instance, we can simply use a prototype, which is its own global
point of access. As applications can use a mix of prototypes and
classes, there is nothing to be gained in implementing this pattern
using classes.
