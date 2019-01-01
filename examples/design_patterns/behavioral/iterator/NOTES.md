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
	Iterator

Description:
	"Provide a way to access the elements of an aggregate object
	sequentially without exposing its underlying representation."

This pattern can be used with both classes and prototypes.

A straightforward implementation of this design pattern is to define an
iterator predicate that enumerates elements using backtracking. This
predicate can then be called from meta-predicates such as the de facto
standard `forall/2` predicate.
