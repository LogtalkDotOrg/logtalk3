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
	Flyweight

Description:
	"Use sharing to support a large number of fine-grained objects
	efficiently."

This pattern can be used with both classes and prototypes.

Flyweight objects store invariant state that can be shared between several
objects, thus allowing more efficient representation of large number of
objects that share that state. This allow those objects to only store the
variant parts of the state.

Note that any object in Logtalk, independently of the role it plays (e.g.
prototype or class), can also act as a flyweight object for its descendants.
Static predicates that should be shared can simply be defined in the flyweight
object and inherited by its descendants. Any dynamic predicates can be handled
(i.e. asserted, accessed, and retracted) in either the flyweight object, if
shared, or in *self*, if it may differ for each descendant.
