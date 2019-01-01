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
	Marker interface

Description:
	Marker interfaces, also known as tagging interfaces, allows
	associating meta-data with a hierarchy of objects. Typically,
	marker interfaces are empty with its presence implying that
	an object implements some functionality or have some property.

This pattern can be used with both classes and prototypes.

This pattern is not described in the GoF book but is common practice
in some languages (notably, Java). See e.g. the following Wikipedia
page for details and references:

https://en.wikipedia.org/wiki/Marker_interface_pattern

Given Logtalk support for protocols (interfaces), this is a trivial
pattern to implement. The reflection API provides built-in predicates
that allows to test if an object (or a category) implements or conforms
to a given protocol. It is also possible to add a marker to an existing
object at runtime using a complementing category (aka hot patching).
