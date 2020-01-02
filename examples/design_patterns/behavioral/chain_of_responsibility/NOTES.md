________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
Copyright 1998-2020 Paulo Moura <pmoura@logtalk.org>

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
	Chain of Responsibility

Description:
	"Avoid coupling the sender of a request to its receiver by giving
	more than one object a chance to handle the request. Chain the
	receiving objects and pass the request along the chain until an
	object handles it."

This pattern can be used with both classes and prototypes.

In this sample implementation, we use prototypes and fixed successors
in the chain of responsibility. But the successors could also easily
be defined dynamically.
