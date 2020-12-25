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
	Proxy

Description:
	"Provide a surrogate or placeholder for another object to control
	access to it."

This pattern can be used with both classes and prototypes.

Proxy objects are simple to implement. We can have the proxy object
implementing a protocol, shared with the real object, that specifies
only the predicates that the proxy object redefines. This allows the
using the `forward/1` handler to delegate all other messages to the
real object. When the proxy object is expected to implement the full
protocol of the real object, an alternative solution is for the proxy
object to inherit privately from the real object and overriding (or
specializing) only those predicates that motivated the use of a proxy.
