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
	State

Description:
	"Allow an object to alter its behavior when its internal state
	changes. The object will appear to change its class."

This pattern can be used with both classes and prototypes.

We use prototypes in this sample implementation. The context object
uses a dynamic predicate to hold the current concrete state object
identifier with a public predicate to set it. The concrete state
objects perform the context switching after handling a forwarded
client request.
