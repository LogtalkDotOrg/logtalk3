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
	Decorator

Description:
	"Attach additional responsibilities to an object dynamically.
	Decorators provide a flexible alternative to subclassing for
	extending functionality."

This pattern can be used with both classes and prototypes.

The sample code uses a parametric object to represent the decorator object
with a parameter being used to pass the decorated object. This is a clean,
declarative solution given that object parameters are logical variables.
It also supports defining decorations dynamically by simply constructing
identifier terms for the parametric object. In those cases where we need
to persist across backtracking the association between a decorator object
and its decorated object, we can use in alternative a dynamic predicate
in the decorator object.

As a decorator object should accept all messages that the decorated object
accepts, we use the `forward/1` handler for unknown messages to forward to
the decorated objects all messages that are not defined in the decorator
itself.

Another possible implementation of this pattern is to use a *complementing
category* (i.e. hot patching) to decorate an object. This can be a good
alternative solution when we need only to decorated a few specific objects
(although a complementing category can complement multiple objects). The
advantage of using a decorator object is that it can be used to decorate
any object at compilation time or at runtime while with a complementing
category we need to know in advance which objects we will be decorating.
