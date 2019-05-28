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


About
-----

The library Java entities define a minimal abstraction for calling Java
from Logtalk. This abstraction makes use of Logtalk parametric objects
and allows creating Java object, accessing Java class fields, and calling
Java class and object methods using syntax closer to Logtalk. It also
gives access to some Java utility predicates.

This abstraction was developed primarily to work with the JPL library
bundled with SWI-Prolog and YAP. However, it's expected to be implementable
with alternative Java interfaces found in other backend Prolog compilers.
Currently, a preliminary implementation is also available for JIProlog.

The main idea in this abstraction layer is to use parametric objects where
the first parameter holds the Java reference (usually to a class or object)
and an optional second parameter holds the return value. Together with a
forward message handler, this allows the use of Java messages with the same
functor and number of arguments as found in the relevant JavaDocs.


API documentation
-----------------

Open the [../docs/index.html](../docs/index.html) file in a web browser
and choose the library index.


Loading
-------

To load all entities in this library, load the `loader.lgt` loader file:

	| ?- logtalk_load(java(loader)).


Usage
-----

The two main objects in this library are `java(Reference, ReturnValue)` and
`java(Reference)`. Use the latter if you want to ignore the return value or
when calling a void Java method.

The `java` object implements utility predicates. For some backend Java
interfaces such as JPL (available in SWI-Prolog and YAP) there is also
a `java_hook` hook object for removing any overhead when using this
library abstraction.

For usage examples and unit tests, see the `java` example.
