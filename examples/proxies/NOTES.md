________________________________________________________________________

This file is part of Logtalk <http://logtalk.org/>  
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


To load this example and for sample queries, please see the `SCRIPT.txt` file.

A parametric object may be used to represent objects whose "state" is static 
and set when the object is defined. However, there can be only one parametric 
object with a given functor and arity. For example, if we define the following
parametric object:

	:- object(circle(_Radius, _Color)).
		...
	:- end_object.

then the following terms may be interpreted as references to the object above:

	circle(1, blue)
	circle(2, yellow)

In the context of parametric objects, the above terms are know as "parametric 
object proxies". Proxies represent different instantiations of a parametric 
object parameters. Proxy terms may be stored on the database as Prolog facts 
or as Prolog rules (parameter instantiation can be deduced instead of being 
fixed). This results in a very compact representation, which can be an 
advantage when dealing with a large number of objects with immutable state. 
In addition, all the predicates managing these compact representation are 
encapsulated in a parametric object. This can be, however, a fragile solution 
as changes on the parametric object ancestors may imply changes to the number 
and meaning of the parametric object parameters which, in turn, may imply 
changes to all the Prolog facts used to represent the individual objects.

Note that parametric objects can co-exist with "normal" objects. For example, 
when using a class-based design, we may use "normal" instances together with
a parametric instance of the same class.
