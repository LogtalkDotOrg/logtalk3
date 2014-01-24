________________________________________________________________________

This file is part of Logtalk <http://logtalk.org/>  

Logtalk is free software. You can redistribute it and/or modify it under
the terms of the FSF GNU General Public License 3  (plus some additional
terms per section 7).        Consult the `LICENSE.txt` file for details.
________________________________________________________________________



To load this example and for sample queries, please see the `SCRIPT.txt`
file. Run this example with no other examples loaded at the same time.

This folder contains an example that shows how to implement a reflective
class-based system. There are three main classes:

- `object`  
	root of the inheritance graph
- `class`  
	default metaclass for all instantiable classes
- `abstract_class`  
	default metaclass for all abstract classes

Each class inherit all the methods form the other two classes and from 
itself (without any inheritance loops of course ;-).

You can find more sophisticated versions of these classes in the `roots`
example.
