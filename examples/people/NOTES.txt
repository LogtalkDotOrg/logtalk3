________________________________________________________________________

This file is part of Logtalk <http://logtalk.org/>  

Logtalk is free software. You can redistribute it and/or modify it under
the terms of the FSF GNU General Public License 3  (plus some additional
terms per section 7).        Consult the `LICENSE.txt` file for details.
________________________________________________________________________


To load this example and for sample queries, please see the `SCRIPT.txt`
file.
 
This example illustrates how to define object constructors for a simple 
hierarchy of objects representing persons, students, and teachers. For
simplicity, prototypes are used instead of classes. Logtalk provides a 
low-level, built-in predicate, `create_object/4`, for dynamically creating 
new objects. This predicate can be used to define object constructors, 
similar to those used in other OOP languages.

This example also illustrates how to efficiently represent objects with 
immutable state using parametric objects and object proxies (Prolog facts).
