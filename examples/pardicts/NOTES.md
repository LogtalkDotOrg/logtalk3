________________________________________________________________________

This file is part of Logtalk <http://logtalk.org/>  

Logtalk is free software. You can redistribute it and/or modify it under
the terms of the FSF GNU General Public License 3  (plus some additional
terms per section 7).        Consult the `LICENSE.txt` file for details.
________________________________________________________________________


To load this example and for sample queries, please see the `SCRIPT.txt`
file.

This example requires using SWI-Prolog 7.x as the backend Prolog compiler.
It illustrates using a SWI-Prolog native dictionary term for representing
a parametric object parameters. This is accomplished by using a single
parameter, instantiated to a dictionary, and by defining a set of predicates
for accessing the individual parameters by a key (instead of using the
Logtalk built-in `parameter/2` method that indexes individual parameters by
position). The access predicates are goal-expanded to the corresponding
SWI-Prolog dictionary built-in predicates.
