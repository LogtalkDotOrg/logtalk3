________________________________________________________________________

This file is part of Logtalk <http://logtalk.org/>  

Logtalk is free software. You can redistribute it and/or modify it under
the terms of the FSF GNU General Public License 3  (plus some additional
terms per section 7).        Consult the `LICENSE.txt` file for details.
________________________________________________________________________


To load this example and for sample queries, please see the `SCRIPT.txt`
file.

This folder contains a very simple example of using the compiler flag 
`hook/1` and the term expansion mechanism to either discard or activate 
debugging goals. For debugging goals in clause bodies, one defines clauses 
for `goal_expansion/2`. For debugging goals in directives (e.g. in the 
`initialization/1` directive), one defines clauses for `term_expansion/2`.

We can use two hook objects, one for developing and debugging code and
one for production code, or a single parametric object. This example
illustrates both approaches although only the first one is used in the
loader auxiliary files and in the sample queries.
