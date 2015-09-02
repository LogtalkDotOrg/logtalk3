________________________________________________________________________

This file is part of Logtalk <http://logtalk.org/>  

Logtalk is free software. You can redistribute it and/or modify it under
the terms of the FSF GNU General Public License 3  (plus some additional
terms per section 7).        Consult the `LICENSE.txt` file for details.
________________________________________________________________________


To load this example and for sample queries, please see the `SCRIPT.txt`
file.

This example illustrates how to use the `begin_of_file` term generated
when compiling a source file to create an object wrapper for the code
in a plain Prolog file. Assuming that the `context_switching_calls` is
set to `allow`, the generated object predicates can be called using the
`<</2` debugging control construct for testing. This wrapper is useful
for e.g. using the Logtalk compiler lint checks to examine predicate
call dependencies of the wrapped code and also to look for possible
portability issues when the `portability` flag is set to `warning`.
