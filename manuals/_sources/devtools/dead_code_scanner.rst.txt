``dead_code_scanner``
=====================

This tool detects *likely* dead code in Logtalk entities and in Prolog
modules compiled as objects. Predicates (and non-terminals) are
classified as dead code when:

1. There is no scope directive for them.
2. They are not called, directly or indirectly, by any predicate with a
   (local or inherited) scope directive.

Predicates (and non-terminals) listed in ``uses/2`` and ``use_module/2``
directives but that are not used are also classified as dead code.

Besides dead code, this tool can also help detect other problems in the
code that often result in reporting false positives. For example, typos
in ``alias/2`` directives, missing scope directives, and missing
``meta_non_terminal/1`` and ``meta_predicate/1`` directives.

Given the possibility of false positives, care must be taken before
deleting reported dead code to ensure that it’s, in fact, code that is
not used. A common cause of false positives is the use of conditional
compilation directives to provide implementations for predicates missing
in some systems.

The ``dead_code_scanner.lgt`` source file implements the scanning
predicates for finding dead code in entities, libraries, and
directories. The source file ``dead_code_scanner_messages.lgt`` defines
the default translations for the messages printed when scanning for dead
code. These messages can be intercepted to customize output, e.g. to
make it less verbose, or for integration with e.g. GUI IDEs and
continuous integration servers.

API documentation
-----------------

To consult this tool API documentation, open in a web browser the link:

`docs/library_index.html#dead-code-scanner <https://logtalk.org/docs/library_index.html#dead-code-scanner>`__

For sample queries, please see the `SCRIPT.txt <SCRIPT.txt>`__ file in
the tool directory.

Loading
-------

This tool can be loaded using the query:

::

   | ?- logtalk_load(dead_code_scanner(loader)).

Known issues
------------

Use of local meta-calls with goal arguments only known at runtime can
result in false positives. When using library or user-defined
meta-predicates, it can be helpful to compile the source files with the
``optimize`` flag turned on so that the meta-calls may be resolved at
compile time and thus allow calling information for the meta-arguments
to be recorded, avoiding false positives for predicates that are only
meta-called.

Other notes
-----------

All source files are formatted using tabs (the recommended setting is a
tab width equivalent to 4 spaces).
