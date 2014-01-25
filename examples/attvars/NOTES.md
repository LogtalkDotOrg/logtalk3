________________________________________________________________________

This file is part of Logtalk <http://logtalk.org/>  

Logtalk is free software. You can redistribute it and/or modify it under
the terms of the FSF GNU General Public License 3  (plus some additional
terms per section 7).        Consult the `LICENSE.txt` file for details.
________________________________________________________________________


To load this example and for sample queries, please see the `SCRIPT.txt`
file.

This an example of using attributed variables within Logtalk objects and
categories. It requires the use of a backend Prolog compiler with support
for attributed variables. Tested with B-Prolog, SWI-Prolog, XSB, and YAP.
The individual examples are adaptations of code found on the SWI-Prolog
documentation.

When using SWI-Prolog or YAP, the `attvars_hook` hook object works by
creating a shadow module (with the same name as the object or category
internal prefix) containing the `attribute_goals//1` and `attr_unify_hook/2`
hooks that are required by the attributed variables implementation. These
hooks are compiled as module multifile predicates in order to avoid spurious
compiler warning messages (note that the hook clause bodies are compiled
within the context of the container object).

When using XSB, the `attvars_hook` hook object works by generating the
`install_verify_attribute_handler/4` and `install_attribute_portray_hook/3`
XSB directives, invoked using Logtalk-compiled arguments. These directives
allows the use of common code for SWI-Prolog, XSB, and YAP.

When using B-Prolog, the `attribute_goals//1` hook grammar rule is discarded.

Caveats:

1. The supported attribute variables built-in predicates (`get_attr/3`,
`put_attr/3`, and `del_attr/3`) can only be called from within objects (or
categories).

2. Calls to the supported attribute variables built-in predicates with a
second argument not corresponding to the enclosing entity are compiled as
references to other Logtalk entities. If you need to refer to a Prolog
module instead, wrap the predicate call using the `{}/1` control construct.
