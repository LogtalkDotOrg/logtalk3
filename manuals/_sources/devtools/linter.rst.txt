``linter``
==========

Logtalk provides a built-in linter tool that runs automatically when
compiling and loading source files. The lint warnings are controlled by
a `set of flags <../userman/programming.html#programming-flags-lint>`__.
The default values for these flags are defined in the backend Prolog
compiler adapter files and can be overriden from a settings file or from
a source file (e.g. a loader file). These flags can be set globally
using the
`set_logtalk_flag/2 <../refman/predicates/set_logtalk_flag_2.html>`__
built-in predicate. For (source file or entity) local scope, use instead
the
`set_logtalk_flag/2 <../refman/directives/set_logtalk_flag_2.html>`__
directive.
