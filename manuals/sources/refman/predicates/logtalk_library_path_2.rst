
.. index:: logtalk_library_path/2
.. _predicates_logtalk_library_path_2:

logtalk_library_path/2
======================

Description
-----------

::

   logtalk_library_path(Library, Path)

Dynamic and multifile user-defined predicate, allowing the declaration
of aliases to library paths. Library aliases may also be used on the
second argument (using the notation *alias(path)*). Paths must always
end with the path directory separator character ('/').

Relative paths (e.g. ``'../'`` or ``'./'``) should only be used within
the *alias(path)*) notation so that library paths can always be expanded
to absolute paths independently of the (usually unpredictable) current
directory at the time the ``logtalk_library_path/2`` predicate is
called.

When working with a relocatable application, the actual application
installation directory can be retrieved by calling the
:ref:`predicates_logtalk_load_context_2` predicate
with the ``directory`` key and using the returned value to define the
``logtalk_library_path/2`` predicate. On a settings file, simply use an
:ref:`directives_initialization_1` directive
to wrap the call to the ``logtalk_load_context/2`` predicate and the
assert of the ``logtalk_library_path/2`` fact.

This predicate may also be used to override the default scratch
directory by defining the library alias ``scratch_directory`` in a
backend Prolog initialization file (assumed to be loaded prior to
Logtalk loading). This allows e.g. Logtalk to be installed in a
read-only directory by setting this alias to the operating-system
directory for temporary files. It also allows several Logtalk instances
to run concurrently without conflict by using a unique scratch directory
per instance (e.g. using a process ID or a UUID generator).

Template and modes
------------------

::

   logtalk_library_path(?atom, -atom)
   logtalk_library_path(?atom, -compound)

Errors
------

``(none)``

Examples
--------

::

   | ?- logtalk_library_path(viewpoints, Path).

   Path = examples('viewpoints/')
   yes

   | ?- logtalk_library_path(Library, Path).

   Library = home,
   Path = '$HOME/' ;

   Library = logtalk_home,
   Path = '$LOGTALKHOME/' ;

   Library = logtalk_user
   Path = '$LOGTALKUSER/' ;

   Library = examples
   Path = logtalk_user('examples/') ;

   Library = library
   Path = logtalk_user('library/') ;

   Library = viewpoints
   Path = examples('viewpoints/')
   yes

.. seealso::

   :ref:`predicates_logtalk_compile_1`,
   :ref:`predicates_logtalk_compile_2`,
   :ref:`predicates_logtalk_load_1`,
   :ref:`predicates_logtalk_load_2`
