..
   This file is part of Logtalk <https://logtalk.org/>  
   Copyright 1998-2020 Paulo Moura <pmoura@logtalk.org>

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.


.. index:: pair: logtalk_library_path/2; Built-in predicate
.. _predicates_logtalk_library_path_2:

logtalk_library_path/2
======================

Description
-----------

::

   logtalk_library_path(Library, Path)

Dynamic and multifile user-defined predicate, allowing the declaration
of aliases to :term:`library` paths. Library aliases may also be used on
the second argument (using the notation *alias(path)*). Paths must always
end with the path directory separator character (``'/'``).

Relative paths (e.g. ``'../'`` or ``'./'``) should only be used within
the *alias(path)*) notation so that library paths can always be expanded
to absolute paths independently of the (usually unpredictable) current
directory at the time the ``logtalk_library_path/2`` predicate is
called.

When working with a relocatable application, the actual application
installation directory can be retrieved by calling the
:ref:`predicates_logtalk_load_context_2` predicate with the ``directory``
key and using the returned value to define the ``logtalk_library_path/2``
predicate. On a settings file or a :term:`loader file` file, simply use
an :ref:`directives_initialization_1` directive to wrap the call to the
``logtalk_load_context/2`` predicate and the assert of the
``logtalk_library_path/2`` fact.

This predicate may also be used to override the default
:term:`scratch directory` by defining the library alias ``scratch_directory``
in a backend Prolog initialization file (assumed to be loaded prior to
Logtalk loading). This allows e.g. Logtalk to be installed in a
read-only directory by setting this alias to the operating-system
directory for temporary files. It also allows several Logtalk instances
to run concurrently without conflict by using a unique scratch directory
per instance (e.g. using a process ID or a UUID generator).

The :ref:`logtalk <objects_logtalk>` built-in object provides an
:ref:`expand_library_path/2 <apis:logtalk/0::expand_library_path/2>`
predicate that can be used to expand library aliases and files expressed
using library notation.

Modes and number of proofs
--------------------------

::

   logtalk_library_path(?atom, -atom) - zero_or_more
   logtalk_library_path(?atom, -compound) - zero_or_more

Errors
------

(none)

Examples
--------

::

   :- initialization((
      logtalk_load_context(directory, Directory),
      assertz(logtalk_library_path(my_application_root, Directory))
   )).

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

::

   | ?- logtalk::expand_library_path(viewpoints, Path).

   Path = '/Users/pmoura/logtalk/examples/viewpoints/'.
   yes

   | ?- logtalk::expand_library_path(viewpoints('loader.lgt'), Path).

   Path = '/Users/pmoura/logtalk/examples/viewpoints/loader.lgt'.
   yes
   

.. seealso::

   :ref:`predicates_logtalk_compile_1`,
   :ref:`predicates_logtalk_compile_2`,
   :ref:`predicates_logtalk_load_1`,
   :ref:`predicates_logtalk_load_2`
