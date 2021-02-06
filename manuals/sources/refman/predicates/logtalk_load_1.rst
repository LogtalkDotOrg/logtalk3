..
   This file is part of Logtalk <https://logtalk.org/>  
   Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
   SPDX-License-Identifier: Apache-2.0

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.


.. index:: pair: logtalk_load/1; Built-in predicate
.. _predicates_logtalk_load_1:

``logtalk_load/1``
==================

Description
-----------

::

   logtalk_load(File)
   logtalk_load(Files)

Compiles to disk and then loads to memory a :term:`source file` or a list
of source files using the default compiler flag values. The Logtalk source
file name extension (by default, ``.lgt``) can be omitted. Source file
paths can be absolute, relative to the current directory, or use
:term:`library notation`. This predicate can also be used to compile Prolog
source files as Logtalk source code. When no recognized Logtalk or Prolog
extension is specified, the compiler tries first to append a Logtalk source
file extension and then a Prolog source file extension. If that fails, the
compiler tries to use the file name as-is. The recognized Logtalk and Prolog
file extensions are defined in the :term:`backend adapter files <adapter file>`.

When this predicate is called from the top-level interpreter, relative source
file paths are resolved using the current working directory. When the calls
are made from a source file, relative source file paths are resolved
using the source file directory.

Note that only the errors related to problems in the predicate argument
are listed below. This predicate fails on the first error found during
compilation of a source file. In this case, no contents of the source
file are loaded.

Depending on the :term:`backend Prolog compiler`, the shortcuts ``{File}``
or ``{File1, File2, ...}`` may be used in alternative. Check the adapter
files for the availability of these shortcuts as they are not part of
the language (and thus should only be used at the top-level
interpreter).

Modes and number of proofs
--------------------------

::

   logtalk_load(@source_file_name) - zero_or_one
   logtalk_load(@list(source_file_name)) - zero_or_one

Errors
------

| ``File`` is a variable:
|     ``instantiation_error``
| ``Files`` is a variable or a list with an element which is a variable:
|     ``instantiation_error``
| ``File``, or an element ``File`` of the ``Files`` list, is neither a variable nor a source file name:
|     ``type_error(source_file_name, File)``
| ``File``, or an element ``File`` of the ``Files`` list, uses library notation but the library does not exist:
|     ``existence_error(library, Library)``
| ``File`` or an element ``File`` of the ``Files`` list, does not exist:
|     ``existence_error(file, File)``

Examples
--------

::

   % compile and load the "set" source file in the
   % current directory:
   | ?- logtalk_load(set).

   % compile and load the "tree" source file in the
   % "types" library directory:
   | ?- logtalk_load(types(tree)).

   % compile and load the "listp" and "list" source
   % files in the current directory:
   | ?- logtalk_load([listp, list]).

.. seealso::

   :ref:`predicates_logtalk_compile_1`,
   :ref:`predicates_logtalk_compile_2`,
   :ref:`predicates_logtalk_load_2`,
   :ref:`predicates_logtalk_make_0`,
   :ref:`predicates_logtalk_make_1`,
   :ref:`predicates_logtalk_library_path_2`
