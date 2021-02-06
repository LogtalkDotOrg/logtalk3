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


.. index:: pair: logtalk_load/2; Built-in predicate
.. _predicates_logtalk_load_2:

``logtalk_load/2``
==================

Description
-----------

::

   logtalk_load(File, Flags)
   logtalk_load(Files, Flags)

Compiles to disk and then loads to memory a :term:`source file` or a list
of source files using a list of compiler flags. The Logtalk source file name
extension (by default, ``.lgt``) can be omitted. Source file paths can be
absolute, relative to the current directory, or use :term:`library notation`.
Compiler flags are represented as *flag(value)*. This predicate can also be
used to compile Prolog source files as Logtalk source code. When no recognized
Logtalk or Prolog extension is specified, the compiler tries first to append a
Logtalk source file extension and then a Prolog source file extension. If that
fails, the compiler tries to use the file name as-is. For a description of the
available compiler flags, please see the :ref:`programming_flags` section in
the User Manual. The recognized Logtalk and Prolog file extensions are defined
in the :term:`backend adapter files <adapter file>`. The recognized Logtalk
and Prolog file extensions are defined in the
:term:`backend adapter files <adapter file>`.

When this predicate is called from the top-level interpreter, relative source
file paths are resolved using the current working directory. When the calls
are made from a source file, relative source file paths are resolved by
default using the source file directory (unless a
:ref:`relative_to <flag_relative_to>` flag is passed).

Note that only the errors related to problems in the predicate argument
are listed below. This predicate fails on the first error found during
compilation of a source file. In this case, no contents of the source
file are loaded.

.. warning::

   The compiler flags specified in the second argument only apply to the
   files listed in the first argument and not to any files that those files
   may load or compile. Notably, if you are loading a :term:`loader file`,
   the flags only apply to the loader file itself and not to the files
   loaded by it.

Modes and number of proofs
--------------------------

::

   logtalk_load(@source_file_name, @list(compiler_flag)) - zero_or_one
   logtalk_load(@list(source_file_name), @list(compiler_flag)) - zero_or_one

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
| ``Flags`` is a variable or a list with an element which is a variable:
|     ``instantiation_error``
| ``Flags`` is neither a variable nor a proper list:
|     ``type_error(list, Flags)``
| An element ``Flag`` of the ``Flags`` list is not a valid compiler flag:
|     ``type_error(compiler_flag, Flag)``
| An element ``Flag`` of the ``Flags`` list defines a value for a read-only compiler flag:
|     ``permission_error(modify, flag, Flag)``
| An element ``Flag`` of the ``Flags`` list defines an invalid value for a flag:
|     ``domain_error(flag_value, Flag+Value)``

Examples
--------

::

   % compile and load the "list" source file in the
   % current directory using default compiler flags:
   | ?- logtalk_load(list, []).

   % compile and load the "tree" source file in the "types"
   % library directory with the source_data flag turned on:
   | ?- logtalk_load(types(tree)).

   % compile and load the "file_system" source file in the
   % current directory with portability warnings suppressed:
   | ?- logtalk_load(file_system, [portability(silent)]).

.. seealso::

   :ref:`predicates_logtalk_compile_1`,
   :ref:`predicates_logtalk_compile_2`,
   :ref:`predicates_logtalk_load_1`,
   :ref:`predicates_logtalk_make_0`,
   :ref:`predicates_logtalk_make_1`,
   :ref:`predicates_logtalk_library_path_2`
