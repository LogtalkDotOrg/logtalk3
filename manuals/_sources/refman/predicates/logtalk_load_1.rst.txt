
.. index:: logtalk_load/1
.. _predicates_logtalk_load_1:

logtalk_load/1
==============

Description
-----------

::

   logtalk_load(File)
   logtalk_load(Files)

Compiles to disk and then loads to memory a :term:`source file` or a list of source
files using the current default compiler flag values. The Logtalk source
file name extension (by default, ``.lgt``) can be omitted. Source file
paths can be absolute, relative to the current directory, or use library
notation. This predicate can also be used to compile Prolog source files
as Logtalk source code. When no recognized Logtalk or Prolog extension
is specified, the compiler tries first to append a Logtalk source file
extension and then a Prolog source file extension. If that fails, the
compiler tries to use the file name as-is.

When this predicate is called from the top-level, relative source file
paths are resolved using the current working directory. When the calls
are made from a source file, relative source file paths are resolved
using the source file directory.

Note that only the errors related to problems in the predicate argument
are listed below. This predicate fails when errors are found during
compilation of a source file.

Depending on the back-end Prolog compiler, the shortcuts ``{File}`` or
``{File1, File2, ...}`` may be used in alternative. Check the adapter
files for the availability of these shortcuts as they are not part of
the language (and thus should only be used at the top-level
interpreter).

Template and modes
------------------

::

   logtalk_load(@source_file_name)
   logtalk_load(@list(source_file_name))

Errors
------

File is a variable:
   ``instantiation_error``
Files is a variable or a list with an element which is a variable:
   ``instantiation_error``
File, or an element File of the Files list, is neither a variable nor a source file name:
   ``type_error(source_file_name, File)``
File, or an element File of the Files list, uses library notation but the library does not exist:
   ``existence_error(library, Library)``
File or an element File of the Files list, does not exist:
   ``existence_error(file, File)``

Examples
--------

::

   | ?- logtalk_load(set).

   | ?- logtalk_load(types(tree)).

   | ?- logtalk_load([listp, list]).

.. seealso::

   :ref:`predicates_logtalk_compile_1`,
   :ref:`predicates_logtalk_compile_2`,
   :ref:`predicates_logtalk_load_2`,
   :ref:`predicates_logtalk_make_0`,
   :ref:`predicates_logtalk_make_1`
   :ref:`predicates_logtalk_library_path_2`
