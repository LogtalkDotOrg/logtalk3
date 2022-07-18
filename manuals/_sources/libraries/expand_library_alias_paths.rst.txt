.. _library_expand_library_alias_paths:

``expand_library_alias_paths``
==============================

This library provides provides a hook object,
``expand_library_alias_paths``, for expanding library alias paths in
``logtalk_library_path/2 facts`` in source files. It is mainly used when
embedding Logtalk and Logtalk applications.

API documentation
-----------------

Open the
`../../docs/library_index.html#expand-library-alias-paths <../../docs/library_index.html#expand-library-alias-paths>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` utility
file:

::

   | ?- logtalk_load(expand_library_alias_paths(loader)).

Usage
-----

Use the ``hook/1`` option when compiling a source file:

::

   | ?- logtalk_load(my_source_file, [hook(expand_library_alias_paths)]).
   ...

Alternatively, assuming it is the only hook object you are using, you
can set it as thew default hook object:

::

   | ?- set_logtalk_flag(hook, expand_library_alias_paths).
   ...
