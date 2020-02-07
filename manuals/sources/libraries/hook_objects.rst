``hook_objects``
================

This library provides a set of convenient hook objects for defining
custom expansion workflows (using e.g. the ``hook_flows`` library).

API documentation
-----------------

Open the
`../../docs/library_index.html#hook_objects <../../docs/library_index.html#hook_objects>`__
link in a web browser.

Loading
-------

To load all hook objects in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(hook_objects(loader)).

To load a specific hook object, e.g. the ``backend_hook`` object:

::

   | ?- logtalk_load(hook_objects(backend_hook)).

Testing
-------

To test this library hook objects, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(hook_objects(tester)).

Usage
-----

The provided hook objects cover different expansion scenarios as
follows:

1. Using the Prolog backend adapter file when defining a custom
   expansion workflow. This can be accomplished by loading the
   ``backend_hook.lgt`` file, which defines a ``backend_hook`` hook
   object that can be used as a workflow step.

2. Restore the default compiler expansion workflow. In this case, load
   the ``default_hook.lgt`` file, which defines a ``default_hook`` hook
   object, and use the following **goal** to set the default hook
   object:

   ::

       | ?- set_logtalk_flag(hook, default_hook).

3. Prevent expanding a source file. Simply load the ``dummy_hook.lgt``
   file, which defines the ``dummy_hook`` hook object, and set it as the
   file specific hook object writing as the first term in the file the
   **directive**:

   ::

       :- set_logtalk_flag(hook, dummy_hook).
