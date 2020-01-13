``events``
==========

The objects ``event_registry``, ``before_event_registry``, and
``after_event_registry`` implement convenient predicates for registering
before and after events.

The code makes use of the ``monitoring`` built-in protocol, which
declares the two basic event handler predicates (``before/3`` and
``after/3``). You will need to refer to this protocol in your objects if
you want to use the super control structure ``(^^/1)`` with these
predicates.

The ``monitor`` object implements more sophisticated event handling
predicates.

API documentation
-----------------

Open the
`../../docs/library_index.html#events <../../docs/library_index.html#events>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` loader
file:

::

   | ?- logtalk_load(events(loader)).

