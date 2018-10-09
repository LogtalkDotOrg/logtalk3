
.. index:: info/1
.. _directives_info_1:

info/1
======

Description
-----------

::

   info(List)

Documentation directive for objects, protocols, and categories. The
directive argument is a list of pairs using the format *Key is Value*.
See the :ref:`documenting_entity` section
for a description of the default keys.

Template and modes
------------------

::

   info(+entity_info_list)

Examples
--------

::

   :- info([
       version is 1.0,
       author is 'Paulo Moura',
       date is 2000/4/20,
       comment is 'List protocol.'
   ]).

.. seealso::

   :ref:`directives_info_2`
