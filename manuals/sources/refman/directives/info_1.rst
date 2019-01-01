..
   This file is part of Logtalk <https://logtalk.org/>  
   Copyright 1998-2019 Paulo Moura <pmoura@logtalk.org>

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.


.. index:: info/1
.. _directives_info_1:

info/1
======

Description
-----------

::

   info([Key is Value, ...])

Documentation directive for objects, protocols, and categories. The
directive argument is a list of pairs using the format *Key is Value*.
See the :ref:`documenting_entity` section for a description of the
default keys.

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

   :ref:`directives_info_2`,
   :ref:`predicates_object_property_2`,
   :ref:`predicates_protocol_property_2`,
   :ref:`predicates_category_property_2`
