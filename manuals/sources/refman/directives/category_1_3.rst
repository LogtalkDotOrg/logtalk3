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


.. index:: category/1-3
.. _directives_category_1_3:

category/1-3
============

Description
-----------

::

   category(Category)

   category(Category,
       implements(Protocols))

   category(Category,
       extends(Categories))    

   category(Category,
       complements(Objects))

   category(Category,
       implements(Protocols),
       extends(Categories))

   category(Category,
       implements(Protocols),
       complements(Objects))

   category(Category,
       extends(Categories),
       complements(Objects))

   category(Category,
       implements(Protocols),
       extends(Categories),
       complements(Objects))

Starting category directive.

Template and modes
------------------

::

   category(+category_identifier)

   category(+category_identifier,
       implements(+implemented_protocols))
       
   category(+category_identifier,
       extends(+extended_categories))
       
   category(+category_identifier,
       complements(+complemented_objects))

   category(+category_identifier,
       implements(+implemented_protocols),
       extends(+extended_categories))

   category(+category_identifier,
       implements(+implemented_protocols),
       complements(+complemented_objects))

   category(+category_identifier,
       extends(+extended_categories),
       complements(+complemented_objects))

   category(+category_identifier,
       implements(+implemented_protocols),
       extends(+extended_categories),
       complements(+complemented_objects))

Examples
--------

::

   :- category(monitoring).

   :- category(monitoring,
       implements(monitoringp)).

   :- category(attributes,
       implements(protected::variables)).
       
   :- category(extended,
       extends(minimal)).

   :- category(logging,
       implements(monitoring),
       complements(employee)).

.. seealso::

   :ref:`directives_end_category_0`
