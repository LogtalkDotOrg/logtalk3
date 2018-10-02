
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

See also
--------

:ref:`directives_end_category_0`
