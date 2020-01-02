..
   This file is part of Logtalk <https://logtalk.org/>  
   Copyright 1998-2020 Paulo Moura <pmoura@logtalk.org>

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.


.. _faq_programming:

Programming
===========

* :ref:`faq_programming_either_prototypes_classes`
* :ref:`faq_programming_both_prototypes_classes`
* :ref:`faq_programming_hierarchy`
* :ref:`faq_programming_protocols_categories`
* :ref:`faq_programming_components`
* :ref:`faq_programming_reflection`

.. _faq_programming_either_prototypes_classes:

Should I use prototypes or classes in my application?
-----------------------------------------------------

Prototypes and classes provide different patterns of code reuse. A
prototype encapsulates code that can be used by itself and by its
descendent prototypes. A class encapsulates code to be used by its
descendent instances. Prototypes provide the best replacement to the
use of modules as encapsulation units, avoiding the need to
instantiate a class in order to access its code.

.. _faq_programming_both_prototypes_classes:

Can I use both classes and prototypes in the same application?
--------------------------------------------------------------

Yes. In addition, you may freely exchange messages between
prototypes, classes, and instances.

.. _faq_programming_hierarchy:

Can I mix classes and prototypes in the same hierarchy?
-------------------------------------------------------

No. However, you may define as many prototype hierarchies and class
hierarchies and classes as needed by your application.

.. _faq_programming_protocols_categories:

Can I use a protocol or a category with both prototypes and classes?
--------------------------------------------------------------------

Yes. A protocol may be implemented by both prototypes and classes in
the same application. Likewise, a category may be imported by both
prototypes and classes in the same application.

.. _faq_programming_components:

What support is provided in Logtalk for defining and using components?
----------------------------------------------------------------------

Logtalk supports component-based programming (since its inception on
January 1998), by using *categories* (which are first-class entities
like objects and protocols). Logtalk categories can be used with both
classes and prototypes and are inspired on the Smalltalk-80
(documentation-only) concept of method categories and on Objective-C
categories, hence the name. For more information, please consult the
:ref:`categories_categories` section and the examples provided with
the distribution.

.. _faq_programming_reflection:

What support is provided in Logtalk for reflective programming?
---------------------------------------------------------------

Logtalk supports meta-classes, behavioral reflection through the use
of event-driven programming, and structural reflection through the
use of a set of built-in predicates and built-in methods which allow
us to query the system about existing entities, entity relations, and
entity predicates.
