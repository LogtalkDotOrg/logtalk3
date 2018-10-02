
.. _faq_portability:

Portability
===========

* :ref:`faq_portability_prolog`
* :ref:`faq_portability_os`

.. _faq_portability_prolog:

Are my Logtalk applications portable across Prolog compilers?
-------------------------------------------------------------

Yes, as long you don't use built-in predicates or special features
only available on some Prolog compilers. There is a compiler flag
(``portability``) that you can set to instruct Logtalk to print a
warning for each occurrence of non-ISO Prolog standard features such
as proprietary built-in predicates. In addition, it is advisable that you constrain,
if possible, the use of platform or compiler dependent code to a
small number of objects with clearly defined protocols. You may also
use Logtalk support for conditional compilation to compile different
entity or predicate definitions depending on the back-end Prolog
compiler being used.

.. _faq_portability_os:

Are my Logtalk applications portable across operating systems?
--------------------------------------------------------------

Yes. However, you may need to change the end-of-lines characters of
your source files to match the ones on the target operating system
and the expectations of your Prolog compiler. Some Prolog compilers
silently fail to compile source files with the wrong end-of-lines
characters.
