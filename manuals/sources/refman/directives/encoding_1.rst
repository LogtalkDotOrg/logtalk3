
.. index:: encoding/1
.. _directives_encoding_1:

encoding/1
==========

Description
-----------

::

   encoding(Encoding)

Declares the source file text encoding. Only supported on some back-end
Prolog compilers. When used, this directive must be the first term in
the source file in the first line. Currently recognized encodings values
include ``'US-ASCII'``, ``'ISO-8859-1'``, ``'ISO-8859-2'``,
``'ISO-8859-15'``, ``'UCS-2'``, ``'UCS-2LE'``, ``'UCS-2BE'``,
``'UTF-8'``, ``'UTF-16'``, ``'UTF-16LE'``, ``'UTF-16BE'``, ``'UTF-32'``,
``'UTF-32LE'``, ``'UTF-32BE'``, ``'Shift_JIS'``, and ``'EUC-JP'``. Be
sure to use an encoding supported by the chosen back-end Prolog compiler
(whose adapter file must define a table that translates between the
Logtalk and Prolog specific atoms that represent each supported
encoding). When writing portable code that cannot be expressed using
ASCII, ``'UTF-8'`` is usually the best choice.

Template and modes
------------------

::

   encoding(+atom)

Examples
--------

::

   :- encoding('UTF-8').
