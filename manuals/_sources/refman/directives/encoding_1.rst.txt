..
   This file is part of Logtalk <https://logtalk.org/>  
   Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.


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
