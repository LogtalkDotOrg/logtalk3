________________________________________________________________________

This file is part of Logtalk <http://logtalk.org/>  

Logtalk is free software. You can redistribute it and/or modify it under
the terms of the FSF GNU General Public License 3  (plus some additional
terms per section 7).        Consult the `LICENSE.txt` file for details.
________________________________________________________________________


To load this example and for sample queries, please see the `SCRIPT.txt`
file.

This folder contains two examples that show how to use a category to 
explicitly complement existing objects (without modifying its source 
code), thus providing functionality similar to Objective-C categories.

The complemented objects need to be compiled with the flag `complements` 
set to either `allow` or `restrict` (its default value is `deny`; this
solution was adapted to improve performance of applications that doesn't
use complementing categories and to provide a solution for preventing
the use of categories to break object encapsulation). Note that the
`complements` flag can be set on a per-object basis as in this example.
