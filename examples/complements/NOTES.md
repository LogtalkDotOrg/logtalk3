________________________________________________________________________

This file is part of Logtalk <http://logtalk.org/>  
Copyright 1998-2016 Paulo Moura <pmoura@logtalk.org>

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
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
