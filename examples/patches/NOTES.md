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

This folder contains an example that shows how to use a complementing
category to replace a broken predicate definition and to add a new imported
category to an existing object (without modifying its source code), thus
providing hot patching functionality similar to Objective-C categories.

The complemented objects must be compiled with the flag `complements` set
to `allow` (its default value is usually `deny`). This solution was adapted
to improve performance of applications that don't make use complementing
categories and to provide a solution for preventing the use of categories
to break object encapsulation. Note that the `complements` flag can be set
on a per-object basis by using the `set_logtalk_flag/2` directive.
