________________________________________________________________________

This file is part of Logtalk <http://logtalk.org/>  
Copyright 1998-2017 Paulo Moura <pmoura@logtalk.org>

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


To load this example and for sample queries, please see the `SCRIPT.txt` file.

This folder contains an example of representation and handling of relations
using events. We have instances of class `block` and a binary `block_stack`
relation between the blocks. Every time we move a block, we want the blocks
on top of it to move along. If we break the stack by moving a middle block,
we want to automatically destroy the corresponding relation tuple.

It's instructive to use the debugger to better understand this example.
Set spy points in all block instances and then activate the debugger.
