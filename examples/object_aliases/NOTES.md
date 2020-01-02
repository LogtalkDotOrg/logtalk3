________________________________________________________________________

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
________________________________________________________________________


To load this example and for sample queries, please see the `SCRIPT.txt`
file.

This is a minimal example illustrating the use of the `uses/1` directive
to experiment with different object implementations of the same protocol
when using explicit message sending. The main idea is to have a single
source line that can be edited to switch between different implementations
as all message sending calls would be written using the object alias.
This is an alternative to accomplish the same goal by using a `uses/2`
directive and implicit message sending. Note that both alternatives allow
compiling the code in optimized mode to take advantage of static binding
for the message sending calls.
