________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
Copyright 1998-2019 Paulo Moura <pmoura@logtalk.org>

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

This folder contains an implementation of a message priority queue using
a perpetual threaded engine holding the priority queue. At any moment, we
ask for a list of the pending messages ordered by priority.

A variant is also provided that splits top messages from normal messages
into separate queues. In this case, asking for a list of the pending
messages returns a list with top messages before the normal messages but
keeping the message sent order otherwise.
