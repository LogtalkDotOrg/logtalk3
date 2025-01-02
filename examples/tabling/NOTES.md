________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
SPDX-FileCopyrightText: 1998-2025 Paulo Moura <pmoura@logtalk.org>  
SPDX-License-Identifier: Apache-2.0

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

This folder contains an example of using tabled predicates within objects.
Currently supported compilers include B-Prolog, XSB, SWI-Prolog (when the
`tabling` library is available), and YAP (when compiled with tabling enabled).

Current tabling implementations don't provide a solution for ignoring the
implicit execution-context argument that the Logtalk compiler adds to all
compiled predicates. Thus, different object sending a message for a tabled
predicate will result in the equivalent of multiple tables. The same will
happen if the sender of the message is a parametric object and different
parameterizations are used. A possible workaround is to always send the
message from the same (non-parametric) object (e.g., `user`).
