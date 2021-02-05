________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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

This example requires using SWI-Prolog 7.x as the backend Prolog compiler.
It illustrates using a SWI-Prolog native dictionary term for representing
a parametric object parameters. This is accomplished simply by passing a
dict as the single object parameter.

Know issue
----------

Don't use dot notation, `./2`, when working with dicts within objects and
categories. SWI-Prolog provides different semantics for compiled versus
asserted clauses that contain `./2` terms and that can clash with Logtalk
dynamic binding caching and inlining optimizations (usually resulting
in instantiation errors).
