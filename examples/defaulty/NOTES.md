________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>  
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

This example compares defaulty and tagged data representations using the
`ports_profiler` tool. The example defines both `defaulty` and `tagged`
objects implementing the same predicate. This predicate, `count_atomics/3`,
counts the number of atoms and the number of numbers in a list that can
contain atoms, numbers, and other types of terms that are irrelevant.
Running the same query for both representations, the `ports_profiler` tool
is used to highlight their performance difference.

For a detailed analysis of this example, see the following blog post:

https://logtalk.org/2019/12/17/the-cost-of-defaulty-representations.html

