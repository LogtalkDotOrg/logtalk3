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

This example illustrates using expected terms to call a conjunction of
goals where any of them may cause an error condition without using the
traditional catch/throw mechanism. This solution enables us to:

1. Abstract the error handling code that would be required to test if
the previous goal generated an error before calling the next goal in
the conjunction.

2. Allow the next goal to decide what to do with the result passed by
the previous goal. For example, recovering from an unexpected error or
just passing the unexpected error to the next goal.

3. Allow postponing handling an error in one of the goals, facilitating
composition (e.g. adding more goals to a conjunction of goals or using
some of the goals in a different context).

4. Handle a goal failure by passing the reason for the failure to the
next goal, thus providing a common solution to handle both failures and
errors.

The original example is taken from standardization proposals and online
discussions on expected values in the context of other OOP languages such
as C++. For example:

https://blog.tartanllama.xyz/optional-expected/

For details please see the comments in the `cascade.lgt` source file. The
`cascade_dcgs.lgt` file contains an alternative implementation of the
`cascade` object using DCGs.

See also the `missing_data` and `books` examples.
