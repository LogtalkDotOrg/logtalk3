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

This example illustrates how to use the `begin_of_file` term generated
when compiling a source file to create an object wrapper for the code
in a plain Prolog file. Assuming that the `context_switching_calls` is
set to `allow`, the generated object predicates can be called using the
`<</2` debugging control construct for testing. This wrapper is useful
for e.g. using the Logtalk compiler lint checks to examine predicate
call dependencies of the wrapped code and also to look for possible
portability issues when the `portability` flag is set to `warning`.
