________________________________________________________________________

This file is part of Logtalk <http://logtalk.org/>  
Copyright 1998-2015 Paulo Moura <pmoura@logtalk.org>

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

This example contains simple wrappers for Prolog profiler tools, requiring
either SICStus Prolog 4 or YAP. The example includes two files:

- `yap_profiler.lgt`  
	simple wrapper for the YAP count profiler 

- `sicstus_profiler.lgt`  
	simple wrapper for the SICStus Prolog 4 profiler

Logtalk also supports the YAP tick profiler (using the latest YAP devel
version) and the SWI-Prolog XPCE profiler. When using the XPCE profiler,
you can avoid profiling the Logtalk compiler (which is invoked e.g. when
you use the `(::)/2` message-sending operator at the top-level) by typing:

	| ?- logtalk << (prolog_statistics:profile(... :: ...)).

Given that `prolog_statistics:profile/1` is a meta-predicate, Logtalk will
compile its argument before calling it. Don't forget, however, to load the
`prolog_statistics` module *before* using or compiling calls to the `profile/1`
predicate by typing:

	| ?- use_module(library(statistics)).

The profiler support makes no attempt to conceal the internal Logtalk
compiler/runtime predicates or the generated predicates that implement
predicate inheritance. Calls to internal compiler and runtime predicates
have functors starting with `$lgt_`. Calls to predicates with functors
such as `_def`, `_dcl`, or `_super`, used to implement inheritance, may
be listed when your code uses dynamic binding.

In order to get user-level object and predicate names instead of compiler
generated names when using the SWI-Prolog and YAP profilers you must set
`code_prefix` flag to a value other than the default `$`. For example:

	| ?- set_logtalk_flag(code_prefix, '.').

See also the `settings-sample` file for automating the necessary setup at
Logtalk startup.

All source files are formatted using tabs (the recommended setting is a
tab width equivalent to 4 spaces).
