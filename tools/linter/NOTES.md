
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


`linter`
========

Logtalk provides a built-in linter tool that runs automatically when
compiling and loading source files. The lint warnings are controlled
by a [set of flags](../userman/programming.html#programming-flags-lint).
The default values for these flags are defined in the backend Prolog
compiler adapter files and can be overriden from a settings file or
from a source file (e.g. a loader file). These flags can be set globally
using the [set_logtalk_flag/2](../refman/predicates/set_logtalk_flag_2.html)
built-in predicate. For (source file or entity) local scope, use instead
the [set_logtalk_flag/2](../refman/directives/set_logtalk_flag_2.html)
directive.
