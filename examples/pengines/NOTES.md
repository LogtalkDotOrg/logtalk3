________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
SPDX-FileCopyrightText: 2020 Paulo Moura <pmoura@logtalk.org>  
SPDX-FileCopyrightText: 2020 Michael T. Richter  
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


# pengines

This example illustrates how to use SWI-Prolog Pengines from objects. It
is based on simple example provided in the Pengines documentation:

https://www.swi-prolog.org/pldoc/man?section=pengine-examples

The main issue when reusing the original example code from within an object
is that the `pengines:pengine_create/1` meta-predicate template is ambiguous
due to the use of `:` as the meta-predicate argument specifier (Logtalk is
not based on a predicate-prefixing mechanism as used by most Prolog module
systems). Thus, we must override the template using the following directive
to avoid a compilation error:

	:- meta_predicate(pengines:pengine_create(*)).

Two object versions are provided. The first version, `dumper`, uses the
original example code plus the overriding directive above to write all
the pengine answers to the current output.

The second version, `engines`, uses a threaded engine to provide an interface
to the pengine in order to access the answers on demand with (1) separate
predicates for creating the pengine and for querying its answers (including
easily collecting all query answers in a list) and (2) asking the pengine to
start computing the next solution when the current solution is retrieved.

The minimal `pengine_server` Prolog module code is also based on the pengines
documentation available at:

https://www.swi-prolog.org/pldoc/doc_for?object=section(%27packages/pengines.html%27)

Load the example:

```logtalk
logtalk_load(pengines(loader)).
```

<!--
true.
-->

Try the version that dumps the answers to the current output:

```logtalk
dumper::ask.
```

<!--
q(a)
q(b)
q(c)

true.
-->

Try the version that allows collecting the answers:

```logtalk
engines::(ask(Engine), answers(Engine, Answers)).
```

<!--
Engine = 1, Answers = [q(a), q(b), q(c)].
-->

Retrieve answers on demand:

```logtalk
engines::ask(my_question).
```

<!--
true.
-->

```logtalk
write('Doing something else ...').
```

<!--
Doing something else ...
true.
-->

```logtalk
engines::answer(my_question, Answer).
```

<!--
Answer = q(a) .
-->

```logtalk
write('Pause for tea ...').
```

<!--
Pause for tea ...
true.
-->

```logtalk
engines::answer(my_question, Answer).
```

<!--
Answer = q(b) .
-->

```logtalk
write('Any answers left?').
```

<!--
Any answers left?
true.
-->

```logtalk
engines::answers(my_question, Answers).
```

<!--
Answers = [q(c)].
-->

Use multiple engines concurrently:

```logtalk
engines::ask(E2).
```

<!--
E2 = 2.
-->

```logtalk
engines::ask(E3).
```

<!--
E3 = 3.
-->

```logtalk
engines::answer(2, A1).
```

<!--
A1 = q(a) .
-->

```logtalk
engines::answer(3, A2).
```

<!--
A2 = q(a) .
-->

```logtalk
%%table
engines::answer(2, A1).
```

<!--
A1 = q(b) ;
A1 = q(c) ;
false.
-->

```logtalk
engines::answer(3, A2).
```

<!--
A2 = q(b) .
-->

```logtalk
engines::answer(3, A2).
```

<!--
A2 = q(c) .
-->

```logtalk
engines::answer(3, A2).
```

<!--
false.
-->
