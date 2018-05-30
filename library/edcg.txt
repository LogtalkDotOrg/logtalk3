________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>

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


To load all entities in this group load the `edcg_loader.lgt` utility file:

	| ?- logtalk_load(library(edcg_loader)).

This library provides a Logtalk port of the Peter Van Roy's extended DCG
implementation. For full documentation on EDCGs, see:

	https://www.info.ucl.ac.be/%7Epvr/edcg.html

This Logtalk version defines an hook object, `edcg`. Source files defining
EDCGs should either be compiled using the compiler option `hook(edcg)` or
by adding the following directive at the beginning of the file:

	:- set_logtalk_flag(hook, edcg).

The hook object automatically adds the EDCGs `-->>` infix operator scoped
to the source file.

This port has simplified by copying and then modifying Michael Hendricks's
`edcg` repo at:

	https://github.com/mndrix/edcg

A notable difference is that Michael's version declares Peter's original
predicates for declaring accumulators and predicates using the hidden
arguments as multifile predicates. But this is risky as two independent
EDCGs may use e.g. the same accumulator names and introduce conflicts.
The Logtalk version uses instead the `edcg` hook object internal state
to temporarily save those predicates in order to parse the corresponding
EDCGs.
