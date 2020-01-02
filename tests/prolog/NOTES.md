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


This directory contains a set of unit tests for Prolog official and de facto
standard features. Most of these unit tests are taken from the official ISO
Prolog standard (updated up to the ISO/IEC 13211-1:1995/Cor.2:2012(en)
standard). Several tests originate from SICS, ECLiPSe, and SWI-Prolog and are
used here with permission.

This conformance suite also includes unit tests for Prolog features that are
slowly becoming de facto standards (e.g. the `(*->)/2` soft-cut control
construct or the `setup_call_cleanup/3` built-in predicate). These tests are
usually skipped, however, when running on a system that doesn't provide such
features.

Writing these tests was made easier by rewriting and, whenever necessary,
updating, the tests found on the ISO Prolog conformance testing framework
written by Péter Szabó and Péter Szeredi, who gracefully allowed me to reuse
their hard work. The framework is described in the following paper:

	@incollection{
		year={2006},
		isbn={978-3-540-36635-5},
		booktitle={Logic Programming},
		volume={4079},
		series={Lecture Notes in Computer Science},
		editor={Etalle, Sandro and Truszczyński, Mirosław},
		doi={10.1007/11799573_20},
		title={Improving the ISO Prolog Standard by Analyzing Compliance Test Results},
		url={http://dx.doi.org/10.1007/11799573_20},
		publisher={Springer Berlin Heidelberg},
		author={Szabó, Péter and Szeredi, Péter},
		pages={257-269},
		language={English}
	}

The test identifier prefixes indicate their origin:

- `iso_` - tests from the ISO Prolog standards
- `sics_` - tests contributed by SICS developers
- `eclipse_` - tests contributed by ECLiPSe developers
- `swi_` - tests contributed by SWI-Prolog developers
- `eddbali_` - tests from A Ed-Dbali's test suite
- `lgt_` - tests originating from work on Logtalk portability
- `commons_` - tests for de facto standard Prolog features

To run all the provided tests with e.g. SWI-Prolog, open a terminal and type:

	$ cd $LOGTALKUSER/tests/prolog
	$ logtalk_tester -p swi
	...

By convention, tests for standard built-in predicates encapsulate the main
test goal using the `{}/1` control construct. In most cases, this precaution
is not necessary as the calls would be compiled as-is. An exception is the
input/output predicates that are affected by operator declarations (which
otherwise would be compiled to ensure that entity declared operators are
local to the entities as required by Logtalk semantics).

Tests that would require a portable way of specifying a source file text
encoding plus a common extended text encoding (e.g. UTF-8) are currently
skipped.

Tests that are specified as undefined in the standards due to the potential
of creating cyclic terms are skipped when using a backend Prolog compiler
that either doesn't support cyclic terms or whose support for cyclic terms
is limited. Currently, these tests are only run when using SICStus Prolog,
SWI-Prolog, or YAP as the backend Prolog compiler.

Failure of unit tests doesn't necessarily mean that a backend Prolog compiler
is faulty. Standards are not perfect and there isn't always a community
consensus on what should be the correct test results. Common causes of failure
include:

- corner cases in arithmetic operations where several systems provide
non-conforming but otherwise valid results
- type checking of output arguments of standard predicates, which is
problematic from both semantics and performance perspectives

Some Prolog compilers provide a strict ISO mode that may result in different
test results. This strict mode, when made available, is usually only used if
it's the default when starting Logtalk.

The ISO Prolog standard seems to allow two interpretations for the exception
that should be generated when using an atom where a stream identifier or a
stream alias is expected. A stream alias should only exist as long as the
corresponding stream is open. Therefore, an atom that is not a stream alias
can be interpreted as either a domain error (the atom is not a member of the
current set of aliases) or a (stream) existence error (as there's no stream
with such an alias). Currently, we accept both exception terms.

In some tests that check that an error condition generates the expected
exception term, alternative exception terms are accepted iff the correct
exception type is generated and the terms only differ on the culprit
argument. For example, accepting `type_error(callable,1)` where the term
`type_error(callable,(fail,1))` is expected. Another example is when the
exception term contains a module-qualified culprit. For example, the
system generating instead a `type_error(callable,user:1)` exception.
