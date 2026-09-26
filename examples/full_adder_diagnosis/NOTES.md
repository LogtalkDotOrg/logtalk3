<!--
________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>
SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
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
-->

# full_adder_diagnosis

This example illustrates model-based diagnosis on top of the `atms`
library, using the classical multiplier-adder circuit from:

- Reiter, Raymond. "A Theory of Diagnosis from First Principles."
  *Artificial Intelligence* 32(1), 57-95, 1987.
- de Kleer, Johan, and Brian C. Williams. "Diagnosing Multiple Faults."
  *Artificial Intelligence* 32(1), 97-130, 1987.

(The circuit computes two sums of products rather than a binary sum, so
it is not a full adder in the digital-logic sense; the "full-adder"
label is how this worked example is commonly known in diagnosis course
notes, and is kept here for that reason.)

The circuit has three multipliers and two adders:

	X1 = A * C     (M1)
	X2 = B * D     (M2)
	X3 = B * E     (M3)
	Y1 = X1 + X2   (A1)
	Y2 = X2 + X3   (A2)

With inputs A=3, B=2, C=2, D=3, E=3, a correctly functioning circuit
predicts X1=X2=X3=6, Y1=12, Y2=12. The scenario observes Y1=10 and
Y2=12: Y2 matches the prediction, but Y1 does not.

Each component gets a Horn justification of the form "if the component
is assumed to behave normally (`ok(Component)`), and its inputs have
their derived values, then its output has its derived value." The two
observed readings are recorded as premises (justified by the empty
environment). The Y1 mismatch is then recorded with a contradiction
node justified from both the predicted and the observed value nodes;
`atms` immediately turns every environment supporting that node into a
nogood and prunes it from every label.

Because `atms::interpretations/2` returns the maximal consistent
environments over all the health assumptions, and there is a single
minimal conflict here, its three results are exactly the three
classical minimal single-fault diagnoses: {M1}, {M2}, {A1}. The
`full_adder_diagnosis` object computes them directly by subtracting
each interpretation from the full assumption set.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Load the example:

```logtalk
logtalk_load(full_adder_diagnosis(loader)).
```

Build the circuit ATMS and inspect the recorded nogood:

```logtalk
full_adder_diagnosis::build(State), atms::nogood(Nogood, State).
```

<!--
State = ..., Nogood = [node(0), node(1), node(3)].
-->

Translate it back into component identifiers:

```logtalk
full_adder_diagnosis::build(State),
atms::nodes(State, Nodes),
atms::nogood(Nogood, State),
full_adder_diagnosis::node_data(Nogood, Nodes, Data).
```

<!--
Data = [ok(m1), ok(m2), ok(a1)].
-->

Compute the minimal single-fault diagnoses:

```logtalk
full_adder_diagnosis::build(State), full_adder_diagnosis::diagnoses(State, Diagnoses).
```

<!--
Diagnoses = [[ok(a1)], [ok(m1)], [ok(m2)]].
-->

Print a full report:

```logtalk
full_adder_diagnosis::report.
```

<!--
Nodes:
  node(0) -> ok(m1)
  node(1) -> ok(m2)
  node(2) -> ok(m3)
  node(3) -> ok(a1)
  node(4) -> ok(a2)
  node(5) -> val(x1,6)
  node(6) -> val(x2,6)
  node(7) -> val(x3,6)
  node(8) -> val(y1,12)
  node(9) -> val(y2,12)
  node(10) -> obs(y1,10)
  node(11) -> contra(y1)
  node(12) -> obs(y2,12)

Nogoods (minimal conflict sets):
  [ok(m1),ok(m2),ok(a1)]

Minimal single-fault diagnoses:
  [ok(a1)]
  [ok(m1)]
  [ok(m2)]
true.
-->
