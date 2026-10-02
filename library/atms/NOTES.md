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


`atms`
======

This library implements the Horn fragment of Johan de Kleer's Assumption-based
Truth Maintenance System (ATMS). The system maintains assumption nodes, ordinary
nodes, Horn justifications, minimal labels, and minimal nogoods. A label is an
antichain of the minimal consistent environments supporting a node.


API documentation
-----------------

Open the [../../apis/library_index.html#atms](../../apis/library_index.html#atms)
link in a web browser.


Loading
-------

To load this library, load the `loader.lgt` file:

	| ?- logtalk_load(atms(loader)).


Testing
-------

To test this library, load its `tester.lgt` file:

	| ?- logtalk_load(atms(tester)).

The test suite runs the same deterministic regressions and bounded QuickCheck
edit histories for all three environment representations. Generated histories
contain 20 edits and are limited to eight live nodes and four assumptions.
After every edit, an independent ground model is reconstructed using public
creation and justification predicates and compared with the current state,
including labels, roles, nogoods, interpretations, explanations, rule order,
and the next node identifier. Removed IDs are represented by ordinary padding
nodes only in the reconstruction. Previous states are also checked again.

QuickCheck test declarations reset the generator to its reproducible default
seed in `setup/1` and restore the previous seed in `cleanup/1`;
the test suite verifies that repeating the seed reproduces the histories.
QuickCheck failures report the failing history and sequence/test seeds. A failure
can be reproduced using `lgtunit::quick_check/3` with its reported opaque seed
and the template `tests(Representation)<<edit_sequence(+list(byte,20))`.
Nonground `Info` identity and aliasing remain covered by deterministic tests,
without copying variables into the generated oracle.


API and state
-------------

The `atms` object uses functional state:

	| ?- atms::new(atms_bitset_environment, S0),
	     atms::create_assumption(sensor_ok, S0, A, S1),
	     atms::create_node(alarm, S1, Alarm, S2),
	     atms::justify(Alarm, [A], sensor_rule, S2, S3),
	     atms::label(Alarm, S3, Label).

`Label` is `[[node(0)]]`. Node identifiers are intentionally opaque; use
`atms::nodes/2` to obtain their associated data. State threading is a deliberate
portability choice: it supports multiple independent ATMSs without dynamic
predicates, global counters, or a dependence on a particular Prolog backend.

`justify/5` incrementally propagates only newly accepted label environments via
a reverse index from antecedent nodes to justifications. A contradiction node is
created with `create_contradiction/4`; every environment in its label becomes a
nogood and prunes supersets from all labels immediately.

`interpretations/2` enumerates maximal consistent environments. With no
nogoods, it returns the environment containing all assumptions directly. When
nogoods require subset search, it is intended for small sets of assumptions.


Retraction
----------

`retract_justification(Consequent, Antecedents, Info, State, NewState)` removes
one matching Horn justification. Antecedents are sorted and deduplicated as in
`justify/5`. Matching uses strict term identity (`==/2`) for the complete
justification, including `Info`, rather than unification. Rules with different
`Info` terms are distinct even when they have the same consequent and
antecedents. For nonground `Info` terms, matching requires the same variables.

`retract_justifications(Justifications, State, NewState)` removes all stored
occurrences matching any target in the list. Each target is a
`justification(Consequent, Antecedents, Info)` term, with the same antecedent
normalization and identity matching as single-rule removal. Duplicate requests
and missing targets have no additional effect. The surviving rules retain
their order, and the batch rebuilds derived state only once if any rule is
removed.

Rules that were distinct at insertion can become identical if variables in
their `Info` terms are later unified. Rebuilding preserves every surviving
occurrence and its original variables. Single-rule removal deletes the most
recently added matching occurrence; repeated single calls can therefore remove
further aliased occurrences. Batch removal deletes all matching occurrences
and is idempotent even in this case. Public `justify/5` still suppresses rules
that are identical when they are added.

`retract_node(Node, State, NewState)` removes an ordinary, assumption, or
contradiction node and every justification that mentions it as consequent or
antecedent. It does not delete downstream nodes: they remain queryable, with
empty labels if they lose all support. Removing an assumption also removes it
from the environments returned by labels, nogoods, and interpretations.

`retract_nodes(Nodes, State, NewState)` removes all requested nodes and every
justification mentioning any of them. Duplicate node requests and missing
nodes are ignored. Like batch rule removal, it rebuilds at most once.

`retract_batch(Nodes, Justifications, State, NewState)` combines node and
justification removal in one operation. It removes every incident rule and
every exact requested rule match in a single traversal, then rebuilds at most
once. Duplicate and overlapping requests do not remove an occurrence twice;
missing targets are ignored. It follows the same identity and identifier
rules as the separate batch operations:

	| ?- atms::new(S0),
	     atms::create_assumption(sensor_ok, S0, A, S1),
	     atms::create_assumption(backup_ok, S1, B, S2),
	     atms::create_node(alarm, S2, Alarm, S3),
	     atms::justify(Alarm, [A], first, S3, S4),
	     atms::justify(Alarm, [B], second, S4, S5),
	     atms::retract_batch([B], [justification(Alarm, [A], first)], S5, S6),
	     atms::label(Alarm, S6, []),
	     atms::assumptions(S6, [A]).

All removal operations are deterministic for their documented input modes.
Node and batch removal are idempotent; single-rule removal is idempotent when
there is at most one matching stored occurrence. If no target is present, the
input state is returned unchanged. A justification referring to an unknown or
already removed node is also absent.

Empty batches return immediately, without scanning stored rules. Rule filters
reuse unchanged list suffixes and preserve the original list when no rule is
removed, including any variables in surviving justification information.
Existing input states remain usable and are never modified:

	| ?- atms::new(S0),
	     atms::create_assumption(sensor_ok, S0, A, S1),
	     atms::create_node(alarm, S1, Alarm, S2),
	     atms::justify(Alarm, [A], sensor_rule, S2, S3),
	     atms::retract_justification(Alarm, [A], sensor_rule, S3, S4),
	     atms::label(Alarm, S4, []),
	     atms::retract_node(A, S3, S5),
	     atms::nodes(S5, [Alarm-alarm]),
	     atms::label(Alarm, S5, []),
	     atms::label(Alarm, S3, [[A]]).

The following query shows both batch operations on separate descendants of the
same input state:

	| ?- atms::new(S0),
	     atms::create_assumption(sensor_ok, S0, A, S1),
	     atms::create_assumption(backup_ok, S1, B, S2),
	     atms::create_node(alarm, S2, Alarm, S3),
	     atms::justify(Alarm, [A], first, S3, S4),
	     atms::justify(Alarm, [B], second, S4, S5),
	     atms::retract_justifications([
	         justification(Alarm, [A], first),
	         justification(Alarm, [B], second)
	     ], S5, S6),
	     atms::label(Alarm, S6, []),
	     atms::retract_nodes([A, B, A], S5, S7),
	     atms::nodes(S7, [Alarm-alarm]),
	     atms::label(Alarm, S7, []).

Node identifiers and the selected environment representation are preserved.
Removed identifiers are not reused in descendant states; new nodes continue
from the previous next identifier. `clear/2` still creates an empty state with
identifiers starting over. Surviving justifications retain their insertion
order in `justifications/2` and `why/3`.

General retractions rebuild all labels, nogoods, and reverse dependency
indexes from surviving assumptions and stored justifications. Replay uses
already normalized rules without repeating public validation or duplicate
checks. This restores previously subsumed supports and environments pruned
by nogoods, while preventing unsupported cycles from retaining stale labels.
Nogoods disappear only when no surviving contradiction derivation supports
them; retracting a contradiction node or justification can therefore restore
consistency and labels throughout the system. Additions remain incremental,
and new nodes and justifications can be added normally after retraction.

Removing isolated ordinary or contradiction nodes takes a fast path: if no
assumption or incident justification is removed, only node, role, and label
entries are deleted. Other labels, nogoods, and dependency indexes are retained.
Both antecedent and consequent references are checked, including unconditional
rules. Assumption removals and any node removal affecting a rule use the
rebuild fallback. This optimization applies to both single-node and batch
removals.

Rule-only removal also takes a fast path when every removed rule has a
surviving rule with the same consequent and canonical antecedents. `Info` is
ignored for this logical redundancy check, but never for target matching.
Labels and nogoods are retained; stored rules and reverse dependency lists
lose exactly the removed occurrences. This includes unconditional and
contradiction rules and rules made identical by variable aliasing. Removing
the final occurrence of any signature rebuilds derived state, even when a
different signature currently produces the same label. Mixed batches use this
fast path only when no node is actually removed.


Environment representations
---------------------------

The public API always accepts and returns environments as ordered lists of node
identifiers. The internal representation is selected with `new/2` and conforms
to `atms_environment_protocol`:

- `atms_ordered_list_environment` is the portable default. Storage is linear in
  the number of assumptions and union and subset use linear merges. It is a
  good choice for small or very sparse environments and is independent of
  integer bit width, but stores a full node term per assumption.
- `atms_bitset_environment` stores environments as non-negative integer bitsets.
  Union and subset each use one integer operation whose runtime cost scales
  with the represented bit width. It is compact and fast for dense environments
  with modest highest identifiers. Large gaps or high identifiers create wide
  integers, and the usable width can be backend-dependent. Use preferably with
  backends supporting unbound integers.
- `atms_segmented_bitset_environment` stores canonical sparse ordered lists of
  nonempty 16-bit blocks. Storage is linear in the number of occupied blocks;
  union and subset use linear block merges and bounded-width bit operations. It
  is a good choice for larger environments or sparse identifier ranges with
  local clustering, but pair and list overhead can exceed ordered lists when
  assumptions occupy separate blocks.

Applications can provide another representation object implementing `empty/1`,
`singleton/2`, `union/3`, `subset/2`, `equal/2`, `from_list/2`, and `to_list/2`.
This keeps the ATMS algorithm independent of storage choices such as ordered
sets, bit vectors, or a future BDD implementation.


Limitations
-----------

- The library implements a Horn ATMS. It does not provide non-Horn
  justifications, focus management, demand-driven labels, or recursive proof
  explanations. The `why/3` predicate reports only the direct stored
  justifications for a node, in insertion order.
- Node identifiers are scoped to a state lineage, not globally unique.
  Independently created states and sibling states can contain equal identifiers
  for different nodes. Applications must not pass a node identifier to an
  unrelated state, where it could silently denote a different local node.
- Labels and nogoods can contain exponentially many minimal environments in the
  number of assumptions. Combining antecedent labels can therefore require an
  exponential number of union and subset checks. Products are accumulated
  directly into the minimal antichain and inconsistent partial products are
  pruned, avoiding materialization of the complete Cartesian product.
- The functional state uses persistent AVL dictionaries for keyed indexes.
  Adding a nogood must nevertheless map over every node label, while label and
  nogood antichain operations require linear subset scans. These costs can
  dominate updates in systems with many nodes or large antichains.
- General retraction is not incremental: it recomputes the complete derived
  state, except for isolated non-assumption node removals and logically
  redundant rule removals. Its cost can be comparable to constructing the
  surviving system anew, including potentially exponential label and nogood
  antichains. Batch removal filters the requested targets before rebuilding at
  most once; repeated single removals can rebuild repeatedly. Justification
  batches with multiple targets index complete ground rule terms in a
  temporary AVL dictionary. Ground membership requires logarithmic key
  comparisons; nonground terms retain strict-identity linear scans to preserve
  variable identity. Node batches similarly index the actual removed node IDs.
  Empty and singleton batches avoid index construction. Even isolated-node
  removal scans stored rules to check for incident references. Identifier gaps
  are not compacted and can increase the bitset representation costs described
  below.
- The number of maximal environments returned by `interpretations/2` and the
  time required to find them can still be exponential in the number of
  assumptions. When nogoods are present, the implementation explores subsets
  incrementally and prunes inconsistent branches, so it does not materialize
  the complete powerset. With no nogoods, it returns the complete assumption
  environment directly.
- The bitset representation uses the global node identifier as the bit index.
  Its arithmetic and storage costs therefore depend on the highest node
  identifier, not just on the number of assumptions. Large or sparse node
  identifiers can be expensive, and the maximum usable identifier is
  backend-dependent where integer arithmetic is bounded. Use the ordered-list
  or segmented bitset representation when this limit may be reached.
- Alternative environment representations are trusted to implement canonical
  conversion, union, subset, and equality consistently. Protocol conformance
  is checked when creating a state, but these algebraic properties are not
  validated at runtime.


References
----------

- de Kleer, Johan. "An Assumption-Based TMS.” *Artificial Intelligence* 28(2),
  127–162, 1986. <https://doi.org/10.1016/0004-3702(86)90080-9>
- de Kleer, Johan. "Extending the ATMS.” *Artificial Intelligence* 28(2),
  163–196, 1986.
- de Kleer, Johan. "Problem Solving with the ATMS.” *Artificial Intelligence*
  28(2), 197–224, 1986.
- de Kleer, Johan. "A General Labeling Algorithm for Assumption-Based Truth
  Maintenance.” *Proceedings of the Seventh National Conference on Artificial
  Intelligence (AAAI-88)*, 1988. <https://cdn.aaai.org/AAAI/1988/AAAI88-034.pdf>
- Forbus, Kenneth D., and Johan de Kleer. *Building Problem Solvers.* MIT Press,
  1993.
