---
jupyter:
  jupytext:
    text_representation:
      extension: .md
      format_name: markdown
      format_version: '1.3'
      jupytext_version: 1.16.7
  kernelspec:
    display_name: Logtalk
    language: logtalk
    name: logtalk_kernel
---

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

# flow_networks_modeling

This example shows how to model project selection, image segmentation, and
baseball elimination as network flow problems using the `flow_networks`
library.

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Load the example:

```logtalk
logtalk_load(flow_networks_modeling(loader)).
```

All examples use the `flow_network(avltree)::new(Vertices, Arcs, Network)`
predicate. The explicit vertices include isolated projects, pixels, and teams.
Synthetic vertices are tagged (`flow_example_source/1`, `flow_example_sink/1`,
and domain-specific node functors), so arbitrary ground input identifiers
cannot collide with terminals or one another's node kinds. Arc capacities are
instance-derived non-negative integers. Integer arithmetic is left exact;
on a bounded-integer Prolog, arithmetic overflow is reported by that backend
instead of being saturated or silently wrapped.

## Maximum-weight closure and project selection

Call:

```logtalk
project_selection::solve(
    [project(design, 8), project(build, 5), project(maintenance, -4)],
    [requires(build, design), requires(build, maintenance)],
    Selected, Weight).
```

<!--
Selected = [design, build, maintenance], Weight = 9.
-->

Weights are signed integers, identifiers are ground and unique, and every
dependency must name a declared project. `Selected` is in input order;
multiple optima are allowed. Dependencies may form cycles, and zero-weight
projects are retained as vertices.

For each positive-weight project, add a source arc of that weight. For each
negative-weight project, add an arc to the sink of capacity equal to its
loss. A dependency `requires(U,V)` adds an arc from U to V. Let
`P = sum(max(Weight, 0))`; the dependency capacity is `M = P + 1`.
Selecting no projects cuts at most P (the source arcs), whereas any cut that
violates even one dependency costs at least M. Thus a minimum cut cannot
violate a dependency. The selected closure is the project vertices on the
source side. Its original objective value is decoded as `P - CutValue`.
The test oracle enumerates every project subset, checks closure feasibility,
and independently computes its signed-weight sum.

## Binary synthetic-image segmentation

Call:

```logtalk
image_segmentation::solve(
    [p1, p2, p3],
    [unary(p1, 0, 4), unary(p2, 2, 0), unary(p3, 1, 3)],
    [neighbor(p1, p2, 2), neighbor(p2, p3, 1)],
    Labels, Energy).
```

<!--
Labels = [p1-background, p2-background, p3-background], Energy = 3.
-->

Every pixel needs exactly one unary record. Unary background/foreground
penalties and pairwise disagreement penalties are non-negative integers.
Repeated neighbor records are independent terms in the energy; self-neighbor
terms never disagree and are harmless.

Source-side pixels decode as `foreground`. For a pixel, source-to-pixel
capacity is its background penalty and pixel-to-sink capacity is its
foreground penalty. Each undirected neighbor penalty is represented by two
opposite arcs of equal capacity. Exactly one of these arcs crosses a cut when
the labels disagree, so the penalty is paid once, not twice. `Energy` is
computed from the returned labels using the original unary and pairwise
records, and is checked against the cut value. Tests enumerate all binary
labelings and independently evaluate the original energy. This example does
not perform image I/O or multi-label optimization.

## Baseball elimination (ties allowed)

Call:

```logtalk
baseball_elimination::analyze(
    [team(red, 50, 3), team(blue, 48, 3), team(gold, 47, 4)],
    [game(blue,gold,2), game(blue,red,1), game(gold,red,2)],
    red, Result).
```

<!--
Result = not_eliminated([allocation(blue,gold,2,0), allocation(blue,red,0,1), allocation(gold,red,0,2)]).
-->

Teams use `team(Name, Wins, RemainingGames)`. The schedule uses exactly one
`game(Team1, Team2, Count)` per unordered pair, with `Team1 @< Team2`; that
single non-negative count is the symmetric game count for the pair. All pairs
must be present, including zero-count pairs, and per-team remaining counts
must agree with the schedule. These rules prevent asymmetric or contradictory
schedule data. A team is trivially eliminated if an opponent already has more
wins than its best possible total. This check occurs before constructing team
capacity bounds.

The team under consideration is credited with all its remaining wins.
For each unordered game between other teams, create one game vertex and an
arc from the source with capacity equal to that game's count. Add game-to-team
arcs for both teams and team-to-sink arcs with capacity
`maximum_wins - current_wins`. Let `G` be the total number of remaining
opponent-vs-opponent games. Game-to-team capacity `M = G + 1` is finite and
sufficient: a complete allocation never sends more than G units through any
such arc, while cutting one such arc costs more than cutting all source-game
arcs. The schedule is feasible exactly when maximum flow equals G, which
forces every source-game arc to be saturated. The returned
`not_eliminated(Allocations)` gives a complete result for every scheduled
game, awarding all games involving the selected team to it and decoding
opponent game wins from the flow.

Otherwise, let R be the opponent team vertices on the source side of a
minimum cut. The returned `eliminated(certificate(R, Left, Right))` reports
`Left = sum(wins(R)) + games_within(R)` and
`Right = |R| * maximum_wins`; the cut inequality guarantees `Left > Right`.
This includes nontrivial certificates, not only a single team already ahead.
If strict first place is required instead, a candidate must finish at most
`maximum_wins - 1`; use that smaller bound for opponent team capacities and
trivial-elimination checks.

The tests compare feasibility with a separate exhaustive integer allocation
oracle on small schedules, verify returned schedules, and check a
nontrivial certificate inequality. Ties for first are therefore explicitly
accepted in the current API.

Run the tests:

```logtalk
logtalk_load(flow_networks_modeling(tester)).
```
