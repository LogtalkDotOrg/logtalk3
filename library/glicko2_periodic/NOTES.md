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


`glicko2_periodic`
===================

Multi-period Glicko-2 ranker over temporal pairwise game datasets.

This library implements the `ranker_protocol` defined in the
`ranking_protocols` library. It learns one rating per item from datasets
implementing the `temporal_pairwise_ranking_dataset_protocol` protocol,
processing declared rating periods in order and applying simultaneous
Glicko-2 player updates inside each period.

Draws are represented directly using game scores on the set `{0.0, 0.5,
1.0}`. Players who are inactive in a declared period keep their rating and
volatility while their rating deviation is inflated for that period.

Players are initialized when they first play instead of being forced to
appear in the first declared period.

The learned ranker is represented by the compound term:

    glicko2_periodic_ranker(Items, Ratings, Diagnostics)

where `Ratings` stores `Item-Rating` pairs and `Diagnostics` stores metadata
including the effective options, per-item rating deviations, per-item
volatilities, processed periods, and the training dataset summary.

Load with:

    | ?- logtalk_load(glicko2_periodic(loader)).

Test with:

    | ?- logtalk_load(glicko2_periodic(tester)).

The supported options are `initial_rating/1`, `initial_deviation/1`,
`initial_volatility/1`, `tau/1`, and `volatility_tolerance/1`.
