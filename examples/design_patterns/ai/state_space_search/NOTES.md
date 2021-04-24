________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>  
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


Design pattern:
	State-space search

Description:
	Allow defining state-spaces (characterized by definitions
	of initial states, goal states, and state transitions) and
	applying different search strategies (blind or heuristic)
	to find solutions (represented by a sequence of transitions
	between an initial state and a final state).

Related examples:
	The `examples/searching` example implements popular
	state-spaces often used in AI courses and common blind
	and heuristic search methods.

This design pattern is usually implemented such that a given state-space
can be searched using any search method and a given search method can be
applied to any state-space. I.e. the state-spaces and search methods
implementations are kept orthogonal for maximum flexibility.
