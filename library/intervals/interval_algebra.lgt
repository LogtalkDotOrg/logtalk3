%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
%
%  Licensed under the Apache License, Version 2.0 (the "License");
%  you may not use this file except in compliance with the License.
%  You may obtain a copy of the License at
%
%      http://www.apache.org/licenses/LICENSE-2.0
%
%  Unless required by applicable law or agreed to in writing, software
%  distributed under the License is distributed on an "AS IS" BASIS,
%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%  See the License for the specific language governing permissions and
%  limitations under the License.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(interval_algebra,
	implements(interval_algebra_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-13,
		comment is 'Allen interval algebra over the 13 base relation atoms. Composition results are returned as canonical ordered duplicate-free lists.'
	]).

	relation(before).
	relation(after).
	relation(meets).
	relation(met_by).
	relation(overlaps).
	relation(overlapped_by).
	relation(starts).
	relation(started_by).
	relation(during).
	relation(contains).
	relation(finishes).
	relation(finished_by).
	relation(equal).

	converse(before, after).
	converse(after, before).
	converse(meets, met_by).
	converse(met_by, meets).
	converse(overlaps, overlapped_by).
	converse(overlapped_by, overlaps).
	converse(starts, started_by).
	converse(started_by, starts).
	converse(during, contains).
	converse(contains, during).
	converse(finishes, finished_by).
	converse(finished_by, finishes).
	converse(equal, equal).

	compose(Relation1, Relation2, Relations) :-
		compose_(Relation1, Relation2, Relations),
		!.

	compose_(before, before, [before]).
	compose_(before, after, [before, after, meets, met_by, overlaps, overlapped_by, starts, started_by, during, contains, finishes, finished_by, equal]).
	compose_(before, meets, [before]).
	compose_(before, met_by, [before, meets, overlaps, starts, during]).
	compose_(before, overlaps, [before]).
	compose_(before, overlapped_by, [before, meets, overlaps, starts, during]).
	compose_(before, starts, [before]).
	compose_(before, started_by, [before]).
	compose_(before, during, [before, meets, overlaps, starts, during]).
	compose_(before, contains, [before]).
	compose_(before, finishes, [before, meets, overlaps, starts, during]).
	compose_(before, finished_by, [before]).
	compose_(before, equal, [before]).
	compose_(after, before, [before, after, meets, met_by, overlaps, overlapped_by, starts, started_by, during, contains, finishes, finished_by, equal]).
	compose_(after, after, [after]).
	compose_(after, meets, [after, met_by, overlapped_by, during, finishes]).
	compose_(after, met_by, [after]).
	compose_(after, overlaps, [after, met_by, overlapped_by, during, finishes]).
	compose_(after, overlapped_by, [after]).
	compose_(after, starts, [after, met_by, overlapped_by, during, finishes]).
	compose_(after, started_by, [after]).
	compose_(after, during, [after, met_by, overlapped_by, during, finishes]).
	compose_(after, contains, [after]).
	compose_(after, finishes, [after]).
	compose_(after, finished_by, [after]).
	compose_(after, equal, [after]).
	compose_(meets, before, [before]).
	compose_(meets, after, [after, met_by, overlapped_by, started_by, contains]).
	compose_(meets, meets, [before]).
	compose_(meets, met_by, [finishes, finished_by, equal]).
	compose_(meets, overlaps, [before]).
	compose_(meets, overlapped_by, [overlaps, starts, during]).
	compose_(meets, starts, [meets]).
	compose_(meets, started_by, [meets]).
	compose_(meets, during, [overlaps, starts, during]).
	compose_(meets, contains, [before]).
	compose_(meets, finishes, [overlaps, starts, during]).
	compose_(meets, finished_by, [before]).
	compose_(meets, equal, [meets]).
	compose_(met_by, before, [before, meets, overlaps, contains, finished_by]).
	compose_(met_by, after, [after]).
	compose_(met_by, meets, [starts, started_by, equal]).
	compose_(met_by, met_by, [after]).
	compose_(met_by, overlaps, [overlapped_by, during, finishes]).
	compose_(met_by, overlapped_by, [after]).
	compose_(met_by, starts, [overlapped_by, during, finishes]).
	compose_(met_by, started_by, [after]).
	compose_(met_by, during, [overlapped_by, during, finishes]).
	compose_(met_by, contains, [after]).
	compose_(met_by, finishes, [met_by]).
	compose_(met_by, finished_by, [met_by]).
	compose_(met_by, equal, [met_by]).
	compose_(overlaps, before, [before]).
	compose_(overlaps, after, [after, met_by, overlapped_by, started_by, contains]).
	compose_(overlaps, meets, [before]).
	compose_(overlaps, met_by, [overlapped_by, started_by, contains]).
	compose_(overlaps, overlaps, [before, meets, overlaps]).
	compose_(overlaps, overlapped_by, [overlaps, overlapped_by, starts, started_by, during, contains, finishes, finished_by, equal]).
	compose_(overlaps, starts, [overlaps]).
	compose_(overlaps, started_by, [overlaps, contains, finished_by]).
	compose_(overlaps, during, [overlaps, starts, during]).
	compose_(overlaps, contains, [before, meets, overlaps, contains, finished_by]).
	compose_(overlaps, finishes, [overlaps, starts, during]).
	compose_(overlaps, finished_by, [before, meets, overlaps]).
	compose_(overlaps, equal, [overlaps]).
	compose_(overlapped_by, before, [before, meets, overlaps, contains, finished_by]).
	compose_(overlapped_by, after, [after]).
	compose_(overlapped_by, meets, [overlaps, contains, finished_by]).
	compose_(overlapped_by, met_by, [after]).
	compose_(overlapped_by, overlaps, [overlaps, overlapped_by, starts, started_by, during, contains, finishes, finished_by, equal]).
	compose_(overlapped_by, overlapped_by, [after, met_by, overlapped_by]).
	compose_(overlapped_by, starts, [overlapped_by, during, finishes]).
	compose_(overlapped_by, started_by, [after, met_by, overlapped_by]).
	compose_(overlapped_by, during, [overlapped_by, during, finishes]).
	compose_(overlapped_by, contains, [after, met_by, overlapped_by, started_by, contains]).
	compose_(overlapped_by, finishes, [overlapped_by]).
	compose_(overlapped_by, finished_by, [overlapped_by, started_by, contains]).
	compose_(overlapped_by, equal, [overlapped_by]).
	compose_(starts, before, [before]).
	compose_(starts, after, [after]).
	compose_(starts, meets, [before]).
	compose_(starts, met_by, [met_by]).
	compose_(starts, overlaps, [before, meets, overlaps]).
	compose_(starts, overlapped_by, [overlapped_by, during, finishes]).
	compose_(starts, starts, [starts]).
	compose_(starts, started_by, [starts, started_by, equal]).
	compose_(starts, during, [during]).
	compose_(starts, contains, [before, meets, overlaps, contains, finished_by]).
	compose_(starts, finishes, [during]).
	compose_(starts, finished_by, [before, meets, overlaps]).
	compose_(starts, equal, [starts]).
	compose_(started_by, before, [before, meets, overlaps, contains, finished_by]).
	compose_(started_by, after, [after]).
	compose_(started_by, meets, [overlaps, contains, finished_by]).
	compose_(started_by, met_by, [met_by]).
	compose_(started_by, overlaps, [overlaps, contains, finished_by]).
	compose_(started_by, overlapped_by, [overlapped_by]).
	compose_(started_by, starts, [starts, started_by, equal]).
	compose_(started_by, started_by, [started_by]).
	compose_(started_by, during, [overlapped_by, during, finishes]).
	compose_(started_by, contains, [contains]).
	compose_(started_by, finishes, [overlapped_by]).
	compose_(started_by, finished_by, [contains]).
	compose_(started_by, equal, [started_by]).
	compose_(during, before, [before]).
	compose_(during, after, [after]).
	compose_(during, meets, [before]).
	compose_(during, met_by, [after]).
	compose_(during, overlaps, [before, meets, overlaps, starts, during]).
	compose_(during, overlapped_by, [after, met_by, overlapped_by, during, finishes]).
	compose_(during, starts, [during]).
	compose_(during, started_by, [after, met_by, overlapped_by, during, finishes]).
	compose_(during, during, [during]).
	compose_(during, contains, [before, after, meets, met_by, overlaps, overlapped_by, starts, started_by, during, contains, finishes, finished_by, equal]).
	compose_(during, finishes, [during]).
	compose_(during, finished_by, [before, meets, overlaps, starts, during]).
	compose_(during, equal, [during]).
	compose_(contains, before, [before, meets, overlaps, contains, finished_by]).
	compose_(contains, after, [after, met_by, overlapped_by, started_by, contains]).
	compose_(contains, meets, [overlaps, contains, finished_by]).
	compose_(contains, met_by, [overlapped_by, started_by, contains]).
	compose_(contains, overlaps, [overlaps, contains, finished_by]).
	compose_(contains, overlapped_by, [overlapped_by, started_by, contains]).
	compose_(contains, starts, [overlaps, contains, finished_by]).
	compose_(contains, started_by, [contains]).
	compose_(contains, during, [overlaps, overlapped_by, starts, started_by, during, contains, finishes, finished_by, equal]).
	compose_(contains, contains, [contains]).
	compose_(contains, finishes, [overlapped_by, started_by, contains]).
	compose_(contains, finished_by, [contains]).
	compose_(contains, equal, [contains]).
	compose_(finishes, before, [before]).
	compose_(finishes, after, [after]).
	compose_(finishes, meets, [meets]).
	compose_(finishes, met_by, [after]).
	compose_(finishes, overlaps, [overlaps, starts, during]).
	compose_(finishes, overlapped_by, [after, met_by, overlapped_by]).
	compose_(finishes, starts, [during]).
	compose_(finishes, started_by, [after, met_by, overlapped_by]).
	compose_(finishes, during, [during]).
	compose_(finishes, contains, [after, met_by, overlapped_by, started_by, contains]).
	compose_(finishes, finishes, [finishes]).
	compose_(finishes, finished_by, [finishes, finished_by, equal]).
	compose_(finishes, equal, [finishes]).
	compose_(finished_by, before, [before]).
	compose_(finished_by, after, [after, met_by, overlapped_by, started_by, contains]).
	compose_(finished_by, meets, [meets]).
	compose_(finished_by, met_by, [overlapped_by, started_by, contains]).
	compose_(finished_by, overlaps, [overlaps]).
	compose_(finished_by, overlapped_by, [overlapped_by, started_by, contains]).
	compose_(finished_by, starts, [overlaps]).
	compose_(finished_by, started_by, [contains]).
	compose_(finished_by, during, [overlaps, starts, during]).
	compose_(finished_by, contains, [contains]).
	compose_(finished_by, finishes, [finishes, finished_by, equal]).
	compose_(finished_by, finished_by, [finished_by]).
	compose_(finished_by, equal, [finished_by]).
	compose_(equal, before, [before]).
	compose_(equal, after, [after]).
	compose_(equal, meets, [meets]).
	compose_(equal, met_by, [met_by]).
	compose_(equal, overlaps, [overlaps]).
	compose_(equal, overlapped_by, [overlapped_by]).
	compose_(equal, starts, [starts]).
	compose_(equal, started_by, [started_by]).
	compose_(equal, during, [during]).
	compose_(equal, contains, [contains]).
	compose_(equal, finishes, [finishes]).
	compose_(equal, finished_by, [finished_by]).
	compose_(equal, equal, [equal]).

:- end_object.
