%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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


% to avoid problems with backend Prolog compilers such as ECLiPSe where
% reloading a file defining clauses for a multifile predicate results in
% the duplication of the clauses, below we load the required libraries
% for the "lgtunit" tool separately so that we can load the "arbitrary"
% library under testing in debug mode

:- initialization((
	set_logtalk_flag(report, warnings),
	logtalk_load(types(loader)),
	logtalk_load(random(loader)),
	logtalk_load(os(loader)),
	% load the "arbitrary" category in debug mode to support collecting code coverage data
	logtalk_load(arbitrary, [debug(on), source_data(on), clean(on)]),
	logtalk_load([lgtunit(lgtunit), lgtunit(lgtunit_messages)], [optimize(on)]),
	logtalk_load(tests, [hook(lgtunit)]),
	tests::run
)).
