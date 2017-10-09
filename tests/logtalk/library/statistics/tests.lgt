%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2017 Paulo Moura <pmoura@logtalk.org>
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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 0.1,
		author is 'Paulo Moura',
		date is 2017/10/09,
		comment is 'Unit tests for the "statistics" library.'
	]).

	:- uses(lgtunit, [
		op(700, xfx, '=~='), '=~='/2
	]).

	test(sample_standard_deviation_2_01) :-
		sample::standard_deviation([35,36,46,68,70], StandardDeviation),
		StandardDeviation =~= 17.0.

	test(sample_z_arithmetic_mean_2_01) :-
		sample::arithmetic_mean([35,36,46,68,70], Mean),
		Mean =~= 51.0.

	test(sample_z_normalization_2_01) :-
		sample::z_normalization([35,36,46,68,70], ZScores),
		ZScores =~= [-0.9411764705882353,-0.8823529411764706,-0.29411764705882354,1.0,1.1176470588235294].

:- end_object.
