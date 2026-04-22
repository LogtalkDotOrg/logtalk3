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


:- object(contact_lenses,
	implements(dataset_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-18,
		comment is 'Classic "Contact Lenses" dataset used for testing decision tree learning algorithms. Based on the dataset from Cendrowska (1987).'
	]).

	attribute_values(age, [young, pre_presbyopic, presbyopic]).
	attribute_values(spectacle_prescription, [myope, hypermetrope]).
	attribute_values(astigmatism, [no, yes]).
	attribute_values(tear_production_rate, [reduced, normal]).

	class(lenses).

	class_values([hard, soft, none]).

	example(Id, Class, [age-Age, spectacle_prescription-SpectaclePrescription, astigmatism-Astigmatism,  tear_production_rate-TearProductionRate]) :-
		example_(Id, Class, [Age, SpectaclePrescription, Astigmatism, TearProductionRate]).

	example_( 1, hard, [young,          myope,        no,  normal]).
	example_( 2, none, [young,          myope,        no,  reduced]).
	example_( 3, hard, [young,          myope,        yes, normal]).
	example_( 4, none, [young,          myope,        yes, reduced]).
	example_( 5, soft, [young,          hypermetrope, no,  normal]).
	example_( 6, none, [young,          hypermetrope, no,  reduced]).
	example_( 7, hard, [young,          hypermetrope, yes, normal]).
	example_( 8, none, [young,          hypermetrope, yes, reduced]).
	example_( 9, soft, [pre_presbyopic, myope,        no,  normal]).
	example_(10, none, [pre_presbyopic, myope,        no,  reduced]).
	example_(11, hard, [pre_presbyopic, myope,        yes, normal]).
	example_(12, none, [pre_presbyopic, myope,        yes, reduced]).
	example_(13, soft, [pre_presbyopic, hypermetrope, no,  normal]).
	example_(14, none, [pre_presbyopic, hypermetrope, no,  reduced]).
	example_(15, none, [pre_presbyopic, hypermetrope, yes, normal]).
	example_(16, none, [pre_presbyopic, hypermetrope, yes, reduced]).
	example_(17, none, [presbyopic,     myope,        no,  normal]).
	example_(18, none, [presbyopic,     myope,        no,  reduced]).
	example_(19, hard, [presbyopic,     myope,        yes, normal]).
	example_(20, none, [presbyopic,     myope,        yes, reduced]).
	example_(21, soft, [presbyopic,     hypermetrope, no,  normal]).
	example_(22, none, [presbyopic,     hypermetrope, no,  reduced]).
	example_(23, none, [presbyopic,     hypermetrope, yes, normal]).
	example_(24, none, [presbyopic,     hypermetrope, yes, reduced]).

:- end_object.
