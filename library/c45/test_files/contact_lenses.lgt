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
		date is 2026-02-14,
		comment is 'Classic "Contact Lenses" dataset used for testing decision tree learning algorithms. Based on the dataset from Cendrowska (1987).'
	]).

	attribute_values(age, [young, pre_presbyopic, presbyopic]).
	attribute_values(spectacle_prescription, [myope, hypermetrope]).
	attribute_values(astigmatism, [no, yes]).
	attribute_values(tear_production_rate, [reduced, normal]).

	class(lenses).

	class_values([hard, soft, none]).

	example( 1, hard, [age-young,           spectacle_prescription-myope,        astigmatism-no,  tear_production_rate-normal]).
	example( 2, none, [age-young,           spectacle_prescription-myope,        astigmatism-no,  tear_production_rate-reduced]).
	example( 3, hard, [age-young,           spectacle_prescription-myope,        astigmatism-yes, tear_production_rate-normal]).
	example( 4, none, [age-young,           spectacle_prescription-myope,        astigmatism-yes, tear_production_rate-reduced]).
	example( 5, soft, [age-young,           spectacle_prescription-hypermetrope, astigmatism-no,  tear_production_rate-normal]).
	example( 6, none, [age-young,           spectacle_prescription-hypermetrope, astigmatism-no,  tear_production_rate-reduced]).
	example( 7, hard, [age-young,           spectacle_prescription-hypermetrope, astigmatism-yes, tear_production_rate-normal]).
	example( 8, none, [age-young,           spectacle_prescription-hypermetrope, astigmatism-yes, tear_production_rate-reduced]).
	example( 9, soft, [age-pre_presbyopic,  spectacle_prescription-myope,        astigmatism-no,  tear_production_rate-normal]).
	example(10, none, [age-pre_presbyopic,  spectacle_prescription-myope,        astigmatism-no,  tear_production_rate-reduced]).
	example(11, hard, [age-pre_presbyopic,  spectacle_prescription-myope,        astigmatism-yes, tear_production_rate-normal]).
	example(12, none, [age-pre_presbyopic,  spectacle_prescription-myope,        astigmatism-yes, tear_production_rate-reduced]).
	example(13, soft, [age-pre_presbyopic,  spectacle_prescription-hypermetrope, astigmatism-no,  tear_production_rate-normal]).
	example(14, none, [age-pre_presbyopic,  spectacle_prescription-hypermetrope, astigmatism-no,  tear_production_rate-reduced]).
	example(15, none, [age-pre_presbyopic,  spectacle_prescription-hypermetrope, astigmatism-yes, tear_production_rate-normal]).
	example(16, none, [age-pre_presbyopic,  spectacle_prescription-hypermetrope, astigmatism-yes, tear_production_rate-reduced]).
	example(17, none, [age-presbyopic,      spectacle_prescription-myope,        astigmatism-no,  tear_production_rate-normal]).
	example(18, none, [age-presbyopic,      spectacle_prescription-myope,        astigmatism-no,  tear_production_rate-reduced]).
	example(19, hard, [age-presbyopic,      spectacle_prescription-myope,        astigmatism-yes, tear_production_rate-normal]).
	example(20, none, [age-presbyopic,      spectacle_prescription-myope,        astigmatism-yes, tear_production_rate-reduced]).
	example(21, soft, [age-presbyopic,      spectacle_prescription-hypermetrope, astigmatism-no,  tear_production_rate-normal]).
	example(22, none, [age-presbyopic,      spectacle_prescription-hypermetrope, astigmatism-no,  tear_production_rate-reduced]).
	example(23, none, [age-presbyopic,      spectacle_prescription-hypermetrope, astigmatism-yes, tear_production_rate-normal]).
	example(24, none, [age-presbyopic,      spectacle_prescription-hypermetrope, astigmatism-yes, tear_production_rate-reduced]).

:- end_object.

