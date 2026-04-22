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


:- object(breast_cancer,
	implements(dataset_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-14,
		comment is 'Breast Cancer dataset from the UCI Machine Learning Repository. Contains 286 examples with 9 attributes and missing values in the node_caps and breast_quad attributes (represented using anonymous variables).'
	]).

	attribute_values(age, ['10_19', '20_29', '30_39', '40_49', '50_59', '60_69', '70_79', '80_89', '90_99']).
	attribute_values(menopause, [lt40, ge40, premeno]).
	attribute_values(tumor_size, ['0_4', '5_9', '10_14', '15_19', '20_24', '25_29', '30_34', '35_39', '40_44', '45_49', '50_54', '55_59']).
	attribute_values(inv_nodes, ['0_2', '3_5', '6_8', '9_11', '12_14', '15_17', '18_20', '21_23', '24_26', '27_29', '30_32', '33_35', '36_39']).
	attribute_values(node_caps, [yes, no]).
	attribute_values(deg_malig, ['1', '2', '3']).
	attribute_values(breast, [left, right]).
	attribute_values(breast_quad, [left_up, left_low, right_up, right_low, central]).
	attribute_values(irradiat, [yes, no]).

	class(recurrence).

	class_values([no_recurrence_events, recurrence_events]).

	example(Id, Class, [age-Age, menopause-Menopause, tumor_size-TumorSize, inv_nodes-InvNodes, node_caps-NodeCaps, deg_malig-DegMalig, breast-Breast, breast_quad-BreastQuad, irradiat-Irradiat]) :-
		example_(Id, Class, [Age, Menopause, TumorSize, InvNodes, NodeCaps, DegMalig, Breast, BreastQuad, Irradiat]).

	example_(  1, no_recurrence_events, ['30_39', premeno, '30_34', '0_2',   no,  '3', left,  left_low,  no]).
	example_(  2, no_recurrence_events, ['40_49', premeno, '20_24', '0_2',   no,  '2', right, right_up,  no]).
	example_(  3, no_recurrence_events, ['40_49', premeno, '20_24', '0_2',   no,  '2', left,  left_low,  no]).
	example_(  4, no_recurrence_events, ['60_69', ge40,    '15_19', '0_2',   no,  '2', right, left_up,   no]).
	example_(  5, no_recurrence_events, ['40_49', premeno, '0_4',   '0_2',   no,  '2', right, right_low, no]).
	example_(  6, no_recurrence_events, ['60_69', ge40,    '15_19', '0_2',   no,  '2', left,  left_low,  no]).
	example_(  7, no_recurrence_events, ['50_59', premeno, '25_29', '0_2',   no,  '2', left,  left_low,  no]).
	example_(  8, no_recurrence_events, ['60_69', ge40,    '20_24', '0_2',   no,  '1', left,  left_low,  no]).
	example_(  9, no_recurrence_events, ['40_49', premeno, '50_54', '0_2',   no,  '2', left,  left_low,  no]).
	example_( 10, no_recurrence_events, ['40_49', premeno, '20_24', '0_2',   no,  '2', right, left_up,   no]).
	example_( 11, no_recurrence_events, ['40_49', premeno, '0_4',   '0_2',   no,  '3', left,  central,   no]).
	example_( 12, no_recurrence_events, ['50_59', ge40,    '25_29', '0_2',   no,  '2', left,  left_low,  no]).
	example_( 13, no_recurrence_events, ['60_69', lt40,    '10_14', '0_2',   no,  '1', left,  right_up,  no]).
	example_( 14, no_recurrence_events, ['50_59', ge40,    '25_29', '0_2',   no,  '3', left,  right_up,  no]).
	example_( 15, no_recurrence_events, ['40_49', premeno, '30_34', '0_2',   no,  '3', left,  left_up,   no]).
	example_( 16, no_recurrence_events, ['60_69', lt40,    '30_34', '0_2',   no,  '1', left,  left_low,  no]).
	example_( 17, no_recurrence_events, ['40_49', premeno, '15_19', '0_2',   no,  '2', left,  left_low,  no]).
	example_( 18, no_recurrence_events, ['50_59', premeno, '30_34', '0_2',   no,  '3', left,  left_low,  no]).
	example_( 19, no_recurrence_events, ['60_69', ge40,    '30_34', '0_2',   no,  '3', left,  left_low,  no]).
	example_( 20, no_recurrence_events, ['50_59', ge40,    '30_34', '0_2',   no,  '1', right, right_up,  no]).
	example_( 21, no_recurrence_events, ['50_59', ge40,    '40_44', '0_2',   no,  '2', left,  left_low,  no]).
	example_( 22, no_recurrence_events, ['60_69', ge40,    '15_19', '0_2',   no,  '2', left,  left_low,  no]).
	example_( 23, no_recurrence_events, ['30_39', premeno, '25_29', '0_2',   no,  '2', right, left_low,  no]).
	example_( 24, no_recurrence_events, ['50_59', premeno, '40_44', '0_2',   no,  '2', left,  left_up,   no]).
	example_( 25, no_recurrence_events, ['50_59', premeno, '35_39', '0_2',   no,  '2', right, left_up,   no]).
	example_( 26, no_recurrence_events, ['40_49', premeno, '25_29', '0_2',   no,  '2', left,  left_up,   no]).
	example_( 27, no_recurrence_events, ['50_59', premeno, '20_24', '0_2',   no,  '1', left,  left_low,  no]).
	example_( 28, no_recurrence_events, ['60_69', ge40,    '25_29', '0_2',   no,  '3', right, left_up,   no]).
	example_( 29, no_recurrence_events, ['40_49', premeno, '40_44', '0_2',   no,  '2', right, left_low,  no]).
	example_( 30, no_recurrence_events, ['60_69', ge40,    '30_34', '0_2',   no,  '2', left,  left_low,  no]).
	example_( 31, no_recurrence_events, ['50_59', ge40,    '40_44', '0_2',   no,  '3', right, left_up,   no]).
	example_( 32, no_recurrence_events, ['50_59', premeno, '15_19', '0_2',   no,  '2', right, left_low,  no]).
	example_( 33, no_recurrence_events, ['50_59', premeno, '10_14', '0_2',   no,  '3', left,  left_low,  no]).
	example_( 34, no_recurrence_events, ['50_59', ge40,    '10_14', '0_2',   no,  '1', right, left_up,   no]).
	example_( 35, no_recurrence_events, ['50_59', ge40,    '10_14', '0_2',   no,  '1', left,  left_up,   no]).
	example_( 36, no_recurrence_events, ['30_39', premeno, '30_34', '0_2',   no,  '2', left,  left_up,   no]).
	example_( 37, no_recurrence_events, ['50_59', ge40,    '0_4',   '0_2',   no,  '2', left,  central,   no]).
	example_( 38, no_recurrence_events, ['50_59', ge40,    '15_19', '0_2',   no,  '1', right, central,   no]).
	example_( 39, no_recurrence_events, ['40_49', premeno, '10_14', '0_2',   no,  '2', left,  left_low,  no]).
	example_( 40, no_recurrence_events, ['40_49', premeno, '30_34', '0_2',   no,  '1', left,  left_low,  no]).
	example_( 41, no_recurrence_events, ['50_59', ge40,    '20_24', '0_2',   no,  '1', right, left_low,  no]).
	example_( 42, no_recurrence_events, ['60_69', ge40,    '25_29', '0_2',   no,  '2', left,  left_low,  no]).
	example_( 43, no_recurrence_events, ['60_69', ge40,    '5_9',   '0_2',   no,  '1', left,  central,   no]).
	example_( 44, no_recurrence_events, ['40_49', premeno, '10_14', '0_2',   no,  '2', left,  left_up,   no]).
	example_( 45, no_recurrence_events, ['50_59', ge40,    '50_54', '0_2',   no,  '1', right, right_up,  no]).
	example_( 46, no_recurrence_events, ['50_59', ge40,    '30_34', '0_2',   no,  '1', left,  left_up,   no]).
	example_( 47, no_recurrence_events, ['40_49', premeno, '25_29', '0_2',   no,  '2', right, left_low,  no]).
	example_( 48, no_recurrence_events, ['50_59', premeno, '25_29', '0_2',   no,  '1', right, left_up,   no]).
	example_( 49, no_recurrence_events, ['40_49', premeno, '20_24', '0_2',   no,  '1', right, right_up,  no]).
	example_( 50, no_recurrence_events, ['40_49', premeno, '20_24', '0_2',   no,  '1', right, left_low,  no]).
	example_( 51, no_recurrence_events, ['50_59', lt40,    '15_19', '0_2',   no,  '2', left,  left_low,  no]).
	example_( 52, no_recurrence_events, ['30_39', premeno, '20_24', '0_2',   no,  '2', left,  right_low, no]).
	example_( 53, no_recurrence_events, ['50_59', premeno, '15_19', '0_2',   no,  '1', left,  left_low,  no]).
	example_( 54, no_recurrence_events, ['70_79', ge40,    '20_24', '0_2',   no,  '3', left,  left_up,   no]).
	example_( 55, no_recurrence_events, ['70_79', ge40,    '40_44', '0_2',   no,  '1', right, left_up,   no]).
	example_( 56, no_recurrence_events, ['70_79', ge40,    '40_44', '0_2',   no,  '1', right, right_up,  no]).
	example_( 57, no_recurrence_events, ['50_59', ge40,    '0_4',   '0_2',   no,  '1', right, central,   no]).
	example_( 58, no_recurrence_events, ['50_59', ge40,    '5_9',   '0_2',   no,  '2', right, right_up,  no]).
	example_( 59, no_recurrence_events, ['60_69', ge40,    '30_34', '0_2',   no,  '1', left,  left_up,   no]).
	example_( 60, no_recurrence_events, ['60_69', ge40,    '15_19', '0_2',   no,  '1', right, left_up,   no]).
	example_( 61, no_recurrence_events, ['40_49', premeno, '20_24', '0_2',   no,  '2', left,  central,   no]).
	example_( 62, no_recurrence_events, ['40_49', premeno, '10_14', '0_2',   no,  '1', right, right_low, no]).
	example_( 63, no_recurrence_events, ['50_59', ge40,    '0_4',   '0_2',   no,  '1', left,  left_low,  no]).
	example_( 64, no_recurrence_events, ['20_29', premeno, '35_39', '0_2',   no,  '2', right, right_up,  no]).
	example_( 65, no_recurrence_events, ['40_49', premeno, '25_29', '0_2',   no,  '1', left,  right_low, no]).
	example_( 66, no_recurrence_events, ['40_49', premeno, '10_14', '0_2',   no,  '1', right, left_up,   no]).
	example_( 67, no_recurrence_events, ['40_49', premeno, '25_29', '0_2',   no,  '1', right, right_low, no]).
	example_( 68, no_recurrence_events, ['50_59', ge40,    '20_24', '0_2',   no,  '3', left,  left_up,   no]).
	example_( 69, no_recurrence_events, ['50_59', ge40,    '35_39', '0_2',   no,  '3', left,  left_low,  no]).
	example_( 70, no_recurrence_events, ['60_69', ge40,    '50_54', '0_2',   no,  '2', left,  left_low,  no]).
	example_( 71, no_recurrence_events, ['60_69', ge40,    '10_14', '0_2',   no,  '1', left,  left_low,  no]).
	example_( 72, no_recurrence_events, ['40_49', premeno, '25_29', '0_2',   no,  '2', right, left_up,   no]).
	example_( 73, no_recurrence_events, ['60_69', ge40,    '20_24', '0_2',   no,  '2', left,  left_up,   no]).
	example_( 74, no_recurrence_events, ['50_59', premeno, '15_19', '0_2',   no,  '2', right, right_low, no]).
	example_( 75, no_recurrence_events, ['30_39', premeno, '5_9',   '0_2',   no,  '2', left,  right_low, no]).
	example_( 76, no_recurrence_events, ['50_59', ge40,    '10_14', '0_2',   no,  '1', left,  left_low,  no]).
	example_( 77, no_recurrence_events, ['50_59', ge40,    '10_14', '0_2',   no,  '2', left,  left_low,  no]).
	example_( 78, no_recurrence_events, ['30_39', premeno, '25_29', '0_2',   no,  '1', left,  central,   no]).
	example_( 79, no_recurrence_events, ['50_59', premeno, '25_29', '0_2',   no,  '2', left,  left_low,  no]).
	example_( 80, no_recurrence_events, ['40_49', premeno, '25_29', '0_2',   no,  '2', right, central,   no]).
	example_( 81, no_recurrence_events, ['50_59', ge40,    '10_14', '0_2',   no,  '2', right, left_low,  no]).
	example_( 82, no_recurrence_events, ['60_69', ge40,    '10_14', '0_2',   no,  '1', left,  left_up,   no]).
	example_( 83, no_recurrence_events, ['60_69', ge40,    '15_19', '0_2',   no,  '2', right, left_low,  no]).
	example_( 84, no_recurrence_events, ['50_59', ge40,    '15_19', '0_2',   no,  '2', right, left_low,  no]).
	example_( 85, no_recurrence_events, ['40_49', premeno, '20_24', '0_2',   no,  '1', left,  right_low, no]).
	example_( 86, no_recurrence_events, ['50_59', ge40,    '35_39', '0_2',   no,  '3', left,  left_up,   no]).
	example_( 87, no_recurrence_events, ['60_69', ge40,    '25_29', '0_2',   no,  '2', right, left_low,  no]).
	example_( 88, no_recurrence_events, ['70_79', ge40,    '0_4',   '0_2',   no,  '1', left,  right_low, no]).
	example_( 89, no_recurrence_events, ['50_59', ge40,    '20_24', '0_2',   no,  '3', right, left_up,   no]).
	example_( 90, no_recurrence_events, ['40_49', premeno, '40_44', '0_2',   no,  '1', right, left_up,   no]).
	example_( 91, no_recurrence_events, ['30_39', premeno, '0_4',   '0_2',   no,  '2', right, central,   no]).
	example_( 92, no_recurrence_events, ['50_59', ge40,    '20_24', '0_2',   no,  '3', left,  left_up,   no]).
	example_( 93, no_recurrence_events, ['50_59', ge40,    '25_29', '0_2',   no,  '2', right, left_up,   no]).
	example_( 94, no_recurrence_events, ['60_69', ge40,    '20_24', '0_2',   no,  '2', right, left_up,   no]).
	example_( 95, no_recurrence_events, ['50_59', premeno, '10_14', '0_2',   no,  '1', left,  left_low,  no]).
	example_( 96, no_recurrence_events, ['40_49', premeno, '30_34', '0_2',   no,  '2', right, right_low, no]).
	example_( 97, no_recurrence_events, ['60_69', ge40,    '30_34', '0_2',   no,  '2', left,  left_up,   no]).
	example_( 98, no_recurrence_events, ['60_69', ge40,    '15_19', '0_2',   no,  '2', right, left_up,   no]).
	example_( 99, no_recurrence_events, ['40_49', premeno, '30_34', '0_2',   no,  '1', left,  right_up,  no]).
	example_(100, no_recurrence_events, ['30_39', premeno, '25_29', '0_2',   no,  '2', left,  left_low,  no]).
	example_(101, no_recurrence_events, ['40_49', ge40,    '20_24', '0_2',   no,  '3', left,  left_low,  no]).
	example_(102, no_recurrence_events, ['50_59', ge40,    '30_34', '0_2',   no,  '3', right, left_low,  no]).
	example_(103, no_recurrence_events, ['50_59', premeno, '25_29', '0_2',   no,  '2', right, right_low, no]).
	example_(104, no_recurrence_events, ['40_49', premeno, '20_24', '0_2',   no,  '2', left,  right_low, no]).
	example_(105, no_recurrence_events, ['40_49', premeno, '10_14', '0_2',   no,  '2', right, left_low,  no]).
	example_(106, no_recurrence_events, ['40_49', premeno, '30_34', '0_2',   no,  '1', right, left_up,   no]).
	example_(107, no_recurrence_events, ['40_49', premeno, '20_24', '0_2',   no,  '2', left,  left_up,   no]).
	example_(108, no_recurrence_events, ['30_39', premeno, '40_44', '0_2',   no,  '2', right, right_up,  no]).
	example_(109, no_recurrence_events, ['40_49', premeno, '30_34', '0_2',   no,  '3', right, right_up,  no]).
	example_(110, no_recurrence_events, ['60_69', ge40,    '30_34', '0_2',   no,  '1', right, left_up,   no]).
	example_(111, no_recurrence_events, ['50_59', ge40,    '25_29', '0_2',   no,  '1', left,  left_low,  no]).
	example_(112, no_recurrence_events, ['50_59', ge40,    '15_19', '0_2',   no,  '1', right, central,   no]).
	example_(113, no_recurrence_events, ['40_49', premeno, '20_24', '0_2',   no,  '2', right, left_up,   no]).
	example_(114, no_recurrence_events, ['40_49', premeno, '10_14', '0_2',   no,  '1', right, left_up,   no]).
	example_(115, no_recurrence_events, ['40_49', premeno, '35_39', '0_2',   no,  '2', right, right_up,  no]).
	example_(116, no_recurrence_events, ['50_59', ge40,    '20_24', '0_2',   no,  '2', right, left_up,   no]).
	example_(117, no_recurrence_events, ['30_39', premeno, '15_19', '0_2',   no,  '1', left,  left_low,  no]).
	example_(118, no_recurrence_events, ['40_49', ge40,    '20_24', '0_2',   no,  '3', left,  left_up,   no]).
	example_(119, no_recurrence_events, ['30_39', premeno, '10_14', '0_2',   no,  '1', right, left_low,  no]).
	example_(120, no_recurrence_events, ['60_69', ge40,    '15_19', '0_2',   no,  '1', left,  right_low, no]).
	example_(121, no_recurrence_events, ['60_69', ge40,    '20_24', '0_2',   no,  '1', left,  left_low,  no]).
	example_(122, no_recurrence_events, ['50_59', ge40,    '15_19', '0_2',   no,  '2', right, right_up,  no]).
	example_(123, no_recurrence_events, ['50_59', ge40,    '40_44', '0_2',   no,  '3', left,  left_up,   no]).
	example_(124, no_recurrence_events, ['50_59', ge40,    '30_34', '0_2',   no,  '1', right, left_low,  no]).
	example_(125, no_recurrence_events, ['60_69', ge40,    '10_14', '0_2',   no,  '1', right, left_low,  no]).
	example_(126, no_recurrence_events, ['70_79', ge40,    '10_14', '0_2',   no,  '2', left,  central,   no]).
	example_(127, no_recurrence_events, ['30_39', premeno, '30_34', '6_8',   yes, '2', right, right_up,  no]).
	example_(128, no_recurrence_events, ['30_39', premeno, '25_29', '6_8',   yes, '2', right, left_up,   yes]).
	example_(129, no_recurrence_events, ['50_59', premeno, '25_29', '0_2',   yes, '2', left,  left_up,   no]).
	example_(130, no_recurrence_events, ['40_49', premeno, '35_39', '9_11',  yes, '2', right, left_up,   yes]).
	example_(131, no_recurrence_events, ['40_49', premeno, '35_39', '9_11',  yes, '2', right, right_up,  yes]).
	example_(132, no_recurrence_events, ['40_49', premeno, '40_44', '3_5',   yes, '3', right, left_up,   yes]).
	example_(133, no_recurrence_events, ['40_49', premeno, '30_34', '6_8',   no,  '2', left,  left_up,   no]).
	example_(134, no_recurrence_events, ['50_59', ge40,    '40_44', '0_2',   no,  '3', left,  right_up,  no]).
	example_(135, no_recurrence_events, ['60_69', ge40,    '30_34', '0_2',   no,  '2', left,  left_low,  yes]).
	example_(136, no_recurrence_events, ['30_39', premeno, '20_24', '3_5',   no,  '2', right, central,   no]).
	example_(137, no_recurrence_events, ['30_39', premeno, '40_44', '3_5',   no,  '3', right, right_up,  yes]).
	example_(138, no_recurrence_events, ['40_49', premeno, '5_9',   '0_2',   no,  '1', left,  left_low,  yes]).
	example_(139, no_recurrence_events, ['30_39', premeno, '40_44', '0_2',   no,  '2', left,  left_low,  yes]).
	example_(140, no_recurrence_events, ['40_49', premeno, '30_34', '0_2',   no,  '2', left,  right_low, no]).
	example_(141, no_recurrence_events, ['50_59', ge40,    '40_44', '3_5',   yes, '2', left,  left_low,  no]).
	example_(142, no_recurrence_events, ['50_59', premeno, '20_24', '3_5',   yes, '2', left,  left_low,  no]).
	example_(143, no_recurrence_events, ['60_69', ge40,    '10_14', '0_2',   no,  '1', left,  left_up,   no]).
	example_(144, no_recurrence_events, ['40_49', premeno, '45_49', '0_2',   no,  '2', left,  left_low,  yes]).
	example_(145, no_recurrence_events, ['60_69', ge40,    '45_49', '6_8',   yes, '3', left,  central,   no]).
	example_(146, no_recurrence_events, ['40_49', premeno, '25_29', '0_2',   _,   '2', left,  right_low, yes]).
	example_(147, no_recurrence_events, ['60_69', ge40,    '50_54', '0_2',   no,  '2', right, left_up,   yes]).
	example_(148, no_recurrence_events, ['50_59', premeno, '30_34', '3_5',   yes, '2', left,  left_low,  yes]).
	example_(149, no_recurrence_events, ['30_39', premeno, '20_24', '0_2',   no,  '3', left,  central,   no]).
	example_(150, no_recurrence_events, ['50_59', lt40,    '30_34', '0_2',   no,  '3', right, left_up,   no]).
	example_(151, no_recurrence_events, ['50_59', ge40,    '25_29', '15_17', yes, '3', right, left_up,   no]).
	example_(152, no_recurrence_events, ['60_69', ge40,    '30_34', '3_5',   yes, '3', left,  left_low,  no]).
	example_(153, no_recurrence_events, ['50_59', ge40,    '35_39', '15_17', no,  '3', left,  left_low,  no]).
	example_(154, no_recurrence_events, ['60_69', ge40,    '15_19', '0_2',   no,  '3', right, left_up,   yes]).
	example_(155, no_recurrence_events, ['30_39', lt40,    '15_19', '0_2',   no,  '3', right, left_up,   no]).
	example_(156, no_recurrence_events, ['60_69', ge40,    '40_44', '3_5',   no,  '2', right, left_up,   yes]).
	example_(157, no_recurrence_events, ['50_59', ge40,    '25_29', '3_5',   yes, '3', right, left_up,   no]).
	example_(158, no_recurrence_events, ['50_59', premeno, '30_34', '0_2',   no,  '1', left,  central,   no]).
	example_(159, no_recurrence_events, ['50_59', ge40,    '30_34', '0_2',   no,  '1', right, central,   no]).
	example_(160, no_recurrence_events, ['40_49', premeno, '35_39', '0_2',   no,  '1', left,  left_low,  no]).
	example_(161, no_recurrence_events, ['40_49', premeno, '25_29', '0_2',   no,  '3', right, left_up,   yes]).
	example_(162, no_recurrence_events, ['40_49', premeno, '30_34', '3_5',   yes, '2', right, left_low,  no]).
	example_(163, no_recurrence_events, ['60_69', ge40,    '10_14', '0_2',   no,  '2', right, left_up,   yes]).
	example_(164, no_recurrence_events, ['60_69', ge40,    '25_29', '3_5',   _,   '1', right, left_up,   yes]).
	example_(165, no_recurrence_events, ['60_69', ge40,    '25_29', '3_5',   _,   '1', right, left_low,  yes]).
	example_(166, no_recurrence_events, ['40_49', premeno, '20_24', '3_5',   no,  '2', right, left_up,   no]).
	example_(167, no_recurrence_events, ['40_49', premeno, '20_24', '3_5',   no,  '2', right, left_low,  no]).
	example_(168, no_recurrence_events, ['40_49', ge40,    '40_44', '15_17', yes, '2', right, left_up,   yes]).
	example_(169, no_recurrence_events, ['50_59', premeno, '10_14', '0_2',   no,  '2', right, left_up,   no]).
	example_(170, no_recurrence_events, ['40_49', ge40,    '30_34', '0_2',   no,  '2', left,  left_up,   yes]).
	example_(171, no_recurrence_events, ['30_39', premeno, '20_24', '3_5',   yes, '2', right, left_up,   yes]).
	example_(172, no_recurrence_events, ['30_39', premeno, '15_19', '0_2',   no,  '1', left,  left_low,  no]).
	example_(173, no_recurrence_events, ['60_69', ge40,    '30_34', '6_8',   yes, '2', right, right_up,  no]).
	example_(174, no_recurrence_events, ['50_59', ge40,    '20_24', '3_5',   yes, '2', right, left_up,   no]).
	example_(175, no_recurrence_events, ['50_59', premeno, '25_29', '3_5',   yes, '2', left,  left_low,  yes]).
	example_(176, no_recurrence_events, ['40_49', premeno, '30_34', '0_2',   no,  '2', right, right_up,  yes]).
	example_(177, no_recurrence_events, ['40_49', ge40,    '25_29', '0_2',   no,  '2', left,  left_low,  no]).
	example_(178, no_recurrence_events, ['60_69', ge40,    '10_14', '0_2',   no,  '2', left,  left_low,  no]).
	example_(179, no_recurrence_events, ['50_59', premeno, '25_29', '3_5',   no,  '2', right, left_up,   yes]).
	example_(180, no_recurrence_events, ['40_49', premeno, '20_24', '0_2',   no,  '3', right, left_low,  yes]).
	example_(181, no_recurrence_events, ['40_49', premeno, '35_39', '0_2',   yes, '3', right, left_up,   yes]).
	example_(182, no_recurrence_events, ['40_49', premeno, '35_39', '0_2',   yes, '3', right, left_low,  yes]).
	example_(183, no_recurrence_events, ['40_49', premeno, '25_29', '0_2',   no,  '1', right, left_low,  yes]).
	example_(184, no_recurrence_events, ['50_59', ge40,    '30_34', '9_11',  _,   '3', left,  left_up,   yes]).
	example_(185, no_recurrence_events, ['50_59', ge40,    '30_34', '9_11',  _,   '3', left,  left_low,  yes]).
	example_(186, no_recurrence_events, ['40_49', premeno, '20_24', '6_8',   no,  '2', right, left_low,  yes]).
	example_(187, no_recurrence_events, ['50_59', ge40,    '25_29', '0_2',   no,  '1', left,  right_low, no]).
	example_(188, no_recurrence_events, ['60_69', ge40,    '15_19', '0_2',   no,  '2', left,  left_up,   yes]).
	example_(189, no_recurrence_events, ['40_49', premeno, '10_14', '0_2',   no,  '2', right, left_up,   no]).
	example_(190, no_recurrence_events, ['50_59', ge40,    '20_24', '0_2',   yes, '2', right, left_up,   no]).
	example_(191, no_recurrence_events, ['40_49', premeno, '15_19', '12_14', no,  '3', right, right_low, yes]).
	example_(192, no_recurrence_events, ['40_49', premeno, '25_29', '0_2',   no,  '2', left,  left_up,   yes]).
	example_(193, no_recurrence_events, ['50_59', ge40,    '30_34', '6_8',   yes, '2', left,  left_low,  no]).
	example_(194, no_recurrence_events, ['30_39', premeno, '10_14', '0_2',   no,  '2', left,  right_low, no]).
	example_(195, no_recurrence_events, ['50_59', premeno, '50_54', '0_2',   yes, '2', right, left_up,   yes]).
	example_(196, no_recurrence_events, ['50_59', ge40,    '35_39', '0_2',   no,  '2', left,  left_up,   no]).
	example_(197, no_recurrence_events, ['50_59', premeno, '10_14', '3_5',   no,  '1', right, left_up,   no]).
	example_(198, no_recurrence_events, ['40_49', premeno, '10_14', '0_2',   no,  '2', left,  left_low,  yes]).
	example_(199, no_recurrence_events, ['50_59', ge40,    '15_19', '0_2',   yes, '2', left,  central,   yes]).
	example_(200, no_recurrence_events, ['50_59', premeno, '25_29', '0_2',   no,  '1', left,  left_low,  no]).
	example_(201, no_recurrence_events, ['60_69', ge40,    '25_29', '0_2',   no,  '3', right, left_low,  no]).
	example_(202, recurrence_events,    ['50_59', premeno, '15_19', '0_2',   no,  '2', left,  left_low,  no]).
	example_(203, recurrence_events,    ['40_49', premeno, '40_44', '0_2',   no,  '1', left,  left_low,  no]).
	example_(204, recurrence_events,    ['50_59', ge40,    '35_39', '0_2',   no,  '2', left,  left_low,  no]).
	example_(205, recurrence_events,    ['50_59', premeno, '25_29', '0_2',   no,  '2', left,  right_up,  no]).
	example_(206, recurrence_events,    ['30_39', premeno, '0_4',   '0_2',   no,  '2', right, central,   no]).
	example_(207, recurrence_events,    ['50_59', ge40,    '30_34', '0_2',   no,  '3', left,  _,         no]).
	example_(208, recurrence_events,    ['50_59', premeno, '25_29', '0_2',   no,  '2', left,  right_up,  no]).
	example_(209, recurrence_events,    ['50_59', premeno, '30_34', '0_2',   no,  '3', left,  right_up,  no]).
	example_(210, recurrence_events,    ['40_49', premeno, '35_39', '0_2',   no,  '1', right, left_up,   no]).
	example_(211, recurrence_events,    ['40_49', premeno, '20_24', '0_2',   no,  '2', left,  left_low,  no]).
	example_(212, recurrence_events,    ['50_59', ge40,    '20_24', '0_2',   no,  '2', right, central,   no]).
	example_(213, recurrence_events,    ['40_49', premeno, '30_34', '0_2',   no,  '3', right, right_up,  no]).
	example_(214, recurrence_events,    ['50_59', premeno, '25_29', '0_2',   no,  '1', right, left_up,   no]).
	example_(215, recurrence_events,    ['60_69', ge40,    '40_44', '0_2',   no,  '2', right, left_low,  no]).
	example_(216, recurrence_events,    ['40_49', ge40,    '20_24', '0_2',   no,  '2', right, left_up,   no]).
	example_(217, recurrence_events,    ['50_59', ge40,    '20_24', '0_2',   no,  '2', left,  left_up,   no]).
	example_(218, recurrence_events,    ['40_49', premeno, '15_19', '0_2',   no,  '2', left,  left_up,   no]).
	example_(219, recurrence_events,    ['60_69', ge40,    '30_34', '0_2',   no,  '3', right, central,   no]).
	example_(220, recurrence_events,    ['30_39', premeno, '15_19', '0_2',   no,  '1', right, left_low,  no]).
	example_(221, recurrence_events,    ['40_49', premeno, '25_29', '0_2',   no,  '3', left,  right_up,  no]).
	example_(222, recurrence_events,    ['30_39', premeno, '30_34', '0_2',   no,  '1', right, left_up,   no]).
	example_(223, recurrence_events,    ['60_69', ge40,    '25_29', '0_2',   no,  '3', left,  right_low, yes]).
	example_(224, recurrence_events,    ['60_69', ge40,    '20_24', '0_2',   no,  '3', right, left_low,  no]).
	example_(225, recurrence_events,    ['30_39', premeno, '25_29', '3_5',   yes, '3', left,  left_low,  yes]).
	example_(226, recurrence_events,    ['40_49', ge40,    '20_24', '3_5',   no,  '3', right, left_low,  yes]).
	example_(227, recurrence_events,    ['40_49', premeno, '30_34', '15_17', yes, '3', left,  left_low,  no]).
	example_(228, recurrence_events,    ['50_59', premeno, '30_34', '0_2',   no,  '3', right, left_up,   yes]).
	example_(229, recurrence_events,    ['60_69', ge40,    '40_44', '3_5',   yes, '3', right, left_low,  no]).
	example_(230, recurrence_events,    ['60_69', ge40,    '45_49', '0_2',   no,  '1', right, right_up,  yes]).
	example_(231, recurrence_events,    ['50_59', premeno, '50_54', '9_11',  yes, '2', right, left_up,   no]).
	example_(232, recurrence_events,    ['40_49', premeno, '30_34', '3_5',   no,  '2', right, left_up,   no]).
	example_(233, recurrence_events,    ['30_39', premeno, '30_34', '3_5',   no,  '3', right, left_up,   yes]).
	example_(234, recurrence_events,    ['70_79', ge40,    '15_19', '9_11',  _,   '1', left,  left_low,  yes]).
	example_(235, recurrence_events,    ['60_69', ge40,    '30_34', '0_2',   no,  '3', right, left_up,   yes]).
	example_(236, recurrence_events,    ['50_59', premeno, '25_29', '3_5',   yes, '3', left,  left_low,  yes]).
	example_(237, recurrence_events,    ['40_49', premeno, '25_29', '0_2',   no,  '2', right, left_low,  no]).
	example_(238, recurrence_events,    ['40_49', premeno, '25_29', '0_2',   no,  '2', right, left_low,  no]).
	example_(239, recurrence_events,    ['30_39', premeno, '35_39', '0_2',   no,  '3', left,  left_low,  no]).
	example_(240, recurrence_events,    ['40_49', premeno, '20_24', '3_5',   yes, '2', right, right_up,  yes]).
	example_(241, recurrence_events,    ['60_69', ge40,    '20_24', '3_5',   no,  '2', left,  left_low,  yes]).
	example_(242, recurrence_events,    ['40_49', premeno, '15_19', '15_17', yes, '3', left,  left_low,  no]).
	example_(243, recurrence_events,    ['50_59', ge40,    '25_29', '6_8',   no,  '3', left,  left_low,  yes]).
	example_(244, recurrence_events,    ['50_59', ge40,    '20_24', '3_5',   yes, '3', right, right_up,  no]).
	example_(245, recurrence_events,    ['40_49', premeno, '30_34', '12_14', yes, '3', left,  left_up,   yes]).
	example_(246, recurrence_events,    ['30_39', premeno, '30_34', '9_11',  no,  '2', right, left_up,   yes]).
	example_(247, recurrence_events,    ['30_39', premeno, '15_19', '6_8',   yes, '3', left,  left_low,  yes]).
	example_(248, recurrence_events,    ['50_59', ge40,    '30_34', '9_11',  yes, '3', left,  right_low, yes]).
	example_(249, recurrence_events,    ['60_69', ge40,    '35_39', '6_8',   yes, '3', left,  left_low,  no]).
	example_(250, recurrence_events,    ['30_39', premeno, '20_24', '3_5',   yes, '2', left,  left_low,  no]).
	example_(251, recurrence_events,    ['40_49', premeno, '25_29', '0_2',   no,  '3', left,  left_up,   no]).
	example_(252, recurrence_events,    ['40_49', premeno, '50_54', '0_2',   no,  '2', right, left_low,  yes]).
	example_(253, recurrence_events,    ['30_39', premeno, '40_44', '0_2',   no,  '1', left,  left_up,   no]).
	example_(254, recurrence_events,    ['60_69', ge40,    '50_54', '0_2',   no,  '3', right, left_up,   no]).
	example_(255, recurrence_events,    ['40_49', premeno, '30_34', '0_2',   yes, '3', right, right_up,  no]).
	example_(256, recurrence_events,    ['40_49', premeno, '30_34', '6_8',   yes, '3', right, left_up,   no]).
	example_(257, recurrence_events,    ['40_49', premeno, '30_34', '0_2',   no,  '1', left,  left_low,  yes]).
	example_(258, recurrence_events,    ['40_49', premeno, '20_24', '3_5',   yes, '2', left,  left_low,  yes]).
	example_(259, recurrence_events,    ['50_59', ge40,    '30_34', '6_8',   yes, '2', left,  right_low, yes]).
	example_(260, recurrence_events,    ['50_59', ge40,    '30_34', '3_5',   no,  '3', right, left_up,   no]).
	example_(261, recurrence_events,    ['60_69', ge40,    '25_29', '3_5',   no,  '2', right, right_up,  no]).
	example_(262, recurrence_events,    ['40_49', ge40,    '25_29', '12_14', yes, '3', left,  right_low, yes]).
	example_(263, recurrence_events,    ['60_69', ge40,    '25_29', '0_2',   no,  '3', left,  left_up,   no]).
	example_(264, recurrence_events,    ['50_59', lt40,    '20_24', '0_2',   _,   '1', left,  left_up,   no]).
	example_(265, recurrence_events,    ['50_59', lt40,    '20_24', '0_2',   _,   '1', left,  left_low,  no]).
	example_(266, recurrence_events,    ['30_39', premeno, '35_39', '9_11',  yes, '3', left,  left_low,  no]).
	example_(267, recurrence_events,    ['40_49', premeno, '30_34', '3_5',   yes, '2', left,  right_up,  no]).
	example_(268, recurrence_events,    ['60_69', ge40,    '20_24', '24_26', yes, '3', left,  left_low,  yes]).
	example_(269, recurrence_events,    ['30_39', premeno, '35_39', '0_2',   no,  '3', left,  left_low,  no]).
	example_(270, recurrence_events,    ['40_49', premeno, '25_29', '0_2',   no,  '2', left,  left_low,  yes]).
	example_(271, recurrence_events,    ['50_59', ge40,    '30_34', '6_8',   yes, '3', left,  right_low, no]).
	example_(272, recurrence_events,    ['50_59', premeno, '25_29', '0_2',   no,  '3', right, left_low,  yes]).
	example_(273, recurrence_events,    ['40_49', premeno, '15_19', '0_2',   yes, '3', right, left_up,   no]).
	example_(274, recurrence_events,    ['60_69', ge40,    '30_34', '0_2',   yes, '2', right, right_up,  yes]).
	example_(275, recurrence_events,    ['60_69', ge40,    '30_34', '3_5',   yes, '2', left,  central,   yes]).
	example_(276, recurrence_events,    ['40_49', premeno, '25_29', '9_11',  yes, '3', right, left_up,   no]).
	example_(277, recurrence_events,    ['30_39', premeno, '25_29', '6_8',   yes, '3', left,  right_low, yes]).
	example_(278, recurrence_events,    ['60_69', ge40,    '10_14', '6_8',   yes, '3', left,  left_up,   yes]).
	example_(279, recurrence_events,    ['50_59', premeno, '35_39', '15_17', yes, '3', right, right_up,  no]).
	example_(280, recurrence_events,    ['50_59', ge40,    '40_44', '6_8',   yes, '3', left,  left_low,  yes]).
	example_(281, recurrence_events,    ['50_59', ge40,    '40_44', '6_8',   yes, '3', left,  left_low,  yes]).
	example_(282, recurrence_events,    ['30_39', premeno, '30_34', '0_2',   no,  '2', left,  left_up,   no]).
	example_(283, recurrence_events,    ['30_39', premeno, '20_24', '0_2',   no,  '3', left,  left_up,   yes]).
	example_(284, recurrence_events,    ['60_69', ge40,    '20_24', '0_2',   no,  '1', right, left_up,   no]).
	example_(285, recurrence_events,    ['40_49', ge40,    '30_34', '3_5',   no,  '3', left,  left_low,  no]).
	example_(286, recurrence_events,    ['50_59', ge40,    '30_34', '3_5',   no,  '3', left,  left_low,  no]).

:- end_object.
