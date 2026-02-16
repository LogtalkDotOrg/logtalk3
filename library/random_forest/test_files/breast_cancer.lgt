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

	example(  1, no_recurrence_events, [age-'30_39', menopause-premeno, tumor_size-'30_34', inv_nodes-'0_2', node_caps-no, deg_malig-'3', breast-left, breast_quad-left_low, irradiat-no]).
	example(  2, no_recurrence_events, [age-'40_49', menopause-premeno, tumor_size-'20_24', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-right, breast_quad-right_up, irradiat-no]).
	example(  3, no_recurrence_events, [age-'40_49', menopause-premeno, tumor_size-'20_24', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-left, breast_quad-left_low, irradiat-no]).
	example(  4, no_recurrence_events, [age-'60_69', menopause-ge40, tumor_size-'15_19', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-right, breast_quad-left_up, irradiat-no]).
	example(  5, no_recurrence_events, [age-'40_49', menopause-premeno, tumor_size-'0_4', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-right, breast_quad-right_low, irradiat-no]).
	example(  6, no_recurrence_events, [age-'60_69', menopause-ge40, tumor_size-'15_19', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-left, breast_quad-left_low, irradiat-no]).
	example(  7, no_recurrence_events, [age-'50_59', menopause-premeno, tumor_size-'25_29', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-left, breast_quad-left_low, irradiat-no]).
	example(  8, no_recurrence_events, [age-'60_69', menopause-ge40, tumor_size-'20_24', inv_nodes-'0_2', node_caps-no, deg_malig-'1', breast-left, breast_quad-left_low, irradiat-no]).
	example(  9, no_recurrence_events, [age-'40_49', menopause-premeno, tumor_size-'50_54', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-left, breast_quad-left_low, irradiat-no]).
	example( 10, no_recurrence_events, [age-'40_49', menopause-premeno, tumor_size-'20_24', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-right, breast_quad-left_up, irradiat-no]).
	example( 11, no_recurrence_events, [age-'40_49', menopause-premeno, tumor_size-'0_4', inv_nodes-'0_2', node_caps-no, deg_malig-'3', breast-left, breast_quad-central, irradiat-no]).
	example( 12, no_recurrence_events, [age-'50_59', menopause-ge40, tumor_size-'25_29', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-left, breast_quad-left_low, irradiat-no]).
	example( 13, no_recurrence_events, [age-'60_69', menopause-lt40, tumor_size-'10_14', inv_nodes-'0_2', node_caps-no, deg_malig-'1', breast-left, breast_quad-right_up, irradiat-no]).
	example( 14, no_recurrence_events, [age-'50_59', menopause-ge40, tumor_size-'25_29', inv_nodes-'0_2', node_caps-no, deg_malig-'3', breast-left, breast_quad-right_up, irradiat-no]).
	example( 15, no_recurrence_events, [age-'40_49', menopause-premeno, tumor_size-'30_34', inv_nodes-'0_2', node_caps-no, deg_malig-'3', breast-left, breast_quad-left_up, irradiat-no]).
	example( 16, no_recurrence_events, [age-'60_69', menopause-lt40, tumor_size-'30_34', inv_nodes-'0_2', node_caps-no, deg_malig-'1', breast-left, breast_quad-left_low, irradiat-no]).
	example( 17, no_recurrence_events, [age-'40_49', menopause-premeno, tumor_size-'15_19', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-left, breast_quad-left_low, irradiat-no]).
	example( 18, no_recurrence_events, [age-'50_59', menopause-premeno, tumor_size-'30_34', inv_nodes-'0_2', node_caps-no, deg_malig-'3', breast-left, breast_quad-left_low, irradiat-no]).
	example( 19, no_recurrence_events, [age-'60_69', menopause-ge40, tumor_size-'30_34', inv_nodes-'0_2', node_caps-no, deg_malig-'3', breast-left, breast_quad-left_low, irradiat-no]).
	example( 20, no_recurrence_events, [age-'50_59', menopause-ge40, tumor_size-'30_34', inv_nodes-'0_2', node_caps-no, deg_malig-'1', breast-right, breast_quad-right_up, irradiat-no]).
	example( 21, no_recurrence_events, [age-'50_59', menopause-ge40, tumor_size-'40_44', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-left, breast_quad-left_low, irradiat-no]).
	example( 22, no_recurrence_events, [age-'60_69', menopause-ge40, tumor_size-'15_19', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-left, breast_quad-left_low, irradiat-no]).
	example( 23, no_recurrence_events, [age-'30_39', menopause-premeno, tumor_size-'25_29', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-right, breast_quad-left_low, irradiat-no]).
	example( 24, no_recurrence_events, [age-'50_59', menopause-premeno, tumor_size-'40_44', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-left, breast_quad-left_up, irradiat-no]).
	example( 25, no_recurrence_events, [age-'50_59', menopause-premeno, tumor_size-'35_39', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-right, breast_quad-left_up, irradiat-no]).
	example( 26, no_recurrence_events, [age-'40_49', menopause-premeno, tumor_size-'25_29', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-left, breast_quad-left_up, irradiat-no]).
	example( 27, no_recurrence_events, [age-'50_59', menopause-premeno, tumor_size-'20_24', inv_nodes-'0_2', node_caps-no, deg_malig-'1', breast-left, breast_quad-left_low, irradiat-no]).
	example( 28, no_recurrence_events, [age-'60_69', menopause-ge40, tumor_size-'25_29', inv_nodes-'0_2', node_caps-no, deg_malig-'3', breast-right, breast_quad-left_up, irradiat-no]).
	example( 29, no_recurrence_events, [age-'40_49', menopause-premeno, tumor_size-'40_44', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-right, breast_quad-left_low, irradiat-no]).
	example( 30, no_recurrence_events, [age-'60_69', menopause-ge40, tumor_size-'30_34', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-left, breast_quad-left_low, irradiat-no]).
	example( 31, no_recurrence_events, [age-'50_59', menopause-ge40, tumor_size-'40_44', inv_nodes-'0_2', node_caps-no, deg_malig-'3', breast-right, breast_quad-left_up, irradiat-no]).
	example( 32, no_recurrence_events, [age-'50_59', menopause-premeno, tumor_size-'15_19', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-right, breast_quad-left_low, irradiat-no]).
	example( 33, no_recurrence_events, [age-'50_59', menopause-premeno, tumor_size-'10_14', inv_nodes-'0_2', node_caps-no, deg_malig-'3', breast-left, breast_quad-left_low, irradiat-no]).
	example( 34, no_recurrence_events, [age-'50_59', menopause-ge40, tumor_size-'10_14', inv_nodes-'0_2', node_caps-no, deg_malig-'1', breast-right, breast_quad-left_up, irradiat-no]).
	example( 35, no_recurrence_events, [age-'50_59', menopause-ge40, tumor_size-'10_14', inv_nodes-'0_2', node_caps-no, deg_malig-'1', breast-left, breast_quad-left_up, irradiat-no]).
	example( 36, no_recurrence_events, [age-'30_39', menopause-premeno, tumor_size-'30_34', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-left, breast_quad-left_up, irradiat-no]).
	example( 37, no_recurrence_events, [age-'50_59', menopause-ge40, tumor_size-'0_4', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-left, breast_quad-central, irradiat-no]).
	example( 38, no_recurrence_events, [age-'50_59', menopause-ge40, tumor_size-'15_19', inv_nodes-'0_2', node_caps-no, deg_malig-'1', breast-right, breast_quad-central, irradiat-no]).
	example( 39, no_recurrence_events, [age-'40_49', menopause-premeno, tumor_size-'10_14', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-left, breast_quad-left_low, irradiat-no]).
	example( 40, no_recurrence_events, [age-'40_49', menopause-premeno, tumor_size-'30_34', inv_nodes-'0_2', node_caps-no, deg_malig-'1', breast-left, breast_quad-left_low, irradiat-no]).
	example( 41, no_recurrence_events, [age-'50_59', menopause-ge40, tumor_size-'20_24', inv_nodes-'0_2', node_caps-no, deg_malig-'1', breast-right, breast_quad-left_low, irradiat-no]).
	example( 42, no_recurrence_events, [age-'60_69', menopause-ge40, tumor_size-'25_29', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-left, breast_quad-left_low, irradiat-no]).
	example( 43, no_recurrence_events, [age-'60_69', menopause-ge40, tumor_size-'5_9', inv_nodes-'0_2', node_caps-no, deg_malig-'1', breast-left, breast_quad-central, irradiat-no]).
	example( 44, no_recurrence_events, [age-'40_49', menopause-premeno, tumor_size-'10_14', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-left, breast_quad-left_up, irradiat-no]).
	example( 45, no_recurrence_events, [age-'50_59', menopause-ge40, tumor_size-'50_54', inv_nodes-'0_2', node_caps-no, deg_malig-'1', breast-right, breast_quad-right_up, irradiat-no]).
	example( 46, no_recurrence_events, [age-'50_59', menopause-ge40, tumor_size-'30_34', inv_nodes-'0_2', node_caps-no, deg_malig-'1', breast-left, breast_quad-left_up, irradiat-no]).
	example( 47, no_recurrence_events, [age-'40_49', menopause-premeno, tumor_size-'25_29', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-right, breast_quad-left_low, irradiat-no]).
	example( 48, no_recurrence_events, [age-'50_59', menopause-premeno, tumor_size-'25_29', inv_nodes-'0_2', node_caps-no, deg_malig-'1', breast-right, breast_quad-left_up, irradiat-no]).
	example( 49, no_recurrence_events, [age-'40_49', menopause-premeno, tumor_size-'20_24', inv_nodes-'0_2', node_caps-no, deg_malig-'1', breast-right, breast_quad-right_up, irradiat-no]).
	example( 50, no_recurrence_events, [age-'40_49', menopause-premeno, tumor_size-'20_24', inv_nodes-'0_2', node_caps-no, deg_malig-'1', breast-right, breast_quad-left_low, irradiat-no]).
	example( 51, no_recurrence_events, [age-'50_59', menopause-lt40, tumor_size-'15_19', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-left, breast_quad-left_low, irradiat-no]).
	example( 52, no_recurrence_events, [age-'30_39', menopause-premeno, tumor_size-'20_24', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-left, breast_quad-right_low, irradiat-no]).
	example( 53, no_recurrence_events, [age-'50_59', menopause-premeno, tumor_size-'15_19', inv_nodes-'0_2', node_caps-no, deg_malig-'1', breast-left, breast_quad-left_low, irradiat-no]).
	example( 54, no_recurrence_events, [age-'70_79', menopause-ge40, tumor_size-'20_24', inv_nodes-'0_2', node_caps-no, deg_malig-'3', breast-left, breast_quad-left_up, irradiat-no]).
	example( 55, no_recurrence_events, [age-'70_79', menopause-ge40, tumor_size-'40_44', inv_nodes-'0_2', node_caps-no, deg_malig-'1', breast-right, breast_quad-left_up, irradiat-no]).
	example( 56, no_recurrence_events, [age-'70_79', menopause-ge40, tumor_size-'40_44', inv_nodes-'0_2', node_caps-no, deg_malig-'1', breast-right, breast_quad-right_up, irradiat-no]).
	example( 57, no_recurrence_events, [age-'50_59', menopause-ge40, tumor_size-'0_4', inv_nodes-'0_2', node_caps-no, deg_malig-'1', breast-right, breast_quad-central, irradiat-no]).
	example( 58, no_recurrence_events, [age-'50_59', menopause-ge40, tumor_size-'5_9', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-right, breast_quad-right_up, irradiat-no]).
	example( 59, no_recurrence_events, [age-'60_69', menopause-ge40, tumor_size-'30_34', inv_nodes-'0_2', node_caps-no, deg_malig-'1', breast-left, breast_quad-left_up, irradiat-no]).
	example( 60, no_recurrence_events, [age-'60_69', menopause-ge40, tumor_size-'15_19', inv_nodes-'0_2', node_caps-no, deg_malig-'1', breast-right, breast_quad-left_up, irradiat-no]).
	example( 61, no_recurrence_events, [age-'40_49', menopause-premeno, tumor_size-'20_24', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-left, breast_quad-central, irradiat-no]).
	example( 62, no_recurrence_events, [age-'40_49', menopause-premeno, tumor_size-'10_14', inv_nodes-'0_2', node_caps-no, deg_malig-'1', breast-right, breast_quad-right_low, irradiat-no]).
	example( 63, no_recurrence_events, [age-'50_59', menopause-ge40, tumor_size-'0_4', inv_nodes-'0_2', node_caps-no, deg_malig-'1', breast-left, breast_quad-left_low, irradiat-no]).
	example( 64, no_recurrence_events, [age-'20_29', menopause-premeno, tumor_size-'35_39', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-right, breast_quad-right_up, irradiat-no]).
	example( 65, no_recurrence_events, [age-'40_49', menopause-premeno, tumor_size-'25_29', inv_nodes-'0_2', node_caps-no, deg_malig-'1', breast-left, breast_quad-right_low, irradiat-no]).
	example( 66, no_recurrence_events, [age-'40_49', menopause-premeno, tumor_size-'10_14', inv_nodes-'0_2', node_caps-no, deg_malig-'1', breast-right, breast_quad-left_up, irradiat-no]).
	example( 67, no_recurrence_events, [age-'40_49', menopause-premeno, tumor_size-'25_29', inv_nodes-'0_2', node_caps-no, deg_malig-'1', breast-right, breast_quad-right_low, irradiat-no]).
	example( 68, no_recurrence_events, [age-'50_59', menopause-ge40, tumor_size-'20_24', inv_nodes-'0_2', node_caps-no, deg_malig-'3', breast-left, breast_quad-left_up, irradiat-no]).
	example( 69, no_recurrence_events, [age-'50_59', menopause-ge40, tumor_size-'35_39', inv_nodes-'0_2', node_caps-no, deg_malig-'3', breast-left, breast_quad-left_low, irradiat-no]).
	example( 70, no_recurrence_events, [age-'60_69', menopause-ge40, tumor_size-'50_54', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-left, breast_quad-left_low, irradiat-no]).
	example( 71, no_recurrence_events, [age-'60_69', menopause-ge40, tumor_size-'10_14', inv_nodes-'0_2', node_caps-no, deg_malig-'1', breast-left, breast_quad-left_low, irradiat-no]).
	example( 72, no_recurrence_events, [age-'40_49', menopause-premeno, tumor_size-'25_29', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-right, breast_quad-left_up, irradiat-no]).
	example( 73, no_recurrence_events, [age-'60_69', menopause-ge40, tumor_size-'20_24', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-left, breast_quad-left_up, irradiat-no]).
	example( 74, no_recurrence_events, [age-'50_59', menopause-premeno, tumor_size-'15_19', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-right, breast_quad-right_low, irradiat-no]).
	example( 75, no_recurrence_events, [age-'30_39', menopause-premeno, tumor_size-'5_9', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-left, breast_quad-right_low, irradiat-no]).
	example( 76, no_recurrence_events, [age-'50_59', menopause-ge40, tumor_size-'10_14', inv_nodes-'0_2', node_caps-no, deg_malig-'1', breast-left, breast_quad-left_low, irradiat-no]).
	example( 77, no_recurrence_events, [age-'50_59', menopause-ge40, tumor_size-'10_14', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-left, breast_quad-left_low, irradiat-no]).
	example( 78, no_recurrence_events, [age-'30_39', menopause-premeno, tumor_size-'25_29', inv_nodes-'0_2', node_caps-no, deg_malig-'1', breast-left, breast_quad-central, irradiat-no]).
	example( 79, no_recurrence_events, [age-'50_59', menopause-premeno, tumor_size-'25_29', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-left, breast_quad-left_low, irradiat-no]).
	example( 80, no_recurrence_events, [age-'40_49', menopause-premeno, tumor_size-'25_29', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-right, breast_quad-central, irradiat-no]).
	example( 81, no_recurrence_events, [age-'50_59', menopause-ge40, tumor_size-'10_14', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-right, breast_quad-left_low, irradiat-no]).
	example( 82, no_recurrence_events, [age-'60_69', menopause-ge40, tumor_size-'10_14', inv_nodes-'0_2', node_caps-no, deg_malig-'1', breast-left, breast_quad-left_up, irradiat-no]).
	example( 83, no_recurrence_events, [age-'60_69', menopause-ge40, tumor_size-'15_19', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-right, breast_quad-left_low, irradiat-no]).
	example( 84, no_recurrence_events, [age-'50_59', menopause-ge40, tumor_size-'15_19', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-right, breast_quad-left_low, irradiat-no]).
	example( 85, no_recurrence_events, [age-'40_49', menopause-premeno, tumor_size-'20_24', inv_nodes-'0_2', node_caps-no, deg_malig-'1', breast-left, breast_quad-right_low, irradiat-no]).
	example( 86, no_recurrence_events, [age-'50_59', menopause-ge40, tumor_size-'35_39', inv_nodes-'0_2', node_caps-no, deg_malig-'3', breast-left, breast_quad-left_up, irradiat-no]).
	example( 87, no_recurrence_events, [age-'60_69', menopause-ge40, tumor_size-'25_29', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-right, breast_quad-left_low, irradiat-no]).
	example( 88, no_recurrence_events, [age-'70_79', menopause-ge40, tumor_size-'0_4', inv_nodes-'0_2', node_caps-no, deg_malig-'1', breast-left, breast_quad-right_low, irradiat-no]).
	example( 89, no_recurrence_events, [age-'50_59', menopause-ge40, tumor_size-'20_24', inv_nodes-'0_2', node_caps-no, deg_malig-'3', breast-right, breast_quad-left_up, irradiat-no]).
	example( 90, no_recurrence_events, [age-'40_49', menopause-premeno, tumor_size-'40_44', inv_nodes-'0_2', node_caps-no, deg_malig-'1', breast-right, breast_quad-left_up, irradiat-no]).
	example( 91, no_recurrence_events, [age-'30_39', menopause-premeno, tumor_size-'0_4', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-right, breast_quad-central, irradiat-no]).
	example( 92, no_recurrence_events, [age-'50_59', menopause-ge40, tumor_size-'20_24', inv_nodes-'0_2', node_caps-no, deg_malig-'3', breast-left, breast_quad-left_up, irradiat-no]).
	example( 93, no_recurrence_events, [age-'50_59', menopause-ge40, tumor_size-'25_29', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-right, breast_quad-left_up, irradiat-no]).
	example( 94, no_recurrence_events, [age-'60_69', menopause-ge40, tumor_size-'20_24', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-right, breast_quad-left_up, irradiat-no]).
	example( 95, no_recurrence_events, [age-'50_59', menopause-premeno, tumor_size-'10_14', inv_nodes-'0_2', node_caps-no, deg_malig-'1', breast-left, breast_quad-left_low, irradiat-no]).
	example( 96, no_recurrence_events, [age-'40_49', menopause-premeno, tumor_size-'30_34', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-right, breast_quad-right_low, irradiat-no]).
	example( 97, no_recurrence_events, [age-'60_69', menopause-ge40, tumor_size-'30_34', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-left, breast_quad-left_up, irradiat-no]).
	example( 98, no_recurrence_events, [age-'60_69', menopause-ge40, tumor_size-'15_19', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-right, breast_quad-left_up, irradiat-no]).
	example( 99, no_recurrence_events, [age-'40_49', menopause-premeno, tumor_size-'30_34', inv_nodes-'0_2', node_caps-no, deg_malig-'1', breast-left, breast_quad-right_up, irradiat-no]).
	example(100, no_recurrence_events, [age-'30_39', menopause-premeno, tumor_size-'25_29', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-left, breast_quad-left_low, irradiat-no]).
	example(101, no_recurrence_events, [age-'40_49', menopause-ge40, tumor_size-'20_24', inv_nodes-'0_2', node_caps-no, deg_malig-'3', breast-left, breast_quad-left_low, irradiat-no]).
	example(102, no_recurrence_events, [age-'50_59', menopause-ge40, tumor_size-'30_34', inv_nodes-'0_2', node_caps-no, deg_malig-'3', breast-right, breast_quad-left_low, irradiat-no]).
	example(103, no_recurrence_events, [age-'50_59', menopause-premeno, tumor_size-'25_29', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-right, breast_quad-right_low, irradiat-no]).
	example(104, no_recurrence_events, [age-'40_49', menopause-premeno, tumor_size-'20_24', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-left, breast_quad-right_low, irradiat-no]).
	example(105, no_recurrence_events, [age-'40_49', menopause-premeno, tumor_size-'10_14', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-right, breast_quad-left_low, irradiat-no]).
	example(106, no_recurrence_events, [age-'40_49', menopause-premeno, tumor_size-'30_34', inv_nodes-'0_2', node_caps-no, deg_malig-'1', breast-right, breast_quad-left_up, irradiat-no]).
	example(107, no_recurrence_events, [age-'40_49', menopause-premeno, tumor_size-'20_24', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-left, breast_quad-left_up, irradiat-no]).
	example(108, no_recurrence_events, [age-'30_39', menopause-premeno, tumor_size-'40_44', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-right, breast_quad-right_up, irradiat-no]).
	example(109, no_recurrence_events, [age-'40_49', menopause-premeno, tumor_size-'30_34', inv_nodes-'0_2', node_caps-no, deg_malig-'3', breast-right, breast_quad-right_up, irradiat-no]).
	example(110, no_recurrence_events, [age-'60_69', menopause-ge40, tumor_size-'30_34', inv_nodes-'0_2', node_caps-no, deg_malig-'1', breast-right, breast_quad-left_up, irradiat-no]).
	example(111, no_recurrence_events, [age-'50_59', menopause-ge40, tumor_size-'25_29', inv_nodes-'0_2', node_caps-no, deg_malig-'1', breast-left, breast_quad-left_low, irradiat-no]).
	example(112, no_recurrence_events, [age-'50_59', menopause-ge40, tumor_size-'15_19', inv_nodes-'0_2', node_caps-no, deg_malig-'1', breast-right, breast_quad-central, irradiat-no]).
	example(113, no_recurrence_events, [age-'40_49', menopause-premeno, tumor_size-'20_24', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-right, breast_quad-left_up, irradiat-no]).
	example(114, no_recurrence_events, [age-'40_49', menopause-premeno, tumor_size-'10_14', inv_nodes-'0_2', node_caps-no, deg_malig-'1', breast-right, breast_quad-left_up, irradiat-no]).
	example(115, no_recurrence_events, [age-'40_49', menopause-premeno, tumor_size-'35_39', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-right, breast_quad-right_up, irradiat-no]).
	example(116, no_recurrence_events, [age-'50_59', menopause-ge40, tumor_size-'20_24', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-right, breast_quad-left_up, irradiat-no]).
	example(117, no_recurrence_events, [age-'30_39', menopause-premeno, tumor_size-'15_19', inv_nodes-'0_2', node_caps-no, deg_malig-'1', breast-left, breast_quad-left_low, irradiat-no]).
	example(118, no_recurrence_events, [age-'40_49', menopause-ge40, tumor_size-'20_24', inv_nodes-'0_2', node_caps-no, deg_malig-'3', breast-left, breast_quad-left_up, irradiat-no]).
	example(119, no_recurrence_events, [age-'30_39', menopause-premeno, tumor_size-'10_14', inv_nodes-'0_2', node_caps-no, deg_malig-'1', breast-right, breast_quad-left_low, irradiat-no]).
	example(120, no_recurrence_events, [age-'60_69', menopause-ge40, tumor_size-'15_19', inv_nodes-'0_2', node_caps-no, deg_malig-'1', breast-left, breast_quad-right_low, irradiat-no]).
	example(121, no_recurrence_events, [age-'60_69', menopause-ge40, tumor_size-'20_24', inv_nodes-'0_2', node_caps-no, deg_malig-'1', breast-left, breast_quad-left_low, irradiat-no]).
	example(122, no_recurrence_events, [age-'50_59', menopause-ge40, tumor_size-'15_19', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-right, breast_quad-right_up, irradiat-no]).
	example(123, no_recurrence_events, [age-'50_59', menopause-ge40, tumor_size-'40_44', inv_nodes-'0_2', node_caps-no, deg_malig-'3', breast-left, breast_quad-left_up, irradiat-no]).
	example(124, no_recurrence_events, [age-'50_59', menopause-ge40, tumor_size-'30_34', inv_nodes-'0_2', node_caps-no, deg_malig-'1', breast-right, breast_quad-left_low, irradiat-no]).
	example(125, no_recurrence_events, [age-'60_69', menopause-ge40, tumor_size-'10_14', inv_nodes-'0_2', node_caps-no, deg_malig-'1', breast-right, breast_quad-left_low, irradiat-no]).
	example(126, no_recurrence_events, [age-'70_79', menopause-ge40, tumor_size-'10_14', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-left, breast_quad-central, irradiat-no]).
	example(127, no_recurrence_events, [age-'30_39', menopause-premeno, tumor_size-'30_34', inv_nodes-'6_8', node_caps-yes, deg_malig-'2', breast-right, breast_quad-right_up, irradiat-no]).
	example(128, no_recurrence_events, [age-'30_39', menopause-premeno, tumor_size-'25_29', inv_nodes-'6_8', node_caps-yes, deg_malig-'2', breast-right, breast_quad-left_up, irradiat-yes]).
	example(129, no_recurrence_events, [age-'50_59', menopause-premeno, tumor_size-'25_29', inv_nodes-'0_2', node_caps-yes, deg_malig-'2', breast-left, breast_quad-left_up, irradiat-no]).
	example(130, no_recurrence_events, [age-'40_49', menopause-premeno, tumor_size-'35_39', inv_nodes-'9_11', node_caps-yes, deg_malig-'2', breast-right, breast_quad-left_up, irradiat-yes]).
	example(131, no_recurrence_events, [age-'40_49', menopause-premeno, tumor_size-'35_39', inv_nodes-'9_11', node_caps-yes, deg_malig-'2', breast-right, breast_quad-right_up, irradiat-yes]).
	example(132, no_recurrence_events, [age-'40_49', menopause-premeno, tumor_size-'40_44', inv_nodes-'3_5', node_caps-yes, deg_malig-'3', breast-right, breast_quad-left_up, irradiat-yes]).
	example(133, no_recurrence_events, [age-'40_49', menopause-premeno, tumor_size-'30_34', inv_nodes-'6_8', node_caps-no, deg_malig-'2', breast-left, breast_quad-left_up, irradiat-no]).
	example(134, no_recurrence_events, [age-'50_59', menopause-ge40, tumor_size-'40_44', inv_nodes-'0_2', node_caps-no, deg_malig-'3', breast-left, breast_quad-right_up, irradiat-no]).
	example(135, no_recurrence_events, [age-'60_69', menopause-ge40, tumor_size-'30_34', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-left, breast_quad-left_low, irradiat-yes]).
	example(136, no_recurrence_events, [age-'30_39', menopause-premeno, tumor_size-'20_24', inv_nodes-'3_5', node_caps-no, deg_malig-'2', breast-right, breast_quad-central, irradiat-no]).
	example(137, no_recurrence_events, [age-'30_39', menopause-premeno, tumor_size-'40_44', inv_nodes-'3_5', node_caps-no, deg_malig-'3', breast-right, breast_quad-right_up, irradiat-yes]).
	example(138, no_recurrence_events, [age-'40_49', menopause-premeno, tumor_size-'5_9', inv_nodes-'0_2', node_caps-no, deg_malig-'1', breast-left, breast_quad-left_low, irradiat-yes]).
	example(139, no_recurrence_events, [age-'30_39', menopause-premeno, tumor_size-'40_44', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-left, breast_quad-left_low, irradiat-yes]).
	example(140, no_recurrence_events, [age-'40_49', menopause-premeno, tumor_size-'30_34', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-left, breast_quad-right_low, irradiat-no]).
	example(141, no_recurrence_events, [age-'50_59', menopause-ge40, tumor_size-'40_44', inv_nodes-'3_5', node_caps-yes, deg_malig-'2', breast-left, breast_quad-left_low, irradiat-no]).
	example(142, no_recurrence_events, [age-'50_59', menopause-premeno, tumor_size-'20_24', inv_nodes-'3_5', node_caps-yes, deg_malig-'2', breast-left, breast_quad-left_low, irradiat-no]).
	example(143, no_recurrence_events, [age-'60_69', menopause-ge40, tumor_size-'10_14', inv_nodes-'0_2', node_caps-no, deg_malig-'1', breast-left, breast_quad-left_up, irradiat-no]).
	example(144, no_recurrence_events, [age-'40_49', menopause-premeno, tumor_size-'45_49', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-left, breast_quad-left_low, irradiat-yes]).
	example(145, no_recurrence_events, [age-'60_69', menopause-ge40, tumor_size-'45_49', inv_nodes-'6_8', node_caps-yes, deg_malig-'3', breast-left, breast_quad-central, irradiat-no]).
	example(146, no_recurrence_events, [age-'40_49', menopause-premeno, tumor_size-'25_29', inv_nodes-'0_2', node_caps-_, deg_malig-'2', breast-left, breast_quad-right_low, irradiat-yes]).
	example(147, no_recurrence_events, [age-'60_69', menopause-ge40, tumor_size-'50_54', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-right, breast_quad-left_up, irradiat-yes]).
	example(148, no_recurrence_events, [age-'50_59', menopause-premeno, tumor_size-'30_34', inv_nodes-'3_5', node_caps-yes, deg_malig-'2', breast-left, breast_quad-left_low, irradiat-yes]).
	example(149, no_recurrence_events, [age-'30_39', menopause-premeno, tumor_size-'20_24', inv_nodes-'0_2', node_caps-no, deg_malig-'3', breast-left, breast_quad-central, irradiat-no]).
	example(150, no_recurrence_events, [age-'50_59', menopause-lt40, tumor_size-'30_34', inv_nodes-'0_2', node_caps-no, deg_malig-'3', breast-right, breast_quad-left_up, irradiat-no]).
	example(151, no_recurrence_events, [age-'50_59', menopause-ge40, tumor_size-'25_29', inv_nodes-'15_17', node_caps-yes, deg_malig-'3', breast-right, breast_quad-left_up, irradiat-no]).
	example(152, no_recurrence_events, [age-'60_69', menopause-ge40, tumor_size-'30_34', inv_nodes-'3_5', node_caps-yes, deg_malig-'3', breast-left, breast_quad-left_low, irradiat-no]).
	example(153, no_recurrence_events, [age-'50_59', menopause-ge40, tumor_size-'35_39', inv_nodes-'15_17', node_caps-no, deg_malig-'3', breast-left, breast_quad-left_low, irradiat-no]).
	example(154, no_recurrence_events, [age-'60_69', menopause-ge40, tumor_size-'15_19', inv_nodes-'0_2', node_caps-no, deg_malig-'3', breast-right, breast_quad-left_up, irradiat-yes]).
	example(155, no_recurrence_events, [age-'30_39', menopause-lt40, tumor_size-'15_19', inv_nodes-'0_2', node_caps-no, deg_malig-'3', breast-right, breast_quad-left_up, irradiat-no]).
	example(156, no_recurrence_events, [age-'60_69', menopause-ge40, tumor_size-'40_44', inv_nodes-'3_5', node_caps-no, deg_malig-'2', breast-right, breast_quad-left_up, irradiat-yes]).
	example(157, no_recurrence_events, [age-'50_59', menopause-ge40, tumor_size-'25_29', inv_nodes-'3_5', node_caps-yes, deg_malig-'3', breast-right, breast_quad-left_up, irradiat-no]).
	example(158, no_recurrence_events, [age-'50_59', menopause-premeno, tumor_size-'30_34', inv_nodes-'0_2', node_caps-no, deg_malig-'1', breast-left, breast_quad-central, irradiat-no]).
	example(159, no_recurrence_events, [age-'50_59', menopause-ge40, tumor_size-'30_34', inv_nodes-'0_2', node_caps-no, deg_malig-'1', breast-right, breast_quad-central, irradiat-no]).
	example(160, no_recurrence_events, [age-'40_49', menopause-premeno, tumor_size-'35_39', inv_nodes-'0_2', node_caps-no, deg_malig-'1', breast-left, breast_quad-left_low, irradiat-no]).
	example(161, no_recurrence_events, [age-'40_49', menopause-premeno, tumor_size-'25_29', inv_nodes-'0_2', node_caps-no, deg_malig-'3', breast-right, breast_quad-left_up, irradiat-yes]).
	example(162, no_recurrence_events, [age-'40_49', menopause-premeno, tumor_size-'30_34', inv_nodes-'3_5', node_caps-yes, deg_malig-'2', breast-right, breast_quad-left_low, irradiat-no]).
	example(163, no_recurrence_events, [age-'60_69', menopause-ge40, tumor_size-'10_14', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-right, breast_quad-left_up, irradiat-yes]).
	example(164, no_recurrence_events, [age-'60_69', menopause-ge40, tumor_size-'25_29', inv_nodes-'3_5', node_caps-_, deg_malig-'1', breast-right, breast_quad-left_up, irradiat-yes]).
	example(165, no_recurrence_events, [age-'60_69', menopause-ge40, tumor_size-'25_29', inv_nodes-'3_5', node_caps-_, deg_malig-'1', breast-right, breast_quad-left_low, irradiat-yes]).
	example(166, no_recurrence_events, [age-'40_49', menopause-premeno, tumor_size-'20_24', inv_nodes-'3_5', node_caps-no, deg_malig-'2', breast-right, breast_quad-left_up, irradiat-no]).
	example(167, no_recurrence_events, [age-'40_49', menopause-premeno, tumor_size-'20_24', inv_nodes-'3_5', node_caps-no, deg_malig-'2', breast-right, breast_quad-left_low, irradiat-no]).
	example(168, no_recurrence_events, [age-'40_49', menopause-ge40, tumor_size-'40_44', inv_nodes-'15_17', node_caps-yes, deg_malig-'2', breast-right, breast_quad-left_up, irradiat-yes]).
	example(169, no_recurrence_events, [age-'50_59', menopause-premeno, tumor_size-'10_14', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-right, breast_quad-left_up, irradiat-no]).
	example(170, no_recurrence_events, [age-'40_49', menopause-ge40, tumor_size-'30_34', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-left, breast_quad-left_up, irradiat-yes]).
	example(171, no_recurrence_events, [age-'30_39', menopause-premeno, tumor_size-'20_24', inv_nodes-'3_5', node_caps-yes, deg_malig-'2', breast-right, breast_quad-left_up, irradiat-yes]).
	example(172, no_recurrence_events, [age-'30_39', menopause-premeno, tumor_size-'15_19', inv_nodes-'0_2', node_caps-no, deg_malig-'1', breast-left, breast_quad-left_low, irradiat-no]).
	example(173, no_recurrence_events, [age-'60_69', menopause-ge40, tumor_size-'30_34', inv_nodes-'6_8', node_caps-yes, deg_malig-'2', breast-right, breast_quad-right_up, irradiat-no]).
	example(174, no_recurrence_events, [age-'50_59', menopause-ge40, tumor_size-'20_24', inv_nodes-'3_5', node_caps-yes, deg_malig-'2', breast-right, breast_quad-left_up, irradiat-no]).
	example(175, no_recurrence_events, [age-'50_59', menopause-premeno, tumor_size-'25_29', inv_nodes-'3_5', node_caps-yes, deg_malig-'2', breast-left, breast_quad-left_low, irradiat-yes]).
	example(176, no_recurrence_events, [age-'40_49', menopause-premeno, tumor_size-'30_34', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-right, breast_quad-right_up, irradiat-yes]).
	example(177, no_recurrence_events, [age-'40_49', menopause-ge40, tumor_size-'25_29', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-left, breast_quad-left_low, irradiat-no]).
	example(178, no_recurrence_events, [age-'60_69', menopause-ge40, tumor_size-'10_14', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-left, breast_quad-left_low, irradiat-no]).
	example(179, no_recurrence_events, [age-'50_59', menopause-premeno, tumor_size-'25_29', inv_nodes-'3_5', node_caps-no, deg_malig-'2', breast-right, breast_quad-left_up, irradiat-yes]).
	example(180, no_recurrence_events, [age-'40_49', menopause-premeno, tumor_size-'20_24', inv_nodes-'0_2', node_caps-no, deg_malig-'3', breast-right, breast_quad-left_low, irradiat-yes]).
	example(181, no_recurrence_events, [age-'40_49', menopause-premeno, tumor_size-'35_39', inv_nodes-'0_2', node_caps-yes, deg_malig-'3', breast-right, breast_quad-left_up, irradiat-yes]).
	example(182, no_recurrence_events, [age-'40_49', menopause-premeno, tumor_size-'35_39', inv_nodes-'0_2', node_caps-yes, deg_malig-'3', breast-right, breast_quad-left_low, irradiat-yes]).
	example(183, no_recurrence_events, [age-'40_49', menopause-premeno, tumor_size-'25_29', inv_nodes-'0_2', node_caps-no, deg_malig-'1', breast-right, breast_quad-left_low, irradiat-yes]).
	example(184, no_recurrence_events, [age-'50_59', menopause-ge40, tumor_size-'30_34', inv_nodes-'9_11', node_caps-_, deg_malig-'3', breast-left, breast_quad-left_up, irradiat-yes]).
	example(185, no_recurrence_events, [age-'50_59', menopause-ge40, tumor_size-'30_34', inv_nodes-'9_11', node_caps-_, deg_malig-'3', breast-left, breast_quad-left_low, irradiat-yes]).
	example(186, no_recurrence_events, [age-'40_49', menopause-premeno, tumor_size-'20_24', inv_nodes-'6_8', node_caps-no, deg_malig-'2', breast-right, breast_quad-left_low, irradiat-yes]).
	example(187, no_recurrence_events, [age-'50_59', menopause-ge40, tumor_size-'25_29', inv_nodes-'0_2', node_caps-no, deg_malig-'1', breast-left, breast_quad-right_low, irradiat-no]).
	example(188, no_recurrence_events, [age-'60_69', menopause-ge40, tumor_size-'15_19', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-left, breast_quad-left_up, irradiat-yes]).
	example(189, no_recurrence_events, [age-'40_49', menopause-premeno, tumor_size-'10_14', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-right, breast_quad-left_up, irradiat-no]).
	example(190, no_recurrence_events, [age-'50_59', menopause-ge40, tumor_size-'20_24', inv_nodes-'0_2', node_caps-yes, deg_malig-'2', breast-right, breast_quad-left_up, irradiat-no]).
	example(191, no_recurrence_events, [age-'40_49', menopause-premeno, tumor_size-'15_19', inv_nodes-'12_14', node_caps-no, deg_malig-'3', breast-right, breast_quad-right_low, irradiat-yes]).
	example(192, no_recurrence_events, [age-'40_49', menopause-premeno, tumor_size-'25_29', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-left, breast_quad-left_up, irradiat-yes]).
	example(193, no_recurrence_events, [age-'50_59', menopause-ge40, tumor_size-'30_34', inv_nodes-'6_8', node_caps-yes, deg_malig-'2', breast-left, breast_quad-left_low, irradiat-no]).
	example(194, no_recurrence_events, [age-'30_39', menopause-premeno, tumor_size-'10_14', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-left, breast_quad-right_low, irradiat-no]).
	example(195, no_recurrence_events, [age-'50_59', menopause-premeno, tumor_size-'50_54', inv_nodes-'0_2', node_caps-yes, deg_malig-'2', breast-right, breast_quad-left_up, irradiat-yes]).
	example(196, no_recurrence_events, [age-'50_59', menopause-ge40, tumor_size-'35_39', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-left, breast_quad-left_up, irradiat-no]).
	example(197, no_recurrence_events, [age-'50_59', menopause-premeno, tumor_size-'10_14', inv_nodes-'3_5', node_caps-no, deg_malig-'1', breast-right, breast_quad-left_up, irradiat-no]).
	example(198, no_recurrence_events, [age-'40_49', menopause-premeno, tumor_size-'10_14', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-left, breast_quad-left_low, irradiat-yes]).
	example(199, no_recurrence_events, [age-'50_59', menopause-ge40, tumor_size-'15_19', inv_nodes-'0_2', node_caps-yes, deg_malig-'2', breast-left, breast_quad-central, irradiat-yes]).
	example(200, no_recurrence_events, [age-'50_59', menopause-premeno, tumor_size-'25_29', inv_nodes-'0_2', node_caps-no, deg_malig-'1', breast-left, breast_quad-left_low, irradiat-no]).
	example(201, no_recurrence_events, [age-'60_69', menopause-ge40, tumor_size-'25_29', inv_nodes-'0_2', node_caps-no, deg_malig-'3', breast-right, breast_quad-left_low, irradiat-no]).
	example(202, recurrence_events, [age-'50_59', menopause-premeno, tumor_size-'15_19', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-left, breast_quad-left_low, irradiat-no]).
	example(203, recurrence_events, [age-'40_49', menopause-premeno, tumor_size-'40_44', inv_nodes-'0_2', node_caps-no, deg_malig-'1', breast-left, breast_quad-left_low, irradiat-no]).
	example(204, recurrence_events, [age-'50_59', menopause-ge40, tumor_size-'35_39', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-left, breast_quad-left_low, irradiat-no]).
	example(205, recurrence_events, [age-'50_59', menopause-premeno, tumor_size-'25_29', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-left, breast_quad-right_up, irradiat-no]).
	example(206, recurrence_events, [age-'30_39', menopause-premeno, tumor_size-'0_4', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-right, breast_quad-central, irradiat-no]).
	example(207, recurrence_events, [age-'50_59', menopause-ge40, tumor_size-'30_34', inv_nodes-'0_2', node_caps-no, deg_malig-'3', breast-left, breast_quad-_, irradiat-no]).
	example(208, recurrence_events, [age-'50_59', menopause-premeno, tumor_size-'25_29', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-left, breast_quad-right_up, irradiat-no]).
	example(209, recurrence_events, [age-'50_59', menopause-premeno, tumor_size-'30_34', inv_nodes-'0_2', node_caps-no, deg_malig-'3', breast-left, breast_quad-right_up, irradiat-no]).
	example(210, recurrence_events, [age-'40_49', menopause-premeno, tumor_size-'35_39', inv_nodes-'0_2', node_caps-no, deg_malig-'1', breast-right, breast_quad-left_up, irradiat-no]).
	example(211, recurrence_events, [age-'40_49', menopause-premeno, tumor_size-'20_24', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-left, breast_quad-left_low, irradiat-no]).
	example(212, recurrence_events, [age-'50_59', menopause-ge40, tumor_size-'20_24', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-right, breast_quad-central, irradiat-no]).
	example(213, recurrence_events, [age-'40_49', menopause-premeno, tumor_size-'30_34', inv_nodes-'0_2', node_caps-no, deg_malig-'3', breast-right, breast_quad-right_up, irradiat-no]).
	example(214, recurrence_events, [age-'50_59', menopause-premeno, tumor_size-'25_29', inv_nodes-'0_2', node_caps-no, deg_malig-'1', breast-right, breast_quad-left_up, irradiat-no]).
	example(215, recurrence_events, [age-'60_69', menopause-ge40, tumor_size-'40_44', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-right, breast_quad-left_low, irradiat-no]).
	example(216, recurrence_events, [age-'40_49', menopause-ge40, tumor_size-'20_24', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-right, breast_quad-left_up, irradiat-no]).
	example(217, recurrence_events, [age-'50_59', menopause-ge40, tumor_size-'20_24', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-left, breast_quad-left_up, irradiat-no]).
	example(218, recurrence_events, [age-'40_49', menopause-premeno, tumor_size-'15_19', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-left, breast_quad-left_up, irradiat-no]).
	example(219, recurrence_events, [age-'60_69', menopause-ge40, tumor_size-'30_34', inv_nodes-'0_2', node_caps-no, deg_malig-'3', breast-right, breast_quad-central, irradiat-no]).
	example(220, recurrence_events, [age-'30_39', menopause-premeno, tumor_size-'15_19', inv_nodes-'0_2', node_caps-no, deg_malig-'1', breast-right, breast_quad-left_low, irradiat-no]).
	example(221, recurrence_events, [age-'40_49', menopause-premeno, tumor_size-'25_29', inv_nodes-'0_2', node_caps-no, deg_malig-'3', breast-left, breast_quad-right_up, irradiat-no]).
	example(222, recurrence_events, [age-'30_39', menopause-premeno, tumor_size-'30_34', inv_nodes-'0_2', node_caps-no, deg_malig-'1', breast-right, breast_quad-left_up, irradiat-no]).
	example(223, recurrence_events, [age-'60_69', menopause-ge40, tumor_size-'25_29', inv_nodes-'0_2', node_caps-no, deg_malig-'3', breast-left, breast_quad-right_low, irradiat-yes]).
	example(224, recurrence_events, [age-'60_69', menopause-ge40, tumor_size-'20_24', inv_nodes-'0_2', node_caps-no, deg_malig-'3', breast-right, breast_quad-left_low, irradiat-no]).
	example(225, recurrence_events, [age-'30_39', menopause-premeno, tumor_size-'25_29', inv_nodes-'3_5', node_caps-yes, deg_malig-'3', breast-left, breast_quad-left_low, irradiat-yes]).
	example(226, recurrence_events, [age-'40_49', menopause-ge40, tumor_size-'20_24', inv_nodes-'3_5', node_caps-no, deg_malig-'3', breast-right, breast_quad-left_low, irradiat-yes]).
	example(227, recurrence_events, [age-'40_49', menopause-premeno, tumor_size-'30_34', inv_nodes-'15_17', node_caps-yes, deg_malig-'3', breast-left, breast_quad-left_low, irradiat-no]).
	example(228, recurrence_events, [age-'50_59', menopause-premeno, tumor_size-'30_34', inv_nodes-'0_2', node_caps-no, deg_malig-'3', breast-right, breast_quad-left_up, irradiat-yes]).
	example(229, recurrence_events, [age-'60_69', menopause-ge40, tumor_size-'40_44', inv_nodes-'3_5', node_caps-yes, deg_malig-'3', breast-right, breast_quad-left_low, irradiat-no]).
	example(230, recurrence_events, [age-'60_69', menopause-ge40, tumor_size-'45_49', inv_nodes-'0_2', node_caps-no, deg_malig-'1', breast-right, breast_quad-right_up, irradiat-yes]).
	example(231, recurrence_events, [age-'50_59', menopause-premeno, tumor_size-'50_54', inv_nodes-'9_11', node_caps-yes, deg_malig-'2', breast-right, breast_quad-left_up, irradiat-no]).
	example(232, recurrence_events, [age-'40_49', menopause-premeno, tumor_size-'30_34', inv_nodes-'3_5', node_caps-no, deg_malig-'2', breast-right, breast_quad-left_up, irradiat-no]).
	example(233, recurrence_events, [age-'30_39', menopause-premeno, tumor_size-'30_34', inv_nodes-'3_5', node_caps-no, deg_malig-'3', breast-right, breast_quad-left_up, irradiat-yes]).
	example(234, recurrence_events, [age-'70_79', menopause-ge40, tumor_size-'15_19', inv_nodes-'9_11', node_caps-_, deg_malig-'1', breast-left, breast_quad-left_low, irradiat-yes]).
	example(235, recurrence_events, [age-'60_69', menopause-ge40, tumor_size-'30_34', inv_nodes-'0_2', node_caps-no, deg_malig-'3', breast-right, breast_quad-left_up, irradiat-yes]).
	example(236, recurrence_events, [age-'50_59', menopause-premeno, tumor_size-'25_29', inv_nodes-'3_5', node_caps-yes, deg_malig-'3', breast-left, breast_quad-left_low, irradiat-yes]).
	example(237, recurrence_events, [age-'40_49', menopause-premeno, tumor_size-'25_29', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-right, breast_quad-left_low, irradiat-no]).
	example(238, recurrence_events, [age-'40_49', menopause-premeno, tumor_size-'25_29', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-right, breast_quad-left_low, irradiat-no]).
	example(239, recurrence_events, [age-'30_39', menopause-premeno, tumor_size-'35_39', inv_nodes-'0_2', node_caps-no, deg_malig-'3', breast-left, breast_quad-left_low, irradiat-no]).
	example(240, recurrence_events, [age-'40_49', menopause-premeno, tumor_size-'20_24', inv_nodes-'3_5', node_caps-yes, deg_malig-'2', breast-right, breast_quad-right_up, irradiat-yes]).
	example(241, recurrence_events, [age-'60_69', menopause-ge40, tumor_size-'20_24', inv_nodes-'3_5', node_caps-no, deg_malig-'2', breast-left, breast_quad-left_low, irradiat-yes]).
	example(242, recurrence_events, [age-'40_49', menopause-premeno, tumor_size-'15_19', inv_nodes-'15_17', node_caps-yes, deg_malig-'3', breast-left, breast_quad-left_low, irradiat-no]).
	example(243, recurrence_events, [age-'50_59', menopause-ge40, tumor_size-'25_29', inv_nodes-'6_8', node_caps-no, deg_malig-'3', breast-left, breast_quad-left_low, irradiat-yes]).
	example(244, recurrence_events, [age-'50_59', menopause-ge40, tumor_size-'20_24', inv_nodes-'3_5', node_caps-yes, deg_malig-'3', breast-right, breast_quad-right_up, irradiat-no]).
	example(245, recurrence_events, [age-'40_49', menopause-premeno, tumor_size-'30_34', inv_nodes-'12_14', node_caps-yes, deg_malig-'3', breast-left, breast_quad-left_up, irradiat-yes]).
	example(246, recurrence_events, [age-'30_39', menopause-premeno, tumor_size-'30_34', inv_nodes-'9_11', node_caps-no, deg_malig-'2', breast-right, breast_quad-left_up, irradiat-yes]).
	example(247, recurrence_events, [age-'30_39', menopause-premeno, tumor_size-'15_19', inv_nodes-'6_8', node_caps-yes, deg_malig-'3', breast-left, breast_quad-left_low, irradiat-yes]).
	example(248, recurrence_events, [age-'50_59', menopause-ge40, tumor_size-'30_34', inv_nodes-'9_11', node_caps-yes, deg_malig-'3', breast-left, breast_quad-right_low, irradiat-yes]).
	example(249, recurrence_events, [age-'60_69', menopause-ge40, tumor_size-'35_39', inv_nodes-'6_8', node_caps-yes, deg_malig-'3', breast-left, breast_quad-left_low, irradiat-no]).
	example(250, recurrence_events, [age-'30_39', menopause-premeno, tumor_size-'20_24', inv_nodes-'3_5', node_caps-yes, deg_malig-'2', breast-left, breast_quad-left_low, irradiat-no]).
	example(251, recurrence_events, [age-'40_49', menopause-premeno, tumor_size-'25_29', inv_nodes-'0_2', node_caps-no, deg_malig-'3', breast-left, breast_quad-left_up, irradiat-no]).
	example(252, recurrence_events, [age-'40_49', menopause-premeno, tumor_size-'50_54', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-right, breast_quad-left_low, irradiat-yes]).
	example(253, recurrence_events, [age-'30_39', menopause-premeno, tumor_size-'40_44', inv_nodes-'0_2', node_caps-no, deg_malig-'1', breast-left, breast_quad-left_up, irradiat-no]).
	example(254, recurrence_events, [age-'60_69', menopause-ge40, tumor_size-'50_54', inv_nodes-'0_2', node_caps-no, deg_malig-'3', breast-right, breast_quad-left_up, irradiat-no]).
	example(255, recurrence_events, [age-'40_49', menopause-premeno, tumor_size-'30_34', inv_nodes-'0_2', node_caps-yes, deg_malig-'3', breast-right, breast_quad-right_up, irradiat-no]).
	example(256, recurrence_events, [age-'40_49', menopause-premeno, tumor_size-'30_34', inv_nodes-'6_8', node_caps-yes, deg_malig-'3', breast-right, breast_quad-left_up, irradiat-no]).
	example(257, recurrence_events, [age-'40_49', menopause-premeno, tumor_size-'30_34', inv_nodes-'0_2', node_caps-no, deg_malig-'1', breast-left, breast_quad-left_low, irradiat-yes]).
	example(258, recurrence_events, [age-'40_49', menopause-premeno, tumor_size-'20_24', inv_nodes-'3_5', node_caps-yes, deg_malig-'2', breast-left, breast_quad-left_low, irradiat-yes]).
	example(259, recurrence_events, [age-'50_59', menopause-ge40, tumor_size-'30_34', inv_nodes-'6_8', node_caps-yes, deg_malig-'2', breast-left, breast_quad-right_low, irradiat-yes]).
	example(260, recurrence_events, [age-'50_59', menopause-ge40, tumor_size-'30_34', inv_nodes-'3_5', node_caps-no, deg_malig-'3', breast-right, breast_quad-left_up, irradiat-no]).
	example(261, recurrence_events, [age-'60_69', menopause-ge40, tumor_size-'25_29', inv_nodes-'3_5', node_caps-no, deg_malig-'2', breast-right, breast_quad-right_up, irradiat-no]).
	example(262, recurrence_events, [age-'40_49', menopause-ge40, tumor_size-'25_29', inv_nodes-'12_14', node_caps-yes, deg_malig-'3', breast-left, breast_quad-right_low, irradiat-yes]).
	example(263, recurrence_events, [age-'60_69', menopause-ge40, tumor_size-'25_29', inv_nodes-'0_2', node_caps-no, deg_malig-'3', breast-left, breast_quad-left_up, irradiat-no]).
	example(264, recurrence_events, [age-'50_59', menopause-lt40, tumor_size-'20_24', inv_nodes-'0_2', node_caps-_, deg_malig-'1', breast-left, breast_quad-left_up, irradiat-no]).
	example(265, recurrence_events, [age-'50_59', menopause-lt40, tumor_size-'20_24', inv_nodes-'0_2', node_caps-_, deg_malig-'1', breast-left, breast_quad-left_low, irradiat-no]).
	example(266, recurrence_events, [age-'30_39', menopause-premeno, tumor_size-'35_39', inv_nodes-'9_11', node_caps-yes, deg_malig-'3', breast-left, breast_quad-left_low, irradiat-no]).
	example(267, recurrence_events, [age-'40_49', menopause-premeno, tumor_size-'30_34', inv_nodes-'3_5', node_caps-yes, deg_malig-'2', breast-left, breast_quad-right_up, irradiat-no]).
	example(268, recurrence_events, [age-'60_69', menopause-ge40, tumor_size-'20_24', inv_nodes-'24_26', node_caps-yes, deg_malig-'3', breast-left, breast_quad-left_low, irradiat-yes]).
	example(269, recurrence_events, [age-'30_39', menopause-premeno, tumor_size-'35_39', inv_nodes-'0_2', node_caps-no, deg_malig-'3', breast-left, breast_quad-left_low, irradiat-no]).
	example(270, recurrence_events, [age-'40_49', menopause-premeno, tumor_size-'25_29', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-left, breast_quad-left_low, irradiat-yes]).
	example(271, recurrence_events, [age-'50_59', menopause-ge40, tumor_size-'30_34', inv_nodes-'6_8', node_caps-yes, deg_malig-'3', breast-left, breast_quad-right_low, irradiat-no]).
	example(272, recurrence_events, [age-'50_59', menopause-premeno, tumor_size-'25_29', inv_nodes-'0_2', node_caps-no, deg_malig-'3', breast-right, breast_quad-left_low, irradiat-yes]).
	example(273, recurrence_events, [age-'40_49', menopause-premeno, tumor_size-'15_19', inv_nodes-'0_2', node_caps-yes, deg_malig-'3', breast-right, breast_quad-left_up, irradiat-no]).
	example(274, recurrence_events, [age-'60_69', menopause-ge40, tumor_size-'30_34', inv_nodes-'0_2', node_caps-yes, deg_malig-'2', breast-right, breast_quad-right_up, irradiat-yes]).
	example(275, recurrence_events, [age-'60_69', menopause-ge40, tumor_size-'30_34', inv_nodes-'3_5', node_caps-yes, deg_malig-'2', breast-left, breast_quad-central, irradiat-yes]).
	example(276, recurrence_events, [age-'40_49', menopause-premeno, tumor_size-'25_29', inv_nodes-'9_11', node_caps-yes, deg_malig-'3', breast-right, breast_quad-left_up, irradiat-no]).
	example(277, recurrence_events, [age-'30_39', menopause-premeno, tumor_size-'25_29', inv_nodes-'6_8', node_caps-yes, deg_malig-'3', breast-left, breast_quad-right_low, irradiat-yes]).
	example(278, recurrence_events, [age-'60_69', menopause-ge40, tumor_size-'10_14', inv_nodes-'6_8', node_caps-yes, deg_malig-'3', breast-left, breast_quad-left_up, irradiat-yes]).
	example(279, recurrence_events, [age-'50_59', menopause-premeno, tumor_size-'35_39', inv_nodes-'15_17', node_caps-yes, deg_malig-'3', breast-right, breast_quad-right_up, irradiat-no]).
	example(280, recurrence_events, [age-'50_59', menopause-ge40, tumor_size-'40_44', inv_nodes-'6_8', node_caps-yes, deg_malig-'3', breast-left, breast_quad-left_low, irradiat-yes]).
	example(281, recurrence_events, [age-'50_59', menopause-ge40, tumor_size-'40_44', inv_nodes-'6_8', node_caps-yes, deg_malig-'3', breast-left, breast_quad-left_low, irradiat-yes]).
	example(282, recurrence_events, [age-'30_39', menopause-premeno, tumor_size-'30_34', inv_nodes-'0_2', node_caps-no, deg_malig-'2', breast-left, breast_quad-left_up, irradiat-no]).
	example(283, recurrence_events, [age-'30_39', menopause-premeno, tumor_size-'20_24', inv_nodes-'0_2', node_caps-no, deg_malig-'3', breast-left, breast_quad-left_up, irradiat-yes]).
	example(284, recurrence_events, [age-'60_69', menopause-ge40, tumor_size-'20_24', inv_nodes-'0_2', node_caps-no, deg_malig-'1', breast-right, breast_quad-left_up, irradiat-no]).
	example(285, recurrence_events, [age-'40_49', menopause-ge40, tumor_size-'30_34', inv_nodes-'3_5', node_caps-no, deg_malig-'3', breast-left, breast_quad-left_low, irradiat-no]).
	example(286, recurrence_events, [age-'50_59', menopause-ge40, tumor_size-'30_34', inv_nodes-'3_5', node_caps-no, deg_malig-'3', breast-left, breast_quad-left_low, irradiat-no]).

:- end_object.

