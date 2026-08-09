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


:- initialization((
	logtalk_load(types(loader)),
	logtalk_load(format(loader)),
	logtalk_load(linear_algebra(loader)),
	logtalk_load(options(loader)),
	logtalk_load(regression_protocols(loader)),
	logtalk_load([
		regression_protocols('test_datasets/collinear_line'),
		regression_protocols('test_datasets/duplicate_attribute_declaration'),
		regression_protocols('test_datasets/duplicate_attribute_example'),
		regression_protocols('test_datasets/grouped_categorical_signal'),
		regression_protocols('test_datasets/intercept_only'),
		regression_protocols('test_datasets/invalid_target'),
		regression_protocols('test_datasets/mixed_signal'),
		regression_protocols('test_datasets/no_attribute_intercept'),
		regression_protocols('test_datasets/plane'),
		regression_protocols('test_datasets/simple_line'),
		regression_protocols('test_datasets/sparse_mixed_signal'),
		regression_protocols('test_datasets/sparse_signal'),
		regression_protocols('test_datasets/step_signal'),
		regression_protocols('test_datasets/undeclared_attribute_example')
	], [source_data(on), debug(on)]),
	logtalk_load(svr_regression, [debug(on), source_data(on)]),
	logtalk_load(lgtunit(loader)),
	logtalk_load(tests, [hook(lgtunit)]),
	tests::run
)).
