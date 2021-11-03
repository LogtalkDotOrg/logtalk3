%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
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


:- object(local_1_registry,
	implements(registry_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2021-11-03,
		comment is 'A local registry for testing.'
	]).

	name(local_1).

	description('A local registry for testing').

	home('file:///home/jdoe/local_1').

	clone('file:///home/jdoe/local_1.git').

	archive('file:///home/jdoe/local_1/master.zip').

:- end_object.
