%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  SPDX-FileCopyrightText: 1998-2024 Paulo Moura <pmoura@logtalk.org>
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


:- object(local_1_d_registry,
	implements(registry_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2024-10-13,
		comment is 'A local registry for testing.'
	]).

	name(local_1_d).

	description('A local registry for testing').

	home('file:///home/johndoe/local_1_d').

	clone('file:///home/johndoe/local_1_d.git').

	archive('file:///home/johndoe/local_1_d/master.zip').

	note(update, 'Check for deprecated packs.').

:- end_object.
