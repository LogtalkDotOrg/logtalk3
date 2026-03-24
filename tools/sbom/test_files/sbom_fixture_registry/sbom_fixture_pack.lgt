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


:- object(sbom_fixture_pack,
	implements(pack_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-03-24,
		comment is 'A local pack for sbom tool tests.'
	]).

	name(sbom_fixture_pack).

	description('A local pack for sbom tool tests').

	license('Apache-2.0').

	home('file://sbom_fixture_pack').

	version(
		1:0:0,
		stable,
		'file://sbom_fixture_pack',
		sha256-'0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef',
		[logtalk @>= 3:0:0, sbom_fixture_registry::sbom_fixture_no_checksum_pack == 1:0:0],
		all
	).

:- end_object.
