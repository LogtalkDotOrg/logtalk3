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


:- protocol(registry_protocol).

	:- info([
		version is 0:10:0,
		author is 'Paulo Moura',
		date is 2021-10-28,
		comment is 'Registry specification protocol.'
	]).

	:- public(name/1).
	:- mode(name(?atom), zero_or_one).
	:- info(name/1, [
		comment is 'Registry name. Preferably a valid unquoted atom.',
		argnames is ['Name']
	]).

	:- public(description/1).
	:- mode(description(?atom), zero_or_one).
	:- info(description/1, [
		comment is 'Registry one line description.',
		argnames is ['Description']
	]).

	:- public(home/1).
	:- mode(home(?atom), zero_or_one).
	:- info(home/1, [
		comment is 'Registry home HTTPS or file URL.',
		argnames is ['Home']
	]).

	:- public(clone/1).
	:- mode(clone(?atom), zero_or_one).
	:- info(clone/1, [
		comment is 'Registry git clone HTTPS URL (must end with the ``.git`` extension). Git repos should have the same name as the registry.',
		argnames is ['URL']
	]).

	:- public(archive/1).
	:- mode(archive(?atom), zero_or_one).
	:- info(archive/1, [
		comment is 'Registry archive download HTTPS URL.',
		argnames is ['URL']
	]).

:- end_protocol.
