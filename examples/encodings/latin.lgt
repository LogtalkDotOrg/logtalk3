:- encoding('ISO-8859-1').	% this directive, when present, must be the first
							% term, in the first line, of a source file

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 1998-2016 Paulo Moura <pmoura@logtalk.org>
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


:- object(latin).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2008/01/10,
		comment is 'Simple test of the encoding/1 directive.'
	]).

	:- public(name/1).
	:- mode(name(?atom), zero_or_more).
	:- info(name/1, [
		comment is 'Table of person names.',
		argnames is ['Name']
	]).

	name('António Simões').
	name('Cátia Conceição').
	name('João Raínho').
	name('Luís Araújo').

:- end_object.
