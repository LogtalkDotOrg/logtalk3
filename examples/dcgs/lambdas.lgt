%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <https://logtalk.org/>  
%  Copyright 1998-2019 Paulo Moura <pmoura@logtalk.org>
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


:- object(lambdas).

	:- info([
		version is 0.1,
		author is 'Paulo Moura',
		date is 2018/08/16,
		comment is 'Example using lambda expressions in grammar rules.',
		source is 'Adapted from example posted by Kuniaki Mukai in the SWI-Prolog mailing list.'
	]).

	% silent variables with dual role in lambda expresions warnings
	:- set_logtalk_flag(lambda_variables, silent).

	:- public(aa//1).

	aa([]) --> [].
	aa([X,X|Xs]) --> {X}/[[X|Y],Y]>>true, aa(Xs).

	% note that the definition of the aa//1 non-terminal is just to
	% *exemplify* the use of lambda expressions in grammar rules as
	% the same functionality could be simply implemented as follows:

	:- public(bb//1).

	bb([]) --> [].
	bb([X,X|Xs]) --> [X], bb(Xs).

:- end_object.
