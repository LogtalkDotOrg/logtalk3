%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <https://logtalk.org/>  
%  Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>
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


:- dynamic([
	s1/0, s2/0, s3/0, s4/0, s5/0, s6/0, s7/0, s8/0
]).

:- dynamic([
	f01/0, f02/0, f03/0, f04/0, f05/0, f06/0, f07/0, f08/0, f09/0, f10/0,
	f11/0, f12/0, f13/0, f14/0, f15/0, f16/0, f17/0, f18/0, f19/0, f20/0,
	f21/0, f22/0, f23/0, f24/0, f25/0, f26/0, f27/0, f28/0, f29/0, f30/0, f31/0
]).


:- if(true).
	s1.
:- endif.


:- if(fail).
	f1.
:- endif.


:- if(true).
	s2.
:- elif(fail).
	f2.
:- elif(fail).
	f3.
:- else.
	f4.
:- endif.


:- if(fail).
	f5.
:- elif(fail).
	f6.
:- elif(fail).
	f7.
:- else.
	s3.
:- endif.


:- if(true).
	s4.
:- elif(fail).
	f8.
	:- if(true).
		f9.
	:- elif(true).
		f10.
	:- else.
		f11.
	:- endif.
:- elif(fail).
	f12.
:- else.
	f14.
:- endif.


:- if(true).
	s5.
	:- if(true).
		s6.
	:- elif(true).
		f15.
	:- else.
		f16.
	:- endif.
:- elif(fail).
	f17.
:- elif(fail).
	f18.
:- else.
	f19.
:- endif.


:- if(fail).
	f20.
:- elif(fail).
	f21.
	:- if(true).
		f22.
	:- elif(true).
		f23.
	:- else.
		f24.
	:- endif.
:- elif(fail).
	f25.
:- else.
	s7.
:- endif.


:- if(fail).
	f26.
:- elif(true).
	s8.
:- else.
	f27.
	:- if(fail).
		f28.
	:- elif(fail).
		f29.
	:- elif(fail).
		f30.
	:- endif.
	f31.
:- endif.


:- initialization((
	s1, s2, s3, s4, s5, s6, s7, s8,
	write('sX tests passed'), nl
)).


:- initialization((
	\+ f01, \+ f02, \+ f03, \+ f04, \+ f05, \+ f06, \+ f07, \+ f08, \+ f09, \+ f10,
	\+ f11, \+ f12, \+ f13, \+ f14, \+ f15, \+ f16, \+ f17, \+ f18, \+ f19, \+ f20,
	\+ f21, \+ f22, \+ f23, \+ f24, \+ f25, \+ f26, \+ f27, \+ f28, \+ f29, \+ f30, \+ f31,
	write('fXX tests passed'), nl
)).
