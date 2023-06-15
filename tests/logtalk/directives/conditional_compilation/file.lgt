%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2023 Paulo Moura <pmoura@logtalk.org>
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
	:- foo.
	f1.
:- endif.


:- if(true).
	s2.
:- elif(fail).
	:- foo.
	f2.
:- elif(fail).
	:- bar.
	f3.
:- else.
	:- baz.
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
