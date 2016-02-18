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


% integration code for XPCE 6.6.21 and later versions supporting Logtalk 
% message sending goals as call-backs goals
%
% with this file you can use the syntax:
% 
%    logtalk(Object, MessageFunctor, MessageArg1, MessageArg2, ...)
%
% as an alternative to XPCE's message(...) call-backs


:- use_module(library(pce)).


:- pce_begin_class(logtalk, message).

	initialise(Msg, Obj:prolog, Functor:prolog, Args:unchecked ...) :->
		Pred =.. [Functor| Args],
		SuperMsg =.. [initialise, @prolog, call, prolog(Obj::Pred)],
		send_super(Msg, SuperMsg).

:- pce_end_class.
