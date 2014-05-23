%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
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
