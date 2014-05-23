%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% the contents of this file should be added to any object or category that
% makes calls to XPCE main predicates by using the Logtalk `include/1`
% directive


% override the XPCE meta-predicate templates to make them usable from within
% Logtalk objects and categories
:- meta_predicate(pce_principal:new(*,*)).
:- meta_predicate(pce_principal:send(*,*)).
:- meta_predicate(pce_principal:get(*,*,*)).

% allow using the XPCE predicates with implicit qualification
:- use_module(pce_principal, [
	new/2, free/1, send/2, get/3
]).
