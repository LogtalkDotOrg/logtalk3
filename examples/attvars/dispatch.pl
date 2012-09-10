%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- module(dispatch, []).

:- multifile(attr_unify_hook/3).

attr_unify_hook(Obj-Value, Var) :-
	attr_unify_hook(Obj, Value, Var).

:- multifile(attribute_goals//2).

attribute_goals(Var) -->
	{get_attr(Var, dispatch, Obj-_))},
	attribute_goals(Obj, Var).
