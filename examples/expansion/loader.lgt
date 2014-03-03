%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% some backend Prolog compilers such as SWI-Prolog define a 'public'
% operator, which can cause syntax errors when loading this example
:- if(current_op(_, fx, (public))).
	:- op(0, fx, (public)).
:- endif.


:- initialization((
	logtalk_load(library(basic_types_loader)),
	logtalk_load([expansion, hooks]),
	logtalk_load(raw, [hook(hh)])
)).
