%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- initialization((
	current_prolog_flag(double_quotes, Value),
	set_prolog_flag(double_quotes, codes),
	logtalk_load([
		parsep,
		enigma,
		parsetree,
		sentences,
		tokenizer,
		morse,
		shell,
		walker,
		bom,
		faa,
		bypass,
		dcgtest,
		metas
	]),
	set_prolog_flag(double_quotes, Value)
)). 

:- if(\+ current_logtalk_flag(prolog_dialect, lean)).

	% Lean Prolog doesn't support the 0'<char> used in these examples
	:- initialization((
		current_prolog_flag(double_quotes, Value),
		set_prolog_flag(double_quotes, codes),
		logtalk_load([
			calculator,
			macaddr,
			url,
			xml
		]),
		set_prolog_flag(double_quotes, Value)
	)).

:- endif.
