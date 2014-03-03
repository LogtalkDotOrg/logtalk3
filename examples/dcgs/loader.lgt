%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- if((current_logtalk_flag(prolog_dialect, swi), \+ current_prolog_flag(double_quotes, codes))).
	:- set_prolog_flag(double_quotes, codes).
:- endif.


:- initialization(
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
	])
). 

:- if(\+ current_logtalk_flag(prolog_dialect, lean)).

	% Lean Prolog doesn't support the 0'<char> used in these examples
	:- initialization(
		logtalk_load([
			calculator,
			macaddr,
			url,
			xml
		])
	). 

:- endif.
