
:- if(current_logtalk_flag(prolog_dialect, swi)).

	:- if(current_prolog_flag(iso, true)).
		:- set_prolog_flag(iso, false).		% workaround CHR ISO compliance issues
		:- ensure_loaded(library(chr)).
		:- set_prolog_flag(iso, true).
	:- else.
		:- ensure_loaded(library(chr)).
	:- endif.

:- elif(current_logtalk_flag(prolog_dialect, yap)).

	:- ensure_loaded(library(chr)).
	:- op(200, fy, ?).						% workaround a YAP-CHR operator bug

:- elif(current_logtalk_flag(prolog_dialect, sicstus)).

	:- ensure_loaded(library(chr)).

:- elif(current_logtalk_flag(prolog_dialect, qp)).

	:- chr_init.

:- endif.


:- if(	(current_logtalk_flag(prolog_dialect, Dialect),
		(Dialect == sicstus; Dialect == swi; Dialect == yap))).

	:- initialization((
		logtalk_load(chr_hook, [source_data(off), reload(skip)]),		% allow for static binding
		% only a single object or category containing CHR code can be loaded at a time
		logtalk_load(dom, [source_data(off), hook(chr_hook)])
%		logtalk_load(family, [source_data(off), hook(chr_hook)]),
%		logtalk_load(fib, [source_data(off), hook(chr_hook)]),
%		logtalk_load(fibonacci, [source_data(off), hook(chr_hook)]),
%		logtalk_load(gcd, [source_data(off), hook(chr_hook)]),
%		logtalk_load(leq, [source_data(off), hook(chr_hook)]),
%		logtalk_load(primes, [source_data(off), hook(chr_hook)])
	)).

:- elif(current_logtalk_flag(prolog_dialect, qp)).

	:- initialization((
		logtalk_load(chr_hook, [reload(skip)]),		% allow for static binding
		logtalk_load(dom, [hook(chr_hook)]),
		logtalk_load(family, [hook(chr_hook)]),
		logtalk_load(fib, [hook(chr_hook)]),
		logtalk_load(fibonacci, [hook(chr_hook)]),
		logtalk_load(gcd, [hook(chr_hook)]),
		logtalk_load(leq, [hook(chr_hook)]),
		logtalk_load(primes, [hook(chr_hook)]),
		logtalk_load(combining, [hook(chr_hook)])	% requires Qu-Prolog
	)).

:- else.

	:- initialization((write('WARNING: example not supported on this back-end Prolog compiler!'), nl)).

:- endif.
