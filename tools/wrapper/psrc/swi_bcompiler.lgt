:- object(swi_bcompiler).

:- uses(builtins,[bbuiltin/3,bin_bbuiltin/1,cutp/1,n_barith/1,n_bbuiltin/1,n_binline/1]).

:- uses(swi,[errmes/2]).

:- uses(swi_fcompiler,[add_instr/5,symcat/3,to_string/2,ttyprint/1]).

:- use_module(lists,[member/2]).

:- public[cc/2,cc_bbuiltins/1,compile_bin/2].

:- end_object.

