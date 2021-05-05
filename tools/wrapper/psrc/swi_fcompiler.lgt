:- set_prolog_flag(double_quotes,codes).

:- object(swi_fcompiler).

:- uses(user,[atomic_list_concat/2,delete_file/1,exists_file/1,nb_delete/1,nb_getval/2,nb_setval/2,put/1,tell/1,telling/1,told/0,writeln/1]).

:- uses(swi_bcompiler,[cc/2,cc_bbuiltins/1,compile_bin/2]).

:- uses(swi_db,[db_dynamic/2,db_is_dynamic/2]).

:- use_module(lists,[append/3,member/2,reverse/2]).

:- public[add_instr/5,compile_to_wam/1,symcat/3,to_string/2,ttyprint/1].

:- dynamic'$run'/1.

:- end_object.

