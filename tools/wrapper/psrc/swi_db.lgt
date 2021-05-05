:- object(swi_db).

:- uses(user,[delete_file/1,(dynamic)/1,file_name_extension/3,tell/1,told/0]).

:- uses(swi_fcompiler,[compile_to_wam/1]).

:- use_module(error,[must_be/2]).

:- use_module(prolog_listing,[listing/1]).

:- public[db_dynamic/2,db_is_dynamic/2].

:- end_object.

