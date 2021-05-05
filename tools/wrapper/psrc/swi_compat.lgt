:- object(swi_compat).

:- uses(user,[absolute_file_name/2,atomic_list_concat/2,between/3,exists_file/1,length/2,name/2,nb_current/2,nb_delete/1,nb_getval/2,nb_setarg/3,nb_setval/2,writeln/1]).

:- uses(swi,[errmes/2]).

:- use_module(apply,[maplist/3]).

:- use_module(jpl,[jpl_array_to_list/2,jpl_call/4,jpl_classname_to_class/2,jpl_classname_to_type/2,jpl_get/3,jpl_list_to_array/2,jpl_new/3,jpl_object_to_class/2,jpl_primitive_type/1,jpl_set/3]).

:- use_module(lists,[append/2,append/3,member/2,nth0/3]).

:- use_module(pairs,[group_pairs_by_key/2]).

:- use_module(read_util,[read_file_to_codes/3]).

:- public[find_file/2].

:- end_object.

