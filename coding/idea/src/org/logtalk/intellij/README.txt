The Logtalk keywords present in the grammar where obtained by means of the following queries:


?- findall(N, (help::built_in_directive(N, A, _, _), A > 0), L), atomic_list_concat(L, '" | "', R).
L = [encoding, initialization, op, set_logtalk_flag, if, elif, calls, category, category|...],
R = 'encoding" | "initialization" | "op" | "set_logtalk_flag" | "if" | "elif" | "calls" | "category" | "category" | "category" | "include" | "info" | "initialization" | "object" | "object" | "object" | "object" | "object" | "protocol" | "protocol" | "uses" | "alias" | "coinductive" | "discontiguous" | "dynamic" | "info" | "meta_predicate" | "meta_non_terminal" | "mode" | "multifile" | "op" | "private" | "protected" | "public" | "synchronized" | "uses" | "use_module'.

?- findall(N, (help::built_in_directive(N, A, _, _), A =:= 0), L), atomic_list_concat(L, '" | "', R).
L = [else, endif, dynamic, end_category, end_object, end_protocol, threaded],
R = 'else" | "endif" | "dynamic" | "end_category" | "end_object" | "end_protocol" | "threaded'.

?- findall(N, (help::built_in_method(N, A, _, _), A > 0), L), atomic_list_concat(L, '" | "', R).
L = [parameter, self, sender, this, current_op, current_predicate, predicate_property, abolish, asserta|...],
R = 'parameter" | "self" | "sender" | "this" | "current_op" | "current_predicate" | "predicate_property" | "abolish" | "asserta" | "assertz" | "clause" | "retract" | "retractall" | "call" | "call" | "call" | "call" | "call" | "call" | "call" | "call" | "once" | "\\+" | "catch" | "throw" | "bagof" | "findall" | "findall" | "forall" | "setof" | "before" | "after" | "forward" | "phrase" | "phrase" | "expand_term" | "term_expansion" | "expand_goal" | "goal_expansion" | "coinductive_success_hook" | "coinductive_success_hook" | "ask_question" | "message_hook" | "message_prefix_stream" | "print_message" | "print_message_tokens" | "print_message_token" | "question_hook" | "question_prompt_stream'.

?- findall(N, (help::built_in_method(N, A, _, _), A =:= 0), L), atomic_list_concat(L, '" | "', R).
L = [],
R = ''.

?- findall(N, (help::built_in_non_terminal(N, A, _, _), A > 0), L), atomic_list_concat(L, '" | "', R).
L = [call, call, call, call, call, call, phrase, message_tokens],
R = 'call" | "call" | "call" | "call" | "call" | "call" | "phrase" | "message_tokens'.

?- findall(N, (help::built_in_non_terminal(N, A, _, _), A =:= 0), L), atomic_list_concat(L, '" | "', R).
L = [eos],
R = eos.

?- findall(N, (help::built_in_predicate(N, A, _, _), A > 0), L), atomic_list_concat(L, '" | "', R).
L = [current_category, current_object, current_protocol, category_property, object_property, protocol_property, create_category, create_object, create_protocol|...],
R = 'current_category" | "current_object" | "current_protocol" | "category_property" | "object_property" | "protocol_property" | "create_category" | "create_object" | "create_protocol" | "abolish_category" | "abolish_object" | "abolish_protocol" | "extends_object" | "extends_object" | "extends_protocol" | "extends_protocol" | "extends_category" | "extends_category" | "implements_protocol" | "implements_protocol" | "imports_category" | "imports_category" | "instantiates_class" | "instantiates_class" | "specializes_class" | "specializes_class" | "complements_object" | "abolish_events" | "current_event" | "define_events" | "threaded" | "threaded_call" | "threaded_call" | "threaded_once" | "threaded_once" | "threaded_ignore" | "threaded_exit" | "threaded_exit" | "threaded_peek" | "threaded_peek" | "threaded_wait" | "threaded_notify" | "threaded_engine" | "threaded_engine_create" | "threaded_engine_destroy" | "threaded_engine_self" | "threaded_engine_next" | "threaded_engine_next_reified" | "threaded_engine_yield" | "threaded_engine_post" | "threaded_engine_fetch" | "logtalk_compile" | "logtalk_compile" | "logtalk_load" | "logtalk_load" | "logtalk_make" | "logtalk_library_path" | "logtalk_load_context" | "current_logtalk_flag" | "set_logtalk_flag" | "create_logtalk_flag'.

?- findall(N, (help::built_in_predicate(N, A, _, _), A =:= 0), L), atomic_list_concat(L, '" | "', R).
L = [logtalk_make],
R = logtalk_make.

?- findall(N, (help::control(N, A, _, _), A > 0), L), atomic_list_concat(L, '" | "', R).
L = [::, '[]', ::, ^^, {}, <<],
R = '::" | "[]" | "::" | "^^" | "{}" | "<<'.

?- findall(N, (help::control(N, A, _, _), A =:= 0), L), atomic_list_concat(L, '" | "', R).
L = [],
R = ''.