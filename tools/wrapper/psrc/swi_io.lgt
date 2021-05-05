:- object(swi_io).

:- uses(user,[put/2,set_stream/2,writeln/1,writeln/2]).

:- uses(swi_compat,[find_file/2]).

:- use_module(gensym,[gensym/2]).

:- use_module(lists,[member/2]).

:- use_module(porter_stem,[tokenize_atom/2]).

:- use_module(read_util,[read_file_to_codes/3,read_file_to_terms/3,read_line_to_codes/2]).

:- end_object.

