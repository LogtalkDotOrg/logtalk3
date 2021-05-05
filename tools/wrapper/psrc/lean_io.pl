% new Lean IO, to be mapped to SWI
% one at at time, move functionality from xio.pl here and validate
% ideally in a separate branch as it is very high impact

iotest(ok). % just tests this file is integrated at all levels


% stream selection and control

current_input(Stream) :- swi_current_input(Stream).
current_output(Stream) :- swi_current_output(Stream).

set_input(Stream) :- swi_do(set_input(Stream)).
set_output(Stream) :- swi_do(set_output(Stream)).

open(File, Mode, Stream) :- swi_open(File, Mode, Stream, []).
open(File, Mode, Stream, Options) :- swi_open(File, Mode, Stream, Options).

close(Stream) :- swi_do(close(Stream)).
close(Stream, Options) :- swi_do(close(Stream, Options)).

flush_output :- swi_do(flush_output).
flush_output(Stream) :- swi_do(flush_output(Stream)).

stream_property(Stream, Property) :- swi_stream_property(Stream, Property).
at_end_of_stream :- swi_do(at_end_of_stream).
at_end_of_stream(Stream) :- swi_do(at_end_of_stream(Stream)).

set_stream_position(Stream, Position) :- swi_do(set_stream_position(Stream, Position)).


% output predicates

write(Term) :- swi_do(write(Term)).
write(Stream, Term) :- swi_do(write(Stream, Term)).

writeq(Term) :- swi_do(writeq(Term)).
writeq(Stream, Term) :- swi_do(writeq(Stream, Term)).

write_canonical(Term) :- swi_do(write_canonical(Term)).
write_canonical(Stream, Term) :- swi_do(write_canonical(Stream, Term)).

write_term(Stream, Term) :- swi_do(write_term(Stream, Term)).
write_term(Stream, Term, Options) :- swi_do(write_term(Stream, Term, Options)).

put_byte(Byte) :- swi_do(put_byte(Byte)).
put_byte(Stream, Byte) :- swi_do(put_byte(Stream, Byte)).

put_char(Char) :- swi_do(put_char(Char)).
put_char(Stream, Char) :- swi_do(put_char(Stream, Char)).

put_code(Code) :- swi_do(put_code(Code)).
put_code(Stream, Code) :- swi_do(put_code(Stream, Code)).

nl :- swi_do(nl).
nl(Stream) :- swi_do(nl(Stream)).


% input predicates

read(Term) :- swi_once(read_term(Term, [dotlists(true)])).
read(Stream, Term) :- swi_once(read_term(Stream, Term, [dotlists(true)])).

% ensured same as SWI
read_term(Term,Options) :- swi_once(read_term(Term, [dotlists(true)|Options])).
read_term(Stream, Term, Options) :- swi_once(read_term(Stream, Term, [dotlists(true)| Options])).

get_byte(Byte) :- swi_once(get_byte(Byte)).
get_byte(Stream, Byte) :- swi_once(get_byte(Stream, Byte)).

get_char(Char) :- swi_once(get_char(Char)).
get_char(Stream, Char) :- swi_do(get_char(Stream, Char)).

get_code(Code) :- swi_once(get_code(Code)).
get_code(Stream, Code) :- swi_once(get_code(Stream, Code)).




% non-standard but useful

%% write_codes(Cs): write each char represented by codes in Cs to stream
write_codes(Cs):-current_output(S),write_codes(S,Cs).

write_codes(_,[]).
write_codes(S,[C|Cs]):-put_code(S,C),write_codes(S,Cs).

tab(N) :- swi_do(tab(N)).
tab(Stream, N) :- swi_do(tab(Stream, N)).

writeln(Term) :- swi_do(writeln(Term)).
writeln(Stream, Term) :- swi_do(writeln(Stream, Term)).

read_term_from_atom(Stream, Term, Variables) :-
	swi_once(read_term_from_atom(Stream, Term, [variables(Variables),dotlists(true)])).


% predicates that should be deprecated (write warning message?)

println(Term) :- swi_do(writeln(Term)).
println(Stream, Term) :- swi_do(writeln(Stream, Term)).

put(Stream, Code) :- put_code(Stream,Code).
put(Code) :- put_code(Code).

get(Code) :- get_code(Code).

get0(Code) :- get_code(Code).
get0(Stream, Code) :- get_code(Stream, Code).





% old definitions

telling(Alias):-swi_current_output(Alias).

%% tell(File), told, telling(File) : direct output to given file (deprecated)
tell(File) :-
	swi_do(tell(File)).

told :-
	swi_do(told).

term_of(F,T):-
  file2terms(F,Ts),
  member(T,Ts).

file2terms(F,Ts):-swi_once(file2terms(F,Ts)).

seing(Alias):-swi_current_input(Alias).

%% see(File), seen, seeing(File) : direct input to given file (deprecated)
see(File) :-
	swi_do(see(File)).

seen :-
	swi_do(seen).

ttyprint(X):-current_output(S),println(S,X).
ttynl:-current_output(S),nl(S).

%% println(T): prints term T + nl to cirrent output
%println(X):-current_output(I),println(I,X).

%xpp(X):-current_output(I),atomic_writeln(I,X).

%println(Stream,X):-writeln(Stream,X).
%println(Stream,X):-sync_println(Stream,X).


%writeln(X):-current_output(I),writeln(I,X).
%       
%writeln(Stream,X):-
%  write(Stream,X),
%  nl(Stream).
%  
%write(T):-
%  current_output(I),
%  write(I,T).
% 
%%% write(Stream,Term): writes term to stream
%write(I,T):-
%  swi_call(with_output_to(atom(S),write_term(T,[]))),
%  fwrite(I,S).


fwrite(T):-
  current_output(I),
  fwrite(I,T).
  
fwrite(I,T):-write(I,T).

fwriteln(X):-current_output(I),fwriteln(I,X).

fwriteln(I,T):-fwrite(I,T),fnl(I).

fnl(I):-nl(I).

%% nl(Stream): writes newline to stream    
%nl(I):-fnl(I).
%nl:-current_output(I),nl(I).

%tab(N):-current_output(I),tab(I,N).

%% tab(Stream,N): writes N spaces to Stream 
%tab(I,N):-between(1,N,_),fwrite(I,' '),fail;true.

%% put_code(Stream,C): write character of given code to stream
%put_code(I,C):-tell_interactor(I,'$cmd'(4,C),_),ask_interactor(I,_). % PUT_CODE
%put_code(C):-current_output(I),put_code(I,C).

write_quoted(X):-current_output(S),write_quoted(S,X).

% limited to atoms
write_quoted(S,X):-swi_do(write_quoted(S,X)).

pp(C):-portray_clause(C).
  
% pretty print
portray_clause(C):-swi_do(portray_clause(C)).


portray_clause(S,C):-swi_do(portray_clause(S,C)).


top_writeq_atom(X):-writeq(X).

%% writeq(T): writes out T while quoting objects in int when needed
%writeq(T):-current_output(O),writeq(O,T).

%writeq(F,T):-object_to_quoted_string(T,S),fwrite(F,S).


%% write_canonical(Stream,Term): writes a term to a stream such that it can be read back
%write_canonical(O,T):-to_rstring(T,S),fwrite(O,S).

%% write_canonical(Term): writes tu current output a term such that it can be read back
%write_canonical(T):-current_output(O),write_canonical(O,T).

%% display(Term): displays a term in a canonical form
display(X):-write_canonical(X).

%%%%%%%%%%%%%%%%%%%% NOT WORKING YET OR NOT WORKING AS USUAL %%%%%%%%%%%%%%%%%

srtest(T,Ws):-current_input(I),swi_read_alt('?--',I,T,Ws).

swi_read_alt(Prompt,I,T,NVs):-
  swi_once(prompt1(Prompt)),
  read_term(I,R,[variable_names(NVs),dotlists(true)]),
  (R==end_of_file->halt;T=R).


stdio(I):-new_interactor('vm.extensions.ConsoleIO','?- ',I). % with jline line editing




%% current_input(Stream) : returns current input stream
lean_current_input(Stream):-get_val(current_input,S),!,Stream=S.
lean_current_input(Stream):-
  %traceln('no current input!!!'),
  stdio(Stream).

%% set_input(Stream) : sets current input stream
lean_set_input(Stream):-interactor(Stream),!,set_val(current_input,Stream).
lean_set_input(Stream):-errmes(bad_arguments,set_input(Stream)).

%% current_output(Stream): returns current output stream
lean_current_output(Stream):-get_val(current_output,S),!,Stream=S.
lean_current_output(Stream):-
  %traceln('no current output!!!'),
  stdio(Stream).

%% set_output(Stream): sets current output stream
lean_set_output(Stream):-interactor(Stream),!,set_val(current_output,Stream).
lean_set_output(Stream):-errmes(bad_arguments,set_output(Stream)).



%% line_of(File,C): backtracks over lines in File
line_of(File,L):-line_codes_of(File,Cs),atom_codes(L,Cs).



%% line_codes_of(File,Cs): backtracks over lines seen as lists of char codes in File
line_codes_of(File,Cs):-swi_call(line_codes_of(File,Cs)).

codes_words(Cs,Ws):-swi_once(codes_words(Cs,Ws)).
