swi_io(available).

swi_writeln(Alias,T):-writeln(Alias,T).

swi_open(FName,Mode, R,Options):-member(alias(Alias),Options),!,
  R=Alias,
  open(FName,Mode,_,Options). 
swi_open(FName,Mode, Alias,Options):-
  gensym(FName,Alias),
  open(FName,Mode,_,[alias(Alias)|Options]).
  
swi_close(Alias):-close(Alias).  
 
swi_current_output(Alias):-
  current_output(S),
  stream_property(S,alias(Alias)),!.
swi_current_output(Alias):-
  current_output(S),
  gensym('$swi_stream_alias',Alias),
  set_stream(S,alias(Alias)).
 
swi_current_input(Alias):-
  current_input(S),
  stream_property(S,alias(Alias)),!.
swi_current_input(Alias):-
  current_input(S),
  gensym('$swi_stream_alias',Alias),
  set_stream(S,alias(Alias)).

swi_stream_property(Alias, Property) :-
	% first get the stream form the alias
	once(stream_property(Stream, alias(Alias))),
	% second get the property
	stream_property(Stream, Property).


swioit1:-
  swi_open('boo.pl',write,R,[]),
  writeln(R),
  writeln(R,hello),
  close(R).


file2codes(F,Cs):-file2codes(F,Cs,[]).

file2codes(F0,Cs,Options):-
  find_file(F0,F),
  read_file_to_codes(F,Cs,Options).

codes_of(F,Cs):-file2codes(F,Cs,[]).

%% code_of(File,C): backtracks over char codes in File
code_of(F,C):-file2codes(F,Cs),member(C,Cs).

codes2file(Cs,F):-
  open(F,write,S),
  ( member(C,Cs),put(S,C),fail
  ; true
  ),
  close(S).
  
  
file2terms(F0,Ts):-
  find_file(F0,F),
  read_file_to_terms(F,Ts,[dotlists(true)]).

term_of(F,T):-
  file2terms(F,Ts),
  member(T,Ts).
  
codes_words(Cs,Ws):-atom_codes(A,Cs),tokenize_atom(A,Ws).
  
  
file2words(F,Ws):-file2codes(F,Cs),codes_words(Cs,Ws).

%% line_of(File,C): backtracks over lines in File
line_of(File,L):-line_codes_of(File,Cs),atom_codes(L,Cs).

%% line_codes_of(File,Cs): backtracks over lines seen as lists of char codes in File
line_codes_of(F0,Cs):-
  find_file(F0,File),
  open(File,read,S),
  repeat,
  ( read_line_to_codes(S,Cs),Cs\=end_of_file->true
  ; !,
    close(S),
    fail
  ).
  

