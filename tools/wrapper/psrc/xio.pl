% xio.pl

%% working_directory(Old,New): binds Old to current directory and changes it to New    
working_directory(Old,New):-
 get_working_directory(Old), % moved to xbuiltins
 set_working_directory(New),
 swi_call(working_directory(_,New)).
 
%% chdir(Dir), cd(Dir): changes (globally) current directory to Dir
chdir(Dir):-working_directory(_,Dir).
cd(Dir):-working_directory(_,Dir).

%% open(File,Mode,Stream): opens a file in given mode, returning a stream

% moved to lean_io.pl

lean_open(File,Mode,Stream):-openable(File),nonvar(Mode),Mode=read,!,
  new_interactor('vm.extensions.LineStreamInput',File,Stream).
lean_open(File,Mode,Stream):-openable(File),nonvar(Mode),Mode=write,!,
  new_interactor('vm.extensions.LineStreamOutput',File,Stream).  
lean_open(File,Mode,Stream):-openable(File),nonvar(Mode),Mode=append,!,
  atom_concat('++',File,AppFile),
  new_interactor('vm.extensions.LineStreamOutput',AppFile,Stream).    
lean_open(File,Mode,_Stream):-
  errmes('bad arguments',open(File,Mode)).  

openable(File):-atom(File),!.
openable(Stream):-interactor(Stream).
 
%%  close(Stream): closes stream 

% moved to lean_io.pl
lean_close(Stream):-openable(Stream),!,stop_interactor(Stream).
lean_close(Stream):-errmes('bad arguments',close(Stream)).  

%% set_alias(Stream,Alias): binds Stream and Alias together
%% get_alias(Stream,Alias): when at least one is nonvar it returns the other
%% remove_alias(StreamOrAlias) -- removes the alias if the stream or the alias are given
  
set_alias(Stream,Name):-
  call_java_class_method('vm.extensions.IOInteractor',set_alias(Stream,Name),_).
  
remove_alias(StreamOrName):-
  call_java_class_method('vm.extensions.IOInteractor',remove_alias(StreamOrName),_).
  
get_alias(Stream,Alias):-interactor(Stream),!,
  call_java_class_method('vm.extensions.IOInteractor',stream2alias(Stream),the(Alias)).
get_alias(Stream,Alias):-nonvar(Alias),!,
  call_java_class_method('vm.extensions.IOInteractor',alias2stream(Alias),the(Stream)).
get_alias(Stream,Alias):-errmes('bad arguments',get_alias(Stream,Alias)).    

get_initiator(Stream,FName):-invoke_java_method(Stream,getInitiator,FName).

get_reader(Stream,Source):-interactor(Stream),!,
  jcall('vm.extensions.LineStreamInput',getReader(Stream),Source).
get_reader(Stream,Source):-errmes(bad_arguments,get_reader(Stream,Source)).

get_tokenizer(Stream,Tokenizer):-invoke_java_method(Stream,getTokenizer,Tokenizer).

statistics(runtime,[T,0]):-cputime(X),T is X*1000.
statistics(symbols,X):-symbols(X).
statistics(engines,X):-engines(X).
statistics(heapsizes,X):-heapsizes(X).
statistics(choice_point_stack_sizes,X):-stacksizes(X).
statistics(trailsizes,X):-trailsizes(X).
statistics(flags,X):-flags(X).
statistics(max_memory_MBytes,X):-max_memory(X).
statistics(used_memory_MBytes,X):-used_memory(X).
statistics(available_memory_MBytes,X):-available_memory(X).
statistics(thread_count,X):-thread_count(X).
statistics(max_cores,X):-max_cores(X).
%statistics(db_size,X):-db_size(X).
statistics(xprofile,X):-xprofile(X).


read_words(R):-lean_current_input(I),read_words(I,R).

%read_words(R):-current_input(I),swi_once(readln(I,R)).

%% read_words(Stream,Words): reads words from stream
read_words(I,R):-
  tell_interactor(I,'$cmd'(6),_), % READWORDS
  ask_interactor(I,R). % R= list of words on a line

word_reader(File,Stream):-
  new_interactor('vm.extensions.WordStreamInput',File,Stream).

word_of_string(S,W):-
  word_of0('$str'(S),W).

/*
%% words_of(File,Words): converts a File to a list of words
words_of(File,Ws):-findall(W,word_of(File,W),Ws).

%% word_of(File,W): backtracks over words found in in a file, one at time
word_of(File,W):-
   find_file(File,F),
   word_of0(F,W).
   
word_of0(F,W):-
  word_reader(F,R),
  element_of(R,W).
 
 
%% codes_words(Cs,Ws): parses/unparses list of codes to/from words    

codes_words(Cs,Ws):-var(Ws),!,
  atom_codes(S,Cs),
  findall(W,word_of_string(S,W),Ws).
codes_words(Cs,Ws):-
  findall(C,words_code(Ws,C),Cs).
*/



%% readln(T): reads line from current input stream
readln(T):-
  lean_current_input(I),
  readln(I,T).
  
readln(I,T):-
  % traceln('INTER'(I)),
  readln_codes(I,R),
  ( R=the(Cs)->atom_codes(T,Cs)
  ; T='end_of_file.'
  ).


readln_codes(R):-
  lean_current_input(I),
  readln_codes(I,R).
  

readln_codes(I,R):-
  % from_codes("$readln",X), % avoids reserved word confusion in self-compilation
  tell_interactor(I,'$cmd'(0),_), % READLN
  % can be anything about what we would like the data format to be
  ask_interactor(I,R). % R= the(Cs) or atom no



/*
% broken as user cannot see line until touching enter

readln_codes(R):-
  current_input(I),
  readln_codes(I,R).
  
readln_codes(I,R):-
   swi_once(read_line_to_codes(I,R)).
 
*/


%% read_string(S): reads from current input, a line as a symbol without printing any prompt
read_string(T):-
  lean_current_input(I),
  read_string(I,T).

%% read_string(I,S): reads from interactor I, a line as a symbol without printing any prompt 
read_string(I,T):-prompt_and_readln_codes(I,'',R),
 ( R=the(Cs)->atom_codes(T,Cs)
  ; T='end_of_file.'
  ).

prompt_and_readln_codes(Prompt,Cs):-
   lean_current_input(I),
   prompt_and_readln_codes(I,Prompt,R), 
   R=the(Cs).
 
 
prompt_and_readln_codes(I,Prompt,R):-
  tell_interactor(I,'$cmd'(0,Prompt),_), % READLN
  ask_interactor(I,R). % R= the(Cs) or atom no

alt_prompt_and_readln_codes(I,Prompt,R):-
  current_input(I),
  swi_once(prompt1(Prompt)),
  readln_codes(R).
   
    


%% get_code(C): reads a char code from current input stream
% moved to lean_io.pl
lean_get_code(R):-
  lean_current_input(I),
  lean_get_code(I,R).
  
%% get_code(I,C): reads a char code from interactor/input stream I
% moved to lean_io.pl
lean_get_code(I,C):-
  tell_interactor(I,'$cmd'(5),_), % GET_CODE
  ask_interactor(I,C).

% codes_of(File,Cs):-findall(C,code_of(File,C),Cs).


%% code_of(File,C): backtracks over char codes in File
% code_of(File,C):-line_codes_of(File,Cs),line2code(Cs,C).



/*
line_codes_of(File0,Cs):-
  find_file(File0,File),
  lean_open(File,read,S),
  repeat,
  ( readln_codes(S,Line),Line=the(Cs0)->Cs=Cs0
  ; !,
    close(S),
    fail
  ).
*/
  
line2code(Cs,C):-member(C,Cs).
line2code(_,10).

find_prolog_file(FileDescr,NewFile):-
  SuffixesCss=["",".pl",".pro",".wam"],
  find_file(FileDescr,SuffixesCss,NewFile).
  
find_file(FileDescr,NewFile):-
  SuffixesCss=["",".pl",".pro"],
  find_file(FileDescr,SuffixesCss,NewFile).


get_path(Pss):-path_elements(Ps),maplist(atom_codes,Ps,Pss).

%% path_element(P): backtracks over path elements P
path_element(P):-path_elements(Ps),member(P,Ps).

find_file(FileDescr,_Sufs,NewFile):-FileDescr='$str'(_),!,NewFile=FileDescr.
find_file(FileDescr,Sufs,NewFile):-
  find_file0(FileDescr,Sufs,NewFile),
  !.
find_file(FileDescr,_Sufs,_NewFile):-	
  errmes(no_such_file_not_found,FileDescr).
   
find_file0(FileDescr,Sufs,NewFile):-
   get_path(Ps),
   append(["","progs"],Ps,Prefs),
   find_file0(Prefs,FileDescr,Sufs,NewFile).

%% find_file(Prefs,File,Sufs,NewFile): finds File with name prefixed and suffixed as NewFile
    
find_file0(_,File,_,NewFile):-
  absolute_file_name(File,AFile),
  exists_file(AFile),
  !,
  NewFile=AFile.
find_file0(Prefs,File,Sufs,NewFile):-
  atom_codes(File,Fs),
  member(Suf,Sufs),
  member(Pref0,Prefs),
  expand_pref(Pref0,Pref),
  append([Pref,Fs,Suf],Cs),
  atom_codes(NewFile0,Cs),
  % traceln(fileCandidate(NewFile)),
  absolute_file_name(NewFile0,NewFile1),
  exists_file(NewFile1),
  !,
  NewFile=NewFile1.

find_file(Prefs,File,Sufs,NewFile):-
  find_file0(Prefs,File,Sufs,NewFile),
  !.  
find_file(_Prefs,File,_Sufs,_NewFile):-
  errmes(file_not_found,File).
  
expand_pref([],Cs):-!,Cs=[].
expand_pref(Cs,NewCs):-nonvar(Cs),append(Cs,"/",NewCs).

%% abs2path_file(Abs,Path,File): splits an absolute file name into Path and File
abs2path_file(Abs,Path,File):-
  atom_codes(Abs,Cs),reverse(Cs,RCs),
  % [Sep1,Sep2]="/\",
  [Sep1,Sep2]=[47,92],
  ( append(RFs,[Sep1|Xs],RCs)->RPs=[Sep1|Xs]
  ; append(RFs,[Sep2|Xs],RCs)->RPs=[Sep2|Xs]
  ; RPs=[],RFs=RCs
  ),
  reverse(RFs,Fs),
  reverse(RPs,Ps),
  !,
  atom_codes(File,Fs),
  atom_codes(Path,Ps),
  % traceln(exiting_abs2path_file(Abs,Path,File)),
  true.
  

