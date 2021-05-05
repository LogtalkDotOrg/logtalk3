% util.pl

[File]:-scompile(File).

scompile(FName):-
  find_prolog_file(FName,File0),
  % println(compiling(File0)),
  trim_prolog_file(File0,File,Suf),
  atom_concat(File,'.wam',WamFile),
  atom_concat(File,Suf, PlFile),
  ( 
    newer_file_of(WamFile,PlFile,F),F=PlFile->
    swi_call(scompile(File))
  ; \+exists_file(WamFile)->swi_call(scompile(File))
  ; true  
  ),
  push_cmd_arg(load_with_new_engine(WamFile)),
  return(restart(File)).

fcompile(F):-swi_call(scompile(F)).

trim_prolog_file(File0,File,Suf):-
  member(Suf,['.pl','.pro','.wam','.bp']),
  atom_concat(F,Suf,File0),
  !,
  File=F.
trim_prolog_file(File,File,'.pl').  

load_with_new_engine(WamFileName):-
  wam_reader(WamFileName,Reader),
  external_top(WamFileName,Reader).

wam_reader(WamFileName,Reader):-
  lean_open(WamFileName,read,Interactor),
  
  jcall('vm.extensions.LineStreamInput',getReader(Interactor),Reader).
  
atom_to_term(S,T):-read_term_from_atom(S,T,_).

op(X,Y,Z):-swi_call(op(X,Y,Z)).

current_op(X,Y,Z):-swi_call(current_op(X,Y,Z)).
