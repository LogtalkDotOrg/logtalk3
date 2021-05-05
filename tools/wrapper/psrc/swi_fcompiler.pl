:- set_prolog_flag(double_quotes, codes).

:-[swi_db].

:-op(100,fx,('?')).
:-op(200,fx,('`')).
:-op(600,xfy,(':')).

:-op(400,'yfx','gcd').
:-op(500,'yfx','eq').

:-op(600,xfx,('~>')).

:-op(700,xfx,('=>')).
:-op(700,xfx,('<=')).
:-op(700,xfx,('==>')).
:-op(700,xfx,('<==')).

:-op(800,fy,('#')).
:-op(800,fy,('@')).

:-op(1200,xfx,('::-')).

% :-op(100,yfx,('.')).

% file compiler

is_prolog(swi_prolog).

:-[swi_bcompiler].
:-[builtins].

 
lboot:-
  %working_directory(Old,'../psrc'),
  bcompile('top.pl','../bin/lwam.bp').
  %working_directory(_,Old).  

cboot:-
  % cd('src'),
  bcompile('ctop.pl','../wam.bp').
  
bcompile(From,To):-scompile([],From,'bp',To).

% deprecated

fcompile(F):-scompile(F).
wcompile(F):-scompile(F).



scompile(F):-scompile([progs],F,'wam',_).

scompile(Dirs,File0,Suf,Target0):-member(Suf,[wam,bp]),!,
  trim_prolog_file(File0,File,_Suf),
  atom_concat(File,'.pl',Source0),
  find_prolog_file(Dirs,Source0,Source),
  trim_prolog_file(Source,TrimmedSource,_),
  ( nonvar(Target0)->Target=Target0
  ; atomic_list_concat([TrimmedSource,('.'),Suf],Target)
  ),  
  just_compile(Source,Target,Suf,Dirs,File).



compile_to_wam(FileNoSuf):-
   atom_concat(FileNoSuf,'.pl',PL),
   atom_concat(FileNoSuf,'.wam',WAM),
   atom_concat(FileNoSuf,'.dyn',DYN),
   just_compile(PL,WAM,wam,[],DYN).
   

just_compile(Source,Target,Suf,Dirs,File):-
  % writeln(compiling(from(Source),to(Target))),
  
  nb_setval(ok,true),
 
  tell(Target),
  (Suf==bp->cc_bbuiltins(wam);true),
  translate_one_file(Dirs,Source,wam,ttyprint),
  !,
  told,
  nb_getval(ok,Ok),
  nb_delete(ok),
  ( Ok==true -> save_directives(File)
  ; abolish('$run'/1),
    delete_file(Target),
    writeln(failing_to_compile(from(Source),to(Target))),
    fail
  ).
  
  
dyn_suffix('.dyn').
 
save_directives(File):-clause('$run'(_),_),!,
   dyn_suffix(Dy),
   atom_concat(File,Dy,Target),
   tell(Target),
   safe_listing('$run'/1),
   told,
   abolish('$run'/1).
save_directives(_File).
 
 
safe_listing(F/N):-
   functor(H,F,N),
   clause(H,B),
   write_canonical((H:-B)),
   put(46),nl,
   fail
; nl.
 
trim_prolog_file(File0,File,Suf):-
  member(Suf,['.pl','.pro']),
  atom_concat(F,Suf,File0),
  !,
  File=F.
trim_prolog_file(File,File,'.pl').  

  


find_prolog_file(_Dirs,AFile,NewFile):- 
  exists_file(AFile),
  !,
  NewFile=AFile.
find_prolog_file(Dirs,File,NewFile):-
  member(Dir,Dirs),
  atomic_list_concat([Dir,'/',File],NewFile1),
  exists_file(NewFile1),
  !,
  NewFile=NewFile1.
find_prolog_file(Dirs,File,_NewFile):-
  ttyprint(file_not_found(File,in(Dirs))),
  fail.

  
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
   
  
translate_all_files(Dirs,Files,Mode,Printer):-
  nonvar(Files),Files=[_|_],
  !,
  (
    member(F,Files),
      translate_one_file(Dirs,F,Mode,Printer),
    fail
  ; true
  ).
translate_all_files(Dirs,Fname,Mode,Printer):-
  translate_one_file(Dirs,Fname,Mode,Printer).

translate_one_file(Dirs,Fname,Mode,Printer):-
  trim_prolog_file(Fname,Fname0,_),
  atom_concat(Fname0,'.pl',FnamePL),
  find_prolog_file(Dirs,FnamePL,F),
  abs2path_file(F,Dir,_),
  %seeing(F0),see(F),

  open(F,read,Stream),
  
  repeat,
    %call(Printer,reading),
    
    ( read_lean_clause(Stream,C) -> true
      % read_clause(Stream,C,[double_quotes(codes),dotlists(true)]) -> true  
    ; % ttyprint('_______failed'),
      nb_setval(ok,false)
    ),
    % call(Printer,read=C),
    translate([Dir|Dirs],C,Mode,Printer),
  !,
  close(Stream).


read_lean_clause(Stream,C):-
    read_term(Stream,C,[
        dotlists(true),
        double_quotes(codes),
        singletons(warning),
        syntax_errors(fail)
      ]
    ).

/*
translate(Dirs,X,Mode,Printer):-
  ttyprint(translate(Dirs,X,Mode,Printer)),
  fail.
*/
translate(_,end_of_file,_,_):-!.
translate(Dirs,':-'([F|Fs]),Mode,Printer):-!,
  include_file(Dirs,[F|Fs],Mode,Printer),
  fail.
translate(Dirs,':-'(include(F)),Mode,Printer):-!,
  include_file(Dirs,[F],Mode,Printer),
  fail. 
  
translate(Dirs,':-'(consult(F)),Mode,Printer):-!,
  translate(Dirs,':-'(lean_consult(F)),Mode,Printer).
translate(Dirs,':-'(reconsult(F)),Mode,Printer):-!,
  translate(Dirs,':-'(lean_reconsult(F)),Mode,Printer).
    
translate(_,':-'(op(A,B,C)),_Mode,_Printer):-!,
  op(A,B,C),
  add_directive(op(A,B,C)),
  fail.    
translate(_,':-'(dynamic(FN)),_Mode,_Printer):-!,
  db_dynamic('$run',FN),
  fail.      
translate(_,':-'(Directive),_Mode,_Printer):-!,
  add_directive(Directive). 
translate(_,'::-'(H,B),Mode,_):-compile_bin(Mode,(H:-B)),fail.
translate(_,(H:-B),_,_):-functor(H,F,A),db_is_dynamic('$run',F/A),!,
  add_directive(assertz((H:-B))).
translate(_,H,_,_):-functor(H,F,A),db_is_dynamic('$run',F/A),!,
  add_directive(assertz(H)).
translate(_,C,Mode,_):-cc(Mode,C),fail.

include_file(Dirs,Fs,Mode,Printer):-
  call(Printer,begin_including(Dirs:Fs)),
  translate_all_files(Dirs,Fs,Mode,Printer),
  call(Printer,end_including(Fs)).

% add_directive(D):-ttyprint(adding_directive(D)),fail.
add_directive(D):-assertz('$run'(D)),fail.

csep:-nl.


add_instr(1,Op,Reg,F0,N):-
  fix_list_cons(F0,F),
  write(Op),csep,
  write(Reg),csep,
  write(N),csep,
  write(F),nl.

fix_list_cons(X,R):-var(X),!,R=X.
fix_list_cons('[|]',R):-!,R='.'.
fix_list_cons([],R):-!,R='[]'.
fix_list_cons(F,F).

ttyprint(T):-telling(S),tell(user),write(T),nl,tell(S).

% ctime(T):-statistics(runtime,[T,_]).
  
symcat([],B,C):-!,symcat0('[]',B,C).
symcat(A,[],C):-!,symcat0(A,'[]',C).
symcat(A,B,C):-symcat0(A,B,C).
  
symcat0(A,B,C):-atomic_list_concat([A,'_',B],C).


to_string(N,S):-number(N),!,number_codes(N,Cs),atom_codes(S,Cs).
to_string(S,S).

%swrite(T,S):-term_to_atom(T,S).

  



   