% db.pl

existence_error(':'(('$'),G)):-warnmes(existence_error,G).

trim_callback(':'(('$'),G),R):-!,R=G.
trim_callback(G,G).

try_ifdef(G0):-trim_callback(G0,G),try_ifdef0(G).

try_ifdef0(G):-
  % writeln(got_from_swi(G)),
  is_compiled(G),
  !,
  G.
try_ifdef0(G):-
  warnmes(undefined_predicate,G).
  
index(P):-functor(P,F,N),dynamic(F/N).

jdb_call(I,O):-jcall('vm.extensions.XBuiltins',I,O).


iget(I,R):-jdb_call(next(I),R).
  
ielement_of(I,X):-iget(I,the(A)),iselect_from(I,A,X).

iselect_from(_,A,A).
iselect_from(I,_,X):-ielement_of(I,X).


db_load(Db,File):-swi_do(db_load(Db,File)).

db_save(Db,File):-swi_do(db_save(Db,File)).

db_to_wam(Db,File):-swi_do(db_to_wam(Db,File)).

db_dynamic(Db,FN):-swi_do(db_dynamic(Db,FN)).

% db_is_dynamic(Db,H):-traceln(db_is_dynamic(Db,H)),fail.
db_is_dynamic(Db,H):-swi_once(db_is_dynamic(Db,H)).

db_current_predicate(Db,FN):-swi_call(db_current_predicate(Db,FN)).
 
db_abolish(Db,FN):-swi_do(db_abolish(Db,FN)).

db_assertz(Db,C):-swi_do(db_assertz(Db,C)).

db_assert(Db,C):-swi_do(db_assertz(Db,C)).

db_asserta(Db,C):-swi_do(db_asserta(Db,C)).

db_clause(Db,H,B):-swi_call(db_clause(Db,H,B)).

db_call(Db,G):-swi_call(db_call(Db,G)). % _alt ?

db_once(Db,G):-swi_call(db_once(Db,G)).

db_retract(Db,C):-swi_call_alt(db_retract(Db,C)).

db_retract1(Db,C):-swi_once(db_retract1(Db,C)).
  
db_retractall(Db,C):-swi_do(db_retractall(Db,C)).

%% db_clear(Db) : clears all clauses of database Db, if any 
db_clear(Db):-swi_do(db_clear(Db)).

db_listing(Db,FN):-swi_do(db_listing(Db,FN)).

db_listing(Db):-swi_do(db_listing(Db)).

db_assert_unique(Db,C):-swi_do(db_assert_unique(Db,C)).

%% db_reconsult(InFile,Db) reconsults InFile to database named Db
db_reconsult(InFile,Db):-swi_do(db_reconsult(InFile,Db)).

%% db_consult(InFile,Db) consults InFile to database named Db  
db_consult(InFile,Db):-
  swi_do(db_consult(InFile,Db)).
  
%% xsave(File,Db): saves clause in database Db to one compressed file
xsave(File,Db):-swi_do(xsave(File,Db)).
  
%% xload(File,Db): replaces all clauses in Db with clauses in compressed file - make sure indexing is on if needed!
xload(File,Db):-
  % exists_file(File),
  swi_do(xload(File,Db)).
 
% default db - Lean only as some built-ins with same name would clash in SWI

%% is_dynamic(H): checks if a predicate H=F(X1..Xn) is dynamic
is_dynamic(H):-db_is_dynamic('$',H).

%% dynamic(F/N): ensures predicate F/N is dynamic
dynamic(FN):-db_dynamic('$',FN).

%% abolish(F/N), abolish(F,N): discards predicate F/N, calling it later triggers an error
abolish(FN):-db_abolish('$',FN).
abolish(F,N):-abolish(F/N).

%% abolish_dynamic(F/N): discards predicate F/N, such that calling it later just fails
abolish_dynamic(FN):-db_abolish('$',FN).

%% assert(C): adds clause C to the database
assertz(C):-db_assertz('$',C).

%% assertz(C): adds clause C to end of the database
assert(C):-assertz(C).

%% asserta(C): adds clause C to the front of the database
asserta(C):-db_asserta('$',C).

%% clause(H,B): true if clause C=(H:-B) is in the database

clause(H,B):-db_clause('$',H,B).

%% retract(H): retracts clauses with head unifying with H from the database, one at a time
retract(H):-db_retract('$',H).
 
%% retract1(H): retracts (at most) one clause with head unifying with H from the database
retract1(H):-db_retract1('$',H).
 
%% retractall(H): retracts all clauses with head unifying with H from the database
retractall(H):-db_retractall('$',H).

%% db_clear: removes all clauses from database
db_clear:-db_clear('$').

assert_unique(C):-db_assert_unique('$',C).

listing(FN):-db_listing('$',FN).

%% listing: list all clauses from default database
listing:-db_listing('$').

%% consult(F): asserts all clauses if F to default database
consult(F0):-lean_consult(F0).

%% reconsult(F):cleans default database and then asserts all clauses if F to defaul database
reconsult(InFile):-lean_reconsult(InFile).

%% consult(F): asserts all clauses if F to default database
lean_consult(F0):-db_consult(F0,'$').

%% reconsult(F):cleans default database and then asserts all clauses if F to defaul database
lean_reconsult(InFile):-db_reconsult(InFile,'$').

current_dynamic(FN):-db_current_predicate('$',FN).
 

current_predicate(FN):-current_compiled(FN).
current_predicate(FN):-current_dynamic(FN). %!!!  
  
  

