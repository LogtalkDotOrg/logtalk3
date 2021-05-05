% lean support operations

db_consult(InFile,Db):-(var(InFile);var(Db)),!,warnmes(nonvar_expected_in,db_reconsult(InFile,Db)).
db_consult('$str'(S),Db):-!,str_to_db(S,Db).
db_consult(F,Db):-Db:consult(F).
 
db_reconsult(InFile,Db):-nonvar(InFile),nonvar(Db),!,
  db_clear(Db),
  db_consult(InFile,Db).
db_reconsult(InFile,Db):-
  warnmes(nonvar_expected_in,db_reconsult(InFile,Db)).
  
lean_consult(F):-db_consult(($),F).
 
lean_reconsult(InFile):-db_reconsult(InFile,'$').
 
str_to_db(S,Db):-
  T='temp___.pl',
  open(T,write,F),
  write(F,S),nl(F),
  close(F),
  db_consult(T,Db),
  delete_file(T).
     
% db_ API to be tested here

db_clear(Database) :-
	must_be(atom, Database),
	current_predicate(Database:Predicate),
	abolish(Database:Predicate),
	fail.
db_clear(_).

db_current_predicate(Database, Predicate) :-
	must_be(atom, Database),
	must_be(var_or_predicate_indicator, Predicate),
	current_predicate(Database:Predicate).

db_abolish(Database, Predicate) :-
	must_be(atom, Database),
	abolish(Database:Predicate).

db_assertz(Database, Clause) :-
	must_be(atom, Database),
	assertz(Database:Clause).

db_asserta(Database, Clause) :-
	must_be(atom, Database),
	asserta(Database:Clause).

db_call(Database, Goal) :-
	call(Database:Goal).

db_once(Database, Goal) :-
	once(Database:Goal).

db_clause(Database, Head, Body) :-
	must_be(atom, Database),
	must_be(callable, Head),
	(	var(Body) ->
		true
	;	must_be(callable, Body)
	),
	Database:clause(Head, Body).

db_retract(Database, Clause) :-
	must_be(atom, Database),
	retract(Database:Clause).

db_retract1(Database,C) :-
	once(retract(Database:C)).

db_retractall(Database, Head) :-
	must_be(atom, Database),
	retractall(Database:Head).

db_listing(Database, Predicate) :-
	must_be(atom, Database),
	must_be(ground, Predicate),
	must_be(predicate_indicator, Predicate),
	listing(Database:Predicate).

db_listing(Database) :-
	must_be(atom, Database),
	listing(Database:_).

db_dynamic(Database, Predicate) :-
	must_be(atom, Database),
	must_be(ground, Predicate),
	(	Predicate == [] ->
		true
	;	Predicate = [_|_] ->
		must_be(list(predicate_indicator), Predicate)
	;	must_be(predicate_indicator, Predicate)
	),
	dynamic(Database:Predicate).

db_is_dynamic(Database, Predicate) :-
	must_be(atom, Database),
	must_be(ground, Predicate),
	must_be(predicate_indicator, Predicate),
	Predicate = Functor/Arity,
	functor(Template, Functor, Arity),
	predicate_property(Database:Template, (dynamic)),
	!.

is_dynamic(H):-db_is_dynamic('$',H).

db_assert_unique(Database,(H:-B)):-!,db_clause(Database,H,B),!.
db_assert_unique(Database,H):-db_clause(Database,H,true),!.
db_assert_unique(Database,C):-assertz(Database:C).


db_save(Database, File) :-
	must_be(atom, Database),
	tell(File),
	listing(Database:_),
	told.

db_load(Database, File) :-
	must_be(atom, Database),
	Database:reconsult(File).

db_to_wam(Database, File) :-
	must_be(atom, Database),
	must_be(atom, File),
	db_save(Database, File),
	file_name_extension(Name, _, File),
	compile_to_wam(Name).


% deprecated predicates; use db_assertz(Database, Clause), db_save(Database, File), db_load(Database, File) instead

db_assert(Database, Clause) :-
	db_assertz(Database, Clause).

%% xsave(File,Database): saves clause in database Database to one compressed file
xsave(File, Database) :-
	db_save(Database, File).

%% xload(File,Database): replaces all clauses in Database with clauses in compressed file - make sure indexing is on if needed!
xload(File, Database) :-
	db_load(Database, File).


% ensure_nonvar(X,_):-nonvar(X),!.
% ensure_nonvar(X,C):-warnmes(nonvar_expected(X),in(C)).


:- multifile(error:has_type/2).

error:has_type(var_or_predicate_indicator, Term) :-
	(	var(Term) ->
		true
	;	Term = Functor/Arity,
		(var(Functor); atom(Functor)),
		(var(Arity); integer(Arity), Arity >= 0), !
	).

% be safe and check that a predicate_indicator type is not
% already provided before adding it
:- if(\+ error:has_type(predicate_indicator,a/1)).

	error:has_type(predicate_indicator, Term) :-
		ground(Term), Term = Functor/Arity, atom(Functor), integer(Arity), Arity >= 0.

:- endif.

/*
% optional goal-expansion support
% 
% by default goal-expansion is diasabled; to enable it call:
%	set_prolog_flag(db_goal_expansion, true)

:- create_prolog_flag(kyndi_db_goal_expansion, false, []).


db_goal_expansion(db_dynamic(Database, Predicates), dynamic(Database:Predicates)) :-
	ground(db_dynamic(Database, Predicates)),
	must_be(atom, Database), (is_list(Predicates) -> must_be(list(predicate_indicator), Predicates); must_be(predicate_indicator, Predicates)).
db_goal_expansion(db_abolish(Database, Predicate), abolish(Database:Predicate)) :-
	ground(db_abolish(Database, Predicate)),
	must_be(atom, Database), must_be(predicate_indicator, Predicate).
db_goal_expansion(db_asserta(Database, Clause), asserta(Database:Clause)) :-
	nonvar(Database), nonvar(Clause),
	must_be(atom, Database), must_be(callable, Clause).	
db_goal_expansion(db_assertz(Database, Clause), assertz(Database:Clause)) :-
	nonvar(Database), nonvar(Clause),
	must_be(atom, Database), must_be(callable, Clause).
db_goal_expansion(db_retract(Database, Clause), retract(Database:Clause)) :-
	nonvar(Database), nonvar(Clause),
	must_be(atom, Database), must_be(callable, Clause).
db_goal_expansion(db_retractall(Database, Head), retractall(Database:Head)) :-
	nonvar(Database), nonvar(Head),
	must_be(atom, Database), must_be(callable, Head).
db_goal_expansion(db_clause(Database, Head, Body), clause(Database:Head, Body)) :-
	nonvar(Database), nonvar(Head),
	must_be(atom, Database), must_be(callable, Head).
db_goal_expansion(db_call(Database, Goal), Database:Goal) :-
	nonvar(Database), nonvar(Goal),
	must_be(atom, Database), must_be(callable, Goal).
db_goal_expansion(db_once(Database, Goal), once(Database:Goal)) :-
	nonvar(Database), nonvar(Goal),
	must_be(atom, Database), must_be(callable, Goal).
db_goal_expansion(db_listing(Database), listing(Database:_)) :-
	nonvar(Database),
	must_be(atom, Database).
db_goal_expansion(db_load(Database, File), Database:reconsult(File)) :-
	ground(db_load(Database, File)),
	must_be(atom, Database), must_be(atom, File).

:- multifile(user:goal_expansion/2).
:- dynamic(user:goal_expansion/2).

user:goal_expansion(Goal, ExpandedGoal) :-
	current_prolog_flag(kyndi_db_goal_expansion, true),
	db_goal_expansion(Goal, ExpandedGoal).
*/
