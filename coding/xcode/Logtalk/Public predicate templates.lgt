
	:- public(Functor/0).
	:- mode(Functor, Solutions).
	:- info(Functor/0, [
		comment is '']).

	:- public(Functor/1).
	:- mode(Functor(), Solutions).
	:- info(Functor/1, [
		comment is '',
		arguments is ['Arg'-'Desc']
	]).

	:- public(Functor/2).
	:- mode(Functor(, ), Solutions).
	:- info(Functor/2, [
		comment is '',
		arguments is ['Arg1'-'Desc1', 'Arg2'-'Desc2']
	]).

	:- public(Functor/3).
	:- mode(Functor(, , ), Solutions).
	:- info(Functor/3, [
		comment is '',
		arguments is ['Arg1'-'Desc1', 'Arg2'-'Desc2', 'Arg3'-'Desc3']
	]).

	:- public(Functor/4).
	:- mode(Functor(, , , ), Solutions).
	:- info(Functor/4, [
		comment is '',
		arguments is ['Arg1'-'Desc1', 'Arg2'-'Desc2', 'Arg3'-'Desc3', 'Arg4'-'Desc4']
	]).

	:- public(Functor/5).
	:- mode(Functor(, , , , ), Solutions).
	:- info(Functor/5, [
		comment is '',
		arguments is ['Arg1'-'Desc1', 'Arg2'-'Desc2', 'Arg3'-'Desc3', 'Arg4'-'Desc4', 'Arg5'-'Desc5']
	]).
