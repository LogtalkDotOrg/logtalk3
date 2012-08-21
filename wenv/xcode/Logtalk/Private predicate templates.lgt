
	:- private(Functor/0).
	:- mode(Functor, Solutions).
	:- info(Functor/0, [
		comment is '']).

	:- private(Functor/1).
	:- mode(Functor(), Solutions).
	:- info(Functor/1, [
		comment is '',
		arguments is ['Arg'-'Desc']]).

	:- private(Functor/2).
	:- mode(Functor(, ), Solutions).
	:- info(Functor/2, [
		comment is '',
		arguments is ['Arg1'-'Desc1', 'Arg2'-'Desc2']]).

	:- private(Functor/3).
	:- mode(Functor(, , ), Solutions).
	:- info(Functor/3, [
		comment is '',
		arguments is ['Arg1'-'Desc1', 'Arg2'-'Desc2', 'Arg3'-'Desc3']]).

	:- private(Functor/4).
	:- mode(Functor(, , , ), Solutions).
	:- info(Functor/4, [
		comment is '',
		arguments is ['Arg1'-'Desc1', 'Arg2'-'Desc2', 'Arg3'-'Desc3', 'Arg4'-'Desc4']]).

	:- private(Functor/5).
	:- mode(Functor(, , , , ), Solutions).
	:- info(Functor/5, [
		comment is '',
		arguments is ['Arg1'-'Desc1', 'Arg2'-'Desc2', 'Arg3'-'Desc3', 'Arg4'-'Desc4', 'Arg5'-'Desc5']]).
