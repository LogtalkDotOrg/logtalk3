
:- object(epq).

	:- info([
		version is 0:0:0,
		author is 'Paul Tarau. Adapted to Logtalk by Paulo Moura',
		date is 2016-06-18,
		comment is 'Description'
	]).

	:- public(new/1).
	:- mode(new(-engine), one).
	:- info(new/1, [
		comment is 'Description',
		argnames is ['Queue']
	]).

	:- public(top/2).
	:- mode(top(@engine, ?term), zero_or_one).
	:- info(top/2, [
		comment is 'Description',
		argnames is ['Queue', 'Term']
	]).

	:- public(insert/2).
	:- mode(insert(@engine, @term), one).
	:- info(insert/2, [
		comment is 'Description',
		argnames is ['Queue', 'Term']
	]).

	:- public(insert/3).
	:- mode(insert(@engine, +integer, @term), one).
	:- info(insert/3, [
		comment is 'Description',
		argnames is ['Queue', 'Priority', 'Term']
	]).

	:- public(delete/2).
	:- mode(delete(@engine, -term), zero_or_one).
	:- info(delete/2, [
		comment is 'Description',
		argnames is ['Queue', 'Term']
	]).

	:- threaded.

	new(Queue) :-
		minheap::new(Heap),
		threaded_engine_create(_, loop(Heap), Queue).

	destroy(Queue) :-
		threaded_engine_destroy(Queue).

	loop(Heap):-
		threaded_engine_fetch(Op),
		dispatch(Op, Heap, NewHeap),
%		{garbage_collect},
		loop(NewHeap).

	dispatch(top(Term), Heap, Heap) :-
		(	minheap::top(Heap, _, Term) ->
			threaded_engine_yield(Term)
		;	threaded_engine_yield('$empty')
		).
	dispatch(insert(Priority,Term), Heap, NewHeap) :-
		minheap::insert(Priority, Term, Heap, NewHeap).
	dispatch(delete, Heap, NewHeap) :-
		minheap::delete(Heap, _, Term, NewHeap),
		threaded_engine_yield(Term).

	top(Queue, Term) :-
		threaded_engine_post(Queue, top(Term)),
		threaded_engine_next(Queue, Term),
		Term \== '$empty'.		

	insert(Queue, Priority, Term) :-
		threaded_engine_post(Queue, insert(Priority,Term)).

	insert(Queue, Term) :-
		Priority is 1<<28,
		threaded_engine_post(Queue, insert(Priority,Term)).

	delete(Queue, Term) :-
		threaded_engine_post(Queue, delete),
		threaded_engine_next(Queue, Term).

:- public(go/0).
go:-
  new(Q),
  insert(Q,one),
  insert(Q,10,two),
  insert(Q,5,three),
  delete(Q,Three),
  delete(Q,Two),
  delete(Q,One),
  write([Three,Two,One]), nl,
  destroy(Q).

:- end_object.
