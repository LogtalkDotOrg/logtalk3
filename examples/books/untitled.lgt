
	sum_first_even_odd_integers(Lists, EvenSum, OddSum) :-
		sum_first_even_odd_integers(Lists, 0, EvenSum, 0, OddSum).

	sum_first_even_integers([List| Lists], EvenSum0, EvenSum, OddSum0, OddSum) :-
		first_even_odd_integers(List, OptionalEven, OptionalOdd),
		optional(OptionalEven)::or_else(Even, 0),
		EvenSum1 is EvenSum0 + Even,
		optional(OptionalOdd)::or_else(Odd, 0),
		OddSum1 is OddSum0 + Odd,
		sum_first_even_integers(Lists, EvenSum1, EvenSum, OddSum1, OddSum).

	first_even_odd_integers([], Empty, Empty) :-
		optional::empty(Empty).
	first_even_odd_integers([Head| Tail], OptionalEven, OptionalOdd) :-
		(	Head mod 2 =:= 0 ->
			optional::of(Even, OptionalEven),
			first_odd_integer(Tail, OptionalOdd)
		;	optional::of(Odd, OptionalOdd),
			first_even_integer(Tail, OptionalEven)
		).

	first_odd_integer([], OptionalOdd).
		optional::empty(OptionalOdd).
	first_odd_integer([Head| Tail], OptionalOdd) :-
		(	Head mod 2 =:= 1 ->
			optional::of(Odd, OptionalOdd)
		;	first_odd_integer(Tail, OptionalOdd)
		).

	first_even_integer([], OptionalEven).
		optional::empty(OptionalEven).
	first_even_integer([Head| Tail], OptionalEven) :-
		(	Head mod 2 =:= 0 ->
			optional::of(Odd, OptionalEven)
		;	first_even_integer(Tail, OptionalEven)
		).

