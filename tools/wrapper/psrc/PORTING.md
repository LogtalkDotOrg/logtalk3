# lists.pl


## mapfun/3



## maplist/2-4

SWI-Prolog

	:- use_module(library(apply)).
	maplist(...)

Logtalk

	logtalk_load(library(meta_compiler_loader)).
	meta::maplist(...)
	meta::map(...)

## sumlist/2

SWI-Prolog

	:- use_module(library(lists)).
	sum_list(List, Sum)

Logtalk

	logtalk_load(library(types_loader)).
	numberlist::sum(List, Sum)

## prodlist/2

SWI-Prolog

	n/a

Logtalk

	logtalk_load(library(types_loader)).
	numberlist::product(List, Sum)

## foldl/4

Warning: SWI-Prolog uses different argument order!

SWI-Prolog

	:- use_module(library(apply)).
	foldl(:Goal, +List, +V0, -V)

Logtalk

	logtalk_load(library(meta_compiler_loader)).
	meta::fold_left(Closure, Accumulator, List, Result)
	meta::foldl(Closure, Accumulator, List, Result)

## foldr/4

SWI-Prolog

	n/a

Logtalk

	logtalk_load(library(meta_compiler_loader)).
	meta::fold_right(Closure, Accumulator, List, Result)
	meta::foldr(Closure, Accumulator, List, Result)

## zip/3

Warning: SWI-Prolog and Logtalk use different argument order!

SWI-Prolog

	:- use_module(library(pairs)).
	pairs_keys_values(Pairs, Keys, Values)

Logtalk

	logtalk_load(library(pairs)).
	pairs::keys_values(Pairs, Keys, Values)

## zipWith2/4

Alias to maplist/4!

## is_list/1

SWI-Prolog

	built-in predicate

Logtalk

	logtalk_load(library(basic_types_loader))
	list::valid(Term)

## append/3

SWI-Prolog

	:- use_module(library(lists)).

Logtalk

	logtalk_load(library(basic_types_loader))
	list::append(L1, L2, L)

## append/2

SWI-Prolog

	:- use_module(library(lists)).

Logtalk

	logtalk_load(library(basic_types_loader))
	list::append(Ls, L)

## select/3

SWI-Prolog

	:- use_module(library(lists)).

Logtalk

	logtalk_load(library(basic_types_loader))
	list::select(Element, List, Tail)

## member/2

SWI-Prolog

	:- use_module(library(lists)).

Logtalk

	logtalk_load(library(basic_types_loader))
	list::member(Element, List)

## member/2

SWI-Prolog

	:- use_module(library(lists)).

Logtalk

	logtalk_load(library(basic_types_loader))
	list::memberchk(Element, List)

## rmember/2

SWI-Prolog

	n/a

Logtalk

	n/a

## rselect/2

SWI-Prolog

	n/a

Logtalk

	n/a

## select_nonvar/3

SWI-Prolog

	n/a

Logtalk

	n/a

## nth0/3

SWI-Prolog

	:- use_module(library(lists)).

Logtalk

	logtalk_load(library(basic_types_loader))
	list::nth0(N, List, Element)

## nth1/3

SWI-Prolog

	:- use_module(library(lists)).

Logtalk

	logtalk_load(library(basic_types_loader))
	list::nth1(N, List, Element)

## last/2

SWI-Prolog

	:- use_module(library(lists)).

Logtalk

	logtalk_load(library(basic_types_loader))
	list::last(List, Last)

## reverse/2

SWI-Prolog

	:- use_module(library(lists)).

Logtalk

	logtalk_load(library(basic_types_loader))
	list::reverse(List, Reverse)

## init/2

SWI-Prolog

	n/a

Logtalk

	n/a

## take/3

SWI-Prolog

	:- use_module(library(lists)).
	nth1(N, List, _ , Rest)

Logtalk

	logtalk_load(library(basic_types_loader))
	list::nth1(N, List, _ , Rest)
	list::prefix(Rest, N, List)

## split_at/4

SWI-Prolog

	n/a

Logtalk

	logtalk_load(library(basic_types_loader))
	list::split(List, N, Sublists, Remaining)

## drop/3

SWI-Prolog

	n/a

Logtalk

	logtalk_load(library(basic_types_loader))
	list::length(List, N), M is N - 1, list::suffix(Rest, M, List)

## split_to_groups/3

SWI-Prolog

	n/a

Logtalk

	n/a

## split_to_size/3

SWI-Prolog

	n/a

Logtalk

	n/a

## partition/4

SWI-Prolog

	:- use_module(library(apply)).

Logtalk

	logtalk_load(library(meta_compiler_loader)).
	meta::partition(Closure, List, Included, Excluded)

## nth0/4

SWI-Prolog

	:- use_module(library(lists)).

Logtalk

	logtalk_load(library(basic_types_loader))
	list::nth0(N, List, Element, Rest)

## subtract/3

SWI-Prolog

	:- use_module(library(lists)).

Logtalk

	n/a (but defined for ordered sets in object `set`)

## union/3

SWI-Prolog

	:- use_module(library(lists)).

Logtalk

	n/a (but defined for ordered sets in object `set`)

## intersection

SWI-Prolog

	:- use_module(library(lists)).

Logtalk

	n/a (but defined for ordered sets in object `set`)

# sort.pl

## sort/2

Standard built-in predicate.

## msort/2

SWI-Prolog

	built-in predicate

Logtalk

	logtalk_load(library(basic_types_loader))
	list::msort(List, Sorted)

## keysort/2

Standard built-in predicate.

## keygroup/3

SWI-Prolog

	n/a

Logtalk

	n/a

## remdup/2

This is equivalent to sort/2.

## compare/3

Standard built-in predicate.

## ord_union/3

SWI-Prolog

	:- use_module(library(ordsets)).

Logtalk

	logtalk_load(library(types_loader))
	set::union(Set1, Set2, Union)

## ord_intersection/3

SWI-Prolog

	:- use_module(library(ordsets)).

Logtalk

	logtalk_load(library(types_loader))
	set::intersection(Set1, Set2, Intersection)

## ord_subtract/3

SWI-Prolog

	:- use_module(library(ordsets)).

Logtalk

	logtalk_load(library(types_loader))
	set::subtract(Set1, Set2, Difference)

## ord_sym_difference/3

SWI-Prolog

	:- use_module(library(ordsets)).
	ord_symdiff(Set1, Set2, Difference)

Logtalk

	logtalk_load(library(types_loader))
	set::symdiff(Set1, Set2, Difference)

## ord_subset/2

SWI-Prolog

	:- use_module(library(ordsets)).

Logtalk

	logtalk_load(library(types_loader))
	set::subset(Subset, Set)

## sublist_of_length/3

SWI-Prolog

	n/a

Logtalk

	n/a

## set_*

	n/a

## mset_*

	n/a

# xlib.pl

## numlist/3

SWI-Prolog

	:- use_module(library(lists)).

Logtalk

	logtalk_load(library(basic_types_loader))
	integer::sequence(Inf, Sup, Sequence)

## for/3

Switch to the `between/3` standard built-in predicate.
