
:- protocol(databasep).

	:- info([
		version is 1.0,
		author is 'Victor Lagerkvist',
		date is 2010/06/13,
		comment is 'Database protocol.']).

	:- public(rule/4).
	:- mode(rule(?callable, ?callable, -, -), zero_or_more).
	:- info(rule/4,
		[comment is 'Clauses for this predicate are automatically generated using term-expansion. The third argument contains the length of Body.',
		 argnames is ['Head', 'Body', 'Length', 'Tail']]).

	:- public(rule/3).
	:- mode(rule(?callable, ?callable, -), zero_or_more).
	:- info(rule/3,
		[comment is 'Clauses for this predicate are automatically generated using term-expansion. The third argument denotes the tail of the Body.',
		 argnames is ['Head', 'Body', 'Tail']]).

	:- public(rule/2).
	:- mode(rule(?callable, -list(callable)), zero_or_more).
	:- info(rule/2, [
		comment is 'Clauses for this predicate are automatically generated using term-expansion.',
		argnames is ['Head', 'Body']]).

	:- public(bench_goal/1).
	:- mode(bench_goal(?callable), zero_or_more).
	:- info(bench_goal/1, [
		comment is 'Table of benchmark goals. They are used from shell.lgt to make benchmarking easier.',
		argnames is ['Goal']]).

:- end_protocol.
