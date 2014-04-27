
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.1,
		author is 'Robert Sasak, Charles University in Prague. Adapted to Logtalk by Paulo Moura.',
		date is 2014/04/27,
		comment is 'Unit tests for the PDDL 3.0 parsers.'
	]).

	:- uses(os, [
		change_directory/1
	]).

	test(elevators) :-
		logtalk::expand_library_path(pddl_parser, Base),
		atom_concat(Base, 'ipc2008-no-cybersec/seq-opt/elevators-strips/', Directory),
		test_colection(Directory).

	test(openstacks) :-
		logtalk::expand_library_path(pddl_parser, Base),
		atom_concat(Base, 'ipc2008-no-cybersec/seq-opt/openstacks-strips/', Directory),
		test_colection(Directory).

	test(parcprinter) :-
		logtalk::expand_library_path(pddl_parser, Base),
		atom_concat(Base, 'ipc2008-no-cybersec/seq-opt/parcprinter-strips/', Directory),
		test_colection(Directory).

	test(pegsol) :-
		logtalk::expand_library_path(pddl_parser, Base),
		atom_concat(Base, 'ipc2008-no-cybersec/seq-opt/pegsol-strips/', Directory),
		test_colection(Directory).

	test(scanalyzer) :-
		logtalk::expand_library_path(pddl_parser, Base),
		atom_concat(Base, 'ipc2008-no-cybersec/seq-opt/scanalyzer-strips/', Directory),
		test_colection(Directory).

	:- if(current_prolog_flag(max_arity, 255)).
		:- initialization((
			write('WARNING: The "sokoban" unit tests cannot run with this back-end Prolog'), nl,
			write('         compiler due to a limitation on the maximum arity of terms.'), nl
		)).
	:- else.
		test(sokoban) :-
			logtalk::expand_library_path(pddl_parser, Base),
			atom_concat(Base, 'ipc2008-no-cybersec/seq-opt/sokoban-strips/', Directory),
			test_colection(Directory).
	:- endif.

	test(transport) :-
		logtalk::expand_library_path(pddl_parser, Base),
		atom_concat(Base, 'ipc2008-no-cybersec/seq-opt/transport-strips/', Directory),
		test_colection(Directory).

	test(woodworking) :-
		logtalk::expand_library_path(pddl_parser, Base),
		atom_concat(Base, 'ipc2008-no-cybersec/seq-opt/woodworking-strips/', Directory),
		test_colection(Directory).

	test_colection(Directory) :-
		forall(
			problem_file(File),
			(	atom_concat(Directory, File, Path),
				pddl::parse_problem(Path, _, _)
			)
		),
		forall(
			domain_file(File),
			(	atom_concat(Directory, File, Path),
				pddl::parse_domain(Path, _, _)
			)
		).

	problem_file('p01.pddl').
	problem_file('p02.pddl').
	problem_file('p03.pddl').
	problem_file('p04.pddl').
	problem_file('p05.pddl').
	problem_file('p06.pddl').
	problem_file('p07.pddl').
	problem_file('p08.pddl').
	problem_file('p09.pddl').
	problem_file('p10.pddl').
	problem_file('p11.pddl').
	problem_file('p12.pddl').
	problem_file('p13.pddl').
	problem_file('p14.pddl').
	problem_file('p15.pddl').
	problem_file('p16.pddl').
	problem_file('p17.pddl').
	problem_file('p18.pddl').
	problem_file('p19.pddl').
	problem_file('p20.pddl').
	problem_file('p21.pddl').
	problem_file('p22.pddl').
	problem_file('p23.pddl').
	problem_file('p24.pddl').
	problem_file('p25.pddl').
	problem_file('p26.pddl').
	problem_file('p27.pddl').
	problem_file('p28.pddl').
	problem_file('p29.pddl').
	problem_file('p30.pddl').

	domain_file('p01-domain.pddl').
	domain_file('p02-domain.pddl').
	domain_file('p03-domain.pddl').
	domain_file('p04-domain.pddl').
	domain_file('p05-domain.pddl').
	domain_file('p06-domain.pddl').
	domain_file('p07-domain.pddl').
	domain_file('p08-domain.pddl').
	domain_file('p09-domain.pddl').
	domain_file('p10-domain.pddl').
	domain_file('p11-domain.pddl').
	domain_file('p12-domain.pddl').
	domain_file('p13-domain.pddl').
	domain_file('p14-domain.pddl').
	domain_file('p15-domain.pddl').
	domain_file('p16-domain.pddl').
	domain_file('p17-domain.pddl').
	domain_file('p18-domain.pddl').
	domain_file('p19-domain.pddl').
	domain_file('p20-domain.pddl').
	domain_file('p21-domain.pddl').
	domain_file('p22-domain.pddl').
	domain_file('p23-domain.pddl').
	domain_file('p24-domain.pddl').
	domain_file('p25-domain.pddl').
	domain_file('p26-domain.pddl').
	domain_file('p27-domain.pddl').
	domain_file('p28-domain.pddl').
	domain_file('p29-domain.pddl').
	domain_file('p30-domain.pddl').

:- end_object.
