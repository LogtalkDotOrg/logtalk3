
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Robert Sasak, Charles University in Prague. Adapted to Logtalk by Paulo Moura.',
		date is 2011/06/17,
		comment is 'Unit tests for the PDDL 3.0 parsers.']).

	:- uses(os, [change_directory/1]).

	test(elevators) :-
		change_directory('ipc2008-no-cybersec/seq-opt/elevators-strips/'),
		test_colection,
		change_directory('../../../').

	test(openstacks) :-
		change_directory('ipc2008-no-cybersec/seq-opt/openstacks-strips/'),
		test_colection,
		change_directory('../../../').

	test(parcprinter) :-
		change_directory('ipc2008-no-cybersec/seq-opt/parcprinter-strips/'),
		test_colection,
		change_directory('../../../').

	test(pegsol) :-
		change_directory('ipc2008-no-cybersec/seq-opt/pegsol-strips/'),
		test_colection,
		change_directory('../../../').

	test(scanalyzer) :-
		change_directory('ipc2008-no-cybersec/seq-opt/scanalyzer-strips/'),
		test_colection,
		change_directory('../../../').

	test(sokoban) :-
		change_directory('ipc2008-no-cybersec/seq-opt/sokoban-strips/'),
		test_colection,
		change_directory('../../../').

	test(transport) :-
		change_directory('ipc2008-no-cybersec/seq-opt/transport-strips/'),
		test_colection,
		change_directory('../../../').

	test(woodworking) :-
		change_directory('ipc2008-no-cybersec/seq-opt/woodworking-strips/'),
		test_colection,
		change_directory('../../../').

	test_colection :-
		parse_file('p01.pddl'),
		parse_file('p01-domain.pddl'),
		parse_file('p02.pddl'),
		parse_file('p02-domain.pddl'),
		parse_file('p03.pddl'),
		parse_file('p03-domain.pddl'),
		parse_file('p04.pddl'),
		parse_file('p04-domain.pddl'),
		parse_file('p05.pddl'),
		parse_file('p05-domain.pddl'),
		parse_file('p06.pddl'),
		parse_file('p06-domain.pddl'),
		parse_file('p07.pddl'),
		parse_file('p07-domain.pddl'),
		parse_file('p08.pddl'),
		parse_file('p08-domain.pddl'),
		parse_file('p09.pddl'),
		parse_file('p09-domain.pddl'),
		parse_file('p10.pddl'),
		parse_file('p10-domain.pddl'),
		parse_file('p11.pddl'),
		parse_file('p11-domain.pddl'),
		parse_file('p12.pddl'),
		parse_file('p12-domain.pddl'),
		parse_file('p13.pddl'),
		parse_file('p13-domain.pddl'),
		parse_file('p14.pddl'),
		parse_file('p14-domain.pddl'),
		parse_file('p15.pddl'),
		parse_file('p15-domain.pddl'),
		parse_file('p16.pddl'),
		parse_file('p16-domain.pddl'),
		parse_file('p17.pddl'),
		parse_file('p17-domain.pddl'),
		parse_file('p18.pddl'),
		parse_file('p18-domain.pddl'),
		parse_file('p19.pddl'),
		parse_file('p19-domain.pddl'),
		parse_file('p20.pddl'),
		parse_file('p20-domain.pddl'),
		parse_file('p21.pddl'),
		parse_file('p21-domain.pddl'),
		parse_file('p22.pddl'),
		parse_file('p22-domain.pddl'),
		parse_file('p23.pddl'),
		parse_file('p23-domain.pddl'),
		parse_file('p24.pddl'),
		parse_file('p24-domain.pddl'),
		parse_file('p25.pddl'),
		parse_file('p25-domain.pddl'),
		parse_file('p26.pddl'),
		parse_file('p26-domain.pddl'),
		parse_file('p27.pddl'),
		parse_file('p27-domain.pddl'),
		parse_file('p28.pddl'),
		parse_file('p28-domain.pddl'),
		parse_file('p29.pddl'),
		parse_file('p29-domain.pddl'),
		parse_file('p30.pddl'),
		parse_file('p30-domain.pddl').

	parse_file(File) :-
		(	pddl::parse_domain(File, _, _)
		;	pddl::parse_problem(File, _, _)
		),
		!.

:- end_object.
