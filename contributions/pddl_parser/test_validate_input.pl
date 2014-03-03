%:-[parseProblem, parseDomain].

%parse_file(+File).
parse_file(F) :-
	(	catch(parse_file_aux(F), Error, parse_file_error(F, Error)) ->
		true
	;	parse_file_failed(F)
	).

parse_file_aux(F) :-
	(	domain::parse(F, L, _)
	;	problem::parse(F, L, _)
	),
	!.

parse_file_error(F, Error) :-
	write('Parsing file error. '), write('('), write(F), write(': '), writeq(Error), write(')'), nl.

parse_file_failed(F) :-
	write('Parsing file failed. '), write('('), write(F), write(')'), nl.

test :-
	'$lgt_change_directory'('ipc2008-no-cybersec/seq-opt/elevators-strips/'),
	write('Testing problem set elevators'), nl,
	test_colection,
	'$lgt_change_directory'('../../../'),
	'$lgt_change_directory'('ipc2008-no-cybersec/seq-opt/openstacks-strips/'),
	write('Testing problem set openstacks'), nl,
	test_colection,
	'$lgt_change_directory'('../../../'),
	'$lgt_change_directory'('ipc2008-no-cybersec/seq-opt/parcprinter-strips/'),
	write('Testing problem set parcprinter'), nl,
	test_colection,
	'$lgt_change_directory'('../../../'),
	'$lgt_change_directory'('ipc2008-no-cybersec/seq-opt/pegsol-strips/'),
	write('Testing problem set pegsol'), nl,
	test_colection,
	'$lgt_change_directory'('../../../'),
	'$lgt_change_directory'('ipc2008-no-cybersec/seq-opt/scanalyzer-strips/'),
	write('Testing problem set scanalyzer'), nl,
	test_colection,
	'$lgt_change_directory'('../../../'),
	'$lgt_change_directory'('ipc2008-no-cybersec/seq-opt/sokoban-strips/'),
	write('Testing problem set sokoban'), nl,
	test_colection,
	'$lgt_change_directory'('../../../'),
	'$lgt_change_directory'('ipc2008-no-cybersec/seq-opt/transport-strips/'),
	write('Testing problem set transport'), nl,
	test_colection,
	'$lgt_change_directory'('../../../'),
	'$lgt_change_directory'('ipc2008-no-cybersec/seq-opt/woodworking-strips/'),
	write('Testing problem set woodworking'), nl,
	test_colection,
	'$lgt_change_directory'('../../../').


test_colection:-
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

