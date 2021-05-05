:- initialization((
	logtalk_load_context(directory, Directory),
	atom_concat(Directory, 'a/', ADirectory),
	assertz(logtalk_library_path(a, ADirectory)),
	atom_concat(Directory, 'b/', BDirectory),
	assertz(logtalk_library_path(b, BDirectory))
)).

:- initialization((
	logtalk_load(a(loader)),
	logtalk_load(b(loader))
)).
