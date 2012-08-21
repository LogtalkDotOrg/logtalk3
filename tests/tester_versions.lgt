
:- initialization((
	current_logtalk_flag(version, version(Generation, Release, Update)),
	write('*****         Logtalk version: '), write(Generation), write('.'), write(Release), write('.'), write(Update), nl,
	current_logtalk_flag(prolog_version, (Major, Minor, Patch)),
	write('*****         Prolog version: '), write(Major), write('.'), write(Minor), write('.'), write(Patch), nl
)).
