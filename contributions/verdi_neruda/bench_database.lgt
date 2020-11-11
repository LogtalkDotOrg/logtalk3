
:- object(bench_database,
	implements(databasep)).

	:- info([
		version is 1:0:0,
		author is 'Victor Lagerkvist',
		date is 2010-06-13,
		comment is 'A simple database for finding related regions using world countries data.'
	]).

	query([C1,D1,C2,D2]) if
		density(C1,D1) and
		density(C2,D2) and
		{D1 > D2} and
		{T1 is 20*D1} and
		{T2 is 21*D2} and
		{T1 < T2}.

	density(C,D) if
		pop(C,P) and
		area(C,A) and
		{D is (P*100)//A}.

	% populations in 100000's
	pop(china,	8250)if true.
	pop(india,	5863)if true.
	pop(ussr,	2521)if true.
	pop(usa,	2119)if true.
	pop(indonesia,	1276)if true.
	pop(japan,	1097)if true.
	pop(brazil,	1042)if true.
	pop(bangladesh,	 750)if true.
	pop(pakistan,	 682)if true.
	pop(w_germany,	 620)if true.
	pop(nigeria,	 613)if true.
	pop(mexico,	 581)if true.
	pop(uk,		 559)if true.
	pop(italy,	 554)if true.
	pop(france,	 525)if true.
	pop(philippines, 415)if true.
	pop(thailand,	 410)if true.
	pop(turkey,	 383)if true.
	pop(egypt,	 364)if true.
	pop(spain,	 352)if true.
	pop(poland,	 337)if true.
	pop(s_korea,	 335)if true.
	pop(iran,	 320)if true.
	pop(ethiopia,	 272)if true.
	pop(argentina,	 251)if true.

	% areas in 1000's of square miles
	area(china,	 3380)if true.
	area(india,	 1139)if true.
	area(ussr,	  8708)if true.
	area(usa,	   3609)if true.
	area(indonesia,	 570)if true.
	area(japan,	  148)if true.
	area(brazil,	3288)if true.
	area(bangladesh,  55)if true.
	area(pakistan,	 311)if true.
	area(w_germany,	  96)if true.
	area(nigeria,	373)if true.
	area(mexico,	 764)if true.
	area(uk,		  86)if true.
	area(italy,	  116)if true.
	area(france,	 213)if true.
	area(philippines, 90)if true.
	area(thailand,	 200)if true.
	area(turkey,	 296)if true.
	area(egypt,	  386)if true.
	area(spain,	  190)if true.
	area(poland,	 121)if true.
	area(s_korea,	 37)if true.
	area(iran,	   628)if true.
	area(ethiopia,	 350)if true.
	area(argentina, 1080)if true.

	bench_goal(query([ethiopia, 77, mexico, 76])).
	bench_goal(query([france, 246, iran, 628])).

:- end_object.
