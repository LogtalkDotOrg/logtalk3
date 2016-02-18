________________________________________________________________________

This file is part of Logtalk <http://logtalk.org/>  
Copyright 1998-2016 Paulo Moura <pmoura@logtalk.org>

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
________________________________________________________________________


To load this example and for sample queries, please see the `SCRIPT.txt` file.
This example requires a recent version of CxProlog, ECLiPSE, SICStus Prolog,
SWI-Prolog, or YAP as the back-end Prolog compiler.

Note that Logtalk support for coinduction is still experimental. The two major
issues are lack of robust Prolog support for cyclic terms and lack of support
for tabling of cyclic terms. The first issue prevents using of some back-end
Prolog compilers. The second issue may prevent some coinductive predicates to
behave as (theoretically) expected.

This folder contains several examples of coinductive predicates, adapted from
the coinduction papers and from the Feliks Kluzniak's DRA meta-interpreter.

The unit tests are currently disabled when using CxProlog or ECLiPSE as the
backend compiler. Although some tests pass, all tests calling `bagof/3` don't
terminate until all available memory is exhausted due to the fragile support
for rational terms in these two Prolog compilers.

For more information see:

	@inproceedings{1778186,
		author = {Gupta, Gopal and Bansal, Ajay and Min, Richard and Simon, Luke and Mallya, Ajay},
		title = {Coinductive logic programming and its applications},
		booktitle = {Proceedings of the 23rd International Conference on Logic programming (ICLP)},
		year = {2007},
		isbn = {3-540-74608-0, 978-3-540-74608-9},
		pages = {27--44},
		location = {Porto, Portugal},
		publisher = {Springer-Verlag},
		address = {Berlin, Heidelberg},
	}

	@inproceedings{
		author = {Neda Saeedloei and Gopal Gupta},
		title = {Verifying Complex Continuous Real-Time Systems with Coinductive CLP(R)},
		booktitle = {Proceedings of the 19th Workshop on Logic-based methods in Programming Environments (WLPE)},
		year = {2009},
		location = {Pasadena, California, USA}
	}

	@inproceedings{saeedloei_et_al:LIPIcs:2010:2599,
		author ={Neda Saeedloei and Gopal Gupta},
		title = {Timed Definite Clause Omega-Grammars},
		booktitle ={Technical Communications of the 26th International Conference on Logic Programming},
		pages = {212--221},
		series = {Leibniz International Proceedings in Informatics (LIPIcs)},
		isbn = {978-3-939897-17-0},
		issn = {1868-8969},
		year = {2010},
		volume = {7},
		editor = {Manuel Hermenegildo and Torsten Schaub},
		publisher = {Schloss Dagstuhl--Leibniz-Zentrum fuer Informatik},
		address = {Dagstuhl, Germany},
		URL = {http://drops.dagstuhl.de/opus/volltexte/2010/2599},
		doi = {http://dx.doi.org/10.4230/LIPIcs.ICLP.2010.212},
		annote = {Keywords: Constraint Logic Programming over reals, Co-induction, Context-Free Grammars, Omega-Grammars}
	}

	@inproceedings{AnconaSAC12,
	  author = {Ancona, D.},
	  title = {Regular corecursion in {P}rolog},
	  booktitle = {A{CM} {S}ymposium on {A}pplied {C}omputing ({SAC} 2012)},
	  ftp = {ftp://ftp.disi.unige.it/person/AnconaD/AnconaSAC12.pdf},
	  keywords = {coinduction,corecursion},
	  year = 2012
	}
