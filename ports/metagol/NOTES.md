________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  

Copyright 2016 Metagol authors  
Copyright 2018-2019 Paulo Moura  
All rights reserved.  
SPDX-License-Identifier: BSD-3-Clause

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimer.

* Redistributions in binary form must reproduce the above copyright notice,
  this list of conditions and the following disclaimer in the documentation
  and/or other materials provided with the distribution.

* Neither the name of the copyright holder nor the names of its
  contributors may be used to endorse or promote products derived from
  this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
________________________________________________________________________


`metagol`
=========

To load this port and for sample queries, please see the `SCRIPT.txt` file.
To run the tests, load the `tester.lgt` file (there are three lengthy tests
that only run when the tests are being run manually instead of automatically).

This folder contains a Logtalk port of `metagol`, an inductive logic
programming (ILP) system based on meta-interpretive learning available
from:

	https://github.com/metagol/metagol

See the original code git repo for details and bibliography on Metagol
and ILP.

The port allows any number of datasets to be loaded simultaneously with
per-dataset learning options. A dataset is simply wrapped in an object
that extends and is expanded by the `metagol` object as illustrated by
the ported examples.

Both the original code and the port requires the coroutining `when/2`
predicate, which is only available in some backend Prolog systems.
The port currently supports ECLiPSe, LVM, SICStus Prolog, SWI-Prolog,
and YAP. It can be used on both POSIX and Windows operating-systems.

The examples are ported from the original Metagol distribution. Some of
the examples are taken from the following paper (with the original Prolog
examples source code files made available by MystikNinja):

	@article{DBLP:journals/jair/EvansG18,
		author    = {Richard Evans and Edward Grefenstette},
		title     = {Learning Explanatory Rules from Noisy Data},
		journal   = {J. Artif. Intell. Res.},
		volume    = {61},
		pages     = {1--64},
		year      = {2018},
		url       = {https://doi.org/10.1613/jair.5714},
		doi       = {10.1613/jair.5714},
		timestamp = {Mon, 21 Jan 2019 15:01:17 +0100},
		biburl    = {https://dblp.org/rec/bib/journals/jair/EvansG18},
		bibsource = {dblp computer science bibliography, https://dblp.org}
	}

The paper can be downloaded at

	https://arxiv.org/pdf/1711.04574.pdf
