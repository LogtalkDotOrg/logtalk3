________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  

Copyright 2016 Metagol authors
Copyright 2018-2019 Paulo Moura
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:

1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in
   the documentation and/or other materials provided with the
   distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.
________________________________________________________________________


To load this port and for sample queries, please see the `SCRIPT.txt`
file.

This folder contains a Logtalk port of `metagol`, an inductive logic
programming (ILP) system based on meta-interpretive learning available
from:

	https://github.com/metagol/metagol

The port allows any number of datasets to be loaded simultaneously with
per-dataset learning options. A dataset is simply wrapped in an object
that extends and is expanded by the `metagol` object.

Both the original code and the port requires the coroutining `when/2`
predicate, which is only available in some backend Prolog systems.
The port currently supports ECLiPSe, SICStus Prolog, SWI-Prolog, and
YAP.
