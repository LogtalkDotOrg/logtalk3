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

This folder contains a Logtalk port of ToyCHR, a reference implementation
of Constraint Handling Rules (CHR) available
from:

	https://www.comp.nus.edu.sg/~gregory/toychr/

The port is work in progress and includes significant modifications to the
original code:

- Instead of compiling `.chr` files, it uses the term-expansion mechanism,
by defining `toychrdb` as a hook object, to support writing rules inside
objects and categories. As a consequence, the original `chr_compile/1` is
not available.

- The port is portable and should run on all supported backends.
