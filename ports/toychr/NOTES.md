________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
Copyright 2003 Gregory J. Duck
Copyright 2019 Paulo Moura <pmoura@logtalk.org>
SPDX-License-Identifier: GPL-2.0-or-later

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
________________________________________________________________________


To load this port and for sample queries, please see the `SCRIPT.txt`
file.

This folder contains a Logtalk port of ToyCHR, a reference implementation
of Constraint Handling Rules (CHR) available from:

	https://www.comp.nus.edu.sg/~gregory/toychr/

The port is work in progress and includes significant modifications to the
original code:

- Instead of compiling `.chr` files, it uses the term-expansion mechanism,
by defining `toychrdb` as a hook object, to support writing rules inside
objects and categories. As a consequence, the original `chr_compile/1` is
not available.

- The port is portable and should run on all supported backends.

The port also includes examples ported from the SWI-Prolog CHR package
examples and documentation. These examples are ported using the same
license of the original code (BSD-2-Clause).
