________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>  
SPDX-License-Identifier: Apache-2.0

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

This folder contains Logtalk versions of two examples of network modeling
for recognizing polyhedra represented as graphs as described in the following
paper:

	@inproceedings{Markov1989AFF,
		title={A Framework for Network Modeling in Prolog},
		author={Z. I. Markov},
		booktitle={IJCAI},
		year={1989}
	}

A copy of the paper can be downloaded from:

https://www.ijcai.org/Proceedings/89-1/Papers/013.pdf

The Logtalk version uses parametric objects to represent the concept of
"net-variables" described in the paper as "global logical variables" and
the coroutining library to approximate the semantics of the original
examples. The object parameter variables provide object "global logical
variables". The implicit use of the `dif/2` constraint is mentioned in
the paper third page: "(The use of a special Prolog extension, ensuring
all different variables to be bound to different objects, is essential
in this example. Such an extension is also available in Prolog III.)".

Note that the use of the `dif` library limits the backend Prolog systems
that can be used to run this example to B-Prolog, ECLiPSe, LVM, SICStus
Prolog, Scryer Prolog, SWI-Prolog, Trealla Prolog, XSB, and YAP.
