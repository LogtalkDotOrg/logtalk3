________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>

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

This folder contains a multi-threading implementation of the merge sort 
algorithm.  Depending on the size of the lists that are ordered, using 
only one thread can be faster. The number of threads to be use in sorting 
is set using the `msort/1` object parameter. You may need to adjust the 
size of the memory areas used by your Prolog compiler, depending on the 
size of the lists you want to sort.

This example uses a simple implementation of the merge sort algorithm,
intended only to illustrate Logtalk multi-threading features. For any 
other purpose, you may find the following paper a worthwhile reading:

@incollection{ apt93modular,
    author = "Krzysztof R. Apt and Dino Pedreschi",
    title = "Modular Termination Proofs for Logic and Pure Prolog Programs.",
    booktitle = "116",
    month = "31",
    publisher = "Centrum voor Wiskunde en Informatica (CWI)",
    address = "ISSN 0169-118X",
    pages = "35",
    year = "1993",
    url = "citeseer.ist.psu.edu/apt93modular.html" }

You probably want to play with the list sizes in order to find out when the 
lists to be sorted are big enough to make the use of multi-threading worth
performance-wise (i.e. to compensate the overhead of thread creation and 
management).

The implementation of the Quicksort algorithm makes the possible performance 
gains due to the use of multi-threading highly dependent on the pivots used 
for vector partition. Increasing the number of threads alleviates the problem 
provided an adequate number of processing cores.
