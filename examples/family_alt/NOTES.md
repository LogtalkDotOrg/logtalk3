________________________________________________________________________

This file is part of Logtalk <http://logtalk.org/>  
Copyright 1998-2017 Paulo Moura <pmoura@logtalk.org>

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


To load this example and for sample queries, please see the `SCRIPT.txt`
file.

This folder contains an alternative implementation of the `family` example,
which in turn is a version of the classical family tree Prolog example.

This alternative solution defines a `family/1` parametric object, providing
predicates for common family relations like `sister/2` and `father/2`. This
parametric object allows concrete families to be plugged-in by defining the 
basic female, male, and parent relations as multifile predicates parameterized
by the family database object. Several family objects can be loaded at the
same time. Using a parametric object to query a family extended relations is
convenient as the object parameter allows us to make name of the concrete
family easily available to any predicate.
