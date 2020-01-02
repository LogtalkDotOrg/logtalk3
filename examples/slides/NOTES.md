________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
Copyright 1998-2020 Paulo Moura <pmoura@logtalk.org>

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

This is a simple example of using the library list zipper implementation
to implement a basic slideshow functionality allowing remote commands to
navigate to the next, previous, first, and last slide in a sequence of
slides. The slides are represented using a predicate with two arguments,
the index and the slide contents. The main predicate, `show/2` takes a
sequence of slides and the name of the predicate, which is used as a
closure for displaying the slides.
