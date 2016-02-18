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


To load this example and for sample queries, please see the `SCRIPT.txt`
file.

Example adapted from the chapter "Classifying Prototype-Based Programming
Languages" by Christophe Dony, Jacques Malenfant, and Daniel Bardou, found 
on the book "Prototype-Based Programming - Concepts, Languages, and 
Applications" published by Springer.


This prototype programming example illustrates how we can do both property 
sharing and value sharing in Logtalk by calling the built-in predicate 
modification methods `asserta/1`, `assertz/1`, and `retract/1` either in
the context of "this" or in the context of "self".

In this example we have a prototype, `joe_person`, containing general data
on Joe such as its age, name, or address, and four descendant prototypes
or viewpoints, `joe_sportsman`, `joe_employee`, `joe_chess_player`, and
`joe_film_enthusiast`. Each descendant contains data related to a particular
viewpoint about Joe.
