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


Design pattern:
	Treading State

Description:
	Definite Clause Grammars (DCGs) can be used to thread state
	whenever we have a sequence of calls where each call takes
	an input state and an output state as arguments with the
	output state of each call being passed to the next call as
	its input state. DCGs notation allows abstracting the state
	arguments and thus simplifying the code. 

The sample implementation converts a floating-point number into an
integer number using a sequence of operations represented using
Definite Clause Grammar rules.
