________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
Copyright 1998-2019 Paulo Moura <pmoura@logtalk.org>

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
 
This example illustrates a very simple solution for persisting an object
dynamic state across sessions. It uses a plain Prolog file for saving the
state and an include/1 directive to automatically restore the saved state
when the object is loaded. The saved state file is created if it doesn't
exist by the `loader.lgt` file before loading the example itself. See also
the `serialization` example.
