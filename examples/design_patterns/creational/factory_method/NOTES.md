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


Design pattern:
	Factory Method

Description:
	"Define an interface for creating an object, but let subclasses
	decide which class to instantiate. Factory Method lets a class
	defer instantiation to subclasses."

This pattern can be used with both classes and prototypes.

The sample implementation uses prototypes for simplicity with categories
playing a similar role to abstract classes.
