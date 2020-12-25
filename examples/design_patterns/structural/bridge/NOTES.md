________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>

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
	Bridge

Description:
	"Decouple an abstraction from its implementation so that the two
	can vary independently."

This pattern can be used with both classes and prototypes.

In this sample implementation, we use classes for representing the
abstraction part of the pattern and prototypes for the implementation
part. As discussed in the pattern description, the instances in the
abstraction hierarchy use object composition to refer to and delegate
operations to the implementation hierarchy objects.
