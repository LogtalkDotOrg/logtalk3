________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>  
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


Design pattern:
	Composite

Description:
	"Compose objects into tree structures to represent part-whole
	hierarchies. Composite lets clients treat individual objects
	and compositions of objects uniformly."

This pattern can be used with both classes and prototypes.

In this sample implementation of the pattern, we choose to use classes
and object composition (i.e. composite objects will store the identifiers
of the objects that are part of the composite). In the sample queries,
we create the instances dynamically at runtime but we could also have
defined (some or all) of the instances statically in the source file.
