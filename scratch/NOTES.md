________________________________________________________________________

This file is part of Logtalk <http://logtalk.org/>  
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


This folder is used as a scratch folder for the temporary files generated
by the Logtalk compiler at startup. If you delete this folder by accident,
it will be recreated the next time you run Logtalk.

You may set the value of the `scratch_directory` flag to this folder if
you want to collect in the same location the Prolog temporary files that
are generated when compiling Logtalk source files (e.g. for embedding).
