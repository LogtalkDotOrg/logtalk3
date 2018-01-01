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


To load this example and for sample queries, please see the `SCRIPT.txt`
file.

This folder contains an example of a bird identification expert system 
adapted with permission from the book "Adventure in Prolog" by Amzi! inc. 
The book is available on-line in HTML format at the URL:

	http://www.amzi.com

Please refer to the book for more information on the original example.


The bird identification hierarchy is organized as a prototype hierarchy
as follows:

	<order>
		<family>
			<bird>

	order
		falconiforms
			falcon
				peregrine_falcon
				sparrow_hawk
			vulture
				california_condor
				turkey_vulture
		passerformes
			flycatcher
				ash_throated_flycatcher
				great_crested_flycatcher
			swallow
				barn_swallow
				cliff_swallow
				purple_martin
		tubenose
			fulmar
			albatross
				black_footed_albatross
				laysan_albatross
		waterfowl
			duck
				female_mallard
				male_mallard
				pintail
			goose
				canada_goose
				snow_goose
			swan
				trumpeter_swan
				whistling_swan
