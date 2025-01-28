---
jupyter:
  jupytext:
    text_representation:
      extension: .md
      format_name: markdown
      format_version: '1.1'
      jupytext_version: 1.16.6
  kernelspec:
    display_name: Logtalk
    language: logtalk
    name: logtalk_kernel
---

<!--
________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
SPDX-FileCopyrightText: 1998-2025 Paulo Moura <pmoura@logtalk.org>  
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
-->

# birds

This folder contains an example of a bird identification expert system
adapted with permission from the book "Adventure in Prolog" by Amzi! Inc.
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

Start by loading the example and the required library files:

```logtalk
logtalk_load(birds(loader)).
...


Ask the expert system for help in identifying a bird:

```logtalk
expert::identify.
```

<!--
Bird identification expert system

bill:sharp_hooked? (yes or no): yes.
eats:birds? (yes or no): yes.
feet:curved_talons? (yes or no): yes.
head:large? (yes or no): yes.

What is the value for tail?
1 : narrow_at_tip
2 : forked
3 : long_rusty
4 : square
5 : other
Enter the number of choice> 1.

wings:long_pointed? (yes or no): yes.

Possible identification : peregrine_falcon

No (more) candidates found.

true.
-->

Identify another bird:

```logtalk
expert::identify.
```

<!--
Bird identification expert system

bill:sharp_hooked? (yes or no): no.
bill:flat? (yes or no): no.
bill:short? (yes or no): no.
bill:hooked? (yes or no): yes.

What is the value for flight?
1 : ponderous
2 : powerful
3 : agile
4 : flap_glide
5 : other
Enter the number of choice> 2.

color:dark? (yes or no): yes.
live:at_sea? (yes or no): yes.
nostrils:external_tubular? (yes or no): yes.

What is the value for size?
1 : large
2 : plump
3 : medium
4 : small
Enter the number of choice> 1.

wings:long_narrow? (yes or no): yes.

Possible identification : black_footed_albatross

No (more) candidates found.

true.
-->

