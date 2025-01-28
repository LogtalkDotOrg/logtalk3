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

# lpa - timetables

This example is an adaptation of the LPA Prolog++ timetables example.

Start by loading the example and the required library files:

```logtalk
logtalk_load(lpa_timetables(loader)).
```

The setup phase initializes the timetable:

```logtalk
timetable::setup.
```

<!--
true.
-->

```logtalk
timetable::make(2).
```

<!--
+ first_year - p1 - nicky - french
+ first_year - p2 - nicky - biology
+ first_year - p3 - brian - maths
+ first_year - p4 - brian - music
+ first_year - p5 - clive - prolog
+ second_year - p1 - brian - maths
+ second_year - p2 - brian - music
+ second_year - p3 - nicky - french
+ second_year - p4 - nicky - biology
+ second_year - p5 - diane - accountancy
+ third_year - p1 - dave - maths
+ third_year - p2 - clive - french
+ third_year - p3 - clive - prolog
+ third_year - p4 - diane - accountancy
+ third_year - p5 - nicky - biology
+ fourth_year - p1 - clive - french
+ fourth_year - p2 - dave - maths
+ fourth_year - p3 - diane - accountancy
+ fourth_year - p4 - clive - prolog
+ fourth_year - p5 - brian - music

true.
-->

The partially completed timetable is ...

```logtalk
timetable::print.
```

<!--
FORM TIMETABLE...

FORM: first_year
p1: nicky teaches french
p2: nicky teaches biology
p3: brian teaches maths
p4: brian teaches music
p5: clive teaches prolog

FORM: second_year
p1: brian teaches maths
p2: brian teaches music
p3: nicky teaches french
p4: nicky teaches biology
p5: diane teaches accountancy

FORM: third_year
p1: dave teaches maths
p2: clive teaches french
p3: clive teaches prolog
p4: diane teaches accountancy
p5: nicky teaches biology

FORM: fourth_year
p1: clive teaches french
p2: dave teaches maths
p3: diane teaches accountancy
p4: clive teaches prolog
p5: brian teaches music



PERIOD TIMETABLE ...

PERIOD: p1
first_year: nicky teaches french
second_year: brian teaches maths
third_year: dave teaches maths
fourth_year: clive teaches french

PERIOD: p2
first_year: nicky teaches biology
second_year: brian teaches music
third_year: clive teaches french
fourth_year: dave teaches maths

PERIOD: p3
first_year: brian teaches maths
second_year: nicky teaches french
third_year: clive teaches prolog
fourth_year: diane teaches accountancy

PERIOD: p4
first_year: brian teaches music
second_year: nicky teaches biology
third_year: diane teaches accountancy
fourth_year: clive teaches prolog

PERIOD: p5
first_year: clive teaches prolog
second_year: diane teaches accountancy
third_year: nicky teaches biology
fourth_year: brian teaches music



TEACHER TIMETABLE ...

TEACHER: nicky
p1: teach french to first_year
p2: teach biology to first_year
p3: teach french to second_year
p4: teach biology to second_year
p5: teach biology to third_year

TEACHER: brian
p1: teach maths to second_year
p2: teach music to second_year
p3: teach maths to first_year
p4: teach music to first_year
p5: teach music to fourth_year

TEACHER: dave
p1: teach maths to third_year
p2: teach maths to fourth_year
p3:
p4:
p5:

TEACHER: clive
p1: teach french to fourth_year
p2: teach french to third_year
p3: teach prolog to third_year
p4: teach prolog to fourth_year
p5: teach prolog to first_year

TEACHER: diane
p1:
p2:
p3: teach accountancy to fourth_year
p4: teach accountancy to third_year
p5: teach accountancy to second_year

TEACHER: phil
p1:
p2:
p3:
p4:
p5:



SUBJECT TIMETABLE ...

SUBJECT: maths
p1: second_year taught by brian
p1: third_year taught by dave
p2: fourth_year taught by dave
p3: first_year taught by brian

SUBJECT: music
p2: second_year taught by brian
p4: first_year taught by brian
p5: fourth_year taught by brian

SUBJECT: french
p1: first_year taught by nicky
p1: fourth_year taught by clive
p2: third_year taught by clive
p3: second_year taught by nicky

SUBJECT: prolog
p3: third_year taught by clive
p4: fourth_year taught by clive
p5: first_year taught by clive

SUBJECT: biology
p2: first_year taught by nicky
p4: second_year taught by nicky
p5: third_year taught by nicky

SUBJECT: prolog++

SUBJECT: accountancy
p3: fourth_year taught by diane
p4: third_year taught by diane
p5: second_year taught by diane

true.
-->

```logtalk
timetable::make(5).
```

<!--
+ first_year - p1 - diane - accountancy
+ first_year - p2 - phil - prolog++
+ second_year - p1 - phil - prolog++
Swap subject... 
- third_year - p4 - diane - accountancy
+ third_year - p4 - phil - prolog++
Swap teacher... 
- third_year - p2 - clive - french
+ third_year - p2 - diane - accountancy
Swap teacher... 
- second_year - p2 - brian - music
+ second_year - p2 - clive - prolog
Swap teacher... 
Swap subject... 
- third_year - p2 - diane - accountancy
+ third_year - p2 - brian - music
Swap teacher... 
- third_year - p2 - brian - music
+ third_year - p2 - diane - accountancy
Swap teacher... 
- second_year - p2 - clive - prolog
+ second_year - p2 - brian - music
Swap teacher... 
Swap teacher... 
- second_year - p2 - brian - music
+ second_year - p2 - clive - prolog
Swap teacher... 
- second_year - p2 - clive - prolog
+ second_year - p2 - brian - music
Swap teacher... 
Swap teacher... 
- second_year - p2 - brian - music
+ second_year - p2 - clive - prolog
Swap teacher... 
- second_year - p2 - clive - prolog
+ second_year - p2 - brian - music
Swap teacher... 
Swap teacher... 
- second_year - p2 - brian - music
+ second_year - p2 - clive - prolog
Swap teacher... 
Swap teacher... 
- second_year - p2 - clive - prolog
+ second_year - p2 - brian - music
Swap teacher... 
- second_year - p2 - brian - music
+ second_year - p2 - clive - prolog
Swap teacher... 
Swap teacher... 
- second_year - p2 - clive - prolog
+ second_year - p2 - brian - music
Swap teacher... 
- second_year - p2 - brian - music
+ second_year - p2 - clive - prolog
Swap teacher... 
Swap teacher... 
- second_year - p2 - clive - prolog
+ second_year - p2 - brian - music
Swap teacher... 
- second_year - p2 - brian - music
+ second_year - p2 - clive - prolog
Swap teacher... 
Swap teacher... 
- second_year - p2 - clive - prolog
+ second_year - p2 - brian - music
Swap teacher... 
- second_year - p2 - brian - music
+ second_year - p2 - clive - prolog
Swap teacher... 
Swap teacher... 
- second_year - p2 - clive - prolog
+ second_year - p2 - brian - music
Swap teacher... 
- second_year - p2 - brian - music
+ second_year - p2 - clive - prolog
Swap teacher... 
Swap teacher... 
- second_year - p2 - clive - prolog
+ second_year - p2 - brian - music
Swap subject... 
- third_year - p2 - diane - accountancy
+ third_year - p2 - clive - french
Swap subject... 
- third_year - p4 - phil - prolog++
+ third_year - p4 - diane - accountancy
Swap teacher... 
- third_year - p3 - clive - prolog
+ third_year - p3 - phil - prolog++
Swap subject... 
Swap subject... 
Swap subject... 
- third_year - p3 - phil - prolog++
+ third_year - p3 - clive - prolog
Swap subject... 
- third_year - p4 - diane - accountancy
+ third_year - p4 - phil - prolog++
Swap teacher... 
- third_year - p2 - clive - french
+ third_year - p2 - diane - accountancy
+ second_year - p2 - clive - prolog
Swap subject... 
- fourth_year - p3 - diane - accountancy
+ fourth_year - p3 - phil - prolog++
Swap teacher... 
- fourth_year - p4 - clive - prolog
+ fourth_year - p4 - diane - accountancy
Swap subject... 
- third_year - p4 - phil - prolog++
+ third_year - p4 - clive - french
Swap teacher... 
- third_year - p3 - clive - prolog
+ third_year - p3 - phil - prolog++
Swap subject... 
- fourth_year - p3 - phil - prolog++
+ fourth_year - p3 - clive - prolog
Swap teacher... 
Swap teacher... 
Swap subject... 
- fourth_year - p4 - diane - accountancy
+ fourth_year - p4 - phil - prolog++
Swap teacher... 
- fourth_year - p3 - clive - prolog
+ fourth_year - p3 - diane - accountancy
Swap subject... 
- fourth_year - p3 - diane - accountancy
+ fourth_year - p3 - clive - prolog
Swap teacher... 
- fourth_year - p3 - clive - prolog
+ fourth_year - p3 - diane - accountancy
Swap subject... 
- third_year - p3 - phil - prolog++
+ third_year - p3 - clive - prolog
Swap teacher... 
- third_year - p5 - nicky - biology
+ third_year - p5 - phil - prolog++
Swap teacher... 
- fourth_year - p5 - brian - music
+ fourth_year - p5 - nicky - biology
Swap teacher... 
Swap teacher... 
- fourth_year - p5 - nicky - biology
+ fourth_year - p5 - brian - music
Swap teacher... 
- fourth_year - p5 - brian - music
+ fourth_year - p5 - nicky - biology
Swap teacher... 
Swap teacher... 
- fourth_year - p5 - nicky - biology
+ fourth_year - p5 - brian - music
Swap teacher... 
- fourth_year - p5 - brian - music
+ fourth_year - p5 - nicky - biology
Swap teacher... 
Swap teacher... 
- fourth_year - p5 - nicky - biology
+ fourth_year - p5 - brian - music
Swap teacher... 
- fourth_year - p5 - brian - music
+ fourth_year - p5 - nicky - biology
+ third_year - p5 - brian - music

true.
-->

The completed timetable is ...

```logtalk
timetable::print.
```

<!--
FORM TIMETABLE...

FORM: first_year
p1: nicky teaches french
p2: nicky teaches biology
p3: brian teaches maths
p4: brian teaches music
p5: clive teaches prolog

FORM: second_year
p1: brian teaches maths
p2: brian teaches music
p3: nicky teaches french
p4: nicky teaches biology
p5: diane teaches accountancy

FORM: third_year
p1: dave teaches maths
p2: diane teaches accountancy
p3: clive teaches prolog
p4: clive teaches french
p5: phil teaches prolog++

FORM: fourth_year
p1: clive teaches french
p2: dave teaches maths
p3: diane teaches accountancy
p4: phil teaches prolog++
p5: nicky teaches biology



PERIOD TIMETABLE ...

PERIOD: p1
first_year: nicky teaches french
second_year: brian teaches maths
third_year: dave teaches maths
fourth_year: clive teaches french

PERIOD: p2
first_year: nicky teaches biology
second_year: brian teaches music
third_year: diane teaches accountancy
fourth_year: dave teaches maths

PERIOD: p3
first_year: brian teaches maths
second_year: nicky teaches french
third_year: clive teaches prolog
fourth_year: diane teaches accountancy

PERIOD: p4
first_year: brian teaches music
second_year: nicky teaches biology
third_year: clive teaches french
fourth_year: phil teaches prolog++

PERIOD: p5
first_year: clive teaches prolog
second_year: diane teaches accountancy
third_year: phil teaches prolog++
fourth_year: nicky teaches biology



TEACHER TIMETABLE ...

TEACHER: nicky
p1: teach french to first_year
p2: teach biology to first_year
p3: teach french to second_year
p4: teach biology to second_year
p5: teach biology to fourth_year

TEACHER: brian
p1: teach maths to second_year
p2: teach music to second_year
p3: teach maths to first_year
p4: teach music to first_year
p5: teach music to third_year

TEACHER: dave
p1: teach maths to third_year
p2: teach maths to fourth_year
p3:
p4:
p5:

TEACHER: clive
p1: teach french to fourth_year
p2: teach prolog to second_year
p3: teach prolog to third_year
p4: teach french to third_year
p5: teach prolog to first_year

TEACHER: diane
p1: teach accountancy to first_year
p2: teach accountancy to third_year
p3: teach accountancy to fourth_year
p4:
p5: teach accountancy to second_year

TEACHER: phil
p1: teach prolog++ to second_year
p2: teach prolog++ to first_year
p3:
p4: teach prolog++ to fourth_year
p5: teach prolog++ to third_year



SUBJECT TIMETABLE ...

SUBJECT: maths
p1: second_year taught by brian
p1: third_year taught by dave
p2: fourth_year taught by dave
p3: first_year taught by brian

SUBJECT: music
p2: second_year taught by brian
p4: first_year taught by brian
p5: third_year taught by brian

SUBJECT: french
p1: first_year taught by nicky
p1: fourth_year taught by clive
p3: second_year taught by nicky
p4: third_year taught by clive

SUBJECT: prolog
p2: second_year taught by clive
p3: third_year taught by clive
p5: first_year taught by clive

SUBJECT: biology
p2: first_year taught by nicky
p4: second_year taught by nicky
p5: fourth_year taught by nicky

SUBJECT: prolog++
p1: second_year taught by phil
p2: first_year taught by phil
p4: fourth_year taught by phil
p5: third_year taught by phil

SUBJECT: accountancy
p1: first_year taught by diane
p2: third_year taught by diane
p3: fourth_year taught by diane
p5: second_year taught by diane

true.
-->
