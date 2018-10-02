#!/usr/bin/env bash

#############################################################################
## 
##   Documentation build script
##   Last updated on October 2, 2018
## 
##   This file is part of Logtalk <https://logtalk.org/>  
##   Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>
##   
##   Licensed under the Apache License, Version 2.0 (the "License");
##   you may not use this file except in compliance with the License.
##   You may obtain a copy of the License at
##   
##       http://www.apache.org/licenses/LICENSE-2.0
##   
##   Unless required by applicable law or agreed to in writing, software
##   distributed under the License is distributed on an "AS IS" BASIS,
##   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
##   See the License for the specific language governing permissions and
##   limitations under the License.
## 
#############################################################################


# allow using this script from any directory
cd "$(dirname "$0")" || exit 1

rm -f ../TheLogtalkHandbook.pdf
rm -f ../TheLogtalkHandbook.epub
rm -rf ../_sources
rm -rf ../_static
rm -rf ../faq
rm -rf ../refman
rm -rf ../tutorial
rm -rf ../userman
make clean

make html
make latexpdf
make epub

mv -f _build/html/* ../
mv -f _build/latex/TheLogtalkHandbook.pdf ../
mv -f _build/epub/TheLogtalkHandbook.epub ../

make clean
