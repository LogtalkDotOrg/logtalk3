#!/usr/bin/env bash

#############################################################################
## 
##   Distribution clean script for packaging
##   Last updated on April 9, 2019
## 
##   This file is part of Logtalk <https://logtalk.org/>  
##   Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
##   SPDX-License-Identifier: Apache-2.0
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

find . -name .git -print0 | xargs -0 rm -rf
find . -name .svn -print0 | xargs -0 rm -rf
find . -name CVS -print0 | xargs -0 rm -rf
find . -name .cvsignore -print0 | xargs -0 rm -f
find . -name .gitignore -print0 | xargs -0 rm -f
find . -name '.#*' -print0 | xargs -0 rm -f
find . -name .DS_Store -print0 | xargs -0 rm -f
find . -name '.gdb*' -print0 | xargs -0 rm -f
find . -name .pl-history -print0 | xargs -0 rm -f
find . -name .lgt_tmp -print0 | xargs -0 rm -rf

find . -type f -print0 | xargs -0 chmod 644
find . -type d -print0 | xargs -0 chmod 755

chmod a+x integration/*.sh
chmod a+x integration/unsupported/*.sh
chmod a+x manuals/sources/*.sh
chmod a+x scripts/*.sh
chmod a-x scripts/*.js
chmod a+x scripts/embedding/eclipse/*.sh
chmod a+x scripts/embedding/gprolog/*.sh
chmod a+x scripts/embedding/jiprolog/*.sh
chmod a+x scripts/embedding/sicstus/*.sh
chmod a+x scripts/embedding/swipl/*.sh
chmod a+x scripts/embedding/xsb/*.sh
chmod a+x scripts/embedding/yap/*.sh
chmod a+x scripts/debian/postinst
chmod a+x scripts/debian/prerm
chmod a+x scripts/debian/postrm
chmod a+x scripts/docker/swi-prolog/*.sh
chmod a+x scripts/linux/*.sh
chmod a+x scripts/macos/postflight
chmod a+x scripts/macos/command_files/*.command
chmod a+x tools/diagrams/*.sh
chmod a+x tools/diagrams/*.js
chmod a+x tools/lgtdoc/xml/*.sh
chmod a-x tools/lgtdoc/xml/*.js
