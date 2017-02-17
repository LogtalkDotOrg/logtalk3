#!/usr/bin/env bash

#############################################################################
## 
##   This file is part of Logtalk <http://logtalk.org/>  
##   Copyright 1998-2017 Paulo Moura <pmoura@logtalk.org>
## 
##   User Manual XHTML to PDF conversion script
##   Last updated on February 17, 2017
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

css2xslfo=/Applications/XML/CSSToXSLFO/css2xslfo1_6_2.jar

xslt_proc="xsltproc -path /opt/local/share/xml/xhtml -nonet"
# xslt_proc=xalan
# xslt_proc=sabcmd

#fo_proc=fop
fo_proc=xep
# fo_proc=xinc

# this script assumes a catalog file with a single line:
#	PUBLIC "-//W3C//DTD XHTML 1.1//EN" "xhtml11.dtd"
#
# the "xhtml11.dtd" file must be a renamed copy of the file:
#	http://www.w3.org/TR/xhtml11/DTD/xhtml11-flat.dtd

catalog=file:///opt/local/share/xml/xhtml/catalog

rm -f userman.fo userman.html
rm -fr *.section
eval $xslt_proc -o index.section userman.xsl index.html
eval $xslt_proc -o features.section userman.xsl features.html
eval $xslt_proc -o nomenclature.section userman.xsl nomenclature.html
eval $xslt_proc -o messages.section userman.xsl messages.html
eval $xslt_proc -o objects.section userman.xsl objects.html
eval $xslt_proc -o protocols.section userman.xsl protocols.html
eval $xslt_proc -o categories.section userman.xsl categories.html
eval $xslt_proc -o predicates.section userman.xsl predicates.html
eval $xslt_proc -o inheritance.section userman.xsl inheritance.html
eval $xslt_proc -o events.section userman.xsl events.html
eval $xslt_proc -o threads.section userman.xsl threads.html
eval $xslt_proc -o errors.section userman.xsl errors.html
eval $xslt_proc -o documenting.section userman.xsl documenting.html
eval $xslt_proc -o installing.section userman.xsl installing.html
eval $xslt_proc -o programming.section userman.xsl programming.html
eval $xslt_proc -o migration.section userman.xsl migration.html

cat -s \
	userman.header \
	index.section \
	userman.body \
	features.section nomenclature.section messages.section \
	objects.section protocols.section categories.section \
	predicates.section inheritance.section events.section \
	threads.section errors.section documenting.section \
	installing.section programming.section migration.section \
	userman.footer \
	> userman.html

java -jar $css2xslfo -c $catalog userman.html -fo userman.fo

if [ "$fo_proc" == "xep" ]; then
	eval $fo_proc -valid -fo userman.fo -pdf userman.pdf
else
	eval $fo_proc -fo userman.fo -pdf userman.pdf
fi

rm userman.fo userman.html
rm -fr *.section
mv userman.pdf ..

