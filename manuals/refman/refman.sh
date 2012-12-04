#!/bin/bash

#############################################################################
## 
##   This file is part of Logtalk <http://logtalk.org/>  
##   Copyright (c) 1998-2012 Paulo Moura <pmoura@logtalk.org>
## 
##   Reference manual to PDF conversion script
## 
##   This program is free software: you can redistribute it and/or modify
##   it under the terms of the GNU General Public License as published by
##   the Free Software Foundation, either version 3 of the License, or
##   (at your option) any later version.
##   
##   This program is distributed in the hope that it will be useful,
##   but WITHOUT ANY WARRANTY; without even the implied warranty of
##   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##   GNU General Public License for more details.
##   
##   You should have received a copy of the GNU General Public License
##   along with this program.  If not, see <http://www.gnu.org/licenses/>.
##   
##   Additional licensing terms apply per Section 7 of the GNU General
##   Public License 3. Consult the `LICENSE.txt` file for details.
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

# rm -f refman.fo refman.html
rm -f *.section directives/*.section builtins/*.section methods/*.section control/*.section

eval $xslt_proc -o index.section refman.xsl index.html
eval $xslt_proc -o grammar.section refman.xsl grammar.html
eval $xslt_proc -o directives/encoding1.section refman.xsl directives/encoding1.html
eval $xslt_proc -o directives/set_logtalk_flag2.section refman.xsl directives/set_logtalk_flag2.html
eval $xslt_proc -o directives/if1.section refman.xsl directives/if1.html
eval $xslt_proc -o directives/elif1.section refman.xsl directives/elif1.html
eval $xslt_proc -o directives/else0.section refman.xsl directives/else0.html
eval $xslt_proc -o directives/endif0.section refman.xsl directives/endif0.html
eval $xslt_proc -o directives/calls1.section refman.xsl directives/calls1.html
eval $xslt_proc -o directives/category1_3.section refman.xsl directives/category1_3.html
eval $xslt_proc -o directives/dynamic0.section refman.xsl directives/dynamic0.html
eval $xslt_proc -o directives/end_category0.section refman.xsl directives/end_category0.html
eval $xslt_proc -o directives/end_object0.section refman.xsl directives/end_object0.html
eval $xslt_proc -o directives/end_protocol0.section refman.xsl directives/end_protocol0.html
eval $xslt_proc -o directives/info1.section refman.xsl directives/info1.html
eval $xslt_proc -o directives/initialization1.section refman.xsl directives/initialization1.html
eval $xslt_proc -o directives/multifile1.section refman.xsl directives/multifile1.html
eval $xslt_proc -o directives/object1_5.section refman.xsl directives/object1_5.html
eval $xslt_proc -o directives/protocol1_2.section refman.xsl directives/protocol1_2.html
eval $xslt_proc -o directives/synchronized0.section refman.xsl directives/synchronized0.html
eval $xslt_proc -o directives/threaded0.section refman.xsl directives/threaded0.html
eval $xslt_proc -o directives/uses1.section refman.xsl directives/uses1.html
eval $xslt_proc -o directives/alias3.section refman.xsl directives/alias3.html
eval $xslt_proc -o directives/coinductive1.section refman.xsl directives/coinductive1.html
eval $xslt_proc -o directives/discontiguous1.section refman.xsl directives/discontiguous1.html
eval $xslt_proc -o directives/dynamic1.section refman.xsl directives/dynamic1.html
eval $xslt_proc -o directives/info2.section refman.xsl directives/info2.html
eval $xslt_proc -o directives/meta_predicate1.section refman.xsl directives/meta_predicate1.html
eval $xslt_proc -o directives/meta_non_terminal1.section refman.xsl directives/meta_non_terminal1.html
eval $xslt_proc -o directives/annotation1.section refman.xsl directives/annotation1.html
eval $xslt_proc -o directives/mode2.section refman.xsl directives/mode2.html
eval $xslt_proc -o directives/op3.section refman.xsl directives/op3.html
eval $xslt_proc -o directives/private1.section refman.xsl directives/private1.html
eval $xslt_proc -o directives/protected1.section refman.xsl directives/protected1.html
eval $xslt_proc -o directives/public1.section refman.xsl directives/public1.html
eval $xslt_proc -o directives/synchronized1.section refman.xsl directives/synchronized1.html
eval $xslt_proc -o directives/uses2.section refman.xsl directives/uses2.html
eval $xslt_proc -o builtins/current_category1.section refman.xsl builtins/current_category1.html
eval $xslt_proc -o builtins/current_object1.section refman.xsl builtins/current_object1.html
eval $xslt_proc -o builtins/current_protocol1.section refman.xsl builtins/current_protocol1.html
eval $xslt_proc -o builtins/category_property2.section refman.xsl builtins/category_property2.html
eval $xslt_proc -o builtins/object_property2.section refman.xsl builtins/object_property2.html
eval $xslt_proc -o builtins/protocol_property2.section refman.xsl builtins/protocol_property2.html
eval $xslt_proc -o builtins/create_category4.section refman.xsl builtins/create_category4.html
eval $xslt_proc -o builtins/create_object4.section refman.xsl builtins/create_object4.html
eval $xslt_proc -o builtins/create_protocol3.section refman.xsl builtins/create_protocol3.html
eval $xslt_proc -o builtins/abolish_category1.section refman.xsl builtins/abolish_category1.html
eval $xslt_proc -o builtins/abolish_object1.section refman.xsl builtins/abolish_object1.html
eval $xslt_proc -o builtins/abolish_protocol1.section refman.xsl builtins/abolish_protocol1.html
eval $xslt_proc -o builtins/extends_object2_3.section refman.xsl builtins/extends_object2_3.html
eval $xslt_proc -o builtins/extends_protocol2_3.section refman.xsl builtins/extends_protocol2_3.html
eval $xslt_proc -o builtins/extends_category2_3.section refman.xsl builtins/extends_category2_3.html
eval $xslt_proc -o builtins/implements_protocol2_3.section refman.xsl builtins/implements_protocol2_3.html
eval $xslt_proc -o builtins/imports_category2_3.section refman.xsl builtins/imports_category2_3.html
eval $xslt_proc -o builtins/instantiates_class2_3.section refman.xsl builtins/instantiates_class2_3.html
eval $xslt_proc -o builtins/specializes_class2_3.section refman.xsl builtins/specializes_class2_3.html
eval $xslt_proc -o builtins/complements_object2.section refman.xsl builtins/complements_object2.html
eval $xslt_proc -o builtins/conforms_to_protocol2_3.section refman.xsl builtins/conforms_to_protocol2_3.html
eval $xslt_proc -o builtins/abolish_events5.section refman.xsl builtins/abolish_events5.html
eval $xslt_proc -o builtins/current_event5.section refman.xsl builtins/current_event5.html
eval $xslt_proc -o builtins/define_events5.section refman.xsl builtins/define_events5.html
eval $xslt_proc -o builtins/threaded1.section refman.xsl builtins/threaded1.html
eval $xslt_proc -o builtins/threaded_call1_2.section refman.xsl builtins/threaded_call1_2.html
eval $xslt_proc -o builtins/threaded_once1_2.section refman.xsl builtins/threaded_once1_2.html
eval $xslt_proc -o builtins/threaded_ignore1.section refman.xsl builtins/threaded_ignore1.html
eval $xslt_proc -o builtins/threaded_exit1_2.section refman.xsl builtins/threaded_exit1_2.html
eval $xslt_proc -o builtins/threaded_peek1_2.section refman.xsl builtins/threaded_peek1_2.html
eval $xslt_proc -o builtins/threaded_wait1.section refman.xsl builtins/threaded_wait1.html
eval $xslt_proc -o builtins/threaded_notify1.section refman.xsl builtins/threaded_notify1.html
eval $xslt_proc -o builtins/logtalk_compile1.section refman.xsl builtins/logtalk_compile1.html
eval $xslt_proc -o builtins/logtalk_compile2.section refman.xsl builtins/logtalk_compile2.html
eval $xslt_proc -o builtins/logtalk_load1.section refman.xsl builtins/logtalk_load1.html
eval $xslt_proc -o builtins/logtalk_load2.section refman.xsl builtins/logtalk_load2.html
eval $xslt_proc -o builtins/logtalk_library_path2.section refman.xsl builtins/logtalk_library_path2.html
eval $xslt_proc -o builtins/logtalk_load_context2.section refman.xsl builtins/logtalk_load_context2.html
eval $xslt_proc -o builtins/current_logtalk_flag2.section refman.xsl builtins/current_logtalk_flag2.html
eval $xslt_proc -o builtins/set_logtalk_flag2.section refman.xsl builtins/set_logtalk_flag2.html
eval $xslt_proc -o builtins/forall2.section refman.xsl builtins/forall2.html
eval $xslt_proc -o builtins/retractall1.section refman.xsl builtins/retractall1.html
eval $xslt_proc -o methods/parameter2.section refman.xsl methods/parameter2.html
eval $xslt_proc -o methods/self1.section refman.xsl methods/self1.html
eval $xslt_proc -o methods/sender1.section refman.xsl methods/sender1.html
eval $xslt_proc -o methods/this1.section refman.xsl methods/this1.html
eval $xslt_proc -o methods/current_op3.section refman.xsl methods/current_op3.html
eval $xslt_proc -o methods/current_predicate1.section refman.xsl methods/current_predicate1.html
eval $xslt_proc -o methods/predicate_property2.section refman.xsl methods/predicate_property2.html
eval $xslt_proc -o methods/abolish1.section refman.xsl methods/abolish1.html
eval $xslt_proc -o methods/asserta1.section refman.xsl methods/asserta1.html
eval $xslt_proc -o methods/assertz1.section refman.xsl methods/assertz1.html
eval $xslt_proc -o methods/clause2.section refman.xsl methods/clause2.html
eval $xslt_proc -o methods/retract1.section refman.xsl methods/retract1.html
eval $xslt_proc -o methods/retractall1.section refman.xsl methods/retractall1.html
eval $xslt_proc -o methods/callN.section refman.xsl methods/callN.html
eval $xslt_proc -o methods/ignore1.section refman.xsl methods/ignore1.html
eval $xslt_proc -o methods/once1.section refman.xsl methods/once1.html
eval $xslt_proc -o methods/not1.section refman.xsl methods/not1.html
eval $xslt_proc -o methods/catch3.section refman.xsl methods/catch3.html
eval $xslt_proc -o methods/throw1.section refman.xsl methods/throw1.html
eval $xslt_proc -o methods/bagof3.section refman.xsl methods/bagof3.html
eval $xslt_proc -o methods/findall3.section refman.xsl methods/findall3.html
eval $xslt_proc -o methods/forall2.section refman.xsl methods/forall2.html
eval $xslt_proc -o methods/setof3.section refman.xsl methods/setof3.html
eval $xslt_proc -o methods/before3.section refman.xsl methods/before3.html
eval $xslt_proc -o methods/after3.section refman.xsl methods/after3.html
eval $xslt_proc -o methods/call1.section refman.xsl methods/call1.html
eval $xslt_proc -o methods/phrase1.section refman.xsl methods/phrase1.html
eval $xslt_proc -o methods/phrase2.section refman.xsl methods/phrase2.html
eval $xslt_proc -o methods/phrase3.section refman.xsl methods/phrase3.html
eval $xslt_proc -o methods/expand_term2.section refman.xsl methods/expand_term2.html
eval $xslt_proc -o methods/term_expansion2.section refman.xsl methods/term_expansion2.html
eval $xslt_proc -o methods/expand_goal2.section refman.xsl methods/expand_goal2.html
eval $xslt_proc -o methods/goal_expansion2.section refman.xsl methods/goal_expansion2.html
eval $xslt_proc -o control/to_object2.section refman.xsl control/to_object2.html
eval $xslt_proc -o control/to_self1.section refman.xsl control/to_self1.html
eval $xslt_proc -o control/super1.section refman.xsl control/super1.html
eval $xslt_proc -o control/external1.section refman.xsl control/external1.html
eval $xslt_proc -o control/context2.section refman.xsl control/context2.html
eval $xslt_proc -o control/direct1.section refman.xsl control/direct1.html

cat -s \
	refman.header \
	index.section \
	refman.body \
	grammar.section \
	directives.header \
	directives/encoding1.section \
	directives/set_logtalk_flag2.section \
	directives/if1.section \
	directives/elif1.section \
	directives/else0.section \
	directives/endif0.section \
	directives/calls1.section \
	directives/category1_3.section \
	directives/dynamic0.section \
	directives/end_category0.section \
	directives/end_object0.section \
	directives/end_protocol0.section \
	directives/info1.section \
	directives/initialization1.section \
	directives/multifile1.section \
	directives/object1_5.section \
	directives/protocol1_2.section \
	directives/synchronized0.section \
	directives/threaded0.section \
	directives/uses1.section \
	directives/alias3.section \
	directives/coinductive1.section \
	directives/discontiguous1.section \
	directives/dynamic1.section \
	directives/info2.section \
	directives/meta_predicate1.section \
	directives/meta_non_terminal1.section \
	directives/annotation1.section \
	directives/mode2.section \
	directives/op3.section \
	directives/private1.section \
	directives/protected1.section \
	directives/public1.section \
	directives/synchronized1.section \
	directives/uses2.section \
	builtins.header \
	builtins/current_category1.section \
	builtins/current_object1.section \
	builtins/current_protocol1.section \
	builtins/category_property2.section \
	builtins/object_property2.section \
	builtins/protocol_property2.section \
	builtins/create_category4.section \
	builtins/create_object4.section \
	builtins/create_protocol3.section \
	builtins/abolish_category1.section \
	builtins/abolish_object1.section \
	builtins/abolish_protocol1.section \
	builtins/extends_object2_3.section \
	builtins/extends_protocol2_3.section \
	builtins/extends_category2_3.section \
	builtins/implements_protocol2_3.section \
	builtins/imports_category2_3.section \
	builtins/instantiates_class2_3.section \
	builtins/specializes_class2_3.section \
	builtins/complements_object2.section \
	builtins/conforms_to_protocol2_3.section \
	builtins/abolish_events5.section \
	builtins/current_event5.section \
	builtins/define_events5.section \
	builtins/threaded1.section \
	builtins/threaded_call1_2.section \
	builtins/threaded_once1_2.section \
	builtins/threaded_ignore1.section \
	builtins/threaded_exit1_2.section \
	builtins/threaded_peek1_2.section \
	builtins/threaded_wait1.section \
	builtins/threaded_notify1.section \
	builtins/logtalk_compile1.section \
	builtins/logtalk_compile2.section \
	builtins/logtalk_load1.section \
	builtins/logtalk_load2.section \
	builtins/logtalk_library_path2.section \
	builtins/logtalk_load_context2.section \
	builtins/current_logtalk_flag2.section \
	builtins/set_logtalk_flag2.section \
	builtins/forall2.section \
	builtins/retractall1.section \
	methods.header \
	methods/parameter2.section \
	methods/self1.section \
	methods/sender1.section \
	methods/this1.section \
	methods/current_op3.section \
	methods/current_predicate1.section \
	methods/predicate_property2.section \
	methods/abolish1.section \
	methods/asserta1.section \
	methods/assertz1.section \
	methods/clause2.section \
	methods/retract1.section \
	methods/retractall1.section \
	methods/callN.section \
	methods/ignore1.section \
	methods/once1.section \
	methods/not1.section \
	methods/catch3.section \
	methods/throw1.section \
	methods/bagof3.section \
	methods/findall3.section \
	methods/forall2.section \
	methods/setof3.section \
	methods/before3.section \
	methods/after3.section \
	methods/call1.section \
	methods/phrase1.section \
	methods/phrase2.section \
	methods/phrase3.section \
	methods/expand_term2.section \
	methods/term_expansion2.section \
	methods/expand_goal2.section \
	methods/goal_expansion2.section \
	control.header \
	control/to_object2.section \
	control/to_self1.section \
	control/super1.section \
	control/external1.section \
	control/context2.section \
	control/direct1.section \
	refman.footer \
	> refman.html

java -jar $css2xslfo -c $catalog refman.html -fo refman.fo

if [ "$fo_proc" == "xep" ]; then
	eval $fo_proc -valid -fo refman.fo -pdf refman.pdf
else
	eval $fo_proc -fo refman.fo -pdf refman.pdf
fi

rm refman.fo refman.html
rm -f *.section directives/*.section builtins/*.section methods/*.section control/*.section
