#!/bin/bash

#############################################################################
## 
##   This file is part of Logtalk <http://logtalk.org/>  
##   Copyright (c) 1998-2015 Paulo Moura <pmoura@logtalk.org>
## 
##   Reference Manual XHTML to PDF conversion script
##   Last updated on August 19, 2014
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
rm -f *.section directives/*.section predicates/*.section methods/*.section control/*.section

eval $xslt_proc -o index.section refman.xsl index.html
eval $xslt_proc -o grammar.section refman.xsl grammar.html
eval $xslt_proc -o directives/encoding_1.section refman.xsl directives/encoding_1.html
eval $xslt_proc -o directives/set_logtalk_flag_2.section refman.xsl directives/set_logtalk_flag_2.html
eval $xslt_proc -o directives/if_1.section refman.xsl directives/if_1.html
eval $xslt_proc -o directives/elif_1.section refman.xsl directives/elif_1.html
eval $xslt_proc -o directives/else_0.section refman.xsl directives/else_0.html
eval $xslt_proc -o directives/endif_0.section refman.xsl directives/endif_0.html
eval $xslt_proc -o directives/built_in_0.section refman.xsl directives/built_in_0.html
eval $xslt_proc -o directives/category_1_3.section refman.xsl directives/category_1_3.html
eval $xslt_proc -o directives/dynamic_0.section refman.xsl directives/dynamic_0.html
eval $xslt_proc -o directives/end_category_0.section refman.xsl directives/end_category_0.html
eval $xslt_proc -o directives/end_object_0.section refman.xsl directives/end_object_0.html
eval $xslt_proc -o directives/end_protocol_0.section refman.xsl directives/end_protocol_0.html
eval $xslt_proc -o directives/include_1.section refman.xsl directives/include_1.html
eval $xslt_proc -o directives/info_1.section refman.xsl directives/info_1.html
eval $xslt_proc -o directives/initialization_1.section refman.xsl directives/initialization_1.html
eval $xslt_proc -o directives/multifile_1.section refman.xsl directives/multifile_1.html
eval $xslt_proc -o directives/object_1_5.section refman.xsl directives/object_1_5.html
eval $xslt_proc -o directives/protocol_1_2.section refman.xsl directives/protocol_1_2.html
eval $xslt_proc -o directives/threaded_0.section refman.xsl directives/threaded_0.html
eval $xslt_proc -o directives/alias_2.section refman.xsl directives/alias_2.html
eval $xslt_proc -o directives/coinductive_1.section refman.xsl directives/coinductive_1.html
eval $xslt_proc -o directives/discontiguous_1.section refman.xsl directives/discontiguous_1.html
eval $xslt_proc -o directives/dynamic_1.section refman.xsl directives/dynamic_1.html
eval $xslt_proc -o directives/info_2.section refman.xsl directives/info_2.html
eval $xslt_proc -o directives/meta_predicate_1.section refman.xsl directives/meta_predicate_1.html
eval $xslt_proc -o directives/meta_non_terminal_1.section refman.xsl directives/meta_non_terminal_1.html
eval $xslt_proc -o directives/mode_2.section refman.xsl directives/mode_2.html
eval $xslt_proc -o directives/op_3.section refman.xsl directives/op_3.html
eval $xslt_proc -o directives/private_1.section refman.xsl directives/private_1.html
eval $xslt_proc -o directives/protected_1.section refman.xsl directives/protected_1.html
eval $xslt_proc -o directives/public_1.section refman.xsl directives/public_1.html
eval $xslt_proc -o directives/synchronized_1.section refman.xsl directives/synchronized_1.html
eval $xslt_proc -o directives/uses_2.section refman.xsl directives/uses_2.html
eval $xslt_proc -o directives/use_module_2.section refman.xsl directives/use_module_2.html
eval $xslt_proc -o predicates/current_category_1.section refman.xsl predicates/current_category_1.html
eval $xslt_proc -o predicates/current_object_1.section refman.xsl predicates/current_object_1.html
eval $xslt_proc -o predicates/current_protocol_1.section refman.xsl predicates/current_protocol_1.html
eval $xslt_proc -o predicates/category_property_2.section refman.xsl predicates/category_property_2.html
eval $xslt_proc -o predicates/object_property_2.section refman.xsl predicates/object_property_2.html
eval $xslt_proc -o predicates/protocol_property_2.section refman.xsl predicates/protocol_property_2.html
eval $xslt_proc -o predicates/create_category_4.section refman.xsl predicates/create_category_4.html
eval $xslt_proc -o predicates/create_object_4.section refman.xsl predicates/create_object_4.html
eval $xslt_proc -o predicates/create_protocol_3.section refman.xsl predicates/create_protocol_3.html
eval $xslt_proc -o predicates/abolish_category_1.section refman.xsl predicates/abolish_category_1.html
eval $xslt_proc -o predicates/abolish_object_1.section refman.xsl predicates/abolish_object_1.html
eval $xslt_proc -o predicates/abolish_protocol_1.section refman.xsl predicates/abolish_protocol_1.html
eval $xslt_proc -o predicates/extends_object_2_3.section refman.xsl predicates/extends_object_2_3.html
eval $xslt_proc -o predicates/extends_protocol_2_3.section refman.xsl predicates/extends_protocol_2_3.html
eval $xslt_proc -o predicates/extends_category_2_3.section refman.xsl predicates/extends_category_2_3.html
eval $xslt_proc -o predicates/implements_protocol_2_3.section refman.xsl predicates/implements_protocol_2_3.html
eval $xslt_proc -o predicates/imports_category_2_3.section refman.xsl predicates/imports_category_2_3.html
eval $xslt_proc -o predicates/instantiates_class_2_3.section refman.xsl predicates/instantiates_class_2_3.html
eval $xslt_proc -o predicates/specializes_class_2_3.section refman.xsl predicates/specializes_class_2_3.html
eval $xslt_proc -o predicates/complements_object_2.section refman.xsl predicates/complements_object_2.html
eval $xslt_proc -o predicates/conforms_to_protocol_2_3.section refman.xsl predicates/conforms_to_protocol_2_3.html
eval $xslt_proc -o predicates/abolish_events_5.section refman.xsl predicates/abolish_events_5.html
eval $xslt_proc -o predicates/current_event_5.section refman.xsl predicates/current_event_5.html
eval $xslt_proc -o predicates/define_events_5.section refman.xsl predicates/define_events_5.html
eval $xslt_proc -o predicates/threaded_1.section refman.xsl predicates/threaded_1.html
eval $xslt_proc -o predicates/threaded_call_1_2.section refman.xsl predicates/threaded_call_1_2.html
eval $xslt_proc -o predicates/threaded_once_1_2.section refman.xsl predicates/threaded_once_1_2.html
eval $xslt_proc -o predicates/threaded_ignore_1.section refman.xsl predicates/threaded_ignore_1.html
eval $xslt_proc -o predicates/threaded_exit_1_2.section refman.xsl predicates/threaded_exit_1_2.html
eval $xslt_proc -o predicates/threaded_peek_1_2.section refman.xsl predicates/threaded_peek_1_2.html
eval $xslt_proc -o predicates/threaded_wait_1.section refman.xsl predicates/threaded_wait_1.html
eval $xslt_proc -o predicates/threaded_notify_1.section refman.xsl predicates/threaded_notify_1.html
eval $xslt_proc -o predicates/logtalk_compile_1.section refman.xsl predicates/logtalk_compile_1.html
eval $xslt_proc -o predicates/logtalk_compile_2.section refman.xsl predicates/logtalk_compile_2.html
eval $xslt_proc -o predicates/logtalk_load_1.section refman.xsl predicates/logtalk_load_1.html
eval $xslt_proc -o predicates/logtalk_load_2.section refman.xsl predicates/logtalk_load_2.html
eval $xslt_proc -o predicates/logtalk_make_0.section refman.xsl predicates/logtalk_make_0.html
eval $xslt_proc -o predicates/logtalk_make_1.section refman.xsl predicates/logtalk_make_1.html
eval $xslt_proc -o predicates/logtalk_library_path_2.section refman.xsl predicates/logtalk_library_path_2.html
eval $xslt_proc -o predicates/logtalk_load_context_2.section refman.xsl predicates/logtalk_load_context_2.html
eval $xslt_proc -o predicates/current_logtalk_flag_2.section refman.xsl predicates/current_logtalk_flag_2.html
eval $xslt_proc -o predicates/set_logtalk_flag_2.section refman.xsl predicates/set_logtalk_flag_2.html
eval $xslt_proc -o predicates/create_logtalk_flag_3.section refman.xsl predicates/create_logtalk_flag_3.html
eval $xslt_proc -o methods/parameter_2.section refman.xsl methods/parameter_2.html
eval $xslt_proc -o methods/self_1.section refman.xsl methods/self_1.html
eval $xslt_proc -o methods/sender_1.section refman.xsl methods/sender_1.html
eval $xslt_proc -o methods/this_1.section refman.xsl methods/this_1.html
eval $xslt_proc -o methods/current_op_3.section refman.xsl methods/current_op_3.html
eval $xslt_proc -o methods/current_predicate_1.section refman.xsl methods/current_predicate_1.html
eval $xslt_proc -o methods/predicate_property_2.section refman.xsl methods/predicate_property_2.html
eval $xslt_proc -o methods/abolish_1.section refman.xsl methods/abolish_1.html
eval $xslt_proc -o methods/asserta_1.section refman.xsl methods/asserta_1.html
eval $xslt_proc -o methods/assertz_1.section refman.xsl methods/assertz_1.html
eval $xslt_proc -o methods/clause_2.section refman.xsl methods/clause_2.html
eval $xslt_proc -o methods/retract_1.section refman.xsl methods/retract_1.html
eval $xslt_proc -o methods/retractall_1.section refman.xsl methods/retractall_1.html
eval $xslt_proc -o methods/call_N.section refman.xsl methods/call_N.html
eval $xslt_proc -o methods/ignore_1.section refman.xsl methods/ignore_1.html
eval $xslt_proc -o methods/once_1.section refman.xsl methods/once_1.html
eval $xslt_proc -o methods/not_1.section refman.xsl methods/not_1.html
eval $xslt_proc -o methods/catch_3.section refman.xsl methods/catch_3.html
eval $xslt_proc -o methods/throw_1.section refman.xsl methods/throw_1.html
eval $xslt_proc -o methods/bagof_3.section refman.xsl methods/bagof_3.html
eval $xslt_proc -o methods/findall_3.section refman.xsl methods/findall_3.html
eval $xslt_proc -o methods/findall_4.section refman.xsl methods/findall_4.html
eval $xslt_proc -o methods/forall_2.section refman.xsl methods/forall_2.html
eval $xslt_proc -o methods/setof_3.section refman.xsl methods/setof_3.html
eval $xslt_proc -o methods/before_3.section refman.xsl methods/before_3.html
eval $xslt_proc -o methods/after_3.section refman.xsl methods/after_3.html
eval $xslt_proc -o methods/forward_1.section refman.xsl methods/forward_1.html
eval $xslt_proc -o methods/call_1.section refman.xsl methods/call_1.html
eval $xslt_proc -o methods/phrase_1.section refman.xsl methods/phrase_1.html
eval $xslt_proc -o methods/phrase_2.section refman.xsl methods/phrase_2.html
eval $xslt_proc -o methods/phrase_3.section refman.xsl methods/phrase_3.html
eval $xslt_proc -o methods/expand_term_2.section refman.xsl methods/expand_term_2.html
eval $xslt_proc -o methods/term_expansion_2.section refman.xsl methods/term_expansion_2.html
eval $xslt_proc -o methods/expand_goal_2.section refman.xsl methods/expand_goal_2.html
eval $xslt_proc -o methods/goal_expansion_2.section refman.xsl methods/goal_expansion_2.html
eval $xslt_proc -o methods/print_message_3.section refman.xsl methods/print_message_3.html
eval $xslt_proc -o methods/message_tokens_2.section refman.xsl methods/message_tokens_2.html
eval $xslt_proc -o methods/message_hook_4.section refman.xsl methods/message_hook_4.html
eval $xslt_proc -o methods/message_prefix_stream_4.section refman.xsl methods/message_prefix_stream_4.html
eval $xslt_proc -o methods/print_message_tokens_3.section refman.xsl methods/print_message_tokens_3.html
eval $xslt_proc -o methods/print_message_token_4.section refman.xsl methods/print_message_token_4.html
eval $xslt_proc -o methods/ask_question_5.section refman.xsl methods/ask_question_5.html
eval $xslt_proc -o methods/question_hook_6.section refman.xsl methods/question_hook_6.html
eval $xslt_proc -o methods/question_prompt_stream_4.section refman.xsl methods/question_prompt_stream_4.html
eval $xslt_proc -o methods/coinductive_success_hook_1_2.section refman.xsl methods/coinductive_success_hook_1_2.html
eval $xslt_proc -o control/send_to_object_2.section refman.xsl control/send_to_object_2.html
eval $xslt_proc -o control/send_to_self_1.section refman.xsl control/send_to_self_1.html
eval $xslt_proc -o control/call_super_1.section refman.xsl control/call_super_1.html
eval $xslt_proc -o control/delegate_message_1.section refman.xsl control/delegate_message_1.html
eval $xslt_proc -o control/external_call_1.section refman.xsl control/external_call_1.html
eval $xslt_proc -o control/context_switch_2.section refman.xsl control/context_switch_2.html

cat -s \
	refman.header \
	index.section \
	refman.body \
	grammar.section \
	directives.header \
	directives/encoding_1.section \
	directives/set_logtalk_flag_2.section \
	directives/if_1.section \
	directives/elif_1.section \
	directives/else_0.section \
	directives/endif_0.section \
	directives/built_in_0.section \
	directives/category_1_3.section \
	directives/dynamic_0.section \
	directives/end_category_0.section \
	directives/end_object_0.section \
	directives/end_protocol_0.section \
	directives/include_1.section \
	directives/info_1.section \
	directives/initialization_1.section \
	directives/multifile_1.section \
	directives/object_1_5.section \
	directives/protocol_1_2.section \
	directives/threaded_0.section \
	directives/alias_2.section \
	directives/coinductive_1.section \
	directives/discontiguous_1.section \
	directives/dynamic_1.section \
	directives/info_2.section \
	directives/meta_predicate_1.section \
	directives/meta_non_terminal_1.section \
	directives/mode_2.section \
	directives/op_3.section \
	directives/private_1.section \
	directives/protected_1.section \
	directives/public_1.section \
	directives/synchronized_1.section \
	directives/uses_2.section \
	directives/use_module_2.section \
	predicates.header \
	predicates/current_category_1.section \
	predicates/current_object_1.section \
	predicates/current_protocol_1.section \
	predicates/category_property_2.section \
	predicates/object_property_2.section \
	predicates/protocol_property_2.section \
	predicates/create_category_4.section \
	predicates/create_object_4.section \
	predicates/create_protocol_3.section \
	predicates/abolish_category_1.section \
	predicates/abolish_object_1.section \
	predicates/abolish_protocol_1.section \
	predicates/extends_object_2_3.section \
	predicates/extends_protocol_2_3.section \
	predicates/extends_category_2_3.section \
	predicates/implements_protocol_2_3.section \
	predicates/imports_category_2_3.section \
	predicates/instantiates_class_2_3.section \
	predicates/specializes_class_2_3.section \
	predicates/complements_object_2.section \
	predicates/conforms_to_protocol_2_3.section \
	predicates/abolish_events_5.section \
	predicates/current_event_5.section \
	predicates/define_events_5.section \
	predicates/threaded_1.section \
	predicates/threaded_call_1_2.section \
	predicates/threaded_once_1_2.section \
	predicates/threaded_ignore_1.section \
	predicates/threaded_exit_1_2.section \
	predicates/threaded_peek_1_2.section \
	predicates/threaded_wait_1.section \
	predicates/threaded_notify_1.section \
	predicates/logtalk_compile_1.section \
	predicates/logtalk_compile_2.section \
	predicates/logtalk_load_1.section \
	predicates/logtalk_load_2.section \
	predicates/logtalk_make_0.section \
	predicates/logtalk_make_1.section \
	predicates/logtalk_library_path_2.section \
	predicates/logtalk_load_context_2.section \
	predicates/current_logtalk_flag_2.section \
	predicates/set_logtalk_flag_2.section \
	predicates/create_logtalk_flag_3.section \
	methods.header \
	methods/parameter_2.section \
	methods/self_1.section \
	methods/sender_1.section \
	methods/this_1.section \
	methods/current_op_3.section \
	methods/current_predicate_1.section \
	methods/predicate_property_2.section \
	methods/abolish_1.section \
	methods/asserta_1.section \
	methods/assertz_1.section \
	methods/clause_2.section \
	methods/retract_1.section \
	methods/retractall_1.section \
	methods/call_N.section \
	methods/ignore_1.section \
	methods/once_1.section \
	methods/not_1.section \
	methods/catch_3.section \
	methods/throw_1.section \
	methods/bagof_3.section \
	methods/findall_3.section \
	methods/findall_4.section \
	methods/forall_2.section \
	methods/setof_3.section \
	methods/before_3.section \
	methods/after_3.section \
	methods/forward_1.section \
	methods/call_1.section \
	methods/phrase_1.section \
	methods/phrase_2.section \
	methods/phrase_3.section \
	methods/expand_term_2.section \
	methods/term_expansion_2.section \
	methods/expand_goal_2.section \
	methods/goal_expansion_2.section \
	methods/print_message_3.section \
	methods/message_tokens_2.section \
	methods/message_hook_4.section \
	methods/message_prefix_stream_4.section \
	methods/print_message_tokens_3.section \
	methods/print_message_token_4.section \
	methods/ask_question_5.section \
	methods/question_hook_6.section \
	methods/question_prompt_stream_4.section \
	methods/coinductive_success_hook_1_2.section \
	control.header \
	control/send_to_object_2.section \
	control/send_to_self_1.section \
	control/call_super_1.section \
	control/delegate_message_1.section \
	control/external_call_1.section \
	control/context_switch_2.section \
	refman.footer \
	> refman.html

java -jar $css2xslfo -c $catalog refman.html -fo refman.fo

if [ "$fo_proc" == "xep" ]; then
	eval $fo_proc -valid -fo refman.fo -pdf refman.pdf
else
	eval $fo_proc -fo refman.fo -pdf refman.pdf
fi

rm refman.fo refman.html
rm -f *.section directives/*.section predicates/*.section methods/*.section control/*.section
mv refman.pdf ..
