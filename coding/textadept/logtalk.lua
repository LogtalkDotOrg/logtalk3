-- Copyright © 2017-2019 Michael T. Richter <ttmrichter@gmail.com>. See License.txt.
-- Logtalk LPeg lexer.

local lexer = require('lexer')
local token, word_match = lexer.token, lexer.word_match
local P, R, S = lpeg.P, lpeg.R, lpeg.S

local lex = lexer.new('logtalk', {inherit = lexer.load('prolog')})

-- add logtalk keywords to prolog ones
lex:modify_rule('keyword', token(lexer.KEYWORD, word_match[[
  -- Logtalk "keywords" generated from Vim syntax highlighting file with Prolog
  -- keywords stripped since were building up on the Prolog lexer.
  abolish_category abolish_events abolish_object abolish_protocol after alias as
  before built_in calls category category_property coinductive complements
  complements_object conforms_to_protocol context create_category create_object
  create_protocol create_logtalk_flag current current_category current_event
  current_logtalk_flag current_object current_protocol define_events domain_error
  encoding end_category end_class end_object end_protocol evaluation_error
  existence_error extends extends_category extends_object extends_protocol forward
  implements implements_protocol imports imports_category include info instantiates
  instantiates_class instantiation_error is logtalk_compile logtalk_library_path
  logtalk_load logtalk_load_context logtalk_make logtalk_make_target_action
  meta_non_terminal mode object object_property parameter permission_error private
  protected protocol_property representation_error resource_error self sender
  set_logtalk_flag specializes specializes_class synchronized syntax_error
  system_error this threaded threaded_call threaded_cancel threaded_engine
  threaded_engine_create threaded_engine_destroy threaded_engine_fetch
  threaded_engine_next threaded_engine_next_reified threaded_engine_post
  threaded_engine_self threaded_engine_yield threaded_exit threaded_ignore
  threaded_notify threaded_once threaded_peek threaded_wait type_error uses
  -- info/1 and info/2 predicates have their own keywords manually extracted
  -- from documentation.
  comment argnames arguments author version date parameters parnames copyright
  license remarks see_also
]]) + lex:get_rule('keyword'))

return lex
