package org.logtalk.intellij;

import com.intellij.lexer.FlexLexer;
import com.intellij.psi.tree.IElementType;
import org.logtalk.intellij.psi.LogtalkTypes;
import com.intellij.psi.TokenType;
%%

%class LogtalkLexer
%implements FlexLexer
%unicode
%function advance
%type IElementType
%eof{  return;
%eof}


%{
      StringBuffer string = new StringBuffer();
      String quote;
%}

LPAREN = "("
RPAREN = ")"
LBRACKET = "["
RBRACKET = "]"
LBRACE = "{"
RBRACE = "}"
CUT = "!"
CONS = "[|]"  //specific to SWI

//PROLOG OPERATORS (http://www.swi-prolog.org/pldoc/man?section=operators)

LONG_ARROW_OP = "-->"

DIRECTIVE_OP = ":-"

QUERY_OP = "?-"

DYNAMIC_OP = "dynamic"

DISCONTIGUOUS_OP = "discontiguous"

// the Prolog "initialization" operator conflicts with Logtalk
INITIALIZATION_OP = "initialization"

META_PREDICATE_OP = "meta_predicate"

MODULE_TRANSPARENT_OP = module_transparent

MULTIFILE_OP = multifile

// the Prolog "public" operator conflicts with Logtalk
PUBLIC_OP = "public"

THREAD_LOCAL_OP = "thread_local"

THREAD_INITIALIZATION_OP = "thread_initialization"

VOLATILE_OP = "volatile"

SEMICOLON_OP = ";"

PIPE_OP = "|"

IMPLICATION_OP = "->"

STAR_IMPLICATION_OP = "*->"

COMMA = ","

COLON_EQUALS_OP = ":="

NOT_OP = "\\+"

LESS_THAN_OP = "<"

EQUALS_OP = "="

EQUALS_DOT_DOT_OP = "=.."

EQUALS_AT_EQUALS_OP = "=@="

BACKSLASH_EQUALS_AT_EQUALS_OP = "\\=@="

EQUALS_COLON_EQUALS_OP = "=:="

LESS_OR_EQUALS_OP =  "=<"

DOUBLE_EQUALS_OP = "=="

EQUALS_BACKSLASH_EQUALS_OP = "=\\="

GREATER_THAN_OP = ">"

GREATER_OR_EQUALS_OP = ">="

AT_LESS_THAN_OP = "@<"

AT_EQUALS_LESS_THAN_OP = "@=<"

AT_GREATER_THAN_OP = "@>"

AT_GREATER_OR_EQUALS_OP = "@>="

BACKSLASH_EQUALS_OP = "\\="

BACKSLASH_DOUBLE_EQUALS_OP = "\\=="

AS_OP = "as"

IS_OP = "is"

BUTTERFLY_OP = ">:<"

COLON_LESS_THAN_OP = ":<"

COLON_OP = ":"

PLUS_OP = "+"

MINUS_OP = "-"

SLASH_BACKSLASH_OP = "/\\"

BACKSLASH_SLASH_OP = "\\/"

XOR_OP = "xor"

QUESTION_MARK_OP = "?"

STAR_OP = "*"

SLASH_OP = "/"

DOUBLE_SLASH_OP = "//"

DIV_OP = "div"

RDIV_OP = "rdiv"

//DOUBLE_LESS_THAN_OP = "<<"

DOUBLE_GREATER_THAN_OP = ">>"

MOD_OP = "mod"

REM_OP = "rem"

DOUBLE_STAR_OP = "**"

CARET_OP = "^"

BACKSLASH_OP = "\\"

DOT = "."

//MAP_OP = {DOT}

DOLLAR_OP = "$"

AT = "@"

BIN_PREFIX = "0b"

OCT_PREFIX = "0o"

HEX_PREFIX = "0x"

CHAR_CODE_ESCAPED = "0'\\"

CHAR_CODE = "0'"





//LOGTALK OPERATORS

LGT_METHOD_CALL_OP = "::"

LGT_SUPER_CALL_OP = "^^"

LGT_CONTEXT_SWITCHING_OP = "<<"





//LGT_KEYWORDS = ({LGT_COMPOUND_NAME_KEYWORDS}|{LGT_ATOM_KEYWORDS})
LGT_COMPOUND_NAME_KEYWORDS = (
"encoding" | "initialization" | "op" | "set_logtalk_flag" | "if" | "elif" | "calls" | "category" |
"include" | "info" | "initialization" | "object" | "protocol" |
"uses" | "alias" | "coinductive" | "discontiguous" | "dynamic" | "info" | "meta_predicate" | "meta_non_terminal" | "mode" |
"multifile" | "private" | "protected" | "public" | "synchronized" | "uses" | "use_module" |
"parameter" | "self" | "sender" | "this" | "current_op" | "current_predicate" | "predicate_property" | "abolish" | "asserta" |
 "assertz" | "clause" | "retract" | "retractall" | "call" | "once" | "catch" | "throw" | "bagof" | "findall" | "forall" |
 "setof" | "before" | "after" | "forward" | "phrase" | "phrase" | "expand_term" | "term_expansion" | "expand_goal" |
 "goal_expansion" | "coinductive_success_hook" | "coinductive_success_hook" | "ask_question" | "message_hook" |
 "message_prefix_stream" | "print_message" | "print_message_tokens" | "print_message_token" | "question_hook" | "question_prompt_stream"|
 "call" | "call" | "call" | "call" | "call" | "call" | "phrase" | "message_tokens"|
 "current_category" | "current_object" | "current_protocol" | "category_property" | "object_property" | "protocol_property" |
 "create_category" | "create_object" | "create_protocol" | "abolish_category" | "abolish_object" | "abolish_protocol" |
 "extends_object" | "extends_object" | "extends_protocol" | "extends_protocol" | "extends_category" | "extends_category" |
 "implements_protocol" | "implements_protocol" | "imports_category" | "imports_category" | "instantiates_class" | "instantiates_class" |
 "specializes_class" | "specializes_class" | "complements_object" | "abolish_events" | "current_event" | "define_events" |
 "threaded" | "threaded_call" | "threaded_call" | "threaded_once" | "threaded_once" | "threaded_ignore" | "threaded_exit" |
 "threaded_peek" | "threaded_wait" | "threaded_notify" | "threaded_engine" | "threaded_engine_create" | "threaded_engine_destroy" |
 "threaded_engine_self" | "threaded_engine_next" | "threaded_engine_next_reified" | "threaded_engine_yield" | "threaded_engine_post" |
 "threaded_engine_fetch" | "logtalk_compile" | "logtalk_compile" | "logtalk_load" | "logtalk_load" | "logtalk_make" | "logtalk_library_path" |
 "logtalk_load_context" | "current_logtalk_flag" | "set_logtalk_flag" | "create_logtalk_flag" |
 "implements" | "imports" | "complements" | "extends" | "instantiates" | "specializes" |
 "ensure_loaded" | "export" | "reexport" | "module" | "set_prolog_flag" |
 "unify_with_occurs_check" | "subsumes_term" | "atom" | "atomic" | "integer" | "float" | "callable" | "compound" | "nonvar" |
 "var" | "number" | "ground" | "acyclic_term" | "compare" | "functor" | "arg" | "copy_term" | "numbervars" | "term_variables" |
 "current_input" | "current_output" | "set_input" | "set_output" | "open" | "close" | "flush_output" | "stream_property" |
 "at_end_of_stream" | "set_stream_position" | "get_char" | "get_code" | "peek_char" | "peek_code" | "put_char" | "put_code" |
 "nl" | "get_byte" | "peek_byte" | "put_byte" | "read" | "read_term" | "writeq" | "write" | "write_canonical" | "write_term" |
 "current_op" | "op" | "current_char_conversion" | "char_conversion" | "call" | "once" | "ignore" | "atom_length" | "atom_chars" |
 "atom_codes" | "atom_concat" | "sub_atom" | "char_code" | "number_chars" | "number_codes" | "current_prolog_flag" | "set_prolog_flag" |
 "halt" | "keysort" | "sort" |
 "atan" | "atan2" | "acos" | "asin" | "sin" | "cos" | "tan" | "sign" | "abs" | "truncate" | "round" | "ceiling" | "exp" |
 "log" | "sqrt" | "rem" | "mod" | "div" | "float_fractional_part" | "float_integer_part" | "float" | "floor" | "min" | "max" | "xor"
)


LGT_ATOM_KEYWORDS = (
"else" | "endif" | "dynamic" | "end_category" | "end_object" | "end_protocol" | "threaded" |
"eos" |
"logtalk_make" |
"built_in" |
"halt" | "flush_output" | "at_end_of_stream" | "true" | "fail" | "false" | "repeat" | "nl" |
"pi" | "e"
)


CRLF = \R
WHITE_SPACE = [\ \n\t\f]

SIGN = "+" | "-"
EXPONENTIATION = "e"|"E"

SIMPLE_INTEGER = [:digit:]+
INTEGER = {SIGN}? {SIMPLE_INTEGER} ({EXPONENTIATION} {SIGN}? {SIMPLE_INTEGER})?
FLOAT = {SIGN}? {INTEGER} {DOT} {INTEGER} ({EXPONENTIATION} {SIGN}? {SIMPLE_INTEGER})?
BIN_NUMBER = {SIGN}? [0-1]+
OCT_NUMBER = {SIGN}? [0-7]+
HEX_NUMBER = {SIGN}? [a-fA-F0-9_]+

ATOM_CHAR = [:jletterdigit:]
UNQUOTED_ATOM = [:lowercase:] {ATOM_CHAR}*

ANONYMOUS_VARIABLE = "_" {ATOM_CHAR}*
NAMED_VARIABLE = [:uppercase:] {ATOM_CHAR}*

STYLE_COMMENT = ("%!"|"%%")[^\r\n]*
END_OF_LINE_COMMENT = ("%")[^\r\n]*
BLOCK_COMMENT = "/*" [^*] ~"*/" | "/*" "*"+ "/"
DOC_COMMENT = "/**" {DOC_COMMENT_CONTENT} "*"+ "/"
DOC_COMMENT_CONTENT = ( [^*] | \*+ [^/*] )*
COMMENT = {STYLE_COMMENT} | {END_OF_LINE_COMMENT} | {BLOCK_COMMENT} | {DOC_COMMENT}
STRING_DELIMITER = \" | "'"
//NON_PRINTABLE = ({CRLF}|{WHITE_SPACE})

FIRST_CHAR_MAP_PROPERTY = ([:letter:]|[:digit:]|"_")



%state STRING, SENTENCE, DIRECTIVE, CHAR_CODE, EXPECTING_HEX, WAITING_VALUE

%%


<YYINITIAL, SENTENCE, DIRECTIVE> {

    {COMMENT}                                       { return LogtalkTypes.COMMENT; }

    ({CRLF}|{WHITE_SPACE})+                         { return TokenType.WHITE_SPACE; }


}

<CHAR_CODE> {

    ({CHAR_CODE_ESCAPED}|{CHAR_CODE}) .             { yybegin(SENTENCE); return LogtalkTypes.CHAR_CODE; }

}


<YYINITIAL> {

    {DIRECTIVE_OP}                                  { yybegin(DIRECTIVE); return LogtalkTypes.DIRECTIVE_OP; }

    {LGT_COMPOUND_NAME_KEYWORDS}  /{LPAREN}         { yybegin(SENTENCE); return LogtalkTypes.HEAD_KEYWORD_COMPOUND_NAME; }

    {UNQUOTED_ATOM}  /{LPAREN}                      { yybegin(SENTENCE); return LogtalkTypes.HEAD_UNQUOTED_COMPOUND_NAME; }

    {LGT_ATOM_KEYWORDS}                             { yybegin(SENTENCE); return LogtalkTypes.HEAD_KEYWORD_ATOM; }

    {UNQUOTED_ATOM}                                 { yybegin(SENTENCE); return LogtalkTypes.HEAD_UNQUOTED_ATOM; }

}



<DIRECTIVE> {

    {LGT_COMPOUND_NAME_KEYWORDS}  /{LPAREN}         { yybegin(SENTENCE); return LogtalkTypes.DIRECTIVE_KEYWORD_COMPOUND_NAME;}

    {UNQUOTED_ATOM}  /{LPAREN}                      { yybegin(SENTENCE); return LogtalkTypes.DIRECTIVE_UNQUOTED_COMPOUND_NAME;}

    {LGT_ATOM_KEYWORDS}                             { yybegin(SENTENCE); return LogtalkTypes.DIRECTIVE_KEYWORD_ATOM;}

    {UNQUOTED_ATOM}                                 { yybegin(SENTENCE); return LogtalkTypes.DIRECTIVE_UNQUOTED_ATOM;}

}



<SENTENCE> {

    {CONS}                                          { return LogtalkTypes.CONS; }

    {CUT}                                           { return LogtalkTypes.CUT; }

    {LPAREN}                                        { return LogtalkTypes.LPAREN; }

    {RPAREN}                                        { return LogtalkTypes.RPAREN; }

    {LBRACKET}                                      { return LogtalkTypes.LBRACKET; }

    {RBRACKET}                                      { return LogtalkTypes.RBRACKET; }

    {LBRACE}                                        { return LogtalkTypes.LBRACE; }

    {RBRACE}                                        { return LogtalkTypes.RBRACE; }

    {BIN_PREFIX} {BIN_NUMBER}                       { return LogtalkTypes.BIN_NUMBER;}

    {OCT_PREFIX} {OCT_NUMBER}                       { return LogtalkTypes.OCT_NUMBER;}

    {HEX_PREFIX} {HEX_NUMBER}                       { return LogtalkTypes.HEX_NUMBER;}

    {INTEGER}                                       { return LogtalkTypes.INTEGER;}

    {FLOAT}                                         { return LogtalkTypes.FLOAT;}

    {ANONYMOUS_VARIABLE}                            { return LogtalkTypes.ANONYMOUS_VARIABLE;}

    {NAMED_VARIABLE}                                { return LogtalkTypes.NAMED_VARIABLE;}

    {DOT}                                           { yybegin(YYINITIAL); return LogtalkTypes.DOT; }


}



/*
<WAITING_VALUE> {CRLF}({CRLF}|{WHITE_SPACE})+       { yybegin(YYINITIAL); return TokenType.WHITE_SPACE; }

<WAITING_VALUE> {WHITE_SPACE}+                      { yybegin(WAITING_VALUE); return TokenType.WHITE_SPACE; }

<YYINITIAL> ({CRLF}|{WHITE_SPACE})+                 { yybegin(YYINITIAL); return LogtalkTypes.WHITE_SPACE; }
*/

<YYINITIAL, SENTENCE> {

    {STRING_DELIMITER}                              { string.setLength(0); quote = yytext().toString(); yybegin(STRING); }

    {LGT_COMPOUND_NAME_KEYWORDS}  /{LPAREN}         { yybegin(SENTENCE); return LogtalkTypes.KEYWORD_COMPOUND_NAME; }

    {UNQUOTED_ATOM}  /{LPAREN}                      { yybegin(SENTENCE); return LogtalkTypes.UNQUOTED_COMPOUND_NAME; }


    //PROLOG OPERATORS

    {LONG_ARROW_OP}                                 { yybegin(SENTENCE); return LogtalkTypes.LONG_ARROW_OP; }

    {DIRECTIVE_OP}                                  { yybegin(SENTENCE); return LogtalkTypes.DIRECTIVE_OP; }

    {QUERY_OP}                                      { yybegin(SENTENCE); return LogtalkTypes.QUERY_OP; }

    {DYNAMIC_OP}                                    { yybegin(SENTENCE); return LogtalkTypes.DYNAMIC_OP; }

    {DISCONTIGUOUS_OP}                              { yybegin(SENTENCE); return LogtalkTypes.DISCONTIGUOUS_OP; }

    {INITIALIZATION_OP}                             { yybegin(SENTENCE); return LogtalkTypes.INITIALIZATION_OP; }

    {META_PREDICATE_OP}                             { yybegin(SENTENCE); return LogtalkTypes.META_PREDICATE_OP; }

    {MODULE_TRANSPARENT_OP}                         { yybegin(SENTENCE); return LogtalkTypes.MODULE_TRANSPARENT_OP; }

    {MULTIFILE_OP}                                  { yybegin(SENTENCE); return LogtalkTypes.MULTIFILE_OP; }

    {PUBLIC_OP}                                     { yybegin(SENTENCE); return LogtalkTypes.PUBLIC_OP; }

    {THREAD_LOCAL_OP}                               { yybegin(SENTENCE); return LogtalkTypes.THREAD_LOCAL_OP; }

    {THREAD_INITIALIZATION_OP}                      { yybegin(SENTENCE); return LogtalkTypes.THREAD_INITIALIZATION_OP; }

    {VOLATILE_OP}                                   { yybegin(SENTENCE); return LogtalkTypes.VOLATILE_OP; }

    {SEMICOLON_OP}                                  { yybegin(SENTENCE); return LogtalkTypes.SEMICOLON_OP; }

    {PIPE_OP}                                       { yybegin(SENTENCE); return LogtalkTypes.PIPE_OP; }

    {IMPLICATION_OP}                                { yybegin(SENTENCE); return LogtalkTypes.IMPLICATION_OP; }

    {STAR_IMPLICATION_OP}                           { yybegin(SENTENCE); return LogtalkTypes.STAR_IMPLICATION_OP; }

    {COMMA}                                         { yybegin(SENTENCE); return LogtalkTypes.COMMA; }

    {COLON_EQUALS_OP}                               { yybegin(SENTENCE); return LogtalkTypes.COLON_EQUALS_OP; }

    {NOT_OP}                                        { yybegin(SENTENCE); return LogtalkTypes.NOT_OP; }

    {LESS_THAN_OP}                                  { yybegin(SENTENCE); return LogtalkTypes.LESS_THAN_OP; }

    {EQUALS_OP}                                     { yybegin(SENTENCE); return LogtalkTypes.EQUALS_OP; }

    {EQUALS_DOT_DOT_OP}                             { yybegin(SENTENCE); return LogtalkTypes.EQUALS_DOT_DOT_OP; }

    {EQUALS_AT_EQUALS_OP}                           { yybegin(SENTENCE); return LogtalkTypes.EQUALS_AT_EQUALS_OP; }

    {BACKSLASH_EQUALS_AT_EQUALS_OP}                 { yybegin(SENTENCE); return LogtalkTypes.BACKSLASH_EQUALS_AT_EQUALS_OP; }

    {EQUALS_COLON_EQUALS_OP}                        { yybegin(SENTENCE); return LogtalkTypes.EQUALS_COLON_EQUALS_OP; }

    {LESS_OR_EQUALS_OP}                             { yybegin(SENTENCE); return LogtalkTypes.LESS_OR_EQUALS_OP; }

    {DOUBLE_EQUALS_OP}                              { yybegin(SENTENCE); return LogtalkTypes.DOUBLE_EQUALS_OP; }

    {EQUALS_BACKSLASH_EQUALS_OP}                    { yybegin(SENTENCE); return LogtalkTypes.EQUALS_BACKSLASH_EQUALS_OP; }

    {GREATER_THAN_OP}                               { yybegin(SENTENCE); return LogtalkTypes.GREATER_THAN_OP; }

    {GREATER_OR_EQUALS_OP}                          { yybegin(SENTENCE); return LogtalkTypes.GREATER_OR_EQUALS_OP; }

    {AT_LESS_THAN_OP}                               { yybegin(SENTENCE); return LogtalkTypes.AT_LESS_THAN_OP; }

    {AT_EQUALS_LESS_THAN_OP}                        { yybegin(SENTENCE); return LogtalkTypes.AT_EQUALS_LESS_THAN_OP; }

    {AT_GREATER_THAN_OP}                            { yybegin(SENTENCE); return LogtalkTypes.AT_GREATER_THAN_OP; }

    {AT_GREATER_OR_EQUALS_OP}                       { yybegin(SENTENCE); return LogtalkTypes.AT_GREATER_OR_EQUALS_OP; }

    {BACKSLASH_EQUALS_OP}                           { yybegin(SENTENCE); return LogtalkTypes.BACKSLASH_EQUALS_OP; }

    {BACKSLASH_DOUBLE_EQUALS_OP}                    { yybegin(SENTENCE); return LogtalkTypes.BACKSLASH_DOUBLE_EQUALS_OP; }

    {AS_OP}                                         { yybegin(SENTENCE); return LogtalkTypes.AS_OP; }

    {IS_OP}                                         { yybegin(SENTENCE); return LogtalkTypes.IS_OP; }

    {BUTTERFLY_OP}                                  { yybegin(SENTENCE); return LogtalkTypes.BUTTERFLY_OP; }

    {COLON_LESS_THAN_OP}                            { yybegin(SENTENCE); return LogtalkTypes.COLON_LESS_THAN_OP; }

    {COLON_OP}                                      { yybegin(SENTENCE); return LogtalkTypes.COLON_OP; }

    {PLUS_OP}                                       { yybegin(SENTENCE); return LogtalkTypes.PLUS_OP; }

    {MINUS_OP}                                      { yybegin(SENTENCE); return LogtalkTypes.MINUS_OP; }

    {SLASH_BACKSLASH_OP}                            { yybegin(SENTENCE); return LogtalkTypes.SLASH_BACKSLASH_OP; }

    {BACKSLASH_SLASH_OP}                            { yybegin(SENTENCE); return LogtalkTypes.BACKSLASH_SLASH_OP; }

    {XOR_OP}                                        { yybegin(SENTENCE); return LogtalkTypes.XOR_OP; }

    {QUESTION_MARK_OP}                              { yybegin(SENTENCE); return LogtalkTypes.QUESTION_MARK_OP; }

    {STAR_OP}                                       { yybegin(SENTENCE); return LogtalkTypes.STAR_OP; }

    {SLASH_OP}                                      { yybegin(SENTENCE); return LogtalkTypes.SLASH_OP; }

    {DOUBLE_SLASH_OP}                               { yybegin(SENTENCE); return LogtalkTypes.DOUBLE_SLASH_OP; }

    {DIV_OP}                                        { yybegin(SENTENCE); return LogtalkTypes.DIV_OP; }

    {RDIV_OP}                                       { yybegin(SENTENCE); return LogtalkTypes.RDIV_OP; }

    //{DOUBLE_LESS_THAN_OP}                           { yybegin(SENTENCE); return LogtalkTypes.DOUBLE_LESS_THAN_OP; }

    {DOUBLE_GREATER_THAN_OP}                        { yybegin(SENTENCE); return LogtalkTypes.DOUBLE_GREATER_THAN_OP; }

    {MOD_OP}                                        { yybegin(SENTENCE); return LogtalkTypes.MOD_OP; }

    {REM_OP}                                        { yybegin(SENTENCE); return LogtalkTypes.REM_OP; }

    {DOUBLE_STAR_OP}                                { yybegin(SENTENCE); return LogtalkTypes.DOUBLE_STAR_OP; }

    {CARET_OP}                                      { yybegin(SENTENCE); return LogtalkTypes.CARET_OP; }

    {BACKSLASH_OP}                                  { yybegin(SENTENCE); return LogtalkTypes.BACKSLASH_OP; }

    {DOT}  /{FIRST_CHAR_MAP_PROPERTY}               { yybegin(SENTENCE); return LogtalkTypes.MAP_OP; }

    {DOLLAR_OP}                                     { yybegin(SENTENCE); return LogtalkTypes.DOLLAR_OP; }

    {AT}                                            { yybegin(SENTENCE); return LogtalkTypes.AT; }



    {CHAR_CODE_ESCAPED}                             { yybegin(CHAR_CODE); yypushback(3); }

    {CHAR_CODE}                                     { yybegin(CHAR_CODE); yypushback(2); }




    //LOGTALK OPERATORS

    {LGT_CONTEXT_SWITCHING_OP}                      { yybegin(SENTENCE); return LogtalkTypes.LGT_CONTEXT_SWITCHING_OP; }

    {LGT_METHOD_CALL_OP}                            { yybegin(SENTENCE); return LogtalkTypes.LGT_METHOD_CALL_OP; }

    {LGT_SUPER_CALL_OP}                             { yybegin(SENTENCE); return LogtalkTypes.LGT_SUPER_CALL_OP; }



    {LGT_ATOM_KEYWORDS}                             { yybegin(SENTENCE); return LogtalkTypes.KEYWORD_ATOM; }

    {UNQUOTED_ATOM}                                 { yybegin(SENTENCE); return LogtalkTypes.UNQUOTED_ATOM;}

}





<STRING> {
    [']  /{LPAREN}                                  {
                                                        if (quote.equals("'")) {
                                                            yybegin(SENTENCE);
                                                            return LogtalkTypes.QUOTED_COMPOUND_NAME;
                                                        } else {
                                                             string.append( yytext() );
                                                        }
                                                    }
    [\"']                                           {
                                                        if (quote.equals(yytext().toString())) {
                                                            yybegin(SENTENCE);
                                                            return quote.equals("'") ?
                                                                LogtalkTypes.QUOTED_ATOM : LogtalkTypes.STRING;
                                                        } else {
                                                             string.append( yytext() );
                                                        }
                                                    }
    {CRLF}                                          { yybegin(SENTENCE); return TokenType.BAD_CHARACTER; }
    [^\n\r\"'\\]+                                   { string.append( yytext() ); }
    \\t                                             { string.append('\t'); }
    \\n                                             { string.append('\n'); }
    \\r                                             { string.append('\r'); }
    \\\"                                            { string.append('\"'); }
    \\                                              { string.append('\\'); }
}


    .                                               { return TokenType.BAD_CHARACTER; }

