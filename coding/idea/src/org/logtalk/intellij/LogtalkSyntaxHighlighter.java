package org.logtalk.intellij;


import static com.intellij.openapi.editor.colors.TextAttributesKey.createTextAttributesKey;

import org.jetbrains.annotations.NotNull;
import org.logtalk.intellij.psi.LogtalkTypes;

import com.intellij.lexer.Lexer;
import com.intellij.openapi.editor.DefaultLanguageHighlighterColors;
import com.intellij.openapi.editor.HighlighterColors;
import com.intellij.openapi.editor.colors.TextAttributesKey;
import com.intellij.openapi.fileTypes.SyntaxHighlighterBase;
import com.intellij.psi.TokenType;
import com.intellij.psi.tree.IElementType;

public class LogtalkSyntaxHighlighter extends SyntaxHighlighterBase {

    public static final TextAttributesKey DIRECTIVE_OP =
            createTextAttributesKey("DIRECTIVE_OP", DefaultLanguageHighlighterColors.CONSTANT);

    public static final TextAttributesKey LONG_ARROW_OP =
            createTextAttributesKey("LONG_ARROW_OP", DefaultLanguageHighlighterColors.CONSTANT);

    public static final TextAttributesKey OPERATOR =
            createTextAttributesKey("OPERATOR", DefaultLanguageHighlighterColors.OPERATION_SIGN);

    public static final TextAttributesKey CUT =
            createTextAttributesKey("CUT", DefaultLanguageHighlighterColors.CONSTANT);

    public static final TextAttributesKey STRING =
            createTextAttributesKey("STRING_TERM", DefaultLanguageHighlighterColors.VALID_STRING_ESCAPE);

    public static final TextAttributesKey INTEGER =
            createTextAttributesKey("INTEGER_TERM", DefaultLanguageHighlighterColors.NUMBER);

    public static final TextAttributesKey FLOAT =
            createTextAttributesKey("FLOAT_TERM", DefaultLanguageHighlighterColors.NUMBER);

    public static final TextAttributesKey BIN_NUMBER =
            createTextAttributesKey("BIN_NUMBER", DefaultLanguageHighlighterColors.NUMBER);

    public static final TextAttributesKey OCT_NUMBER =
            createTextAttributesKey("OCT_NUMBER", DefaultLanguageHighlighterColors.NUMBER);

    public static final TextAttributesKey HEX_NUMBER =
            createTextAttributesKey("HEX_NUMBER", DefaultLanguageHighlighterColors.NUMBER);

    public static final TextAttributesKey CHAR_CODE =
            createTextAttributesKey("CHAR_CODE", DefaultLanguageHighlighterColors.NUMBER);

    public static final TextAttributesKey QUOTED_ATOM =
            createTextAttributesKey("QUOTED_ATOM", DefaultLanguageHighlighterColors.STRING);

    public static final TextAttributesKey UNQUOTED_ATOM =
            createTextAttributesKey("UNQUOTED_ATOM", DefaultLanguageHighlighterColors.IDENTIFIER);

    public static final TextAttributesKey KEYWORD_ATOM =
            createTextAttributesKey("KEYWORD_ATOM", DefaultLanguageHighlighterColors.STATIC_FIELD);

    public static final TextAttributesKey HEAD_UNQUOTED_ATOM =
            createTextAttributesKey("HEAD_UNQUOTED_ATOM", DefaultLanguageHighlighterColors.CONSTANT);

    public static final TextAttributesKey HEAD_KEYWORD_ATOM =
            createTextAttributesKey("HEAD_KEYWORD_ATOM", DefaultLanguageHighlighterColors.STATIC_FIELD);

    public static final TextAttributesKey DIRECTIVE_UNQUOTED_ATOM =
            createTextAttributesKey("DIRECTIVE_UNQUOTED_ATOM", DefaultLanguageHighlighterColors.IDENTIFIER); //unrecognized keyword

    public static final TextAttributesKey DIRECTIVE_KEYWORD_ATOM =
            createTextAttributesKey("DIRECTIVE_KEYWORD_ATOM", DefaultLanguageHighlighterColors.MARKUP_ATTRIBUTE);

    public static final TextAttributesKey QUOTED_COMPOUND_NAME =
            createTextAttributesKey("QUOTED_COMPOUND_NAME", DefaultLanguageHighlighterColors.STRING);

    public static final TextAttributesKey UNQUOTED_COMPOUND_NAME =
            createTextAttributesKey("UNQUOTED_COMPOUND_NAME", DefaultLanguageHighlighterColors.IDENTIFIER);

    public static final TextAttributesKey KEYWORD_COMPOUND_NAME =
            createTextAttributesKey("KEYWORD_COMPOUND_NAME", DefaultLanguageHighlighterColors.STATIC_FIELD);

    public static final TextAttributesKey HEAD_UNQUOTED_COMPOUND_NAME =
            createTextAttributesKey("HEAD_UNQUOTED_COMPOUND_NAME", DefaultLanguageHighlighterColors.CONSTANT);

    public static final TextAttributesKey HEAD_KEYWORD_COMPOUND_NAME =
            createTextAttributesKey("HEAD_KEYWORD_COMPOUND_NAME", DefaultLanguageHighlighterColors.STATIC_FIELD);

    public static final TextAttributesKey DIRECTIVE_UNQUOTED_COMPOUND_NAME =
            createTextAttributesKey("DIRECTIVE_UNQUOTED_COMPOUND_NAME", DefaultLanguageHighlighterColors.IDENTIFIER); //unrecognized keyword

    public static final TextAttributesKey DIRECTIVE_KEYWORD_COMPOUND_NAME =
            createTextAttributesKey("DIRECTIVE_KEYWORD_COMPOUND_NAME", DefaultLanguageHighlighterColors.MARKUP_ATTRIBUTE);


    public static final TextAttributesKey ANONYMOUS_VARIABLE =
            createTextAttributesKey("ANONYMOUS_VARIABLE", DefaultLanguageHighlighterColors.LABEL);

    public static final TextAttributesKey NAMED_VARIABLE =
            createTextAttributesKey("NAMED_VARIABLE", DefaultLanguageHighlighterColors.LABEL);

    public static final TextAttributesKey PARENTHESIS =
            createTextAttributesKey("PARENTHESIS", DefaultLanguageHighlighterColors.PARENTHESES);

    public static final TextAttributesKey COMMENT =
            createTextAttributesKey("PROLOG_COMMENT", DefaultLanguageHighlighterColors.LINE_COMMENT);

    public static final TextAttributesKey BAD_CHARACTER =
            createTextAttributesKey("PROLOG_BAD_CHARACTER", HighlighterColors.BAD_CHARACTER);



    private static final TextAttributesKey[] BAD_CHAR_KEYS = new TextAttributesKey[]{BAD_CHARACTER};
    private static final TextAttributesKey[] DIRECTIVE_OP_KEYS = new TextAttributesKey[]{DIRECTIVE_OP};
    private static final TextAttributesKey[] LONG_ARROW_OP_KEYS = new TextAttributesKey[]{LONG_ARROW_OP};
    private static final TextAttributesKey[] OPERATOR_KEYS = new TextAttributesKey[]{OPERATOR};
    private static final TextAttributesKey[] CUT_KEYS = new TextAttributesKey[]{CUT};
    private static final TextAttributesKey[] STRING_KEYS = new TextAttributesKey[]{STRING};
    private static final TextAttributesKey[] INTEGER_KEYS = new TextAttributesKey[]{INTEGER};
    private static final TextAttributesKey[] FLOAT_KEYS = new TextAttributesKey[]{FLOAT};
    private static final TextAttributesKey[] BIN_NUMBER_KEYS = new TextAttributesKey[]{BIN_NUMBER};
    private static final TextAttributesKey[] OCT_NUMBER_KEYS = new TextAttributesKey[]{OCT_NUMBER};
    private static final TextAttributesKey[] HEX_NUMBER_KEYS = new TextAttributesKey[]{HEX_NUMBER};
    private static final TextAttributesKey[] CHAR_CODE_KEYS = new TextAttributesKey[]{CHAR_CODE};
    private static final TextAttributesKey[] QUOTED_ATOM_KEYS = new TextAttributesKey[]{QUOTED_ATOM};
    private static final TextAttributesKey[] UNQUOTED_ATOM_KEYS = new TextAttributesKey[]{UNQUOTED_ATOM};
    private static final TextAttributesKey[] KEYWORD_ATOM_KEYS = new TextAttributesKey[]{KEYWORD_ATOM};
    private static final TextAttributesKey[] HEAD_UNQUOTED_ATOM_KEYS = new TextAttributesKey[]{HEAD_UNQUOTED_ATOM};
    private static final TextAttributesKey[] HEAD_KEYWORD_ATOM_KEYS = new TextAttributesKey[]{HEAD_KEYWORD_ATOM};
    private static final TextAttributesKey[] DIRECTIVE_UNQUOTED_ATOM_KEYS = new TextAttributesKey[]{DIRECTIVE_UNQUOTED_ATOM};
    private static final TextAttributesKey[] DIRECTIVE_KEYWORD_ATOM_KEYS = new TextAttributesKey[]{DIRECTIVE_KEYWORD_ATOM};
    private static final TextAttributesKey[] QUOTED_COMPOUND_NAME_KEYS = new TextAttributesKey[]{QUOTED_COMPOUND_NAME};
    private static final TextAttributesKey[] UNQUOTED_COMPOUND_NAME_KEYS = new TextAttributesKey[]{UNQUOTED_COMPOUND_NAME};
    private static final TextAttributesKey[] KEYWORD_COMPOUND_NAME_KEYS = new TextAttributesKey[]{KEYWORD_COMPOUND_NAME};
    private static final TextAttributesKey[] HEAD_UNQUOTED_COMPOUND_NAME_KEYS = new TextAttributesKey[]{HEAD_UNQUOTED_COMPOUND_NAME};
    private static final TextAttributesKey[] HEAD_KEYWORD_COMPOUND_NAME_KEYS = new TextAttributesKey[]{HEAD_KEYWORD_COMPOUND_NAME};
    private static final TextAttributesKey[] DIRECTIVE_UNQUOTED_COMPOUND_NAME_KEYS = new TextAttributesKey[]{DIRECTIVE_UNQUOTED_COMPOUND_NAME};
    private static final TextAttributesKey[] DIRECTIVE_KEYWORD_COMPOUND_NAME_KEYS = new TextAttributesKey[]{DIRECTIVE_KEYWORD_COMPOUND_NAME};
    private static final TextAttributesKey[] ANONYMOUS_VARIABLE_KEYS = new TextAttributesKey[]{ANONYMOUS_VARIABLE};
    private static final TextAttributesKey[] NAMED_VARIABLE_KEYS = new TextAttributesKey[]{NAMED_VARIABLE};
    private static final TextAttributesKey[] PARENTHESIS_KEYS = new TextAttributesKey[]{PARENTHESIS};
    private static final TextAttributesKey[] COMMENT_KEYS = new TextAttributesKey[]{COMMENT};
    private static final TextAttributesKey[] EMPTY_ATTRIBUTES = new TextAttributesKey[0];

    @NotNull
    @Override
    public Lexer getHighlightingLexer() {
        return new LogtalkLexerAdapter();
    }

    @NotNull
    @Override
    public TextAttributesKey[] getTokenHighlights(IElementType tokenType) {
        if (tokenType.equals(LogtalkTypes.DIRECTIVE_OP)) {
            return DIRECTIVE_OP_KEYS;
        } else if (tokenType.equals(LogtalkTypes.LONG_ARROW_OP)) {
            return LONG_ARROW_OP_KEYS;
        } else if (isOperator(tokenType)) {
            return OPERATOR_KEYS;
        } else if (tokenType.equals(LogtalkTypes.CUT)) {
            return CUT_KEYS;
        } else if (tokenType.equals(LogtalkTypes.STRING)) {
            return STRING_KEYS;
        } else if (tokenType.equals(LogtalkTypes.INTEGER)) {
            return INTEGER_KEYS;
        } else if (tokenType.equals(LogtalkTypes.FLOAT)) {
            return FLOAT_KEYS;
        } else if (tokenType.equals(LogtalkTypes.BIN_NUMBER)) {
            return BIN_NUMBER_KEYS;
        } else if (tokenType.equals(LogtalkTypes.OCT_NUMBER)) {
            return OCT_NUMBER_KEYS;
        } else if (tokenType.equals(LogtalkTypes.HEX_NUMBER)) {
            return HEX_NUMBER_KEYS;
        } else if (tokenType.equals(LogtalkTypes.CHAR_CODE)) {
            return CHAR_CODE_KEYS;
        } else if (tokenType.equals(LogtalkTypes.QUOTED_ATOM)) {
            return QUOTED_ATOM_KEYS;
        } else if (tokenType.equals(LogtalkTypes.UNQUOTED_ATOM)) {
            return UNQUOTED_ATOM_KEYS;
        } else if (tokenType.equals(LogtalkTypes.KEYWORD_ATOM)) {
            return KEYWORD_ATOM_KEYS;
        } else if (tokenType.equals(LogtalkTypes.HEAD_UNQUOTED_ATOM)) {
            return HEAD_UNQUOTED_ATOM_KEYS;
        } else if (tokenType.equals(LogtalkTypes.HEAD_KEYWORD_ATOM)) {
            return HEAD_KEYWORD_ATOM_KEYS;
        } else if (tokenType.equals(LogtalkTypes.DIRECTIVE_UNQUOTED_ATOM)) {
            return DIRECTIVE_UNQUOTED_ATOM_KEYS;
        } else if (tokenType.equals(LogtalkTypes.DIRECTIVE_KEYWORD_ATOM)) {
            return DIRECTIVE_KEYWORD_ATOM_KEYS;
        } else if (tokenType.equals(LogtalkTypes.QUOTED_COMPOUND_NAME)) {
            return QUOTED_COMPOUND_NAME_KEYS;
        } else if (tokenType.equals(LogtalkTypes.UNQUOTED_COMPOUND_NAME)) {
            return UNQUOTED_COMPOUND_NAME_KEYS;
        } else if (tokenType.equals(LogtalkTypes.KEYWORD_COMPOUND_NAME)) {
            return KEYWORD_COMPOUND_NAME_KEYS;
        } else if (tokenType.equals(LogtalkTypes.HEAD_UNQUOTED_COMPOUND_NAME)) {
            return HEAD_UNQUOTED_COMPOUND_NAME_KEYS;
        } else if (tokenType.equals(LogtalkTypes.HEAD_KEYWORD_COMPOUND_NAME)) {
            return HEAD_KEYWORD_COMPOUND_NAME_KEYS;
        } else if (tokenType.equals(LogtalkTypes.DIRECTIVE_UNQUOTED_COMPOUND_NAME)) {
            return DIRECTIVE_UNQUOTED_COMPOUND_NAME_KEYS;
        } else if (tokenType.equals(LogtalkTypes.DIRECTIVE_KEYWORD_COMPOUND_NAME)) {
            return DIRECTIVE_KEYWORD_COMPOUND_NAME_KEYS;
        } else if (tokenType.equals(LogtalkTypes.ANONYMOUS_VARIABLE)) {
            return ANONYMOUS_VARIABLE_KEYS;
        } else if (tokenType.equals(LogtalkTypes.NAMED_VARIABLE)) {
            return NAMED_VARIABLE_KEYS;
        } else if (isParenthesis(tokenType)) {
            return PARENTHESIS_KEYS;
        } else if (tokenType.equals(LogtalkTypes.COMMENT)) {
            return COMMENT_KEYS;
        } else if (tokenType.equals(TokenType.BAD_CHARACTER)) {
            return BAD_CHAR_KEYS;
        } else {
            return EMPTY_ATTRIBUTES;
        }
    }

    private static boolean isParenthesis(IElementType tokenType) {
        return tokenType.equals(LogtalkTypes.LPAREN) || tokenType.equals(LogtalkTypes.RPAREN);
    }

    private static boolean isOperator(IElementType tokenType) {
        return //tokenType.equals(LogtalkTypes.LONG_ARROW_OP) || //these operators have a different treatment.
                //tokenType.equals(LogtalkTypes.DIRECTIVE_OP) ||
                tokenType.equals(LogtalkTypes.QUERY_OP) ||
                tokenType.equals(LogtalkTypes.DYNAMIC_OP) ||
                tokenType.equals(LogtalkTypes.DISCONTIGUOUS_OP) ||
                tokenType.equals(LogtalkTypes.INITIALIZATION_OP) ||
                tokenType.equals(LogtalkTypes.META_PREDICATE_OP) ||
                tokenType.equals(LogtalkTypes.MODULE_TRANSPARENT_OP) ||
                tokenType.equals(LogtalkTypes.MULTIFILE_OP) ||
                tokenType.equals(LogtalkTypes.PUBLIC_OP) ||
                tokenType.equals(LogtalkTypes.THREAD_LOCAL_OP) ||
                tokenType.equals(LogtalkTypes.THREAD_INITIALIZATION_OP) ||
                tokenType.equals(LogtalkTypes.VOLATILE_OP) ||
                tokenType.equals(LogtalkTypes.SEMICOLON_OP) ||
                tokenType.equals(LogtalkTypes.PIPE_OP) ||
                tokenType.equals(LogtalkTypes.IMPLICATION_OP) ||
                tokenType.equals(LogtalkTypes.STAR_IMPLICATION_OP) ||
                tokenType.equals(LogtalkTypes.COMMA) ||
                tokenType.equals(LogtalkTypes.COLON_EQUALS_OP) ||
                tokenType.equals(LogtalkTypes.NOT_OP) ||
                tokenType.equals(LogtalkTypes.LESS_THAN_OP) ||
                tokenType.equals(LogtalkTypes.EQUALS_OP) ||
                tokenType.equals(LogtalkTypes.EQUALS_DOT_DOT_OP) ||
                tokenType.equals(LogtalkTypes.EQUALS_AT_EQUALS_OP) ||
                tokenType.equals(LogtalkTypes.BACKSLASH_EQUALS_AT_EQUALS_OP) ||
                tokenType.equals(LogtalkTypes.EQUALS_COLON_EQUALS_OP) ||
                tokenType.equals(LogtalkTypes.LESS_OR_EQUALS_OP) ||
                tokenType.equals(LogtalkTypes.DOUBLE_EQUALS_OP) ||
                tokenType.equals(LogtalkTypes.EQUALS_BACKSLASH_EQUALS_OP) ||
                tokenType.equals(LogtalkTypes.GREATER_THAN_OP) ||
                tokenType.equals(LogtalkTypes.GREATER_OR_EQUALS_OP) ||
                tokenType.equals(LogtalkTypes.AT_LESS_THAN_OP) ||
                tokenType.equals(LogtalkTypes.AT_EQUALS_LESS_THAN_OP) ||
                tokenType.equals(LogtalkTypes.AT_GREATER_THAN_OP) ||
                tokenType.equals(LogtalkTypes.AT_GREATER_OR_EQUALS_OP) ||
                tokenType.equals(LogtalkTypes.BACKSLASH_EQUALS_OP) ||
                tokenType.equals(LogtalkTypes.BACKSLASH_DOUBLE_EQUALS_OP) ||
                tokenType.equals(LogtalkTypes.AS_OP) ||
                tokenType.equals(LogtalkTypes.IS_OP) ||
                tokenType.equals(LogtalkTypes.BUTTERFLY_OP) ||
                tokenType.equals(LogtalkTypes.COLON_LESS_THAN_OP) ||
                tokenType.equals(LogtalkTypes.COLON_OP) ||
                tokenType.equals(LogtalkTypes.PLUS_OP) ||
                tokenType.equals(LogtalkTypes.MINUS_OP) ||
                tokenType.equals(LogtalkTypes.SLASH_BACKSLASH_OP) ||
                tokenType.equals(LogtalkTypes.BACKSLASH_SLASH_OP) ||
                tokenType.equals(LogtalkTypes.XOR_OP) ||
                tokenType.equals(LogtalkTypes.QUESTION_MARK_OP) ||
                tokenType.equals(LogtalkTypes.STAR_OP) ||
                tokenType.equals(LogtalkTypes.SLASH_OP) ||
                tokenType.equals(LogtalkTypes.DOUBLE_SLASH_OP) ||
                tokenType.equals(LogtalkTypes.DIV_OP) ||
                tokenType.equals(LogtalkTypes.RDIV_OP) ||
                //tokenType.equals(LogtalkTypes.DOUBLE_LESS_THAN_OP) ||
                tokenType.equals(LogtalkTypes.DOUBLE_GREATER_THAN_OP) ||
                tokenType.equals(LogtalkTypes.MOD_OP) ||
                tokenType.equals(LogtalkTypes.REM_OP) ||
                tokenType.equals(LogtalkTypes.DOUBLE_STAR_OP) ||
                tokenType.equals(LogtalkTypes.CARET_OP) ||
                tokenType.equals(LogtalkTypes.BACKSLASH_OP) ||
                tokenType.equals(LogtalkTypes.DOT) ||
                tokenType.equals(LogtalkTypes.MAP_OP) ||
                tokenType.equals(LogtalkTypes.DOLLAR_OP) ||
                tokenType.equals(LogtalkTypes.AT) ||
                tokenType.equals(LogtalkTypes.LGT_METHOD_CALL_OP) ||
                tokenType.equals(LogtalkTypes.LGT_SUPER_CALL_OP)||
                tokenType.equals(LogtalkTypes.LGT_CONTEXT_SWITCHING_OP);
    }

}