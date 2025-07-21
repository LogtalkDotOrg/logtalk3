// CodeMirror 6 Logtalk language support
// Copyright (c) by Marijn Haverbeke and others
// Distributed under an MIT license: https://codemirror.net/LICENSE

// Parts from Ace; see <https://raw.githubusercontent.com/ajaxorg/ace/master/LICENSE>
// Author: Paulo Moura

import { StreamLanguage } from "@codemirror/language"

// Logtalk stream parser for CodeMirror 6
const logtalkParser = {
  name: "logtalk",

  startState() {
    return {
      inComment: false,
      inString: false,
      stringDelim: null
    };
  },

  token(stream, state) {
    // Handle block comments
    if (state.inComment) {
      if (stream.match(/\*\//)) {
        state.inComment = false;
        return "comment";
      }
      stream.next();
      return "comment";
    }

    // Handle strings
    if (state.inString) {
      if (stream.match(state.stringDelim)) {
        state.inString = false;
        state.stringDelim = null;
        return "string";
      }
      if (stream.match(/\\./)) {
        return "string";
      }
      stream.next();
      return "string";
    }

    // Start of block comment
    if (stream.match(/\/\*/)) {
      state.inComment = true;
      return "comment";
    }

    // Line comment
    if (stream.match(/%.*$/)) {
      return "comment";
    }

    // String literals
    if (stream.match(/'/)) {
      state.inString = true;
      state.stringDelim = "'";
      return "string";
    }
    if (stream.match(/"/)) {
      state.inString = true;
      state.stringDelim = '"';
      return "string";
    }

    // Object/protocol/category/module declarations
    if (stream.match(/:-\s*(?:object|protocol|category|module)(?=\()/)) {
      return "keyword";
    }

    // End declarations
    if (stream.match(/:-\s*end_(?:object|protocol|category)(?=\.)/)) {
      return "keyword";
    }

    // Relations
    if (stream.match(/\b(?:complements|extends|instantiates|imports|implements|specializes)(?=\()/)) {
      return "keyword";
    }

    // Directives
    if (stream.match(/:-\s*(?:else|endif|built_in|dynamic|synchronized|threaded)(?=\.)/)) {
      return "keyword";
    }

    if (stream.match(/:-\s*(?:calls|coinductive|elif|encoding|ensure_loaded|export|if|include|initialization|info|reexport|set_(?:logtalk|prolog)_flag|uses)(?=\()/)) {
      return "keyword";
    }

    if (stream.match(/:-\s*(?:alias|info|dynamic|discontiguous|meta_(?:non_terminal|predicate)|mode|multifile|public|protected|private|op|uses|use_module|synchronized)(?=\()/)) {
      return "keyword";
    }

    // Message sending operators
    if (stream.match(/[:^]{1,2}/)) {
      return "operator";
    }

    // External call operators
    if (stream.match(/[{}]/)) {
      return "operator";
    }

    // Mode operators
    if (stream.match(/[?@]/)) {
      return "operator";
    }

    // Comparison operators
    if (stream.match(/@(?:=<|<|>|>=)|==|\\==/)) {
      return "operator";
    }

    if (stream.match(/=<|[<>]=?|=:=|=\\=/)) {
      return "operator";
    }

    // Bitwise operators
    if (stream.match(/<<|>>|\/\\|\\\/|\\/)) {
      return "operator";
    }

    // Arithmetic operators
    if (stream.match(/\*\*|[+\-*/]|\/\//)) {
      return "operator";
    }

    // Evaluable functions
    if (stream.match(/\b(?:e|pi|div|mod|rem)\b(?![-!(^~])/)) {
      return "builtin";
    }

    // Misc operators
    if (stream.match(/:-|!|\\+|[,;]|-->|->|=|\\=|\.|\.\.|\\^|\bas\b|\bis\b/)) {
      return "operator";
    }

    // Built-in predicates - evaluable functions
    if (stream.match(/\b(?:abs|acos|asin|atan|atan2|ceiling|cos|div|exp|float(?:_(?:integer|fractional)_part)?|floor|log|max|min|mod|rem|round|sign|sin|sqrt|tan|truncate|xor)(?=\()/)) {
      return "builtin";
    }

    // Control predicates
    if (stream.match(/\b(?:true|fail|false|repeat|(?:instantiation|system)_error)\b(?![-!(^~])/)) {
      return "builtin";
    }

    if (stream.match(/\b(?:uninstantiation|type|domain|consistency|existence|permission|representation|evaluation|resource|syntax)_error(?=\()/)) {
      return "builtin";
    }

    if (stream.match(/\b(?:call|catch|ignore|throw|once)(?=\()/)) {
      return "builtin";
    }

    // I/O predicates
    if (stream.match(/\b(?:(?:get|peek|put)_(?:char|code|byte)|nl)(?=\()/)) {
      return "builtin";
    }

    if (stream.match(/\bnl\b/)) {
      return "builtin";
    }

    // Atom/term processing
    if (stream.match(/\b(?:atom_(?:length|chars|concat|codes)|sub_atom|char_code|number_(?:char|code)s)(?=\()/)) {
      return "builtin";
    }

    // Term testing
    if (stream.match(/\b(?:var|atomic?|integer|float|callable|compound|nonvar|number|ground|acyclic_term)(?=\()/)) {
      return "builtin";
    }

    // Numbers
    if (stream.match(/\b(?:0b[01]+|0o[0-7]+|0x[0-9a-fA-F]+)\b/)) {
      return "number";
    }

    if (stream.match(/\b0'[\\]?.\b/)) {
      return "number";
    }

    if (stream.match(/\b\d+\.?\d*(?:[eE][+-]?\d+)?\b/)) {
      return "number";
    }

    // Variables
    if (stream.match(/\b[A-Z_][A-Za-z0-9_]*\b/)) {
      return "variable";
    }

    // Skip whitespace
    if (stream.match(/\s+/)) {
      return null;
    }

    // Default: consume one character
    stream.next();
    return null;
  },

  languageData: {
    commentTokens: { line: "%", block: { open: "/*", close: "*/" } },
    closeBrackets: { brackets: ["(", "[", "{", "'", '"'] },
    indentOnInput: /^\s*(?:\}|end_(?:object|protocol|category)\.)/
  }
};
// Create the language support
export const logtalk = StreamLanguage.define(logtalkParser);

// Export for MIME type registration
export function logtalkLanguage() {
  return logtalk;
}
