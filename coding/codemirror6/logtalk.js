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

    // Quoted atom
    if (stream.match(/'/)) {
      state.inString = true;
      state.stringDelim = "'";
      return "string";
    }

    // Double-quoted term
    if (stream.match(/"/)) {
      state.inString = true;
      state.stringDelim = '"';
      return "string";
    }

    // Entity opening directives
    if (stream.match(/:-\s(?:object|protocol|category|module)(?=\()/)) {
      return "keyword";
    }

    // End entity directives
    if (stream.match(/:-\send_(?:object|protocol|category)(?=\.)/)) {
      return "keyword";
    }

    // Entity relations
    if (stream.match(/\b(?:complements|extends|instantiates|imports|implements|specializes)(?=\()/)) {
      return "keyword";
    }

    // Other directives
    if (stream.match(/:-\s(?:else|endif|built_in|dynamic|synchronized|threaded)(?=\.)/)) {
      return "keyword";
    }
    if (stream.match(/:-\s(?:calls|coinductive|elif|encoding|ensure_loaded|export|if|include|initialization|info|reexport|set_(?:logtalk|prolog)_flag|uses)(?=\()/)) {
      return "keyword";
    }
    if (stream.match(/:-\s(?:alias|info|dynamic|discontiguous|meta_(?:non_terminal|predicate)|mode|multifile|public|protected|private|op|uses|use_module|synchronized)(?=\()/)) {
      return "keyword";
    }

    // Message sending operator
    if (stream.match(/::/)) {
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
    if (stream.match(/\*\*|[+\-*\/]|\/\//)) {
      return "operator";
    }

    // Evaluable functions
    if (stream.match(/\b(?:e|pi|div|mod|rem)\b(?![_!(^~])/)) {
      return "builtin";
    }

    // Misc operators
    if (stream.match(/:-|!|\\+|[,;]|-->|->|=|\\=|\.|\.\.|\^|\bas\b|\bis\b/)) {
      return "operator";
    }

    // Built-in predicates - evaluable functions
    if (stream.match(/\b(?:abs|acos|asin|atan|atan2|ceiling|cos|div|exp|float(?:_(?:integer|fractional)_part)?|floor|log|max|min|mod|rem|round|sign|sin|sqrt|tan|truncate|xor)(?=\()/)) {
      return "builtin";
    }

    // Control predicates
    if (stream.match(/\b(?:true|fail|false|repeat|(?:instantiation|system)_error)\b(?![_!(^~])/)) {
      return "builtin";
    }
    if (stream.match(/\b(?:uninstantiation|type|domain|consistency|existence|permission|representation|evaluation|resource|syntax)_error(?=\()/)) {
      return "builtin";
    }
    if (stream.match(/\b(?:call|catch|ignore|throw|once)(?=\()/)) {
      return "builtin";
    }

    // Event handlers
    if (stream.match(/\b(after|before)(?=\()/)) {
      return "builtin";
    }
    
    // Message forwarding handler
    if (stream.match(/\bforward(?=\()/)) {
      return "builtin";
    }
    // Execution-context methods
    if (stream.match(/\b(context|parameter|this|se(lf|nder))(?=\()/)) {
      return "builtin";
    }
    // Reflection
    if (stream.match(/\b(current_predicate|predicate_property)(?=\()/)) {
      return "builtin";
    }
    // DCGs and term expansion
    if (stream.match(/\b(expand_(goal|term)|(goal|term)_expansion|phrase)(?=\()/)) {
      return "builtin";
    }

    // Entity creation and destruction
    if (stream.match(/\b(abolish|c(reate|urrent))_(object|protocol|category)(?=\()/)) {
      return "builtin";
    }

    // Entity properties
    if (stream.match(/\b(object|protocol|category)_property(?=\()/)) {
      return "builtin";
    }

    // Entity relations
    if (stream.match(/\bco(mplements_object|nforms_to_protocol)(?=\()/)) {
      return "builtin";
    }
    if (stream.match(/\bextends_(object|protocol|category)(?=\()/)) {
      return "builtin";
    }
    if (stream.match(/\bimp(lements_protocol|orts_category)(?=\()/)) {
      return "builtin";
    }
    if (stream.match(/\b(instantiat|specializ)es_class(?=\()/)) {
      return "builtin";
    }

    // Events
    if (stream.match(/\b(current_event|(abolish|define)_events)(?=\()/)) {
      return "builtin";
    }

    // Flags
    if (stream.match(/\b(create|current|set)_logtalk_flag(?=\()/)) {
      return "builtin";
    }

    // Compiling, loading, and library paths
    if (stream.match(/\blogtalk_(compile|l(ibrary_path|oad|oad_context)|make(_target_action)?)(?=\()/)) {
      return "builtin";
    }
    if (stream.match(/\blogtalk_make\b/)) {
      return "builtin";
    }

    // Database
    if (stream.match(/\b(clause|retract(all)?)(?=\()/)) {
      return "builtin";
    }
    if (stream.match(/\ba(bolish|ssert(a|z))(?=\()/)) {
      return "builtin";
    }

    // All solutions
    if (stream.match(/\b((bag|set)of|f(ind|or)all)(?=\()/)) {
      return "builtin";
    }

    // Multi-threading predicates
    if (stream.match(/\bthreaded(_(ca(ll|ncel)|once|ignore|exit|peek|wait|notify))?(?=\()/)) {
      return "builtin";
    }

    // Engine predicates
    if (stream.match(/\bthreaded_engine(_(create|destroy|self|next|next_reified|yield|post|fetch))?(?=\()/)) {
      return "builtin";
    }

    // Term unification
    if (stream.match(/\b(subsumes_term|unify_with_occurs_check)(?=\()/)) {
      return "builtin";
    }

    // Term creation and decomposition
    if (stream.match(/\b(functor|arg|copy_term|numbervars|term_variables)(?=\()/)) {
      return "builtin";
    }

    // Stream selection and control
    if (stream.match(/\b(curren|se)t_(in|out)put(?=\()/)) {
      return "builtin";
    }
    if (stream.match(/\b(open|close)(?=[(])(?=\()/)) {
      return "builtin";
    }
    if (stream.match(/\bflush_output(?=[(])(?=\()/)) {
      return "builtin";
    }
    if (stream.match(/\b(at_end_of_stream|flush_output)\b/)) {
      return "builtin";
    }
    if (stream.match(/\b(stream_property|at_end_of_stream|set_stream_position)(?=\()/)) {
      return "builtin";
    }

    // Character and byte input/output
    if (stream.match(/\b(?:(?:get|peek|put)_(?:char|code|byte)|nl)(?=\()/)) {
      return "builtin";
    }
    if (stream.match(/\bnl\b/)) {
      return "builtin";
    }

    // Term input/output
    if (stream.match(/\bread(_term)?(?=\()/)) {
      return "builtin";
    }
    if (stream.match(/\bwrite(q|_(canonical|term))?(?=\()/)) {
      return "builtin";
    }
      if (stream.match(/\b(current_)?op(?=\()/)) {
      return "builtin";
    }
    if (stream.match(/\b(current_)?char_conversion(?=\()/)) {
      return "builtin";
    }

    // Atom/term processing
    if (stream.match(/\b(?:atom_(?:length|chars|concat|codes)|sub_atom|char_code|number_(?:char|code)s)(?=\()/)) {
      return "builtin";
    }

    // Term testing
    if (stream.match(/\b(?:var|atom(ic)?|integer|float|callable|compound|nonvar|number|ground|acyclic_term)(?=\()/)) {
      return "builtin";
    }

    // Term comparison
    if (stream.match(/\bcompare(?=\()/)) {
      return "builtin";
    }

    // Sorting
    if (stream.match(/\b(key)?sort(?=\()/)) {
      return "builtin";
    }

    // Implementation defined hooks functions
    if (stream.match(/\b(se|curren)t_prolog_flag(?=\()/)) {
      return "builtin";
    }
    if (stream.match(/\bhalt\b/)) {
      return "builtin";
    }
    if (stream.match(/\bhalt(?=\()/)) {
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
