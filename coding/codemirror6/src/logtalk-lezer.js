// CodeMirror 6 Logtalk language support using Lezer parser
// Copyright (c) by Marijn Haverbeke and others
// Distributed under an MIT license: https://codemirror.net/LICENSE

// Parts from Ace; see <https://raw.githubusercontent.com/ajaxorg/ace/master/LICENSE>
// Author: Paulo Moura

import { LRLanguage, LanguageSupport, indentNodeProp, foldNodeProp, foldInside, delimitedIndent } from "@codemirror/language"
import { styleTags, tags as t } from "@lezer/highlight"
import { parser } from "./parser.js"

export const logtalkLanguage = LRLanguage.define({
  parser: parser.configure({
    props: [
      indentNodeProp.add({
        EntityDefinition: context => {
          // Custom indentation for entity definitions
          const closing = context.node.getChild("EntityClosing")
          if (closing && context.pos >= closing.from) {
            return context.baseIndent
          }
          return context.baseIndent + context.unit
        },
        EntityOpening: context => context.baseIndent + context.unit,
        EntityBody: context => context.baseIndent + context.unit,
        Clause: context => {
          const body = context.node.getChild("ClauseBody")
          if (body && context.pos >= body.from) {
            return context.baseIndent + context.unit
          }
          return context.baseIndent
        },
        ClauseBody: context => context.baseIndent + context.unit,
        Goals: context => context.baseIndent + context.unit,
        CompoundTerm: delimitedIndent({ closing: ")", align: false }),
        ArgumentList: context => context.baseIndent + context.unit,
        List: delimitedIndent({ closing: "]", align: false }),
        ListElements: context => context.baseIndent + context.unit,
        IfThenElse: delimitedIndent({ closing: ")", align: false }),
        ExternalCall: delimitedIndent({ closing: "}", align: false }),
        "( )": delimitedIndent({ closing: ")", align: false }),
        "[ ]": delimitedIndent({ closing: "]", align: false }),
        "{ }": delimitedIndent({ closing: "}", align: false })
      }),
      foldNodeProp.add({
        EntityDefinition: foldInside,
        CompoundTerm: foldInside,
        List: foldInside,
        IfThenElse: foldInside,
        ExternalCall: foldInside,
        BlockComment: foldInside
      }),
      styleTags({
        // Comments
        LineComment: t.lineComment,
        BlockComment: t.blockComment,
        
        // Literals
        Number: t.number,
        String: t.string,
        QuotedAtom: t.atom,
        Variable: t.variableName,
        
        // Identifiers and atoms
        identifier: t.name,
        Atom: t.atom,
        
        // Operators
        Operator: t.operator,
        ":-": t.operator,
        "::": t.operator,
        ":": t.operator,
        "-->": t.operator,
        "\\+": t.operator,
        "=": t.operator,
        "\\=": t.operator,
        "==": t.operator,
        "\\==": t.operator,
        "is": t.operator,
        "as": t.operator,
        "mod": t.operator,
        "rem": t.operator,
        "div": t.operator,
        
        // Punctuation
        ".": t.punctuation,
        ",": t.separator,
        ";": t.separator,
        "|": t.separator,
        "( )": t.paren,
        "[ ]": t.squareBracket,
        "{ }": t.brace,
        
        // Entity definitions
        EntityType: t.keyword,
        EntityOpening: t.meta,
        EntityClosing: t.meta,
        EndEntityType: t.keyword,
        
        // Directives
        Directive: t.meta,
        DirectiveName: t.keyword,
        
        // Entity relations
        RelationType: t.keyword,
        ScopeOperator: t.modifier,
        
        // Control constructs
        Cut: t.keyword,
        IfThenElse: t.operator,
        Negation: t.keyword,
        ExternalCall: t.meta,
        
        // Message sending
        MessageSending: t.special(t.operator),
        ModuleQualification: t.special(t.operator),
        
        // Terms
        CompoundTerm: t.function(t.name),
        Functor: t.function(t.name),
        List: t.bracket,

        // Clauses
        Clause: t.definition(t.name),
        ClauseHead: t.definition(t.name),
        ClauseBody: t.content,
        Goal: t.name,
        Goals: t.content
      })
    ]
  }),
  languageData: {
    name: "logtalk",
    extensions: [".lgt", ".logtalk"],
    commentTokens: { line: "%", block: { open: "/*", close: "*/" } },
    closeBrackets: { brackets: ["(", "[", "{", "'", '"'] },
    indentOnInput: /^\s*(?::-\s*(?:end_(?:object|protocol|category)|else|elif|endif)\.|[)\]}]|.*\.$)/
  }
})

export function logtalk() {
  return new LanguageSupport(logtalkLanguage)
}

// Enhanced indentation function that uses the Lezer parser
export function logtalkIndentService(context) {
  try {
    const { state, pos } = context
    const tree = logtalkLanguage.parser.parse(state.doc.toString())

    // Get the current line
    const line = state.doc.lineAt(pos)
    const lineText = line.text

    // Get the node at the current position - add bounds checking
    const docLength = state.doc.length
    const safePos = Math.max(0, Math.min(pos, docLength))
    const node = tree.resolveInner(safePos, -1)

  // Calculate base indentation from parent nodes
  let baseIndent = 0
  let current = node

  // Walk up the tree to calculate indentation level
  while (current && current.parent) {
    const parent = current.parent

    switch (parent.type.name) {
      case "EntityDefinition":
        // Inside an entity definition
        baseIndent += 1
        break
      case "EntityBody":
        // Inside entity body (already counted by EntityDefinition)
        break
      case "Clause":
        // Inside a clause
        const clauseBody = parent.getChild("ClauseBody")
        if (clauseBody && current.from >= clauseBody.from) {
          // We're in the clause body
          baseIndent += 1
        }
        break
      case "CompoundTerm":
        // Inside compound term arguments
        if (current.type.name === "ArgumentList") {
          baseIndent += 1
        }
        break
      case "List":
        // Inside list elements
        if (current.type.name === "ListElements") {
          baseIndent += 1
        }
        break
      case "IfThenElse":
        // Inside if-then-else construct
        baseIndent += 1
        break
      case "ExternalCall":
        // Inside external call braces
        baseIndent += 1
        break
      case "Goals":
        // Inside goal sequence (clause body)
        baseIndent += 1
        break
    }
    current = parent
  }

  // Adjust indentation based on current line content
  let adjustedIndent = baseIndent

  // De-indent closing constructs
  if (lineText.match(/^\s*:-\s*end_(?:object|protocol|category)\./)) {
    // Entity closing directive
    adjustedIndent = Math.max(0, baseIndent - 1)
  } else if (lineText.match(/^\s*[)\]}]/)) {
    // Closing parentheses, brackets, or braces
    adjustedIndent = Math.max(0, baseIndent - 1)
  } else if (lineText.match(/^\s*\./)) {
    // Clause terminator
    adjustedIndent = Math.max(0, baseIndent - 1)
  }

  // Special handling for specific constructs
  const prevLine = line.number > 1 ? state.doc.line(line.number - 1) : null
  if (prevLine) {
    const prevText = prevLine.text

    // Indent after entity opening
    if (prevText.match(/:-\s*(?:object|protocol|category|module)\s*\(/)) {
      adjustedIndent = baseIndent + 1
    }

    // Indent after clause head with body
    if (prevText.match(/:-\s*$/) || prevText.match(/:-\s*[^.]*[^.]$/)) {
      adjustedIndent = baseIndent + 1
    }

    // Indent after opening constructs
    if (prevText.match(/[(\[{]\s*$/)) {
      adjustedIndent = baseIndent + 1
    }

    // Indent after control constructs
    if (prevText.match(/\(\s*$/) || prevText.match(/->\s*$/) || prevText.match(/;\s*$/)) {
      adjustedIndent = baseIndent + 1
    }
  }

  return Math.max(0, adjustedIndent) * 2 // 2 spaces per indent level
  } catch (error) {
    // If there's an error in parsing or indentation calculation, return default indentation
    console.warn('Logtalk indentation service error:', error)
    return 0
  }
}

// Export complete language support bundle
export const logtalkSupport = [
  logtalk(),
  // Additional extensions can be added here
]

// All exports are already defined above
