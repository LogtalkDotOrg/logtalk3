import re
try:
    set
except NameError:
    from sets import Set as set

from pygments.lexer import RegexLexer
from pygments.token import \
     Text, Comment, Operator, Keyword, Name, String, Number, Punctuation, \
     Error

__all__ = ['LogtalkLexer']

class LogtalkLexer(RegexLexer):
    """
    For Logtalk source code.

    :copyright: 2008-2024 by Paulo Moura, Clara Dimene.
    :license: BSD, see LICENSE for more details.
    """

    name = 'Logtalk'
    url = 'http://logtalk.org/'
    aliases = ['logtalk']
    filenames = ['*.lgt', '*.logtalk']
    mimetypes = ['text/x-logtalk']
    version_added = '0.10'

    tokens = {
        'root': [
            # Directives
            (r'^\s*:-\s', Punctuation, 'directive'),
            # Comments
            (r'%.*?\n', Comment),
            (r'/\*(.|\n)*?\*/', Comment),
            # Whitespace
            (r'\n', Text),
            (r'\s+', Text),
            # Numbers
            (r"0'[\\]?.", Number),
            (r'0b[01]+', Number.Bin),
            (r'0o[0-7]+', Number.Oct),
            (r'0x[0-9a-fA-F]+', Number.Hex),
            (r'\d+\.?\d*((e|E)(\+|-)?\d+)?', Number),
            # Variables
            (r'([A-Z_][a-zA-Z0-9_]*)', Name.Variable),
            # Event handlers
            (r'(after|before)(?=[(])', Keyword),
            # Message forwarding handler
            (r'forward(?=[(])', Keyword),
            # Execution-context methods
            (r'(context|parameter|this|se(lf|nder))(?=[(])', Keyword),
            # Reflection
            (r'(current_predicate|predicate_property)(?=[(])', Keyword),
            # DCGs and term expansion
            (r'(expand_(goal|term)|(goal|term)_expansion|phrase)(?=[(])', Keyword),
            # Entity
            (r'(abolish|c(reate|urrent))_(object|protocol|category)(?=[(])', Keyword),
            (r'(object|protocol|category)_property(?=[(])', Keyword),
            # Entity relations
            (r'co(mplements_object|nforms_to_protocol)(?=[(])', Keyword),
            (r'extends_(object|protocol|category)(?=[(])', Keyword),
            (r'imp(lements_protocol|orts_category)(?=[(])', Keyword),
            (r'(instantiat|specializ)es_class(?=[(])', Keyword),
            # Events
            (r'(current_event|(abolish|define)_events)(?=[(])', Keyword),
            # Flags
            (r'(create|current|set)_logtalk_flag(?=[(])', Keyword),
            # Compiling, loading, and library paths
            (r'logtalk_(compile|l(ibrary_path|oad|oad_context)|make(_target_action)?)(?=[(])', Keyword),
            (r'\blogtalk_make\b', Keyword),
            # Database
            (r'(clause|retract(all)?)(?=[(])', Keyword),
            (r'a(bolish|ssert(a|z))(?=[(])', Keyword),
            # Control constructs
            (r'(ca(ll|tch)|throw)(?=[(])', Keyword),
            (r'(fa(il|lse)|true|(instantiation|system)_error)\b', Keyword),
            (r'(uninstantiation|type|domain|consistency|existence|permission|representation|evaluation|resource|syntax)_error(?=[(])', Keyword),
            # All solutions
            (r'((bag|set)of|f(ind|or)all)(?=[(])', Keyword),
            # Multi-threading predicates
            (r'threaded(_(ca(ll|ncel)|once|ignore|exit|peek|wait|notify))?(?=[(])', Keyword),
            # Engine predicates
            (r'threaded_engine(_(create|destroy|self|next|next_reified|yield|post|fetch))?(?=[(])', Keyword),
            # Term unification
            (r'(subsumes_term|unify_with_occurs_check)(?=[(])', Keyword),
            # Term creation and decomposition
            (r'(functor|arg|copy_term|numbervars|term_variables)(?=[(])', Keyword),
            # Evaluable functors
            (r'(div|rem|m(ax|in|od)|abs|sign)(?=[(])', Keyword),
            (r'float(_(integer|fractional)_part)?(?=[(])', Keyword),
            (r'(floor|t(an|runcate)|round|ceiling)(?=[(])', Keyword),
            # Other arithmetic functors
            (r'(cos|a(cos|sin|tan|tan2)|exp|log|s(in|qrt)|xor)(?=[(])', Keyword),
            # Term testing
            (r'(var|atom(ic)?|integer|float|c(allable|ompound)|n(onvar|umber)|ground|acyclic_term)(?=[(])', Keyword),
            # Term comparison
            (r'compare(?=[(])', Keyword),
            # Stream selection and control
            (r'(curren|se)t_(in|out)put(?=[(])', Keyword),
            (r'(open|close)(?=[(])', Keyword),
            (r'flush_output(?=[(])', Keyword),
            (r'(at_end_of_stream|flush_output)\b', Keyword),
            (r'(stream_property|at_end_of_stream|set_stream_position)(?=[(])', Keyword),
            # Character and byte input/output
            (r'(nl|(get|peek|put)_(byte|c(har|ode)))(?=[(])', Keyword),
            (r'\bnl\b', Keyword),
            # Term input/output
            (r'read(_term)?(?=[(])', Keyword),
            (r'write(q|_(canonical|term))?(?=[(])', Keyword),
            (r'(current_)?op(?=[(])', Keyword),
            (r'(current_)?char_conversion(?=[(])', Keyword),
            # Atomic term processing
            (r'atom_(length|c(hars|o(ncat|des)))(?=[(])', Keyword),
            (r'(char_code|sub_atom)(?=[(])', Keyword),
            (r'number_c(har|ode)s(?=[(])', Keyword),
            # Implementation defined hooks functions
            (r'(se|curren)t_prolog_flag(?=[(])', Keyword),
            (r'\bhalt\b', Keyword),
            (r'halt(?=[(])', Keyword),
            # Message sending operators
            (r'(::|:|\^\^)', Operator),
            # External call
            (r'[{}]', Keyword),
            # Logic and control
            (r'(ignore|once)(?=[(])', Keyword),
            (r'\brepeat\b', Keyword),
            # Sorting
            (r'(key)?sort(?=[(])', Keyword),
            # Bitwise functors
            (r'(>>|<<|/\\|\\\\|\\)', Operator),
            # Predicate aliases
            (r'\bas\b', Operator),
            # Arithmetic evaluation
            (r'\bis\b', Keyword),
            # Arithmetic comparison
            (r'(=:=|=\\=|<|=<|>=|>)', Operator),
            # Term creation and decomposition
            (r'=\.\.', Operator),
            # Term unification
            (r'(=|\\=)', Operator),
            # Term comparison
            (r'(==|\\==|@=<|@<|@>=|@>)', Operator),
            # Evaluable functors
            (r'(//|[-+*/])', Operator),
            (r'\b(e|pi|div|mod|rem)\b', Operator),
            # Other arithmetic functors
            (r'\b\*\*\b', Operator),
            # DCG rules
            (r'-->', Operator),
            # Control constructs
            (r'([!;]|->)', Operator),
            # Logic and control
            (r'\\+', Operator),
            # Mode operators
            (r'[?@]', Operator),
            # Existential quantifier
            (r'\^', Operator),
            # Punctuation
            (r'[()\[\],.|]', Text),
            # Atoms
            (r"[a-z][a-zA-Z0-9_]*", Text),
            (r"'", String, 'quoted_atom'),
            # Double-quoted terms
            (r'"', String, 'double_quoted_term'),
        ],

        'quoted_atom': [
            (r"''", String),
            (r"'", String, '#pop'),
            (r'\\([\\abfnrtv"\']|(x[a-fA-F0-9]+|[0-7]+)\\)', String.Escape),
            (r"[^\\'\n]+", String),
            (r'\\', String),
        ],

        'double_quoted_term': [
            (r'""', String),
            (r'"', String, '#pop'),
            (r'\\([\\abfnrtv"\']|(x[a-fA-F0-9]+|[0-7]+)\\)', String.Escape),
            (r'[^\\"\n]+', String),
            (r'\\', String),
        ],

        'directive': [
            # Conditional compilation directives
            (r'(el)?if(?=[(])', Keyword, 'root'),
            (r'(e(lse|ndif))(?=[.])', Keyword, 'root'),
            # Entity directives
            (r'(category|object|protocol)(?=[(])', Keyword, 'entityrelations'),
            (r'(end_(category|object|protocol))(?=[.])', Keyword, 'root'),
            # Predicate scope directives
            (r'(public|protected|private)(?=[(])', Keyword, 'root'),
            # Other directives
            (r'e(n(coding|sure_loaded)|xport)(?=[(])', Keyword, 'root'),
            (r'in(clude|itialization|fo)(?=[(])', Keyword, 'root'),
            (r'(built_in|dynamic|synchronized|threaded)(?=[.])', Keyword, 'root'),
            (r'(alias|d(ynamic|iscontiguous)|m(eta_(non_terminal|predicate)|ode|ultifile)|s(et_(logtalk|prolog)_flag|ynchronized))(?=[(])', Keyword, 'root'),
            (r'op(?=[(])', Keyword, 'root'),
            (r'(c(alls|oinductive)|module|reexport|use(s|_module))(?=[(])', Keyword, 'root'),
            (r'[a-z][a-zA-Z0-9_]*(?=[(])', Text, 'root'),
            (r'[a-z][a-zA-Z0-9_]*(?=[.])', Text, 'root'),
        ],

        'entityrelations': [
            (r'(complements|extends|i(nstantiates|mp(lements|orts))|specializes)(?=[(])', Keyword),
            # Numbers
            (r"0'[\\]?.", Number),
            (r'0b[01]+', Number.Bin),
            (r'0o[0-7]+', Number.Oct),
            (r'0x[0-9a-fA-F]+', Number.Hex),
            (r'\d+\.?\d*((e|E)(\+|-)?\d+)?', Number),
            # Variables
            (r'([A-Z_][a-zA-Z0-9_]*)', Name.Variable),
            # Atoms
            (r"[a-z][a-zA-Z0-9_]*", Text),
            (r"'", String, 'quoted_atom'),
            # Double-quoted terms
            (r'"', String, 'double_quoted_term'),
            # End of entity-opening directive
            (r'([)]\.)', Text, 'root'),
            # Scope operator
            (r'(::)', Operator),
            # Punctuation
            (r'[()\[\],.|]', Text),
            # Comments
            (r'%.*?\n', Comment),
            (r'/\*(.|\n)*?\*/', Comment),
            # Whitespace
            (r'\n', Text),
            (r'\s+', Text),
        ]
    }

    def analyse_text(text):
        if ':- object(' in text:
            return 1.0
        elif ':- protocol(' in text:
            return 1.0
        elif ':- category(' in text:
            return 1.0
        elif re.search(r'^:-\s[a-z]', text, re.M):
            return 0.9
        else:
            return 0.0
