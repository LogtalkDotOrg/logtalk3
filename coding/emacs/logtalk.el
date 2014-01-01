;; logtalk.el -- font lock support for Logtalk (http://logtalk.org/)

;; Copyright (c) 2003-2014 Paulo Moura

;; Author: Paulo Moura <pmoura@logtalk.org>
;; Creation date: November 15, 2003
;; Last modification date: September 11, 2013
;; Version: 1.26

;; Installation:
;;
;; First, copy this file to the appropriated directory. For FSF Emacs this will 
;; probably be /usr/local/share/emacs/site-lisp. For XEmacs, the directory is
;; usully /usr/local/lib/xemacs/site-lisp. You may also copy the file to a 
;; sub-directory in your home directory depending on your Emacs configuration.
;; Type "C-h v load-path" in Emacs to find the list of paths that are searched
;; for when looking for lisp files.
;;
;; Second, add the following lines in your Emacs init file, for example
;; your ~/.emacs file:
;;
;; (autoload 'logtalk-mode "logtalk" "Major mode for editing Logtalk programs." t)
;; (add-to-list 'auto-mode-alist '("\\.lgt\\'" . logtalk-mode))
;; (add-to-list 'auto-mode-alist '("\\.logtalk\\'" . logtalk-mode))



;; setup 

(defvar logtalk-mode-version "1.26"
	"Logtalk mode version number")

(defvar logtalk-mode-hook nil)

(defvar logtalk-mode-map nil) 

(if logtalk-mode-map nil
	(setq logtalk-mode-map (make-keymap)))



;; syntax highlighting 

(defvar logtalk-font-lock-keywords nil)



;; syntax table 

(defvar logtalk-mode-syntax-table
	(let ((logtalk-mode-syntax-table (make-syntax-table)))
		(modify-syntax-entry ?_ "w" logtalk-mode-syntax-table)
		(modify-syntax-entry ?/ ". 14b" logtalk-mode-syntax-table)
		(modify-syntax-entry ?* ". 23b" logtalk-mode-syntax-table)
		(modify-syntax-entry ?% "<" logtalk-mode-syntax-table)
		(modify-syntax-entry ?\n ">" logtalk-mode-syntax-table)
		(modify-syntax-entry ?\' "w" logtalk-mode-syntax-table)
		logtalk-mode-syntax-table)
	"Syntax table for logtalk-mode")



;; make logtalk font-lock-faces

(make-face 'logtalk-default-face)
(make-face 'logtalk-directive-face)
(make-face 'logtalk-built-in-predicate-face)
(make-face 'logtalk-built-in-method-face)
(make-face 'logtalk-message-operator-face)
(make-face 'logtalk-variable-face)
(make-face 'logtalk-number-face)
(make-face 'logtalk-comment-face)
(make-face 'logtalk-string-face)

;; set logtalk font-lock-faces

(require 'font-lock)

(copy-face 'default 'logtalk-default-face)
(copy-face 'font-lock-keyword-face 'logtalk-directive-face)
(copy-face 'font-lock-builtin-face 'logtalk-built-in-predicate-face)
(copy-face 'font-lock-builtin-face 'logtalk-built-in-method-face)
(copy-face 'font-lock-function-name-face 'logtalk-message-operator-face)
(copy-face 'font-lock-variable-name-face 'logtalk-variable-face)
(copy-face 'font-lock-constant-face 'logtalk-number-face)
(copy-face 'font-lock-comment-face 'logtalk-comment-face)
(copy-face 'font-lock-string-face 'logtalk-string-face)



(setq logtalk-font-lock-strings
	'(
		("\\(\"\\([^\n\"]\\|\\\\\"\\)*\"\\)" 1 'logtalk-string-face)
		("\\(^\\|[^0-9]\\)\\('\\([^\n']\\|\\\\'\\)*'\\)" 2 'logtalk-string-face)
	))


(setq logtalk-font-lock-directives
	'(
		("\\(:- \\)\\(\\(e\\(?:lse\\|nd\\(?:if\\|_\\(?:category\\|object\\|protocol\\)\\)\\)\\)\\)\\([\.]\\)" 2 'logtalk-directive-face)
		("\\(:- \\)\\(category\\|object\\|protocol\\|module\\)\\([(]\\)" 2 'logtalk-directive-face)
		("\\(:- \\)\\(p\\(?:r\\(?:ivate\\|otected\\)\\|ublic\\)\\)\\([(]\\)" 2 'logtalk-directive-face)
		("\\(:- \\)\\(alias\\|c\\(?:alls\\|oinductive\\)\\|d\\(?:iscontiguous\\|ynamic\\)\\|e\\(?:lif\\|n\\(?:coding\\|sure_loaded\\)\\|xport\\)\\|i\\(?:f\\|n\\(?:clude\\|fo\\|itialization\\)\\)\\|m\\(?:\\(?:eta_\\(?:non_terminal\\|predicate\\)\\|ode\\|ultifile\\)\\)\\|op\\|use\\(?:s\\|_module\\)\\|reexport\\|synchronized\\)\\([(]\\)" 2 'logtalk-directive-face)
		("\\(:- \\)\\(built_in\\|dynamic\\|s\\(?:et_\\(?:logtalk\\|prolog\\)_flag\\|ynchronized\\)\\|threaded\\)\\([\.]\\)" 2 'logtalk-directive-face)
		("\\<\\(\\(?:complement\\|extend\\|i\\(?:mp\\(?:\\(?:lemen\\|or\\)t\\)\\|nstantiate\\)\\|specialize\\)s\\)\\([(]\\)" 1 'logtalk-directive-face)
	))


(setq logtalk-font-lock-built-in-methods
	'(
		("\\<\\(parameter\\|se\\(?:lf\\|nder\\)\\|this\\)\\([(]\\)" 1 'logtalk-built-in-method-face)
		("\\<\\(current_predicate\\|predicate_property\\)\\([(]\\)" 1 'logtalk-built-in-method-face)
		("\\<\\(a\\(?:bolish\\|ssert[az]\\)\\|clause\\|retract\\(?:all\\)?\\)\\([(]\\)" 1 'logtalk-built-in-method-face)
		("\\<\\(bagof\\|f\\(?:\\(?:ind\\|or\\)all\\)\\|setof\\)\\([(]\\)" 1 'logtalk-built-in-method-face)
		("\\<\\(after\\|before\\|forward\\)\\([(]\\)" 1 'logtalk-built-in-method-face)
		("\\<\\(phrase\\|expand_\\(?:goal\\|term\\)\\|\\(?:goal\\|term\\)_expansion\\)\\([(]\\)" 1 'logtalk-built-in-method-face)
	))


(setq logtalk-font-lock-built-in-predicates
	'(
		("\\<\\(c\\(?:urrent\\|reate\\)_\\(?:category\\|object\\|protocol\\)\\)\\([(]\\)" 1 'logtalk-built-in-predicate-face)
		("\\<\\(abolish_\\(?:category\\|object\\|protocol\\)\\)\\([(]\\)" 1 'logtalk-built-in-predicate-face)
		("\\<\\(\\(?:category\\|object\\|protocol\\)_property\\)\\([(]\\)" 1 'logtalk-built-in-predicate-face)
		("\\<\\(co\\(?:mplements_object\\|nforms_to_protocol\\)\\|extends_\\(?:object\\|protocol\\|category\\)\\|i\\(?:mp\\(?:lements_protocol\\|orts_category\\)\\|nstantiates_class\\)\\|specializes_class\\)\\([(]\\)" 1 'logtalk-built-in-predicate-face)
		("\\<\\(\\(?:abolish\\|define\\)_events\\|current_event\\)\\([(]\\)" 1 'logtalk-built-in-predicate-face)
		("\\<\\(\\(?:curren\\|se\\)t_logtalk_flag\\)\\([(]\\)" 1 'logtalk-built-in-predicate-face)
		("\\<\\(logtalk_\\(?:compile\\|load\\|library_path\\|load_context\\|make\\)\\)\\([(]\\)" 1 'logtalk-built-in-predicate-face)
		("\\<logtalk_make\\>" 0 'logtalk-built-in-predicate-face)
		("\\<\\(forall\\)\\([(]\\)" 1 'logtalk-built-in-predicate-face)
		("\\<\\(retractall\\)\\([(]\\)" 1 'logtalk-built-in-predicate-face)
		;;
		;; control constructs:
		;;
		("\\<\\(ca\\(?:ll\\|tch\\)\\|throw\\)\\([(]\\)" 1 'logtalk-built-in-predicate-face)
		("\\<\\(fa\\(?:il\\|lse\\)\\|true\\)\\>" 0 'logtalk-built-in-predicate-face)
		("\\(!\\|->\\|;\\)" 0 'logtalk-built-in-predicate-face)
		;;
		;; multi-threading:
		;;
		("\\<\\(threaded\\(_\\(?:call\\|once\\|ignore\\|exit\\|peek\\|wait\\|notify\\)\\)?\\)\\([(]\\)" 1 'logtalk-built-in-predicate-face)
		;;
		;; logic and control:
		;;
		("\\<\\(once\\|ignore\\)\\([(]\\)" 1 'logtalk-built-in-predicate-face)
		("\\\\\\+\\|\\<repeat\\>" 0 'logtalk-built-in-predicate-face)
		;;
		;; term testing:
		;;
		("\\<\\(atom\\(?:ic\\)?\\|c\\(?:allable\\|ompound\\)\\|float\\|ground\\|\\(?:intege\\|n\\(?:onva\\|umbe\\)\\|va\\)r\\)\\([(]\\)" 1 'logtalk-built-in-predicate-face)
		;;
		;; term comparison:
		;;
		("==\\|@\\(?:=<\\|>=\\|[<>]\\)\\|\\\\==" 0 'logtalk-built-in-predicate-face)
		("\\<\\(compare\\)\\([(]\\)" 1 'logtalk-built-in-predicate-face)
		;;
		;; term creation and decomposition:
		;;
		("\\<\\(arg\\|\\(?:acyclic\\|copy\\|subsumes\\)_term\\|functor\\|numbervars\\|term_variables\\)\\([(]\\)" 1 'logtalk-built-in-predicate-face)
		("=\\.\\." 0 'logtalk-built-in-predicate-face)
		;;
		;; arithemtic evaluation:
		;;
		("\\<is\\>" 0 'logtalk-built-in-predicate-face)
		;;
		;; arithmetic comparison:
		("=:=\\|\\(?:=<\\|>=\\|[<>]\\)\\|\\\\==" 0 'logtalk-built-in-predicate-face)
		;;
		;; term unification:
		;;
		("\\\\?=" 0 'logtalk-built-in-predicate-face)
		("\\<\\(unify_with_occurs_check\\)\\([(]\\)" 1 'logtalk-built-in-predicate-face)
		;;
		;; dcgs:
		;;
		("-->" 0 'logtalk-built-in-predicate-face)
		;;
		;; evaluable functors:
		;;
		("\\<\\(abs\\|ceiling\\|flo\\(?:at\\(?:_\\(?:\\(?:fractional\\|integer\\)_part\\)\\)?\\|or\\)\\|m\\(?:ax\\|in\\|od\\)\\|r\\(?:em\\|ound\\)\\|sign\\|truncate\\)\\([(]\\)" 1 'logtalk-built-in-predicate-face)
		("//\\|[*/]" 0 'logtalk-built-in-predicate-face)
		("\\([^eE]\\)\\([+]\\)" 2 'logtalk-built-in-predicate-face)
		("\\([^:eE]\\)\\([-]\\)" 2 'logtalk-built-in-predicate-face)
		("\\<\\(e\\|pi\\|rem\\|mod\\)\\>" 0 'logtalk-built-in-predicate-face)
		;;
		;; other arithemtic functors:
		;;
		("\\<\\(a\\(?:cos\\|sin\\|tan\\)\\|cos\\|exp\\|log\\|s\\(?:in\\|qrt\\)\\)\\([(]\\)" 1 'logtalk-built-in-predicate-face)
		("\\*\\*" 1 'logtalk-built-in-predicate-face)
		;;
		;; stream selection and control:
		;;
		("\\<\\(at_end_of_stream\\|c\\(?:lose\\|urrent_\\(?:\\(?:in\\|out\\)put\\)\\)\\|flush_output\\|open\\|s\\(?:et_\\(?:input\\|output\\|stream_position\\)\\|tream_property\\)\\)\\([(]\\)" 1 'logtalk-built-in-predicate-face)
		("\\<\\(at_end_of_stream\\|flush_output\\)\\>" 0 'logtalk-built-in-predicate-face)
		;;
		;; character input/output:
		;;
		("\\<\\(get_c\\(?:har\\|ode\\)\\|nl\\|p\\(?:eek_c\\(?:har\\|ode\\)\\|ut_c\\(?:har\\|ode\\)\\)\\)\\([(]\\)" 1 'logtalk-built-in-predicate-face)
		("\\<nl\\>" 0 'logtalk-built-in-predicate-face)
		;;
		;; byte input/output:
		;;
		("\\<\\(\\(?:get\\|p\\(?:eek\\|ut\\)\\)_byte\\)\\([(]\\)" 1 'logtalk-built-in-predicate-face)
		;;
		;; term input/output:
		;;
		("\\<\\(c\\(?:har_conversion\\|urrent_\\(?:char_conversion\\|op\\)\\)\\|op\\|read\\(?:_term\\)?\\|write\\(?:_\\(?:canonical\\|term\\)\\|q\\)?\\)\\([(]\\)" 1 'logtalk-built-in-predicate-face)
		;;
		;; implementation defined hooks functions:
		;;
		("\\<\\(\\(?:curren\\|se\\)t_prolog_flag\\|halt\\)\\([(]\\)" 1 'logtalk-built-in-predicate-face)
		("\\<halt\\>" 0 'logtalk-built-in-predicate-face)
		;;
		;; atomic term processing:
		;;
        ("\\<\\(atom_\\(?:c\\(?:hars\\|o\\(?:des\\|ncat\\)\\)\\|length\\)\\|char_code\\|number_c\\(?:har\\|ode\\)s\\|sub_atom\\)\\([(]\\)" 1 'logtalk-built-in-predicate-face)
		;;
		;; sorting
		;;
		("\\<\\(keysort\\|sort\\)\\([(]\\)" 1 'logtalk-built-in-predicate-face)
		;;
		;; bitwise functors:
		;;
		("/\\\\|<<\\|>>\\|\\\\/" 0 'logtalk-built-in-predicate-face)
		("\\\\" 0 'logtalk-built-in-predicate-face)
	))


(setq logtalk-font-lock-operators
	'(
		;;
		;; clause operator:
		;;
		(":-" 0 'logtalk-default-face)
		;;
		;; message sending operators:
		;;
		("::\\|\\^\\^\\|[{}]" 0 'logtalk-message-operator-face)
		;;
		;; category predicate direct call:
		;;
		(":" 0 'logtalk-message-operator-face)
		;;
		;; mode operators:
		;;
		("[@?]" 0 'logtalk-built-in-predicate-face)
	))


(setq logtalk-font-lock-numbers
	'(
		("\\<\\(0x[a-fA-F0-9]+\\)\\>" 1 'logtalk-number-face)
		("\\<\\(0o[0-7]+\\)\\>" 1 'logtalk-number-face)
		("\\<\\(0b[0-1]+\\)\\>" 1 'logtalk-number-face)
		("\\<\\(0['].\\)\\>" 1 'logtalk-number-face)
		("\\<\\([0-9]+\\([.][0-9]+\\)?\\([eE][+-][0-9]+\\)?\\)\\>" 1 'logtalk-number-face)
	))


(setq logtalk-font-lock-variables
	'(
		("\\<\\([_A-Z][a-zA-Z0-9_]*\\)\\>" 1 'logtalk-variable-face)
	))


(setq logtalk-font-lock-keywords
	(append
		logtalk-font-lock-strings
		logtalk-font-lock-directives
		logtalk-font-lock-built-in-methods
		logtalk-font-lock-built-in-predicates
		logtalk-font-lock-operators
		logtalk-font-lock-variables
		logtalk-font-lock-numbers
	))


;; entry function

(defun logtalk-mode ()
	"Major mode for editing Logtalk files"
	(interactive)
	(kill-all-local-variables)
	(setq tab-width 4)
	(set-syntax-table logtalk-mode-syntax-table)
	(set (make-local-variable 'font-lock-defaults) '(logtalk-font-lock-keywords))
	(turn-on-font-lock)
	(setq major-mode 'logtalk-mode)
	(setq mode-name "Logtalk")
	(run-hooks 'logtalk-mode-hook))

(provide 'logtalk-mode) 
