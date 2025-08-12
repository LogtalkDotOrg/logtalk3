"  Maintainer:	Paulo Moura <pmoura@logtalk.org>
"  Revised on:	2025.08.12
"		2023 Aug 28 by Vim Project (undo_indent)
"  Language:	Logtalk

" This Logtalk indent file started as a modified version of the Prolog
" indent file written by Gergely Kontra but contaisn major changes
" to properly handle Logtalk's syntax.

" Only load this indent file when no other was loaded.
if exists("b:did_indent")
	finish
endif

let b:did_indent = 1

setlocal indentexpr=GetLogtalkIndent()
setlocal indentkeys-=:,0#
setlocal indentkeys+=0%,-,0;,>,0)

let b:undo_indent = "setlocal indentexpr< indentkeys<"

" Only define the function once.
if exists("*GetLogtalkIndent")
	finish
endif

function! GetLogtalkIndent()
	" Find a non-blank line above the current line.
	let pnum = prevnonblank(v:lnum - 1)
	" Hit the start of the file, use zero indent.
	if pnum == 0
		return 0
	endif
	let line = getline(v:lnum)
	let pline = getline(pnum)

	let ind = indent(pnum)
	" Previous line was comment -> use previous line's indent
	if pline =~ '^\s*%'
		return ind
	endif

	" Check for entity opening directive on previous line
	if pline =~ '^\s*:-\s*\(object\|protocol\|category\)\ze('
		let ind = ind + shiftwidth()
	" Check for other directives that don't end on the same line
	elseif pline =~ '^\s*:-\s*' && pline !~ '\.\s*\(%.*\)\?$'
		let ind = ind + shiftwidth()
	" Check for continuation of multi-line entity opening directive
	elseif pline =~ '^\s*\(implements\|imports\|extends\|instantiates\|specializes\|complements\)(' && pline !~ ')\.\s*\(%.*\)\?$'
		" Keep same indentation for entity directive parameters
		return ind
	" Check for end of entity opening directive
	elseif pline =~ ')\.\s*\(%.*\)\?$' && s:IsInEntityDirective(pnum)
		" After entity opening directive, keep the increased indentation for entity content
		return ind
	" Check for end of other directives
	elseif pline =~ '\.\s*\(%.*\)\?$' && s:IsInDirective(pnum) && !s:IsInEntityDirective(pnum)
		" After other directives, return to the directive's original indentation level
		let directive_indent = s:GetDirectiveIndent(pnum)
		if directive_indent >= 0
			return directive_indent
		else
			let ind = ind - shiftwidth()
		endif
	" Check for clause head on previous line (rule)
	elseif pline =~ ':-\s*\(%.*\)\?$'
		let ind = ind + shiftwidth()
	" Check for grammar rule head on previous line
	elseif pline =~ '-->\s*\(%.*\)\?$'
		let ind = ind + shiftwidth()
	" Check for entity closing directive on previous line
	elseif pline =~ '^\s*:-\s*end_\(object\|protocol\|category\)\.\(%.*\)\?$'
		let ind = ind - shiftwidth()
	" Check for end of clause on previous line
	elseif pline =~ '\.\s*\(%.*\)\?$'
		" Check if this is a fact (no neck operator found in clause)
		if s:IsFactClause(pnum)
			" For facts, keep the same indentation as the previous line
			return ind
		else
			" For rules, decrease indentation back to the rule head level
			let ind = ind - shiftwidth()
		endif
	endif

	" Check for opening conditional on previous line
	if pline =~ '^\s*\([(;]\|->\)' && pline !~ '\.\s*\(%.*\)\?$' && pline !~ '^.*\([)][,]\s*\(%.*\)\?$\)'
		let ind = ind + shiftwidth()
	endif
	" Check for closing an unclosed paren, or middle ; or ->
	if line =~ '^\s*\([);]\|->\)'
		let ind = ind - shiftwidth()
	endif
	return ind
endfunction

" Helper function to check if we're inside an entity opening directive
function! s:IsInEntityDirective(lnum)
	let lnum = a:lnum
	" Look backwards to find the start of the entity directive
	while lnum > 0
		let line = getline(lnum)
		if line =~ '^\s*:-\s*\(object\|protocol\|category\)\ze('
			return 1
		elseif line =~ '^\s*:-\s*end_\(object\|protocol\|category\)\.'
			return 0
		endif
		let lnum = lnum - 1
	endwhile
	return 0
endfunction

" Helper function to check if we're inside any directive
function! s:IsInDirective(lnum)
	let lnum = a:lnum
	" Look backwards to find the start of a directive
	while lnum > 0
		let line = getline(lnum)
		" Skip comments and empty lines
		if line =~ '^\s*%' || line =~ '^\s*$'
			let lnum = lnum - 1
			continue
		endif
		" If we find a directive start, we're in a directive
		if line =~ '^\s*:-\s*'
			return 1
		endif
		" If we find the end of a previous directive/clause, stop
		if lnum < a:lnum && line =~ '\.\s*\(%.*\)\?$'
			return 0
		endif
		let lnum = lnum - 1
	endwhile
	return 0
endfunction

" Helper function to get the indentation level of the directive start
function! s:GetDirectiveIndent(lnum)
	let lnum = a:lnum
	" Look backwards to find the start of the directive
	while lnum > 0
		let line = getline(lnum)
		" Skip comments and empty lines
		if line =~ '^\s*%' || line =~ '^\s*$'
			let lnum = lnum - 1
			continue
		endif
		" If we find a directive start, return its indentation
		if line =~ '^\s*:-\s*'
			return indent(lnum)
		endif
		" If we find the end of a previous directive/clause, stop
		if lnum < a:lnum && line =~ '\.\s*\(%.*\)\?$'
			break
		endif
		let lnum = lnum - 1
	endwhile
	return -1
endfunction

" Helper function to check if a clause is a fact (no neck operator)
function! s:IsFactClause(lnum)
	let lnum = a:lnum
	let found_neck = 0

	" Look backwards through the current clause to find if there's a neck operator
	while lnum > 0
		let line = getline(lnum)
		" Skip comments and empty lines
		if line =~ '^\s*%' || line =~ '^\s*$'
			let lnum = lnum - 1
			continue
		endif

		" If we find a neck operator in this clause, it's a rule
		if line =~ ':-'
			let found_neck = 1
		endif

		" If we find the end of a previous clause (not the current one), stop looking
		if lnum < a:lnum && line =~ '\.\s*\(%.*\)\?$'
			break
		endif

		" If we find an entity directive or other structural element, stop
		if line =~ '^\s*:-\s*\(object\|protocol\|category\|end_\)'
			break
		endif

		let lnum = lnum - 1
	endwhile

	" Return 1 if it's a fact (no neck operator found), 0 if it's a rule
	return !found_neck
endfunction
