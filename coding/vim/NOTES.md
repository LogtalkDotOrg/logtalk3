________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
Copyright 1998-2020 Paulo Moura <pmoura@logtalk.org>

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
________________________________________________________________________


This directory contains files that provides syntax highlighting, code 
folding, code completion, and auto-indentation for editing Logtalk 
source files with the text editor Vim 7.0 or later version:

	http://www.vim.org/

These support files are dual-licensed under the Apache License 2.0 and
the Vim charity-ware license.

Recent versions of Vim already contain support for Logtalk. However, 
this directory may contain updated support files. If that is the case,
install the Logtalk support files by performing the following steps:

1. Copy the file `syntax/logtalk.vim` to the `syntax` sub-directory in 
your VIM installation directory (replacing any existing older file).

2. Add the following entries to the `filetype.vim` configuration file 
(only if not already present):

	" Logtalk source files
	au BufNewFile,BufRead *.lgt                     setf logtalk
	au BufNewFile,BufRead *.logtalk                 setf logtalk

3. Copy the file `indent/logtalk.vim` to the `indent` sub-directory in 
your VIM installation directory (replacing any existing older file).

4. Copy the files `completion/logtalk.dict` and `ftplugin/logtalk.vim` 
to the `ftplugin` sub-directory in your Vim installation directory 
(replacing any existing files; current Vim versions are distributed
with a `ftplugin/logtalk.vim` that comments out some of the necessary
settings despite their local scope!).

5. Add the following lines to your ~/.vimrc file:

	syntax on
	filetype indent on
	filetype plugin on

You may then perform code completion by typing CTRL-X, CTRL-K and then 
CTRL-P or CTRL-N to cycle between all the completion choices.

6. Check the `coding/ctags` in the Logtalk installation directory for
instructions on how to enable a source code browser for Logtalk source
files using the plugin Taglist:

	http://vim-taglist.sourceforge.net/

After installing the plugin, add the following line to your `.vimrc` file:

	let tlist_logtalk_settings = 'logtalk;o:Objects;p:Protocols;c:Categories;m:Modules;u:Public predicates;r:Protected predicates;v:Private predicates'

