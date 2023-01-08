" Disable compatibility with vi which can cause unexpected issues.
set nocompatible

call plug#begin()
Plug 'preservim/nerdtree'
Plug 'tpope/vim-sensible'
Plug 'fatih/vim-go', { 'do': ':GoInstallBinaries' }
Plug 'vim-pandoc/vim-pandoc'
Plug 'srcery-colors/srcery-vim'
call plug#end()

" Enable type file detection. Vim will be able to try to detect the type of file in use.
filetype on

" Enable plugins and load plugin for the detected file type.
filetype plugin on

" Load an indent file for the detected file type.
filetype indent on

" Set line numbers in the gutter
set nu
" set relativenumber

" Line wrap (number of cols)
set textwidth=120

set encoding=utf-8
set fileencoding=utf-8
set encoding=utf-8

" Seems to show hidden characters when this is set
set list
set showbreak=↪\ 

set listchars=trail:␣,tab:»\ 

set autoindent noexpandtab tabstop=4 shiftwidth=4

colorscheme srcery

set hlsearch    "Highlight all search results
set smartcase   "Enable smart-case search
set ignorecase  "Always case-insensitive
set incsearch   "Searches for strings incrementally

let mapleader=" "

" Stop with the madness
set noswapfile
set nobackup

" force some files to be specific file type
au bufnewfile,bufRead $SNIPPETS/md/* set ft=pandoc
au bufnewfile,bufRead $SNIPPETS/sh/* set ft=sh
au bufnewfile,bufRead $SNIPPETS/bash/* set ft=bash
au bufnewfile,bufRead $SNIPPETS/go/* set ft=go
au bufnewfile,bufRead $SNIPPETS/c/* set ft=c
au bufnewfile,bufRead $SNIPPETS/html/* set ft=html
au bufnewfile,bufRead $SNIPPETS/css/* set ft=css
au bufnewfile,bufRead $SNIPPETS/js/* set ft=javascript
au bufnewfile,bufRead $SNIPPETS/python/* set ft=python
au bufnewfile,bufRead $SNIPPETS/perl/* set ft=perl
au bufnewfile,bufRead user-data set ft=yaml
au bufnewfile,bufRead meta-data set ft=yaml
au bufnewfile,bufRead keg set ft=yaml
au bufnewfile,bufRead *.bash* set ft=bash
au bufnewfile,bufRead *.{peg,pegn} set ft=config
au bufnewfile,bufRead *.profile set filetype=sh
au bufnewfile,bufRead *.crontab set filetype=crontab
au bufnewfile,bufRead *ssh/config set filetype=sshconfig
au bufnewfile,bufRead .dockerignore set filetype=gitignore
au bufnewfile,bufRead *gitconfig set filetype=gitconfig
au bufnewfile,bufRead /tmp/psql.edit.* set syntax=sql
au bufnewfile,bufRead *.go set spell spellcapcheck=0
au bufnewfile,bufRead commands.yaml set spell

" bash
fun! s:DetectBash()
    if getline(1) == '#!/usr/bin/bash' || getline(1) == '#!/bin/bash'
        set ft=bash
        set shiftwidth=4
    endif
endfun
autocmd BufNewFile,BufRead * call s:DetectBash()

" pandoc
let g:pandoc#formatting#mode = 'h' " A'
let g:pandoc#formatting#textwidth = 72

" golang
let g:go_fmt_fail_silently = 0
let g:go_fmt_command = 'goimports'
let g:go_fmt_autosave = 1
let g:go_gopls_enabled = 1
let g:go_highlight_types = 1
let g:go_highlight_fields = 1
let g:go_highlight_functions = 1
let g:go_highlight_function_calls = 1
let g:go_highlight_operators = 1
let g:go_highlight_extra_types = 1
let g:go_highlight_variable_declarations = 1
let g:go_highlight_variable_assignments = 1
let g:go_highlight_build_constraints = 1
let g:go_highlight_diagnostic_errors = 1
let g:go_highlight_diagnostic_warnings = 1
"let g:go_auto_type_info = 1 " forces 'Press ENTER' too much
let g:go_auto_sameids = 0
"let g:go_metalinter_command='golangci-lint'
"let g:go_metalinter_command='golint'
"let g:go_metalinter_autosave=1
set updatetime=100
"let g:go_gopls_analyses = { 'composites' : v:false }
au FileType go nmap <leader>m ilog.Print("made")<CR><ESC>
au FileType go nmap <leader>n iif err != nil {return err}<CR><ESC>

" Brace pairing
inoremap <> <><Left>
inoremap () ()<Left>
inoremap {} {}<Left>
inoremap [] []<Left>
inoremap "" ""<Left>
inoremap '' ''<Left>
inoremap `` ``<Left>`

" intellij specific
if has('ide')
    if &ide =~? 'intellij idea'
        " some mappings and options for IntelliJ IDEA Community Edition
        if &ide =~? 'community'

        " some mappings and options for IntelliJ IDEA Ultimate Edition
        elseif &ide =~? 'ultimate'
        endif

    " Webstorm specific mappings and options
    elseif &ide =~? 'webstorm'

    " PyCharm specific mappings and options
    elseif &ide =~? 'pycharm'
    endif
else

    " ========== Steve Losh hacks ==========="
    " w!! to write a file as sudo
    cmap w!! w !sudo tee % >/dev/null

endif

" vim:set ft=vim et sw=4:
