set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'
Plugin 'Raimondi/delimitMate'          " matches closing charactors
Plugin 'Shougo/neocomplete.vim'
Plugin 'tpope/vim-fugitive'
Plugin 'tpope/vim-surround'
Plugin 'nicwest/rainbow_parentheses.vim'

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required
" To ignore plugin indent changes, instead use:
"filetype plugin on
"
" Brief help
" :PluginList       - lists configured plugins
" :PluginInstall    - installs plugins; append `!` to update or just :PluginUpdate
" :PluginSearch foo - searches for foo; append `!` to refresh local cache
" :PluginClean      - confirms removal of unused plugins; append `!` to auto-approve removal
"
" see :h vundle for more details or wiki for FAQ
" Put your non-Plugin stuff after this line

set number                      "Line numbers are good
set linebreak	                  " Break lines at word (requires Wrap lines)
set backspace=indent,eol,start  "Allow backspace in insert mode
set undolevels=1000             "Number of undo levels
set history=1000                "Store lots of :cmdline history
set showcmd                     "Show incomplete cmds down the bottom
set showmode                    "Show current mode down the bottom
set gcr=a:blinkon0              "Disable cursor blink
set visualbell                  "No sounds
set autoread                    "Reload files changed outside vim
set fileformats=unix,dos,mac    "Prefer Unix over Windows over OS 9 formats
set ambiwidth=double            "make all charactors the same width
set clipboard=unnamedplus       "clipboard now defaults to system
set nowrap                      "Don't wrap lines
set showmatch                   "highlight matching brace
set textwidth=100               "Line wrap (number of cols)
"set showbreak=+++               "Wrap-broken line prefix
set wrapscan
set ruler                       "Show row and column ruler information

set encoding=utf-8
set fileencoding=utf-8
set encoding=utf-8
set list
set showbreak=↪\ 
set listchars=trail:␣,tab:»\ ,eol:⏎

set autoindent noexpandtab tabstop=4 shiftwidth=4

let mapleader=","

set noswapfile
set nobackup

nnoremap w b
nnoremap W B
noremap e w
noremap E W
noremap R r
noremap b %

vnoremap w b
vnoremap W B
vnoremap e w
vnoremap E W

set hlsearch	 "Highlight all search results
set smartcase	 "Enable smart-case search
set ignorecase "Always case-insensitive
set incsearch	 "Searches for strings incrementally

set wildmode=list:longest
set wildmenu                "enable ctrl-n and ctrl-p to scroll thru matches
set wildignore=*.o,*.obj,*~ "stuff to ignore when tab completing
set wildignore+=*vim/backups*
set wildignore+=*sass-cache*
set wildignore+=vendor/rails/**
set wildignore+=vendor/cache/**
set wildignore+=*.gem
set wildignore+=log/**
set wildignore+=tmp/**
set wildignore+=*.png,*.jpg,*.gif
set wildignore+=.hg,.git,.svn                    " Version control
set wildignore+=*.aux,*.out,*.toc                " LaTeX intermediate files
set wildignore+=*.jpg,*.bmp,*.gif,*.png,*.jpeg   " binary images
set wildignore+=*.o,*.obj,*.exe,*.dll,*.manifest " compiled object files
set wildignore+=*.spl                            " compiled spelling word lists
set wildignore+=*.sw?                            " Vim swap files
set wildignore+=*.luac                           " Lua byte code
set wildignore+=migrations                       " Django migrations
set wildignore+=golang/pkg                       " Go static files
set wildignore+=golang/bin                       " Go bin files
set wildignore+=golang/bin-vagrant               " Go bin-vagrant files
set wildignore+=*.pyc                            " Python byte code
set wildignore+=*.orig                           " Merge resolution files
set complete=.,w,b,u,t,i,kspell
set completeopt=longest,menuone

" ========== Steve Losh hacks ==========="
" w!! to write a file as sudo
cmap w!! w !sudo tee % >/dev/null
