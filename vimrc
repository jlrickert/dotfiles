"===============================================================================
" Initalize vundle
"===============================================================================
set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" let Vundle manage Vundle, required
Plugin 'gmarik/Vundle.vim'

Plugin 'scrooloose/syntastic.git'      " code linter
Plugin 'sjl/gundo.vim'                 " history diffs
Plugin 'godlygeek/tabular'             " code aligner
Plugin 'jamessan/vim-gnupg'
Plugin 'tomtom/tcomment_vim'           " commenting addon
Plugin 'Raimondi/delimitMate'          " matches closing charactors
Plugin 'tpope/vim-fugitive'            " git
Plugin 'tpope/vim-surround'
Plugin 'VisIncr'
Plugin 'kien/ctrlp.vim'
Plugin 'scrooloose/nerdtree.git'
Plugin 'jistr/vim-nerdtree-tabs.git'
Plugin 'Lokaltog/vim-easymotion'
Plugin 'Shougo/neocomplete.vim'
Plugin 'Shougo/neosnippet'
Plugin 'jlrickert/neosnippet-snippets'
Plugin 'Yggdroot/indentLine'
Plugin 'rhysd/vim-clang-format'        "formatting

" Visual Plugins
Plugin 'altercation/vim-colors-solarized'
Plugin 'itchyny/lightline.vim'
Plugin 'nicwest/rainbow_parentheses.vim'

call vundle#end()
filetype plugin indent on

"===============================================================================
" General settings
"===============================================================================
set number                      "Line numbers are good
set backspace=indent,eol,start  "Allow backspace in insert mode
set history=1000                "Store lots of :cmdline history
set showcmd                     "Show incomplete cmds down the bottom
set showmode                    "Show current mode down the bottom
set gcr=a:blinkon0              "Disable cursor blink
set visualbell                  "No sounds
set autoread                    "Reload files changed outside vim
set fileformats=unix,dos,mac    "Prefer Unix over Windows over OS 9 formats
set ambiwidth=double            "make all charactors the same width
set clipboard=unnamed           "clipboard now defaults to system

" This makes vim act like all other editors, buffers can
" exist in the background without being in a window.
" http://items.sjbach.com/319/configuring-vim-right
set hidden

" The mapleader has to be set before vundle starts loading all the plugins.
let mapleader="\<Space>"

"------------------------------------------------------------------------------
" Turn Off Swap Files
"------------------------------------------------------------------------------
set noswapfile
set nobackup
set nowb

"------------------------------------------------------------------------------
" Performance
"------------------------------------------------------------------------------
set lazyredraw
set nocursorline
set nocursorcolumn

"------------------------------------------------------------------------------
" Persistent Undo
"------------------------------------------------------------------------------
" Keep undo history across sessions, by storing in file.
" Only works all the time.
if has('persistent_undo')
  silent !mkdir ~/.vim/backups > /dev/null 2>&1
  set undodir=~/.vim/backups
  set undofile
endif

"------------------------------------------------------------------------------
" Indentation
"------------------------------------------------------------------------------

set autoindent
set smartindent
set smarttab
set shiftwidth=2
set softtabstop=2
set tabstop=2
set expandtab
set textwidth=0

filetype plugin on
filetype indent on

" Display tabs and trailing spaces visually

set nowrap       "Don't wrap lines
set wrapscan
set linebreak    "Wrap lines at convenient points

"------------------------------------------------------------------------------
" Folds
"------------------------------------------------------------------------------
set foldopen=block,insert,jump,mark,percent,quickfix,search,tag,undo
set foldmethod=indent   "fold based on indent
set foldnestmax=3       "deepest fold is 3 levels
set foldenable          "fold by default
set foldlevel=99

"------------------------------------------------------------------------------
" Completion
"------------------------------------------------------------------------------
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
set wildignore+=go/pkg                           " Go static files
set wildignore+=go/bin                           " Go bin files
set wildignore+=go/bin-vagrant                   " Go bin-vagrant files
set wildignore+=*.pyc                            " Python byte code
set wildignore+=*.orig                           " Merge resolution files
set complete=.,w,b,u,t,i,kspell
set completeopt=longest,menuone

"------------------------------------------------------------------------------
" Snippets
"------------------------------------------------------------------------------
" For snippet_complete marker.
if has('conceal')
  set conceallevel=2 concealcursor=i
endif

let g:acp_enableAtStartup = 0
" Use neocomplete.
let g:neocomplete#enable_at_startup = 1
" Use smartcase.
let g:neocomplete#enable_smart_case = 1
" Set minimum syntax keyword length.
let g:neocomplete#sources#syntax#min_keyword_length = 3
let g:neocomplete#lock_buffer_name_pattern = '\*ku\*'

" Define dictionary.
let g:neocomplete#sources#dictionary#dictionaries = {
      \ 'default' : '',
      \ 'vimshell' : $HOME.'/.vimshell_hist',
      \ 'scheme' : $HOME.'/.gosh_completions'
      \ }

" overides other completion frameworks if present
let g:neocomplete#force_overwrite_completefunc = 1

" Default # of completions is 100, that's crazy.
let g:neocomplete#max_list = 6

" Define keyword.
if !exists('g:neocomplete#keyword_patterns')
  let g:neocomplete#keyword_patterns = {}
endif
let g:neocomplete#keyword_patterns['default'] = '\h\w*'

" Enable omni completion.
autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags
autocmd FileType ruby setlocal omnifunc=rubycomplete#Complete

" Enable heavy omni completion.
if !exists('g:neocomplete#sources#omni#input_patterns')
  let g:neocomplete#sources#omni#input_patterns = {}
endif
let g:neocomplete#sources#omni#input_patterns.php = '[^. \t]->\h\w*\|\h\w*::'
let g:neocomplete#sources#omni#input_patterns.c = '[^.[:digit:] *\t]\%(\.\|->\)'
let g:neocomplete#sources#omni#input_patterns.cpp = '[^.[:digit:] *\t]\%(\.\|->\)\|\h\w*::'

"------------------------------------------------------------------------------
" Search Settings
"------------------------------------------------------------------------------

set incsearch       " Find the next match as we type the search
set hlsearch        " Hilight searches by default
set viminfo='100,f1 " Save up to 100 marks, enable capital marks
set ignorecase      " Ignore case when searching...
set smartcase       " ...unless we type a capital

"------------------------------------------------------------------------------
" Scrolling
"------------------------------------------------------------------------------

set scrolloff=8         "Start scrolling when we're 8 lines away from margins
set sidescrolloff=15
set sidescroll=1

"------------------------------------------------------------------------------
" mouse
"------------------------------------------------------------------------------

set guicursor=n-v-c:block-Cursor-blinkon0,ve:ver35-Cursor,o:hor50-Cursor,i-ci:ver25-Cursor,r-cr:hor20-Cursor,sm:block-Cursor-blinkwait175-blinkoff150-blinkon175
set guioptions=acg
set mousehide
" set mouse=a

"------------------------------------------------------------------------------
" Syntastic
"------------------------------------------------------------------------------

set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_enable_signs=1
let g:syntastic_always_populate_loc_list = 1
" let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

let g:syntastic_cpp_remove_include_errors = 1
let g:syntastic_cpp_check_header = 0
let g:syntastic_cpp_compiler_options = '-std=c++11'

"------------------------------------------------------------------------------
" Delimate
"------------------------------------------------------------------------------

" prevent delimate from eating the space bar when pressed
let g:delimitMate_expand_cr = 1
let g:delimitMate_expand_space = 1

imap <expr> <CR> pumvisible() ? "\<c-y>" : "<Plug>delimitMateCR"

"------------------------------------------------------------------------------
" CtrlP Settings
"------------------------------------------------------------------------------

if executable('ag')
  " Use ag over grep
  set grepprg=ag\ --nogroup\ --nocolor

  " Use ag in CtrlP for listing files. Lightning fast and respects .gitignore
  let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'

  " ag is fast enough that CtrlP doesn't need to cache
  let g:ctrlp_use_caching = 0
endif

let g:ctrlp_map = ''
let g:ctrlp_switch_buffer = 'E'
let g:ctrlp_tabpage_position = 'c'
let g:ctrlp_working_path_mode = 'rc'
let g:ctrlp_root_markers = ['.project.root']
let g:ctrlp_custom_ignore = '\v'
let g:ctrlp_custom_ignore .= '%(/\.'
let g:ctrlp_custom_ignore .= '%(git|hg|svn)|'
let g:ctrlp_custom_ignore .= '\.%(class|o|png|jpg|jpeg|bmp|tar|jar|tgz|deb|zip|xml|html)$|'
let g:ctrlp_custom_ignore .= '/target/%(quickfix|resolution-cache|streams)|'
let g:ctrlp_custom_ignore .= '/target/scala-2.1./%(classes|test-classes|sbt-0.13|cache)|'
let g:ctrlp_custom_ignore .= '/project/target|/project/project'
let g:ctrlp_custom_ignore .= '/resources/%(out)'
let g:ctrlp_custom_ignore .= '/node_modules'
let g:ctrlp_custom_ignore .= ')'
let g:ctrlp_custom_ignore .= '\.%o'
let g:ctrlp_open_new_file = 'r'
let g:ctrlp_open_multiple_files = '1ri'
let g:ctrlp_match_window = 'max:40'
let g:ctrlp_prompt_mappings = {
      \ 'PrtSelectMove("j")':   ['<c-n>'],
      \ 'PrtSelectMove("k")':   ['<c-p>'],
      \ 'PrtHistory(-1)':       ['<c-j>', '<down>'],
      \ 'PrtHistory(1)':        ['<c-i>', '<up>']
      \ }

"------------------------------------------------------------------------------
" NerdTree
"------------------------------------------------------------------------------

" Make nerdtree look nice
let NERDTreeMinimalUI = 1
let NERDTreeDirArrows = 1
let g:NERDTreeWinSize = 30

" Auto open nerd tree on startup
let g:nerdtree_tabs_open_on_gui_startup = 0
" Focus in the main content window
let g:nerdtree_tabs_focus_on_files = 1

"------------------------------------------------------------------------------
" Autocmd
"------------------------------------------------------------------------------
" no whitespaces
autocmd BufWritePre * :%s/\s\+$//e

" change to unix endlines
autocmd BufWritePre * :set fileformat=unix

function! s:MkNonExDir(file, buf)
  if empty(getbufvar(a:buf, '&buftype')) && a:file!~#'\v^\w+\:\/'
    let dir=fnamemodify(a:file, ':h')
    if !isdirectory(dir)
      call mkdir(dir, 'p')
    endif
  endif
endfunction
augroup BWCCreateDir
  autocmd!
  autocmd BufWritePre * :call s:MkNonExDir(expand('<afile>'), +expand('<abuf>'))
augroup END

au CursorHold * if exists("t:NerdTreeBufName") | call <SNR>15_refreshRoot() | endif

"------------------------------------------------------------------------------
" Eye candy
"------------------------------------------------------------------------------

"turn on syntax highlighting
syntax on

"make the colors look nice
set t_Co=256
set encoding=utf-8
colorscheme solarized
set background=dark

set colorcolumn=80
highlight ColorColumn ctermbg=233

let g:lightline = {
      \ 'colorscheme': 'solarized',
      \ 'active': {
      \   'left': [ [ 'mode', 'paste' ], [ 'fugitive', 'filename' ], ['ctrlpmark'] ],
      \   'right': [ [ 'syntastic', 'lineinfo' ], ['percent'], [ 'fileformat', 'fileencoding', 'filetype' ] ]
      \ },
      \ 'component_function': {
      \   'fugitive': 'MyFugitive',
      \   'filename': 'MyFilename',
      \   'fileformat': 'MyFileformat',
      \   'filetype': 'MyFiletype',
      \   'fileencoding': 'MyFileencoding',
      \   'mode': 'MyMode',
      \   'ctrlpmark': 'CtrlPMark',
      \ },
      \ 'component_expand': {
      \   'syntastic': 'SyntasticStatuslineFlag',
      \ },
      \ 'component_type': {
      \   'syntastic': 'error',
      \ },
      \ 'subseparator': { 'left': '|', 'right': '|' }
      \ }

function! MyModified()
  return &ft =~ 'help' ? '' : &modified ? '+' : &modifiable ? '' : '-'
endfunction

function! MyReadonly()
  return &ft !~? 'help' && &readonly ? '' : ''
endfunction

function! MyFilename()
  let fname = expand('%:t')
  return fname == 'ControlP' ? g:lightline.ctrlp_item :
        \ fname == '__Tagbar__' ? g:lightline.fname :
        \ fname =~ '__Gundo\|NERD_tree' ? '' :
        \ &ft == 'vimfiler' ? vimfiler#get_status_string() :
        \ &ft == 'unite' ? unite#get_status_string() :
        \ &ft == 'vimshell' ? vimshell#get_status_string() :
        \ ('' != MyReadonly() ? MyReadonly() . ' ' : '') .
        \ ('' != fname ? fname : '[No Name]') .
        \ ('' != MyModified() ? ' ' . MyModified() : '')
endfunction

function! MyFugitive()
  try
    if expand('%:t') !~? 'Tagbar\|Gundo\|NERD' && &ft !~? 'vimfiler' && exists('*fugitive#head')
      let mark = ' '  " edit here for cool mark
      let _ = fugitive#head()
      return strlen(_) ? mark._ : ''
    endif
  catch
  endtry
  return ''
endfunction

function! MyFileformat()
  return winwidth(0) > 70 ? &fileformat : ''
endfunction

function! MyFiletype()
  return winwidth(0) > 70 ? (strlen(&filetype) ? &filetype : 'no ft') : ''
endfunction

function! MyFileencoding()
  return winwidth(0) > 70 ? (strlen(&fenc) ? &fenc : &enc) : ''
endfunction

function! MyMode()
  let fname = expand('%:t')
  return fname == '__Tagbar__' ? 'Tagbar' :
        \ fname == 'ControlP' ? 'CtrlP' :
        \ fname == '__Gundo__' ? 'Gundo' :
        \ fname == '__Gundo_Preview__' ? 'Gundo Preview' :
        \ fname =~ 'NERD_tree' ? 'NERDTree' :
        \ &ft == 'unite' ? 'Unite' :
        \ &ft == 'vimfiler' ? 'VimFiler' :
        \ &ft == 'vimshell' ? 'VimShell' :
        \ winwidth(0) > 60 ? lightline#mode() : ''
endfunction

function! CtrlPMark()
  if expand('%:t') =~ 'ControlP'
    call lightline#link('iR'[g:lightline.ctrlp_regex])
    return lightline#concatenate([g:lightline.ctrlp_prev, g:lightline.ctrlp_item
          \ , g:lightline.ctrlp_next], 0)
  else
    return ''
  endif
endfunction

let g:ctrlp_status_func = {
      \ 'main': 'CtrlPStatusFunc_1',
      \ 'prog': 'CtrlPStatusFunc_2',
      \ }

function! CtrlPStatusFunc_1(focus, byfname, regex, prev, item, next, marked)
  let g:lightline.ctrlp_regex = a:regex
  let g:lightline.ctrlp_prev = a:prev
  let g:lightline.ctrlp_item = a:item
  let g:lightline.ctrlp_next = a:next
  return lightline#statusline(0)
endfunction

function! CtrlPStatusFunc_2(str)
  return lightline#statusline(0)
endfunction

let g:tagbar_status_func = 'TagbarStatusFunc'

function! TagbarStatusFunc(current, sort, fname, ...) abort
  let g:lightline.fname = a:fname
  return lightline#statusline(0)
endfunction

augroup AutoSyntastic
  autocmd!
  autocmd BufWritePost *.c,*.cpp call s:syntastic()
augroup END
function! s:syntastic()
  SyntasticCheck
  call lightline#update()
endfunction

let g:unite_force_overwrite_statusline = 0
let g:vimfiler_force_overwrite_statusline = 0
let g:vimshell_force_overwrite_statusline = 0

let g:rbpt_colorpairs = [
      \ ['brown',       'RoyalBlue3'],
      \ ['Darkblue',    'SeaGreen3'],
      \ ['darkgray',    'DarkOrchid3'],
      \ ['darkgreen',   'firebrick3'],
      \ ['darkcyan',    'RoyalBlue3'],
      \ ['darkred',     'SeaGreen3'],
      \ ['darkmagenta', 'DarkOrchid3'],
      \ ['brown',       'firebrick3'],
      \ ['gray',        'RoyalBlue3'],
      \ ['black',       'SeaGreen3'],
      \ ['darkmagenta', 'DarkOrchid3'],
      \ ['Darkblue',    'firebrick3'],
      \ ['darkgreen',   'RoyalBlue3'],
      \ ['darkcyan',    'SeaGreen3'],
      \ ['darkred',     'DarkOrchid3'],
      \ ['red',         'firebrick3'],
      \ ]

let g:rbpt_max = 16
let g:rbpt_loadcmd_toggle = 0
let g:rbpt_disabled_colors = ['black', 'brown']

au VimEnter * RainbowParenthesesToggle
au Syntax * RainbowParenthesesLoadRound
au Syntax * RainbowParenthesesLoadSquare
au Syntax * RainbowParenthesesLoadBraces

set laststatus=2 "always show the statusline

"===============================================================================
" Keymap
"===============================================================================
" easy window splits
set splitbelow
set splitright

nnoremap <silent> vv <C-w>v
nnoremap <silent> ss <C-w>s

" tabs
nnoremap <leader>wp :tabprev<CR>
nnoremap <leader>wn :tabnext<CR>
nnoremap <leader>wc :tabe<CR>k

" Map Control-# to switch tabs
map  <C-0> 0gt
imap <C-0> <Esc>0gt
map  <C-1> 1gt
imap <C-1> <Esc>1gt
map  <C-2> 2gt
imap <C-2> <Esc>2gt
map  <C-3> 3gt
imap <C-3> <Esc>3gt
map  <C-4> 4gt
imap <C-4> <Esc>4gt
map  <C-5> 5gt
imap <C-5> <Esc>5gt
map  <C-6> 6gt
imap <C-6> <Esc>6gt
map  <C-7> 7gt
imap <C-7> <Esc>7gt
map  <C-8> 8gt
imap <C-8> <Esc>8gt
map  <C-9> 9gt
imap <C-9> <Esc>9gt

" navigation
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

nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

" CtrlP
nmap <leader>fb :CtrlPBuffer<cr>
nmap <leader>ff :CtrlP .<cr>
nmap <leader>fF :execute ":CtrlP " . expand('%:p:h')<cr>
nmap <leader>fr :CtrlP<cr>
nmap <leader>fm :CtrlPMixed<cr

" tag
noremap [ <C-]>
noremap ] <C-t>

nnoremap j gj
nnoremap k gk

" go to last edit position
nnoremap ,. '.

" command fast mode
nore ; :

" toggle paste mode
nmap <silent> <leader>p :set invpaste<CR>:set paste?<CR>

"Reindent whole file
map <leader>= mzgg=G`z<CR>

" reload vim
nmap <Leader>sv :source $MYVIMRC<CR>

" load plugins and modes
map <leader>ln :NERDTreeTabsToggle<CR>
nnoremap <leader>ls :setlocal spell! spell?<CR>
nnoremap <leader>s :mksession<CR>
nmap <leader>lt :TagbarToggle<CR>


nnoremap <Leader>/ :TComment<CR>
vnoremap <Leader>/ :TComment<CR>

" ========== Steve Losh hacks ==========="
" w!! to write a file as sudo
cmap w!! w !sudo tee % >/dev/null

function! s:VSetSearch()
  let temp = @@
  norm! gvy
  let @/ = '\V' . substitute(escape(@@, '\'), '\n', '\\n', 'g')
  let @@ = temp
endfunction

" Prettify json
com! JSONFormat %!python -m json.tool

" clear search highlighting
nmap <silent> <leader>? :set invhlsearch<CR>

" Code folding options
nmap <leader>f0 :set foldlevel=0<CR>
nmap <leader>f1 :set foldlevel=1<CR>
nmap <leader>f2 :set foldlevel=2<CR>
nmap <leader>f3 :set foldlevel=3<CR>
nmap <leader>f4 :set foldlevel=4<CR>
nmap <leader>f5 :set foldlevel=5<CR>
nmap <leader>f6 :set foldlevel=6<CR>
nmap <leader>f7 :set foldlevel=7<CR>
nmap <leader>f8 :set foldlevel=8<CR>
nmap <leader>f9 :set foldlevel=9<CR>

" Snippets
imap jk <Plug>(neosnippet_expand_or_jump)

imap <expr><TAB> pumvisible() ? "\<C-n>" : "\<TAB>"
smap <expr><TAB> pumvisible() ? "\<C-n>" : "\<TAB>"
xmap <expr><TAB> pumvisible() ? "\<C-n>" : "\<TAB>"

" <CR>: close popup and save indent.
inoremap <silent> <CR> <C-r>=<SID>my_cr_function()<CR>
function! s:my_cr_function()
  return neocomplete#close_popup() . "\<CR>"
  " For no inserting <CR> key.
  "return pumvisible() ? neocomplete#close_popup() : "\<CR>"
endfunction
" <TAB>: completion.
inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"
" <C-h>, <BS>: close popup and delete backword char.
inoremap <expr><BS> neocomplete#smart_close_popup()."\<C-h>"
" Close popup by <Space>.
inoremap <expr><Space> pumvisible() ? neocomplete#close_popup() . "\<Space>" : "\<Space>"
