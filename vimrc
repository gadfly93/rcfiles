" allow mouse usage
set mouse=a
" use vim instead of vi settings
set nocompatible
" autoindent a line as the previous
set autoindent
" display current mode
set showmode
" show matching brackets
set showmatch
" do not create backup files
set nobackup
set noswapfile
" manage backspace for all cases
set backspace=indent,eol,start
" show typed keys in normal mode
set showcmd
" show cursor coordinates all the time
set ruler
" let horizontal movement of the cursor cross lines
set whichwrap+=<,>,h,l,[,]
" do not wrap long lines
set nowrap
set sidescroll=1
set sidescrolloff=10

" enable syntax highlightening
syntax on
" enable folds based on syntax
set foldmethod=syntax
set foldlevelstart=50

" TAB options
set expandtab
set tabstop=2
set shiftwidth=2
set softtabstop=2

" search options
set ignorecase
set smartcase
set hlsearch
set wrapscan
set incsearch
" type of highlight for search words
hi clear Search
hi Search ctermfg=Black ctermbg=Yellow 
" press space to clear search highlighting and any message already displayed.
nnoremap <silent> <Space> :silent noh<Bar>echo<CR>

" split window management
set splitbelow
set splitright

" autocompletion of filenames in command mode 
"set wildmode=longest,list,full
set wildmenu

" set Modeline mode so that Vim configuration can change if the open file
" requires it
set modeline

" spelling options
" set default spelling language
set spelllang=en_us
" use F5 to toggle spelling (using US English)
nnoremap <F5> :setlocal spell! <CR>
" type of highlight mode for misspelled words (when not using color scheme)
hi SpellBad ctermfg=Red term=Reverse guisp=Red gui=undercurl ctermbg=None
hi SpellCap ctermfg=Green term=Reverse guisp=Green gui=undercurl ctermbg=None
hi SpellLocal ctermfg=Blue term=Underline guisp=Blue gui=undercurl ctermbg=None
hi SpellRare ctermfg=Yellow term=underline guisp=Yellow gui=undercurl ctermbg=None

" set the leader
let mapleader = ","
" define mapping with leader
" - fast save
nnoremap <Leader><Leader> :update<CR>
vnoremap <Leader><Leader> <Esc>:update<CR>
" - fast quit
nnoremap <Leader>. :quit<CR>
vnoremap <Leader>. <Esc>:quit<CR>

" map paragraph indentation to Q without moving the cursor
" (uses mark Z to remember current cursor position)
nnoremap Q gwip

" yank till end of line
nnoremap Y y$
" leave the cursor where it is after a yank in visual mode
vmap y ygv<Esc>

" change 'inside inside' word, breaking words with underscores
nnoremap ciiw :set iskeyword-=_<CR>ciw<Esc>:set iskeyword+=_<CR>a

" CtrlP configuration
let g:ctrlp_map = '<C-p>'
let g:crtlp_cmd = 'CtrlP'
let g:ctrlp_working_path_mode = 'ra'
let g:ctrlp_by_filename = 0
let g:ctrlp_regexp_search = 1
let g:ctrlp_match_window = 'results:100'
" Use git to have list of files, faster but cannot search files
" not indexed by git
"let g:ctrlp_user_command = {                               
"  \ 'types': {
"    \ 1: ['.git', 'cd %s && git ls-files'],
"  \ },
"\ }
" Total number of files is 10000 (the nearest to current one)
" if unlimited may slow down the search
"let g:ctrlp_max_files = 0 

" default make command
:command -nargs=* Make make <args> | cwindow 10
" jump to error line when make fails
autocmd QuickFixCmdPost [^l]* nested cwindow

" enable context-aware autocompletion
filetype plugin on
set omnifunc=syntaxcomplete#Complete
" remap autocompletion to CTRL+SPACE
inoremap <C-Space> <C-x><C-o>
if !has("gui_running")
    inoremap <C-@> <C-x><C-o>
endif

" set color scheme
let g:aldmeris_transparent = 1
colorscheme aldmeris 
set number

" retab files from spaces to tabs, usage:
"   first select the text with V, then type :'<,'>SuperRetab [NUM_SPACES]
:command! -nargs=1 -range SuperRetab <line1>,<line2>s/\v%(^ *)@<= {<args>}/\t/g

" search for selected text, forwards or backwards
vnoremap <silent> * :<C-U>
  \let old_reg=getreg('"')<Bar>let old_regtype=getregtype('"')<CR>
  \gvy/<C-R><C-R>=substitute(
  \escape(@", '/\.*$^~['), '\_s\+', '\\_s\\+', 'g')<CR><CR>
  \gV:call setreg('"', old_reg, old_regtype)<CR>
vnoremap <silent> # :<C-U>
  \let old_reg=getreg('"')<Bar>let old_regtype=getregtype('"')<CR>
  \gvy?<C-R><C-R>=substitute(
  \escape(@", '?\.*$^~['), '\_s\+', '\\_s\\+', 'g')<CR><CR>
  \gV:call setreg('"', old_reg, old_regtype)<CR>

" options for Verilog files
autocmd BufNewFile,BufRead *.v    call VerilogOptions()
autocmd BufNewFile,BufRead *.sv   call VerilogOptions()
autocmd BufNewFile,BufRead *.svh  call VerilogOptions()
autocmd BufNewFile,BufRead *.mds  call VerilogOptions()
autocmd BufNewFile,BufRead *.m4   call VerilogOptions()
autocmd BufNewFile,BufRead *.app  call VerilogOptions()
autocmd BufNewFile,BufRead *.a    call VerilogOptions()

autocmd BufNewFile,BufRead *.app :set nowrap
autocmd BufNewFile,BufRead *.pm  :set nowrap

function VerilogOptions()
  set filetype=verilog_midas
  map gs <Esc>:set nohls<CR>:call FindAssignment()<CR>:set hls<CR>
  let b:match_words='% *for:% *endfor,% *if:% *endif,% *validation:% *endvalidation,% *sva_preamble:% *sva_postamble,% *vinclude:% *endvinclude,% *instrid:% *endinstrid,% *tarmac:% *endtarmac'
endfunction

function FindAssignment()
  execute "normal /\\<\<C-R>\<C-W>\\>\\([^\|>!=_A-Za-z0-9\(\)]*\\|\[[` _A-Za-z0-9:\(\)+-]*\]\\) *\\(<=\\|=\\)\\([^=]\\|$\\)/\<CR>"
  let @/ = '\V\<'.escape(expand('<cword>'), '\').'\>'
endfunction

" set do syntax for QuestaSim scripts
autocmd BufNewFile,BufRead *.do set filetype=do
" set syntax for TARMAC log
autocmd BufNewFile,BufRead *tarmac*.log set filetype=tarmac_log

