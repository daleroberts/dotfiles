set nocompatible
filetype off

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'gmarik/vundle'
Plugin 'sjl/gundo.vim'
Plugin 'nanotech/jellybeans.vim'
Plugin 'airblade/vim-gitgutter'
Plugin 'Valloric/YouCompleteMe'
"Plugin 'scrooloose/syntastic'
"Plugin 'tomtom/tlib_vim'
"Plugin 'MarcWeber/vim-addon-mw-utils'
"Plugin 'garbas/vim-snipmate'
"Plugin 'daleroberts/vim-snippets'

call vundle#end()
filetype plugin indent on
syntax on

colorscheme jellybeans

set autochdir
set noautoread " tells vim not to automatically reload changed files
set modeline
set scrolloff=6 "" keep cursor away from edge
set visualbell "" no beep
set ruler "" show line and col number
set autowrite "" write file on make
set autoindent
set ttyfast
set backspace=indent,eol,start
set ignorecase
set smartcase
set incsearch "" incremental search
set spelllang=en_au
set nobackup
set backupdir=/tmp "" backups here
set nowb
set noswapfile
set directory=/tmp "" swap here
set nofoldenable    " disable folding
set wildmenu "" enhanced command completion
set wildmode=list:longest
set wildignore+=*.swp,*.bak,*.pyc,*.class,*.pdf,*.aux,*.toc,*.lof,*.lot,*.tdo,*.out,*.png,*.o,*.obj,.git,.svn,*.avi,*.zip,*.rar,*.doc,*.tar,*.jpg,*git*,*.gz,*.djvu,*~*,*.log,*.nav,*.snm,*.sig,*.avi,*.xls,*.pdf,*.aux,*.log,*.docx,*latexmk,*.rtf,*.ipe
set nowrap
set undolevels=2048
set t_Co=256
set splitbelow
set splitright
set diffopt=vertical,filler,context:1000000 
set mouse=a
set fillchars=vert:\│
set colorcolumn=79

if version >= 701
  set rnu "" relative line numbers
  set undodir=~/undo
  set undofile
endif

let mapleader = ','
let filetype_m="mma"
let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlPMixed'
let g:gundo_width = 30
let g:gundo_right = 1
let g:tex_flavor='latex'

let g:jedi#goto_assignments_command = ""
let g:jedi#goto_definitions_command = "gd"
let g:jedi#documentation_command = "K"
let g:jedi#usages_command = ""
let g:jedi#completions_command = ""
let g:jedi#rename_command = ""
let g:jedi#popup_on_dot = 0
let g:jedi#popup_select_first = 0
let g:jedi#show_call_signatures = 1
let g:jedi#use_tabs_not_buffers = 0
let g:jedi#auto_vim_configuration = 0

let g:ultisnips_python_style="sphinx"
let g:syntastic_cpp_compiler = 'g++-4.9'
let g:syntastic_cpp_compiler_options = ' -std=c++11'
let g:syntastic_check_on_open=1
let g:syntastic_python_checkers=["pylint"]
let g:syntastic_matlab_checkers=["mlint"]
let g:SuperTabDefaultCompletionType = "context"

let g:ycm_confirm_extra_conf = 0
let g:ycm_global_ycm_extra_conf = '~/.dotfiles/.ycm_extra_conf.py'

nnoremap ; :
nnoremap <silent> <C-l> :nohl<CR><C-l>
nnoremap j gj
nnoremap k gk
nnoremap U :GundoToggle<CR>
vmap Q gq
nmap Q gqap
map s <Leader><Leader>w
map S <Leader><Leader>b
nmap <CR> :write<CR>
map <space> /\v
map <Leader>a :wa\|:VimuxRunLastCommand<CR>

if exists(":Tabularize")
  nmap <Leader>a= :Tabularize /=<CR>
  vmap <Leader>a= :Tabularize /=<CR>
  nmap <Leader>a: :Tabularize /:\zs<CR>
  vmap <Leader>a: :Tabularize /:\zs<CR>
endif

hi EasyMotionTarget ctermbg=none ctermfg=green guifg=#F6AA11 

function! DoDiffSaved()
  let filetype=&ft
  diffthis
  vnew | r # | normal! 1Gdd
  diffthis
  exe "setlocal bt=nofile bh=wipe nobl noswf ro ft=" . filetype
endfunction

com! DiffSaved call DoDiffSaved()
map <Leader>ds :DiffSaved<CR>

autocmd FileType r setlocal tw=0 expandtab shiftwidth=2 tabstop=8 formatoptions+=croq softtabstop=2 autoindent textwidth=79 
autocmd FileType cpp setlocal tw=0 expandtab shiftwidth=4 tabstop=8 formatoptions+=croq softtabstop=4 autoindent textwidth=79 
autocmd FileType python setlocal tw=0 expandtab shiftwidth=4 tabstop=8 formatoptions+=croq softtabstop=4 autoindent textwidth=79 
autocmd FileType python map <Leader>f :w<CR>:%!autopep8 %<CR>
