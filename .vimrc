set nocompatible
filetype off

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

Bundle 'gmarik/vundle'
Bundle 'nanotech/jellybeans.vim'
Bundle 'sjl/gundo.vim'
Bundle 'kien/ctrlp.vim'
Bundle 'benmills/vimux'
Bundle 'godlygeek/tabular'

if version >= 701
  Bundle 'davidhalter/jedi-vim'
endif

filetype plugin indent on

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
let g:jedi#use_tabs_not_buffers = 0
let g:jedi#popup_on_dot = 0
let g:jedi#show_function_definition = "0"
let g:ultisnips_python_style="sphinx"
let g:syntastic_cpp_compiler = 'g++-4.7'
let g:syntastic_cpp_compiler_options = ' -std=c++0x'

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

autocmd FileType python setlocal tw=0 expandtab shiftwidth=4 tabstop=8 formatoptions+=croq softtabstop=4 autoindent textwidth=79 
autocmd FileType python map <Leader>f :w<CR>:%!autopep8 %<CR>
