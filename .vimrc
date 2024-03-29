set nocompatible

"colorscheme jellybeans
colorscheme zellner

set autochdir
set noautoread " tells vim not to automatically reload changed files
set modeline
set scrolloff=6 "" keep cursor away from edge
set visualbell "" no beep
set ruler "" show line and col number
set autowrite "" write file on make
set autoindent
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
set rnu "" relative line numbers
set undodir=~/undo
set undofile

nnoremap ; :
nnoremap <silent> <C-l> :nohl<CR><C-l>
nnoremap j gj
nnoremap k gk
vmap Q gq
nmap Q gqap
nmap <CR> :write<CR>
map <space> /\v

autocmd FileType r setlocal tw=0 expandtab shiftwidth=2 tabstop=8 formatoptions+=croq softtabstop=2 autoindent textwidth=79 
autocmd FileType cpp setlocal tw=0 expandtab shiftwidth=4 tabstop=8 formatoptions+=croq softtabstop=4 autoindent textwidth=79 
autocmd FileType python setlocal tw=0 expandtab shiftwidth=4 tabstop=8 formatoptions+=croq softtabstop=4 autoindent textwidth=79 
