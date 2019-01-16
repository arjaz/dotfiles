call  plug#begin('~/.local/share/nvim/plugged')

Plug 'junegunn/vim-easy-align'
Plug 'junegunn/goyo.vim'
Plug 'jreybert/vimagit'
Plug 'vimwiki/vimwiki'

call plug#end()

" Basics
    set nocompatible
    filetype plugin on
    syntax on
    set modelines=0
    set noswapfile
    set nobackup
    set ignorecase
    set smartcase
    set incsearch
    set smartindent
    set wrap
    set nu rnu

    " tabs set to 4 spaces and 2 spaces with html
    set tabstop=4
    set shiftwidth=4
    set expandtab
    set showcmd

    autocmd BufRead,BufNewFile *.htm,*.html setlocal tabstop=2 shiftwidth=2 softtabstop=2

" Autocompletion

    set wildmode=longest,list,full

" Open split at the bottom and right
    set splitbelow splitright

" Spit navigation
    map <C-h> <C-w>h
    map <C-j> <C-w>j
    map <C-k> <C-w>k
    map <C-l> <C-w>l

" vim-easy-align plugin
    " Start interactive EasyAlign in visual mode (e.g. vipga)
    xmap ga <Plug>(EasyAlign)

    " Start interactive EasyAlign for a motion/text object (e.g. gaip)
    nmap ga <Plug>(EasyAlign)

" goyo plugin
    nmap gy :Goyo<CR>
