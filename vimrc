call  plug#begin('~/.local/share/nvim/plugged')

Plug 'junegunn/vim-easy-align'
Plug 'junegunn/goyo.vim'
Plug 'tpope/vim-fugitive'
Plug 'vimwiki/vimwiki'
Plug 'mattn/emmet-vim'
Plug 'junegunn/fzf'
Plug 'junegunn/fzf.vim'
Plug 'itchyny/lightline.vim'
Plug 'tpope/vim-surround'
Plug 'scrooloose/nerdtree'
Plug 'joshdick/onedark.vim'

call plug#end()

" Basics
    set nocompatible
    filetype plugin on
    set modelines=0
    set encoding=utf-8
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

" Set colorscheme
    let $NVIM_TUI_ENABLE_TRUE_COLOR=1
    syntax enable
    colorscheme onedark

" Autocompletion
    set wildmode=longest,list,full

" Open split at the bottom and right
    set splitbelow splitright

" Highlighy last column
    set colorcolumn=110
    highlight ColorColumn ctermbg=darkgray

" Spit navigation
    map <C-h> <C-w>h
    map <C-j> <C-w>j
    map <C-k> <C-w>k
    map <C-l> <C-w>l

" Delete trailing whitespaces on save
    autocmd BufWritePre * %s/\s\+$//e

" Make map
    nnoremap <F4> :make!<CR>

" vim-easy-align plugin
    " Start interactive EasyAlign in visual mode (e.g. vipga)
    xmap ga <Plug>(EasyAlign)

    " Start interactive EasyAlign for a motion/text object (e.g. gaip)
    nmap ga <Plug>(EasyAlign)

" goyo plugin
    nmap gy :Goyo<CR>

" emmet-vim plugin
    let g:user_emmet_install_global = 0
    autocmd FileType html,css EmmetInstall
    let g:user_emmet_leader_key='<C-Z>'

" fzf plugin
    map ; :Files<CR>

" lightline plugin
    set laststatus=2
    set noshowmode
    let g:lightline = { 'colorscheme': 'onedark' }

" nerdtree plugin
    map <C-o> :NERDTreeToggle<CR>

" onedark plugin
    let g:onedark_terminal_italics=1
