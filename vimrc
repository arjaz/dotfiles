call plug#begin('~/.local/share/nvim/plugged')

" Languages
Plug 'plasticboy/vim-markdown'
Plug 'pboettch/vim-cmake-syntax'

" Completion and snippets
Plug 'jiangmiao/auto-pairs'

" Mechanics and general improvements
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-surround'
Plug 'moll/vim-bbye'
Plug 'junegunn/fzf.vim'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-repeat'
Plug 'scrooloose/nerdtree'

" Visual
Plug 'itchyny/lightline.vim'
Plug 'Yggdroot/indentLine'
Plug 'arcticicestudio/nord-vim'
Plug 'kien/rainbow_parentheses.vim'
Plug 'romainl/vim-cool'
Plug 'tiagofumo/vim-nerdtree-syntax-highlight'

call plug#end()

" Basics
    set nocompatible
    filetype indent plugin off
    set hidden
    set modelines=0
    set encoding=utf-8
    set noswapfile
    set nobackup
    set nowritebackup
    set splitbelow splitright
    set ignorecase
    set smartcase
    set incsearch
    set autoindent
    set wrap
    set nu rnu
    set cursorline
    set tabstop=4
    set softtabstop=4
    set shiftwidth=4
    set expandtab
    autocmd BufRead,BufNewFile *.htm,*.html,*.xml setlocal tabstop=2 shiftwidth=2 softtabstop=2
    set showcmd
    set cmdheight=2
    set updatetime=300
    set signcolumn=yes
    set shortmess+=c
    set wildmode=longest,list,full

" Preview substitutions
    set inccommand=nosplit

" Folding
    set foldmethod=indent
    set foldlevelstart=99

" Set colorscheme
    let $NVIM_TUI_ENABLE_TRUE_COLOR=1
    syntax enable
    colorscheme nord
    set colorcolumn=110

" Delete trailing whitespaces on save
    autocmd BufWritePre * %s/\s\+$//e

" leader mapping
    let mapleader = ","

" map esc
    imap jk <esc>
    imap kj <esc>

" Split navigation
    map <C-h> <C-w>h
    map <C-j> <C-w>j
    map <C-k> <C-w>k
    map <C-l> <C-w>l

" Buffers mapping
    nmap <Tab> :bnext<CR>
    nmap <S-Tab> :bprevious<CR>
    nmap <leader>q :Bdelete<CR>
    nmap <leader>d :bd!<CR>
    nmap <leader>b :Buffers

" Plus buffer
    set clipboard=unnamedplus
    vnoremap <C-c> "+y
    map <C-p> "+P

" Terminal mapping
    nmap <leader>t :terminal<CR>
    nmap <leader>c :new<CR>:res 15<CR>:terminal<CR>

" Terminal resize mapping
    nmap <leader>r :res 15<CR>

" Split mapping
    nmap <leader>v :vsplit<CR>
    nmap <leader>h :split<CR>

" disable json conceal
    let g:vim_json_conceal = 0

" fzf plugin
    nmap <silent> <leader><leader> :GFiles --exclude-standard --others --cached<CR>

" lightline plugin
    set laststatus=2
    set noshowmode
    let g:lightline = {
    \   'colorscheme': 'nord',
    \   'active': {
    \       'left': [
    \                   [ 'mode', 'paste' ],
    \                   [ 'gitbranch', 'readonly', 'filename', 'modified' ],
    \               ]
    \   },
    \   'component_function': {
    \       'gitbranch': 'fugitive#head'
    \   },
    \ }

" nerdtree plugin
    map <C-o> :NERDTreeToggle<CR>
    let g:NERDTreeShowLineNumbers=1
    let g:NERDTreeMinimalUI=1
    let g:NERDTreeShowHidden=1

" indentLine
    let g:indentLine_char = '▏'

" rainbow_parentheses
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
    \ ['darkmagenta', 'DarkOrchid3'],
    \ ['Darkblue',    'firebrick3'],
    \ ['darkgreen',   'RoyalBlue3'],
    \ ['darkcyan',    'SeaGreen3'],
    \ ['darkred',     'DarkOrchid3'],
    \ ['red',         'firebrick3'],
    \ ]
    au VimEnter * RainbowParenthesesToggle
    au Syntax * RainbowParenthesesLoadRound
    au Syntax * RainbowParenthesesLoadSquare
    au Syntax * RainbowParenthesesLoadBraces
