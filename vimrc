call  plug#begin('~/.local/share/nvim/plugged')

Plug 'tpope/vim-sensible'
Plug 'tpope/vim-fugitive'
Plug 'vimwiki/vimwiki'
" Plug 'mattn/emmet-vim'
Plug 'junegunn/fzf.vim'
Plug 'itchyny/lightline.vim'
Plug 'tpope/vim-surround'
Plug 'scrooloose/nerdtree'
" Plug 'xuyuanp/nerdtree-git-plugin'
" Plug 'morhetz/gruvbox'
Plug 'arcticicestudio/nord-vim'
Plug 'sirver/ultisnips'
Plug 'honza/vim-snippets'
Plug 'neoclide/coc.nvim', {'do': { -> coc#util#install()}}
" Plug 'easymotion/vim-easymotion'
" Plug 'turbio/bracey.vim'
Plug 'pangloss/vim-javascript'
Plug 'mxw/vim-jsx'
Plug 'Yggdroot/indentLine'
Plug 'airblade/vim-gitgutter'
Plug 'romainl/vim-cool'
Plug 'jiangmiao/auto-pairs'
" Plug 'gko/vim-coloresque'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-commentary'
" Plug 'tweekmonster/django-plus.vim'
" Plug 'peterhoeg/vim-qml'
Plug 'lepture/vim-jinja'
Plug 'moll/vim-bbye'
Plug 'luochen1990/rainbow'
Plug 'craigemery/vim-autotag'
Plug 'jalvesaq/nvim-r'
Plug 'prettier/vim-prettier', { 'do': 'yarn install' }

call plug#end()

" Basics
    set nocompatible
    filetype plugin on
    set hidden
    set modelines=0
    set encoding=utf-8
    set noswapfile
    set nobackup
    set nowritebackup
    set ignorecase
    set smartcase
    set incsearch
    set smartindent
    set wrap
    set nu rnu
    set cursorline

    " tabs set to 4 spaces and 2 spaces with html
    set tabstop=4
    set softtabstop=4
    set shiftwidth=4
    set expandtab
    set showcmd
    set cmdheight=2

    set updatetime=300

    set signcolumn=yes

    set shortmess+=c

    autocmd BufRead,BufNewFile *.htm,*.html setlocal tabstop=2 shiftwidth=2 softtabstop=2

    " system clipboard access
    set clipboard=unnamedplus

" Set colorscheme
    let $NVIM_TUI_ENABLE_TRUE_COLOR=1
    syntax enable
    " let g:gruvbox_italic=1
    colorscheme nord

" Autocompletion
    set wildmode=longest,list,full

" Open split at the bottom and right
    set splitbelow splitright

" Highlighy last column
    set colorcolumn=110

" leader mapping
    let mapleader = ","

" Spit navigation
    map <C-h> <C-w>h
    map <C-j> <C-w>j
    map <C-k> <C-w>k
    map <C-l> <C-w>l

" Buffers mapping
    nmap <Tab> :bnext<CR>
    nmap <S-Tab> :bprevious<CR>
    " That is <Alt> + <Tab>
    nmap <M-Tab> :Buffers<CR>
    nmap <leader>q :Bdelete<CR>
    nmap <leader>d :bd!<CR>

" Delete trailing whitespaces on save
    autocmd BufWritePre * %s/\s\+$//e

" Make mapping
    nnoremap <F4> :make!<CR>

" Plus buffer mapping
    vnoremap <C-c> "+y
    map <C-p> "+P

" Terminal mapping
    nmap <leader>t :terminal<CR>

" Terminal resize mapping
    nmap <leader>r :res 15<CR>

" Split mapping
    nmap <leader>v :vsplit<CR>
    nmap <leader>c :new<CR>:res 15<CR>:terminal<CR>
    nmap <leader>b :split<CR>

" emmet-vim plugin
    " let g:user_emmet_leader_key='<C-M>'
    " let g:user_emmet_install_global = 0
    " autocmd FileType html,css,jsx,js EmmetInstall

" fzf plugin
    map <leader><leader> :Files<CR>

" lightline plugin
    set laststatus=2
    set noshowmode
    let g:lightline = {
    \   'colorscheme': 'nord',
    \   'active': {
    \       'left': [
    \                   [ 'mode', 'paste' ],
    \                   [  'cocstatus', 'gitbranch', 'readonly', 'filename', 'modified' ],
    \               ]
    \   },
    \   'component_function': {
    \       'gitbranch': 'fugitive#head',
    \       'cocstatus': 'coc#status',
    \   },
    \ }

" nerdtree plugin
    map <C-o> :NERDTreeToggle<CR>
    " let g:NERDTreeDirArrowExpandable = '>'
    " let g:NERDTreeDirArrowCollapsible = '<'

" ultisnips plugin
    let g:UltiSnipsExpandTrigger="<C-X>"

" vim-easymotion
"     let g:EasyMotion_do_mapping = 0
"     let g:EasyMotion_smartcase = 1

"     nmap s <Plug>(easymotion-overwin-f)
"     map <Leader>j <Plug>(easymotion-j)
"     map <Leader>k <Plug>(easymotion-k)

" vim-jsx
    let g:jsx_ext_required = 0 " Allow JSX in ordinary JS files

" indentLine
    let g:indentLine_char = '▏'

" coc
    " Use tab for trigger completion with characters ahead and navigate.
    " Use command ':verbose imap <tab>' to make sure tab is not mapped by other plugin.
    inoremap <silent><expr> <TAB>
          \ pumvisible() ? "\<C-n>" :
          \ <SID>check_back_space() ? "\<TAB>" :
          \ coc#refresh()
    inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

    function! s:check_back_space() abort
      let col = col('.') - 1
      return !col || getline('.')[col - 1]  =~# '\s'
    endfunction

    " Use <c-space> for trigger completion.
    inoremap <silent><expr> <c-space> coc#refresh()

    " Use <cr> for confirm completion, `<C-g>u` means break undo chain at current position.
    " Coc only does snippet and additional edit on confirm.
    inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"

    " Use `[c` and `]c` for navigate diagnostics
    nmap <silent> [c <Plug>(coc-diagnostic-prev)
    nmap <silent> ]c <Plug>(coc-diagnostic-next)

    " Remap keys for gotos
    nmap <silent> gd <Plug>(coc-definition)
    nmap <silent> gy <Plug>(coc-type-definition)
    nmap <silent> gi <Plug>(coc-implementation)
    nmap <silent> gr <Plug>(coc-references)

    " Use K for show documentation in preview window
    nnoremap <silent> K :call <SID>show_documentation()<CR>

    function! s:show_documentation()
      if &filetype == 'vim'
        execute 'h '.expand('<cword>')
      else
        call CocAction('doHover')
      endif
    endfunction

    " Highlight symbol under cursor on CursorHold
    autocmd CursorHold * silent call CocActionAsync('highlight')

    " Remap for rename current word
    nmap <leader>rn <Plug>(coc-rename)

    " Remap for format selected region
    vmap <leader>f  <Plug>(coc-format-selected)
    nmap <leader>f  <Plug>(coc-format-selected)

    augroup mygroup
      autocmd!
      " Setup formatexpr specified filetype(s).
      autocmd FileType typescript,json setl formatexpr=CocAction('formatSelected')
      " Update signature help on jump placeholder
      autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
    augroup end

    " Remap for do codeAction of selected region, ex: `<leader>aap` for current paragraph
    vmap <leader>a  <Plug>(coc-codeaction-selected)
    nmap <leader>a  <Plug>(coc-codeaction-selected)

    " Remap for do codeAction of current line
    nmap <leader>ac  <Plug>(coc-codeaction)
    " Fix autofix problem of current line
    nmap <leader>qf  <Plug>(coc-fix-current)

    " Use `:Format` for format current buffer
    command! -nargs=0 Format :call CocAction('format')

    " Use `:Fold` for fold current buffer
    command! -nargs=? Fold :call     CocAction('fold', <f-args>)

    " Using CocList
    " Show all diagnostics
    nnoremap <silent> <space>a  :<C-u>CocList diagnostics<cr>
    " Manage extensions
    nnoremap <silent> <space>e  :<C-u>CocList extensions<cr>
    " Show commands
    nnoremap <silent> <space>c  :<C-u>CocList commands<cr>
    " Find symbol of current document
    nnoremap <silent> <space>o  :<C-u>CocList outline<cr>
    " Search workspace symbols
    nnoremap <silent> <space>s  :<C-u>CocList -I symbols<cr>
    " Do default action for next item.
    nnoremap <silent> <space>j  :<C-u>CocNext<CR>
    " Do default action for previous item.
    nnoremap <silent> <space>k  :<C-u>CocPrev<CR>
    " Resume latest coc list
    nnoremap <silent> <space>p  :<C-u>CocListResume<CR>

" vim-autotag
    let g:autotagTagsFile="tags"
