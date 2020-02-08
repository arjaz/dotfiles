call  plug#begin('~/.local/share/nvim/plugged')

" Languages
Plug 'pangloss/vim-javascript'
Plug 'mxw/vim-jsx'
" Plug 'jalvesaq/nvim-r'
Plug 'lepture/vim-jinja'
" Plug 'jvirtanen/vim-octave'
Plug 'neovimhaskell/haskell-vim'
" Plug 'tweekmonster/django-plus.vim'
" Plug 'peterhoeg/vim-qml'
" Plug 'vim-scripts/ebnf.vim'
Plug 'vim-scripts/bnf.vim'
Plug 'godlygeek/tabular'
Plug 'plasticboy/vim-markdown'
Plug 'pboettch/vim-cmake-syntax'
Plug 'lervag/vimtex'
" Plug 'vim-scripts/slimv.vim'

" Completion and snippets
Plug 'jiangmiao/auto-pairs'
Plug 'sirver/ultisnips'
Plug 'honza/vim-snippets'
" Plug 'neoclide/coc.nvim', {'branch': 'release'}

" Tags and git
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'
Plug 'craigemery/vim-autotag'

" Text objects
Plug 'wellle/targets.vim'
" Plug 'jeetsukumaran/vim-pythonsense'

" Mechanics and general improvements
" Plug 'dhruvasagar/vim-table-mode'
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
Plug 'gko/vim-coloresque'
Plug 'tiagofumo/vim-nerdtree-syntax-highlight'
Plug 'ryanoasis/vim-devicons'

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

" fzf + devicons
    function! Fzf_dev()
        function! s:files()
            let files = split(system("git ls-files -co --exclude-standard"), '\n')
            if files[0] == "fatal: not a git repository (or any parent up to mount point /)"
                let files = split(system("find -type f"), '\n')
            endif
            return s:prepend_icon(files)
        endfunction

        function! s:prepend_icon(candidates)
            let result = []
            for candidate in a:candidates
                let filename = fnamemodify(candidate, ':p:t')
                let icon = WebDevIconsGetFileTypeSymbol(filename, isdirectory(filename))
                call add(result, printf("%s %s", icon, candidate))
            endfor

            return result
        endfunction

        function! s:edit_file(item)
            let parts = split(a:item, ' ')
            let file_path = get(parts, 1, '')
            execute 'silent e' file_path
        endfunction

        call fzf#run({
                    \ 'source': <sid>files(),
                    \ 'sink':   function('s:edit_file'),
                    \ 'options': '-m',
                    \ 'down':    '40%' })
    endfunction

" fzf plugin
    " nmap <silent> <leader><leader> :GFiles --exclude-standard --others --cached<CR>
    nmap <silent> <leader><leader> :call Fzf_dev()<CR>

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

" ultisnips plugin
    let g:UltiSnipsExpandTrigger="<C-X>"

" vim-jsx
    let g:jsx_ext_required = 0 " Allow JSX in ordinary JS files

" indentLine
    let g:indentLine_char = '▏'

" " coc
"     " Use tab for trigger completion with characters ahead and navigate.
"     " Use command ':verbose imap <tab>' to make sure tab is not mapped by other plugin.
"     inoremap <silent><expr> <TAB>
"           \ pumvisible() ? "\<C-n>" :
"           \ <SID>check_back_space() ? "\<TAB>" :
"           \ coc#refresh()
"     inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

"     function! s:check_back_space() abort
"       let col = col('.') - 1
"       return !col || getline('.')[col - 1]  =~# '\s'
"     endfunction

"     " Use <c-space> for trigger completion.
"     inoremap <silent><expr> <c-space> coc#refresh()

"     " Use <cr> for confirm completion, `<C-g>u` means break undo chain at current position.
"     " Coc only does snippet and additional edit on confirm.
"     inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"

"     " Use `[c` and `]c` for navigate diagnostics
"     nmap <silent> [c <Plug>(coc-diagnostic-prev)
"     nmap <silent> ]c <Plug>(coc-diagnostic-next)

"     " Remap keys for gotos
"     nmap <silent> gd <Plug>(coc-definition)
"     nmap <silent> gy <Plug>(coc-type-definition)
"     nmap <silent> gi <Plug>(coc-implementation)
"     nmap <silent> gr <Plug>(coc-references)

"     " Use K for show documentation in preview window
"     nnoremap <silent> K :call <SID>show_documentation()<CR>

"     function! s:show_documentation()
"       if &filetype == 'vim'
"         execute 'h '.expand('<cword>')
"       else
"         call CocAction('doHover')
"       endif
"     endfunction

"     " Highlight symbol under cursor on CursorHold
"     autocmd CursorHold * silent call CocActionAsync('highlight')

"     " Remap for rename current word
"     nmap <leader>rn <Plug>(coc-rename)

"     " Remap for format selected region
"     vmap <leader>f  <Plug>(coc-format-selected)
"     nmap <leader>f  <Plug>(coc-format-selected)

"     " function text objects
"     xmap if <Plug>(coc-funcobj-i)
"     xmap af <Plug>(coc-funcobj-a)
"     omap if <Plug>(coc-funcobj-i)
"     omap af <Plug>(coc-funcobj-a)

"     " augroup mygroup
"     "   autocmd!
"     "   " Setup formatexpr specified filetype(s).
"     "   autocmd FileType typescript,json setl formatexpr=CocAction('formatSelected')
"     "   " Update signature help on jump placeholder
"     "   autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
"     " augroup end

"     " Remap for do codeAction of selected region, ex: `<leader>aap` for current paragraph
"     " vmap <leader>a  <Plug>(coc-codeaction-selected)
"     " nmap <leader>a  <Plug>(coc-codeaction-selected)

"     " Remap for do codeAction of current line
"     nmap <leader>ac  <Plug>(coc-codeaction)
"     " Fix autofix problem of current line
"     nmap <leader>qf  <Plug>(coc-fix-current)

"     " Use `:Format` for format current buffer
"     command! -nargs=0 Format :call CocAction('format')

"     " Use `:Fold` for fold current buffer
"     " command! -nargs=? Fold :call     CocAction('fold', <f-args>)

"     " Using CocList
"     " Show all diagnostics
"     " nnoremap <silent> <space>a  :<C-u>CocList diagnostics<cr>
"     " Manage extensions
"     " nnoremap <silent> <space>e  :<C-u>CocList extensions<cr>
"     " Show commands
"     " nnoremap <silent> <space>c  :<C-u>CocList commands<cr>
"     " Find symbol of current document
"     " nnoremap <silent> <space>o  :<C-u>CocList outline<cr>
"     " Search workspace symbols
"     " nnoremap <silent> <space>s  :<C-u>CocList -I symbols<cr>
"     " Do default action for next item.
"     " nnoremap <silent> <space>j  :<C-u>CocNext<CR>
"     " Do default action for previous item.
"     " nnoremap <silent> <space>k  :<C-u>CocPrev<CR>
"     " Resume latest coc list
"     " nnoremap <silent> <space>p  :<C-u>CocListResume<CR>

" vim-autotag
    let g:autotagTagsFile=".tags"

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

" vim-lsp-cxx-highlight
    let g:lsp_cxx_hl_use_text_props = 1

" gitgutter
    let g:gitgutter_highlight_linenrs = 1

" vim-latex
    let g:tex_flavor='latex'
    let g:tex_conceal=''
