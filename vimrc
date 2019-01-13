set nocompatible

filetype off

syntax on

filetype plugin indent on

set modelines=0

set wrap

set nu rnu

set tabstop=4
set shiftwidth=4
set expandtab
set showcmd

" pathogen
execute pathogen#infect()

" syntastic conf
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
let g:syntastic_warning_symbol = '>'

let g:syntastic_cpp_checkers = ['avgcc']
let g:syntastic_html_checkers = []
let g:syntastic_css_chekcers = ['csslint']

" ycm conf
let g:ycm_global_ycm_extra_conf = "~/.ycm_extra_conf.py"

" NERDTree autostart with no files or a directory
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif

autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 1 && isdirectory(argv()[0]) && !exists("s:std_in") | exe 'NERDTree' argv()[0] | wincmd p | ene | endif


" map NERDTree to Ctrl+n
map <C-n> :NERDTreeToggle<CR>

" remap emmet trigger key
let g:user_emmet_leader_key='<C-Z>'
