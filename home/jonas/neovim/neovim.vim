filetype plugin indent on
syntax enable

"" Airline
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#left_sep = ' '
let g:airline#extensions#tabline#left_alt_sep = '|'

"" QuickScope
let g:qs_highlight_on_keys = ['f', 'F']
let g:qs_max_chars = 300

"" Better-Whitesapces
let g:better_whitespace_enabled = 1

"" Vim Config
let maplocalleader = "\\"

if has('termguicolors')
    set termguicolors
endif

let g:tex_flavor = "latex"

set undofile
set undolevels=1000
set undodir=~/.vim/undodir
set expandtab

set number
set numberwidth=4
set nocursorline

set spell

set autoindent
set shiftwidth=4
set smartindent
set smarttab
set softtabstop=4
set lazyredraw
set nojoinspaces
set noshowmode " for echodoc.vim
set showtabline=2

set clipboard=

let g:bufferline_echo = 0
let g:bufferline_modified = ' +'
let g:bufferline_show_bufnr = 0
let g:bufferline_separator = ' '
let g:bufferline_active_buffer_left = ''
let g:bufferline_active_buffer_right = ''
let g:bufferline_fname_mod = ':t:s?^$?[No Name]?'

autocmd BufNewFile,BufRead *.md set shiftwidth=4
autocmd BufNewFile,BufRead *.md set softtabstop=4

" enables the sign bar on the left
set signcolumn=yes

set t_Co=256
set background=dark
colorscheme base16-eighties

" rebind exit
cnoreabbrev W! w!
cnoreabbrev Q! q!
cnoreabbrev Qall! qall!
cnoreabbrev Wq wq
cnoreabbrev Wa wa
cnoreabbrev wQ wq
cnoreabbrev WQ wq
cnoreabbrev W w
cnoreabbrev Q q
cnoreabbrev Qall qall

" auto complete
tnoremap <Esc> <C-\><C-n>
inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
autocmd! CompleteDone * if pumvisible() == 0 | pclose | endif

" change tabs
nnoremap <tab>   :bnext<CR>
nnoremap <S-tab> :bprevious<CR>

let mapleader = "\<Space>"

nnoremap <up> <nop>
nnoremap <down> <nop>
inoremap <up> <nop>
inoremap <down> <nop>
inoremap <left> <nop>
inoremap <right> <nop>

set backspace=indent,eol,start
set autoindent

" open in same line
if has("autocmd")
    au BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$") | exe "normal! g`\"" | endif
endif
