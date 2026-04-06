" Set relative line numbers
set relativenumber
set number

" Enable syntax highlighting
syntax on

" Show existing tab with 4 spaces width
set tabstop=4
" When indenting with '>', use 4 spaces width
set shiftwidth=4
" When pressing tab, insert 4 spaces
set expandtab

" Tell what background the terminal uses
if getenv('BACKGROUND') ==# 'light'
    set background=light
else
    set background=dark
endif
" Highlight current line
set cursorline
if getenv('BACKGROUND') ==# 'light'
    highlight CursorLine cterm=NONE ctermbg=254
else
    highlight CursorLine cterm=NONE ctermbg=235
endif

" Change color of the line number
highlight LineNr cterm=NONE ctermfg=DarkGrey

" Search as characters are entered
set incsearch
" Live match highlighting
set showmatch

" Ignore case in search patterns
set ignorecase
set smartcase

" Make tabs and trailing spaces visible
set list
if &encoding == 'utf-8'
    set listchars=tab:\\xbb\ ,trail:\\xb7,nbsp:\\u23b5
else
    set listchars=tab:>\ ,trail:.
endif

" Show line and column number
set ruler
" Show command in bottom bar
set showcmd

" Set text width
autocmd FileType gitcommit setlocal textwidth=72

" Mapping
nnoremap Y y$
nnoremap <Leader>h :set hlsearch!<Esc>
nnoremap <Leader>s :set spell!<Esc>
nnoremap <Leader>t :term<Esc><C-W>L

" Allow saving file as root when starting vim without sudo
cnoremap w!! w !sudo tee >/dev/null %

" Basic key bindings of GNU Emacs
" Inspired by: https://github.com/maxbrunsfeld/vim-emacs-bindings/blob/master/plugin/emacs-bindings.vim
inoremap <C-a> <Home>
inoremap <C-b> <Left>
inoremap <C-d> <Del>
inoremap <C-e> <End>
inoremap <C-f> <Right>
inoremap <C-k> <C-o>D
