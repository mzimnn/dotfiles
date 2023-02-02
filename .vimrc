" set relative line numbers
set relativenumber
set number

" enable syntax highlighting
syntax on

" show existing tab with 4 spaces width
set tabstop=4
" when indenting with '>', use 4 spaces width
set shiftwidth=4
" when pressing tab, insert 4 spaces
set expandtab

" tell what background the terminal uses
set background=dark
" highlight current line
set cursorline
highlight CursorLine cterm=NONE ctermbg=235

" change color of the line number
highlight LineNr cterm=NONE ctermfg=DarkGrey

" search as characters are entered
set incsearch
" live match highlighting
set showmatch

" ignore case in search patterns
set ignorecase
set smartcase

" make tabs and trailing spaces visible
set list
if &encoding == 'utf-8'
    set listchars=tab:\\xbb\ ,trail:\\xb7,nbsp:\\u23b5
else
    set listchars=tab:>\ ,trail:.
endif

" show line and column number
set ruler
" show command in bottom bar
set showcmd

" set text width
autocmd FileType gitcommit setlocal textwidth=72

" mapping
nnoremap <Leader>h :set hlsearch!<Esc>
nnoremap <Leader>s :set spell!<Esc>
nnoremap <Leader>t :term<Esc><C-W>L

" allow saving file as root when starting vim without sudo
cnoremap w!! w !sudo tee >/dev/null %
