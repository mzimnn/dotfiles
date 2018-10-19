" set relative line numbers
set relativenumber
set number

syntax on
" show existing tab with 4 spaces width
set tabstop=4
" when indenting with '>', use 4 spaces width
set shiftwidth=4
" On pressing tab, insert 4 spaces
set expandtab

" highlight current line
set cursorline
highlight CursorLine cterm=NONE ctermbg=235

" change color of the line number
highlight LineNr cterm=NONE ctermfg=DarkGrey

" search as characters are entered
set incsearch
" live match highlighting
set showmatch
" highlight search matches
"set hlsearch

" make tabs and trailing spaces visible
set list
set listchars=tab:»\ ,trail:·,nbsp:⎵

" show command in bottom bar
set showcmd

" set text width
autocmd FileType gitcommit setlocal textwidth=72

" mapping
nnoremap <Leader>h :set hlsearch!<Esc>
nnoremap <Leader>t :term<Esc><C-W>L
