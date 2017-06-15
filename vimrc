set relativenumber
set number

syntax on
set tabstop=4							" show existing tab with 4 spaces width
set shiftwidth=4						" when indenting with '>', use 4 spaces width

set cursorline							" highlight current line
highlight CursorLine cterm=NONE ctermbg=235

" change color of the line number
highlight LineNr cterm=NONE ctermfg=DarkGrey

set incsearch							" search as characters are entered
"set hlsearch							" highlight search matches

set list							" make tabs and trailing spaces visible
set listchars=tab:»\ ,trail:·,nbsp:⎵
