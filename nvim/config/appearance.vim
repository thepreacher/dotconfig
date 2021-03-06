" appearance.vim
" Settings styling vim

" Color Scheme
packadd! dracula
colorscheme dracula

" Use true colors
set termguicolors

" don't syntax-highlight long lines
set synmaxcol=200

" Show whitespace
set list
set listchars=tab:▸\ ,extends:❯,precedes:❮,trail:·,nbsp:·,space:·

" Highlight the column after 'textwidth'
set colorcolumn=+1

" Set program title to 'nvim'
set title
setglobal titlestring=nvim

" Show line numbers
set number

" Highlight the current line, don't highlight the current column
set cursorline
set nocursorcolumn

" Use relative line numbers
if exists("&relativenumber")
	set relativenumber
	au BufReadPost * set relativenumber
endif

" Always draw gutter for gitgutter, signify, etc.
set signcolumn=yes

" Don't redraw for macros
set lazyredraw

" Code Folding
" See https://github.com/w0rp/ale/issues/1829
" Don't set foldmethod=syntax
set foldlevelstart=99

" Show clipboards and macros
let g:peekaboo_delay = 500
let g:peekaboo_compact = 1

" Colorize
packadd nvim-colorizer.lua
lua require 'colorizer'.setup(nil, { css = true; })
