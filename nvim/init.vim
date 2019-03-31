call plug#begin('~/.local/share/nvim/plugged')
"UI
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
Plug 'vim-airline/vim-airline'
Plug 'crusoexia/vim-monokai'

"Coding
Plug 'valloric/youcompleteme'
Plug 'tpope/vim-fugitive'
Plug 'vim-scripts/Rainbow-Parenthesis'
Plug 'tpope/vim-surround'
Plug 'sirver/ultisnips'

"Python
Plug 'python-mode/python-mode', { 'branch': 'develop' }
Plug 'janko-m/vim-test'
"Plug 'skyleach/pudb.vim'

"Other
Plug 'ryanoasis/vim-devicons'
Plug 'wakatime/vim-wakatime'
"Plug 'lervag/vimtex'


call plug#end()

"LaTeX
"let g:tex_flavor='latex'
"let g:vimtex_view_method='zathura'
"let g:vimtex_quickfix_mode=0
"set conceallevel=1
"let g:tex_conceal='abdmg'

"Python
"let g:python_host_prog='/usr/bin/python'
"let g:python3_host_prog='/usr/bin/python3'
let g:pymode_python = 'python3'
map <F8> :PymodeRun<CR>

"Usability
set number
set hlsearch
set mouse=a
set splitbelow
set splitright

map <C-n> :NERDTreeToggle<CR>
nmap <S-Ins> "+p


"Indent stuff
set tabstop=4
set shiftwidth=4
set smarttab
set expandtab
set smartindent
set autoindent

"ui
colorscheme monokai
set termguicolors
set guifont=DroidSansMono\ Nerd\ Font\ 11
