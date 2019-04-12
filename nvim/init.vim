call plug#begin('~/.local/share/nvim/plugged')
"UI
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
Plug 'vim-airline/vim-airline'
Plug 'crusoexia/vim-monokai'
Plug 'vim-airline/vim-airline-themes'

"Coding
Plug 'valloric/youcompleteme'
Plug 'tpope/vim-fugitive'
Plug 'vim-scripts/Rainbow-Parenthesis'
Plug 'tpope/vim-surround'
Plug 'mbbill/undotree'

"Snippets stuff
Plug 'honza/vim-snippets'
Plug 'sirver/ultisnips'

"Python
Plug 'python-mode/python-mode', { 'branch': 'develop' }
Plug 'janko-m/vim-test'
Plug 'heavenshell/vim-pydocstring'
"Plug 'skyleach/pudb.vim'

"Other
Plug 'wakatime/vim-wakatime'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'lervag/vimtex'
Plug 'KeitaNakamura/tex-conceal.vim', {'for': 'tex'}
Plug 'tikhomirov/vim-glsl'


Plug 'ryanoasis/vim-devicons'
call plug#end()

set runtimepath+=~/.config/nvim/my-snippets/

"LaTeX
let g:tex_flavor='latex'
let g:vimtex_view_method='zathura'
let g:vimtex_quickfix_mode=0
set conceallevel=2
let g:tex_conceal='abdmgs'

"Python
"let g:python_host_prog='/usr/bin/python'
"let g:python3_host_prog='/usr/bin/python3'
let g:pymode_python = 'python3'
let g:pymode_rope = 1
let g:pymode_rope_completion = 0


map <F8> :PymodeRun<CR>
au BufNewFile,BufRead *.frag,*.vert,*.fp,*.vp,*.glsl setf glsl

"Usability
set number
set hlsearch
set cursorline
set mouse=a
set splitbelow
set splitright

map <C-n> :NERDTreeToggle<CR>
nmap <S-Ins> "+p
map <C-m> :UndotreeToggle<CR>

"Splits
nmap <C-h> <C-W>h
nmap <C-j> <C-W>j
nmap <C-k> <C-W>k
nmap <C-l> <C-W>l

"Snippets
let g:UltiSnipsUsePythonVersion = 3
let g:UltiSnipsExpandTrigger="<C-j>"
let g:UltiSnipsJumpForwardTrigger="<s-tab>"
let g:UltiSnipsJumpBackwardTrigger=""

"Indent stuff
set tabstop=4
set shiftwidth=4
set smarttab
set expandtab
set smartindent
set autoindent

"spell
"set spell spelllang=en,ru

"ui
colorscheme monokai
set termguicolors
"set guifont=DroidSansMono\ Nerd\ Font\ 11
set laststatus=2
let g:airline_theme='powerlineish'
let g:airline_powerline_fonts = 1

hi Conceal guibg=Normal guifg=Normal
