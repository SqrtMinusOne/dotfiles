call plug#begin('~/.local/share/nvim/plugged')

"UI
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
Plug 'jistr/vim-nerdtree-tabs'
Plug 'xuyuanp/nerdtree-git-plugin'
Plug 'tiagofumo/vim-nerdtree-syntax-highlight'

Plug 'vim-airline/vim-airline'
Plug 'crusoexia/vim-monokai'
Plug 'vim-airline/vim-airline-themes'
Plug 'lifepillar/vim-solarized8'

Plug 'kien/tabman.vim'

Plug 'chrisbra/colorizer'

"Coding
"Plug 'valloric/youcompleteme'
Plug 'shougo/deoplete.nvim'
Plug 'tpope/vim-fugitive'
Plug 'luochen1990/rainbow'
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

"Js & stuff
Plug 'pangloss/vim-javascript'
Plug 'mxw/vim-jsx'
Plug 'posva/vim-vue'
Plug 'alvan/vim-closetag'

"LaTeX
Plug 'lervag/vimtex'
Plug 'KeitaNakamura/tex-conceal.vim', {'for': 'tex'}

"Navigation
"Plug 'ctrlpvim/ctrlp.vim'
Plug 'easymotion/vim-easymotion'
Plug 'yuttie/comfortable-motion.vim'
Plug 'vim-scripts/restore_view.vim'
Plug 'ericbn/vim-relativize'
Plug 'majutsushi/tagbar'

"Other files
Plug 'elzr/vim-json'
Plug 'tpope/vim-jdaddy'
Plug 'tikhomirov/vim-glsl'
Plug 'plasticboy/vim-markdown'
Plug 'digitaltoad/vim-jade'
Plug 'chrisbra/csv.vim'

"General syntax check
"Plug 'scrooloose/syntastic'
Plug 'w0rp/ale'

"Other
"Plug 'https://gitlab.com/code-stats/code-stats-vim.git'
Plug 'wakatime/vim-wakatime'
Plug 'nathanaelkane/vim-indent-guides'

Plug 'raimondi/delimitmate'
"Plug 'cohama/lexima.vim'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --bin' }
Plug 'junegunn/fzf.vim'

Plug 'ryanoasis/vim-devicons'
call plug#end()

set runtimepath+=~/.config/nvim/my-snippets/
let g:codestats_api_key="SFMyNTY.VTNGeWRFMXBiblZ6VDI1bCMjTlRRek1RPT0.V3iEAki_kRH75zwoXQ3u5Zng-Q_h0XRlUsb9ld09Cdc"
let $FZF_DEFAULT_COMMAND='fd --type f --exclude .git'

"LaTeX {{{
let g:tex_flavor='latex'
let g:vimtex_fold_enabled = 1
let g:vimtex_view_method='zathura'
let g:vimtex_quickfix_mode=0
set conceallevel=2
let g:tex_conceal='abdmgs'
"let g:syntastic_tex_lacheck_quiet_messages = { 'regex': ['\Vpossible unwanted space at', '\VUse ` to begin'] }
call deoplete#custom#var('omni', 'input_patterns', {
            \   'tex': g:vimtex#re#deoplete
            \})
let g:ale_tex_chktex_options='-I --nowarn 32'

" }}}

"Python {{{
"let g:python_host_prog='/usr/bin/python'
"let g:python3_host_prog='/usr/bin/python3'
let g:pymode_python = 'python3'
let g:pymode_lint = 0
let g:pymode_rope = 1
let g:pymode_rope_completion = 0
let g:pymode_rope_autoimport = 0
let NERDTreeIgnore = ['\.pyc$', '^__pycache__$']

" }}}

let g:closetag_filenames = "*.html,*.xhtml,*.phtml,*.php,*.jsx"

au BufNewFile,BufRead *.frag,*.vert,*.fp,*.vp,*.glsl setf glsl

"Usability {{{
set number
set hlsearch
set cursorline
set mouse=a
set splitbelow
set splitright

" }}}

" Mappings {{{

noremap <C-n> :NERDTreeToggle<CR>
nnoremap <S-Ins> "+p
noremap <C-m> :UndotreeToggle<CR>

noremap <silent> <ScrollWheelDown> :call comfortable_motion#flick(40)<CR>
noremap <silent> <ScrollWheelUp>   :call comfortable_motion#flick(-40)<CR>
nnoremap <Tab> :TagbarToggle<CR>

nnoremap <C-p> :Files<CR>
:tnoremap <Esc> <C-\><C-n>
nnoremap , :lclose<CR> :pclose<CR> :cclose<CR>
noremap - ddkP
noremap _ ddp


let g:UltiSnipsUsePythonVersion = 3
let g:UltiSnipsExpandTrigger="<C-j>"
let g:UltiSnipsJumpForwardTrigger="<C-S-j>"
let g:UltiSnipsJumpBackwardTrigger=""

" }}}

" Subterminal {{{

function! GetSubTerminalName()
    let current_tabpage = tabpagenr()
    return current_tabpage . '_terminal'
endfunction

function! SubTerminal()
    let subterminal_name = GetSubTerminalName()
    let terminal_num = bufnr(subterminal_name)
    if terminal_num == -1
        execute ':new ' . subterminal_name
        execute ':resize 10'
        if isdirectory("venv")
            execute ':terminal'
        else
            execute ':terminal'
        endif
        normal i
    else
        execute ':bwipeout! ' . terminal_num 
        execute ':bwipeout! ' . (terminal_num + 1)
    endif
endfunction

function! WipeSubTerminalBuffer()
    let subterminal_name = GetSubTerminalName()
    let terminal_num = bufnr(subterminal_name)
    if terminal_num != -1
        execute ':bwipeout! ' . terminal_num
    endif
endfunction

autocmd BufWipeout term://* call WipeSubTerminalBuffer()

nnoremap ` :call SubTerminal()<CR>

" }}}

"Indent & folding stuff {{{
set tabstop=4
set shiftwidth=4
set smarttab
set expandtab
set smartindent
set autoindent

set foldmethod=syntax
set foldlevelstart=2

" Filetypes {{{

augroup filetype_pug
    autocmd!
    autocmd Filetype pug setlocal foldmethod=indent
    autocmd Filetype pug setlocal foldlevel=20
augroup END

augroup filetype_vue
    autocmd!
augroup END

au Filetype python
            \ setlocal foldmethod=indent

au Filetype tex
            \ setlocal foldlevel=0

au Filetype vim 
            \ setlocal foldmethod=marker

" }}}

let g:indent_guides_enable_on_vim_startup = 1
let g:indent_guides_exclude_filetypes = ['help', 'nerdtree', 'tagbar']
let g:indent_guides_guide_size = 1

" }}}

"Syntax check {{{
syntax on
"set statusline+=%#warningmsg#
"set statusline+=%{SyntasticStatuslineFlag()}
"set statusline+=%*
"
"let g:syntastic_ignore_files = ['\.py$', '\.tex$']
"let g:syntastic_always_populate_loc_list = 1
"let g:syntastic_auto_loc_list = 1
"let g:syntastic_check_on_open = 1
"let g:syntastic_check_on_wq = 0

let g:ale_open_list = 'on_save'
let g:ale_list_window_size = 7
let g:ale_close_preview_on_insert = 1
let g:ale_lint_on_text_changed = 'never'
let g:ale_lint_on_enter = 0
let g:ale_completion_enabled = 0
let g:ale_linters = {'python': ['pyls'], 'tex': ['chktex']}
let g:ale_fixers = {
            \    'python': ['yapf', 'isort', 'remove_trailing_lines', 'trim_whitespace'],
            \    'tex': ['latexindent', 'textlint', 'remove_trailing_lines', 'trim_whitespace']
            \}
let g:airline#extensions#ale#enabled = 1

" }}}

autocmd QuitPre * if empty(&bt) | lclose | endif

" Deoplete {{{

let g:deoplete#enable_at_startup = 1
call deoplete#custom#source('ale', 'rank', 999)

inoremap <silent><expr> <TAB>
            \ pumvisible() ? "\<C-n>" :
            \ <SID>check_back_space() ? "\<TAB>" :
            \ deoplete#mappings#manual_complete()
function! s:check_back_space() abort 
    let col = col('.') - 1
    return !col || getline('.')[col - 1]  =~ '\s'
endfunction

" }}}
"spell
"set spell spelllang=en,ru

" ui {{{
colorscheme solarized8
set background=dark
let g:solarized_visibility='high'
set termguicolors
"set guifont=DroidSansMono\ Nerd\ Font\ 11
set laststatus=2

" Airline
let g:airline_theme='powerlineish'
let g:airline_powerline_fonts = 1
"let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#formatter = 'unique_tail'

highlight! TermCursorNC guibg=red

let g:colorizer_auto_filetype='css,html,python,js'
let g:tagbar_sort = 0
let g:tagbar_show_line_numbers = 1
let g:tagbar_width = 60

" Brackets
let g:rainbow_active = 1
let g:rainbow_conf = {
            \   'guifgs': ['red', 'yellow', 'lightgreen', 'lightblue'],
            \ }
hi Conceal guibg=Normal guifg=Normal
let g:closetag_filenames = '*.html,*.xhtml,*.vue'
let g:closetag_filetypes = 'html,xhtml,vue'
let delimitMate_matchpairs = "(:),[:],{:}"
" }}}
