" curl -fLo ~/.vim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
call plug#begin('~/.local/share/nvim/plugged')
" UI
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
Plug 'xuyuanp/nerdtree-git-plugin'
Plug 'jistr/vim-nerdtree-tabs'
Plug 'luochen1990/rainbow'
Plug 'arcticicestudio/nord-vim'

Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" Global replace
Plug 'skwp/greplace.vim'

" VCS
Plug 'tpope/vim-fugitive'

" Coding
Plug 'valloric/matchtagalways'
Plug 'alvan/vim-closetag'
Plug 'tpope/vim-surround'
Plug 'junegunn/vim-easy-align'
Plug 'tpope/vim-commentary'
Plug 'christoomey/vim-sort-motion'
Plug 'AndrewRadev/splitjoin.vim'
Plug 'tpope/vim-repeat'
Plug 'jiangmiao/auto-pairs'

" Moving around
Plug 'easymotion/vim-easymotion'
Plug 'justinmk/vim-sneak'
Plug 'vim-scripts/restore_view.vim'

" Text objects
Plug 'kana/vim-textobj-user'
Plug 'kana/vim-textobj-indent'
Plug 'kana/vim-textobj-line'

" Misc
Plug 'wakatime/vim-wakatime'

call plug#end()

" General settings {{{

set hidden
set nobackup
set nowritebackup
set shortmess+=c

set exrc
set secure
set number
set relativenumber

set hlsearch
set cursorline
set mouse=a
set splitbelow
set splitright
set switchbuf=vsplit
set redrawtime=250

" Indent
set tabstop=4
set shiftwidth=4
set smarttab
set expandtab
set autoindent

" Folding
set foldmethod=syntax
set foldlevelstart=20

" }}}

" Mappings {{{
nnoremap <C-n> :NERDTreeToggle<CR>
nnoremap <S-Ins> "+p

tnoremap <Esc> <C-\><C-n>
nnoremap , :lclose<CR> :pclose<CR> :cclose<CR> :noh<CR>

noremap - ddkP
noremap _ ddp
nnoremap H ^
nnoremap L $

nnoremap ; :

nnoremap gn :tabnew<CR>
nnoremap gN :tabclose<CR>

xmap ga <Plug>(EasyAlign)
nmap ga <Plug>(EasyAlign)
" }}}

" Functions {{{
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
"
" Terminal in tab {{{
function! OpenTerminalInTab()
    execute ':tabnew'
    execute ':terminal'
    normal i
endfunction

nnoremap ~ :call OpenTerminalInTab()<CR>
" }}}

" {{{ Delete Hidden buffers
function! DeleteHiddenBuffers()
    let tpbl=[]
    let closed = 0
    let terminals = 0
    call map(range(1, tabpagenr('$')), 'extend(tpbl, tabpagebuflist(v:val))')
    for buf in filter(range(1, bufnr('$')), 'bufexists(v:val) && index(tpbl, v:val)==-1')
        if getbufvar(buf, '&mod') == 0
            if matchstr(bufname(buf), '^term:\/\/.*$') == ''
                silent execute 'bwipeout' buf
                let closed += 1
            else
                silent execute 'bwipeout!' buf
                let closed += 1
                let terminals += 1
            endif
        endif
    endfor
    echo "Closed " .closed. " hidden buffers (" . terminals . " terminals)"
endfunction

command! DeleteHiddenBuffers call DeleteHiddenBuffers()
" }}}

" {{{ Better Home
function! Home()
    let l:before = getpos('.')
    normal ^
    let l:after = getpos('.')
    if l:before[2] == l:after[2]
        call cursor(l:after[0], 1)
    endif
endfunction

nnoremap <silent> <Home> :call Home()<CR>

" }}}

" }}}

" UI {{{
set background=dark
colorscheme nord
" let g:solarized_visibility='high'
" tmux cursor
if exists('+termguicolors')
  let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
  let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
  let &t_SI="\<Esc>[2 q"
  set termguicolors
endif
"set guifont=DroidSansMono\ Nerd\ Font\ 11
set laststatus=2

highlight! TermCursorNC guibg=red
"highlight Conceal guibg=Normal guifg=Normal
highlight! link Conceal Normal
highlight! illuminatedWord cterm=underline gui=underline

" Airline
let g:airline_theme='nord'
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#buffer_idx_mode = 1
let g:airline#extensions#tabline#formatter = 'unique_tail'

" Brackets
let g:rainbow_active = 1
let g:rainbow_conf = {
            \   'guifgs': ['red', 'yellow', 'lightgreen', 'lightblue'],
            \   'separately': { 'nerdtree': 0, 'vimwiki': 0, 'sparql': 0 }
            \ }

" }}}
