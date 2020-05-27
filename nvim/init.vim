call plug#begin('~/.local/share/nvim/plugged')

" UI
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
Plug 'xuyuanp/nerdtree-git-plugin'
Plug 'nathanaelkane/vim-indent-guides'
Plug 'jistr/vim-nerdtree-tabs'
Plug 'luochen1990/rainbow'
Plug 'RRethy/vim-illuminate'

Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'edkolev/tmuxline.vim'

" Plug 'lilydjwg/colorizer'
Plug 'mhinz/vim-startify'

Plug 'camspiers/animate.vim'
Plug 'camspiers/lens.vim'

" Plug 'kien/tabman.vim'

" LaTeX
Plug 'lervag/vimtex'
Plug 'KeitaNakamura/tex-conceal.vim', {'for': 'tex'}

" Python
Plug 'python-mode/python-mode', { 'branch': 'develop' }
" Plug 'ivanov/vim-ipython'

" Js & Co.
Plug 'pangloss/vim-javascript'
Plug 'posva/vim-vue'
" Plug 'heavenshell/vim-jsdoc'
Plug 'leafgarland/typescript-vim'
Plug 'mattn/emmet-vim'
Plug 'leafOfTree/vim-svelte-plugin'
" Plug 'chrisbra/colorizer'
" Plug 'mxw/vim-jsx'

" C++
" Plug 'octol/vim-cpp-enhanced-highlight'
" Plug 'zchee/deoplete-clang'

" Misc file formats
Plug 'elzr/vim-json'
Plug 'plasticboy/vim-markdown'
" Plug 'adimit/prolog.vim'
" Plug 'udalov/kotlin-vim'
" Plug 'suan/vim-instant-markdown', {'for': 'markdown'} "npm -g install instant-markdown-d
Plug 'euclio/vim-markdown-composer'
Plug 'chrisbra/csv.vim'
Plug 'aklt/plantuml-syntax'
Plug 'tyru/open-browser.vim'
Plug 'weirongxu/plantuml-previewer.vim'
" Plug 'rvesse/vim-sparql'
" Plug 'tikhomirov/vim-glsl'
" Plug 'digitaltoad/vim-jade'
" Plug 'tpope/vim-jdaddy'

"Git
Plug 'tpope/vim-fugitive'
Plug 'junegunn/gv.vim'
Plug 'airblade/vim-gitgutter'
Plug 'jreybert/vimagit'
" Plug 'drn/blamer.nvim', {'branch': 'silence-file-length-fatal-messages'}

"Snippets stuff
Plug 'honza/vim-snippets'
Plug 'sirver/ultisnips'

" General powerful plugins
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'w0rp/ale'
" Plug 'shougo/deoplete.nvim'
Plug 'janko-m/vim-test'
Plug 'metakirby5/codi.vim'
" Plug 'axvr/zepl.vim'
" Plug 'valloric/youcompleteme'
" Plug 'scrooloose/syntastic'

" Coding & navigation
Plug 'valloric/matchtagalways'
Plug 'alvan/vim-closetag'
Plug 'tpope/vim-surround'
Plug 'junegunn/vim-easy-align'
Plug 'tpope/vim-commentary'
Plug 'justinmk/vim-sneak'
Plug 'christoomey/vim-sort-motion'
Plug 'AndrewRadev/splitjoin.vim'
Plug 'unblevable/quick-scope'
" Plug 'terryma/vim-multiple-cursors'
" Plug 'scrooloose/nerdcommenter'
" Plug 'chrisbra/nrrwrgn'

Plug 'jiangmiao/auto-pairs'
" Plug 'raimondi/delimitmate'
" Plug 'cohama/lexima.vim'

Plug 'dhruvasagar/vim-table-mode'
Plug 'bronson/vim-trailing-whitespace'

Plug 'dkprice/vim-easygrep'
Plug 'easymotion/vim-easymotion'
Plug 'liuchengxu/vista.vim'
Plug 'pechorin/any-jump.vim'
Plug 'vim-scripts/restore_view.vim'
Plug 'tpope/vim-repeat'
Plug 'kkoomen/vim-doge'
" Plug 'kshenoy/vim-signature'
" Plug 'ericbn/vim-relativize'
" Plug 'majutsushi/tagbar'
" Plug 'yuttie/comfortable-motion.vim'
" Plug 'derekwyatt/vim-fswitch'

" Different extensions
Plug 'kana/vim-submode'
Plug 'skywind3000/asynctasks.vim'
Plug 'skywind3000/asyncrun.vim'

" Undo tree implementation
Plug 'sjl/gundo.vim'
" Plug 'mbbill/undotree'
" Plug 'simnalamburt/vim-mundo'

" Custom text objects
Plug 'kana/vim-textobj-user'
Plug 'kana/vim-textobj-indent'
Plug 'kana/vim-textobj-line'
" Plug 'kana/vim-textobj-entire'

" Themes
Plug 'drewtempelmeyer/palenight.vim'
" Plug 'crusoexia/vim-monokai'
" Plug 'lifepillar/vim-solarized8'
" Plug 'haishanh/night-owl.vim'
" Plug 'arcticicestudio/nord-vim'

" fuzzy file stuff
Plug 'liuchengxu/vim-clap'
" Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --bin' }
" Plug 'ctrlpvim/ctrlp.vim'

" Misc
Plug 'wakatime/vim-wakatime'
Plug 'vimwiki/vimwiki'
" Plug 'tpope/vim-rhubarb'
Plug 'mtth/scratch.vim'

" Has to be plugged the last
Plug 'ryanoasis/vim-devicons'
Plug 'tiagofumo/vim-nerdtree-syntax-highlight'
call plug#end()

" General settings {{{
set runtimepath+=~/.config/nvim/my-snippets/
let $FZF_DEFAULT_COMMAND='fd --type f --exclude .git'

set hidden

set nobackup
set nowritebackup
set shortmess+=c

set exrc
set secure
set number
set relativenumber
set linebreak
set nojoinspaces

set hlsearch
set cursorline
set mouse=a
set splitbelow
set splitright
set inccommand=split
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
" nnoremap <C-p> :Files<CR>
nnoremap <C-p> :Clap files<CR>
nnoremap <Leader>ca :Clap grep<CR>
nnoremap <Leader>aa :Clap grep<CR>
nnoremap <Leader>cl :Clap blines<CR>
nnoremap <Leader>cc :Clap filer<CR>
nnoremap <Leader>ch :Clap history<CR>
nnoremap <Leader>cj :Clap jumps<CR>

" nnoremap <Leader>s :%s/\<<C-r><C-w>\>/

tnoremap <Esc> <C-\><C-n>
tnoremap <A-q> <C-\><C-n> :call chansend(b:terminal_job_id, "\<lt>Esc>")<CR>i
nnoremap <Leader>q :lclose<CR> :pclose<CR> :cclose<CR> :noh<CR>

noremap - ddkP
noremap _ ddp
nnoremap H ^
nnoremap L $

nnoremap <End> $

" Toggle conceal
nnoremap <Leader>hc :let &cole=(&cole == 2) ? 0 : 2 <bar> echo 'conceallevel ' . &cole <CR>

" lens.vim
nnoremap <Leader>hl :call lens#toggle()<CR>

" colorizer
nnoremap <Leader>ho :ColorToggle<CR>

" Delete line into _
nnoremap <leader>d "_d
vnoremap <leader>d "_d

" lol
" nnoremap ; :

nnoremap <Leader>u :GundoToggle<CR>

" noremap <silent> <ScrollWheelDown> :call comfortable_motion#flick(40)<CR>
" noremap <silent> <ScrollWheelUp>   :call comfortable_motion#flick(-40)<CR>
" nnoremap <S-Tab> :TagbarToggle<CR>
nnoremap <S-Tab> :Vista!!<CR>

" vim-test
nnoremap <Leader>tl :TestLast<CR>
nnoremap <Leader>tf :TestFile<CR>
nnoremap <Leader>ts :TestSuit<CR>
nnoremap <Leader>tn :TestNearest<CR>
nnoremap <Leader>tv :TestVisit<CR>

" Tabs
nnoremap gn :tabnew<CR>
nnoremap gN :tabclose<CR>
" nnoremap t1 1gt
" nnoremap t2 2gt
" nnoremap t3 3gt
" nnoremap t4 4gt
" nnoremap t5 5gt
" nnoremap t6 6gt
" nnoremap t7 7gt
" nnoremap t8 8gt
" nnoremap t9 9gt

" ALE
nnoremap <Leader>af :ALEFix<CR>
nnoremap <Leader>ad :ALEGoToDefinition<CR>
nnoremap <Leader>asd :ALEGoToDefinitionInVSplit<CR>
nnoremap <Leader>assd :ALEGoToDefinitionInSplit<CR>
nnoremap <Leader>atd :ALEGoToDefinitionInTab<CR>
nnoremap <Leader>ar :ALEFindReferences<CR>
nnoremap <Leader>ah :ALEHover<CR>
nnoremap <Leader>ac :ALERename<CR>

" coc
" nnoremap <silent> <Leader>ad <Plug>(coc-definition)
" nnoremap <silent> <Leader>ai <Plug>(coc-implementation)
" nnoremap <silent> <Leader>ar <Plug>(coc-references)
" nnoremap <Leader>ac <Plug>(coc-rename)

" Async tasks
nnoremap <Leader>wl :AsyncTaskList<CR>
nnoremap <Leader>wr :AsyncTask 
nnoremap <Leader>wa :CocList tasks<CR>


" Replace
nnoremap <Leader>r :%s/\<<C-r><C-w>\>/

" REPL
" nnoremap <leader>r :Repl<CR>

" EasyAlign
xmap ga <Plug>(EasyAlign)
nmap ga <Plug>(EasyAlign)

" quick-scope
let g:qs_highlight_on_keys = ['f', 'F', 't', 'T']

" vim-sneak
let g:sneak#s_next = 1

" Close vim-clap with Esc
autocmd FileType clap_input inoremap <silent> <buffer> <Esc> <Esc>:call clap#handler#exit()<CR>

" vimwiki
nnoremap <Leader>wah :VimwikiAll2HTML<CR>
nnoremap <Leader>wl :VimwikiTabnewLink<CR>

" FSwitch
" noremap + :FSAbove<CR>

" Snippets
let g:UltiSnipsUsePythonVersion = 3
let g:UltiSnipsExpandTrigger="<a-q>"
let g:UltiSnipsJumpForwardTrigger="<a-q>"
let g:UltiSnipsJumpBackwardTrigger=""

" Multiple cursors
let g:multi_cursor_start_word_key = '<C-m>'

" Scratch
let g:scratch_no_mappings = 1
"}}}

" Windows management mode {{{
" Submode settings
let g:submode_always_show_submode = 1
let g:submode_timeout = 0
let g:submode_keyseqs_to_leave = []

" Enter and leave the mode
call submode#enter_with('Windows', 'n', '', '<Leader>s', ':call WindowsModeEnter()<CR>')
call submode#map('Windows', 'n', 'x', '<Esc>', ':call WindowsModeLeave()<CR>')

function! WindowsModeEnter()
    if g:lens#disabled == 0
        let g:lens#disabled = 1
    endif
endfunction

function! WindowsModeLeave()
    let g:lens#disabled = 0
    call lens#run()
endfunction

" Switch to the left. If the window is left-most, swith to the right-most window
" of the previous tab
function! SwitchLeft()
    let l:win = winnr()
    execute 'wincmd h'
    if l:win == winnr()
        normal gT
        execute winnr('$') . 'wincmd w'
    endif
endfunction

" The same for the left
function! SwitchRight()
    let l:win = winnr()
    execute 'wincmd l'
    if l:win == winnr()
        normal gt
        execute 1 . 'wincmd w'
    endif
endfunction

" Switching
call submode#map('Windows', 'n', '', 'j', '<C-w>j')
call submode#map('Windows', 'n', '', 'k', '<C-w>k')
call submode#map('Windows', 'n', '', 'h', ':call SwitchLeft()<CR>')
call submode#map('Windows', 'n', '', 'l', ':call SwitchRight()<CR>')

call submode#map('Windows', 'n', '', '<Down>', '<C-w>j')
call submode#map('Windows', 'n', '', '<Up>', '<C-w>k')
call submode#map('Windows', 'n', '', '<Left>', ':call SwitchLeft()<CR>')
call submode#map('Windows', 'n', '', '<Right>', ':call SwitchRight()<CR>')

" Closing
call submode#map('Windows', 'n', '', 'q', '<C-w>c')
call submode#map('Windows', 'n', '', 'Q', ':q!')

" Moving windows
call submode#map('Windows', 'n', '', '<C-j>', '<C-w>J')
call submode#map('Windows', 'n', '', '<C-k>', '<C-w>K')
call submode#map('Windows', 'n', '', '<C-h>', '<C-w>H')
call submode#map('Windows', 'n', '', '<C-l>', '<C-w>L')

call submode#map('Windows', 'n', '', '<C-Down>', '<C-w>J')
call submode#map('Windows', 'n', '', '<C-Up>', '<C-w>K')
call submode#map('Windows', 'n', '', '<C-Left>', '<C-w>H')
call submode#map('Windows', 'n', '', '<C-Right>', '<C-w>L')

call submode#map('Windows', 'n', '', 't', '<C-w>T')

" New splits
call submode#map('Windows', 'n', '', 'v', '<C-w>v')
call submode#map('Windows', 'n', '', 's', '<C-w>s')

" Mappings for normal mode
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <silent> <C-h> :call SwitchLeft()<CR>
nnoremap <silent> <C-l> :call SwitchRight()<CR>

nnoremap <C-Down> <C-w>j
nnoremap <C-Up> <C-w>k
nnoremap <silent> <C-Left> :call SwitchLeft()<CR>
nnoremap <silent> <C-Right> :call SwitchRight()<CR>
" }}}

" Misc plugins settings {{{
" NERDTree
let NERDTreeIgnore = ['\.pyc$', '^__pycache__$']
let NERDTreeMinimalUI = 1
let NERDTreeDirArrows = 1
let NERDTreeMouseMode = 2

" csv
let g:csv_arrange_align = 'l*'

" git
autocmd BufWritePost * GitGutter
let g:magit_default_fold_level = 0
" let g:blamer_enabled = 1
" let g:blamer_delay = 1000
" let g:blamer_prefix = ' > '


" splitjoin
let g:splitjoin_align = 1
let g:splitjoin_python_brackets_on_separate_lines = 1

" asyncrun
let g:asyncrun_open = 6
let g:asyncrun_rootmarks = ['.nvimrc', '.git']
let g:asynctasks_term_pos = 'tab'

" vimwiki
let g:vimwiki_list = [{'path': '~/MEGAsync/Sync/vimwiki/', 'path_html': '~/MEGAsync/Sync/vimwiki-html/'}]
" }}}

" Filetype-specific settings {{{

" web-dev {{{
augroup filetype_pug
    autocmd!
    autocmd Filetype pug setlocal foldmethod=indent
    autocmd Filetype pug setlocal foldlevel=20
augroup END

augroup filetype_vue
    autocmd!
    autocmd Filetype vue command! -register JsDoc call jsdoc#insert()
    autocmd Filetype vue setlocal conceallevel=2
augroup END

autocmd Filetype javascript setlocal conceallevel=2

autocmd Filetype typescript execute ':CloseTagDisableBuffer'

let g:mta_filetypes = { 'html' : 1, 'xhtml' : 1, 'xml' : 1, 'jinja' : 1, 'xsd': 1, 'vue': 1 }
let g:closetag_filenames = '*.html,*.xhtml,*.vue,*.svelte'
let g:closetag_filetypes = 'html,xhtml,vue,xml,xsd,svelte'
let g:colorizer_auto_filetype='css,html,python,javascript,vue'

let g:jsdoc_allow_input_prompt = 1
let g:jsdoc_enable_es6 = 1
let g:jsdoc_input_description = 1

let g:vue_pre_processors = []
let g:vim_svelte_plugin_use_sass = 1

let g:javascript_plugin_jsdoc = 1

let g:javascript_conceal_function                  = "ƒ"
let g:javascript_conceal_null                      = "ø"
" let g:javascript_conceal_this                      = "#"
let g:javascript_conceal_return                    = "⮜"
let g:javascript_conceal_undefined                 = "¿"
let g:javascript_conceal_NaN                       = "ℕ"
let g:javascript_conceal_prototype                 = "¶"
let g:javascript_conceal_static                    = "•"
let g:javascript_conceal_super                     = "Ω"
let g:javascript_conceal_arrow_function            = "⮞"
let g:javascript_conceal_noarg_arrow_function      = "🞅"
let g:javascript_conceal_underscore_arrow_function = "🞅"

" }}}

" LaTeX {{{
let g:tex_flavor='latex'
let g:vimtex_fold_enabled = 1
let g:vimtex_view_method='zathura'
let g:vimtex_quickfix_mode=0
let g:tex_conceal='abdmgs'
let g:ale_tex_chktex_options='-I --nowarn 32'
" let g:syntastic_tex_lacheck_quiet_messages = { 'regex': ['\Vpossible unwanted space at', '\VUse ` to begin'] }

augroup filetype_tex
    autocmd!
    autocmd Filetype tex setlocal foldlevel=0
    autocmd Filetype tex
            \ let b:AutoPairs = {"(": ")", '[': ']', '{': '}', '$': '$', '\left(': '\right)',
            \ '\left[': 'right]', "''": '``', "``": "''", '"': '"', 'sympy': 'sympy', '<<': '>>'}
    autocmd Filetype tex setlocal conceallevel=2
augroup END

" }}}

" Python {{{
autocmd Filetype python
            \ setlocal foldmethod=indent

" let g:python_host_prog='/usr/bin/python'
" let g:python3_host_prog='/usr/bin/python3'
let g:pymode_python = 'python3'
let g:pymode_lint = 0
let g:pymode_rope = 1
let g:pymode_rope_completion = 0
let g:pymode_rope_autoimport = 0
let g:pymode_doc = 1
" }}}

" REPL {{{
augroup zepl
    autocmd!
    autocmd FileType python     let b:repl_config = { 'cmd': 'ipython --colors=Linux' }
    autocmd FileType javascript let b:repl_config = { 'cmd': 'node' }
augroup END
" }}}

" Misc file commands {{{
augroup filetype_vim
    autocmd!
    autocmd Filetype vim setlocal foldmethod=marker
    autocmd Filetype vim setlocal foldlevel=0
augroup END

augroup filetype_sparql let g:rainbow_active = 0
augroup END

au BufNewFile,BufRead *.frag,*.vert,*.fp,*.vp,*.glsl setf glsl
au BufNewFile,BufRead *.pl setf prolog
au BufNewFile,BufRead *.sparql setf sparql

au TermEnter * IndentGuidesDisable
au TermLeave * IndentGuidesEnable

let g:vim_markdown_conceal = 2
let g:instant_markdown_autostart = 0
let g:instant_markdown_mathjax = 1
let g:instant_markdown_open_to_the_world = 1
let g:instant_markdown_allow_external_content = 1

let g:markdown_composer_autostart = 0
" }}}

" }}}

" Syntax check & autocomplete {{{
" coc {{{
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"
inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Use <c-space> to trigger completion.
inoremap <silent><expr> <c-space> coc#refresh()
function! s:cocActionsOpenFromSelected(type) abort
  execute 'CocCommand actions.open ' . a:type
endfunction
xmap <silent> <leader>p :<C-u>execute 'CocCommand actions.open ' . visualmode()<CR>
nmap <silent> <leader>p :<C-u>set operatorfunc=<SID>cocActionsOpenFromSelected<CR>g@
nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

autocmd CursorHold * silent call CocActionAsync('highlight')

let g:coc_global_extensions =
            \ [
            \ 'coc-actions',
            \ 'coc-calc',
            \ 'coc-css',
            \ 'coc-json',
            \ 'coc-marketplace',
            \ 'coc-python',
            \ 'coc-svelte',
            \ 'coc-tabnine',
            \ 'coc-tasks',
            \ 'coc-tsserver',
            \ 'coc-ultisnips',
            \ 'coc-vetur',
            \ 'coc-vimtex'
            \ ]

" }}}


" Syntastic {{{
" set statusline+=%#warningmsg#
" set statusline+=%{SyntasticStatuslineFlag()}
" set statusline+=%*
"
" let g:syntastic_ignore_files = ['\.py$', '\.tex$']
" let g:syntastic_always_populate_loc_list = 1
" let g:syntastic_auto_loc_list = 1
" let g:syntastic_check_on_open = 1
" let g:syntastic_check_on_wq = 0
" }}}

" ALE {{{
let g:ale_open_list = 'on_save'
let g:ale_list_window_size = 7
let g:ale_close_preview_on_insert = 1
let g:ale_lint_on_text_changed = 'never'
let g:ale_lint_on_enter = 0
let g:ale_lint_on_insert_leave = 0
let g:ale_completion_enabled = 0
let g:ale_linter_aliases = {'svelte': ['css', 'javascript']}
let g:ale_linters = {'python': ['pyls'], 'tex': ['chktex'], 'cpp': ['clang'], 'vue': ['eslint'], 'typescript': ['tsserver', 'tslint'], 'svelte': ['eslint']}
let g:ale_fixers = {
            \    'python': ['yapf', 'isort', 'remove_trailing_lines', 'trim_whitespace'],
            \    'tex': ['latexindent', 'textlint', 'remove_trailing_lines', 'trim_whitespace'],
            \    'js': ['prettier', 'eslint'],
            \    'javascript': ['prettier', 'eslint'],
            \    'typescript': ['prettier', 'tslint'],
            \    'jsx': ['prettier', 'eslint'],
            \    'vue': ['prettier', 'eslint'],
            \    'cpp': ['clang-format', 'remove_trailing_lines', 'trim_whitespace'],
            \    'json': ['prettier'],
            \    'svelte': ['eslint']
            \}
let g:airline#extensions#ale#enabled = 1
" }}}

" Deoplete {{{
" call deoplete#custom#var('omni', 'input_patterns', {
"             \   'tex': g:vimtex#re#deoplete
"             \})
"
" let g:deoplete#enable_at_startup = 1
" call deoplete#custom#source('ale', 'rank', 999)
"
" inoremap <silent><expr> <TAB>
"             \ pumvisible() ? "\<C-n>" :
"             \ <SID>check_back_space() ? "\<TAB>" :
"             \ deoplete#mappings#manual_complete()
" function! s:check_back_space() abort
"     let col = col('.') - 1
"     return !col || getline('.')[col - 1]  =~ '\s'
" endfunction
"
" let g:deoplete#sources#clang#libclang_path = '/usr/lib/llvm-6.0/lib/libclang.so.1'
" let g:deoplete#sources#clang#clang_header = '/usr/lib/llvm-6.0/lib/clang/6.0.0/include'
" }}}

" Auto-pairs
let g:AutoPairsFlyMode = 0
let g:AutoPairsShortcutBackInsert = '<a-w>'

" }}}

" My scripts {{{
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

" Terminal in tab {{{
function! OpenTerminalInTab()
    execute ':tabnew'
    execute ':terminal'
    normal i
endfunction

nnoremap ~ :call OpenTerminalInTab()<CR>
" }}}

" Diff with saved {{{
function! s:DiffWithSaved()
  let filetype=&ft
  diffthis
  vnew | r # | normal! 1Gdd
  diffthis
  exe "setlocal bt=nofile bh=wipe nobl noswf ro ft=" . filetype
endfunction

com! DiffSaved call s:DiffWithSaved()
" }}}

" Git diff stats {{{
function! Gstats()
    execute '!git diff --stat'
endfunction

command! Gstats call Gstats()

" }}}

" Uptime {{{
let s:start_time = localtime()

function! UptimeSeconds()
    let l:current_time = localtime()
    let l:uptime = l:current_time - s:start_time
    return l:uptime
endfunction

function! s:AddLeadingZero(i)
    if a:i == 0
        return "00"
    elseif a:i < 10
        return "0" . a:i
    else
        return "" . a:i
    endif
endfu

function! Uptime(...)
    let l:show_seconds = get(a:, 1, 0)
    let l:uptime = UptimeSeconds()
    let l:m_s = (l:uptime) % 60
    let l:m_m = (l:uptime/ 60) % 60
    let l:m_h = (l:uptime/ (60 * 60)) % 24
    let l:m_d = (l:uptime/ (60 * 60 * 24))

    let l:msg = ""
    if (l:m_d > 0)
        let l:msg = l:msg . l:m_d . "d "
    endif
    let l:msg = l:msg . s:AddLeadingZero(l:m_h) . ":" .
                \       s:AddLeadingZero(l:m_m)
    if l:show_seconds == 1
        let l:msg = l:msg . ":" . s:AddLeadingZero(l:m_s)
    endif
    return l:msg
endfunction

command! Uptime echo Uptime(1)

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

" {{{ Fugitive conflict resolution

function! GitVMergeSplit()
    execute 'Gdiffsplit!'
    wincmd k
    wincmd H
    let s:leftMergeBuffer = bufnr('%')
    wincmd l
    wincmd j
    wincmd L
    let s:rightMergeBuffer = bufnr('%')
    wincmd h
endfunction

function! GitMergeGetLeft()
    execute ":diffget" . s:leftMergeBuffer
endfunction

function! GitMergeGetRight()
    execute ":diffget" . s:rightMergeBuffer
endfunction

command! Gdiffmerge call GitVMergeSplit()
nnoremap gdh :call GitMergeGetLeft()<CR>
nnoremap gdl :call GitMergeGetRight()<CR>
nnoremap gd<Left> :call GitMergeGetLeft()<CR>
nnoremap gd<Right> :call GitMergeGetRight()<CR>

" }}}

" {{{ Folding

function! SetFoldLevel() range
    execute ":set foldlevel=" . v:count
    echo("set foldlevel=" . v:count)
endfunction

function! OnSpace()
    if foldlevel('.')
        if foldclosed('.') != -1
            return 'zO'
        else
            return 'za'
        endif
    else
        return "\<Space>"
    endif
endfunction

nnoremap <Leader>ff :call SetFoldLevel()<CR>
nnoremap <silent> <Space> @=(OnSpace())<CR>

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

" {{{ Deoplete and multiple cursors
" function! Multiple_cursors_before()
"     if deoplete#is_enabled()
"         call deoplete#disable()
"         let g:deoplete_is_enable_before_multi_cursors = 1
"     else
"         let g:deoplete_is_enable_before_multi_cursors = 0
"     endif
" endfunc
" function! Multiple_cursors_after()
"     if g:deoplete_is_enable_before_multi_cursors
"         call deoplete#enable()
"     endif
" endfunction
" }}}

" {{{ Switch to project root
let g:markers = split('.git') " TODO nvimrc
function! CdToRepoRoot() abort
    for marker in g:markers
        let root = finddir(marker, expand('%:p:h') . ';')
        if !empty(root)
            let root = fnamemodify(root, ':h')
            execute 'cd ' . root
            echo 'cd ' . root . ' (found ' . marker . ')'
            return
        endif
    endfor
    echoerr 'No repo root found.'
endfunction

nnoremap cr :call CdToRepoRoot()<CR>
" }}}

" Put current time
command! Timestamp :put =strftime('%d-%m-%y %H:%M:%S')
" }}}

" UI settings {{{
set background=dark
" let g:solarized_visibility='high'
" tmux cursor
if exists('+termguicolors')
  let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
  let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
  let &t_SI="\<Esc>[2 q"
  set termguicolors
endif
"set guifont=DroidSansMono\ Nerd\ Font\ 11
augroup qs_colors
  autocmd!
  autocmd ColorScheme * highlight QuickScopePrimary guifg='#ff0000' gui=underline ctermfg=155 cterm=underline
  autocmd ColorScheme * highlight QuickScopeSecondary guifg='#ffff00' gui=underline ctermfg=81 cterm=underline
augroup END

set laststatus=2
colorscheme palenight

highlight! TermCursorNC guibg=red
"highlight Conceal guibg=Normal guifg=Normal
highlight! link Conceal Normal
highlight! illuminatedWord cterm=underline gui=underline

" Airline
let g:airline_theme='palenight'
let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#buffer_idx_mode = 1
let g:airline#extensions#tabline#formatter = 'unique_tail'
let g:airline_section_b = '%{Uptime()}'

" vim-clap
"let g:clap_search_box_border_symbols = {'curve': ["f", "g"], 'nil': ['', ''] }
let g:clap_search_box_border_style = 'nul'
let g:clap_layout = { 'relative': 'editor' }

" Gundo
let g:gundo_preview_height = 15

" tmux line
let g:tmuxline_preset = {
      \'a'    : '#S',
      \'b'    : '#W',
      \'win'  : '#I #W #[align=left]',
      \'cwin' : '#I #W',
      \'x'    : '%-H:%M',
      \'y'    : '%a, %b %d',
      \'z'    : '#H'}

" Indent guides
let g:indent_guides_enable_on_vim_startup = 1
let g:indent_guides_exclude_filetypes = ['help', 'nerdtree', 'tagbar', 'startify', 'vim-plug', 'clap_input', 'codi']
let g:indent_guides_guide_size = 1

" Autoresize
let g:lens#disabled = 1
let g:lens#disabled_filetypes = ['nerdtree', 'tagbar', 'clap_input', 'startify', 'diff', 'gundo']
let g:lens#width_resize_min = 10
let g:lens#height_resize_min = 10
let g:lens#width_resize_max = 90

" Highligh whitespace
let g:extra_whitespace_ignored_filetypes = ['help', 'nerdtree', 'tagbar', 'startify', 'vim-plug', 'clap_input', 'GV']

" Brackets
let g:rainbow_active = 1
let g:rainbow_conf = {
            \   'guifgs': ['red', 'yellow', 'lightgreen', 'lightblue'],
            \   'separately': { 'nerdtree': 0, 'vimwiki': 0, 'sparql': 0 }
            \ }

" Tagbar
let g:tagbar_sort = 0
let g:tagbar_show_line_numbers = 1
let g:tagbar_width = 60
let g:tagbar_autofocus = 1

let g:vista_sidebar_width = 60

let g:startify_custom_header = [
    \ ' ―――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――',
    \ '     _____            __  __  ____                  ____            ',
    \ '    / ___/____ ______/ /_/  |/  (_)___  __  _______/ __ \____  ___  ',
    \ '    \__ \/ __ `/ ___/ __/ /|_/ / / __ \/ / / / ___/ / / / __ \/ _ \ ',
    \ '   ___/ / /_/ / /  / /_/ /  / / / / / / /_/ (__  ) /_/ / / / /  __/ ',
    \ '  /____/\__, /_/   \__/_/  /_/_/_/ /_/\__,_/____/\____/_/ /_/\___/  ',
    \ '          /_/                                                       ',
    \ ' ―――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――',
    \ '                   Conquering the darkest places                    ',
    \ ]

" }}}
