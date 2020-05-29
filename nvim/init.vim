"                                        ▟▙            
"                                        ▝▘            
"██▃▅▇█▆▖  ▗▟████▙▖   ▄████▄   ██▄  ▄██  ██  ▗▟█▆▄▄▆█▙▖
"██▛▔ ▝██  ██▄▄▄▄██  ██▛▔▔▜██  ▝██  ██▘  ██  ██▛▜██▛▜██
"██    ██  ██▀▀▀▀▀▘  ██▖  ▗██   ▜█▙▟█▛   ██  ██  ██  ██
"██    ██  ▜█▙▄▄▄▟▊  ▀██▙▟██▀   ▝████▘   ██  ██  ██  ██
"▀▀    ▀▀   ▝▀▀▀▀▀     ▀▀▀▀       ▀▀     ▀▀  ▀▀  ▀▀  ▀▀

call plug#begin('~/.local/share/nvim/plugged')
Plug 'neoclide/coc.nvim', {'branch': 'release'} " coc.nvim
Plug 'neovimhaskell/haskell-vim'                " haskell-vim
call plug#end()

"split panes below/right the old one
set splitbelow splitright hidden mouse=a nu

set termguicolors
colo default
set background=light
set signcolumn = "auto:1"

set ignorecase smartcase incsearch
set shortmess+=rwIcs
set laststatus=1

hi NonText guifg=White
hi LineNr guifg=Gray
hi VertSplit gui=NONE
hi SignColmn gui=NONE guifg=black guibg=white
hi StatusLine gui=NONE cterm=NONE guifg=Gray
hi StatusLineNC gui=NONE cterm=NONE guifg=Gray

let g:haskell_indent_guard = 2
let g:haskell_indent_after_bare_where = 2
let g:haskell_indent_before_where = 2

noremap <leader><leader> "+
noremap <leader>yg :%y+<CR>
nnoremap <silent><Esc> :noh<CR>
tnoremap <silent><C-q> <C-\><C-n>

command! Evimrc edit $MYVIMRC

au TermOpen * setlocal nonumber norelativenumber signcolumn=no

"bindigs and settings for coc

" Having longer updatetime (default is 4000 ms = 4 s) leads to noticeable
" delays and poor user experience.
set updatetime=300

" Always show the signcolumn, otherwise it would shift the text each time
" diagnostics appear/become resolved.
" set signcolumn=yes

" Use tab for trigger completion with characters ahead and navigate.
" NOTE: Use command ':verbose imap <tab>' to make sure tab is not mapped by
" other plugin before putting this into your config.
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Use <c-space> to trigger completion.
inoremap <silent><expr> <c-space> coc#refresh()

" Use `[g` and `]g` to navigate diagnostics
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)

" Use K to show documentation in preview window.
nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

" Highlight the symbol and its references when holding the cursor.
autocmd CursorHold * silent call CocActionAsync('highlight')

" Symbol renaming.
nmap <leader>rn <Plug>(coc-rename)

inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"

set expandtab  shiftwidth=4 softtabstop=-1
