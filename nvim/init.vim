"                                        ▟▙            
"                                        ▝▘            
"██▃▅▇█▆▖  ▗▟████▙▖   ▄████▄   ██▄  ▄██  ██  ▗▟█▆▄▄▆█▙▖
"██▛▔ ▝██  ██▄▄▄▄██  ██▛▔▔▜██  ▝██  ██▘  ██  ██▛▜██▛▜██
"██    ██  ██▀▀▀▀▀▘  ██▖  ▗██   ▜█▙▟█▛   ██  ██  ██  ██
"██    ██  ▜█▙▄▄▄▟▊  ▀██▙▟██▀   ▝████▘   ██  ██  ██  ██
"▀▀    ▀▀   ▝▀▀▀▀▀     ▀▀▀▀       ▀▀     ▀▀  ▀▀  ▀▀  ▀▀

call plug#begin('~/.local/share/nvim/plugged')
Plug 'chrisbra/Colorizer'                       " highlight colors
Plug 'tpope/vim-fugitive'                       " vim plugin for git
Plug 'chriskempson/base16-vim'                  " base16 themes
Plug 'vim-airline/vim-airline'                  " airline
Plug 'vim-airline/vim-airline-themes'           " airline themes
Plug 'neoclide/coc.nvim', {'branch': 'release'} " coc.nvim
Plug 'neovimhaskell/haskell-vim'                " haskell-vim
"Plug 'sdiehl/vim-ormolu'                        " haskell-vim
"Plug 'nbouscal/vim-stylish-haskell'             " stylish haskell
Plug 'godlygeek/tabular'                        " tabular (aligns text)

call plug#end()

"split panes below/right the old one
"enable hiding unsaved buffers
"enable mouse
"enable line numbers
set splitbelow splitright hidden mouse=a nu

set termguicolors
"colo base16-atelier-sulphurpool-light
colo default
set background=light
let g:airline_theme='xtermlight'
set signcolumn = "auto:1"

hi VertSplit gui=NONE
hi SignColmn gui=NONE guifg=black guibg=white

set list          " Display unprintable characters f12 - switches
set listchars=tab:•\ ,trail:•,extends:»,precedes:« " Unprintable chars mapping
noremap <silent><leader>l :set list!<CR>

let g:haskell_indent_guard = 4
let g:haskell_indent_after_bare_where = 2
let g:haskell_indent_before_where = 2

let g:colorizer_x11_names = 1
"highlight rgb colors
" black cyan red yellow 
" #123123 #f0f800

noremap <leader><leader> "+
noremap <leader>yg :%y+<CR>
nnoremap <silent><Esc> :noh<CR>
tnoremap <silent><C-q> <C-\><C-n>
command! Evimrc edit $MYVIMRC

set ignorecase smartcase incsearch
"shortmess = filnxtToOF
set shortmess+=rwIcs
set noshowmode
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
