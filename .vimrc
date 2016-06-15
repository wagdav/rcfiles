set tw=0
set ts=4
set sts=4
set shiftwidth=4
set expandtab
set linebreak

set spellsuggest=5
set colorcolumn=80

filetype plugin indent on
syntax on

"Remap \zz to toggle the value of 'scrolloff' between 0 and 999
:nnoremap <Leader>zz :let &scrolloff=999-&scrolloff<CR>

"Insert date
nnoremap <F12> "=strftime("%Y-%m-%d")<CR>P
inoremap <F12> <C-R>=strftime("%Y-%m-%d")<CR>

"Move between logical lines
nmap j gj
nmap k gk

"Search options
set incsearch
set ignorecase
set smartcase

" LatexSuite options
" ==================
let g:Tex_UseMakefile='1'
let g:Tex_ViewRule_pdf = 'viewer'
let g:Tex_ViewRule_dvi = 'okular'
let g:Tex_DefaultTargetFormat = 'pdf'

let g:Tex_CompileRule_dvi = 'latex -interaction=nonstopmode -src-specials $*'

" Set the warning messages to ignore.
let g:Tex_IgnoredWarnings =
\"Underfull\n".
\"Overfull\n".
\"specifier changed to\n".
\"You have requested\n".
\"Missing number, treated as zero.\n".
\"There were undefined references\n".
\"Citation %.%# undefined\n".
\'LaTeX Font Warning:'"
" This number N says that latex-suite should ignore the first N of the
" above.
let g:Tex_IgnoreLevel = 8


"new environments for beamer presentation
let g:Tex_Env_columns =
    \"\\begin{columns}\<CR>\\column{.5\\textwidth}\<CR>".
    \"\\column{.5\\textwidth}\<CR>".
    \"\\end{columns}<++>"

let g:Tex_Env_block =
    \"\\begin{block}{<+block title+>}\<CR>".
    \"<++>\<CR>".
    \"\\end{block}<++>"

let g:Tex_Env_frame =
    \"\\begin{frame}\<CR>".
    \"\\frametitle{<+frame title+>}\<CR>".
    \"<++>\<CR>".
    \"\\end{frame}<++>"


"command to open my action list
command! Gtd call SwitchToList()
function! SwitchToList()
    let number = bufnr("Dropbox/txt/lists.otl")
    if number == -1
        edit $HOME/Dropbox/txt/lists.otl
    else
        exe 'buffer' number
    endif
endfunction

"code
set tags=./tags;/

let g:ctrlp_custom_ignore = {
    \ 'dir': 'build\|env',
    \ }

" Fugitive status line
set statusline=%<%f\ %h%m%r%{fugitive#statusline()}%=%-14.(%l,%c%V%)\ %P
