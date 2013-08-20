set tw=0
set ts=4
set sts=4
set shiftwidth=4
set expandtab
set linebreak

set spellsuggest=5

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


" Do make with different makeprg settings.
" Error lists from each makeprg are combined into one quickfix list.
" ==================
command! Pycheck call DoMake('pyflakes', 'pep8')
function! DoMake(...)
  update  " save any changes because makeprg checks the file on disk
  let savemp = &makeprg
  let qflist = []
  for prg in a:000
    let &makeprg = prg . ' %'
    silent make!
    let qflist += getqflist()
  endfor
  if empty(qflist)
    cclose
  else
    call setqflist(qflist)
    copen
    cfirst
  endif
  let &makeprg = savemp
endfunction

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

"mark item done
nmap <leader>md ^s<C-V>u2713<ESC>
"mark item neutral
nmap <leader>mn ^s-<ESC>
"mark item cancelled
nmap <leader>mc ^s<C-V>u2717<ESC>

"actions for navigation in code
nnoremap ,cd :cd %:p:h<CR>:pwd<CR>
"nnoremap <F2> :vimgrep /\<<C-R><C-W>\>/ **/*.%:e
nnoremap <F2> :grep -r --include='*.<C-R>=expand('%:e')<CR>' '<C-R><C-W>' ./<CR><CR>:cw<CR>
