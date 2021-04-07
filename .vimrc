set nocompatible

" Install vim-plug
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

"Plugins
call plug#begin("~/.vim/bundle")
Plug 'ctrlpvim/ctrlp.vim'
Plug 'editorconfig/editorconfig'
Plug 'kana/vim-altr'
Plug 'kana/vim-operator-user'
Plug 'LnL7/vim-nix'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'ntpeters/vim-better-whitespace'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'vim-airline/vim-airline'
Plug 'vmchale/dhall-vim'
call plug#end()


"Plugin configuration

" Airline
set laststatus=2

" Altr
command! A call altr#forward()

" Ctrlp
" ignore files in .gitignore
let g:ctrlp_user_command = ['.git', 'cd %s && git ls-files -co --exclude-standard']
let g:ctrlp_root_markers = ['.ctrlp']

" Fugitive status line
set statusline=%<%f\ %h%m%r%{fugitive#statusline()}%=%-14.(%l,%c%V%)\ %P

" vim-lsp
autocmd FileType python,go,rust nmap gd <plug>(lsp-definition)

"Other customizations
filetype plugin indent on
set tw=0
set ts=4
set sts=4
set shiftwidth=4
set expandtab
set linebreak

set spellsuggest=5

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

"command to open my action list
command! Gtd call SwitchToList()
function! SwitchToList()
    let number = bufnr("Dropbox/Apps/Notes for Android/")
    if number == -1
        edit $HOME/Dropbox/Apps/Notes\ for\ Android
    else
        exe 'buffer' number
    endif
endfunction

"code
set tags=./tags;/

" Viewer for key combination 'gx'
let g:netrw_browsex_viewer="xdg-open"

" Close buffer but not split window ',d'
" https://stackoverflow.com/a/19619038/513809
nmap ,d :b#<bar>bd#<CR>

nmap <Leader>g :Ggrep <C-R>=expand("<cword>")<CR>
