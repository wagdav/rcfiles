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
Plug 'eraserhd/parinfer-rust', {'do': 'nix shell nixpkgs#cargo nixpkgs#gcc --command build --release'}
Plug 'LnL7/vim-nix'
Plug 'ntpeters/vim-better-whitespace'
Plug 'Olical/conjure'
Plug 'nvim-treesitter/nvim-treesitter'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'vim-airline/vim-airline'
" Clojure
Plug 'tpope/vim-dispatch'
Plug 'clojure-vim/vim-jack-in'
Plug 'radenling/vim-dispatch-neovim'
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

" conjure
let g:conjure#log#hud#enabled = v:false

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

"code
set tags=./tags;/

" Viewer for key combination 'gx'
let g:netrw_browsex_viewer="xdg-open"

let maplocalleader = ","

" map <Esc> to exit terminal-mode:
:tnoremap <Esc> <C-\><C-n>
