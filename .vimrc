" Vundle
set nocompatible " we're running Vim, not Vi!
filetype off     " disable file type detection

set rtp+=~/.vim/vundle/
call vundle#rc()

  Bundle 'gmarik/vundle'
  Bundle "Shougo/unite.vim"
  Bundle "rails.vim"
  Bundle "php.vim"
  Bundle 'JavaScript-syntax'
  Bundle 'ack.vim'
  Bundle 'Shougo/vimfiler'
  Bundle 'glidenote/memolist.vim'
  Bundle 'Lokaltog/vim-powerline'
  Bundle 'thinca/vim-quickrun'
  Bundle 'altercation/vim-colors-solarized'
  Bundle 'rodjek/vim-puppet'

syntax on

" ハイライト
if &t_Co > 2 || has("gui_running")
  " シンタックスハイライトを有効にする
  syntax on
  " 検索結果文字列のハイライトを有効にする
  set hlsearch
endif

" コマンド、検索パターンを50個まで履歴に残す
set history=50

filetype on        " enable filetype detection
filetype indent on " enable filetype-specific indenting
filetype plugin on " enable filetype-specific plugins


" set number       " show line numbers
set ruler        " show the cursor position all the time
set nolist       " don't display tabs or line breaks
set nowrap       " no wrap long line
set cmdheight=1  " number of screen lines to use for the command-line
set showcmd      " Show (partial) command in the last line of the screen
set showmatch    " Breifly show matching brace/parenthese/bracket
set scrolljump=5 " Minimal number of lines to scroll when the cursor gets off the screen
set scrolloff=3  " scroll before the border
set laststatus=2 " Set the window to display a status line
set t_Co=256     " 256 colors
set visualbell   " no beep



" Indent
autocmd FileType apache     setlocal sw=4 sts=4 ts=4 et
autocmd FileType aspvbs     setlocal sw=4 sts=4 ts=4 noet
autocmd FileType c          setlocal sw=4 sts=4 ts=4 et
autocmd FileType cpp        setlocal sw=4 sts=4 ts=4 et
autocmd FileType cs         setlocal sw=4 sts=4 ts=4 et
autocmd FileType css        setlocal sw=4 sts=4 ts=4 noet
autocmd FileType diff       setlocal sw=4 sts=4 ts=4 noet
autocmd FileType eruby      setlocal sw=4 sts=4 ts=4 noet
autocmd FileType html       setlocal sw=4 sts=4 ts=4 noet
autocmd FileType java       setlocal sw=4 sts=4 ts=4 et
autocmd FileType javascript setlocal sw=4 sts=4 ts=4 noet
autocmd FileType perl       setlocal sw=4 sts=4 ts=4 et
autocmd FileType php        setlocal sw=4 sts=4 ts=4 et
autocmd FileType python     setlocal sw=4 sts=4 ts=4 et
autocmd FileType ruby       setlocal sw=2 sts=2 ts=2 et
autocmd FileType haml       setlocal sw=2 sts=2 ts=2 et
autocmd FileType eruby      setlocal sw=2 sts=2 ts=2 et
autocmd FileType sh         setlocal sw=4 sts=4 ts=4 et
autocmd FileType sql        setlocal sw=4 sts=4 ts=4 et
autocmd FileType vb         setlocal sw=4 sts=4 ts=4 noet
autocmd FileType vim        setlocal sw=2 sts=2 ts=2 et
autocmd FileType wsh        setlocal sw=4 sts=4 ts=4 et
autocmd FileType xhtml      setlocal sw=4 sts=4 ts=4 noet
autocmd FileType xml        setlocal sw=4 sts=4 ts=4 noet
autocmd FileType yaml       setlocal sw=2 sts=2 ts=2 et
autocmd FileType zsh        setlocal sw=4 sts=4 ts=4 et
autocmd FileType scala      setlocal sw=2 sts=2 ts=2 et
autocmd FileType coffee     setlocal sw=2 sts=2 ts=2 et


set nobackup   " do not keep backup files
set noswapfile " do not write annoying intermediate swap files,
set nowritebackup


:set encoding=utf-8
" :set fileencodings=utf-8
:set fileencodings=euc-jp,sjis



""" 編集、文書整形関連
"
" backspaceキーの挙動を設定する
" indent        : 行頭の空白の削除を許す
" eol           : 改行の削除を許す
" start         : 挿入モードの開始位置での削除を許す
set backspace=indent,eol,start
" " 新しい行を直前の行と同じインデントにする
set autoindent
" " Tab文字を画面上の見た目で何文字幅にするか設定
set tabstop=4
" " cindentやautoindent時に挿入されるタブの幅
set shiftwidth=4
" " Tabキー使用時にTabでは無くホワイトスペースを入れたい時に使用する
" " この値が0以外の時はtabstopの設定が無効になる
set softtabstop=0
" " タブの入力を空白文字に置き換える
set expandtab

" Encoding {{{
  let mapleader = ' k'
  nnoremap <Leader>u  :set fileencoding=utf-8       <CR>
  nnoremap <Leader>6  :set fileencoding=ucs-2le     <CR>
  nnoremap <Leader>e  :set fileencoding=euc-jp      <CR>
  nnoremap <Leader>s  :set fileencoding=cp932       <CR>
  nnoremap <Leader>j  :set fileencoding=iso-2022-jp <CR>
  nnoremap <Leader>n  :set fileformat=unix          <CR>
  nnoremap <Leader>r  :set fileformat=mac           <CR>
  nnoremap <Leader>rn :set fileformat=dos           <CR>
  let mapleader = ' r'
  nnoremap <Leader>u  :e ++enc=utf-8       <CR>
  nnoremap <Leader>6  :e ++enc=ucs-2le     <CR>
  nnoremap <Leader>e  :e ++enc=euc-jp      <CR>
  nnoremap <Leader>s  :e ++enc=cp932       <CR>
  nnoremap <Leader>j  :e ++enc=iso-2022-jp <CR>
  nnoremap <Leader>n  :e ++fileformat=unix <CR>
  nnoremap <Leader>r  :e ++fileformat=mac  <CR>
  nnoremap <Leader>rn :e ++fileformat=dos  <CR>
" }}}

map <Leader>mn  :MemoNew<CR>
map <Leader>ml  :MemoList<CR>
map <Leader>mg  :MemoGrep<CR>
