" Vundle
set nocompatible
filetype off     " disable file type detection
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

  Bundle 'gmarik/vundle'
  Bundle "Shougo/unite.vim"
  Bundle "tpope/vim-rails"
  Bundle "php.vim"
  Bundle 'JavaScript-syntax'
  Bundle 'ack.vim'
  Bundle 'glidenote/memolist.vim'
  Bundle 'thinca/vim-quickrun'
  Bundle 'rodjek/vim-puppet'
  Bundle 'vim-scripts/vim-auto-save'
  Bundle 'scrooloose/nerdtree'
  Bundle 'itchyny/lightline.vim'
  Bundle 'kchmck/vim-coffee-script'

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


" 文字コード, 改行コード {{{
set encoding=utf-8
set fileencodings=ucs_bom,utf8,euc-jp
set fileformats=unix,dos,mac

" from ずんWiki http://www.kawaz.jp/pukiwiki/?vim#content_1_7
" 文字コードの自動認識
if &encoding !=# 'utf-8'
  set encoding=japan
  set fileencoding=japan
endif
if has('iconv')
  let s:enc_euc = 'euc-jp'
  let s:enc_jis = 'iso-2022-jp'
  " iconvがeucJP-msに対応しているかをチェック
  if iconv("\x87\x64\x87\x6a", 'cp932', 'eucjp-ms') ==# "\xad\xc5\xad\xcb"
    let s:enc_euc = 'eucjp-ms'
    let s:enc_jis = 'iso-2022-jp-3'
  " iconvがJISX0213に対応しているかをチェック
  elseif iconv("\x87\x64\x87\x6a", 'cp932', 'euc-jisx0213') ==# "\xad\xc5\xad\xcb"
    let s:enc_euc = 'euc-jisx0213'
    let s:enc_jis = 'iso-2022-jp-3'
  endif
  " fileencodingsを構築
  if &encoding ==# 'utf-8'
    let s:fileencodings_default = &fileencodings
    let &fileencodings = s:enc_jis .','. s:enc_euc .',cp932'
    let &fileencodings = s:fileencodings_default .','. &fileencodings
    unlet s:fileencodings_default
  else
    let &fileencodings = &fileencodings .','. s:enc_jis
    set fileencodings+=utf-8,ucs-2le,ucs-2
    if &encoding =~# '^\(euc-jp\|euc-jisx0213\|eucjp-ms\)$'
      set fileencodings+=cp932
      set fileencodings-=euc-jp
      set fileencodings-=euc-jisx0213
      set fileencodings-=eucjp-ms
      let &encoding = s:enc_euc
      let &fileencoding = s:enc_euc
    else
      let &fileencodings = &fileencodings .','. s:enc_euc
    endif
  endif
  " 定数を処分
  unlet s:enc_euc
  unlet s:enc_jis
endif
" }}}


""" 編集、文書整形関連
"
" backspaceキーの挙動を設定する
" indent        : 行頭の空白の削除を許す
" eol           : 改行の削除を許す
" start         : 挿入モードの開始位置での削除を許す
set backspace=indent,eol,start

" 新しい行を直前の行と同じインデントにする
set autoindent

" Tab文字を画面上の見た目で何文字幅にするか設定
set tabstop=4

" cindentやautoindent時に挿入されるタブの幅
set shiftwidth=4

" Tabキー使用時にTabでは無くホワイトスペースを入れたい時に使用する
" この値が0以外の時はtabstopの設定が無効になる
set softtabstop=0

" タブの入力を空白文字に置き換える
set expandtab

" Indent
autocmd FileType apache     setlocal sw=4 sts=4 ts=4 et
autocmd FileType aspvbs     setlocal sw=4 sts=4 ts=4 noet
autocmd FileType c          setlocal sw=4 sts=4 ts=4 et
autocmd FileType cpp        setlocal sw=4 sts=4 ts=4 et
autocmd FileType cs         setlocal sw=4 sts=4 ts=4 et
autocmd FileType css        setlocal sw=4 sts=4 ts=4 noet
autocmd FileType diff       setlocal sw=4 sts=4 ts=4 noet
autocmd FileType eruby      setlocal sw=4 sts=4 ts=4 noet
autocmd FileType html       setlocal sw=2 sts=2 ts=2 noet
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

" デフォルトで有効にする
let g:auto_save = 1

""" Map
" File encoding
let mapleader = ' f'
  " Encoding
  nnoremap <Leader>u  :set fileencoding=utf-8       <CR>
  nnoremap <Leader>e  :set fileencoding=euc-jp      <CR>

" Memolist
let mapleader = ' m'
  nnoremap <Leader>n  :MemoNew<CR>
  nnoremap <Leader>l  :MemoList<CR>
  nnoremap <Leader>g  :MemoGrep<CR>

" Unite
let mapleader = ' u'
  nnoremap <Leader>b :Unite buffer<CR>

" NERDTree
let mapleader = ' n'
  nnoremap <Leader>t :NERDTree<CR>

" 検索のハイライトを消す
  nnoremap  <C-c><C-c> :<C-u>nohlsearch<cr><Esc>

" for PHP
  inoremap <C-z><C-d> Zend_Debug::dump(); exit;
  inoremap <C-v><C-d> var_dump(); exit;
  inoremap <C-a> ->
  inoremap <C-a><C-a> =>
