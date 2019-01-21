" Vundle
set nocompatible
filetype off     " disable file type detection

call plug#begin('~/.vim/plugged')

  Plug 'Shougo/unite.vim'
  Plug 'Shougo/vimproc'
  Plug 'tpope/vim-rails'
  Plug 'php.vim'
  Plug 'JavaScript-syntax'
  Plug 'ack.vim'
  Plug 'glidenote/memolist.vim'
  Plug 'thinca/vim-quickrun'
  Plug 'rodjek/vim-puppet'
  Plug 'vim-scripts/vim-auto-save'
  Plug 'scrooloose/nerdtree'
  Plug 'itchyny/lightline.vim'
  Plug 'kchmck/vim-coffee-script'
  Plug 'tyru/open-browser.vim'
  Plug 'hail2u/vim-css3-syntax'
  Plug 'leafgarland/typescript-vim'
  Plug 'tpope/vim-pathogen'
  Plug 'scrooloose/syntastic'
  Plug 'tpope/vim-endwise'
  Plug 'Shougo/neomru.vim'
  Plug 'bronson/vim-trailing-whitespace'
  Plug 'pangloss/vim-javascript'
  Plug 'mxw/vim-jsx'
  Plug 'keith/swift.vim'
  Plug 'elixir-lang/vim-elixir'
  Plug 'slim-template/vim-slim'

call plug#end()

syntax on
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
set noundofile

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
set shiftwidth=2

" Tabキー使用時にTabでは無くホワイトスペースを入れたい時に使用する
" この値が0以外の時はtabstopの設定が無効になる
set softtabstop=0

" タブの入力を空白文字に置き換える
set expandtab

set nobackup   " do not keep backup files
set noswapfile " do not write annoying intermediate swap files,
set nowritebackup

" デフォルトで有効
let g:auto_save = 1

" do not save while in insert mode
let g:auto_save_in_insert_mode = 0

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
""""""""""""""""""""""""""""""""""""
" insert modeで開始
" let g:unite_enable_start_insert = 1

" 大文字小文字を区別しない
let g:unite_enable_ignore_case = 1
let g:unite_enable_smart_case = 1

" バッファ一覧
noremap <C-P> :Unite buffer<CR>
" ファイル一覧
noremap <C-N> :Unite -buffer-name=file file<CR>
" 最近使ったファイルの一覧
noremap <C-Z> :Unite file_mru<CR>
" sourcesを「今開いているファイルのディレクトリ」とする
noremap :uff :<C-u>UniteWithBufferDir file -buffer-name=file<CR>

" ウィンドウを分割して開く
au FileType unite nnoremap <silent> <buffer> <expr> <C-J> unite#do_action('split')
au FileType unite inoremap <silent> <buffer> <expr> <C-J> unite#do_action('split')

" ウィンドウを縦に分割して開く
au FileType unite nnoremap <silent> <buffer> <expr> <C-K> unite#do_action('vsplit')
au FileType unite inoremap <silent> <buffer> <expr> <C-K> unite#do_action('vsplit')

" ESCキーを2回押すと終了する
au FileType unite nnoremap <silent> <buffer> <ESC><ESC> :q<CR>
au FileType unite inoremap <silent> <buffer> <ESC><ESC> <ESC>:q<CR>

" grep検索
nnoremap <silent> ug  :<C-u>Unite grep:. -buffer-name=search-buffer<CR>

" カーソル位置の単語をgrep検索
nnoremap <silent> ucg :<C-u>Unite grep:. -buffer-name=search-buffer<CR><C-R><C-W>

" grep検索結果の再呼出
nnoremap <silent> ur  :<C-u>UniteResume search-buffer<CR>

" unite grep に ag(The Silver Searcher) を使う
if executable('ag')
  let g:unite_source_grep_command = 'ag'
  let g:unite_source_grep_default_opts = '--nogroup --nocolor --column'
  let g:unite_source_grep_recursive_opt = ''
endif


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

" open-browser.vim
" http://vim-users.jp/2011/08/hack225/

let g:netrw_nogx = 1 " disable netrw's gx mapping.
  nmap gx <Plug>(openbrowser-smart-search)
  vmap gx <Plug>(openbrowser-smart-search)

execute pathogen#infect()

exe "set rtp+=" . globpath(substitute($GOPATH, (has('win32')||has('win64'))?';':':',',','g'), 'src/github.com/peco/migemogrep/misc/vim')
