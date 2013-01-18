" Vundle
set nocompatible " we're running Vim, not Vi!
filetype off     " disable file type detection

set rtp+=~/.vim/vundle/
call vundle#rc()

  Bundle "Shougo/unite.vim"
  Bundle "rails.vim"
  Bundle "php.vim"
  Bundle 'JavaScript-syntax'

syntax on
filetype on        " enable filetype detection
filetype indent on " enable filetype-specific indenting
filetype plugin on " enable filetype-specific plugins


set number       " show line numbers
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
:set fileencodings=euc-jp,sjis
