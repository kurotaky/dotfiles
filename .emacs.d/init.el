;;-*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; スタートアップ時のメッセージを抑制
(setq inhibit-startup-message t)

;; エンコーディングは基本的にUTF-8
(set-language-environment "Japanese")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;;; 履歴数を大きくする
(setq history-length 10000)
;;; 最近開いたファイルを保存する数を増やす
(setq recentf-max-saved-items 10000)
;;; ミニバッファの履歴を保存する
(savehist-mode 1)


;; Emacs Lisp Package Archive（ELPA）──Emacs Lispパッケージマネーャ
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; load-path を追加する関数を定義
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
              (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))

;; 引数のディレクトリとそのサブディレクトリをload-pathに追加
(add-to-load-path "elisp" "public_repos" "elpa")


;;auto-installの設定
;;(when(require 'auto-install nil t)
  ;;インストールディレクトリを設定する
;;  (setq auto-install-directory "~/.emacs.d/elisp/")
  ;;EmacsWikiに登録されているelispの名前を取得する
;;  (auto-install-update-emacswiki-package-name t)
  ;;必要であればプロキシの設定を行う
  ;;(setq url-proxy-services '(("http" . "localhost:8339")))
  ;;install-elispの関数を利用可能にする
;;  (auto-install-compatibility-setup))


;; coffee-mode インデントを2にする
(defun coffee-custom ()
  "coffee-mode-hook"
  (set (make-local-variable 'tab-width) 2)
  (setq coffee-tab-width 2))

(add-hook 'coffee-mode-hook
          '(lambda() (coffee-custom)))

;; web-mode
;; http://web-mode.org/
(require 'web-mode)
;;; emacs 23以下の互換
(when (< emacs-major-version 24)
    (defalias 'prog-mode 'fundamental-mode))

;;; 適用する拡張子
(add-to-list 'auto-mode-alist '("\\.phtml$"     . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp$"       . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x$"   . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb$"       . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?$"     . web-mode))

;;; インデント数
(defun web-mode-hook ()
    "Hooks for Web mode."
      (setq web-mode-html-offset   2)
        (setq web-mode-css-offset    2)
          (setq web-mode-script-offset 2)
            (setq web-mode-php-offset    2)
              (setq web-mode-java-offset   2)
                (setq web-mode-asp-offset    2))
(add-hook 'web-mode-hook 'web-mode-hook)


;; rakefile編集時にruby-modeで開く
(add-to-list 'auto-mode-alist '("[Rr]akefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake" . ruby-mode))

;; scss mode
(setq exec-path (cons (expand-file-name "~/.gem/ruby/1.8/bin") exec-path))
(autoload 'scss-mode "scss-mode")
(setq scss-compile-at-save nil) ;; 自動コンパイルをオフにする
(add-to-list 'auto-mode-alist '("\\.scss$" . scss-mode))

;; yaml-mode
(autoload 'yaml-mode "yaml-mode")
(when (require 'yaml-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode)))


;; M-x の補完候補をミニバッファに表示
;; http://www.bookshelf.jp/soft/meadow_27.html#SEC339
(require
 'mcomplete) (turn-on-mcomplete-mode)


;; ------------------------------------------------------------------------
;; @ auto-complete.el

;; 自動補完機能
;; https://github.com/m2ym/auto-complete
(when (require 'auto-complete-config nil t)
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
  (setq ac-ignore-case t)
  (ac-config-default))


;;
;; meta key
;;______________________________________________________________________

(setq ns-command-modifier (quote meta))
(setq ns-alternate-modifier (quote super))

;; ビープ音を抑制
(setq ring-bell-function '(lambda ()))

;; 行数、列数を表示
(line-number-mode t)
(column-number-mode t)

;; yes/noを、y/nで選択できるようにする。
(fset 'yes-or-no-p 'y-or-n-p)

;; kill-lineで行末の改行文字も削除
(setq kill-whole-line t)

;; リージョンをC-hで削除
(delete-selection-mode 1)

;; インデントはスペースで
(setq-default indent-tabs-mode nil)


;;
;;dired
;;______________________________________________________________________

;;; diredを便利にする
(require 'dired-x)

;;; diredから"r"でファイル名をインライン編集する
(require 'wdired)
(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)

;;
;; keymaps
;;______________________________________________________________________

;; C-hをBackSpaceに
(global-set-key "\C-h" 'delete-backward-char)

;; C-m 改行 + インデント
(global-set-key "\C-m" 'newline-and-indent)

;; C-x ?でヘルプ
(global-set-key "\C-x?" 'help)

;; 折り返しトグルコマンド
(define-key global-map (kbd "C-c l") 'toggle-truncate-lines)

;; "C-t" でウィンドウを切り替える。初期値はtranspose-chars
(define-key global-map (kbd "C-t") 'other-window)

;; grep
(define-key global-map (kbd "M-C-g") 'grep)

;; 再帰的にgrep
(require 'grep)
(setq grep-command-before-query "grep -nH -r -e ")
(defun grep-default-command ()
  (if current-prefix-arg
      (let ((grep-command-before-target
             (concat grep-command-before-query
                     (shell-quote-argument (grep-tag-default)))))
        (cons (if buffer-file-name
                  (concat grep-command-before-target
                          " *."
                          (file-name-extension buffer-file-name))
                (concat grep-command-before-target " ."))
              (+ (length grep-command-before-target) 1)))
    (car grep-command)))
(setq grep-command (cons (concat grep-command-before-query " .")
                         (+ (length grep-command-before-query) 1)))

;; anything.el
;; 起動してウインドウ開いた時に キーバインドを再び押すとウインドウを閉じる
(require 'anything)
(setq my-anything-keybind (kbd "C-]"))
(global-set-key my-anything-keybind 'anything-for-files)
(define-key anything-map my-anything-keybind 'abort-recursive-edit)
