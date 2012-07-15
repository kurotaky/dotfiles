;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; スタートアップ時のメッセージを抑制
(setq inhibit-startup-message t)

;; エンコーディングは基本的にUTF-8
(set-language-environment "Japanese")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

;;; 履歴数を大きくする
(setq history-length 10000)

;;; 最近開いたファイルを保存する数を増やす
(setq recentf-max-saved-items 10000)


;; Emacs Lisp Package Archive（ELPA）──Emacs Lispパッケージマネーャ
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("ELPA" . "http://tromey.com/elpa/"))
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
(when(require 'auto-install nil t)
  ;;インストールディレクトリを設定する
  (setq auto-install-directory "~/.emacs.d/elisp/")
  ;;EmacsWikiに登録されているelispの名前を取得する
  (auto-install-update-emacswiki-package-name t)
  ;;必要であればプロキシの設定を行う
  ;;(setq url-proxy-services '(("http" . "localhost:8339")))
  ;;install-elispの関数を利用可能にする
  (auto-install-compatibility-setup))


;; coffee-mode インデントを2にする
(defun coffee-custom ()
  "coffee-mode-hook"
  (set (make-local-variable 'tab-width) 2)
  (setq coffee-tab-width 2))

(add-hook 'coffee-mode-hook
          '(lambda() (coffee-custom)))

;;scss mode
(setq exec-path (cons (expand-file-name "~/.gem/ruby/1.8/bin") exec-path))
(autoload 'scss-mode "scss-mode")
(setq scss-compile-at-save nil) ;; 自動コンパイルをオフにする
(add-to-list 'auto-mode-alist '("¥¥.scss$" . scss-mode))


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

;;; 再帰的にgrep
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
