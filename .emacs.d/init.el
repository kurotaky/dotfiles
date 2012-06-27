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

;; Marmaladeのリポジトリを使用
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
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
(add-to-load-path "vendor") 

;; Highlight indentation
(add-to-list 'load-path "~/.emacs.d/vendor/Highlight-Indentation-for-Emacs")
(require 'highlight-indentation)

;; scss-mode
;;(autoload 'scss-mode "scss-mode")
;;(setq scss-compile-at-save nil) ;; 自動コンパイルをオフにする
;;(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))

;;coffee-mode
(add-to-list 'load-path "~/.emacs.d/vendor/coffee-mode")
(require 'coffee-mode)

(defun coffee-custom ()
  "coffee-mode-hook"
  (set (make-local-variable 'tab-width) 2)
  (setq coffee-tab-width 2))

(add-hook 'coffee-mode-hook
          '(lambda() (coffee-custom)))


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


;; keymaps

;; C-hをBackSpaceに
(global-set-key "\C-h" 'delete-backward-char)

;; C-mでインデントも。
(global-set-key "\C-m" 'newline-and-indent)

;; C-x ?でヘルプ
(global-set-key "\C-x?" 'help)


;;
;; Color
;;______________________________________________________________________

(set-foreground-color                                  "#CCCCCC") ; 文字色
;;(set-background-color                                  "#333333") ; 背景色
(set-cursor-color                                      "#FF0000") ; カーソル色
(set-face-background 'region                           "#222244") ; リージョン
(set-face-foreground 'modeline                         "#CCCCCC") ; モードライン文字
(set-face-background 'modeline                         "#333333") ; モードライン背景
(set-face-foreground 'mode-line-inactive               "#333333") ; モードライン文字(非アクティブ)
(set-face-background 'mode-line-inactive               "#CCCCCC") ; モードライン背景(非アクティブ)
(set-face-foreground 'font-lock-comment-delimiter-face "#888888") ; コメントデリミタ
(set-face-foreground 'font-lock-comment-face           "#888888") ; コメント
(set-face-foreground 'font-lock-string-face            "#7FFF7F") ; 文字列
(set-face-foreground 'font-lock-function-name-face     "#BF7FFF") ; 関数名
(set-face-foreground 'font-lock-keyword-face           "#FF7F7F") ; キーワード
(set-face-foreground 'font-lock-constant-face          "#FFBF7F") ; 定数(this, selfなども)
(set-face-foreground 'font-lock-variable-name-face     "#7F7FFF") ; 変数
(set-face-foreground 'font-lock-type-face              "#FFFF7F") ; クラス
(set-face-foreground 'fringe                           "#666666") ; fringe(折り返し記号なでが出る部分)
(set-face-background 'fringe                           "#282828") ; fringe

(add-hook 'org-mode-hook
          '(lambda ()
             (set-face-foreground 'org-hide "#282828")))

(add-hook 'mmm-mode-hook
          '(lambda ()
             (set-face-background 'mmm-default-submode-face "#404040")))

(add-hook 'linum-mode-hook
          '(lambda ()
             (set-face-foreground 'linum "#666666")
             (set-face-background 'linum "#000000")))


;;; P172-173 Ruby編集用の便利なマイナーモード
;; 括弧の自動挿入──ruby-electric
(require 'ruby-electric nil t)
;; endに対応する行のハイライト──ruby-block
(when (require 'ruby-block nil t)
      (setq ruby-block-highlight-toggle t))
;; インタラクティブRubyを利用する──inf-ruby
(autoload 'run-ruby "inf-ruby"
      "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby"
      "Set local key defs for inf-ruby in ruby-mode")

;; ruby-mode-hook用の関数を定義
(defun ruby-mode-hooks ()
      (inf-ruby-keys)
            (ruby-electric-mode t)
                    (ruby-block-mode t))
;; ruby-mode-hookに追加
(add-hook 'ruby-mode-hook 'ruby-mode-hooks)
