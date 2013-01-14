;;Emacs ver.22以下は user-emacs-directory が未定義のため設定を追加
(when (< emacs-major-version 23)
  (defvar user-emacs-directory "~/.emacs.d/"))

;; load-pathを追加する関数を定義
(defun add-to-load-path (&rest paths)
  (let(path)
    (dolist (path paths paths)
      (let ((default-directory
	      (expand-file-name(concat user-emacs-directory path))))
	(add-to-list 'load-path default-directory)
	(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
	    (normal-top-level-add-subdirs-to-load-path))))))

;;引数のディレクトリとそのサブディレクトリをload-pathに追加
(add-to-load-path "elisp" "conf" "public_repos")

;; flymake読み込み
(require 'flymake)

;;http://coderepos.org/share/export/28647/lang/elisp/init-loader/init-loader.el
(require 'init-loader)
(init-loader-load "~/.emacs.d/conf") ; 設定ファイルがあるディレクトリを指定

;; auto-installの設定
(when (require 'auto-install nil t)
  (setq auto-install-directory "~/.emacs.d/elisp")
  ;; EmacsWikiに登録されているelispの名前を取得する
  (auto-install-update-emacswiki-package-name t)
  (auto-install-compatibility-setup))

;; redo+.elの設定
;; http://www.emacswiki.orag/emacs/download/redo+.el
(when (require 'redo+ nil t)
  (global-set-key (kbd "C-.") 'redo))

;; undohistの設定 - ファイルと閉じてもundoできる
(when (require 'undohist nil t)
  (undohist-initialize))

;; 自動コンパイルの設定
;; emacswiki auto-async-byte-compile.el
(require 'auto-async-byte-compile)
;; 自動コンパイルを無効にするファイル
(setq auto-async-byte-compile-exclude-files-regexp "/junk/")
(add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode)

(load "header-template")

;;Emacsの設定============================================

;; ElScreen - Crabon Emacs の標準パッケージ
(require 'elscreen)

;;フルスクリーン
(defun toggle-fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen (if (frame-parameter nil 'fullscreen)
                                           nil
                                           'fullboth)))
(global-set-key [(meta return)] 'toggle-fullscreen)
;; 背景透明化(アルファ設定)
(if window-system (progn
   (set-background-color "Black")
   (set-foreground-color "White")
   (set-cursor-color "Gray")
   (set-frame-parameter nil 'alpha 80)
   ))
;; メニューバーを消す
(menu-bar-mode 0)
(tool-bar-mode 0)
; Undo回数を設定
(setq undo-limit 100000)
(setq undo-strong-limit 130000)
;; 初期ページを非表示
(setq inhibit-startup-message t)
;; シフト + 矢印で範囲選択
(setq pc-select-selection-keys-only t)
(pc-selection-mode 1)
;; ビープ音を消す
(setq visible-bell t)
;; タブ文字の幅 - タブ文字を利用しない
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
;; 現在行をハイライト表示
(defface my-hl-line-face
  ;; 背景がdarkならば背景を紺に
  '((((class color) (background dark))
     (:background "NavyBlue" t))
    ;; 背景がlightならば背景色を緑に
    (((class color) (background light))
     (:background "LightGoldenrodYellow" t))
    (t (:bold t)))
  "hl-line's my face")
(setq hl-line-face 'my-hl-line-face)
(global-hl-line-mode t)
;; 対応する括弧のハイライト - paren-mode
(setq show-paren-delay 0) ; 表示までの０秒数。初期値は0.125
(show-paren-mode t) ; 有効か
;; parenのスタイル : expressionは括弧内も強調表示
(setq show-paren-style 'expression)
;; フェイスを変更する
(set-face-background 'show-paren-match-face nil)
(set-face-underline-p 'show-paren-match-face "yellow")
;; バックアップファイル + オートセーブファイルを格納場所をTempディレクトリに変更する
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
;; オートセーブファイルの作成までの秒間隔
(setq auto-save-timeout 15)
;; オートセーブファイル作成までのタイプ間隔
(setq auto-save-interval 60)

;; 文字コード指定 utf-8
(set-language-environment "japanese")
(prefer-coding-system 'utf-8)
;;Max OS X の場合のファイル名の設定
(when (eq system-type 'darwin)
  (require 'ucs-normalize)
  (set-file-name-coding-system 'utf-8-hfs)
  (setq locale-coding-system 'utf-8-hfs))
;; Windows の場合のファイル名設定
(when (eq window-system 'w32)
  (set-file-name-coding-system 'cp932)
  (setq locale-coding-system 'cp932))

