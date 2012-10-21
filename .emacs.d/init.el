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

;;http://coderepos.org/share/export/28647/lang/elisp/init-loader/init-loader.el
(require 'init-loader)
(init-loader-load "~/.emacs.d/conf") ; 設定ファイルがあるディレクトリを指定

;;Emacsの設定============================================

;;フルスクリーン
(defun toggle-fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen (if (frame-parameter nil 'fullscreen)
                                           nil
                                           'fullboth)))
(global-set-key [(meta return)] 'toggle-fullscreen)
;; 背景透明化(アルファ設定)
(require 'color-theme)
(color-theme-initialize)
(color-theme-dark-laptop)
(set-frame-parameter nil 'alpha 80 )
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
