;;; ruby-modeの設定

;; 括弧の自動挿入
;; 標準パッケージ
(defun ruby-insert-end ()
  (interactive)
  (insert "end")
  (ruby-indent-line t)
  (end-of-line))

(require 'ruby-electric nil t)

;; endに対応する行のハイライト
;; M-x auto-install-from-emacswiki RET ruby-block.el
(when (require 'ruby-block nil t)
  (setq ruby-block-highlight-toggle t))
;; irbを利用する
;; M-x install-elisp RET https://raw.github.com/ruby/ruby/trunk/misc/inf-ruby.el
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

;; Ruby用Flymakeの設定
(defun flymake-ruby-init ()
  (list "ruby" (list "-c" (flymake-init-create-temp-buffer-copy
                           'flymake-create-temp-inplace))))
(add-to-list 'flymake-allowed-file-name-masks
             '("\\.rb\\'" flymake-ruby-init))

(add-to-list 'flymake-err-line-patterns
             '("\\(.*\\):(\\([0-9]+\\)): \\(.*\\)" 1 2 nil 3))

;; Rubyのインデント
(define-key ruby-mode-map [return] 'newline-and-indent)
(define-key ruby-mode-map (kbd "C-m") 'newline-and-indent)

