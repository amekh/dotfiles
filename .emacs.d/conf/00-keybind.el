;; [C-m] newline-and-indentを割り当てる。
(global-set-key (kbd "C-m") 'newline-and-indent)

;; [C-h] backspaceにする
(keyboard-translate ?\C-h ?\C-?)

;; [C-c l] 折り返しトグルコマンド
(define-key global-map (kbd "C-c l") 'toggle-truncate-lines)

;; [C-t] ウィンドウ切り替え - 初期値:transpose-chars
(define-key global-map (kbd "C-t") 'other-window)





