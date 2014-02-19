;;M系
;;==========================
(global-set-key [(meta return)] 'toggle-fullscreen)

;;C-x系
;;==========================

; yasnippet
(define-key yas-minor-mode-map (kbd "C-x i i") 'yas-insert-snippet)     ; 既存スニペットを挿入する
(define-key yas-minor-mode-map (kbd "C-x i n") 'yas-new-snippet)        ; 新規スニペットを作成するバッファを用意する
(define-key yas-minor-mode-map (kbd "C-x i v") 'yas-visit-snippet-file) ; 既存スニペットを閲覧・編集する

(define-key yas-minor-mode-map (kbd "C-x ]") 'anything-for-files) ; ファイルを探す


;;C-m系
(global-set-key "\C-m" 'newline-and-indent)

;; keybind入れ替え
;;==========================
;; CommandとOptionを入れ替える
(setq ns-command-modifier (quote meta))
(setq ns-alternate-modifier (quote super))
