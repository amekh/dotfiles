;; point-undo の設定
;; M-x auto-install-from-emacswiki RET point-undo.el RET
(when (require 'point-undo nil t)
  (define-key global-map (kbd "M-[") 'point-undo)
  (define-key global-map (kbd "M-]") 'point-redo))
  