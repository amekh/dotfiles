;; ツールバーを非表示
(tool-bar-mode -1)
;; メニューバーを非表示
(menu-bar-mode -1)
;; スタートアップ画面
(setq inhibit-startup-screen t)

;; 英語
(set-face-attribute 'default nil
           :family "Menlo" ;; font
           :height 100)    ;; font size
 
;; 日本語
(set-fontset-font
 nil 'japanese-jisx0208
;; (font-spec :family "Hiragino Mincho Pro")) ;; font
  (font-spec :family "Hiragino Kaku Gothic ProN")) ;; font
 
;; 半角と全角の比を1:2に
(setq face-font-rescale-alist
      '((".*Hiragino_Mincho_pro.*" . 1.2)))

;; 背景色設定
(custom-set-faces
 '(default ((t (:background "#000" :foreground "#EEEEEE"))))
 '(cursor (
           (((class color) (background dark )) (:background "#AAA"))
           (((class color) (background light)) (:background "#999999"))
           (t ())
           )))
;; フレーム透過設定
(add-to-list 'default-frame-alist '(alpha . (0.85 0.85)))

;;フルスクリーン
(setq ns-use-native-fullscreen nil)

(defun toggle-fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen (if (frame-parameter nil 'fullscreen)
                                           nil
                                           'fullboth)))
