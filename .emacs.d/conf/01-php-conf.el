;; 配列内のインデントを調節
(add-hook 'php-mode-hook
          (lambda()
            ;; '+はc-basic-offsetの値を利用
            (c-set-offset 'arglist-intro '+)
            ;; 0はインデントなし
            (c-set-offset 'arglist-close 0)
            ))