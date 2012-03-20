;; cocoa-emacs-config.el
;; Tatsuya Hoshino

;; Mac の場合 command を meta に割り当て
(when (eq system-type 'darwin)
  (setq ns-command-modifier (quote meta))
  )