;; cocoa-emacs-config.el
;; Tatsuya Hoshino

;; Mac の場合 command を meta に割り当て
(when (eq system-type 'darwin)
  (setq ns-command-modifier (quote meta))
  )

;; ======  font  ====== ;;
;; ascii フォントを Ricty にする
(set-face-attribute 'default nil
                    :family "Ricty"
                    :height 150)
;; 日本語フォントをメイリオにする
(set-fontset-font
 nil 'japanese-jisx0208
 (font-spec :family "Ricty"))
;; =============== ;;
