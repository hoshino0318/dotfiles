;; cocoa-emacs-config.el
;; Tatsuya Hoshino

;; Mac �ξ�� command �� meta �˳������
(when (eq system-type 'darwin)
  (setq ns-command-modifier (quote meta))
  )

;; ======  font  ====== ;;
;; ascii �ե���Ȥ� Ricty �ˤ���
(set-face-attribute 'default nil
                    :family "Ricty"
                    :height 170)
;; ���ܸ�ե���Ȥ� Ricty �ˤ���
(set-fontset-font
 nil 'japanese-jisx0208
 (font-spec :family "Ricty"))
;; =============== ;;
