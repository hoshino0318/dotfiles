;; ntemacs.el
;; Tatsuya Hoshino

;; NTEmacs �̐F�ݒ�
(set-face-foreground 'default "black")
(set-face-background 'default "white")
;(setq frame-background-mode 'dark)
(setq frame-background-mode 'light)

;; ======  font  ====== ;;
;; ascii �t�H���g�� Menlo �ɂ���
(set-face-attribute 'default nil
                    :family "Consolas"
                    :height 110)
;; ���{��t�H���g�����C���I�ɂ���
(set-fontset-font
 nil 'japanese-jisx0208
 (font-spec :family "Meiryo"))
;; =============== ;;