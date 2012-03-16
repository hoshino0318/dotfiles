;; ntemacs.el
;; Tatsuya Hoshino

;; NTEmacs の色設定
(set-face-foreground 'default "black")
(set-face-background 'default "white")
;(setq frame-background-mode 'dark)
(setq frame-background-mode 'light)


;; C-z を undo にする (Windows だけ)
(define-key global-map "\C-z" 'undo)

;; ======  font  ====== ;;
;; ascii フォントを Menlo にする
(set-face-attribute 'default nil
                    :family "Consolas"
                    :height 110)
;; 日本語フォントをメイリオにする
(set-fontset-font
 nil 'japanese-jisx0208
 (font-spec :family "Meiryo"))
;; =============== ;;