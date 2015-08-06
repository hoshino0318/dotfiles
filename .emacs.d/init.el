;; init.el
;; Author: Tatsuya Hoshino
;; Update: 2015/01/29

;; load-path を追加する関数を定義
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
              (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))

(defvar emacs23-p (<= emacs-major-version 23))  ; 23 以下
(defvar emacs24-p (>= emacs-major-version 24))  ; 24 以上
(defvar darwin-p (eq system-type 'darwin))      ; Mac OS X 用
(defvar nt-p (eq system-type 'windows-nt))      ; Windows 用

;; 引数のディレクトリとそのサブディレクトリを load-path に追加
(add-to-load-path "elisp" "conf" "helm" "async")

;; 日本語環境設定
(set-language-environment "Japanese")
;; utf-8 を優先して使用
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; isearchで日本語を検索できるようにする
(defun w32-isearch-update ()
  (interactive)
  (isearch-update))
(define-key isearch-mode-map [compend] 'w32-isearch-update)
(define-key isearch-mode-map [kanji] 'isearch-toggle-input-method)

;; 起動直後の find-file のパスを ~/ にする
(cd "~/")

;; C-h をバックスペースにする
(define-key global-map (kbd "C-h") 'delete-backward-char)
;; C-j を newline-and-indent にする
(define-key global-map (kbd "C-j") 'newline-and-indent)
;; C-m を newline-and-indent にする
(define-key global-map (kbd "C-m") 'newline-and-indent)
;; C-q を other-window にする
(define-key global-map (kbd "C-q") 'other-window)

;; 反対側のウィンドウにいけるように
(setq windmove-wrap-around t)
;; C-M-{h,j,k,l}でウィンドウ間を移動
(define-key global-map (kbd "C-M-k") 'windmove-up)
(define-key global-map (kbd "C-M-j") 'windmove-down)
(define-key global-map (kbd "C-M-l") 'windmove-right)
(define-key global-map (kbd "C-M-h") 'windmove-left)

; (when emacs23-p
;   (when (require 'redo+ nil t)
;     ;; C-M-_' にリドゥに割り当てる
;     (define-key global-map (kbd "C-M-_") 'redo)
;     ))
;; ターミナルの場合、molokai を使う
(cond
  ((null window-system)
   (when emacs23-p
     (require 'color-theme-molokai)
     (color-theme-molokai)
     ;; customize
     (set-face-foreground 'font-lock-function-name-face "#5EC84E")
     (set-face-foreground 'font-lock-type-face "#FF8700")
     (set-face-foreground 'font-lock-string-face "#CCCCAC")
     (set-face-foreground 'font-lock-comment-face "#A6A4A7")
     )
   (when emacs24-p
     (setq custom-theme-directory "~/.emacs.d/themes/")
     (load-theme 'molokai t)
     ; (package-initialize)
     ; (require 'color-theme)
     ; (color-theme-initialize)
     )
   )
  )

;; ターミナル以外の場合
(when window-system
  (tool-bar-mode 0)
  ;; C-z を undo にする
  (define-key global-map (kbd "C-z") 'undo)
  )

;; bar を非表示
(menu-bar-mode 0)

;; 起動時の画面はいらない
(setq inhibit-startup-message t)

;; cua-mode
(cua-mode t)
(setq cua-enable-cua-keys nil)

;; mode line color
(set-face-foreground 'mode-line "white")
(set-face-background 'mode-line "#005fd7")
(set-face-foreground 'mode-line-inactive "gray30")
(set-face-background 'mode-line-inactive "gray85")

;; 最大限色付け
(setq font-lock-maximum-decoration t)

;; mark 領域に色付け
(setq transient-mark-mode t)

;; ベルの音とフラッシュの両方を消す
(setq ring-bell-function 'ignore)

;; BackUpファイル (xxx~) を作らない
(setq make-backup-files nil)

;; タイトルバーにファイル名を表示する
(setq frame-title-format "%b")

;; インデントを tab ではなく空白で行う
(setq-default indent-tabs-mode nil)

;; emacs-nav
(require 'nav)
(global-set-key "\C-x\C-d" 'nav-toggle)

;; paren-mode : 対応する括弧を強調して表示する
(setq show-paren-delay 0) ; 表示までの秒数。初期値は 0.125
(show-paren-mode t) ; 有効化

;; multi-term
;; (when (require 'multi-term nil t)
;;   (setq multi-term-program shell-file-name)
;;   )

;; 現在行のハイライト
(defface my-hl-line-face
  ;; 背景が dark ならば背景色を #303030 に
  '((((class color)(background dark))
     (:background "#303030" t)
     )
    ;; 背景が light ならば背景色を#FFDFEF に
    (((class color)(background light))
     (:background "#FFDFEF" t)
     )
    (t (:bold t)))
  "hl-line's my face")
(setq hl-line-face 'my-hl-line-face)
(global-hl-line-mode t)

;; smart-compile
(require 'smart-compile)
(global-set-key "\C-c\C-m" 'smart-compile)
(global-set-key "\C-c\C-r" 'recompile)
(global-set-key "\C-c'"    'next-error)

;;; linum
(require 'linum)
(global-linum-mode)
(setq linum-format "%d ")

;;; recentf
(require 'recentf)
(setq recentf-max-saved-items 1000)
(recentf-mode 1)
(custom-set-variables
 '(recentf-save-file "~/.emacs.d/.recentf"))

;; auto-install の設定
;; (cond
;;  ((null window-system)
;;   (when (require 'auto-install nil t)
;;     ;; インストールディレクトリを設定する 初期値は ~/.emacs.d/auto-install
;;     (setq auto-install-directory "~/.emacs.d/elisp/")
;;     ;; EmacsWiki に登録されている elisp の名前を取得する
;;     (auto-install-update-emacswiki-package-name t)
;;     ;; 必要であればプロキシの設定を行う
;;     ;;(setq url-proxy-services '(("http" . "localhost:8080")))
;;     ;; install-elisp の関数を利用可能にする
;;     (auto-install-compatibility-setup)
;;     (setq ediff-window-setup-function 'ediff-setup-windows-plain))))

;; package.el
;; (when (require 'package nil t)
;;   (add-to-list 'package-archives
;;                '("marmalade" . "http://marmalade-repo.org/packages/"))
;;   (add-to-list 'package-archives '("ELPA" . "http://tromey.com/elpa/"))
;;   (package-initialize))

;;; emacs technique bible
;; 試行錯誤用ファイルを開くための設定
(require 'open-junk-file)
;; C-x C-z で試行錯誤用ファイルを開く
(global-set-key (kbd "C-x C-z") 'open-junk-file)
;; 式の評価結果を注釈するための設定
(require 'lispxmp)
;; emacs-list-mode で C-c C-d を押すと注釈される
(define-key emacs-lisp-mode-map (kbd "C-c C-d") 'lispxmp)
;; 括弧の対応を保持する設定
(require 'paredit)
(add-hook 'emacs-lisp-mode-hook       'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-mode-hook             'enable-paredit-mode)
(add-hook 'ielm-mode-hook             'enable-paredit-mode)
;; 自動バイトコンパイルを無効にするファイル名の正規表現
;; (require 'auto-async-byte-compile)
;; (setq auto-async-byte-compile-exclude-files-regexp "/junk/")
;; (add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode)
;; (add-hook 'emacs-list-mode-hook 'turn-on-eldoc-mode)
;; (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
;; (add-hook 'ielm-mode-hook       'turn-on-eldoc-mode)
;; (setq eldoc-idle-delay 0.2)         ;; すぐに表示したい
;; (setq eldoc-minor-mode-string "")   ;; モードラインに Eldoc と表示しない
;; find-function をキー割り当てする
(find-function-setup-keys)
;;; end

;; auto-complete
(when (require 'auto-complete-config nil t)
  (add-to-list 'ac-dictionary-directories
               "~/.emacs.d/elisp/ac-dict")
  (define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
  (define-key ac-complete-mode-map (kbd "C-p") 'ac-previous)
  (define-key ac-complete-mode-map (kbd "C-n") 'ac-next)
  (ac-config-default))

;; color-moccur
(when (require 'color-moccur nil t)
  ;; M-o に occur-by-moccur を割り当て
  (define-key global-map (kbd "M-o") 'occur-by-moccur)
  ;; スペース区切りで AND 検索
  (setq moccur-split-word t)
  ;; ディレクトリ検索のとき除外するファイル
  (add-to-list 'dmoccur-exclusion-mask "\\.DS_Store")
  (add-to-list 'dmoccur-exclusion-mask "^#.+#$")
  ;; Migemo を利用できる環境であれば Migemo を使う
  (when (and (executable-find "cmigemo")
             (require 'migemo nil t))
    (setq moccur-use-migemo t)))

;; http://coderepos.org/share/browser/lang/elisp/init-loader/init-loader.el)
;; init-loader の設定
;; 環境に応じて適切な ***-config.el ファイルを読み込む
(require 'init-loader)
(init-loader-load "~/.emacs.d/conf") ; 設定ファイルがあるディレクトリを指定

;; redo+ の設定
(when emacs23-p
  (when (require 'redo+ nil t)
    ;; C-M-_' にリドゥに割り当てる
    (define-key global-map (kbd "C-M-_") 'redo)
    ))

;; shell-mode でエスケープを綺麗に表示
(autoload 'ansi-color-for-comint-mode-on "ansi-color"
     "Set `ansi-color-for-comint-mode' to t." t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; helm
(when (require 'helm-config nil t)
  (helm-mode 1)
  (define-key global-map (kbd "M-x")     'helm-M-x)
  (define-key global-map (kbd "C-x C-f") 'helm-find-files)
  (define-key global-map (kbd "C-x C-r") 'helm-recentf)
  (define-key global-map (kbd "C-c i")   'helm-imenu)
  (define-key global-map (kbd "C-x b")   'helm-buffers-list)

  (define-key helm-map (kbd "C-h") 'delete-backward-char)
  (define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)

  ;; For find-file etc.
  (define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)
  ;; For helm-find-files etc.
  (define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)

  (defadvice helm-ff-kill-or-find-buffer-fname (around execute-only-if-exist activate)
    "Execute command only if CANDIDATE exists"
    (when (file-exists-p candidate)
      ad-do-it))
  )

;; ##### Ruby ######
;; 括弧の自動挿入
;; (require 'ruby-electric nil t)
;; end に対応する行のハイライト
(when (require 'ruby-block nil t)
  (setq ruby-block-highlight-toggle t))
;; インタラクティブ Ruby を利用する
(autoload 'run-ruby "inf-ruby"
  "Run an inferior Ruby Process")
(autoload 'inf-ruby-keys "inf-ruby"
  "Set local key defs for inf-ruby in ruby-mode")

;; align for ruby
(require 'align)
(add-to-list 'align-rules-list
             '(ruby-comma-delimiter
               (regexp . ",\\(\\s-*\\)[^# \t\n]")
               (repeat . t)
               (modes  . '(ruby-mode))))
(add-to-list 'align-rules-list
             '(ruby-hash-literal
               (regexp . "\\(\\s-*\\)=>\\s-*[^# \t\n]")
               (repeat . t)
               (modes  . '(ruby-mode))))
(add-to-list 'align-rules-list
             '(ruby-assignment-literal
               (regexp . "\\(\\s-*\\)=\\s-*[^# \t\n]")
               (repeat . t)
               (modes  . '(ruby-mode))))
(add-to-list 'align-rules-list
             '(ruby-xmpfilter-mark
               (regexp . "\\(\\s-*\\)# => [^#\t\n]")
               (repeat . nil)
               (modes  . '(ruby-mode))))

;; fix ruby mode indent
(setq ruby-deep-indent-paren-style nil)
(defadvice ruby-indent-line (after unindent-closing-paren activate)
  (let ((column (current-column))
        indent offset)
    (save-excursion
      (back-to-indentation)
      (let ((state (syntax-ppss)))
        (setq offset (- column (current-column)))
        (when (and (eq (char-after) ?\))
                   (not (zerop (car state))))
          (goto-char (cadr state))
          (setq indent (current-indentation)))))
    (when indent
      (indent-line-to indent)
      (when (> offset 0) (forward-char offset)))))

;; ruby-mode-hook 用の関数を定義
(defun ruby-mode-hooks ()
  (inf-ruby-keys)
  ;;(ruby-electric-mode t)
  (ruby-block-mode t))
;; ruby-mode-hook に追加
(add-hook 'ruby-mode-hook 'ruby-mode-hooks)
;; extensions
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$"  . ruby-mode))
;; #################

;; Haskell
(when (load "haskell-site-file")
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
  (add-hook 'haskell-mode-hook 'font-lock-mode)
  (add-hook 'haskell-mode-hook 'imenu-add-menubar-index)
  )

;; Java
(add-hook 'java-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (setq c-basic-offset 2)
            (c-set-offset 'topmost-intro-cont 0)))

;; Groovy
(when (require 'groovy-mode)
  (add-to-list 'auto-mode-alist '("\\.\\(groovy\\|gradle\\)$" . groovy-mode)))

;; Scala
(when (require 'scala-mode-auto)
  (add-to-list 'auto-mode-alist '("\\.scala$" . scala-mode)))

;; Coffee
(when (require 'coffee-mode)
  (add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
  (custom-set-variables '(coffee-tab-width 2))
  )

;; Visual Basic
(when (require 'visual-basic-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.\\(frm\\|bas\\|cls\\|vbs\\|vba\\|xla\\)$" . visual-basic-mode)))

;; Haml
(when (require 'haml-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode)))

;; Less
(when (require 'less-css-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.less$" . less-css-mode)))

;; Markdown
(when (require 'markdown-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode)))

;; Slim
(when (require 'slim-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.slim$" . slim-mode)))

;; Yaml
(when (require 'yaml-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode)))

;; YASnippet
(when (require 'yasnippet-bundle nil t))

;; Zen Coding
(when (require 'zencoding-mode nil t)
  (add-hook 'sgml-mode-hook 'zencoding-mode)
  (add-hook 'html-mode-hook 'zencoding-mode)
  ;; M-e を zenconding-expand-line にする
  (define-key zencoding-mode-keymap (kbd "M-e") 'zencoding-expand-line)
  )

;; SVN
(when (executable-find "svn")
  (setq svn-status-verbose nil)
  (autoload 'svn-status "psvn" "Run `svn status'." t))

;; Git
(when (executable-find "git")
  (require 'egg nil t))

;; SQL
(eval-after-load "sql"
  (load-library "sql-indent"))
(custom-set-variables
 '(sql-indent-offset 2))

;; CSS
(setq css-indent-offset 2)

;; web-mode
;; http://web-mode.org/
(when (require 'web-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.html$" . web-mode))

  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)

  ;; 色の設定
  (custom-set-faces
   '(web-mode-doctype-face
     ((t (:foreground "#BF1E56"))))
   '(web-mode-html-tag-face
     ((t (:foreground "#DB531F"))))
   '(web-mode-html-attr-name-face
     ((t (:foreground "#44A5CB"))))
   '(web-mode-html-attr-value-face
     ((t (:foreground "#D8DAB4"))))
   '(web-mode-comment-face
     ((t (:foreground "#D9333F"))))
   '(web-mode-server-comment-face
     ((t (:foreground "#D9333F"))))
   '(web-mode-css-rule-face
     ((t (:foreground "#A0D8EF"))))
   '(web-mode-css-pseudo-class-face
     ((t (:foreground "#FF7F00"))))
   '(web-mode-css-at-rule-face
     ((t (:foreground "#FF7F00")))))
  )

;; JavaScript
(when (require 'js2-mode nil t)
  ; Use default js-mode instead of espresso
  ;; see: http://16777215.blogspot.jp/2011/05/emacs23-js2-mode-without-espresso.html
  (autoload 'js-mode "js")
  (defun my-js2-indent-function ()
    (interactive)
    (save-restriction
      (widen)
      (let* ((inhibit-point-motion-hooks t)
             (parse-status (save-excursion (syntax-ppss (point-at-bol))))
             (offset (- (current-column) (current-indentation)))
             (indentation (js--proper-indentation parse-status))
             node)
        (save-excursion
          ;; I like to indent case and labels to half of the tab width
          (back-to-indentation)
          (if (looking-at "case\\s-")
              (setq indentation (+ indentation (/ js-indent-level 2)))))
        (indent-line-to indentation)
        (when (> offset 0) (forward-char offset)))))

  (defun my-indent-sexp ()
    (interactive)
    (save-restriction
      (save-excursion
        (widen)
        (let* ((inhibit-point-motion-hooks t)
               (parse-status (syntax-ppss (point)))
               (beg (nth 1 parse-status))
               (end-marker (make-marker))
               (end (progn (goto-char beg) (forward-list) (point)))
               (ovl (make-overlay beg end)))
          (set-marker end-marker end)
          (overlay-put ovl 'face 'highlight)
          (goto-char beg)
          (while (< (point) (marker-position end-marker))
            ;; don't reindent blank lines so we don't set the "buffer
            ;; modified" property for nothing
            (beginning-of-line)
            (unless (looking-at "\\s-*$")
              (indent-according-to-mode))
            (forward-line))
          (run-with-timer 0.5 nil '(lambda(ovl)
                                     (delete-overlay ovl)) ovl)))))
  (defun my-js2-mode-hook ()
    (require 'js)
    (setq js-indent-level 2
          indent-tabs-mode nil
          c-basic-offset 2
          js2-strict-trailing-comma-warning nil
          js2-strict-inconsistent-return-warning nil)
    (c-toggle-auto-state 0)
    (c-toggle-hungry-state 1)
    (set (make-local-variable 'indent-line-function) 'my-js2-indent-function)
                                        ;  (define-key js2-mode-map [(meta control |)] 'cperl-lineup)
    (define-key js2-mode-map [(meta control \;)]
      '(lambda()
         (interactive)
         (insert "/* -----[ ")
         (save-excursion
           (insert " ]----- */"))
         ))
    (define-key js2-mode-map [(return)] 'newline-and-indent)
    (define-key js2-mode-map [(backspace)] 'c-electric-backspace)
    (define-key js2-mode-map [(control d)] 'c-electric-delete-forward)
    (define-key js2-mode-map [(control meta q)] 'my-indent-sexp)
    (if (featurep 'js2-highlight-vars)
        (js2-highlight-vars-mode))
    (message "My JS2 hook"))

  (add-hook 'js2-mode-hook 'my-js2-mode-hook)
  (add-to-list 'auto-mode-alist '("\\.\\(js\\|json\\)$" . js2-mode))
  )
