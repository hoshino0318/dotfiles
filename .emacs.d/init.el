;; init.el
;; Author: Tatsuya Hoshino
;; Update: 2013-02-21

;; load-path を追加する関数を定義
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
              (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))

;; 引数のディレクトリとそのサブディレクトリを load-path に追加
(add-to-load-path "elisp" "conf")

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

;; ターミナルの場合、molokai を使う
(cond
 ((null window-system)
  (require 'color-theme-molokai)
  (color-theme-molokai)
  ))

;; ターミナル以外の場合
(when window-system
  ;; tool-bar を非表示
  (tool-bar-mode 0)
  ;; C-z を undo にする
  (define-key global-map (kbd "C-z") 'undo)
  )

;; 起動時の画面はいらない
(setq inhibit-startup-message t)

;; 色設定
;; (set-face-foreground 'default "white")
;; (set-face-background 'default "black")
;; (setq frame-background-mode 'dark)

;; dmacro
(defconst *dmacro-key* "\C-t" "繰返し指定キー")
(global-set-key *dmacro-key* 'dmacro-exec)
(autoload 'dmacro-exec "dmacro" nil t)

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
  ;; 背景が dark ならば背景色を紺に
  '((((class color)(background dark))
     (:background "NavyBlue" t))
    ;; 背景が light ならば背景色を緑に
    (((class color)(background light))
     (:background "LightGoldenrodYellow" t))
    (t (:bold t)))
  "hl-line's my face")
(setq hl-line-face 'my-hl-line-face)
(global-hl-line-mode t)

;; smart-compile
(require 'smart-compile)
(global-set-key "\C-c\C-m" 'smart-compile)
(global-set-key "\C-c\C-r" 'recompile)
(global-set-key "\C-c'"    'next-error)

;; 行番号表示
(require 'linum)
(global-linum-mode)
(setq linum-format "%4d ")

;; auto-install の設定
(cond
 ((null window-system)
  (when (require 'auto-install nil t)
    ;; インストールディレクトリを設定する 初期値は ~/.emacs.d/auto-install
    (setq auto-install-directory "~/.emacs.d/elisp/")
    ;; EmacsWiki に登録されている elisp の名前を取得する
    (auto-install-update-emacswiki-package-name t)
    ;; 必要であればプロキシの設定を行う
    ;;(setq url-proxy-services '(("http" . "localhost:8339")))
    ;; install-elisp の関数を利用可能にする
    (auto-install-compatibility-setup)
    (setq ediff-window-setup-function 'ediff-setup-windows-plain))))

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
(require 'auto-async-byte-compile)
(setq auto-async-byte-compile-exclude-files-regexp "/junk/")
(add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode)
(add-hook 'emacs-list-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook       'turn-on-eldoc-mode)
(setq eldoc-idle-delay 0.2)         ;; すぐに表示したい
(setq eldoc-minor-mode-string "")   ;; モードラインに Eldoc と表示しない
;; find-function をキー割り当てする
(find-function-setup-keys)
;;; end

;; color-moccur の設定
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
(when (require 'redo+ nil t)
  ;; C-M-_' にリドゥに割り当てる
  (define-key global-map (kbd "C-M-_") 'redo)
  )

;; shell-mode でエスケープを綺麗に表示
(autoload 'ansi-color-for-comint-mode-on "ansi-color"
     "Set `ansi-color-for-comint-mode' to t." t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; ##### Ruby ######
;; 括弧の自動挿入
;; (require 'ruby-electric nil t)
;; end に対応する行のハイライト
(add-hook 'ruby-mode-hook
          #'(lambda()
              (when (require 'ruby-block nil t)
                (setq ruby-block-highlight-toggle t)
                )))
;; インタラクティブ Ruby を利用する
(autoload 'run-ruby "inf-ruby"
  "Run an inferior Ruby Process")
(autoload 'inf-ruby-keys "inf-ruby"
  "Set local key defs for inf-ruby in ruby-mode")

;; ruby-mode-hook 用の関数を定義
(defun ruby-mode-hooks ()
  (inf-ruby-keys)
  ;;(ruby-electric-mode t)
  (ruby-block-mode t))
;; ruby-mode-hook に追加
(add-hook 'ruby-mode-hook 'ruby-mode-hooks)
;; #################

;;; anything
;; (auto-install-batch "anything")
(when (require 'anything nil t)
  (setq
   ;; kill-ring する時の要素の最小値
   anything-kill-ring-threshold 4
   ;; 候補を表示するまでの時間 デフォルトは 0.5
   anything-idle-delay 0.3
   ;; タイプして再描画するまでの時間 デフォルトは 0.1
   anything-input-idle-delay 0.2
   ;; 候補の最大表示数 デフォルトは 50
   anything-candidate-number-limit 100
   ;; 候補が多いときに体感速度を早くする
   anything-quick-update t
   ;; 候補選択ショートカットをアルファベットに
   anything-enable-shortcuts 'alphabet)

  (when (require 'anything-config nil t)
    ;; root 権限でアクションを実行するときのコマンド
    ;; デフォルトでは "su"
    (setq anything-su-or-sudo "sudo"))

  (require 'anything-match-plugin nil t)

  (when (and (executable-find "cmigemo")
             (require 'migemo nil t))
    (require 'anything-migemo nil t))

  (when (require 'anything-complete nil t)
    ;; lisp シンボルの補完候補の再検索時間
    (anything-lisp-complete-symbol-set-timer 150))

  (require 'anything-show-completion nil t)

  (when (require 'auto-install nil t)
    (require 'anything-auto-install nil t))

  (when (require 'descbinds-anything nil t)
    ;; describe-bindings をAnything に置き換える
    (descbinds-anything-install))
  ;; M-y にanything-show-kill-ring を割り当てる
  (define-key global-map (kbd "M-y") 'anything-show-kill-ring)
  )
;; end anything

;; js2-mode
(add-hook 'js2-mode-hook
          #'(lambda()
              (when (require 'js2-mode nil t)
                (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
                (add-to-list 'auto-mode-alist '("\\.json$" . js2-mode)))))
;; Zen Coding
(when (require 'zencoding-mode nil t)
  (add-hook 'sgml-mode-hook 'zencoding-mode)
  (add-hook 'html-mode-hook 'zencoding-mode)
  ;; M-e を zenconding-expand-line にする
  (define-key zencoding-mode-keymap (kbd "M-e") 'zencoding-expand-line)
  )

;; YASnippet
(when (require 'yasnippet-bundle nil t))

;; Haskell
(when (load "haskell-site-file")
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
  (add-hook 'haskell-mode-hook 'font-lock-mode)
  (add-hook 'haskell-mode-hook 'imenu-add-menubar-index)
  )

;; Yaml
(when (require 'yaml-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode)))

;; Haml
(when (require 'haml-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode)))

;; Visual Basic
(when (require 'visual-basic-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.\\(frm\\|bas\\|cls\\|vbs\\|xla\\)$" . visual-basic-mode)))

;; Java
(add-hook 'java-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (setq c-basic-offset 2)))

;; Scala
(when (require 'scala-mode-auto)
  (add-to-list 'auto-mode-alist '("\\.scala$" . scala-mode)))
