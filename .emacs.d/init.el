;; init.el
;; Edit by Tatsuya Hoshino
;; 2012-03-20

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
;; C-j を newline にする
(define-key global-map (kbd "C-j") 'newline)
;; C-m を newline-and-indent にする
(define-key global-map (kbd "C-m") 'newline-and-indent)
;; C-o を other-window にする
(define-key global-map (kbd "C-o") 'other-window)

;; ターミナル以外の場合
(when window-system
  ;; tool-bar を非表示
  (tool-bar-mode 0)
  ;; C-z を undo にする
  (define-key global-map (kbd "C-z") 'undo)
  )

;; 起動時の画面はいらない
(setq inhibit-startup-message t)

;; 最大限色付け
(setq font-lock-maximum-decoration t)

;; ベルの音とフラッシュの両方を消す
(setq ring-bell-function 'ignore)

;; mark 領域に色付け
(setq transient-mark-mode t)

;; デフォルトの透明度を設定する (85%)
(add-to-list 'default-frame-alist '(alpha . (100 80)))
;; カレントウィンドウの透明度を変更する (85%)
;; (set-frame-parameter nil 'alpha 0.85)
(set-frame-parameter nil 'alpha '(100 80))

;; 色設定
(set-face-foreground 'default "white")
(set-face-background 'default "black")
(setq frame-background-mode 'dark)

;; コンパイル関係
;; (require 'smart-compile)
;; (global-set-key "\C-c\C-m" 'smart-compile)
;; (global-set-key "\C-c\C-r" 'recompile)
;; (global-set-key "\C-c'"    'next-error)

;; BackUpファイル（hogehoge~）を作らない
(setq make-backup-files nil)

;; タイトルバーにファイル名を表示する
(setq frame-title-format "%b")

;; インデントを tab ではなく空白で行う
(setq-default indent-tabs-mode nil)

;; 行番号表示
(require 'linum)
(global-linum-mode)
(setq linum-format "%4d ")

;; auto-install の設定
(when (require 'auto-install nil t)
  ;; インストールディレクトリを設定する 初期値は ~/.emacs.d/auto-install
  (setq auto-install-directory "~/.emacs.d/elisp/")
  ;; EmacsWiki に登録されている elisp の名前を取得する
  (auto-install-update-emacswiki-package-name t)
  ;; 必要であればプロキシの設定を行う
  (setq url-proxy-services '(("http" . "localhost:8339")))
  ;; install-elisp の関数を利用可能にする
  (auto-install-compatibility-setup))

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

;; ##### Ruby ######
;; 括弧の自動挿入
;;(require 'ruby-electric nil t)
;; end に対応する行のハイライト
(when (require 'ruby-block nil t)
  (setq ruby-block-highlight-toggle t))
;; インタラクティブ Ruby を利用する
(autoload 'run-ruby "inf-ruby"
  "Run an inferior Ruby Process")
(autoload 'inf-ruby-keys "inf-ruby"
  "Set local key defs for inf-ruby in ruby-mode")

;; paren-mode : 対応する括弧を強調して表示する
(setq show-paren-delay 0) ; 表示までの秒数。初期値は 0.125
(show-paren-mode t) ; 有効化

;; ruby-mode-hook 用の関数を定義
(defun ruby-mode-hooks ()
  (inf-ruby-keys)
  ;;(ruby-electric-mode t)
  (ruby-block-mode t))
;; ruby-mode-hook に追加
(add-hook 'ruby-mode-hook 'ruby-mode-hooks)
;; #################
