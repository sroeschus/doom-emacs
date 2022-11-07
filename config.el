;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Stefan"
      user-mail-address "xxx@xxx.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;;(setq doom-theme 'doom-one)
(setq doom-theme 'spacemacs-dark)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
;; (setq display-line-numbers-type t)
(setq display-line-numbers-type nil)

;; Disable exit confirmation
(setq confirm-kill-emacs nil)

;; Make windows full height
(add-to-list 'initial-frame-alist '(fullscreen . fullheight))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
(load! "lisp/formatp.el")

;; Setting up linux kernel formatting options
;;
(defun linux-kernel-coding-style/c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces"
  (let* ((anchor (c-langelem-pos c-syntactic-element))
   (column (c-langelem-2nd-pos c-syntactic-element))
   (offset (- (1+ column) anchor))
   (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))

;; Add Linux kernel style
(add-hook 'c-mode-common-hook
    (lambda ()
      (c-add-style "linux-kernel"
       '("linux" (c-offsets-alist
            (arglist-cont-nonempty
             c-lineup-gcc-asm-reg
             linux-kernel-coding-style/c-lineup-arglist-tabs-only))))))

(defun linux-kernel-coding-style/setup ()
  (let ((filename (buffer-file-name)))
    ;; Enable kernel mode for the appropriate files
    (when (and buffer-file-name
               ( or (string-match "linux" buffer-file-name)
                    (string-match "liburing" buffer-file-name)))
                    ;; (string-match "xfstests" buffer-file-name)))
      (setq indent-tabs-mode t)
      (setq tab-width 8)
      (setq c-basic-offset 8)
      (c-set-style "linux-kernel"))))

(add-hook 'c-mode-hook 'linux-kernel-coding-style/setup)

;; Configure LSP.
;;
(setq lsp-clients-clangd-args '("-j=3"
                                "--background-index"
                                "--clang-tidy"
                                "--completion-style=detailed"
                                "--header-insertion=never"
                                "--header-insertion-decorators=0"))
(after! lsp-clangd (set-lsp-priority! 'clangd 2))
(after! lsp-clangd (setq exec-path(append '("~/fbsource/fbcode/third-party-buck/platform009/build/llvm-fb/9.0.0/bin/") exec-path)))

;; Make flycheck use C++11.
;; (after! flycheck
;;         (setq-default flycheck-clang-standard-library  "libc++"
;;                       flycheck-clang-language-standard "c++11"
;;                       flycheck-c/c++-clang-executable  "clang++"
;;                       flycheck-clang-include-path      '("/usr/local/include")))

;; (after! cc-mode
;;   (setq-local tab-width 8))

;; Configure git gutter.
(custom-set-variables
 '(git-gutter:added-sign "█|")
 '(git-gutter:modified-sign "█⫶")
 '(git-gutter:deleted-sign "█▁"))

(after! git-gutter
  (set-face-foreground 'git-gutter:modified "yellow"))

;; Overwrite existing key bindings.
;;
(map!
  (:leader
    (:prefix "b" :desc "Switch buffer" "b" 'consult-buffer)
    (:prefix "b" :desc "Switch workspace buffer" "B" '+vertico/switch-workspace-buffer)
    (:prefix "g" :desc "Format patch" "p" 'formatp-menu)))

(map!
 (:after evil-easymotion
  :m "gs" evilem-map
  (:map evilem-map
   "l" #'avy-goto-line)))

;; (map! :leader
;;  (:prefix-map ("c" . "code")
;;   (:prefix ("h" . "cscope")
;;    :desc "Search definition" "d" #'helm-cscope-find-global-definition-no-prompt
;;    :desc "Search definition" "D" #'helm-cscope-find-global-definition
;;    :desc "Search file" "f" #'helm-cscope-find-files-including-file
;;    :desc "Search text" "T" #'helm-cscope-find-this-text-string
;;    :desc "Search symbol" "s" #'helm-cscope-find-this-symbol-no-prompt
;;    :desc "Search symbol with prompt" "S" #'helm-cscope-find-this-symbol-no-prompt
;;    :desc "Search symbol" "t" #'helm-cscope-pop-mark)))
;;
;; (cscope-setup)
;; (setq helm-cscope-execute-action-if-one t)

;; (use-package! consult-cscope
;;   :defer t
;;   :commands (consult-cscope-symbol
;;              consult-cscope-definition
;;              consult-cscope-called-by
;;              consult-cscope-calling
;;              consult-cscope-text
;;              consult-cscope-egrep
;;              consult-cscope-file
;;              consult-cscope-including
;;              consult-cscope-assignment))
;;
;; (map! :leader
;;  (:prefix-map ("c" . "code")
;;   (:prefix ("h" . "cscope")
;;    :desc "Search symbol" "s" #'consult-cscope-symbol
;;    :desc "Search definition" "d" #'consult-cscope-definition
;;    :desc "Search text" "t" #'consult-cscope-definition
;;    :desc "Search file" "f" #'consult-cscope-file)))

;; Set string so environment is available when terminal is invoked
;;
(defun my/source-bashrc ()
      (interactive)
      (vterm-send-string "source ~/.bash_profile"))

(add-hook 'vterm-mode-hook #'my/source-bashrc)

;; (setq epg-pinentry-mode 'loopback)

;; Set sendmail properties and use msmtp.
;;
(setq sendmail-program "/usr/bin/msmtp"
      send-mail-function 'smtpmail-send-it
      message-sendmail-f-is-evil t
      message-sendmail-extra-arguments '("--read-envelope-from")
      message-send-mail-function 'message-send-mail-with-sendmail)
(setq smtpmail-smtp-server "127.0.0.1")
(setq smtpmail-smtp-service 13776)


;; Set sendmail shortcuts for different folders.
;;
(setq mu4e-maildir-shortcuts
  '( (:maildir "/inbox"     :key  ?i)
     (:maildir "/drafts"    :key  ?d)
     (:maildir "/patches"   :key  ?p)
     (:maildir "/archive"   :key  ?a)
     (:maildir "/sent"      :key  ?s)))

;; Map different email folders.
;;
(set-email-account! "devkernel.io"
  '((mu4e-sent-folder       . "/Sent")
    (mu4e-drafts-folder     . "/Drafts")
    (mu4e-trash-folder      . "/Trash")
    (mu4e-refile-folder     . "/Archive")
    (smtpmail-smtp-user     . "shr@devkernel.io"))
  t)

;; (setq mu4e-headers-buffer-name "*mu4e-headers*")

(defalias 'mu4e-search 'mu4e-headers-search)
(defalias 'mu4e-search-edit 'mu4e-headers-search-edit)
(defalias 'mu4e-search-bookmark 'mu4e-headers-search-bookmark)
(defalias 'mu4e-search-bookmark-edit 'mu4e-headers-search-bookmark-edit)
(defalias 'mu4e-search-narrow 'mu4e-headers-search-narrow)


;; Fix error message on startup of mu4e.
;;
(after! evil-collection
  (after! mu4e
    (setq mu4e-update-interval 60)
    (setq evil-collection-mu4e-end-region-misc "\\[q\\]uit")))

(defhydra hydra-mu4e-main (:color blue :hint nil)
  "
_C_: compose             _b_: search bookmark    _u_: update    _m_ toogle mail send
_J_: jump maildir        _s_: search             _$_: log
_f_: send queued mail    _B_: edit bookmark
_q_: quit                _S_: edit search
"

  ;; general
  ("C" mu4e-compose-new)
  ("J" mu4e~headers-jump-to-maildir)
  ("f" smtp-mail-send-queued-mail)
  ("q" mu4e-quit)

  ;; search
  ("b" mu4e-search-bookmark)
  ("s" mu4e-search)
  ("S" mu4e-search-edit)
  ("B" mu4e-search-bookmark-edit)

  ;; miscellany
  ("A" mu4e-about)
  ("H" mu4e-display-manual)
  ("N" mu4e-news)

  ;; switches
  ("m" mu4e--main-toggle-mail-sending-mode)

  ;; more miscellany
  ("u" mu4e-update-mail-and-index)
  ("$" mu4e-show-log)

  ("." nil))

(defhydra hydra-mu4e-headers (:color blue :hint nil)
  "
_C_: compose           _s_: search             _j_ : next            _a_: action       _A_: mark action       _zr_: incl related       _u_: update
_E_: edit              _S_: edit search        _k_ : prev            _m_: move         _*_: mark something    _zt_: threading          _$_: log
_F_: forward           _b_: search bookmark                        _r_: refile       _&_: mark custom       _zd_: skip duplicates
_R_: reply             _B_: edit bookmark      _gj_: next unread     _d_: trash        _!_: mark read
                                           _gk_: prev unread     _D_: delete       _%_: mark pattern
_J_: jump maildir      _/_: narrow search                          _x_: execute      _+_: mark flag
_q_: quit              _o_: change sort        _gv_: other view
                                                                               _u_: unmark
                                                                               _U_: unmark all

                                                                               _=_: mark untrash
                                                                               _?_: mark unread
                                                                               _-_: mark unflag




"

  ;; general
  ("C" mu4e-compose-new)
  ("E" mu4e-compose-edit)
  ("F" mu4e-compose-forward)
  ("R" mu4e-compose-reply)
  ("q" mu4e~headers-quit-buffer)

  ;; search
  ("b" mu4e-search-bookmark)
  ("s" mu4e-search)
  ("S" mu4e-search-edit)
  ("B" mu4e-search-bookmark-edit)
  ("o" mu4e-headers-change-sorting)
  ("/" mu4e-search-narrow)

  ;; move
  ("j" mu4e-headers-next)
  ("k" mu4e-headers-prev)
  ("gj" mu4e-headers-next-unread)
  ("gk" mu4e-headers-prev-unread)
  ("gv" mu4e-select-other-view)

  ;; action
  ("a" mu4e-headers-action)
  ("d" mu4e-headers-mark-for-trash)
  ("m" mu4e-headers-mark-for-move)
  ("r" mu4e-headers-mark-for-refile)
  ("u" mu4e-headers-mark-for-unmark)
  ("x" mu4e-mark-execute-all)
  ("A" mu4e-headers-mark-for-action)
  ("D" mu4e-headers-mark-for-delete)
  ("U" mu4e-mark-unmark-all)
  ("*" mu4e-headers-mark-for-something)
  ("&" mu4e-headers-mark-custom)
  ("=" mu4e-headers-mark-for-untrash)
  ("?" mu4e-headers-mark-for-unread)
  ("!" mu4e-headers-mark-for-read)
  ("%" mu4e-headers-mark-pattern)
  ("+" mu4e-headers-mark-for-flag)
  ("-" mu4e-headers-mark-for-unflag)

  ;; more miscellany
  ("u" mu4e-update-mail-and-index)
  ("J" mu4e~headers-jump-to-maildir)
  ("$" mu4e-show-log)

  ;; switches
  ("zr" mu4e-headers-toggle-include-related)
  ("zt" mu4e-headers-toggle-threading)
  ("zd" mu4e-headers-toggle-skip-duplicates)

  ("." nil))

(map! :after mu4e
      :map mu4e-main-mode-map
      :nv "S" #'mu4e-headers-search-edit
      :nv "$" #'mu4e-show-log
      :nv "." #'hydra-mu4e-main/body)

(map! :after mu4e
      :map mu4e-headers-mode-map
      :nv "." #'hydra-mu4e-headers/body)
