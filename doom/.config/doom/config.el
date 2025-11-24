;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
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
(setq doom-theme 'doom-dracula)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; NOTE DOOM FONT

;;; Fonts (Fira Code stack)
(setq doom-font                (font-spec :family "FiraCode Nerd Font" :size 16)
      doom-variable-pitch-font (font-spec :family "Noto Sans"          :size 17)
      doom-symbol-font         (font-spec :family "Symbols Nerd Font Mono"))

;; Fallbacks (fonts-only path; no emojify, no unicode-fonts rewrites)
(when (member "Symbols Nerd Font Mono" (font-family-list))
  (set-fontset-font t 'symbol "Symbols Nerd Font Mono" nil 'prepend))

;; Try monochrome emoji first (rock-solid); switch to Color if it behaves on your stack
(let ((emoji (cond
              ((member "Noto Emoji" (font-family-list))         "Noto Emoji")         ; monochrome, safest
              ((member "Noto Color Emoji" (font-family-list))   "Noto Color Emoji")   ; color, can glitch on some PGTK setups
              ((member "Twitter Color Emoji" (font-family-list)) "Twitter Color Emoji")
              (t nil))))
  (when emoji
    (set-fontset-font t 'emoji emoji nil 'prepend)))

(after! unicode-fonts
  (setq unicode-fonts-skip-font-groups '(emoji))
  (unicode-fonts-setup))

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
;; NOTE PACKAGES

;; Globals
(use-package! cua-base
  :init
  (setq cua-enable-cua-keys t
        cua-prefix-override-inhibit-delay 0.001
        cua-keep-region-after-copy nil)
  :config
  (cua-mode 1))

;; ;; Full CUA, no delay, keep selection after copy
;; (setq cua-enable-cua-keys t
;;       cua-keep-region-after-copy t
;;       cua-prefix-override-inhibit-delay 0)  ;; 0 = no pause deciding C-x/C-c vs cut/copy

;; (cua-mode 1)

;; Kill flycheck
;; (after! flycheck
;;   (global-flycheck-mode -1))

;; Remove fringe
(set-fringe-mode 0)

;; Word wrap
(setq-default word-wrap t
              truncate-lines nil)
(global-visual-line-mode 1)

(after! adaptive-wrap
  (adaptive-wrap-prefix-mode -1))

;; Doom scratch buffers
(setq doom-scratch-initial-major-mode 'lisp-interaction-mode)

;; More colors
(setq treesit-font-lock-level 4
      font-lock-maximum-decoration t)

;; Highlighting
(use-package! rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Multi-Cursor
(setq mc/always-run-for-all t)
(with-eval-after-load 'multiple-cursors
  (define-key mc/keymap (kbd "<return>") nil)) ; prevent newline ending mc session

;; Org Download
(use-package! org-download
  :after org
  :hook (org-mode . org-download-enable)
  :init
  ;; Put images next to the note: ./images/...
  (setq org-download-method 'directory
        org-download-image-dir "images"
        org-download-heading-lvl nil         ; don't nest per heading
        org-download-timestamp "_%Y%m%d_%H%M%S" ; unique names like foo_20251108_2130.png
        org-image-actual-width '(600))       ; default render width (optional)
  :config
  ;; Show the image right after saving it
  (add-hook 'org-download-after-download-hook #'org-display-inline-images))

(map! :map org-mode-map "C-M-y" #'org-download-clipboard)

;; ;; Dirvish
;; (use-package dirvish
;;   :init
;;   (dirvish-override-dired-mode)  ;; make Dirvish the default for Dired
;;   :config
;;   ;; Nice defaults; tune to taste
;;   (setq dirvish-attributes '(subtree-state all-the-icons file-size)
;;         dirvish-preview-dispatchers '(image video audio epub pdf)
;;         dired-kill-when-opening-new-dired-buffer t))

;; NOTE CUSTOM FUNCTIONS

;; ORG
(defun my/org-select-forward-paragraph ()
  (interactive)
  (unless (use-region-p) (set-mark-command nil))
  (org-forward-paragraph))   ;; respects headings/blank lines better than forward-paragraph

(defun my/org-select-backward-paragraph ()
  (interactive)
  (unless (use-region-p) (set-mark-command nil))
  (org-backward-paragraph))

(defun org-sentences-to-bullets (beg end)
  "Split sentences in region into Org bullets."
  (interactive "r")
  (save-excursion
    (let ((inhibit-read-only t))
      (goto-char beg)
      (unless (looking-at "^[ \t]*- ")
        (insert "- "))
      (while (re-search-forward "\\([.!?]\\)\\s-+" end t)
        (replace-match "\\1\n- " nil nil)))))

;; UPDATE KEYBINDINGS
(defun my/backward-delete-word ()
  "Delete word backward without adding to kill ring."
  (interactive)
  (delete-region (point) (save-excursion (backward-word) (point))))

;; WINDOW MOVEMENT
(defun my/split-left-and-focus ()
  (interactive)
  (split-window-right)  (windmove-right))

(defun my/split-right-and-focus ()
  (interactive)
  (split-window-right)  (windmove-right))

(defun my/split-up-and-focus ()
  (interactive)
  (split-window-below) (windmove-down))

(defun my/split-down-and-focus ()
  (interactive)
  (split-window-below) (windmove-down))

(defun my/resize-pane-right ()
  "Resize window (pane) to the right"
  (interactive)
  (if (window-in-direction 'right)
      (enlarge-window-horizontally 5)
    (shrink-window-horizontally 5)))

(defun my/resize-pane-left ()
  "Resize window (pane) to the left"
  (interactive)
  (if (window-in-direction 'left)
      (enlarge-window-horizontally 5)
    (shrink-window-horizontally 5)))

(defun my/resize-pane-up ()
  "Resize window (pane) upwards"
  (interactive)
  (if (window-in-direction 'above)
      (enlarge-window 5)
    (shrink-window 5)))

(defun my/resize-pane-down ()
  "Resize window (pane) upwards"
  (interactive)
  (if (window-in-direction 'below)
      (enlarge-window 5)
    (shrink-window 5)))

;; Pop Pane to New Window
(defvar pop-out-window-configuration nil)
(defvar pop-out-original-frame nil)

(defun my/move-buffer-to-new-frame ()
  "Opens Emacs pane into a new window."
  (interactive)
  (setq pop-out-window-configuration (current-window-configuration))
  (setq pop-out-original-frame (selected-frame))
  (let ((buf (current-buffer)))
    (when (not (one-window-p))
      (delete-window))
    (select-frame (make-frame))
    (switch-to-buffer buf)))

(defun my/return-buffer-to-previous-frame ()
  "Returns Emacs pane into a new window."
  (interactive)
  (when (and pop-out-window-configuration pop-out-original-frame)
    (let ((buf (current-buffer))
          (this-frame (selected-frame)))
      (with-selected-frame pop-out-original-frame
        (set-window-configuration pop-out-window-configuration)
        (switch-to-buffer buf))
      (delete-frame this-frame)
      (setq pop-out-window-configuration nil)
      (setq pop-out-original-frame nil))))

;;; NOTE KEYBINDS
;; ORG
(use-package! org
  :init
  (setq org-support-shift-select 'always
        shift-select-mode t)
  :config
  (define-key org-mode-map (kbd "C-S-<down>")  #'my/org-select-forward-paragraph)
  (define-key org-mode-map (kbd "C-S-<up>")    #'my/org-select-backward-paragraph))

;; Global bindings
(map!
 ;; Make escape into quit
 "<escape>"      #'doom/escape
 ;; Custom C-backspace to make it behave as expected
 "C-<backspace>" #'my/backward-delete-word
 ;; Comment line for ease of use (C-z is undo)
 "C-/"           #'comment-line
 ;; Replace C-v and M-v
 "C-,"           #'scroll-up-command
 "C-."           #'scroll-down-command
 ;; Better undo
 "C-z"           #'undo-fu-only-undo
 "C-S-z"         #'undo-fu-only-redo
 ;; Flip for Ergonomics
 "C-y"           #'consult-yank-pop
 "M-y"           #'yank)

;; Minor Mode Mods
(after! (minibuffer vertico isearch)
  (map!
   :map (minibuffer-local-map minibuffer-local-completion-map
         vertico-map isearch-mode-map)
   "C-/")
  ;; keep isearch edit handy without hijacking undo
  (map! :map isearch-mode-map "C-z" #'isearch-edit-string))


;; ORG Map
(map! :after org
      :map org-mode-map
      :prefix ("C-c d" . "my-org")
      :desc "Sentences → bullets" "b" #'org-sentences-to-bullets
      :desc "Convert region → table" "t" #'org-table-convert-region)

;;F13 as my personal leader
(defvar my/f6-map (make-sparse-keymap) "Prefix map for F13 leader.")
(define-key global-map (kbd "<f6>") my/f6-map)

;; which-key label for the prefix
(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements "<f6>" "▸ F6 leader"))

;; (TTY only) make sure F6 is recognized
(define-key input-decode-map "\e[17~" [f6])

;; bindings under the F13 prefix
;; Taken: a,b,c,d,_,f,g,_,_,_,k,_,_,m,_,_,''q,r,s,t,_,_,w_,_,_,_
;; Taken: . , '''arrows 1-9
(map! :map my/f6-map
      ;; move focus
      "<left>"  #'windmove-left
      "<right>" #'windmove-right
      "<up>"    #'windmove-up
      "<down>"  #'windmove-down

      ;; split + focus
      "S-<left>"  #'my/split-left-and-focus
      "S-<right>" #'my/split-right-and-focus
      "S-<up>"    #'my/split-up-and-focus
      "S-<down>"  #'my/split-down-and-focus

      ;; resize
      "C-<left>"  #'my/resize-pane-left
      "C-<right>" #'my/resize-pane-right
      "C-<up>"    #'my/resize-pane-up
      "C-<down>"  #'my/resize-pane-down

      ;; pop
      "C-<return>"   #'my/move-buffer-to-new-frame
      "C-S-<return>" #'my/return-buffer-to-previous-frame

      ;; General
      "q"       #'delete-window
      "S-q"     #'kill-buffer-and-window
      "C-q"     #'delete-other-windows

      "a" #'mark-whole-buffer
      "f" #'consult-line
      "b" #'consult-buffer
      "g" #'consult-ripgrep
      "d" #'mc/mark-next-like-this
      "n" #'mc/mark-all-like-this

      ;; Workspaces
      "1" #'+workspace/switch-to-0
      "2" #'+workspace/switch-to-1
      "3" #'+workspace/switch-to-2
      "4" #'+workspace/switch-to-3
      "5" #'+workspace/switch-to-4
      "6" #'+workspace/switch-to-5
      "7" #'+workspace/switch-to-6
      "8" #'+workspace/switch-to-7
      "9" #'+workspace/switch-to-8

      "c" #'+workspace/new
      "k" #'+workspace/delete

      ;; Tabs
      "t" #'tab-new
      "w" #'tab-close

      "[" #'tab-previous
      "]" #'tab-next

      )



      ;; "b" #'eval-buffer
      ;; "r" #'doom/reload

;; (map!
;;  ("M-S-<left>"  #'resize-left)
;;  ("M-S-<right>" #'resize-right)
;;  ("M-S-<up>"    #'resize-up)
;;  ("M-S-<down>"  #'resize-down))

;; (map!
;;  ("M-s <left>"  (lambda () (interactive) (split-window-horizontally) (windmove-left)))
;;  ("M-s <right>" (lambda () (interactive) (split-window-horizontally) (windmove-right)))
;;  ("M-s <up>"    (lambda () (interactive) (split-window-vertically) (windmove-up)))
;;  ("M-s <down>"  (lambda () (interactive) (split-window-vertically) (windmove-down))))


;; (global-set-key (kbd "C-<backspace>") 'my/backward-delete-word)

;; (map!
;;  ("C-<backspace>" #'my/backward-delete-word)
;;  ("<escape>" #'doom/escape))

;; ;; Copy-Cut-Paste
;; (map!
;;  ("C-S-v"       #'yank)
;;  ("C-S-c"       #'kill-ring-save)
;;  ("C-S-x"       #'kill-region))

;; ;; Extra Functionality
;; (map!
;;  ("C-S-d"   #'mc/mark-next-like-this)
;;  ("C-S-n" #'mc/mark-all-like-this))

;; (map!
;;  ("C-S-f" #'consult-line)
;;  ("C-S-b" #'consult-buffer)
;;  ("C-S-p" #'consult-ripgrep))

;; (map!
;;  ("C-/"     #'comment-line)
;;  ("C-c C-/" #'comment-line))

;; ;; Workflow Keybindings

;;  ;; ("C-M-c"   compile)
;;  ;; ("C-M-t"   shell)
;;  ;; ("C-M-b"   #'reload-init)
;; (map!
;;  ("M-q"     #'delete-window)
;;  ("C-M-q"   #'delete-other-windows)
;;  ("C-M-s"   #'my/open-persistent-scratchpad-vertical)
;;  ("C-M-S-s" #'my/open-persistent-scratchpad-horizontal))


;; ;; Text Scale Resize
;; (map!
;;   ("C-=" #'text-scale-increase)
;;   ("C--" #'text-scale-decrease)
;;   ("C-0" (lambda () (interactive) (text-scale-set 0))))

;; (map!
;;  ("<escape>" #'my/universal-escape))

;; ;; Change Pane Focus
;; (map!
;;  ("M-<left>"    #'windmove-left)
;;  ("M-<right>"   #'windmove-right)
;;  ("M-<up>"      #'windmove-up)
;;  ("M-<down>"    #'windmove-down))

;; ;; Creating Panes
;; (map!
;;  ("M-s <left>"  (lambda () (interactive) (split-window-horizontally) (windmove-left)))
;;  ("M-s <right>" (lambda () (interactive) (split-window-horizontally) (windmove-right)))
;;  ("M-s <up>"    (lambda () (interactive) (split-window-vertically) (windmove-up)))
;;  ("M-s <down>"  (lambda () (interactive) (split-window-vertically) (windmove-down))))

;; (map!
;;  ("M-S-<left>"  #'resize-left)
;;  ("M-S-<right>" #'resize-right)
;;  ("M-S-<up>"    #'resize-up)
;;  ("M-S-<down>"  #'resize-down))

;; (map!
;;  ("C-M-<return>"   #'my/move-buffer-to-new-frame)
;;  ("C-M-S-<return>" #'my/return-buffer-to-previous-frame))

;; (map!
;;  ("C-." #'tab-bar-switch-to-next-tab)
;;  ("C-," #'tab-bar-switch-to-prev-tab)
;;  ("C-t" #'tab-bar-new-tab)              ; overwrites transpose-chars
;;  ("M-t" #'tab-bar-close-tab))           ; overwrites transpose-words

;; ;; NOTE ORG MODE
;; (after! org
;;   ;; keep your Meta+Arrows for window focus
;;   (map! :map org-mode-map
;;         "M-<left>"  nil
;;         "M-<right>" nil
;;         "M-<up>"    nil
;;         "M-<down>"  nil
;;         "M-S-<left>"  #'org-promote-subtree
;;         "M-S-<right>" #'org-demote-subtree
;;         "M-S-<up>"    #'org-move-subtree-up
;;         "M-S-<down>"  #'org-move-subtree-down

;;         ;; org’s comment is handy too
;;         "C-/" #'org-comment-dwim))
