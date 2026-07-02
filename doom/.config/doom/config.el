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

(add-to-list 'custom-theme-load-path
             (expand-file-name "themes" doom-user-dir))

(defvar my/light-themes
  '(doom-one-light
    ;; doom-dracula-light-v1
    ;; doom-dracula-light-v2
    doom-dracula-light-v3
    ;; doom-invalid
    doom-flatwhite
    doom-homage-white)
  "Light themes to cycle through")

(defvar my/light-theme-index 0
  "Current index in `my/light-themes'")

(defun my/disable-themes ()
  (mapc #'disable-theme
        (bound-and-true-p custom-enabled-themes)))

(defun my/load-theme (theme)
  "Disable current themes and load THEME"
  (my/disable-themes)
  (load-theme theme t)
  (setq doom-theme theme)
  (message "Loaded theme: %s" theme))

(defun my/cycle-light-theme ()
  "Cycle through `my/light-themes'"
  (interactive)
  (let ((theme (nth my/light-theme-index my/light-themes)))
    (setq my/light-theme theme)
    (my/load-theme theme)
    (setq my/light-theme-index
          (mod (1+ my/light-theme-index)
               (length my/light-themes)))))

(setq my/dark-theme 'doom-dracula
      my/light-theme 'doom-flatwhite)

(defvar my/current-theme-mode nil)

(defun my/disable-themes ()
  (mapc #'disable-theme custom-enabled-themes))

(defun my/gsettings-dark-p ()
  "Return non-nil if GNOME/GTK prefers dark"
  (string-match-p
   "prefer-dark"
   (shell-command-to-string
    "gsettings get org.gnome.desktop.interface color-scheme 2>/dev/null")))

(defun my/apply-theme-from-gsettings ()
  "Manually apply Emacs theme based on GTK/GSettings light/dark mode"
  (interactive)
  (let ((theme (if (my/gsettings-dark-p)
                   my/dark-theme
                 my/light-theme)))
  (my/disable-themes)
  (load-theme theme t)
  (message "Loaded %s theme")))

(map!
 "C-c t g" #'my/apply-theme-from-gsettings
 "C-c t h" #'my/cycle-light-theme)


;; TEMP
(after! org
  (defface my/org-quote-delim-face
    '((t (:foreground "#A48819" :weight normal)))
    "Face for double quote delimiters in Org prose.")

  (defface my/org-quote-text-face
    '((t (:foreground "#A48819" :weight normal)))
    "Face for quoted text in Org prose.")

  (defface my/org-backtick-delim-face
    '((t (:foreground "#7A5E9E" :weight normal)))
    "Face for backtick delimiters in Org prose.")

  (defface my/org-backtick-text-face
    '((t (:foreground "#4F6478"
          :background "#ECEAF5"
          :weight normal)))
    "Face for backtick-delimited text in Org prose.")

  (font-lock-add-keywords
   'org-mode
   `(
     ;; "quoted text"
     ("\\(\"\\)\\(\\(?:\\\\.\\|[^\"\n]\\)*\\)\\(\"\\)"
      (1 'my/org-quote-delim-face nil)
      (2 'my/org-quote-text-face nil)
      (3 'my/org-quote-delim-face nil))

     ;; ``double-backtick text``
     ("\\(``\\)\\([^`\n]+\\)\\(``\\)"
      (1 'my/org-backtick-delim-face nil)
      (2 'my/org-backtick-text-face nil)
      (3 'my/org-backtick-delim-face nil))

     ;; `single-backtick text`
     ("\\(`\\)\\([^`\n]+\\)\\(`\\)"
      (1 'my/org-backtick-delim-face nil)
      (2 'my/org-backtick-text-face nil)
      (3 'my/org-backtick-delim-face nil)))
   'append))


;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; NOTE DOOM FONT
;; (setq doom-font "Spleen-8x16-12")
(setq face-font-rescale-alist nil)

(defconst my/spleen-16
  "-misc-Spleen-regular-normal-normal-*-16-*-*-*-m-*-iso10646-1")

(set-frame-font my/spleen-16 t)

(add-to-list 'default-frame-alist `(font . ,my/spleen-16))

(set-face-attribute 'fixed-pitch nil
                    :font my/spleen-16
                    :height 'unspecified)

(defun my/no-bold-faces ()
  (dolist (face (face-list))
    (set-face-attribute face nil :weight 'normal))
  (set-face-attribute 'bold nil :weight 'normal)
  (set-face-attribute 'bold-italic nil :weight 'normal))

(add-hook 'doom-load-theme-hook #'my/no-bold-faces)
(add-hook 'after-init-hook #'my/no-bold-faces)

;; ;;; Fonts (Fira Code stack)
;; (setq doom-font                (font-spec :family "FiraCode Nerd Font" :size 14)
;;       doom-variable-pitch-font (font-spec :family "FiraCode Nerd Font"          :size 16)
;;       doom-symbol-font         (font-spec :family "Symbols Nerd Font Mono"))

;; ;; Fallbacks (fonts-only path; no emojify, no unicode-fonts rewrites)
;; (when (member "Symbols Nerd Font Mono" (font-family-list))
;;   (set-fontset-font t 'symbol "Symbols Nerd Font Mono" nil 'prepend))

;; ;; Try monochrome emoji first (rock-solid); switch to Color if it behaves on your stack
;; (let ((emoji (cond
;;               ((member "Noto Emoji" (font-family-list))         "Noto Emoji")         ; monochrome, safest
;;               ((member "Noto Color Emoji" (font-family-list))   "Noto Color Emoji")   ; color, can glitch on some PGTK setups
;;               ((member "Twitter Color Emoji" (font-family-list)) "Twitter Color Emoji")
;;               (t nil))))
;;   (when emoji
;;     (set-fontset-font t 'emoji emoji nil 'prepend)))

;; (after! unicode-fonts
;;   (setq unicode-fonts-skip-font-groups '(emoji))
;;   (unicode-fonts-setup))

;; Remove CSD
;; Kill the window manager decorations (title bar, borders)
;; (add-to-list 'default-frame-alist '(undecorated . t))
;; (add-to-list 'initial-frame-alist '(undecorated . t))

;; ;; If you use emacsclient/daemon, make new frames match too
;; (add-hook 'after-make-frame-functions
;;           (lambda (f) (set-frame-parameter f 'undecorated t)))

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
;; (use-package! cua-base
;;   :init
;;   (setq cua-enable-cua-keys nil
;;         cua-prefix-override-inhibit-delay 0.001
;;         cua-keep-region-after-copy nil)
;;   :config
;;   (cua-mode 1))
(setq cua-enable-cua-keys nil)
(cua-selection-mode 1)

;; Remove fringe
; (set-fringe-mode 0)

;; Word wrap
(setq-default word-wrap t
              truncate-lines nil)
(global-visual-line-mode 1)

(after! adaptive-wrap
  (adaptive-wrap-prefix-mode -1))

;; Auto update
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t) ; helps for dired too


;; Doom scratch buffers
(setq doom-scratch-initial-major-mode 'lisp-interaction-mode)

;; More colors
(setq treesit-font-lock-level 4
      font-lock-maximum-decoration t)

;; Highlighting
(use-package! rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; ;; Multi-Cursor
;; (setq mc/always-run-for-all t)
;; (with-eval-after-load 'multiple-cursors
;;   (define-key mc/keymap (kbd "<return>") nil)) ; prevent newline ending mc session

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

;; (map! :map org-mode-map "C-c y" #'org-download-clipboard)

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

(after! vterm
  (add-hook! 'vterm-mode-hook
    (when (bound-and-true-p vterm-copy-mode)
      (vterm-copy-mode -1))))

;; (defun my/fix-tty-client-frames (frame)
;;   "Make TTY frames created by emacsclient look sane."
;;   (with-selected-frame frame
;;     (unless (display-graphic-p frame)
;;       ;; Force Emacs to decide dark/light correctly for this terminal.
;;       ;; Pick one if you know your terminal is dark.
;;       (set-terminal-parameter nil 'background-mode 'dark)
;;       (tty-set-up-initial-frame-faces)
;;       ;; Re-apply theme so faces aren't half-GUI, half-TTY.
;;       (when (boundp 'custom-enabled-themes)
;;         (mapc (lambda (th) (load-theme th t)) custom-enabled-themes)))))

;; (add-hook 'after-make-frame-functions #'my/fix-tty-client-frames)

(defun my/window-to-new-tab ()
  "Move the current window's buffer into a new tab and delete this window in the old tab."
  (interactive)
  (let ((buf (current-buffer)))
    (tab-bar-new-tab)
    (switch-to-buffer buf)
    (tab-bar-switch-to-prev-tab)
    (delete-window)
    (tab-bar-switch-to-next-tab)))

(defun my/footclient-here ()
  "Open footclient in `default-directory`."
  (interactive)
  (let ((dir (expand-file-name default-directory)))
    (start-process "footclient-here" nil
                   "footclient" "--working-directory" dir)))

(defun my/thunar-here ()
  "Open thunar in `default-directory`."
  (interactive)
  (let ((dir (expand-file-name default-directory)))
    (if (file-remote-p dir)
        (user-error "Cannot open remote TRAMP path: %s" dir)
    (start-process "thunar-here" nil
                   "thunar" dir))))

;; ;; KEEP THIS
;; Image Gallery
;; (after! org
;;   (require 'org-element)
;;   (require 'cl-lib)

;;   (defvar my/org-gallery-files nil)
;;   (defvar my/org-gallery-index 0)

;;   (defvar my/org-gallery-extensions
;;     '("png" "jpg" "jpeg" "gif" "webp" "bmp" "svg"))

;;   (defun my/org-gallery-image-p (file)
;;     "Return non-nil if FILE is an image."
;;     (and file
;;          (file-readable-p file)
;;          (member (downcase (or (file-name-extension file) ""))
;;                  my/org-gallery-extensions)))

;;   (defun my/org-gallery-collect-files ()
;;     "Collect image file links from the current Org buffer."
;;     (let (files)
;;       (org-element-map (org-element-parse-buffer) 'link
;;         (lambda (link)
;;           (when (string= (org-element-property :type link) "file")
;;             (let* ((raw (org-element-property :path link))
;;                    (file (expand-file-name
;;                           (org-link-unescape raw)
;;                           (file-name-directory
;;                            (or buffer-file-name default-directory)))))
;;               (when (my/org-gallery-image-p file)
;;                 (push file files))))))
;;       (nreverse files)))

;;   (defun my/org-gallery-show-current ()
;;     "Show the current gallery image."
;;     (let ((file (nth my/org-gallery-index my/org-gallery-files)))
;;       (unless file
;;         (user-error "No image at index %s" my/org-gallery-index))

;;       (find-file file)
;;       (image-mode)

;;       ;; Arrow-key slideshow behavior.
;;       (local-set-key (kbd "<right>") #'my/org-gallery-next)
;;       (local-set-key (kbd "<left>")  #'my/org-gallery-prev)
;;       (local-set-key (kbd "n")       #'my/org-gallery-next)
;;       (local-set-key (kbd "p")       #'my/org-gallery-prev)

;;       (message "Image %d/%d: %s"
;;                (1+ my/org-gallery-index)
;;                (length my/org-gallery-files)
;;                (file-name-nondirectory file))))

;;   (defun my/org-gallery-next ()
;;     "Go to next image in Org gallery."
;;     (interactive)
;;     (setq my/org-gallery-index
;;           (mod (1+ my/org-gallery-index)
;;                (length my/org-gallery-files)))
;;     (my/org-gallery-show-current))

;;   (defun my/org-gallery-prev ()
;;     "Go to previous image in Org gallery."
;;     (interactive)
;;     (setq my/org-gallery-index
;;           (mod (1- my/org-gallery-index)
;;                (length my/org-gallery-files)))
;;     (my/org-gallery-show-current))

;;   (defun my/org-gallery-open ()
;;     "Open image links in the current Org buffer as a simple gallery."
;;     (interactive)
;;     (setq my/org-gallery-files (my/org-gallery-collect-files))
;;     (setq my/org-gallery-index 0)

;;     (unless my/org-gallery-files
;;       (user-error "No image links found in this Org buffer"))

;;     (my/org-gallery-show-current))

;;   ;; Normal Emacs binding.
;;   (define-key org-mode-map (kbd "C-c i g") #'my/org-gallery-open)

;;   ;; Doom localleader binding.
;;   (map! :map org-mode-map
;;         :localleader
;;         :desc "Open simple image gallery"
;;         "i g" #'my/org-gallery-open))


;;; MISC
(defun my/replace-all-newlines-in-buffer ()
(interactive)
(save-excursion
  (save-restriction
    (widen)
    (replace-string "\\n" "\n" nil (point-min) (point-max)))))

; (define-key (kbd "C-c d n") #'my/replace-all-newlines-in-buffer)

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
 ;;C-/"           #'comment-line
 ;; Replace C-v and M-v
 ;; "C-,"           #'scroll-up-command
 ;; "C-."           #'scroll-down-command
 ;; Better undo
 ;; "C-z"           #'undo-fu-only-undo
 ;; "C-S-z"         #'undo-fu-only-redo
 ;; Flip for Ergonomics
 ;; "C-y"           #'consult-yank-pop
 ;; "M-y"           #'yank
)

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

;; Winmove Map
;; (map! :after org
;;       :map winmove-mode-map
;;       :prefix ("C-c z" . "my-move")
;;       :desc "Swap window left" "S-<left>" #'windmove-swap-states-left
;;       :desc "Swap window right" "S-<right>" #'windmove-swap-states-right
;;       :desc "Swap window up" "S-<up>" #'windmove-swap-states-up
;;       :desc "Swap window down" "S-<down>" #'windmove-swap-states-down
;;       )

;;F13 as my personal leader
;; Make C-z a real prefix command
(define-prefix-command 'my/f5-map)
;; High-precedence global override
(defvar my/leader-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-z") my/f5-map)
    map))

(define-minor-mode my/leader-mode
  "Force C-z to be my leader key."
  :global t
  :init-value t
  :keymap my/leader-mode-map)

;; Also kill the other suspend binding while we're at it
(global-set-key (kbd "C-x C-z") #'ignore)


;; Your leader bindings live here
(let ((m my/f5-map))
  (define-key m (kbd "<left>")  #'windmove-left)
  (define-key m (kbd "<right>") #'windmove-right)
  (define-key m (kbd "<up>")    #'windmove-up)
  (define-key m (kbd "<down>")  #'windmove-down)

  (define-key m (kbd "S-<left>")  #'my/split-left-and-focus)
  (define-key m (kbd "S-<right>") #'my/split-right-and-focus)
  (define-key m (kbd "S-<up>")    #'my/split-up-and-focus)
  (define-key m (kbd "S-<down>")  #'my/split-down-and-focus)

  (define-key m (kbd "C-<left>")  #'my/resize-pane-left)
  (define-key m (kbd "C-<right>") #'my/resize-pane-right)
  (define-key m (kbd "C-<up>")    #'my/resize-pane-up)
  (define-key m (kbd "C-<down>")  #'my/resize-pane-down)

  (define-key m (kbd "C-<return>")   #'my/move-buffer-to-new-frame)
  (define-key m (kbd "C-S-<return>") #'my/return-buffer-to-previous-frame)

  (define-key m (kbd "q")   #'delete-window)
  (define-key m (kbd "S-q") #'kill-buffer-and-window)
  (define-key m (kbd "C-q") #'delete-other-windows)

  (define-key m (kbd "a") #'mark-whole-buffer)
  (define-key m (kbd "b") #'consult-buffer)
  (define-key m (kbd "f") #'consult-line)
  (define-key m (kbd "g") #'consult-ripgrep)
  (define-key m (kbd "d") #'mc/mark-next-like-this)
  (define-key m (kbd "n") #'mc/mark-all-like-this)
  (define-key m (kbd "v") #'org-download-clipboard)
  (define-key m (kbd "/") #'comment-line)

  (define-key m (kbd "1") #'+workspace/switch-to-0)
  (define-key m (kbd "2") #'+workspace/switch-to-1)
  (define-key m (kbd "3") #'+workspace/switch-to-2)
  (define-key m (kbd "4") #'+workspace/switch-to-3)
  (define-key m (kbd "5") #'+workspace/switch-to-4)
  (define-key m (kbd "6") #'+workspace/switch-to-5)
  (define-key m (kbd "7") #'+workspace/switch-to-6)
  (define-key m (kbd "8") #'+workspace/switch-to-7)
  (define-key m (kbd "9") #'+workspace/switch-to-8)

  (define-key m (kbd "c") #'+workspace/new)
  (define-key m (kbd "k") #'+workspace/delete)

  (define-key m (kbd "t") #'tab-new)
  (define-key m (kbd "w") #'tab-close)
  (define-key m (kbd "[") #'tab-previous)
  (define-key m (kbd "]") #'tab-next))



;; (defvar my/f5-map (make-sparse-keymap) "Prefix map for F13 leader.")
;; (define-key global-map (kbd "C-z") my/f5-map)

;; ;; which-key label for the prefix
;; (with-eval-after-load 'which-key
;;   (which-key-add-key-based-replacements "<f5>" "▸ F5 leader"))

;; ;; (TTY only) make sure F6 is recognized
;; (define-key input-decode-map "\e[16~" [f5])

;; bindings under the F13 prefix
;; Taken: a,b,c,d,_,f,g,_,_,_,k,_,_,m,_,_,''q,r,s,t,_,_,w_,_,_,_
;; Taken: . , '''arrows 1-9
;; (global-unset-key (kbd "C-z"))
;; (after! doom-keybinds
;;   (map! :map doom-override-mode-map "C-z" nil)
;;   (map! :map my/shortcuts-map
;;         :prefix ("C-z" . "frame")
;;         ;; move focus
;;         "<left>"  #'windmove-left
;;         "<right>" #'windmove-right
;;         "<up>"    #'windmove-up
;;         "<down>"  #'windmove-down

;;         ;; split + focus
;;         "S-<left>"  #'my/split-left-and-focus
;;         "S-<right>" #'my/split-right-and-focus
;;         "S-<up>"    #'my/split-up-and-focus
;;         "S-<down>"  #'my/split-down-and-focus

;;         ;; resize
;;         "C-<left>"  #'my/resize-pane-left
;;         "C-<right>" #'my/resize-pane-right
;;         "C-<up>"    #'my/resize-pane-up
;;         "C-<down>"  #'my/resize-pane-down

;;         ;; pop
;;         "C-<return>"   #'my/move-buffer-to-new-frame
;;         "C-S-<return>" #'my/return-buffer-to-previous-frame

;;         ;; General
;;         "q"       #'delete-window
;;         "S-q"     #'kill-buffer-and-window
;;         "C-q"     #'delete-other-windows

;;         "a" #'mark-whole-buffer
;;         "b" #'consult-buffer
;;         "f" #'consult-line
;;         "g" #'consult-ripgrep
;;         "d" #'mc/mark-next-like-this
;;         "n" #'mc/mark-all-like-this
;;         "v" #'org-download-clipboard

;;         "/" #'comment-line

;;         ;; Workspaces
;;         "1" #'+workspace/switch-to-0
;;         "2" #'+workspace/switch-to-1
;;         "3" #'+workspace/switch-to-2
;;         "4" #'+workspace/switch-to-3
;;         "5" #'+workspace/switch-to-4
;;         "6" #'+workspace/switch-to-5
;;         "7" #'+workspace/switch-to-6
;;         "8" #'+workspace/switch-to-7
;;         "9" #'+workspace/switch-to-8

;;         "c" #'+workspace/new
;;         "k" #'+workspace/delete

;;         ;; Tabs
;;         "t" #'tab-new
;;         "w" #'tab-close

;;         "[" #'tab-previous
;;         "]" #'tab-next

;;         ))

;; (use-package! odin-mode
;;   :mode ("\\.odin\\'" . odin-mode))
      ;; "b" #'eval-buffer
      ;; "r" #'doom/reload

;; (map!
;;  ("C-x @ h a" #'mark-whole-buffer))

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
