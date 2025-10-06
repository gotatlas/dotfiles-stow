;; Globals
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(setq display-line-numbers-type t)
(global-display-line-numbers-mode t)
(global-visual-line-mode 1)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq tab-always-indent 'complete)

(electric-pair-mode 1) 
(delete-selection-mode 1)

;; Smooth Scrolling
(setq scroll-margin 5
      scroll-conservatively 9999
      scroll-step 1)

(global-auto-revert-mode t)

(setq inhibit-startup-screen t)

(setq initial-buffer-choice nil) ;; was t

;(defalias 'yes-or-no-p (lambda (&rest args) t))
;(defalias 'y-or-n-p (lambda (&rest args) t))

(fset 'yes-or-no-p 'y-or-n-p)

;; Auto-create parent dirs
(add-hook 'before-save-hook
          (lambda ()
            (when buffer-file-name
              (let ((dir (file-name-directory buffer-file-name)))
                (unless (file-exists-p dir)
                  (make-directory dir t))))))

;; Set Hyperkey if not done for system
;; (define-key key-translation-map (kbd "<Multi_key>") (kbd "H-"))
;; (define-key key-translation-map (kbd "<XF86CapsLock>") 'event-apply-hyper-modifier)

;; (global-set-key (kbd "C-c h") #'event-apply-hyper-modifier) ; C-c h f => H-f
;; Wayland Hyperkey
;; (define-key key-translation-map (kbd "<caps>") 'event-apply-hyper-modifier)
;; (define-key input-decode-map (kbd "<Hyper_L>") (kbd "H-"))
;; (define-key input-decode-map (kbd "<hyper>") (kbd "H-"))

;; Rebind Macro
(defmacro rebind (&rest bindings)
  `(progn
     ,@(cl-loop for (key cmd) in bindings
                collect `(global-set-key (kbd ,key)
                          ,(if (symbolp cmd)
                               `#',cmd
                             cmd)))))

(defmacro rebind-in (map &rest bindings)
  `(progn
     ,@(cl-loop for (key cmd) in bindings
                collect `(define-key ,map (kbd ,key) #',cmd))))

;; Unsets
(global-unset-key (kbd "C-z"))

;; Package setup
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Installer Helper
(defun require-package (package)
  "Install PACKAGE unless already installed."
  (unless (package-installed-p package)
    (unless package-archive-contents
      (package-refresh-contents))
    (package-install package)))

;; Managing autosaves and backupfiles
;; Put all backup files in ~/.emacs.d/backups/<full/path/to/file>
(setq backup-directory-alist
      `(("." . ,(expand-file-name "backups/" user-emacs-directory))))

;; Use versioned backups (foo.txt~, foo.txt~~, …)
(setq version-control t)
(setq kept-new-versions 10)
(setq kept-old-versions 2)
(setq delete-old-versions t)

;; Put all auto-saves in ~/.emacs.d/auto-save-list/<full/path/to/file>
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "auto-save-list/" user-emacs-directory) t)))

;; Highlighting
(require-package 'highlight-defined)
(require-package 'rainbow-delimiters)
(autoload 'highlight-defined-mode "highlight-defined" nil t)
(autoload 'rainbow-delimiters-mode "rainbow-delimiters" nil t)

(when (require 'highlight-defined nil t)
  (add-hook 'prog-mode-hook #'highlight-defined-mode))

(when (require 'rainbow-delimiters nil t)
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; NEW CODE
(setq initial-major-mode 'lisp-interaction-mode
      initial-scratch-message "")

(defun my/create-persistent-scratchpad ()
  "Create the persistent scratchpad buffer."
  (let ((file (expand-file-name ".persistent-scratchpad" user-emacs-directory)))
    ;; Create file if it doesn't exist
    (unless (file-exists-p file)
      (with-temp-file file ""))
    ;; Create and setup buffer
    (with-current-buffer (get-buffer-create "*persistent-scratchpad*")
      (org-mode)
      (when (> (nth 7 (file-attributes file)) 0)
        (insert-file-contents file))
      (current-buffer))))

;; Auto-save periodically and on exit
(defun my/save-persistent-scratchpad ()
  "Save the persistent scratchpad."
  (when (get-buffer "*persistent-scratchpad*")
    (with-current-buffer "*persistent-scratchpad*"
      (write-region (point-min) (point-max) 
                    (expand-file-name ".persistent-scratchpad" user-emacs-directory)
                    nil 'silent))))

(add-hook 'emacs-startup-hook #'my/create-persistent-scratchpad)
(add-hook 'kill-emacs-hook #'my/save-persistent-scratchpad)
(run-with-timer 300 300 #'my/save-persistent-scratchpad) ; Auto-save every 5 minutes

(defun my/open-persistent-scratchpad-vertical ()
  "Toggle a right split window showing *persistent-scratchpad*."
  (interactive)
  (let ((win (get-buffer-window "*persistent-scratchpad*")))
      (if win
          (delete-window win)
        (select-window (split-window-right))
        (switch-to-buffer "*persistent-scratchpad*"))))

(defun my/open-persistent-scratchpad-horizontal ()
  "Toggle a bottom split window showing *persistent-scratchpad*."
  (interactive)
  (let ((win (get-buffer-window "*persistent-scratchpad*")))
    (if win
        (delete-window win)
      (select-window (split-window-below))
      (switch-to-buffer "*persistent-scratchpad*"))))

(defun my/ensure-persistent-scratchpad ()
  "Recreate persistent scratchpad if it was killed."
  (unless (get-buffer "*persistent-scratchpad*")
    (my/create-persistent-scratchpad)))

(run-with-idle-timer 60 t #'my/ensure-persistent-scratchpad)

;; Editor Keybindings
(rebind
 ("C-S-v" yank)
 ("C-S-c" kill-ring-save)
 ("C-S-x" kill-region))

(global-set-key (kbd "C-/") #'comment-line)

;; Workflow Keybindings
(rebind
 ("H-c"   compile)
 ("H-t"   shell)
 ("H-q"   delete-window)
 ("H-b"   my/eval-buffer-with-message)
 ("H-C-q" delete-other-windows)
 ("H-s"   my/open-persistent-scratchpad-vertical)
 ("H-C-s" my/open-persistent-scratchpad-horizontal))

;; Custom Editor Keybindings
(defun my/backward-delete-word ()
  "Delete word backward without adding to kill ring."
  (interactive)
  (delete-region (point) (save-excursion (backward-word) (point))))

(global-set-key (kbd "C-<backspace>") 'my/backward-delete-word)

;; Multiple Cursors
(require-package 'multiple-cursors)
(require 'multiple-cursors)

(add-hook 'org-mode-hook
  (lambda ()
    (when (featurep 'multiple-cursors-core)
      (multiple-cursors-mode 1))))

(setq mc/always-repeat-command t
      mc/always-run-for-all t)

;; Overwrites default delete
(rebind
 ("C-d"   mc/mark-next-like-this)
 ("C-S-d" mc/mark-all-like-this)
 ("C-M-d" mc/repeat-command))
;; mc/edit-lines & mc/mark-previous-like-this

;; Consult
(require-package 'consult)
(require 'consult)

(rebind
 ("C-f" consult-line)
 ("C-b" consult-buffer)
 ("C-p" consult-ripgrep)
 ("C-y" consult-yank-pop)
 ("M-y" yank))
;;consult-find, consult-grep, consult-ripgrep

;; Undo/Redo
(require-package 'undo-fu)
(require 'undo-fu)

(rebind
 ("C-z"   undo-fu-only-undo)
 ("C-S-z" undo-fu-only-redo)
 ("M-z"   undo-fu-only-redo))
;; removed zap-to-char

;; Transpose
(rebind
  ("M-<up>"    (lambda () (interactive) (let ((col (current-column))) (transpose-lines 1) (forward-line -2) (move-to-column col))))
  ("M-<down>"  (lambda () (interactive) (forward-line 1) (transpose-lines 1) (forward-line -1)))
  ("M-H-<left>"  (lambda () (interactive) (transpose-words -1)))
  ("M-H-<right>" transpose-words))

;; End/Start of Line 
;(global-unset-key (kbd "M-S-<up>"))
;(global-unset-key (kbd "M-S-<down>"))

(rebind
 ("M-<left>"  move-beginning-of-line)
 ("M-<right>" move-end-of-line))

;; Text Scale Resize
(rebind
  ("C-=" text-scale-increase)
  ("C--" text-scale-decrease)
  ("C-0" (lambda () (interactive) (text-scale-set 0))))

;; Universal Escape
(defun my/universal-escape ()
  (interactive)
  (when (bound-and-true-p multiple-cursors-mode) (mc/keyboard-quit))
  (when (use-region-p) (deactivate-mark))
  (when (and (boundp 'isearch-mode) isearch-mode) (isearch-exit))
  (when (minibufferp) (keyboard-escape-quit))
  (keyboard-quit))

(rebind
 ("<escape>" my/universal-escape))

;; Misc
(defun my/eval-buffer-with-message ()
  "Eval buffer and show a message."
  (interactive)
  (eval-buffer)
  (message "Buffer evaluated."))

;; Keep warnings at the bottom
(setq display-buffer-alist
      '(("\\*Warnings\\*"
         (display-buffer-reuse-window display-buffer-at-bottom)
         (window-height . 0.2))))

;; Dired
(require 'dired-x)

(setq dired-create-destination-dirs 'always)

;; See fresh files
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

(add-hook 'dired-mode-hook 'auto-revert-mode)

;; Omit temp/backup files
(setq dired-omit-files (concat dired-omit-files "\\|^#.*#$\\|^\\..*\\.swp$\\|~$"))
(setq dired-omit-verbose nil)
(add-hook 'dired-mode-hook #'dired-omit-mode)

;; Magit
(require-package 'magit)
(require 'magit)

(defun magit-dotfiles ()
  "Open magit-status for the dotfiles bare repo."
  (interactive)
  (let ((default-directory (expand-file-name "~/"))
        (process-environment (cons (concat "GIT_DIR=" (expand-file-name "~/.dotfiles"))
                                   (cons (concat "GIT_WORK_TREE=" (expand-file-name "~"))
                                         process-environment))))
    (magit-status default-directory)))

(global-set-key (kbd "C-c d") #'magit-dotfiles)

;; Updated Search/Autocomplete
;; VERTICO
(require-package 'vertico)
(require 'vertico)
(vertico-mode 1)

;; ORDERLESS
(require-package 'orderless)
(require 'orderless)
(setq completion-styles '(orderless)
      completion-category-defaults nil
      completion-category-overrides '((file (styles partial-completion))))

;; MARGINALIA
(require-package 'marginalia)
(require 'marginalia)
(marginalia-mode 1)
(setq marginalia-annotators
      '(marginalia-annotators-heavy marginalia-annotators-light nil))

;; CORFU
;; Corfu completion setup (good defaults)
(require-package 'corfu)
(require 'corfu)
(setq corfu-auto t
      corfu-cycle t
      corfu-preselect 'prompt
      corfu-quit-at-boundary t
      corfu-quit-no-match t)
(global-corfu-mode 1)

;; Org
(require 'url-handlers)  ; needed for org-download for some reason
(require-package 'org-modern)
(require-package 'org-download)

(autoload 'org-modern-mode "org-modern" nil t)
(autoload 'org-download-clipboard "org-download" nil t)
(autoload 'org-download-enable   "org-download" nil t)

(setq shift-select-mode t)
(setq org-adapt-indentation nil)
(setq org-use-sub-superscripts nil)

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-S-<up>") nil)
  (define-key org-mode-map (kbd "C-S-<down>") nil))

;; Org modern visuals
(setq org-hide-leading-stars t
      org-pretty-entities t
      org-support-shift-select t
      org-M-RET-may-split-line nil
      org-list-indent-offset 2)

(add-hook 'org-mode-hook #'org-indent-mode)

(when (require 'org-modern nil 'noerror)
  (setq org-modern-star '("◉" "○" "✿" "◆" "▶"))
  (setq org-modern-list '((?- . "•") (?+ . "‣") (?* . "•")))
  (add-hook 'org-mode-hook #'org-modern-mode))

(add-hook 'org-mode-hook #'org-display-inline-images)

;; Org mode customizations
(defface doom-org-mention-face
  '((t :foreground "#51afef" :weight bold))
  "Doom Emacs blue for @mentions.")

(defface doom-org-hashtag-face
  '((t :foreground "#c678dd" :weight bold))
  "Doom Emacs magenta for #hashtags.")

(font-lock-add-keywords
 'org-mode
 '(("@\\w+"  . 'doom-org-mention-face)
   ("#\\w+"  . 'doom-org-hashtag-face)))

(defface org-inline-code-backtick
  '((t :foreground "#8aff80" :weight semi-bold))
  "Face for text wrapped in backticks in org-mode.")

(defun my/org-fontify-backticks ()
  (font-lock-add-keywords nil
   '(("\\(`[^`\n]+`\\)"
      1 'org-inline-code-backtick prepend))))

(add-hook 'org-mode-hook #'my/org-fontify-backticks)

;; Org download for image pasting
(when (require 'org-download nil 'noerror)
  (add-hook 'org-mode-hook #'org-download-enable)
  (add-hook 'dired-mode-hook #'org-download-enable)
  (setq org-download-image-dir "./images"))

(defun my/org-download-clipboard-if-image ()
  "Paste clipboard image into org if one exists, else do nothing."
  (interactive)
  (if (image-type-from-data (gui-get-selection 'CLIPBOARD 'image/png))
      (call-interactively #'org-download-clipboard)
    (message "No image data in clipboard.")))

(rebind
 ("H-v" my/org-download-clipboard-if-image))

;; Org Keybinds
(defun my/org-continue-list-or-paragraph ()
  "Continue Org list or paragraph at point, bringing rest of line to new item/line below."
  (interactive)
  (let* ((rest-of-line (buffer-substring-no-properties (point) (line-end-position)))
         (in-list (org-at-item-p)))
    ;; Split the line at point, move the rest down
    (delete-region (point) (line-end-position))
    (end-of-line)
    (insert "\n")
    (when in-list
      (insert (org-list-bullet-string)))
    (insert (string-trim-left rest-of-line))
    (when (looking-at "[ \t]*$")
      (end-of-line))))

(defun my/org-paragraph-to-list (beg end)
  "Turn sentences in region into list items."
  (interactive "r")
  (let ((text (buffer-substring-no-properties beg end)))
    (delete-region beg end)
    (dolist (sentence (split-string text "[.!?]" t "[ \t\n]+"))
      (insert "- " sentence ".\n"))))

(defun my/org-tree-to-indirect-buffer ()
  "Clone current Org subtree to indirect buffer in the SAME window."
  (interactive)
  (let ((buffer (org-tree-to-indirect-buffer)))
    (when buffer
      (switch-to-buffer buffer))))


(define-key org-mode-map (kbd "C-M-<return>") #'my/org-continue-list-or-paragraph)
(define-key org-mode-map (kbd "C-c o") #'my/org-paragraph-to-list)

;; Org keybindings
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-M-<left>") 'backward-sentence)
  (define-key org-mode-map (kbd "C-M-<right>") 'forward-sentence))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c i") 'my/org-tree-to-indirect-buffer))

;; Fixing Tab
(setq org-adapt-indentation t)

(defun my/org-smart-tab ()
  "Smart tab behavior: indent or cycle."
  (interactive)
  (if (or (org-at-table-p)
          (org-in-src-block-p)
          (org-in-item-p))
      (indent-for-tab-command)
    (org-cycle)))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "TAB") #'my/org-smart-tab))

(require 'org-id)
;; Prefer CUSTOM_ID when storing links; else create an ID automatically
(setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

;; ;; Reset Org
;; ;; --- Winner: keep the defaults when winner-mode is enabled
;; (with-eval-after-load 'winner
;;   ;; Defaults are C-c <left>/<right> ; ensure we didn't shadow them
;;   (define-key global-map (kbd "C-c <left>")  #'winner-undo)
;;   (define-key global-map (kbd "C-c <right>") #'winner-redo))

;; ;; --- Org: put back the well-known keys we might've overridden
;; (with-eval-after-load 'org
;;   (define-key org-mode-map (kbd "C-c l")   #'org-store-link)
;;   (define-key org-mode-map (kbd "C-c C-s") #'org-schedule)
;;   (define-key org-mode-map (kbd "C-c /")   #'org-sparse-tree)
;;   (define-key org-mode-map (kbd "C-c .")   #'org-time-stamp)
;;   (define-key org-mode-map (kbd "C-c ,")   #'org-time-stamp-inactive)
;;   (define-key org-mode-map (kbd "C-c C-a") #'org-attach))


;; START OF WORKSPACE LOGIC
;; START OF PERSPECTIVE


;; PERSPECTIVE - Minimal, reliable workspace setup

(require-package 'persp-mode)
(require 'persp-mode)

;; Turn OFF automatic buffer adding - we control everything manually
(setq persp-autokill-buffer-on-remove 'kill-weak)
(setq persp-add-buffer-on-find-file nil)  ; KEY: Don't auto-add
(setq persp-add-buffer-on-after-change-major-mode nil)  ; KEY: Don't auto-add
(setq persp-auto-save-opt 0)
(setq persp-auto-resume-time -1)
(setq persp-set-last-persp-for-new-frames nil)
(setq persp-reset-windows-on-nil-window-conf nil)

;; Enable persp-mode
(persp-mode 1)

;; Create T1 and switch to it - but DON'T force any buffer
(persp-add-new "T1")
(persp-switch "T1")

;; Mode line - shows all workspaces
(defun my/persp-mode-line ()
  "Show workspace numbers in mode line."
  (let* ((persps (hash-table-keys *persp-hash*))
         (sorted (sort (cl-remove-if-not 
                       (lambda (n) (string-match "^T[1-9]$" n)) 
                       persps)
                      (lambda (a b) (< (string-to-number (substring a 1))
                                      (string-to-number (substring b 1))))))
         (current (safe-persp-name (get-current-persp))))
    (if sorted
        (concat "["
                (mapconcat (lambda (name)
                            (let ((num (substring name 1)))
                              (if (string= name current)
                                  (propertize num 'face 'mode-line-emphasis)
                                num)))
                          sorted ", ")
                "] ")
      "")))

;; Clean existing mode line entries
(setq-default mode-line-format
              (cl-remove-if (lambda (item)
                             (and (listp item)
                                  (eq (car item) :eval)
                                  (listp (cadr item))
                                  (eq (car (cadr item)) 'my/persp-mode-line)))
                           mode-line-format))

;; Add to mode line
(setq-default mode-line-format
              (cons '(:eval (my/persp-mode-line))
                    mode-line-format))

;; Simple switch - create if doesn't exist
(defun my/switch-workspace (num)
  "Switch to workspace NUM (1-9)."
  (let ((persp-name (format "T%d" num)))
    (unless (gethash persp-name *persp-hash*)
      (persp-add-new persp-name))
    (persp-switch persp-name)))

;; Kill workspace
(defun my/kill-workspace (num)
  "Kill workspace NUM and recreate it empty."
  (interactive "nWorkspace to kill (1-9): ")
  (let* ((persp-name (format "T%d" num))
         (current-persp (safe-persp-name (get-current-persp))))
    
    (when (string= current-persp persp-name)
      (user-error "Can't kill current workspace. Switch first."))
    
    (when (gethash persp-name *persp-hash*)
      (persp-kill persp-name))
    
    (persp-add-new persp-name)))

;; Keybindings
(dotimes (i 9)
  (let ((n (1+ i)))
    (global-set-key (kbd (format "H-%d" n))
                    `(lambda () (interactive)
                       (my/switch-workspace ,n)))))

(global-set-key (kbd "H-k") #'my/kill-workspace)

;; Auto-cleanup killed buffers
(defun my/persp-kill-buffer-cleanup ()
  "Remove killed buffers from current perspective."
  (when (and (bound-and-true-p persp-mode)
             (get-current-persp))
    (persp-remove-buffer (current-buffer) (get-current-persp) nil)))

(add-hook 'kill-buffer-hook #'my/persp-kill-buffer-cleanup)

;; Smart quit
(global-set-key (kbd "C-c q")
  (lambda () (interactive)
    (let ((buf (current-buffer)))
      (cond
       ((cdr (window-list))
        (delete-window)
        (unless (get-buffer-window buf 'visible)
          (persp-remove-buffer buf)
          (kill-buffer buf)))
       
       (t
        (when (yes-or-no-p "Last window. Kill workspace?")
          (let* ((current-persp-name (safe-persp-name (get-current-persp)))
                 (workspace-num (when (string-match "^T\\([1-9]\\)$" current-persp-name)
                                 (string-to-number (match-string 1 current-persp-name)))))
            (if workspace-num
                (progn
                  (my/switch-workspace (if (= workspace-num 1) 2 1))
                  (my/kill-workspace workspace-num))
              (kill-buffer buf)))))))))

;; Perspective-scoped buffer switching
(defun my/consult-buffer-persp ()
  "Switch buffer within current perspective."
  (interactive)
  (if (and (bound-and-true-p persp-mode) (get-current-persp))
      (let* ((persp-buffers (persp-buffers (get-current-persp))))
        (if persp-buffers
            (switch-to-buffer 
             (completing-read "Buffer: " 
                             (mapcar #'buffer-name persp-buffers)))
          (message "No buffers in perspective")))
    (switch-to-buffer (completing-read "Buffer: " 
                                       (mapcar #'buffer-name (buffer-list))))))

(global-set-key (kbd "C-x b") #'my/consult-buffer-persp)

;; Helper: manually add current buffer to perspective
(defun my/add-buffer-to-persp ()
  "Manually add current buffer to current perspective."
  (interactive)
  (when (get-current-persp)
    (persp-add-buffer (current-buffer))
    (message "Added %s to %s" (buffer-name) (safe-persp-name (get-current-persp)))))

(global-set-key (kbd "H-a") #'my/add-buffer-to-persp)

;; END OF PERSPECTIVE
;; START OF TABS

;; Alternative: tab-bar-mode for global tabs
(tab-bar-mode 1)
;; (setq tab-bar-close-button-show nil)
;; (setq tab-bar-new-button-show nil)
 
;; Global tab keybindings (if using tab-bar instead)
(global-set-key (kbd "H-.") 'tab-bar-switch-to-next-tab)
(global-set-key (kbd "H-,") 'tab-bar-switch-to-prev-tab)
(global-set-key (kbd "H-t") 'tab-bar-new-tab)
(global-set-key (kbd "H-w") 'tab-bar-close-tab)

;; Terminal
(require-package 'vterm)
(require 'vterm)

(defun my/open-vterm-for-perspective ()
  "Open or switch to a vterm buffer tied to the current perspective, in a bottom split."
  (interactive)
  (let* ((persp-name (if (fboundp 'safe-persp-name)
                         (safe-persp-name (get-current-persp))
                       "default"))
         (buf-name (format "*vterm: %s*" persp-name))
         (existing (get-buffer buf-name)))
    (if existing
        (pop-to-buffer buf-name)
      (let ((vterm-buf (generate-new-buffer "*vterm-tmp*"))
            (win (split-window (selected-window) (- (window-height) 15) 'below)))
        (select-window win)
        (vterm vterm-buf)
        (rename-buffer buf-name)))))

(defun my/kill-vterm-for-perspective ()
  "Silently kill the vterm buffer for the current perspective and close its window."
  (interactive)
  (let* ((persp-name (if (fboundp 'safe-persp-name)
                         (safe-persp-name (get-current-persp))
                       "default"))
         (buf-name (format "*vterm: %s*" persp-name))
         (buf (get-buffer buf-name)))
    (when buf
      (with-current-buffer buf
        (let ((proc (get-buffer-process buf)))
          (when proc
            (set-process-query-on-exit-flag proc nil)
            (kill-process proc))))
      (when-let ((win (get-buffer-window buf)))
        (delete-window win))
      (kill-buffer buf))))

(defun my/toggle-vterm-for-perspective ()
  "Toggle the vterm buffer for the current perspective.
Show it in a bottom split if hidden, hide if visible."
  (interactive)
  (let* ((persp-name (if (fboundp 'safe-persp-name)
                         (safe-persp-name (get-current-persp))
                       "default"))
         (buf-name (format "*vterm: %s*" persp-name))
         (buf (get-buffer buf-name))
         (win (get-buffer-window buf)))
    (cond
     (win
      (delete-window win))
     (buf
      (let ((win (split-window (selected-window) (- (window-height) 15) 'below)))
        (set-window-buffer win buf)
        (select-window win)))
     (t
      (let ((vterm-buf (generate-new-buffer "*vterm-tmp*"))
            (win (split-window (selected-window) (- (window-height) 15) 'below)))
        (select-window win)
        (vterm vterm-buf)
        (rename-buffer buf-name))))))

(rebind
  ("H-<return>"     my/open-vterm-for-perspective)
  ("H-S-<return>"     my/toggle-vterm-for-perspective)
  ("H-C-<return>"   my/kill-vterm-for-perspective))

;; END OF WORKSPACE LOGIC

;; Tiling
(require-package 'windmove)

(windmove-default-keybindings 'hyper)

;; Winner Mode
;; NEW
(require 'winner)
(winner-mode 1)

;; Undo/Redo Window Changes
(rebind
 ("H-z" winner-undo)
 ("H-r" winner-redo)) ;maybe make z & S-z/Z

;; Change Pane Focus
(rebind
 ("H-<left>" windmove-left)
 ("H-<right>" windmove-right)
 ("H-<up>" windmove-up)
 ("H-<down>" windmove-down))

;; Creating Panes 
(rebind
 ("H-S-<left>"  (lambda () (interactive) (split-window-horizontally) (windmove-left)))
 ("H-S-<right>" (lambda () (interactive) (split-window-horizontally) (windmove-right)))
 ("H-S-<up>"    (lambda () (interactive) (split-window-vertically) (windmove-up)))
 ("H-S-<down>"  (lambda () (interactive) (split-window-vertically) (windmove-down))))

;; Resizing Panes
(defun smart-resize ()
  (interactive)
  (let* ((key (key-description (this-command-keys-vector)))
         (direction
          (cond
           ((string-match "<left>" key)  'left)
           ((string-match "<right>" key) 'right)
           ((string-match "<up>" key)    'up)
           ((string-match "<down>" key)  'down)
           (t (user-error "Can't determine direction from: %s" key)))))
    (pcase direction
      ('left  (if (window-in-direction 'left)
                  (enlarge-window-horizontally 5)
                (shrink-window-horizontally 5)))
      ('right (if (window-in-direction 'right)
                  (enlarge-window-horizontally 5)
                (shrink-window-horizontally 5)))
      ('up    (if (window-in-direction 'above)
                  (enlarge-window 5)
                (shrink-window 5)))
      ('down  (if (window-in-direction 'below)
                  (enlarge-window 5)
                (shrink-window 5))))))

(rebind
 ("H-C-<left>" smart-resize)
 ("H-C-<right>" smart-resize)
 ("H-C-<up>" smart-resize)
 ("H-C-<down>" smart-resize))

;; Moving Panes
(defvar my-unmovable-modes '(eat-mode vterm-mode term-mode))

(defun move-window (dir)
  (let ((target (window-in-direction dir)))
    (if (and target
             (not (memq major-mode my-unmovable-modes))
             (not (memq (buffer-local-value 'major-mode (window-buffer target))
                        my-unmovable-modes)))
        (let ((this-buffer (current-buffer)))
          (set-window-buffer target this-buffer)
          (let ((prev (other-buffer)))
            (when (memq prev (persp-buffer-list))
              (switch-to-buffer prev))))
      (message "Cannot move buffer (%s) or no window in that direction" major-mode))))

(rebind
      ("H-C-S-<left>"  (lambda () (interactive) (move-window 'left)))
      ("H-C-S-<right>" (lambda () (interactive) (move-window 'right)))
      ("H-C-S-<up>"    (lambda () (interactive) (move-window 'up)))
      ("H-C-S-<down>"  (lambda () (interactive) (move-window 'down))))

;; Pop Pane to New Window
(defvar pop-out-window-configuration nil)
(defvar pop-out-original-frame nil)

(defun move-buffer-to-new-frame ()
  (interactive)
  (setq pop-out-window-configuration (current-window-configuration))
  (setq pop-out-original-frame (selected-frame))
  (let ((buf (current-buffer)))
    (when (not (one-window-p))
      (delete-window))
    (select-frame (make-frame))
    (switch-to-buffer buf)))

(defun return-buffer-to-previous-frame ()
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

(rebind
 ("H-M-<return>"   move-buffer-to-new-frame)
 ("H-M-S-<return>" return-buffer-to-previous-frame))

;; Install and test Edwina
;; (require-package 'edwina)

;; (require 'edwina)
;; (setq edwina-split-default-direction 'right) ;; or 'below
;; (edwina-mode 1)

;; (setcdr edwina-mode-map nil)
;; (rebind
;;            ("H-<right>"   edwina-select-next-window)
;;            ("H-<left>"    edwina-select-previous-window)
;;            ("H-<down>"    edwina-select-next-window)
;;            ("H-<up>"      edwina-select-previous-window)
;;            ("H-q"         edwina-delete-window)
;;            ("H-C-<right>" edwina-inc-mfact)
;;            ("H-C-<left>"  edwina-dec-mfact)
;;            ("H-M-<right>" edwina-swap-next-window)
;;            ("H-M-<left>"  edwina-swap-previous-window)
;;            ("H-M-<up>"    edwina-swap-previous-window)
;;            ("H-M-<down>"  edwina-swap-next-window)          
;;            ("H-<return>"  edwina-clone-window t)
;;            ("H-f"         edwina-zoom)
;;            ("H-C-<up>"    edwina-inc-nmaster)
;;            ("H-C-<down>"  edwina-dec-nmaster))

;(define-key edwina-mode-map (kbd "C-x 2") nil)
;(define-key edwina-mode-map (kbd "C-x 3") nil)
;; Optional: kill it all if needed
;; (edwina-mode -1)
;; (unload-feature 'edwina t)

;; Formatting 
(require-package 'treesit-auto)
(require 'treesit-auto)

(setq treesit-auto-install 'prompt)

(global-treesit-auto-mode 1)
(treesit-auto-add-to-auto-mode-alist 'all)
(setq treesit-font-lock-level 4)

;; Non-Treesitter Languages
(require-package 'markdown-mode)
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(add-to-list 'load-path "~/.emacs.d/lisp")

(require-package 'reformatter)
(require 'reformatter)

(require-package 'eglot)
(autoload 'eglot-ensure "eglot" nil t)

(require-package 'zig-mode)

(add-hook 'zig-mode-hook  #'eglot-ensure)
(add-hook 'rust-mode-hook #'eglot-ensure)

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(zig-mode . ("zls"))))

(setq zig-format-on-save nil)

;; Keep the minibuffer quiet and turn off some LSP niceties
(setq eldoc-message-function #'ignore)

;; Tell Eglot to ignore capabilities we don't want from servers
;; Add documentHighlightProvider to stop symbol highlighting without touching internals
(setq eglot-ignored-server-capabilities '(:inlayHintProvider :documentHighlightProvider))

;; When Eglot manages a buffer, also turn off local modes you don't want
(add-hook 'eglot-managed-mode-hook
          (lambda ()
            (flymake-mode -1)              ;; disable diagnostics UI
            (eldoc-mode -1)                ;; no eldoc hints
            ;; belt-and-suspenders if your Emacs has these minor modes:
            (when (fboundp 'eglot-inlay-hints-mode)
              (eglot-inlay-hints-mode -1))
            (when (fboundp 'eglot-highlight-symbol-mode)
              (eglot-highlight-symbol-mode -1))))

;; (add-hook 'eglot-managed-mode-hook
;;           (lambda ()
;;             (flymake-mode -1)              ;; No diagnostics
;;             (eldoc-mode -1)                ;; No eldoc popups or minibuffer docs
;;             (when (boundp 'eglot--document-highlight)
;;               (remove-hook 'post-command-hook #'eglot--document-highlight t))))

(add-to-list 'auto-mode-alist '("\\.c3\\'" . c-mode))

;; Mute echo area more
(setq eldoc-message-function #'ignore)
(setq eglot-ignored-server-capabilities '(:inlayHintProvider))

;; ;; Format-on-save via rust-analyzer
;; (setq-default eglot-workspace-configuration
;;               '((:rust-analyzer . (:rustfmt . (:enable t)))))

;; Jump to Definition
(require-package 'dumb-jump)
(require 'dumb-jump)

(setq dumb-jump-selector 'completing-read)
(setq dumb-jump-prefer-searcher 'rg)

(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
(setq xref-show-definitions-function #'xref-show-definitions-completing-read)

;; Linter
(require-package 'flyspell)
(require 'flyspell)
(require-package 'typescript-mode)

;; Formater
(require-package 'format-all)
;; NEW
(autoload 'format-all-buffer "format-all" nil t) ; 

(global-set-key (kbd "C-c M-f") #'format-all-buffer)

;; Custom Theme
(set-fringe-mode 0)

;; Theme
(require-package 'doom-themes)
(load-theme 'doom-dracula t)

(set-face-foreground 'vertical-border (face-background 'default))
(set-face-background 'fringe (face-background 'default))
(set-face-background 'line-number (face-background 'default))

;; Highlights
(require-package 'hl-todo)
(require 'hl-todo)

(setq hl-todo-keywords
      '("TODO" "FIXME" "DEBUG" "GOTCHA" "STUB" "NOTE" "HACK" "REVIEW" "DEPRECATED"))

;; Optional: set custom keywords
(setq hl-todo-keyword-faces
      '(("TODO"   . "#ff6c6b") ;; red
        ("FIXME"  . "#da8548") ;; orange
        ("DEBUG"  . "#a9a1e1") ;; purple
        ("GOTCHA" . "#ff6c6b") ;; red
        ("STUB"   . "#dcaeea") ;; pink
        ("NOTE"   . "#51afef") ;; blue
        ("HACK"   . "#98be65") ;; green
        ("REVIEW" . "#c678dd") ;; soft violet
        ("DEPRECATED" . "#999999"))) ;; gray

(global-hl-todo-mode 1)

;; ;; Require a colon after tag (e.g., BUG: something) to avoid false positives
;; (setq hl-todo-require-punctuation t)

;; ;; Dracula-ish palette (foregrounds)
;; (defconst my/drac-red      "#ff5555")
;; (defconst my/drac-orange   "#ffb86c")
;; (defconst my/drac-yellow   "#f1fa8c")
;; (defconst my/drac-green    "#50fa7b")
;; (defconst my/drac-cyan     "#8be9fd")
;; (defconst my/drac-blue     "#6272a4")
;; (defconst my/drac-purple   "#bd93f9")
;; (defconst my/drac-pink     "#ff79c6")

;; ;; Practical keywords
;; (setq hl-todo-keywords
;;       '("NOW" "LATER" "BUG" "FIX" "WARN" "DEBT" "PERF" "IDEA" "TEMP" "NOTE" "DOC"))

;; (setq hl-todo-keyword-faces
;;       `(("ERROR"   . ,my/drac-red)
;;         ("WARN"  . ,my/drac-orange)
;;         ("EDIT" . ,my/drac-yellow)
;;         ("SECTION"  . ,my/drac-purple)
;;         ("IDEA"  . ,my/drac-pink)
;;         ("TEMP"  . ,my/drac-blue)
;;         ("NOTE"  . ,my/drac-cyan)
;;         ("DOC"   . ,my/drac-green)))

;; ;; Make hl-todo tags a touch bolder (optional)
;; (custom-set-faces '(hl-todo ((t (:weight bold :underline nil)))))

;; ;; ---- Tint the message after TAG: -----------------------------------
;; (defun my/hl-todo--msg-face (hex)
;;   "Return a face symbol that renders message text with HEX."
;;   (let ((f (intern (format "my/hl-todo-msg-%s" (substring hex 1))))) ; face names can't include '#'
;;     (unless (facep f)
;;       (make-face f)
;;       (set-face-attribute f nil :foreground hex :slant 'italic))
;;     f))

;; (defun my/hl-todo--build-rules ()
;;   "Build font-lock rules to color message after TAG: using tag color."
;;   (let (rules)
;;     (dolist (kv hl-todo-keyword-faces (nreverse rules))
;;       (let* ((tag (car kv))
;;              (hex (cdr kv))
;;              (msg-face (my/hl-todo--msg-face hex))
;;              ;; Match: TAG: message   (group1=TAG, group2=message)
;;              (rx  (concat "\\b\\(" (regexp-quote tag) "\\)\\s*:\\s-*\\(.*\\)$")))
;;         (push
;;          `(,rx
;;            ;; group 1 is the tag (hl-todo already colors it; reinforce bold)
;;            (1 (list :inherit (quote hl-todo) :weight 'bold) t)
;;            ;; group 2 is the trailing message (our tinted face)
;;            (2 (list :inherit (quote ,msg-face)) t))
;;          rules)))))

;; (defun my/hl-todo-extend-message-highlighting ()
;;   "Activate message tinting in the current buffer."
;;   (font-lock-add-keywords nil (my/hl-todo--build-rules) 'append)
;;   (when font-lock-mode (font-lock-flush)))

;; ;; Enable in code + org; add our message-tint hook when hl-todo turns on
;; (add-hook 'prog-mode-hook #'hl-todo-mode)
;; (add-hook 'org-mode-hook  #'hl-todo-mode)
;; (add-hook 'hl-todo-mode-hook #'my/hl-todo-extend-message-highlighting)

;; Usage: write lines like
;;   BUG: crashes on empty input
;;   PERF: stream decode to cut mem
;;   DEBT: split module


;; NOTE: LIGATURES
(require-package 'ligature)
(require 'ligature)

; (ligature-set-ligatures 't '("www" "->" "<-" "=>" "<=" "!=" "===" "&&" "||" "::" "++" "--" "->>" "<<-" "lambda"))
(ligature-set-ligatures 'prog-mode
  '("!=" "!=="
    "->" "->>" "<-" "<-<" "<->" "<-->" "<--" "-->" "=>" "=>>"
    "<=>" "<==>" "<=" ">=" "=="
    "===" "=/=" "!=="
    "<>" "<<<" ">>>" "<<=" ">>="
    "::=" "::" ":=" ":>" "<:"
    "++" "+++"
    "||" "|||"
    "&&" "&&&"
    "**" "***"
    "##" "###"
    "%%" "%%%"
    "@@" "@@@"
    "..." ".." "..<" "..="
    ".=" ".?" "?." "?:" "?="
    "<|" "<|>" "|>" "<>"
    "<$" "<$>" "$>" "<+>" "<*>" "<.>" "<^>" "<=>"
    "|-" "|=" "|->" "|-->" "|=>"
    "#{" "#[" "#(" "#_" "#!" "#?" "#:" "#="
    "~@" "~=" "~>"
    "/=" "/==" "//" "///" "/*" "*/"
    "\\\\" "\\/" "/\\" "\\>"
    "!!." "!!=" "!!?" "!~"
    "<==" "<=!" "<~" "<~~"
    "~>" "~~" "~~>" "~>>"
    "=~" "==>" "==="
    "+=" "-=" "*=" "/=" "%=" "&=" "|=" "^=" "<<=" ">>="
    "++=" "--=" "**="))

;; Good ligatures for prose/code inside Org
(ligature-set-ligatures 'org-mode
  '("!=" "==" "==="
    "<=" ">=" "=~" "/=" "~=" "~>" "=>"
    "->" "<-" "<->" "<--" "-->" "<-->" "=>>" "<=>" "<==>"
    "... " "..." "..<" "..=" ".=" ".?" "?." "?="
    "<|" "<|>" "|>" "<$" "<$>" "$>" "<+>" "<*>" "<.>" "<^>" "<=>"
    "~@" "\\/" "/\\"
    ":=" ":>" "<:" "::=" "::"  ;; keep if you like, but consider removing "::"
    "+=" "-=" "*=" "/=" "%=" "&=" "|=" "^=" "<<=" ">>="
    "++=" "--=" "**=" "!!=" "!!?" "!~"
    ))


(global-ligature-mode 1)

;; Fonts
;; Twemoji for emoji rendering
; (set-fontset-font t 'emoji (font-spec :family "Twemoji Mozilla") nil 'prepend)
;; Minimalist monochrome emoji (Unifont or Symbola)
; (set-fontset-font t 'emoji (font-spec :family "Unifont") nil 'prepend)
;; Main coding font
(add-to-list 'default-frame-alist '(font . "JetBrains Mono-14"))
(set-frame-font "JetBrains Mono-14" t t)
;; OR
; (set-frame-font "Fira Code-12" t t)

;; Automatic
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("8c7e832be864674c220f9a9361c851917a93f921fedb7717b1b5ece47690c098"
     "0325a6b5eea7e5febae709dab35ec8648908af12cf2d2b569bedc8da0a3a81c1"
     default))
 '(package-selected-packages
   '(consult corfu doom-themes dumb-jump format-all ggtags
             highlight-defined hl-todo ligature magit marginalia
             markdown-mode multiple-cursors orderless org-download
             org-modern persistent-scratch persp-mode
             rainbow-delimiters tide treesit-auto typescript-mode
             undo-fu vertico vterm zig-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-region 'disabled nil)
