;; Globals
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(global-display-line-numbers-mode t)
(global-visual-line-mode 1)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

(delete-selection-mode 1)
(setq delete-selection-save-to-kill-ring nil)
;; (add-hook 'dired-mode-hook
;;           (lambda ()
;;             (setq-local delete-selection-save-to-kill-ring nil)))

;; Smooth Scrolling
(setq scroll-margin 5
      scroll-conservatively 9999
      scroll-step 1)

(global-auto-revert-mode t)

(setq inhibit-startup-screen t)
(setq initial-buffer-choice t)

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
;;(define-key key-translation-map (kbd "<capslock>") (kbd "H-"))

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

;; Highlighting
(require-package 'highlight-defined)
(require-package 'rainbow-delimiters)

(when (require 'highlight-defined nil t)
  (add-hook 'prog-mode-hook #'highlight-defined-mode))

(when (require 'rainbow-delimiters nil t)
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; Scratch
(setq initial-major-mode 'org-mode
      initial-scratch-message "")

(require-package 'persistent-scratch)
(require 'persistent-scratch)

(let ((file (expand-file-name ".persistent-scratch" user-emacs-directory)))
  (unless (file-exists-p file)
    (with-temp-file file "")))

(setq persistent-scratch-backup-directory
      (expand-file-name "scratch-backups/" user-emacs-directory))

(add-hook 'emacs-startup-hook #'persistent-scratch-setup-default)

(add-hook 'emacs-startup-hook
          (lambda ()
            (with-current-buffer "*scratch*"
              (unless (derived-mode-p 'org-mode)
                (org-mode)))))

(global-set-key (kbd "H-s")
  (lambda ()
    (interactive)
    (let ((win (get-buffer-window "*scratch*")))
      (if win
          (delete-window win)
        (select-window (split-window-right))
        (switch-to-buffer "*scratch*")))))

;; Editor Keybindings
(rebind
 ("C-S-v" yank)
 ("C-S-c" kill-ring-save)
 ("C-S-x" kill-region))

(rebind
 ("C-c v" yank)
 ("C-c c" kill-ring-save)
 ("C-c x" kill-region)
 ("C-c a" mark-whole-buffer)
 ("C-c /" comment-line))

;; (global-set-key (kbd "C-c a") 'mark-whole-buffer)

;; (rebind
;;  ("C-c /" comment-line))

;; Workflow Keybindings
(rebind
 ("H-c"   compile)
 ("H-t"   shell)
 ("H-b"   eval-buffer)
 ("H-q"   delete-window)
 ("H-S-q" delete-other-windows))

;; Multiple Cursors
(require-package 'multiple-cursors)
(require 'multiple-cursors)

(add-hook 'org-mode-hook
  (lambda ()
    (when (featurep 'multiple-cursors-core)
      (multiple-cursors-mode 1))))

(setq mc/always-repeat-command t
      mc/always-run-for-all t)

(rebind
 ("C-d"   mc/mark-next-like-this)
 ("C-S-d" mc/mark-all-like-this)
 ("C-M-d" mc/repeat-command))
;; mc/edit-lines & mc/mark-previous-like-this

;; Consult
(require-package 'consult)
(require 'consult)

(rebind
 ("C-S-f" consult-line)
 ("C-S-y" consult-yank-pop))
;;consult-find, consult-grep, consult-ripgrep

;; Undo/Redo
(require-package 'undo-fu)
(require 'undo-fu)

(rebind
  ("C-z"   undo-fu-only-undo)
  ("C-S-z" undo-fu-only-redo))

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

(rebind
 ("C-i" my/eval-buffer-with-message))

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

;; (defun my/dired-fzf-find-file ()
;;   "Use fzf to select a file, then open its directory in dired and jump to it."
;;   (interactive)
;;   (let* ((default-directory (if (derived-mode-p 'dired-mode)
;;                                 (dired-current-directory)
;;                               default-directory))
;;          (file (string-trim (shell-command-to-string "fzf --height=40% --preview 'head -100 {}'"))))
;;     (when (and file (not (string= file "")))
;;       (dired (file-name-directory (expand-file-name file)))
;;       (dired-goto-file (expand-file-name file)))))

;; (rebind
;;  ("H-f" 'my/dired-fzf-find-file))

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

;; Eglot for Rust (auto-start in rust-mode)
(add-hook 'rust-mode-hook #'eglot-ensure)

(add-hook 'eglot-managed-mode-hook
          (lambda ()
            (flymake-mode -1)              ;; No diagnostics
            (eldoc-mode -1)                ;; No eldoc popups or minibuffer docs
            (when (boundp 'eglot--document-highlight)
              (remove-hook 'post-command-hook #'eglot--document-highlight t))))

;; Mute echo area more
(setq eldoc-message-function #'ignore)
(setq eglot-ignored-server-capabilities '(:inlayHintProvider))

;; Format-on-save via rust-analyzer
(setq-default eglot-workspace-configuration
              '((:rust-analyzer . (:rustfmt . (:enable t)))))

;; Org
(require 'url-handlers)  ; needed for org-download for some reason
(require-package 'org-modern)
(require-package 'org-download)

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
  "Turn region/paragraph into Org list, splitting at sentence end, newline, or heading. Punctuation is kept. Stops at heading lines."
  (interactive "r")
  (let ((text (buffer-substring-no-properties beg end))
        (start 0)
        (len 0)
        (regex "\\([.!?]\\)[ \t]+\\|\\(?:\n\\{1,2\\}\\)\\|\\(?:\n\\*+ \\)"))
    (delete-region beg end)
    (while (and (< start (length text))
                (string-match regex text start))
      (setq len (- (match-end 0) start))
      (let ((piece (string-trim (substring text start (match-end 0)))))
        (unless (string-match-p "^\\*+ " piece)
          (insert "- " piece "\n")))
      (setq start (match-end 0)))
    ;; Handle the final chunk
    (let ((final (string-trim (substring text start))))
      (unless (string-match-p "^\\*+ " final)
        (unless (string-empty-p final)
          (insert "- " final "\n"))))))

(defun my/org-tree-to-indirect-buffer ()
  "Clone current Org subtree to indirect buffer in the SAME window."
  (interactive)
  (let ((buffer (org-tree-to-indirect-buffer)))
    (when buffer
      (switch-to-buffer buffer))))


(define-key org-mode-map (kbd "C-M-<return>") #'my/org-continue-list-or-paragraph)
(define-key org-mode-map (kbd "C-c l") #'my/org-paragraph-to-list)

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

;; PERSPECTIVE

;; Pure perspective-mode solution for isolated workspaces
(require-package 'persp-mode)
(require 'persp-mode)

;; Configure persp-mode for automatic buffer management
(setq persp-autokill-buffer-on-remove 'kill-weak)
;; Let persp-mode handle buffer adding automatically
(setq persp-add-buffer-on-find-file t)
(setq persp-add-buffer-on-after-change-major-mode t)
;; Disable perspective save/restore to prevent old state loading
(setq persp-auto-save-opt 0)
(setq persp-auto-resume-time -1)

;; Don't kill existing perspectives - let them persist
;; (Removed the destructive cleanup that was causing the issue)

(persp-mode 1)

;; Clean mode line first to prevent duplication on re-eval
(setq-default mode-line-format
              (cl-remove-if (lambda (item)
                             (and (listp item)
                                  (eq (car item) :eval)
                                  (listp (cadr item))
                                  (eq (car (cadr item)) 'my/persp-mode-line)))
                           mode-line-format))

;; Simple mode line to show workspace tabs (only on main buffer)
(defun my/persp-mode-line ()
  "Show perspective tabs in mode line, but only on the first/main window."
  (when (and (not (minibufferp))
             (eq (selected-window) (frame-first-window))
             (not (string-match "^\\*vterm" (buffer-name)))
             (not (string-match "^\\*scratch-T[0-9]+\\*$" (buffer-name))))
    (let* ((persps (hash-table-keys *persp-hash*))
           (sorted-persps (sort (cl-remove-if-not 
                                (lambda (name) (string-match "^T[1-9]$" name)) 
                                persps)
                               (lambda (a b) (< (string-to-number (substring a 1))
                                                (string-to-number (substring b 1))))))
           (current (safe-persp-name (get-current-persp))))
      (if sorted-persps
          (concat "["
                  (mapconcat (lambda (name)
                              (let ((num (substring name 1)))
                                (if (string= name current)
                                    (propertize num 'face 'mode-line-emphasis)
                                  num)))
                            sorted-persps
                            ", ")
                  "] ")
        ""))))

;; Add back to mode line
(setq-default mode-line-format
              (cons '(:eval (my/persp-mode-line))
                    mode-line-format))

(defun my/get-scratch-buffer-for-workspace (workspace-num)
  "Get or create a scratch buffer for WORKSPACE-NUM."
  (let ((buffer-name (format "*scratch-T%d*" workspace-num)))
    (or (get-buffer buffer-name)
        (with-current-buffer (get-buffer-create buffer-name)
          ;; Use the same mode as the default *scratch* buffer
          (funcall initial-major-mode)
          ;; Insert initial message if it exists
          (when (and initial-scratch-message (not (string= initial-scratch-message "")))
            (insert initial-scratch-message))
          (current-buffer)))))

;; Initialize all 9 workspaces at startup
(defun my/initialize-all-workspaces ()
  "Create all 9 workspaces with proper isolation."
  ;; Only initialize if we don't already have ALL our workspaces
  (unless (and (gethash "T1" *persp-hash*)
               (gethash "T2" *persp-hash*)
               (gethash "T9" *persp-hash*))
    
    ;; Save current state BEFORE doing anything
    (let ((starting-persp (safe-persp-name (get-current-persp)))
          (starting-buffer (current-buffer))
          (starting-window-config (current-window-configuration)))
      
      ;; Only create missing workspaces
      (let ((current-buffers (buffer-list)))
        
        ;; Create T1 with current buffers (preserve what's open) - only if it doesn't exist
        (unless (gethash "T1" *persp-hash*)
          (persp-add-new "T1")
          (persp-switch "T1")
          (dolist (buf current-buffers)
            (unless (or (string-match "^ " (buffer-name buf))  ; skip hidden
                       (string-match "^\\*scratch-T[0-9]+\\*$" (buffer-name buf))) ; skip workspace scratch
              (persp-add-buffer buf))))
        
        ;; Create T2-T9 with only their scratch buffers
        (dotimes (i 8)
          (let* ((num (+ i 2))
                 (persp-name (format "T%d" num)))
            (unless (gethash persp-name *persp-hash*)
              (let ((scratch-buf (my/get-scratch-buffer-for-workspace num)))
                (persp-add-new persp-name)
                (persp-switch persp-name)
                (persp-add-buffer scratch-buf))))))
      
      ;; Restore everything exactly as it was
      (cond
       ((string= starting-persp "none") (persp-switch "T1"))
       ((gethash starting-persp *persp-hash*) (persp-switch starting-persp))
       (t (persp-switch "T1")))
      
      (set-window-configuration starting-window-config)
      (when (buffer-live-p starting-buffer)
        (switch-to-buffer starting-buffer)))))  ; Remove tab bar update

;; Simple switching function
(defun my/switch-workspace (num)
  "Switch to workspace NUM (1-9)."
  (let ((persp-name (format "T%d" num)))
    (persp-switch persp-name)))  ; Remove tab bar update

(defun my/kill-workspace (num)
  "Kill workspace NUM."
  (interactive "nWorkspace to kill (1-9): ")
  (let* ((persp-name (format "T%d" num))
         (current-persp (safe-persp-name (get-current-persp)))
         (scratch-buffer (get-buffer (format "*scratch-T%d*" num))))
    
    (when (string= current-persp persp-name)
      (user-error "Cannot kill current workspace. Switch to another first."))
    
    (when scratch-buffer
      (kill-buffer scratch-buffer))
    
    (persp-kill persp-name)
    
    ;; Recreate it fresh
    (let ((scratch-buf (my/get-scratch-buffer-for-workspace num)))
      (persp-add-new persp-name)
      (persp-switch persp-name)
      (persp-add-buffer scratch-buf)
      (delete-other-windows)
      (switch-to-buffer scratch-buf)
      ;; Switch back to where we were
      (persp-switch current-persp))))

;; Set up keybindings
(dotimes (i 9)
  (let ((n (1+ i)))
    (global-set-key (kbd (format "H-%d" n))
                    `(lambda () (interactive)
                       (my/switch-workspace ,n)))))

(global-set-key (kbd "H-k") #'my/kill-workspace)

;; Initialize everything
(my/initialize-all-workspaces)
(defun my/persp-kill-buffer-cleanup ()
  "Automatically remove killed buffers from the current perspective."
  (when (and (bound-and-true-p persp-mode)
             (get-current-persp))
    (persp-remove-buffer (current-buffer) (get-current-persp) nil)))

(add-hook 'kill-buffer-hook #'my/persp-kill-buffer-cleanup)

(global-set-key (kbd "C-c q")
  (lambda () (interactive)
    (let ((buf (current-buffer)))
      (cond
       ;; If there's more than one window: kill this one and maybe the buffer
       ((cdr (window-list))
        (delete-window)
        ;; If no window is now showing the buffer, kill it
        (unless (get-buffer-window buf 'visible)
          (persp-remove-buffer buf)
          (kill-buffer buf)))

       ;; If this is the only window, prompt to kill the whole workspace
       (t
        (when (yes-or-no-p "Last window. Kill this workspace and buffer?")
          (let* ((current-persp-name (safe-persp-name (get-current-persp)))
                 (workspace-num (when (string-match "^T\\([1-9]\\)$" current-persp-name)
                                 (string-to-number (match-string 1 current-persp-name)))))
            (if workspace-num
                (my/kill-workspace workspace-num)
              (kill-buffer buf)))))))))

(defun my/consult-buffer-persp ()
  "Buffer switching scoped to current perspective."
  (interactive)
  (if (and (bound-and-true-p persp-mode) (get-current-persp))
      (let* ((current-persp (get-current-persp))
             (persp-buffers (when current-persp
                             (persp-buffers current-persp))))
        (if persp-buffers
            (switch-to-buffer 
             (completing-read "Buffer (perspective): " 
                             (mapcar #'buffer-name persp-buffers)))
          (message "No buffers in current perspective")))
    ;; Fallback to regular buffer switching if not in a perspective
    (switch-to-buffer (completing-read "Buffer: " (mapcar #'buffer-name (buffer-list))))))

(global-set-key (kbd "C-x b") #'my/consult-buffer-persp)

;; END OF PERSPECTIVE 

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
  ("C-S-<return>"     my/open-vterm-for-perspective)
  ("C-H-<return>"     my/toggle-vterm-for-perspective)
  ("C-H-S-<return>"   my/kill-vterm-for-perspective))

;; Tiling
(require-package 'windmove)

(windmove-default-keybindings 'hyper)
(winner-mode 1)

;; Undo/Redo Window Changes
(rebind
 ("H-z" winner-undo)
 ("H-S-z" winner-redo)) ;maybe make z & S-z/Z

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

;; LANGUAGE SUPPORT
;; (require-package 'web-mode)

;; (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))

;; (with-eval-after-load 'web-mode
;;   (setq web-mode-enable-auto-quoting nil)
;;   (add-to-list 'web-mode-content-types-alist '("jsx" . "\\.tsx\\'")))
(require-package 'treesit-auto)
(require 'treesit-auto)  ;; ensures functions are loaded

(setq treesit-auto-install 'prompt)  ;; 't to auto-install silently, 'prompt to confirm

(global-treesit-auto-mode 1)
(treesit-auto-add-to-auto-mode-alist 'all)

;; Non-Treesitter Languages
(require-package 'markdown-mode)
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(add-to-list 'load-path "~/.emacs.d/lisp")
;; (require 'reformatter)
;; (add-to-list 'load-path "~/.emacs.d/zig-mode")
;; (require 'zig-mode)
;; (add-to-list 'auto-mode-alist '("\\.zig\\'" . zig-mode))
(add-to-list 'auto-mode-alist '("\\.c3\\'" . c-mode))

;; ;; Tell Emacs where to find your Odin grammar
;; (add-to-list 'treesit-language-source-alist
;;              '(odin . ("https://github.com/amaanq/tree-sitter-odin")))

;; ;; Register odin mode
;; (add-to-list 'major-mode-remap-alist
;;              '(odin-mode . odin-ts-mode))

;; ;; Optionally: Set file associations
;; (add-to-list 'auto-mode-alist '("\\.odin\\'" . odin-mode))

;; (require-package 'tree-sitter)
;; (require-package 'tree-sitter-langs)

;; (add-hook 'prog-mode-hook #'tree-sitter-mode)
;; (add-hook 'prog-mode-hook #'tree-sitter-hl-mode)

;; ;; Web
;; (add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))
;; (add-to-list 'auto-mode-alist '("\\.jsx\\'" . js-ts-mode))
;; (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
;; (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
;; (add-to-list 'auto-mode-alist '("\\.html\\'" . html-ts-mode))
;; (add-to-list 'auto-mode-alist '("\\.css\\'" . css-ts-mode))
;; (add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))
;; (add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.svelte\\'" . web-mode))

;; ;; Programming
;; (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
;; (add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
;; (add-to-list 'auto-mode-alist '("\\.c\\'" . c-ts-mode))
;; (add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-ts-mode))
;; (add-to-list 'auto-mode-alist '("\\.h\\'" . c-ts-mode))
;; (add-to-list 'auto-mode-alist '("\\.java\\'" . java-ts-mode))
;; (add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-ts-mode))

;; ;; More
;; (add-to-list 'auto-mode-alist '("\\.toml\\'" . toml-ts-mode))
;; (add-to-list 'auto-mode-alist '("\\.v\\'" . verilog-mode))
;; (add-to-list 'auto-mode-alist '("\\.sv\\'" . verilog-mode))
;; (add-to-list 'auto-mode-alist '("\\.xml\\'" . xml-mode))
;; (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-ts-mode))
;; (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-ts-mode))
;; (add-to-list 'auto-mode-alist '("\\.py\\'" . python-ts-mode))
;; (add-to-list 'auto-mode-alist '("\\.sql\\'" . sql-mode))
;; (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
;; (add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))



;; Linter
(require-package 'flyspell) ;; only if you're not already loading it
(require 'flyspell)
(require-package 'typescript-mode)

;; Formater
(require-package 'format-all)

(global-set-key (kbd "C-c f") #'format-all-buffer)

;; Custom Theme
(set-fringe-mode 0)

;; Theme
(require-package 'doom-themes)
;(require-package 'catppuccin-theme)
;(load-theme 'doom-one t)
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
   '(consult corfu doom-themes format-all highlight-defined hl-todo
             ligature magit marginalia markdown-mode multiple-cursors
             orderless org-download org-modern persistent-scratch
             persp-mode rainbow-delimiters tree-sitter
             tree-sitter-langs treesit-auto typescript-mode undo-fu
             vertico vterm web-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-region 'disabled nil)
