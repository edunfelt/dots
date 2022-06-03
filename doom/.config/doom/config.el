;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;;                                            __
;; .----.-----.-----.'  _|__|.-----.  .-----.|  |
;; |  __|  _  |     |   _|  ||  _  |__|  -__||  |
;; |____|_____|__|__|__| |__||___  |__|_____||__|
;;
;; Emilia's config.el <3
;; Author: Emilia Dunfelt, edun@dunfelt.se

;; General settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; user configuration
(setq user-full-name "Emilia Dunfelt"
      user-mail-address "edun@dunfelt.se"
      display-line-numbers-type t)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)


; settings
(setq-default 
  delete-by-moving-to-trash t                         ; delete to trash
  indent-tabs-mode nil                                ; don't use tab for indent
  tab-width 4                                         ; tabs are four spaces
  initial-scratch-message "What's on your mind?"      ; *scratch* buffer msg
  select-enable-clipboard t                           ; merge system and emacs clipboard
  x-stretch-cursor t)                                 ; stretch cursor to glyph

(set-default-coding-systems 'utf-8)                   ; default to utf-8

(setq undo-limit 80000000                             ; increase undo limit
                 evil-want-fine-undo t                ; granular undo
                 auto-save-default t)                 ; autosaving



;; Appearance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; toolbars
;(tool-bar-mode 1)
;(menu-bar-mode 1)
;(scroll-bar-mode 1)


; font
(setq doom-font (font-spec :family "Terminus (TTF)" :size 16)
      doom-big-font (font-spec :family "Terminus (TTF) Bold" :size 16))


; theme
(setq doom-theme 'doom-opera-light)

; Change theme based on time of day
;; (defun synchronize-theme ()
;;   (let* ((light-theme 'doom-opera-light)
;;          (dark-theme 'doom-opera)
;;          (start-time-light-theme 7)
;;          (end-time-light-theme 18)
;;          (hour (string-to-number (substring (current-time-string) 11 13)))
;;          (next-theme (if (member hour (number-sequence start-time-light-theme end-time-light-theme))
;;                          light-theme dark-theme)))
;;     (when (not (equal doom-theme next-theme))
;;       (setq doom-theme next-theme)
;;       (load-theme next-theme))))
;; (run-with-timer 0 900 'synchronize-theme)


; modeline
(setq doom-modeline-mode 1)
(setq doom-modeline-height 15)


; fancy splash screen
(defvar +fl/splashcii-query ""
  "The query to search on asciiur.com")

(defun +fl/splashcii ()
  (split-string (with-output-to-string
                  (call-process "splashcii" nil standard-output nil +fl/splashcii-query))
                "\n" t))

(defun +fl/doom-banner ()
  (let ((point (point)))
    (mapc (lambda (line)
            (insert (propertize (+doom-dashboard--center +doom-dashboard--width line)
                                'face 'doom-dashboard-banner) " ")
            (insert "\n"))
          (+fl/splashcii))
    (insert (make-string (or (cdr +doom-dashboard-banner-padding) 0) ?\n))))

; override the first doom dashboard function
(setcar (nthcdr 0 +doom-dashboard-functions) #'+fl/doom-banner)
(setq +fl/splashcii-query "space")

; remove useful commands and disable modeline
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)
;(add-hook! '+doom-dashboard-mode-hook (hide-mode-line-mode 1) (hl-line-mode -1))
(setq-hook! '+doom-dashboard-mode-hook evil-normal-state-cursor (list nil))



;; Org-stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'org-habit)
(setq org-directory "~/current"
      org-ellipsis " ▼ "
      org-adapt-indentation nil)

; all the pretty things
(add-hook! 'org-mode-hook #'+org-pretty-mode)


; toggle LaTeX fragments
(use-package! org-fragtog
  :after org
  :hook (org-mode . org-fragtog-mode))
(add-hook 'org-mode-hook 'turn-on-org-cdlatex)


; agenda
(use-package org-agenda
  :after org
  :config
  (setq org-agenda-skip-deadline-prewarning-if-scheduled t)     ; don't show deadline if i'm on it
  (setq org-agenda-skip-deadline-if-done t)                     ; hide done deadlines
  (setq org-agenda-skip-scheduled-if-done t)                    ; hide schedule if done
  (setq org-agenda-start-with-log-mode t)
  (setq org-agenda-use-time-grid nil)
  (setq org-agenda-start-day nil)
  :custom
  (org-agenda-prefix-format '((agenda . " %i %-20:c%?-12t%-6e% s")
                              (todo   . " %i %-20:c %-6e")
                              (tags   . " %i %-20:c")
                              (search . " %i %-20:c"))))

; agenda view with ghd sections
(setq org-agenda-custom-commands
    '(("d" "Today's Tasks"
                ((agenda "" ((org-agenda-span 1)
                        (org-agenda-overriding-header "Today's tasks")))
                (tags-todo
                        "GHD+ACTIVE+PRIORITY=\"A\""
                        ((org-agenda-files '("~/current/ghd.org"))
                        (org-agenda-overriding-header "Primary goals this month")))
                (tags-todo
                        "GHD+ACTIVE+PRIORITY=\"C\""
                        ((org-agenda-files '("~/current/ghd.org"))
                         (org-agenda-overriding-header "Secondary goals this month")))))
      ("w" "This Week's Tasks"
                ((agenda)
                (tags-todo
                        "GHD+ACTIVE+PRIORITY=\"A\""
                        ((org-agenda-files '("~/current/ghd.org"))
                        (org-agenda-overriding-header "Primary goals this month")))
                (tags-todo
                        "GHD+ACTIVE+PRIORITY=\"C\""
                        ((org-agenda-files '("~/current/ghd.org"))
                         (org-agenda-overriding-header "Secondary goals this month")))))))


; clock stuff
(setq org-clock-sound "~/media/sys/for-sure-576.wav")

; always change the task to IN-PROGRESS when clock activated
(setq org-clock-in-switch-to-state "STRT")

; use a function to decide what to change the state to.
(setq org-clock-in-switch-to-state #'edun/switch-task-on-clock-start)
(defun edun/switch-task-on-clock-start (task-state)
  "Change a task to 'STRT' when TASK-STATE is 'TODO'."
  (if (string= task-state "TODO")
      "STRT"
      task-state))


; babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python .t)))
(setq org-confirm-babel-evaluate nil)


; structure templates
(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))


; uuids for anki
(global-set-key (kbd "C-c C-'") 'uuid-insert)
(defun uuid-insert()
  (interactive)
  (require 'uuid)
  (insert (upcase (uuid-string))))


; roam stuff
(setq org-roam-directory (file-truename "~/roam"))
(org-roam-db-autosync-mode)
(require 'org-roam-protocol)

; capture templates
(setq org-roam-capture-ref-templates
      '(("i" "internet" plain #'org-roam-capture--get-point "%?"
         :file-name "float/%<%Y%m%d%H%M>-${slug}"
         :head "#+title: ${title}\n#+roam_key: ${ref}\n#+roam_tags: bookmark"
         :unnarrowed t)))

(use-package! websocket
    :after org-roam)

(use-package! org-roam-ui
    :after org-roam
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))


; deft
(use-package deft
  :after org
  :bind
  ("C-c n d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory org-roam-directory))


; pdf things
(use-package! org-noter
  :config
  (setq
   org-noter-notes-window-location 'other-frame
   ;; Please stop opening frames
   ;;org-noter-always-create-frame nil
   org-noter-hide-other nil
   org-noter-notes-search-path "~/roam/refs"
   )
)

(use-package! org-pdftools
  :hook (org-load . org-pdftools-setup-link))



;; LaTeX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; AucTeX settings - almost no changes
(use-package latex
  :ensure auctex
  :hook ((LaTeX-mode . prettify-symbols-mode))
  :bind (:map LaTeX-mode-map
         ("C-S-e" . latex-math-from-calc))
  :config
  ;; Format math as a Latex string with Calc
  (defun latex-math-from-calc ()
    "Evaluate `calc' on the contents of line at point."
    (interactive)
    (cond ((region-active-p)
           (let* ((beg (region-beginning))
                  (end (region-end))
                  (string (buffer-substring-no-properties beg end)))
             (kill-region beg end)
             (insert (calc-eval `(,string calc-language latex
                                          calc-prefer-frac t
                                          calc-angle-mode rad)))))
          (t (let ((l (thing-at-point 'line)))
               (end-of-line 1) (kill-line 0) 
               (insert (calc-eval `(,l
                                    calc-language latex
                                    calc-prefer-frac t
                                    calc-angle-mode rad))))))))

(use-package preview
  :after latex
  :hook ((LaTeX-mode . preview-larger-previews))
  :config
  (defun preview-larger-previews ()
    (setq preview-scale-function
          (lambda () (* 1.25
                   (funcall (preview-scale-from-face)))))))

; CDLatex settings
(use-package cdlatex
  :ensure t
  :hook (LaTeX-mode . turn-on-cdlatex)
  :bind (:map cdlatex-mode-map 
              ("<tab>" . cdlatex-tab)))

; Yasnippet settings
(add-hook 'yas-before-expand-snippet-hook (lambda () (smartparens-mode -1)))
(add-hook 'yas-after-exit-snippet-hook (lambda () (smartparens-mode 1)))

(use-package yasnippet
  :ensure t
  :hook ((LaTeX-mode . yas-minor-mode)
         (post-self-insert . my/yas-try-expanding-auto-snippets))
  :config
  (use-package warnings
    :config
    (cl-pushnew '(yasnippet backquote-change)
                warning-suppress-types
                :test 'equal))

  (setq yas-triggers-in-field t)
  (setq yas-snippet-dirs
        '("~/archive/00-09-meta/01-emacs/01.03-yasnippet"))
  
  ;; Function that tries to autoexpand YaSnippets
  ;; The double quoting is NOT a typo!
  (defun my/yas-try-expanding-auto-snippets ()
    (when (and (boundp 'yas-minor-mode) yas-minor-mode)
      (let ((yas-buffer-local-condition ''(require-snippet-condition . auto)))
        (yas-expand)))))
;(yas-global-mode 1)
;(defun my-yas-try-expanding-auto-snippets ()
  ;(when yas-minor-mode
    ;(let ((yas-buffer-local-condition ''(require-snippet-condition . auto)))
      ;(yas-expand))))
;(add-hook 'post-command-hook #'my-yas-try-expanding-auto-snippets)

;; CDLatex integration with YaSnippet: Allow cdlatex tab to work inside Yas
;; fields
(use-package cdlatex
  :hook ((cdlatex-tab . yas-expand)
         (cdlatex-tab . cdlatex-in-yas-field))
  :config
  (use-package yasnippet
    :bind (:map yas-keymap
           ("<tab>" . yas-next-field-or-cdlatex)
           ("TAB" . yas-next-field-or-cdlatex))
    :config
    (defun cdlatex-in-yas-field ()
      ;; Check if we're at the end of the Yas field
      (when-let* ((_ (overlayp yas--active-field-overlay))
                  (end (overlay-end yas--active-field-overlay)))
        (if (>= (point) end)
            ;; Call yas-next-field if cdlatex can't expand here
            (let ((s (thing-at-point 'sexp)))
              (unless (and s (assoc (substring-no-properties s)
                                    cdlatex-command-alist-comb))
                (yas-next-field-or-maybe-expand)
                t))
          ;; otherwise expand and jump to the correct location
          (let (cdlatex-tab-hook minp)
            (setq minp
                  (min (save-excursion (cdlatex-tab)
                                       (point))
                       (overlay-end yas--active-field-overlay)))
            (goto-char minp) t))))

    (defun yas-next-field-or-cdlatex ()
      (interactive)
      "Jump to the next Yas field correctly with cdlatex active."
      (if (bound-and-true-p cdlatex-mode)
          (cdlatex-tab)
        (yas-next-field-or-maybe-expand)))))

;; Array/tabular input with org-tables and cdlatex 
(use-package org-table
  :after cdlatex
  :bind (:map orgtbl-mode-map
              ("<tab>" . lazytab-org-table-next-field-maybe)
              ("TAB" . lazytab-org-table-next-field-maybe))
  :init
  (add-hook 'cdlatex-tab-hook 'lazytab-cdlatex-or-orgtbl-next-field 90)
  ;; Tabular environments using cdlatex
  (add-to-list 'cdlatex-command-alist '("smat" "Insert smallmatrix env"
                                       "\\left( \\begin{smallmatrix} ? \\end{smallmatrix} \\right)"
                                       lazytab-position-cursor-and-edit
                                       nil nil t))
  (add-to-list 'cdlatex-command-alist '("bmat" "Insert bmatrix env"
                                       "\\begin{bmatrix} ? \\end{bmatrix}"
                                       lazytab-position-cursor-and-edit
                                       nil nil t))
  (add-to-list 'cdlatex-command-alist '("pmat" "Insert pmatrix env"
                                       "\\begin{pmatrix} ? \\end{pmatrix}"
                                       lazytab-position-cursor-and-edit
                                       nil nil t))
  (add-to-list 'cdlatex-command-alist '("tbl" "Insert table"
                                        "\\begin{table}\n\\centering ? \\caption{}\n\\end{table}\n"
                                       lazytab-position-cursor-and-edit
                                       nil t nil))
  :config
  ;; Tab handling in org tables
  (defun lazytab-position-cursor-and-edit ()
    ;; (if (search-backward "\?" (- (point) 100) t)
    ;;     (delete-char 1))
    (cdlatex-position-cursor)
    (lazytab-orgtbl-edit))

  (defun lazytab-orgtbl-edit ()
    (advice-add 'orgtbl-ctrl-c-ctrl-c :after #'lazytab-orgtbl-replace)
    (orgtbl-mode 1)
    (open-line 1)
    (insert "\n|"))

  (defun lazytab-orgtbl-replace (_)
    (interactive "P")
    (unless (org-at-table-p) (user-error "Not at a table"))
    (let* ((table (org-table-to-lisp))
           params
           (replacement-table
            (if (texmathp)
                (lazytab-orgtbl-to-amsmath table params)
              (orgtbl-to-latex table params))))
      (kill-region (org-table-begin) (org-table-end))
      (open-line 1)
      (push-mark)
      (insert replacement-table)
      (align-regexp (region-beginning) (region-end) "\\([:space:]*\\)& ")
      (orgtbl-mode -1)
      (advice-remove 'orgtbl-ctrl-c-ctrl-c #'lazytab-orgtbl-replace)))
  
  (defun lazytab-orgtbl-to-amsmath (table params)
    (orgtbl-to-generic
     table
     (org-combine-plists
      '(:splice t
                :lstart ""
                :lend " \\\\"
                :sep " & "
                :hline nil
                :llend "")
      params)))

  (defun lazytab-cdlatex-or-orgtbl-next-field ()
    (when (and (bound-and-true-p orgtbl-mode)
               (org-table-p)
               (looking-at "[[:space:]]*\\(?:|\\|$\\)")
               (let ((s (thing-at-point 'sexp)))
                 (not (and s (assoc s cdlatex-command-alist-comb)))))
      (call-interactively #'org-table-next-field)
      t))

  (defun lazytab-org-table-next-field-maybe ()
    (interactive)
    (if (bound-and-true-p cdlatex-mode)
        (cdlatex-tab)
      (org-table-next-field))))



;; RSS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook! elfeed-search-mode-hook 'elfeed-update)
(global-set-key (kbd "C-x w") 'elfeed)
(use-package elfeed)
; elfeed protocol, nextcloud news feeds
(use-package elfeed-protocol
  :after elfeed
  :ensure t
  :config
  (setq elfeed-use-curl t)
  (elfeed-set-timeout 36000)

  ;(setq elfeed-protocol-enabled-protocols '(owncloud))
  (setq elfeed-feeds '(("owncloud+https://emilia@www.cloud.dunfelt.se"
                        :password-file "~/usr/secret/nc")))

  (setq elfeed-protocol-owncloud-update-with-modified-time t)
  (setq elfeed-protocol-owncloud-maxsize 1000)
  (setq elfeed-protocol-lazy-sync t)

  (elfeed-protocol-enable))



;; Other
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package which-key
             :init (which-key-mode)
             :diminish which-key-mode
             :config
             (setq which-key-idle-delay 0.3))

(use-package helpful
             :custom
             (counsel-describe-function-function #'helpful-callable)
             (counsel-describe-variable-function #'helpful-variable)
             :bind
             ([remap describe-function] . counsel-describe-function)
             ([remap describe-command]  . helpful-command)
             ([remap describe-variable] . counsel-describe-variable)
             ([remap describe-key]      . helpful-key))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)


;; projectile
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ; look for projects here
  (when (file-directory-p "~/current/")
    (setq projectile-project-search-path '("~/current/")))
  ; load dired when switching projects
  (setq projectile-switch-project-action #'projectile-dired))


; olivetti
; https://www.reddit.com/r/emacs/comments/mgy54u/disable_basic_gui_elements_for_olivetti/
(define-minor-mode custom/olivetti-mode
   "Launch olivetti-mode and disable visual elements such as 'global-display-line-numbers-mode' and 'global-hl-line-mode'."
   :init-value nil
   :global nil

   (if custom/olivetti-mode
     (progn
       (olivetti-mode 1)
       (global-display-line-numbers-mode -1)
       (global-hl-line-mode -1)
       (setq-local mode-line-format nil))
     (olivetti-mode -1)
     (global-display-line-numbers-mode 1)
     (global-hl-line-mode 1)
     (kill-local-variable 'mode-line-format)
     (force-mode-line-update)))
(global-set-key (kbd "C-c o") 'custom/olivetti-mode)



;; Code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; lsp-mode (language server protocol)
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")   ; alternatively 'C-l'
  :config
  (setq lsp-idle-delay 0.5
        lsp-enable-symbol-highlighting t
        lsp-enable-snippet nil
        lsp-pyls-plugins-flake8-enabled t)
  (lsp-register-custom-settings
   '(("pyls.plugins.pyls_mypy.enabled" t t)
     ("pyls.plugins.pyls_mypy.live_mode" nil t)
     ("pyls.plugins.pyls_black.enabled" t t)
     ("pyls.plugins.pyls_isort.enabled" t t)

     ;; Disable these as they're duplicated by flake8
     ("pyls.plugins.pycodestyle.enabled" nil t)
     ("pyls.plugins.mccabe.enabled" nil t)
     ("pyls.plugins.pyflakes.enabled" nil t)))
  :hook
  ((python-mode . lsp)
   (lsp-mode . lsp-enable-which-key-integration))
  :bind (:map evil-normal-state-map
              ("gh" . lsp-describe-thing-at-point)))

(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

(use-package pyvenv
  :demand t
  :config
  (setq pyvenv-workon "emacs")  ; Default venv
  (pyvenv-tracking-mode 1))  ; Automatically use pyvenv-workon via dir-locals
