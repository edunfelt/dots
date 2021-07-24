;; $DOOMDIR/config.el -*- lexical-binding: t; -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                    ___ __                  __                              ;; 
;; .----.-----.-----.'  _|__|.-----.  .-----.|  |                             ;;
;; |  __|  _  |     |   _|  ||  _  |__|  -__||  |                             ;;
;; |____|_____|__|__|__| |__||___  |__|_____||__|                             ;;
;;                           |_____|                                          ;;
;; Emilia's config.el <3                                                      ;;
;; Edited: 2021-07-18                                                         ;;
;; Author: Emilia Dunfelt, edun@dunfelt.se                                    ;;
;;                                                                            ;;
;; Structure:                                                                 ;;
;; 1. General settings                                                        ;;
;; 2. Appearance                                                              ;;
;; 3. Org-stuff                                                               ;;
;; 4. Yasnippet                                                               ;;
;;                                                                            ;;
;; Based on the tecosaur's and sunnyhasija's Doom emacs configs               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1. General settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User configuration
(setq user-full-name "Emilia Dunfelt"
      user-mail-address "edun@dunfelt.se"
      display-line-numbers-type nil)

;; Settings
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
                 auto-save-default t                  ; autosaving
                 auto-save-timeout 180)

;; Mappings
(map! :map evil-window-map                            ; arrow key window navigation
      "SPC" #'rotate-layout
      ;; Navigation
      "<left>"     #'evil-window-left
      "<down>"     #'evil-window-down
      "<up>"       #'evil-window-up
      "<right>"    #'evil-window-right
      ;; Swapping windows
      "C-<left>"       #'+evil/window-move-left
      "C-<down>"       #'+evil/window-move-down
      "C-<up>"         #'+evil/window-move-up
      "C-<right>"      #'+evil/window-move-right)

;; Custom set variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-journal-date-format "%A, %d %B %Y" t)
 '(org-journal-date-prefix "#+TITLE: " t)
 '(org-journal-dir "/media/nas/home/00-09_Meta/01_Emacs/01.01_Org/journal/" t)
 '(org-journal-file-format "%Y-%m-%d.org" t)
 '(org-directory "/media/nas/home/00-09_Meta/01_Emacs/01.01_Org")
 '(org-agenda-files (list org-directory)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2. Appearance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Font
(setq doom-font (font-spec :family "Iosevka" :size 14)
      doom-big-font (font-spec :family "Iosevka Bold" :size 14))

;; Theme
(setq doom-theme 'doom-solarized-light)

;; Buffer names
(setq doom-fallback-buffer-name "✻ doom"
      +doom-dashboard-name "✻ doom")

;; Window title
(setq frame-title-format
      '(""
        (:eval
         (if (s-contains-p org-roam-directory (or buffer-file-name ""))
             (replace-regexp-in-string
              ".*/[0-9]*-?" "☰ "
              (subst-char-in-string ?_ ?  buffer-file-name))
           "%b"))
        (:eval
         (let ((project-name (projectile-project-name)))
           (unless (string= "-" project-name)
             (format (if (buffer-modified-p)  " ☆ %s" "  ✭  %s") project-name))))))

;; Fancy splash screen
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

;; override the first doom dashboard function
(setcar (nthcdr 0 +doom-dashboard-functions) #'+fl/doom-banner)
(setq +fl/splashcii-query "peanuts")

;; remove useful commands and disable modeline
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)
(add-hook! '+doom-dashboard-mode-hook (hide-mode-line-mode 1) (hl-line-mode -1))
(setq-hook! '+doom-dashboard-mode-hook evil-normal-state-cursor (list nil))

;; Tabs
(after! centaur-tabs
  (centaur-tabs-mode -1)
  (setq centaur-tabs-height 36
        centaur-tabs-set-icons t
        centaur-tabs-modified-marker "o"
        centaur-tabs-close-button "×"
        centaur-tabs-set-bar 'above
        centaur-tabs-gray-out-icons 'buffer))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3. Org-stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq org-directory "/media/nas/home/00-09_Meta/01_Emacs/01.01_Org"
      org-ellipsis " ▼ "
      org-adapt-indentation nil)

(use-package org-agenda
  :after org
  :custom
  (org-agenda-prefix-format '((agenda . " %i %-20:c%?-12t%-6e% s")
			      (todo   . " %i %-20:c %-6e")
			      (tags   . " %i %-20:c")
			      (search . " %i %-20:c"))))

(setq org-agenda-custom-commands
      '(("d" "Today's Tasks"
	 ((agenda "" ((org-agenda-span 1)
		      (org-agenda-overriding-header "Today's Tasks")))))))

;; Always change the task to IN-PROGRESS.
(setq org-clock-in-switch-to-state "STRT")

;; Use a function to decide what to change the state to.
(setq org-clock-in-switch-to-state #'edun/switch-task-on-clock-start)

(defun edun/switch-task-on-clock-start (task-state)
  "Change a task to 'STRT' when TASK-STATE is 'TODO'."
  (if (string= task-state "TODO")
      "STRT"
      task-state))

;; Org-Journal
(require 'org-id)
(use-package org-journal
  :init
  (setq org-journal-dir "/media/nas/home/00-09_Meta/01_Emacs/01.01_Org/journal/"
        org-journal-file-header "#+STARTUP: folded\n#+TITLE: "
        org-journal-file-format "%Y-%m-%d.org"
        org-journal-date-format "%A, %d %B %Y")
  :config
  (setq org-journal-find-file #'find-file-other-window )
  (setq org-journal-enable-agenda-integration t)
  (map! :map org-journal-mode-map
        "C-c n s" #'evil-save-modified-and-close )
  )


(defvar org-journal--date-location-scheduled-time nil)

(org-id-update-id-locations)

(set-file-template! 'org-mode :ignore t)
(use-package deft
  :bind
  (("C-c d" . deft))
  :custom
  ;; Set deft path to full path so that zetteldeft works.
  (deft-directory         zetteldirectory)
  (deft-extensions        '("org"))
  (deft-default-extension "org")
  (deft-recursive         t))

(setq deft-strip-summary-regexp
 (concat "\\("
         "[\n\t]" ;; blank
         "\\|^#\\+[a-zA-Z_]+:.*$" ;;org-mode metadata
         "\\)"))

(use-package! zetteldeft
             :init
             (map! :leader
                   :prefix "d"
                   :desc "deft" "d" #'deft
                   :desc "deft-refresh" "R" #'deft-refresh
                   :desc "zetteldeft-deft-new-search" "D" #'zetteldeft-deft-new-search
                   :desc "zetteldeft-search-at-point" "s" #'zetteldeft-search-at-point
                   :desc "zetteldeft-search-current-id" "c" #'zetteldeft-search-current-id
                   :desc "zetteldeft-follow-link" "f" #'zetteldeft-follow-link
                   :desc "zetteldeft-avy-file-search-ace-window" "F" #'zetteldeft-avy-file-search-ace-window
                   :desc "zetteldeft-avy-link-search" "l" #'zetteldeft-avy-link-search
                   :desc "zetteldeft-avy-tag-search" "t" #'zetteldeft-avy-tag-search
                   :desc "zetteldeft-tag-buffer" "T" #'zetteldeft-tag-buffer
                   :desc "zetteldeft-find-file-id-insert" "i" #'zetteldeft-find-file-id-insert
                   :desc "zetteldeft-find-file-full-title-insert" "I" #'zetteldeft-find-file-full-title-insert
                   :desc "zetteldeft-find-file" "o" #'zetteldeft-find-file
                   :desc "zetteldeft-new-file" "n" #'zetteldeft-new-file
                   :desc "zetteldeft-new-file-and-link" "N" #'zetteldeft-new-file-and-link
                   :desc "edun/deft-open-preview" "p" #'edun/deft-open-preview
                   :desc "edun/deft-open-other" "v" #'edun/deft-open-other
                   :desc "zetteldeft-file-rename" "r" #'zetteldeft-file-rename
                   :desc "zetteldeft-count-words" "x" #'zetteldeft-count-words)
             (setq zetteldirectory "/media/nas/home/00-09_Meta/01_Emacs/01.04_Zettelkasten")
             :config
             (defun edun/deft-open-preview ()
               (interactive)
               (deft-open-file-other-window))
             (defun edun/deft-open-other ()
               (interactive)
               (deft-open-file-other-window t))

             (font-lock-add-keywords
               'org-mode
               `((,zetteldeft-id-regex  . font-lock-warning-face)
                 (,zetteldeft-tag-regex . font-lock-warning-face))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4. Yasnippet
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq yas-snippet-dirs
      '("/media/nas/home/00-09_Meta/01_Emacs/01.02_Yasnippet"))
(yas-global-mode 1)
(defun my-yas-try-expanding-auto-snippets ()
  (when yas-minor-mode
    (let ((yas-buffer-local-condition ''(require-snippet-condition . auto)))
      (yas-expand))))
(add-hook 'post-command-hook #'my-yas-try-expanding-auto-snippets)

(setq-default mode-require-final-newline nil)
(setq yas-indent-line 'fixed)
(setq yas-also-auto-indent-first-line nil)
