;; $DOOMDIR/config.el -*- lexical-binding: t; -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                    ___ __                  __                              ;; 
;; .----.-----.-----.'  _|__|.-----.  .-----.|  |                             ;;
;; |  __|  _  |     |   _|  ||  _  |__|  -__||  |                             ;;
;; |____|_____|__|__|__| |__||___  |__|_____||__|                             ;;
;;                           |_____|                                          ;;
;; Emilia's config.el <3                                                      ;;
;; Edited: 2021-04-19                                                         ;;
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
      user-mail-address "edun@dunfelt.se")

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
                 auto-save-default t)                 ; autosaving

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
 '(org-journal-file-format "%Y-%m-%d.org" t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Keybindings
(map! :leader "f a"#'helm-bibtex)  ; "find article" : opens up helm bibtex for search.
(map! :leader "o n"#'org-noter)    ; "org noter"  : opens up org noter in a headline

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2. Appearance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Font
(setq doom-font (font-spec :family "Anonymice Nerd Font" :size 16)
      doom-big-font (font-spec :family "Anonymice Nerd Font" :size 20))

;; Theme
(load-theme 'base16-cupcake t)

;; Line numbers
(setq display-line-numbers-type t)

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
(setq org-directory "/media/nas/home/org/")
(setq org-agenda-files (list "/media/nas/home/00-09_Meta/01_Emacs/01.01_Org/uni.org"
                             "/media/nas/home/00-09_Meta/01_Emacs/01.01_Org/home.org"))

;; Helm-Bibtex
(use-package! helm-bibtex
  :after org
  :init
  :config
  )

(setq bibtex-format-citation-functions
      '((org-mode . (lambda (x) (insert (concat
                                         "\\cite{"
                                         (mapconcat 'identity x ",")
                                         "}")) ""))))
(setq
      bibtex-completion-pdf-field "file"
      bibtex-completion-bibliography
      '("/media/nas/home/00-09_Meta/03_Zotero/export.bib")
      bibtex-completion-library-path '("/media/nas/home/00-09_Meta/03_Zotero/")
      )

;; Org-Roam-Bibtex
(use-package! org-roam-bibtex
  :load-path "/media/nas/home/00-09_Meta/03_Zotero/export.bib" ;Modify with your own path
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :bind (:map org-mode-map
         (("C-c n a" . orb-note-actions))))
(setq orb-templates
      '(("r" "ref" plain (function org-roam-capture--get-point) ""
         :file-name "${citekey}"
         :head "#+TITLE: ${citekey}: ${title}\n#+ROAM_KEY: ${ref}\n" ; <--
         :unnarrowed t)))
(setq orb-preformat-keywords   '(("citekey" . "=key=") "title" "url" "file" "author-or-editor" "keywords"))

(setq orb-templates
      '(("n" "ref+noter" plain (function org-roam-capture--get-point)
         ""
         :file-name "${slug}"
         :head "#+TITLE: ${citekey}: ${title}\n#+ROAM_KEY: ${ref}\n#+ROAM_TAGS:

- tags ::
- keywords :: ${keywords}
\* ${title}
:PROPERTIES:
:Custom_ID: ${citekey}
:URL: ${url}
:AUTHOR: ${author-or-editor}
:NOTER_DOCUMENT: %(orb-process-file-field \"${citekey}\")
:NOTER_PAGE:
:END:")))

;; Org Roam
(setq org-roam-directory "/media/nas/home/00-09_Meta/01_Emacs/01.01_Org/org-roam")
(after! org-roam
        (map! :leader
            :prefix "n"
            :desc "org-roam" "l" #'org-roam
            :desc "org-roam-insert" "i" #'org-roam-insert
            :desc "org-roam-switch-to-buffer" "b" #'org-roam-switch-to-buffer
            :desc "org-roam-find-file" "f" #'org-roam-find-file
            :desc "org-roam-show-graph" "g" #'org-roam-show-graph
            :desc "org-roam-insert" "i" #'org-roam-insert
            :desc "org-roam-capture" "c" #'org-roam-capture))
(after! org-roam
      (setq org-roam-ref-capture-templates
            '(("r" "ref" plain (function org-roam-capture--get-point)
               "%?"
               :file-name "websites/${slug}"
               :head "#+TITLE: ${title}
    #+ROAM_KEY: ${ref}
    - source :: ${ref}"
               :unnarrowed t))))  ; capture template to grab websites. Requires org-roam protocol.

;; Org-Journal
(use-package org-journal
  :init
  (setq org-journal-dir "/media/nas/home/00-09_Meta/01_Emacs/01.01_Org/journal/"
        org-journal-file-header "#+STARTUP: folded\n#+TITLE: Weekly Journal\n"
        org-journal-file-format "%Y-%V-%m%d.org"
        org-journal-file-type 'weekly
        org-journal-date-prefix "* "
        org-journal-date-format "%A, %d")
  :config
  (setq org-journal-find-file #'find-file-other-window )
  (map! :map org-journal-mode-map
        "C-c n s" #'evil-save-modified-and-close )
  )

(setq org-journal-enable-agenda-integration t)

;; Deft
(use-package deft
      :after org
      :bind
      ("C-c n d" . deft)
      :custom
      (deft-recursive t)
      (deft-use-filter-string-for-filename t)
      (deft-default-extension "org")
      (deft-directory "/media/nas/home/00-09_Meta/01_Emacs/01.01_Org/org-roam/"))

;; Org-Roam-Server
(use-package! org-roam-server
  :after org-roam
  :config
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 8080
        org-roam-server-export-inline-images t
        org-roam-server-authenticate nil
        org-roam-server-label-truncate t
        org-roam-server-label-truncate-length 60
        org-roam-server-label-wrap-length 20)
  (defun org-roam-server-open ()
    "Ensure the server is active, then open the roam graph."
    (interactive)
    (org-roam-server-mode 1)
    (browse-url-xdg-open (format "http://localhost:%d" org-roam-server-port))))
(after! org-roam
  (org-roam-server-mode))

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
