;; -*- no-byte-compile: t; -*- $DOOMDIR/packages.el ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                    __                                      __              ;;
;; .-----.---.-.----.|  |--.---.-.-----.-----.-----.  .-----.|  |             ;;
;; |  _  |  _  |  __||    <|  _  |  _  |  -__|__ --|__|  -__||  |             ;;
;; |   __|___._|____||__|__|___._|___  |_____|_____|__|_____||__|             ;;
;; |__|                          |_____|                                      ;;
;;                                                                            ;;
;; Emilia's packages.el                                                       ;;
;; Edited: 2021-04-09                                                         ;;
;; Author: Emilia Dunfelt, edun@dunfelt.se                                    ;;
;;                                                                            ;;
;; Structure:                                                                 ;;
;; 1. Appearance                                                              ;;
;; 2. Utilities                                                               ;;
;; 3. Research                                                                ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1. Appearance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(package! base16-theme)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2. Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(package! evil-easymotion)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3. Research
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(package! auctex)
(package! cdlatex)
(package! org-fragtog)
(package! org-journal)
(package! org-noter)
(package! pdf-tools)
(package! elfeed-protocol)
(package! elfeed-goodies)
(package! olivetti)

(package! org-roam-bibtex
  :recipe (:host github :repo "org-roam/org-roam-bibtex"))

;; When using org-roam via the `+roam` flag
(unpin! org-roam)
(package! org-roam-ui)

(package! anki-editor)
