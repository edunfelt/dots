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
;; Author: Emilia Dunfelt, edun@dunfelt.se                                    ;;

;; Appearance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(package! base16-theme)
(package! olivetti)


;; Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(package! evil-easymotion)


;; LaTeX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(package! auctex)
(package! cdlatex)
(package! org-fragtog)


;; Other org things
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(package! org-noter)
(package! org-roam-bibtex
  :recipe (:host github :repo "org-roam/org-roam-bibtex"))

;; When using org-roam via the `+roam` flag
(unpin! org-roam)
(package! org-roam-ui)


;; Other tools
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(package! anki-editor)

(package! pdf-tools)

(package! elfeed-protocol)
(package! elfeed-goodies)
