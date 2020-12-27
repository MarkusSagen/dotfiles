;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Markus Sagen"
      user-mail-address "Markus.John.Sagen@gmail.com"

      doom-scratch-initial-major-mode 'lisp-interaction-mode
      doom-theme 'doom-dracula)


;; (setq doom-theme 'doom-one)
(custom-theme-set-faces! 'doom-dracula
  `(markdown-code-face :background ,(doom-darken 'bg 0.075))
  `(font-lock-variable-name-face :foreground ,(doom-lighten 'magenta 0.6)))

(setq doom-font (font-spec
                 :family "monospace" :size 20 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "sans" :size 24))

;; Doom snippets
(add-load-path!  "~/.doom.d/snippets")


;; Lanch in fullscreen
(add-to-list 'initial-frame-alist '(fullscreen . maximized))


;;; Set relative linenumbers
;; (setq display-line-numbers-type t)
(setq display-line-numbers-type 'relative)
(custom-set-faces! '(line-number :foreground "#3e66b3"))
(custom-set-faces! '(line-number-current-line :foreground "53ffef"))


(setq org_dir "~/Dropbox/org/")
(setq zot_bib "~/Dropbox/zotero/refs_mac.bib")
(setq bibtex_dir  "~/Dropbox/roam")
(setq bibtex_dir "~/Dropbox/roam/bibnotes.org")
(setq zot_bib org_dir)
(setq org-directory org_dir )
(setq deft-directory org_dir )
(setq org-roam-directory org_dir)


;; Spell checking
(use-package! flycheck
  :init (setq ispell-dictionary "en_US"))
(map! :leader
      :n "z" 'flycheck-correct-at-point)

;; Prevents some cases of Emacs flickering
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))




;; TODO
;; org-download
(require 'org-download)
;; Drag-and-drop to `dired`
(add-hook 'dired-mode-hook 'org-download-enable)



;; Searching
(global-set-key (kbd "C-\\") 'swiper)

;; Faster search with vim snipe for seraching
(require 'evil-snipe)
(evil-snipe-mode +1)
(evil-snipe-override-mode +1)
(setq evil-snipe-scope 'whole-visible)




;; Switch between windows quickly
(global-set-key (kbd "C-s-h") 'winner-undo)
(global-set-key (kbd "C-s-l") 'winner-redo)

;; Enable searches across all open windows, not just current one
(setq avy-all-windows t)

;; configure tramp
(after! tramp
  :config
  (setq recentf-auto-cleanup 'never)
  (setq projectile--mode-line "Projectile")
  (setq tramp-completion-reread-directory-timeout nil)
  (setq tramp-verbose 1)
  (setq vc-ignore-dir-regexp
        (format "\\(%s\\)\\|\\(%s\\)"
                vc-ignore-dir-regexp
                tramp-file-name-regexp)))



;; TODO Autocomplete
;; (add-hook! 'after-init-hook 'global-company-mode)
;; (global-company-mode t)
;; (setq company-minimum-prefix-length 1)
;; (setq company-idle-delay nil)

;; LSP ui
;; (setq lsp-ui-sideline-enable nil)
;; (setq lsp-enable-symbol-highlight nil)

;; ;; company quickhelp
;; (company-quickhelp-mode 1)
;; (setq company-quickhelp-delay 0)


;; Configure split
;; Switch to the new window after splitting
(setq evil-split-window-below t
      evil-vsplit-window-right t)

;; configure searches
(setq eval-ex-substitute-global t)




;; TODO org

(setq org-archive-location (concat org-directory ".archive/%s::" )
      org-startup-folded 'overview
      org-ellipsis " [...] " )

;; Better headings
(require 'org-bullets)
(add-hook! 'org-mode-hook #'org-bullets-mode)

;; deft:: serach org documents
(use-package! deft
  :after org
  :bind
  ("C-c n d" . deft)
  :custom
  (deft-recursive t)
  ;; (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
        (deft-directory org_dir))


;; org-journal
(use-package! org-journal
  :bind
  ("C-c n j" . org-journal-new-entry)
  :custom
  (org-journal-dir "~/Dropbox/org/journal")
  (org-journal-date-prefix "#+TITLE: ")
  (org-journal-file-format "%Y-%m-%d.org")
  (org-journal-date-format "%A, %d %B %Y"))
(setq org-journal-enable-agenda-integration t)


;; org-agenda
(use-package! org-super-agenda
  :after org-agenda
  :init
  (setq org-super-agenda-groups '((:name "Today"
                                   :time-grid t
                                   :scheduled today)
                                  (:name "Due Today"
                                   :deadline today)
                                  (:name "Very Important"
                                   :priority "A")
                                  (:name "Little Important"
                                   :priority "B")
                                  (:name "Not Important"
                                   :priority "C")
                                  (:name "Overdue"
                                   :deadline past)
                                  (:name "Future"
                                   :deadline future)))
  :config
    (org-super-agenda-mode))


;; ;; org-roam
(use-package! org-roam
  :ensure t
  :custom (org-roam-directory org_dir)
  :bind (:map org-roam-mode-map
         (("C-c n l" . org-roam)
          ("C-c n f" . org-roam-find-file)
          ("C-c n g" . org-roam-graph-show))
         :map org-mode-map
         (("C-c n i" . org-roam-insert))
         (("C-c n I" . org-roam-insert-immediate))))




;; TODO UI

;; Prevents some cases of Emacs flickering
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

;; enable icons
(use-package! mode-icons-mode
  :defer t
  :commands (mode-icons-mode))

;; Add icons to company help
(use-package! company-box
  :hook (company-mode . company-box-mode))

;; Show LSP errors in
(with-eval-after-load 'lsp-mode
  (setq lsp-modeline-diagnostics-scope :workspace))

;; UI:: popup window
(after! vterm
  ;; Spc o t   = "+vterm/toggle"
  (set-popup-rule! "*doom:vterm-popup:main" :size 0.32 :vslot -4 :select t :quit nil :ttl 0 :side 'right))
(set-popup-rule! "^\\*Org Agenda" :side 'bottom :size 0.90 :select t :ttl nil)
(set-popup-rule! "^CAPTURE.*\\.org$" :side 'bottom :size 0.90 :select t :ttl nil)




;; TODO config

;; Disable arrow keys
(global-unset-key (kbd "<left>"))
(global-unset-key (kbd "<right>"))
(global-unset-key (kbd "<up>"))
(global-unset-key (kbd "<down>"))


;; Add back a smarter emacs keybinding of move to begining and end of line
;; C-a (beginning),  C-e (end)
(defun my-move-beginning-of-line (arg)
  (interactive "P")
  (when (bolp) (previous-line (if arg -1 1)))
  (move-beginning-of-line nil))

(defun my-move-end-of-line (arg)
  (interactive "P")
  (when (eolp) (forward-line (if arg -1 1)))
  (move-end-of-line nil))

(global-set-key [remap move-beginning-of-line] #'my-move-beginning-of-line)
(global-set-key [remap move-end-of-line] #'my-move-end-of-line)





;; Move to end and insert newline
;; (global-set-key (kbd "C-;") 'move-end-of-line)
(global-set-key (kbd "C-;")
                (lambda ()
                  (interactive)
                    (call-interactively 'move-end-of-line)
                    (insert "  ")))

;; (global-set-key (kbd "C-;") 'move-end-of-line)
(global-set-key (kbd "C-j") '+default/newline-below)

;; Insert simicolon
;; TODO semicolons automatic on save in C/C++, C#, Rust, Java
(global-set-key (kbd "M-;")
                (lambda ()
                  (interactive)
                  (save-excursion
                    (call-interactively 'move-end-of-line)
                    (insert ";"))))


(map! :ne "M-/" #'comment-or-uncomment-region)
(map! :ne "SPC / r" #'deadgrep)
(map! :ne "SPC m m" #'org-latex-preview)

;; Make the '*' character easier to reach
(global-set-key (kbd "M-\\") (kbd "*"))
(global-set-key (kbd "C-|") (kbd "*"))

;; Make the  '-' character easier to reach
(global-set-key (kbd "C-?") (kbd "-"))


;; Move between buffers
(map! :n "M-<" #'evil-prev-buffer)
(map! :n "M->" #'evil-next-buffer)
(map! :n "M-(" #'evil-next-close-paren)
(map! :n "M-)" #'evil-previous-open-paren)

;; Move Org-items with Vim keybindings
(after! org (map! :map org-mode-map
                  :n "M-j" #'org-metadown
                  :n "M-k" #'org-metaup))


;; ;; Re-map C-c commands to M-c and set C-c to ESC
;; ;; (unmap! doom-leader-map "C-c")
;; (defun setup-input-decode-map ()
;;   (define-key input-decode-map (kbd "C-e") (kbd "C-c")))
;; (setup-input-decode-map)
;; (add-hook 'tty-setup-hook #'setup-input-decode-map)

(setq evil-escape-key-sequence "jk")
(setq-default evil-escape-delay 0.2)


;; From hlissner
;; Macros for editing dotfiles and show keys used
(map! :n [tab] (cmds! (and (featurep! :editor fold)
                           (save-excursion (end-of-line) (invisible-p (point))))
                      #'+fold/toggle
                      (fboundp 'evil-jump-item)
                      #'evil-jump-item)
      :v [tab] (cmds! (and (bound-and-true-p yas-minor-mode)
                           (or (eq evil-visual-selection 'line)
                               (not (memq (char-after) (list ?\( ?\[ ?\{ ?\} ?\] ?\))))))
                      #'yas-insert-snippet
                      (fboundp 'evil-jump-item)
                      #'evil-jump-item)

      :leader
      "h L" #'global-keycast-mode ;; Displays the macros used when working with a file
      "f t" #'find-in-dotfiles ;; TODO
      "f T" #'browse-dotfiles) ;; TODO




;; (use-package evil
;;  :ensure t
;;  :config
;;  (evil-mode 1)
 ;; (define-key evil-insert-state-map "kj" 'evil-normal-state)
 ;; (define-key evil-insert-state-map "\C-c" 'evil-normal-state)
 ;; (define-key evil-insert-state-map "jk" 'evil-normal-state))
;; (global-set-key (kbd "C-c") ')






;; TODO for pdfs and Bibtex

;; ;; Using org-present for presenting .org slides
;; (eval-after-load "org-present"
;;   '(progn
;;      (add-hook 'org-present-mode-hook
;;                (lambda ()
;;                  (org-present-big)
;;                  (org-display-inline-images)
;;                  (org-present-hide-cursor)
;;                  (org-present-read-only)))
;;      (add-hook 'org-present-mode-quit-hook
;;                (lambda ()
;;                  (org-present-small)
;;                  (org-remove-inline-images)
;;                  (org-present-show-cursor)
;;                  (org-present-read-write)))))


;; ;; `Bibtex-Config'
;; ;; ;; Add Bibtex references search
;; (autoload 'helm-bibtex "helm-bibtex" "" t)
;; (setq
;;  ;; bibtex-completion-notes-path (concat (getenv "HOME") "/org/")
;;  ;; bibtex-completion-bibliography (concat (getenv "HOME") "/org/refs/refs.bib")
;;  ;; bibtex-completion-pdf-field "file"
;;  ;; bibtex-completion-library-path (concat (getenv "HOME") "/Zotero/storage/")
;;  bibtex-completion-notes-path   "~/Dropbox/org"
;;  bibtex-completion-bibliography "~/Dropbox/zotero/refs.bib"
;;  bibtex-completion-pdf-field "file"
;;  bibtex-completion-library-path "~/Dropbox/zotero/"
;;  bibtex-completion-pdf-open-function 'org-open-file
;;  bibtex-completion-notes-template-multiple-files
;;  (concat
;;   "#+TITLE: ${title}\n"
;;   "#+ROAM_KEY: cite:${=key=}\n"
;;   "* TODO Notes\n"
;;   ":PROPERTIES:\n"
;;   ":Custom_ID: ${=key=}\n"
;;   ":NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n"
;;   ":AUTHOR: ${author-abbrev}\n"
;;   ":JOURNAL: ${journaltitle}\n"
;;   ":DATE: ${date}\n"
;;   ":YEAR: ${year}\n"
;;   ":DOI: ${doi}\n"
;;   ":URL: ${url}\n"
;;   ":END:\n\n"
;;   ))


;; bibtex-completion
;; (setq bibtex-completion-format-citation-functions
;;       '((org-mode      . bibtex-completion-format-citation-org-link-to-PDF)
;;         (latex-mode    . bibtex-completion-format-citation-cite)
;;         (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
;;         (default       . bibtex-completion-format-citation-default)))


;; ;; org-ref
;; ;; TODO LOADS really slow for some reason...!
;; (use-package! org-ref
;;   :config
;;   (setq
;;    org-ref-completion-library 'org-ref-ivy-cite
;;    org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex
;;    org-ref-note-title-format "* TODO %y - %t\n :PROPERTIES:\n  :Custom_ID: %k\n  :NOTER_DOCUMENT: %F\n :ROAM_KEY: cite:%k\n  :AUTHOR: %9a\n  :JOURNAL: %j\n  :YEAR: %y\n  :VOLUME: %v\n  :PAGES: %p\n  :DOI: %D\n  :URL: %U\n :END:\n\n"
;;    org-ref-default-bibliography "~/Dropbox/zotero/refs.bib"
;;    org-ref-bibliography-notes   "~/Dropbox/org/Research-Notes/notes.org"
;;    org-ref-notes-directory      "~/Dropbox/org"
;;    org-ref-notes-function 'orb-edit-notes))



;; (after! org-roam
;;   (setq org-roam-graph-viewer "/usr/bin/open")
;;   (setq org-roam-ref-capture-templates
;;         '(("r" "ref" plain (function org-roam-capture--get-point)
;;            "%?"
;;            :file-name "websites/${slug}"
;;            :head "#+TITLE: ${title}
;;                  #+ROAM_KEY: ${ref}
;;                  - source :: ${ref}"
;;            :unnarrowed t)))
;;   (setq org-roam-capture-templates
;;         '(("d" "default" plain (function org-roam--capture-get-point)
;;            "%?"
;;            :file-name "${slug}"
;;            :head "#+TITLE: ${title}\n"
;;            :unnarrowed t)) ))


;; ;; Capture from websites
;; (after! org-roam
;;   (setq org-roam-capture-ref-templates
;;         '(("r" "ref" plain (function org-roam-capture--get-point)
;;            "%?"
;;            :file-name "websites/${slug}"
;;                           :head "#+TITLE: ${title}
;;     #+ROAM_KEY: ${ref}
;;     - source :: ${ref}"
;;                                          :unnarrowed t))))


;; (use-package! org-roam-bibtex
;;   :after org-roam
;;   :ensure t
;;   :hook (org-roam-mode . org-roam-bibtex-mode)
;;   :bind (:map org-mode-map
;;          (("C-c n a" . orb-note-actions)))
;;   :config
;;   (setq orb-preformat-keywords
;;         '(("citekey" . "=key=") "title" "url" "file" "author-or-editor" "keywords"))
;;   (setq orb-templates
;;         '(("r" "ref" plain (function org-roam-capture--get-point)
;;            ""
;;            :file-name "${slug}"
;;            :head "#+TITLE: ${title}\n#+ROAM_KEY: ${ref}
;; - tags ::
;; - keywords :: ${keywords}
;; * ${title}
;; :PROPERTIES:
;; :Custom_ID: ${citekey}
;; :URL: ${url}
;; :AUTHOR: ${author-or-editor}
;; :NOTER_DOCUMENT: %(orb-process-file-field \"${citekey}\")
;; :NOTER_PAGE:
;; :END:"))))



;; ;; bibtex-completion
;; (map! :ne "SPC n b" #'helm-bibtex)


;; ;; (setq bibtex-completion-bibliography "~/Dropbox/zotero/refs.bib"
;; ;;       bibtex-completion-library-path "~/Dropbox/zotero"
;; ;;       bibtex-completion-notes-path "~/Dropbox/org")

;; ;; ;; open pdf with system pdf viewer (works on mac)
;; ;; (setq bibtex-completion-pdf-open-function
;; ;;   (lambda (fpath)
;; ;;     (start-process "open" "*open*" "open" fpath)))
;; ;; (setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f"))


;; ;; org-noter
;; (use-package org-noter
;;   :after org
;;   :ensure t
;;   :bind (:map org-noter-doc-mode-map
;;          (("C-c i" . org-noter-insert-note)))
;;   :config
;;   (setq
;;    org-noter-always-create-frame nil
;;    org-noter-hide-other nil
;;    org-noter-doc-split-fraction '(0.4 0.6)
;;    ;; org-noter-notes-search-path (list (concat (getenv "HOME") "/org/"))))
;;    org-noter-notes-search-path  (list "~/Dropbox/org/")))



;; ;; TODO find how to install .el files
;; ;; ;; Linkage and highlight in pdf files
;; ;; ;; (use-package org-pdftools
;; ;;   ;; :hook (org-load . org-pdftools-setup-link))
;; ;;
;; ;; (use-package org-noter-pdftools
;; ;; :after org-noter
;; ;; :config
;; ;; (with-eval-after-load 'pdf-annot
;; ;; (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))


;; ;; ;; org-to-html
;; ;; ;; modify path and mathml
;; ;; ;; Makes MathJax and math export look correct again when exporting from org mode
;; ;; ;; https://emacs.stackexchange.com/questions/31271/siunitx-mathjax-org-mode-and-html-export
;; ;; (setq org-html-mathjax-options
;; ;;       '((path "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML")
;; ;;         (scale "100")
;; ;;         (align "center")
;; ;;         (indent "2em")
;; ;;         (mathml t)))

;; ;; (setq org-html-mathjax-template
;; ;;             "<script type=\"text/javascript\" src=\"%PATH\"></script>
;; ;; <script type=\"text/javascript\">
;; ;; <!--/*--><![CDATA[/*><!--*/
;; ;;     MathJax.Hub.Config({
;; ;;         jax: [\"input/TeX\", \"output/HTML-CSS\"],
;; ;;         extensions: [\"tex2jax.js\",\"TeX/AMSmath.js\",\"TeX/AMSsymbols.js\",
;; ;;                      \"TeX/noUndefined.js\", \"[Contrib]/siunitx/siunitx.js\", \"[Contrib]/mhchem/mhchem.js\"],
;; ;;         tex2jax: {
;; ;;             inlineMath: [ [\"\\\\(\",\"\\\\)\"] ],
;; ;;             displayMath: [ ['$$','$$'], [\"\\\\[\",\"\\\\]\"], [\"\\\\begin{displaymath}\",\"\\\\end{displaymath}\"] ],
;; ;;             skipTags: [\"script\",\"noscript\",\"style\",\"textarea\",\"pre\",\"code\"],
;; ;;             ignoreClass: \"tex2jax_ignore\",
;; ;;             processEscapes: false,
;; ;;             processEnvironments: true,
;; ;;             preview: \"TeX\"
;; ;;         },
;; ;;         TeX: {extensions: [\"AMSmath.js\",\"AMSsymbols.js\",  \"[Contrib]/siunitx/siunitx.js\", \"[Contrib]/mhchem/mhchem.js\"]},
;; ;;         showProcessingMessages: true,
;; ;;         displayAlign: \"%ALIGN\",
;; ;;         displayIndent: \"%INDENT\",
;; ;;         \"HTML-CSS\": {
;; ;;              scale: %SCALE,
;; ;;              availableFonts: [\"STIX\",\"TeX\"],
;; ;;              preferredFont: \"TeX\",
;; ;;              webFont: \"TeX\",
;; ;;              imageFont: \"TeX\",
;; ;;              showMathMenu: true,
;; ;;         },
;; ;;         MMLorHTML: {
;; ;;              prefer: {
;; ;;                  MSIE:    \"MML\",
;; ;;                  Firefox: \"MML\",
;; ;;                  Opera:   \"HTML\",
;; ;;                  other:   \"HTML\"
;; ;;              }
;; ;;         }
;; ;;     });
;; ;; /*]]>*///-->
;; ;; </script>")

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Markus Sagen"
      user-mail-address "Markus.John.Sagen@gmail.com"

      doom-scratch-initial-major-mode 'lisp-interaction-mode
      doom-theme 'doom-dracula)


;; (setq doom-theme 'doom-one)
(custom-theme-set-faces! 'doom-dracula
  `(markdown-code-face :background ,(doom-darken 'bg 0.075))
  `(font-lock-variable-name-face :foreground ,(doom-lighten 'magenta 0.6)))

(setq doom-font (font-spec
                 :family "monospace" :size 20 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "sans" :size 24))


;; Doom snippets
(add-load-path!  "~/.doom.d/snippets")


;; Lanch in fullscreen
(add-to-list 'initial-frame-alist '(fullscreen . maximized))


;;; Set relative linenumbers
;; (setq display-line-numbers-type t)
(setq display-line-numbers-type 'relative)
(custom-set-faces! '(line-number :foreground "#3e66b3"))
(custom-set-faces! '(line-number-current-line :foreground "53ffef"))


(setq org_dir "~/Dropbox/org/")
(setq zot_bib "~/Dropbox/zotero/refs_mac.bib")
(setq bibtex_dir  "~/Dropbox/roam")
(setq bibtex_dir "~/Dropbox/roam/bibnotes.org")
(setq zot_bib org_dir)
(setq org-directory org_dir )
(setq deft-directory org_dir )
(setq org-roam-directory org_dir)


;; Spell checking
(use-package! flycheck
  :init (setq ispell-dictionary "en_US"))
(map! :leader
      :n "z" 'flycheck-correct-at-point)

;; Prevents some cases of Emacs flickering
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

;; Copy and paste  TODO
;; (global-set-key (kbd "M-v") '+default/yank-pop)


;; TODO
;; org-download
(require 'org-download)
;; Drag-and-drop to `dired`
(add-hook 'dired-mode-hook 'org-download-enable)



;; Searching
(global-set-key (kbd "C-\\") 'swiper)

;; Faster search with vim snipe for seraching
(require 'evil-snipe)
(evil-snipe-mode +1)
(evil-snipe-override-mode +1)
(setq evil-snipe-scope 'whole-visible)




;; Switch between windows quickly
(global-set-key (kbd "C-s-h") 'winner-undo)
(global-set-key (kbd "C-s-l") 'winner-redo)

;; Enable searches across all open windows, not just current one
(setq avy-all-windows t)

;; configure tramp
(after! tramp
  :config
  (setq recentf-auto-cleanup 'never)
  (setq projectile--mode-line "Projectile")
  (setq tramp-completion-reread-directory-timeout nil)
  (setq tramp-verbose 1)
  (setq vc-ignore-dir-regexp
        (format "\\(%s\\)\\|\\(%s\\)"
                vc-ignore-dir-regexp
                tramp-file-name-regexp)))



;; TODO Autocomplete
;; (add-hook! 'after-init-hook 'global-company-mode)
;; (global-company-mode t)
;; (setq company-minimum-prefix-length 1)
;; (setq company-idle-delay nil)

;; LSP ui
;; (setq lsp-ui-sideline-enable nil)
;; (setq lsp-enable-symbol-highlight nil)

;; ;; company quickhelp
;; (company-quickhelp-mode 1)
;; (setq company-quickhelp-delay 0)


;; Configure split
;; Switch to the new window after splitting
(setq evil-split-window-below t
      evil-vsplit-window-right t)

;; configure searches
(setq eval-ex-substitute-global t)




;; Async buffers
(defun async-shell-command-no-window
    (command)
  (interactive)
  (let
      ((display-buffer-alist
        (list
         (cons
          "\\*Async Shell Command\\*.*"
          (cons #'display-buffer-no-window nil)))))
    (async-shell-command
     command)))




;; TODO org

(setq org-archive-location (concat org-directory ".archive/%s::" )
      org-startup-folded 'overview
      org-ellipsis " [...] " )

;; Better headings
(require 'org-bullets)
(add-hook! 'org-mode-hook #'org-bullets-mode)

;; deft:: serach org documents
(use-package! deft
  :after org
  :bind
  ("C-c n d" . deft)
  :custom
  (deft-recursive t)
  ;; (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
        (deft-directory org_dir))


;; org-journal
(use-package! org-journal
  :bind
  ("C-c n j" . org-journal-new-entry)
  :custom
  (org-journal-dir "~/Dropbox/org/journal")
  (org-journal-date-prefix "#+TITLE: ")
  (org-journal-file-format "%Y-%m-%d.org")
  (org-journal-date-format "%A, %d %B %Y"))
(setq org-journal-enable-agenda-integration t)


;; org-agenda
(use-package! org-super-agenda
  :after org-agenda
  :init
  (setq org-super-agenda-groups '((:name "Today"
                                   :time-grid t
                                   :scheduled today)
                                  (:name "Due Today"
                                   :deadline today)
                                  (:name "Very Important"
                                   :priority "A")
                                  (:name "Little Important"
                                   :priority "B")
                                  (:name "Not Important"
                                   :priority "C")
                                  (:name "Overdue"
                                   :deadline past)
                                  (:name "Future"
                                   :deadline future)))
  :config
    (org-super-agenda-mode))


;; ;; org-roam
(use-package! org-roam
  :ensure t
  :custom (org-roam-directory org_dir)
  :bind (:map org-roam-mode-map
         (("C-c n l" . org-roam)
          ("C-c n f" . org-roam-find-file)
          ("C-c n g" . org-roam-graph-show))
         :map org-mode-map
         (("C-c n i" . org-roam-insert))
         (("C-c n I" . org-roam-insert-immediate))))



;; Lookup
 (map! :leader
       (:prefix ("l" . "lookup")
        :desc "Lookup Online" "o" '+lookup/online
        :desc "Lookup Implementation" "i" '+lookup/implementations
        :desc "Lookup Synonyms" "s" '+lookup/synonyms
        :desc "Lookup Definition" "d" '+lookup/definition
        :desc "Lookup Type Definition" "t" '+lookup/type-definition
        :desc "Lookup Documentation" "D" '+lookup/documentation
        :desc "Lookup File" "f" '+lookup/file))






;; TODO UI

;; Prevents some cases of Emacs flickering
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

;; enable icons
(use-package! mode-icons-mode
  :defer t
  :commands (mode-icons-mode))

;; Add icons to company help
(use-package! company-box
  :hook (company-mode . company-box-mode))

;; Show LSP errors in
(with-eval-after-load 'lsp-mode
  (setq lsp-modeline-diagnostics-scope :workspace))

;; UI:: popup window
(after! vterm
  ;; Spc o t   = "+vterm/toggle"
  (set-popup-rule! "*doom:vterm-popup:main" :size 0.32 :vslot -4 :select t :quit nil :ttl 0 :side 'right))
(set-popup-rule! "^\\*Org Agenda" :side 'bottom :size 0.90 :select t :ttl nil)
(set-popup-rule! "^CAPTURE.*\\.org$" :side 'bottom :size 0.90 :select t :ttl nil)




;; TODO config

;; Disable arrow keys
(global-unset-key (kbd "<left>"))
(global-unset-key (kbd "<right>"))
(global-unset-key (kbd "<up>"))
(global-unset-key (kbd "<down>"))


;; Add back a smarter emacs keybinding of move to begining and end of line
;; C-a (beginning),  C-e (end)
(defun my-move-beginning-of-line (arg)
  (interactive "P")
  (when (bolp) (previous-line (if arg -1 1)))
  (move-beginning-of-line nil))

(defun my-move-end-of-line (arg)
  (interactive "P")
  (when (eolp) (forward-line (if arg -1 1)))
  (move-end-of-line nil))

(global-set-key [remap move-beginning-of-line] #'my-move-beginning-of-line)
(global-set-key [remap move-end-of-line] #'my-move-end-of-line)





;; Move to end and insert newline
;; (global-set-key (kbd "C-;") 'move-end-of-line)
(global-set-key (kbd "C-;")
                (lambda ()
                  (interactive)
                    (call-interactively 'move-end-of-line)
                    (insert "  ")))

;; (global-set-key (kbd "C-;") 'move-end-of-line)
(global-set-key (kbd "C-j") '+default/newline-below)

;; Insert simicolon
;; TODO semicolons automatic on save in C/C++, C#, Rust, Java
(global-set-key (kbd "M-;")
                (lambda ()
                  (interactive)
                  (save-excursion
                    (call-interactively 'move-end-of-line)
                    (insert ";"))))


(map! :ne "M-/" #'comment-or-uncomment-region)
(map! :ne "SPC / r" #'deadgrep)
(map! :ne "SPC m m" #'org-latex-preview)

;; Make the '*' character easier to reach
(global-set-key (kbd "M-\\") (kbd "*"))
(global-set-key (kbd "C-|") (kbd "*"))

;; Make the  '-' character easier to reach
(global-set-key (kbd "C-?") (kbd "-"))


;; Move between buffers
(map! :n "M-<" #'evil-prev-buffer)
(map! :n "M->" #'evil-next-buffer)
(map! :n "M-(" #'evil-next-close-paren)
(map! :n "M-)" #'evil-previous-open-paren)

;; Move Org-items with Vim keybindings
(after! org (map! :map org-mode-map
                  :n "M-j" #'org-metadown
                  :n "M-k" #'org-metaup))


;; ;; Re-map C-c commands to M-c and set C-c to ESC
;; ;; (unmap! doom-leader-map "C-c")
;; (defun setup-input-decode-map ()
;;   (define-key input-decode-map (kbd "C-e") (kbd "C-c")))
;; (setup-input-decode-map)
;; (add-hook 'tty-setup-hook #'setup-input-decode-map)

(setq evil-escape-key-sequence "jk")
(setq-default evil-escape-delay 0.2)


;; From hlissner
;; Macros for editing dotfiles and show keys used
(map! :n [tab] (cmds! (and (featurep! :editor fold)
                           (save-excursion (end-of-line) (invisible-p (point))))
                      #'+fold/toggle
                      (fboundp 'evil-jump-item)
                      #'evil-jump-item)
      :v [tab] (cmds! (and (bound-and-true-p yas-minor-mode)
                           (or (eq evil-visual-selection 'line)
                               (not (memq (char-after) (list ?\( ?\[ ?\{ ?\} ?\] ?\))))))
                      #'yas-insert-snippet
                      (fboundp 'evil-jump-item)
                      #'evil-jump-item)

      :leader
      "h L" #'global-keycast-mode ;; Displays the macros used when working with a file
      "f t" #'find-in-dotfiles ;; TODO
      "f T" #'browse-dotfiles) ;; TODO




;; (use-package evil
;;  :ensure t
;;  :config
;;  (evil-mode 1)
 ;; (define-key evil-insert-state-map "kj" 'evil-normal-state)
 ;; (define-key evil-insert-state-map "\C-c" 'evil-normal-state)
 ;; (define-key evil-insert-state-map "jk" 'evil-normal-state))
;; (global-set-key (kbd "C-c") ')






;; TODO for pdfs and Bibtex

;; ;; Using org-present for presenting .org slides
;; (eval-after-load "org-present"
;;   '(progn
;;      (add-hook 'org-present-mode-hook
;;                (lambda ()
;;                  (org-present-big)
;;                  (org-display-inline-images)
;;                  (org-present-hide-cursor)
;;                  (org-present-read-only)))
;;      (add-hook 'org-present-mode-quit-hook
;;                (lambda ()
;;                  (org-present-small)
;;                  (org-remove-inline-images)
;;                  (org-present-show-cursor)
;;                  (org-present-read-write)))))


;; ;; `Bibtex-Config'
;; ;; ;; Add Bibtex references search
;; (autoload 'helm-bibtex "helm-bibtex" "" t)
;; (setq
;;  ;; bibtex-completion-notes-path (concat (getenv "HOME") "/org/")
;;  ;; bibtex-completion-bibliography (concat (getenv "HOME") "/org/refs/refs.bib")
;;  ;; bibtex-completion-pdf-field "file"
;;  ;; bibtex-completion-library-path (concat (getenv "HOME") "/Zotero/storage/")
;;  bibtex-completion-notes-path   "~/Dropbox/org"
;;  bibtex-completion-bibliography "~/Dropbox/zotero/refs.bib"
;;  bibtex-completion-pdf-field "file"
;;  bibtex-completion-library-path "~/Dropbox/zotero/"
;;  bibtex-completion-pdf-open-function 'org-open-file
;;  bibtex-completion-notes-template-multiple-files
;;  (concat
;;   "#+TITLE: ${title}\n"
;;   "#+ROAM_KEY: cite:${=key=}\n"
;;   "* TODO Notes\n"
;;   ":PROPERTIES:\n"
;;   ":Custom_ID: ${=key=}\n"
;;   ":NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n"
;;   ":AUTHOR: ${author-abbrev}\n"
;;   ":JOURNAL: ${journaltitle}\n"
;;   ":DATE: ${date}\n"
;;   ":YEAR: ${year}\n"
;;   ":DOI: ${doi}\n"
;;   ":URL: ${url}\n"
;;   ":END:\n\n"
;;   ))


;; bibtex-completion
;; (setq bibtex-completion-format-citation-functions
;;       '((org-mode      . bibtex-completion-format-citation-org-link-to-PDF)
;;         (latex-mode    . bibtex-completion-format-citation-cite)
;;         (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
;;         (default       . bibtex-completion-format-citation-default)))


;; ;; org-ref
;; ;; TODO LOADS really slow for some reason...!
;; (use-package! org-ref
;;   :config
;;   (setq
;;    org-ref-completion-library 'org-ref-ivy-cite
;;    org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex
;;    org-ref-note-title-format "* TODO %y - %t\n :PROPERTIES:\n  :Custom_ID: %k\n  :NOTER_DOCUMENT: %F\n :ROAM_KEY: cite:%k\n  :AUTHOR: %9a\n  :JOURNAL: %j\n  :YEAR: %y\n  :VOLUME: %v\n  :PAGES: %p\n  :DOI: %D\n  :URL: %U\n :END:\n\n"
;;    org-ref-default-bibliography "~/Dropbox/zotero/refs.bib"
;;    org-ref-bibliography-notes   "~/Dropbox/org/Research-Notes/notes.org"
;;    org-ref-notes-directory      "~/Dropbox/org"
;;    org-ref-notes-function 'orb-edit-notes))



;; (after! org-roam
;;   (setq org-roam-graph-viewer "/usr/bin/open")
;;   (setq org-roam-ref-capture-templates
;;         '(("r" "ref" plain (function org-roam-capture--get-point)
;;            "%?"
;;            :file-name "websites/${slug}"
;;            :head "#+TITLE: ${title}
;;                  #+ROAM_KEY: ${ref}
;;                  - source :: ${ref}"
;;            :unnarrowed t)))
;;   (setq org-roam-capture-templates
;;         '(("d" "default" plain (function org-roam--capture-get-point)
;;            "%?"
;;            :file-name "${slug}"
;;            :head "#+TITLE: ${title}\n"
;;            :unnarrowed t)) ))


;; ;; Capture from websites
;; (after! org-roam
;;   (setq org-roam-capture-ref-templates
;;         '(("r" "ref" plain (function org-roam-capture--get-point)
;;            "%?"
;;            :file-name "websites/${slug}"
;;                           :head "#+TITLE: ${title}
;;     #+ROAM_KEY: ${ref}
;;     - source :: ${ref}"
;;                                          :unnarrowed t))))


;; (use-package! org-roam-bibtex
;;   :after org-roam
;;   :ensure t
;;   :hook (org-roam-mode . org-roam-bibtex-mode)
;;   :bind (:map org-mode-map
;;          (("C-c n a" . orb-note-actions)))
;;   :config
;;   (setq orb-preformat-keywords
;;         '(("citekey" . "=key=") "title" "url" "file" "author-or-editor" "keywords"))
;;   (setq orb-templates
;;         '(("r" "ref" plain (function org-roam-capture--get-point)
;;            ""
;;            :file-name "${slug}"
;;            :head "#+TITLE: ${title}\n#+ROAM_KEY: ${ref}
;; - tags ::
;; - keywords :: ${keywords}
;; * ${title}
;; :PROPERTIES:
;; :Custom_ID: ${citekey}
;; :URL: ${url}
;; :AUTHOR: ${author-or-editor}
;; :NOTER_DOCUMENT: %(orb-process-file-field \"${citekey}\")
;; :NOTER_PAGE:
;; :END:"))))



;; ;; bibtex-completion
;; (map! :ne "SPC n b" #'helm-bibtex)


;; ;; (setq bibtex-completion-bibliography "~/Dropbox/zotero/refs.bib"
;; ;;       bibtex-completion-library-path "~/Dropbox/zotero"
;; ;;       bibtex-completion-notes-path "~/Dropbox/org")

;; ;; ;; open pdf with system pdf viewer (works on mac)
;; ;; (setq bibtex-completion-pdf-open-function
;; ;;   (lambda (fpath)
;; ;;     (start-process "open" "*open*" "open" fpath)))
;; ;; (setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f"))


;; ;; org-noter
;; (use-package org-noter
;;   :after org
;;   :ensure t
;;   :bind (:map org-noter-doc-mode-map
;;          (("C-c i" . org-noter-insert-note)))
;;   :config
;;   (setq
;;    org-noter-always-create-frame nil
;;    org-noter-hide-other nil
;;    org-noter-doc-split-fraction '(0.4 0.6)
;;    ;; org-noter-notes-search-path (list (concat (getenv "HOME") "/org/"))))
;;    org-noter-notes-search-path  (list "~/Dropbox/org/")))



;; ;; TODO find how to install .el files
;; ;; ;; Linkage and highlight in pdf files
;; ;; ;; (use-package org-pdftools
;; ;;   ;; :hook (org-load . org-pdftools-setup-link))
;; ;;
;; ;; (use-package org-noter-pdftools
;; ;; :after org-noter
;; ;; :config
;; ;; (with-eval-after-load 'pdf-annot
;; ;; (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))


;; ;; ;; org-to-html
;; ;; ;; modify path and mathml
;; ;; ;; Makes MathJax and math export look correct again when exporting from org mode
;; ;; ;; https://emacs.stackexchange.com/questions/31271/siunitx-mathjax-org-mode-and-html-export
;; ;; (setq org-html-mathjax-options
;; ;;       '((path "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML")
;; ;;         (scale "100")
;; ;;         (align "center")
;; ;;         (indent "2em")
;; ;;         (mathml t)))

;; ;; (setq org-html-mathjax-template
;; ;;             "<script type=\"text/javascript\" src=\"%PATH\"></script>
;; ;; <script type=\"text/javascript\">
;; ;; <!--/*--><![CDATA[/*><!--*/
;; ;;     MathJax.Hub.Config({
;; ;;         jax: [\"input/TeX\", \"output/HTML-CSS\"],
;; ;;         extensions: [\"tex2jax.js\",\"TeX/AMSmath.js\",\"TeX/AMSsymbols.js\",
;; ;;                      \"TeX/noUndefined.js\", \"[Contrib]/siunitx/siunitx.js\", \"[Contrib]/mhchem/mhchem.js\"],
;; ;;         tex2jax: {
;; ;;             inlineMath: [ [\"\\\\(\",\"\\\\)\"] ],
;; ;;             displayMath: [ ['$$','$$'], [\"\\\\[\",\"\\\\]\"], [\"\\\\begin{displaymath}\",\"\\\\end{displaymath}\"] ],
;; ;;             skipTags: [\"script\",\"noscript\",\"style\",\"textarea\",\"pre\",\"code\"],
;; ;;             ignoreClass: \"tex2jax_ignore\",
;; ;;             processEscapes: false,
;; ;;             processEnvironments: true,
;; ;;             preview: \"TeX\"
;; ;;         },
;; ;;         TeX: {extensions: [\"AMSmath.js\",\"AMSsymbols.js\",  \"[Contrib]/siunitx/siunitx.js\", \"[Contrib]/mhchem/mhchem.js\"]},
;; ;;         showProcessingMessages: true,
;; ;;         displayAlign: \"%ALIGN\",
;; ;;         displayIndent: \"%INDENT\",
;; ;;         \"HTML-CSS\": {
;; ;;              scale: %SCALE,
;; ;;              availableFonts: [\"STIX\",\"TeX\"],
;; ;;              preferredFont: \"TeX\",
;; ;;              webFont: \"TeX\",
;; ;;              imageFont: \"TeX\",
;; ;;              showMathMenu: true,
;; ;;         },
;; ;;         MMLorHTML: {
;; ;;              prefer: {
;; ;;                  MSIE:    \"MML\",
;; ;;                  Firefox: \"MML\",
;; ;;                  Opera:   \"HTML\",
;; ;;                  other:   \"HTML\"
;; ;;              }
;; ;;         }
;; ;;     });
;; ;; /*]]>*///-->
;; ;; </script>")
