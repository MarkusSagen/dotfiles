;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Markus Sagen"
      user-mail-address "Markus.John.Sagen@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:

;;(setq doom-theme 'doom-vibrant)
(setq doom-theme 'doom-one)
(setq display-line-numbers-type 'relative)
(custom-set-faces! '(line-number :foreground "#3e66be"))
(custom-set-faces! '(line-number-current-line :foreground "#53ffef"))
;; C-h F for line-number will show you
;; the available facesyou can change.
;; line-number-current-line is also available and now that
;; I re-read your question probably what you want.


;; (setq ms/code-font "Fira Code")
;; (setq ms/regular-font "Helvetica Neue") ;; or Helvetiva Neue

;; (setq doom-font (font-spec :family ms/code-font :size 14)
;;       doom-variable-pitch-font (font-spec :family ms/regular-font)
;;       doom-unicode-font (font-spec :family ms/code-font)
;;       doom-big-font (font-spec :family ms/code-font :size 24))

(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(setq org_dir "~/Dropbox/org/")
(setq zot_bib "~/Dropbox/zotero/refs_mac.bib")
(setq bibtex_dir "~/Dropbox/org/roam/")
(setq bibtex_notes "~/Dropbox/org/roam/bibnotes.org")
(setq org-directory org_dir)
(setq deft-directory org_dir)
(setq org-roam-directory org_dir)

;; (setq org-hide-block-startup t)         ;; #+startup: hideblocks or showblocks
(setq org-startup-folded 'content)         ;; Start org in folded mode
(setq shell-command-switch "-ic")          ;; For interactable


(use-package! flyspell
  :init (setq ispell-dictionary "en_US"))


;; Nicer headings and bullets
(require 'org-bullets)
(add-hook 'org-mode-hook #'org-bullets-mode)


;; For line and quick search
(require 'evil-snipe)
(evil-snipe-mode +1)
(evil-snipe-override-mode +1)
;;Dictates the scope of searches, which can be one of:
;;      'line           search line after the cursor (this is vim-seek behavior) (default)
;;      'buffer         search rest of the buffer after the cursor (vim-sneak behavior)
;;      'visible        search rest of visible buffer (Is more performant than 'buffer, but
;;                      will not highlight/jump past the visible buffer)
;;      'whole-line     same as 'line, but highlight matches on either side of cursor
;;      'whole-buffer   same as 'buffer, but highlight *all* matches in buffer
;;      'whole-visible  same as 'visible, but highlight *all* visible matches in buffer"
(setq evil-snipe-scope 'whole-visible)


;; set M-s-left, right switch between projects
;; Improved buffer, undo and redo
(winner-mode 1)
(global-set-key (kbd "<C-s-left>") 'winner-undo)
(global-set-key (kbd "<C-s-right>") 'winner-redo)


;; When in writing mode, enable full screen
(setq writeroom-fullscreen-effect t)


(setq org-log-done 'time
      org-log-into-drawer t
      org-log-state-notes-insert-after-drawers nil)


(use-package! mode-icons-mode
  :defer t
  :commands (mode-icons-mode))


;; vterm and make popup open in right window and with correct size
;; popup rules: https://github.com/hlissner/doom-emacs/blob/207ce02a3371e0bcd67c90d1d7b00aa72c8eccf2/modules/ui/popup/README.org
(after! vterm
  ;; Spc o t  - "+vterm/toggle"
  (set-popup-rule! "*doom:vterm-popup:main" :size 0.32 :vslot -4 :select t :quit nil :ttl 0 :side 'right))
(set-popup-rule! "^\\*Org Agenda" :side 'bottom :size 0.90 :select t :ttl nil)
(set-popup-rule! "^CAPTURE.*\\.org$" :side 'bottom :size 0.90 :select t :ttl nil)


;; Make Tramp fast when used with projectile
(after! tramp
 :config
 (setq recentf-auto-cleanup 'never)
 (setq projectile-mode-line "Projectile")
 (setq tramp-completion-reread-directory-timeout nil)
 (setq tramp-verbose 1)
 (setq vc-ignore-dir-regexp
       (format "\\(%s\\)\\|\\(%s\\)"
               vc-ignore-dir-regexp
               tramp-file-name-regexp)))



;; Auto complete
;; Show icons when given company completions
(add-hook 'after-init-hook 'global-company-mode)
(global-company-mode t)
(setq company-minimum-prefix-length 1)
(setq company-idle-delay 0)

;; company-quickhelp
(company-quickhelp-mode 1)
(setq company-quickhelp-delay 0)

;; Anaconda
(add-hook! 'python-mode-hook 'anaconda-mode)

;; company anaconda
(require 'rx)
(add-to-list 'company-backends 'company-anaconda)

;; Add icons to company
(use-package! company-box
  :hook (company-mode . company-box-mode))

(setq company-minimum-prefix-length 1
      company-idle-delay 0.0) ;; default is 0.2

;; LSP handle errors inline
(with-eval-after-load 'lsp-mode
  ;; :global/:workspace/:file
  (setq lsp-modeline-diagnostics-scope :workspace))



;; Rust config and formatiing
;; setting up rust::     --> https://emacs-lsp.github.io/lsp-mode/page/lsp-rust/
;; Setup instructions::  --> https://github.com/rust-lang/rls
(add-hook 'before-save-hook (lambda () (when (eq 'rust-ode major-mode)
                                           (lsp-format-buffer))))



(defun rust-save-compile-and-run ()
  (interactive)
  (save-buffer)
  (if (locate-dominating-file (buffer-file-name) "Cargo.toml")
      (compile "cargo run")

    (compile
     (format "rustc %s & %s"
         (buffer-file-name)
         (file-name-sans-extension (buffer-file-name))))))
(add-hook 'rust-mode-hook
      (lambda ()
        (define-key rust-mode-map (kbd "<f5>") 'rust-save-compile-and-run)))








;; ;; Allow `rustic-cargo-run' to pass in input
;; (with-eval-after-load 'rust-mode
;;   (define-key rust-mode-map (kbd "C-t") 'my-cargo-run))
;;     (defun my-cargo-run ()
;;     "Build and run Rust code."
;;         (interactive)
;;         (cargo-process-build)
;;         (cargo-process-run)
;;         (let ((orig-win (selected-window))
;;         (run-win (display-buffer (get-buffer "*Cargo Run*") nil 'visible)))
;;         (select-window run-win)
;;         (comint-mode)
;;         (read-only-mode 0)
;;         (select-window orig-win)))




;; Better more to end
(global-set-key (kbd "C-/") 'comment-dwim)     ;; Add cooment to end of line
(global-set-key (kbd "C-;") 'move-end-of-line) ;; Move to end of line
(global-set-key (kbd "M-RET") '+default/newline-below)


;; Insert semicolon ';' at the end of the line
(global-set-key (kbd "M-;")
(lambda ()
  (interactive)
  ;; Keep cursor motion within this block (don't move the users cursor).
  (save-excursion
    ;; Typically mapped to the "End" key.
    (call-interactively 'move-end-of-line)
    (insert ";"))))



;; LaTeX mode
(add-hook! LaTeX-mode
        :append
        (set-company-backend! 'latex-mode
 '(company-dabbrev :with company-yasnippet)))






;; ORG
(use-package! org-journal
  :bind
  ("C-c n j" . org-journal-new-entry)
  :custom
  ;; (org-journal-dir (concat (getenv "HOME") "/org/journal"))
  (org-journal-dir "~/Dropbox/org/journal")
  (org-journal-date-prefix "#+TITLE: ")
  (org-journal-file-format "%Y-%m-%d.org")
  (org-journal-date-format "%A, %d %B %Y"))
(setq org-journal-enable-agenda-integration t)


(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)



;; Find and replace under cursor
;; replace current word or selection using vim style for evil mode
(defun evil-replace-word-selection()
"Find and replace with Vim global search"
(interactive)
(if (use-region-p)
    (let (
            (selection (buffer-substring-no-properties (region-beginning) (region-end))))
        (if (= (length selection) 0)
        (message "empty string")
        (evil-ex (concat "'<,'>s/" selection "/"))
        ))
  (evil-ex (concat "%s/" (thing-at-point 'word) "/"))))
(global-set-key (kbd "\C-cf") 'evil-replace-word-selection)



;; https://www.reddit.com/r/orgmode/comments/8n45ds/why_highlighting_text_is_so_painful_in_orgmode/
;; (setq org-hide-emphasis-markers t)           ;; hides emphasis of org commands
(setq org-emphasis-alist
      (quote (("*" bold)
              ("/" italic)
              ("_" underline)
              ("=" (:foreground "yellow" :background "black"))
              ("?" (:foreground "white" :background "green"))
              ("~" org-verbatim verbatim)
              ("+"
               (:strike-through t)))))

;; Find and replace improve with
(defun org-add-my-extra-markup ()
  "Add highlight emphasis: M-% "
  (add-to-list 'org-font-lock-extra-keywords
               '("[^\\w]\\(:\\[^\n\r\t]+:\\)[^\\w]"
                 (1 '(face highlight invisible nil)))))

(add-hook 'org-font-lock-set-keywords-hook #'org-add-my-extra-markup)







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


(setq org-log-done 'time
      org-log-into-drawer t
      org-log-state-notes-insert-after-drawers nil)


(use-package! mode-icons-mode
  :defer t
  :commands (mode-icons-mode))



;; Search across org and org-roam files
(use-package deft
  :after org
  :bind
  ("C-c n d" . deft)
  :commands deft
  :init
  (setq deft-default-extension "org"
        ;; de-couples filename and note title:
        deft-use-filename-as-title nil
        deft-use-filter-string-for-filename t
        ;; disable auto-save
        deft-auto-save-interval -1.0
        ;; converts the filter string into a readable file-name using kebab-case:
        deft-file-naming-rules
        '((noslash . "-")
          (nospace . "-")
          (case-fn . downcase)))
  :config
  (add-to-list 'deft-extensions "tex"))




;; Toggle for org-roam buffer
(setq org-roam-buffer-width 0.15)
(map! :leader
      (:when (featurep! :lang org +roam)
      ;; <leader> t --- toggle
      (:prefix-map ("t" . "toggle")
       :desc "Org-roam buffer" "q" #'org-roam-buffer-toggle-display)))
;; Remove open org-roam side buffer
(remove-hook 'find-file-hook #'+org-roam-open-buffer-maybe-h)


(use-package org-ref
    :config
    (setq
         org-ref-completion-library 'org-ref-helm-cite
         org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex
         org-ref-default-bibliography (list zot_bib)
         org-ref-bibliography-notes bibtex_notes
         org-ref-note-title-format "* TODO %y - %t\n :PROPERTIES:\n  :Custom_ID: %k\n  :NOTER_DOCUMENT: %F\n :ROAM_KEY: cite:%k\n  :AUTHOR: %9a\n  :JOURNAL: %j\n  :YEAR: %y\n  :VOLUME: %v\n  :PAGES: %p\n  :DOI: %D\n  :URL: %U\n :END:\n\n"
         org-ref-notes-directory bibtex_dir
         org-ref-notes-function 'orb-edit-notes))


(use-package helm-bibtex
  :ensure t
  :defer t
  :init
  ;; (my/leader-keys
  ;;   "ib" 'ivy-bibtex)
  (setq bibtex-completion-pdf-field "file"
	bibtex-completion-find-additional-pdfs t
	bibtex-completion-bibliography zot_bib
	bibtex-completion-notes-path bibtex_dir
	bibtex-completion-notes-template-one-file
	"\n* ${title} cite:${=key=}\n  :PROPERTIES:\n  :Custom_ID:  :${=key=}\n  :NOTER_DOCUMENT: ${file}\n  :END:\n\n"))



(use-package org-roam-bibtex
  :after (org-roam)
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :config
  (setq ;; org-roam-bibtex-preformat-keywords
        orb-preformat-keywords
   '("=key=" "title" "url" "file" "author-or-editor" "keywords"))
  (setq orb-templates
        '(("r" "ref" plain (function org-roam-capture--get-point)
           ""
           :file-name "${slug}"
           :head "#+TITLE: ${=key=}: ${title}\n#+ROAM_KEY: ${ref}

- tags ::
- keywords :: ${keywords}

\n* ${title}\n  :PROPERTIES:\n  :Custom_ID: ${=key=}\n  :URL: ${url}\n  :AUTHOR: ${author-or-editor}\n  :NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n  :NOTER_PAGE: \n  :END:\n\n"

           :unnarrowed t))))



(use-package org-noter
  :after (:any org pdf-view)
  :config
  (setq
   ;; The WM can handle splits
   org-noter-notes-window-location 'other-frame
   ;; Please stop opening frames
   org-noter-always-create-frame nil
   ;; I want to see the whole file
   org-noter-hide-other nil
   ;; Everything is relative to the main notes file
   org-noter-notes-search-path (list org_dir)))

;; org-noter
(use-package org-noter
  :after org
  :ensure t
  :bind (:map org-noter-doc-mode-map
         (("C-c i" . org-noter-insert-note)))
  :config
  (setq
   org-noter-always-create-frame nil
   org-noter-hide-other nil
   org-noter-doc-split-fraction '(0.4 0.6)
   ;; org-noter-notes-search-path (list (concat (getenv "HOME") "/org/"))))
   org-noter-notes-search-path  (list org_dir)))


;; ;; open pdf with system pdf viewer (works on mac)
;; (setq bibtex-completion-pdf-open-function
;;   (lambda (fpath)
;;     (start-process "open" "*open*" "open" fpath)))


;; ;; open pdf with system pdf viewer (works on mac)
;; (setq bibtex-completion-pdf-open-function
;;   (lambda (fpath)
;;     (start-process "open" "*open*" "open" fpath)))

;; (setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f"))





;; LATEX
(setq org-latex-compiler "xelatex")
(setq org-latex-pdf-process '("latexmk -xelatex -quiet -shell-escape -f %f"))
(setq org-preview-latex-image-directory "/tmp/ltximg/")
(setq-default TeX-engine 'xetex)
(setq-default TeX-PDF-mode t)


;; Set org to always preview latex formulas if in side buffer
(add-to-list 'org-roam-buffer-prepare-hook (lambda () (org--latex-preview-region (point-min) (point-max))) t)


;; ;; View PDFs in emacs
;; https://github.com/fuxialexander/org-pdftools
;; (use-package org-pdftools
;;   :hook (org-load . org-pdftools-setup-link))


;; (use-package org-noter-pdftools
;; ;   :after org-noter
;;   :config
;;   (with-eval-after-load 'pdf-annot
;;     (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))

;; (use-package org-noter-pdftools
;; :after org-noter
;; :config
;; (with-eval-after-load 'pdf-annot
;; (add-hook 'pdf-annot-a
;; ;; ;; Latex config
;; (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer) ;; revert pdf after compile
;; (setq TeX-view-program-selection '((output-pdf "PDF Tools"))) ;; use pdf-tools for viewing
;; (setq LaTeX-command "latex --synctex=1") ;; optional: enable synctex


;; Optional command to use in 'export-latex-pdf'
(defun my-kill-other-buffers ()
  "Kill all other buffers... and windows"
  (interactive)
  (delete-other-windows)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))


;; If Org-Roam active in window when open pdf, remove it
(defun my-kill-org-roam ()
  (interactive)
  (condition-case nil
    ;; https://github.com/org-roam/org-roam/blob/master/org-roam-buffer.el
    (org-roam-buffer-deactivate)
    ((debug error) nil) ))
    ;; (kill-buffer "*org-roam*")))


;; Export org to latex-pdf
(defun export-latex-pdf-on-start ()
  (let ((buffer (compilation-find-buffer)))
    (unless (get-buffer-process buffer)
      ;; (my-kill-other-buffers)
      (my-kill-org-roam)
      (org-pandoc-export-to-latex-pdf-and-open)
      (add-to-list 'org-file-apps
                   '("\\.pdf\\'" . (lambda (file link)
                                     (find-alternate-file-other-window link)))))))


;; Set mode in Org mode, when saving, export and open pdf side-by-side
(define-minor-mode my-org-latex-export-mode
  "Minor mode to automatically call `recompile' whenever the
current buffer is saved. When there is ongoing compilation,
nothing happens."
  :lighter " CoS"
    (if my-org-latex-export-mode
        ;; Try remove all other frames and buffers when loading
        (progn  (make-local-variable 'after-save-hook)
                (add-hook 'after-save-hook 'export-latex-pdf-on-start nil t))
      (kill-local-variable 'after-save-hook)))
(require 'ox-extra)
(ox-extras-activate '(latex-header-blocks ignore-headlines))





;; Actually start using templates
(after! org-capture
  ;; Firefox and Chrome
  (add-to-list 'org-capture-templates
               '("P" "Protocol" entry ; key, name, type
                 (file+headline +org-capture-notes-file "Inbox") ; target
                 "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?"
                 :prepend t ; properties
                 :kill-buffer t))
  (add-to-list 'org-capture-templates
               '("L" "Protocol Link" entry
                 (file+headline +org-capture-notes-file "Inbox")
                 "* %? [[%:link][%(transform-square-brackets-to-round-ones \"%:description\")]]\n"
                 :prepend t
                 :kill-buffer t)))


;; org-download
(require 'org-download)
;; Drag-and-drop to `dired`
(add-hook 'dired-mode-hook 'org-download-enable)


;; ;; TODO Add automatic find link name
;; ;; Pasting links gets truncated
;; (defun compress-org-link (arg)
;;   (interactive "P")
;;   (let ((url (thing-at-point 'url))
;;     (bounds (bounds-of-thing-at-point 'url)))
;;     (kill-region (car bounds) (cdr bounds))
;;     (insert (format "[[%s][%s]]" url (truncate-string-to-width url (if arg (prefix-numeric-value arg) 25) nil nil "...")))))



;; BIBTEX
(setq
 bibtex-completion-notes-path bibtex_dir ;(concat (getenv "HOME") "/Dropbox/org/Research-Notes/")
 bibtex-completion-bibliography zot_bib ;(concat (getenv "HOME") "/Dropbox/zotero/refs_mac.bib")
 bibtex-completion-pdf-field "file"
 bibtex-completion-notes-template-multiple-files
 (concat
  "#+TITLE: ${title}\n"
  "#+ROAM_KEY: cite:${=key=}\n"
  "* TODO Notes\n"
  ":PROPERTIES:\n"
  ":Custom_ID: ${=key=}\n"
  ":NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n"
  ":AUTHOR: ${author-abbrev}\n"
  ":JOURNAL: ${journaltitle}\n"
  ":DATE: ${date}\n"
  ":YEAR: ${year}\n"
  ":DOI: ${doi}\n"
  ":URL: ${url}\n"
  ":END:\n\n"))


;; REVEAL.JS
;; For presentations
(require 'ox-reveal)
  (use-package ox-reveal
  :ensure t
  :config
  (require 'ox-reveal)
    (setq org-reveal-root "http://cdn.jsdelivr.net/npm/reveal.js")
    (setq org-reveal-mathjax t)
)
    (use-package htmlize
    :ensure t)



;; Custom Key mappings
(map! :ne "SPC k" #'save-buffer)
(map! :ne "M-/" #'comment-or-uncomment-region)
(map! :ne "SPC / r" #'deadgrep)
(map! :ne "SPC m m" #'org-latex-preview)

;; Make the '*' character easier to reach
(global-set-key (kbd "M-\\") (kbd "*"))
(global-set-key (kbd "C-|") (kbd "*"))

;; Make the  '-' character easier to reach
(global-set-key (kbd "C-?") (kbd "-"))

;; Easier toggle Org-Header key
(map! :n "SPC M-\\" #'org-ctrl-c-star)

;; Move between buffers
(map! :n "M-<" #'evil-prev-buffer)
(map! :n "M->" #'evil-next-buffer)
(map! :n "M-(" #'evil-next-close-paren)
(map! :n "M-)" #'evil-previous-open-paren)

;; Move Org-items with Vim keybindings
(after! org (map! :map org-mode-map
                  :n "M-j" #'org-metadown
                  :n "M-k" #'org-metaup))







;; Lanaguage Specific

;; Babel
(require 'org-tempo)
(org-babel-do-load-languages
 'org-babel-do-load-languages
 '((emacs-lisp . t)
   (python . t)
   (jupyter-python . t)
   (julia . t)
   (jupyter-julia . t)
   (sh . t)
   (javascript .t)))
(setq org-confirm-babel-evaluate nil)


(add-to-list 'org-structure-template-alist '("make" . "src Makefile"))
(add-to-list 'org-structure-template-alist '("doc" . "src Dockerfile"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("ipy" . "src jupyter-python"))
(add-to-list 'org-structure-template-alist '("j" . "src julia"))
(add-to-list 'org-structure-template-alist '("ij" . "src jupyter-julia"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("sh". "src sh"))


;; Java
;; https://wrigstad.com/ioopm/editors.html
;; TODO https://emacs.stackexchange.com/questions/47933/are-there-any-tutorials-documentation-for-lsp-java
;; (use-package! meghanada
;;   :diminish meghanada-mode "Mm"
;;   :config
;;   (add-hook 'java-mode-hook
;;             (lambda ()
;;               (meghanada-mode t)
;;               (flycheck-mode t)
;;               (company-mode t)
;;               (add-hook 'before-save-hook 'delete-trailing-whitespace))))


;; (use-package lsp-mode
;;     :hook (java-mode . lsp)
;;     :commands lsp
;;     :config (setq lsp-enable-symbol-highlighting nil))

;;   (use-package company-lsp
;;     :config (push 'company-lsp company-backends))

;;   (use-package lsp-java
;;     :after lsp)

;; (use-package! lsp
;;   :config
;;   (add-hook! 'java-mode-hook
;;     (lambda ()
;;       (flycheck-mode t)
;;       (company-mode t)
;;       (lsp-mode t)
;;       (lsp-ui-mode t)
;;       (add-hook! 'before-save-hook 'delete-trailing-whitespace))))









;; ESS
(use-package! ein
  :config
  (setq ein:worksheet-endable-undo t)
  (setq ein:output-area-inlined-images t))


;; python


;; conda
(use-package! conda
  :config
  (lambda() conda-env-initialize-interactive-shell))

;; conda


;; c/c++





;; Here are some additional functions/macros that could help you configure Doom:
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
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
