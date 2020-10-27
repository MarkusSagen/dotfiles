;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-


;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))
;;
;; highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(setq user-full-name "Markus Sagen"
      user-mail-address "Markus.John.Sagen@gmail.com")


;; (setq doom-theme 'doom-gruvbox)
;; (setq doom-theme 'doom-one)
(setq doom-theme 'doom-vibrant)
(setq display-line-numbers-type 'relative)
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
;(define-globalized-minor-mode global-rainbow-mode rainbow-mode (lambda () (rainbow-mode 1)))
;(global-rainbow-mode 1)


;; Enable searches across all open windows, not just current one
(setq avy-all-windows t)


;; Improved buffer, undo and redo
(winner-mode 1)
(global-set-key (kbd "<C-S-left>") 'winner-undo)
(global-set-key (kbd "<C-S-right>") 'winner-redo)


;; TODO Add automatic find link name
;; Pasting links gets truncated
(defun compress-org-link (arg)
  (interactive "P")
  (let ((url (thing-at-point 'url))
    (bounds (bounds-of-thing-at-point 'url)))
    (kill-region (car bounds) (cdr bounds))
    (insert (format "[[%s][%s]]" url (truncate-string-to-width url (if arg (prefix-numeric-value arg) 25) nil nil "...")))))


;; Store large context of history
(setq-default history-length 1000)
(setq-default prescient-history-length 1000)




;; COMPANY config
;;      Autocomplete for program files and org files
;;
;; TODO - make tabnine not run constantly
;;
;; Tabnine python autocomplete
(use-package! company-tabnine :ensure t)
(use-package company-tabnine
  :ensure t
  :when (featurep! :completion company)
  :after prog-mode
  ;;:config
  :init
  (add-hook 'prog-mode-hook #'(set-company-backend! 'prog-mode '(company-tabnine company-yasnippet company-dabbrev))))
(add-to-list 'company-backends 'company-tabnine)

;; Number the candidates (use M-1, M-2 etc to select completions).
(setq company-show-numbers t)
(with-eval-after-load 'company
   (define-key company-active-map (kbd "C-SPC") #'company-tabnine))

;; '(:settings
;;     ('python-auto-format-code t))

;;   '((:mode-local python-mode)

;;     (:settings
;;      ((:macro set-indent) 4)
;;      ('python-indent 4)
;;      ('python-indent-offset 4)
;;      ((:require yasnippet)
;;       ('yas-indent-line 'auto))
;;      ((:require elpy)
;;       ('elpy-rpc-timeout 2.5)))

;;     (:minor-modes
;;      ;; Note: elpy is automatically enabled.
;;      ((:require flycheck)
;;       (flycheck-mode -1)))

;;     (:on-before-save
;;      ((:require elpy)
;;       (python-format-code)))

;;     ((:mode-local elpy-mode)

;;      (:settings
;;       ((:require company)
;;        ('company-idle-delay 0)
;;        ((:require company-tabnine)
;;         ('company-backends :ensure-front 'company-tabnine))))))



;; (use-package! company-lsp
;;   :ensure t
;;    :commands (company-lsp))

;; Show icons when given company completions
(use-package! company-box
  :hook (company-mode . company-box-mode))




;; https://www.reddit.com/r/orgmode/comments/8n45ds/why_highlighting_text_is_so_painful_in_orgmode/
(setq org-hide-emphasis-markers t)
(setq org-emphasis-alist
      (quote (("*" bold)
              ("/" italic)
              ("_" underline)
              ("=" (:foreground "yellow" :background "black"))
              ("?" (:foreground "white" :background "green"))
              ("~" org-verbatim verbatim)
              ("+"
               (:strike-through t)))))

 ;; '(org-emphasis-alist
 ;;   (quote
 ;;    (
 ;;     ("!" org-habit-overdue-face)
 ;;     ("%" org-habit-alert-face)
 ;;     ("*" bold)
 ;;     ("/" italic)
 ;;     ("_" underline)
 ;;     ("=" org-verbatim verbatim)
 ;;     ("~" org-code verbatim)
 ;;     ("+" (:strike-through t))
 ;;     )))

(defun org-add-my-extra-markup ()
  "Add highlight emphasis."
  (add-to-list 'org-font-lock-extra-keywords
               '("[^\\w]\\(:\\[^\n\r\t]+:\\)[^\\w]"
                 (1 '(face highlight invisible nil)))))

(add-hook 'org-font-lock-set-keywords-hook #'org-add-my-extra-markup)







;; header bullets
(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))


;; org-journal
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



;; TODO
;; org-download
(require 'org-download)
;; Drag-and-drop to `dired`
(add-hook 'dired-mode-hook 'org-download-enable)


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


(set-popup-rule! "^\\*Org Agenda" :side 'bottom :size 0.90 :select t :ttl nil)
(set-popup-rule! "^CAPTURE.*\\.org$" :side 'bottom :size 0.90 :select t :ttl nil)
(set-popup-rule! "^\\*org-brain" :side 'right :size 1.00 :select t :ttl nil)


;; org-to-html
;; modify path and mathml
;; Makes MathJax and math export look correct again when exporting from org mode
;; https://emacs.stackexchange.com/questions/31271/siunitx-mathjax-org-mode-and-html-export
(setq org-html-mathjax-options
      '((path "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML")
        (scale "100")
        (align "center")
        (indent "2em")
        (mathml t)))

(setq org-html-mathjax-template
            "<script type=\"text/javascript\" src=\"%PATH\"></script>
<script type=\"text/javascript\">
<!--/*--><![CDATA[/*><!--*/
    MathJax.Hub.Config({
        jax: [\"input/TeX\", \"output/HTML-CSS\"],
        extensions: [\"tex2jax.js\",\"TeX/AMSmath.js\",\"TeX/AMSsymbols.js\",
                     \"TeX/noUndefined.js\", \"[Contrib]/siunitx/siunitx.js\", \"[Contrib]/mhchem/mhchem.js\"],
        tex2jax: {
            inlineMath: [ [\"\\\\(\",\"\\\\)\"] ],
            displayMath: [ ['$$','$$'], [\"\\\\[\",\"\\\\]\"], [\"\\\\begin{displaymath}\",\"\\\\end{displaymath}\"] ],
            skipTags: [\"script\",\"noscript\",\"style\",\"textarea\",\"pre\",\"code\"],
            ignoreClass: \"tex2jax_ignore\",
            processEscapes: false,
            processEnvironments: true,
            preview: \"TeX\"
        },
        TeX: {extensions: [\"AMSmath.js\",\"AMSsymbols.js\",  \"[Contrib]/siunitx/siunitx.js\", \"[Contrib]/mhchem/mhchem.js\"]},
        showProcessingMessages: true,
        displayAlign: \"%ALIGN\",
        displayIndent: \"%INDENT\",
        \"HTML-CSS\": {
             scale: %SCALE,
             availableFonts: [\"STIX\",\"TeX\"],
             preferredFont: \"TeX\",
             webFont: \"TeX\",
             imageFont: \"TeX\",
             showMathMenu: true,
        },
        MMLorHTML: {
             prefer: {
                 MSIE:    \"MML\",
                 Firefox: \"MML\",
                 Opera:   \"HTML\",
                 other:   \"HTML\"
             }
        }
    });
/*]]>*///-->
</script>
")

;; ;; (with-eval-after-load 'company
;; ;;   (define-key company-active-map (kbd "<return>") nil)
;; ;;   (define-key company-active-map (kbd "RET") nil)
;; ;;   (define-key company-active-map (kbd "C-SPC") #'company-complete-selection))


;; ;; flyspell
;; ;; Spell-check
;; ;; (require 'flyspell)
;; ;; (setq flyspell-issue-message-flag nil
;; ;;       ispell-local-dictionary "en_GB"
;; ;;       ispell-program-name "aspell"
;; ;;       ispell-extra-args '("--sug-mode=ultra"))

;; ;; (add-hook 'text-mode-hook 'flyspell-mode)
;; ;; (add-hook 'prog-mode-hook 'flyspell-prog-mode)









;; Custom Key mappings
;;
(map! :ne "SPC k" #'save-buffer)
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


;; custom toggle command for displaying org-roam buffer
(map! :leader
      (:when (featurep! :lang org +roam)
      ;; <leader> t --- toggle
      (:prefix-map ("t" . "toggle")
       :desc "Org-roam buffer" "q" #'org-roam-buffer-toggle-display)))


;; ;; ;; Compiler setup
;; ;; (use-package compile
;; ;;   :init
;; ;;   (setq compilation-ask-about-save nil
;; ;;         compilation-scroll-output 'next-error
;; ;;         ;; Don't stop on info or warnings.
;; ;;         compilation-skip-threshold 2))

;; ;; ;; Taken from https://emacs.stackexchange.com/questions/31493/print-elapsed-time-in-compilation-buffer/56130#56130
;; ;; (make-variable-buffer-local 'my-compilation-start-time)
;; ;; (add-hook 'compilation-start-hook #'my-compilation-start-hook)
;; ;; (defun my-compilation-start-hook (proc)
;; ;;   (setq my-compilation-start-time (current-time)))
;; ;; (add-hook 'compilation-finish-functions #'my-compilation-finish-function)
;; ;; (defun my-compilation-finish-function (buf why)
;; ;;   (let* ((elapsed  (time-subtract nil my-compilation-start-time))
;; ;;          (msg (format "Compilation took: %s" (format-time-string "%T.%N" elapsed t))))
;; ;;     (save-excursion (goto-char (point-max)) (insert msg))
;; ;;     (message "Compilation %s: %s" (string-trim-right why) msg)))


;; ;; ;; ;; Auto complete
;; ;; ;; (use-package company
;; ;; ;;   :init
;; ;; ;;   (setq company-idle-delay 0.0
;; ;; ;;         company-minimum-prefix-length 1))
;; ;; ;; (global-company-mode 1)
;; ;; ;; (global-set-key (kbd "<C-return>") 'company-complete)
;; ;; ;; (use-package company-emoji)
;; ;; ;; (add-to-list 'company-backends 'company-emoji)



;; ;; ;; Java
;; ;; (defun tkj-insert-serial-version-uuid()
;; ;;   (interactive)
;; ;;   (insert "private static final long serialVersionUID = 1L;"))
;; ;; (defun tkj-default-code-style-hook()
;; ;;   (setq c-basic-offset 2
;; ;;         c-label-offset 0
;; ;;         tab-width 2
;; ;;         indent-tabs-mode nil
;; ;;         compile-command "mvn -q -o -f ~/src/content-engine/engine/engine-core/pom.xml test -DtrimStackTrace=false"
;; ;;         require-final-newline nil))
;; ;; (add-hook 'java-mode-hook 'tkj-default-code-style-hook)
;; ;; (use-package flycheck
;; ;;   :init
;; ;;   (add-to-list 'display-buffer-alist
;; ;;                `(,(rx bos "*Flycheck errors*" eos)
;; ;;                  (display-buffer-reuse-window
;; ;;                   display-buffer-in-side-window)
;; ;;                  (side            . bottom)
;; ;;                  (reusable-frames . visible)
;; ;;                  (window-height   . 0.15))))
;; ;; (use-package idle-highlight)
;; ;; (defun my-java-mode-hook ()
;; ;;   (auto-fill-mode)
;; ;;   (flycheck-mode)
;; ;;   (git-gutter+-mode)
;; ;;   (gtags-mode)
;; ;;   (idle-highlight)
;; ;;   (subword-mode)
;; ;;   (yas-minor-mode)
;; ;;   (set-fringe-style '(8 . 0))
;; ;;   (define-key c-mode-base-map (kbd "C-M-j") 'tkj-insert-serial-version-uuid)
;; ;;   (define-key c-mode-base-map (kbd "C-m") 'c-context-line-break)
;; ;;   (define-key c-mode-base-map (kbd "S-<f7>") 'gtags-find-tag-from-here)
;; ;;   ;; Fix indentation for anonymous classes
;; ;;   (c-set-offset 'substatement-open 0)
;; ;;   (if (assoc 'inexpr-class c-offsets-alist)
;; ;;       (c-set-offset 'inexpr-class 0))
;; ;;   ;; Indent arguments on the next line as indented body.
;; ;;   (c-set-offset 'arglist-intro '++))
;; ;; (add-hook 'java-mode-hook 'my-java-mode-hook)
;; ;; (use-package projectile :ensure t)
;; ;; (use-package yasnippet :ensure t)
;; ;; (use-package lsp-mode :ensure t
;; ;;   :bind (("\C-\M-b" . lsp-find-implementation)
;; ;;          ("M-RET" . lsp-execute-code-action))
;; ;;   :config
;; ;;   (setq lsp-inhibit-message t
;; ;;         lsp-eldoc-render-all nil
;; ;;         lsp-enable-file-watchers nil
;; ;;         lsp-highlight-symbol-at-point nil)
;; ;;   ;; Performance tweaks, see
;; ;;   ;; https://github.com/emacs-lsp/lsp-mode#performance
;; ;;   (setq gc-cons-threshold 100000000)
;; ;;   (setq read-process-output-max (* 1024 1024)) ;; 1mb
;; ;;   (setq lsp-idle-delay 0.500))
;; ;; (use-package hydra :ensure t)
;; ;; (use-package company-lsp :ensure t)
;; ;; (use-package lsp-ui
;; ;;   :ensure t
;; ;;   :config
;; ;;   (setq lsp-prefer-flymake nil
;; ;;         lsp-ui-doc-delay 5.0
;; ;;         lsp-ui-sideline-enable nil
;; ;;         lsp-ui-sideline-show-symbol nil))
;; ;; (use-package lsp-java
;; ;;   :ensure t
;; ;;   :init
;; ;;   (setq lsp-java-vmargs
;; ;;         (list
;; ;;          "-noverify"
;; ;;          "-Xmx2G"
;; ;;          "-XX:+UseG1GC"
;; ;;          "-XX:+UseStringDeduplication"
;; ;;          "-javaagent:/Users/admin/.emacs.d/modules/lang/java/lombok.jar"
;; ;;          )
;; ;;         ;; Don't organise imports on save
;; ;;         lsp-java-save-action-organize-imports nil
;; ;;         ;; Currently (2019-04-24), dap-mode works best with Oracle
;; ;;         ;; JDK, see https://github.com/emacs-lsp/dap-mode/issues/31
;; ;;         ;;
;; ;;         ;; lsp-java-java-path "~/.emacs.d/oracle-jdk-12.0.1/bin/java"
;; ;;         ;; lsp-java-java-path "/usr/lib/jvm/java-11-openjdk-amd64/bin/java"
;; ;;         lsp-java-java-path "/usr/bin/java")
;; ;;   :config
;; ;;   (add-hook 'java-mode-hook #'lsp))
;; ;; (use-package dap-mode
;; ;;   :ensure t
;; ;;   :after lsp-mode
;; ;;   :config
;; ;;   (dap-mode t)
;; ;;   (dap-ui-mode t)
;; ;;   (dap-tooltip-mode 1)
;; ;;   (tooltip-mode 1)
;; ;;   (dap-register-debug-template
;; ;;    "localhost:5005"
;; ;;    (list :type "java"
;; ;;          :request "attach"
;; ;;          :hostName "localhost"
;; ;;          :port 5005))
;; ;;   (dap-register-debug-template
;; ;;    "10.186.38.171:5005"
;; ;;    (list :type "java"
;; ;;          :request "attach"
;; ;;          :hostName "10.186.38.171"
;; ;;          :port 5005)))
;; ;; ;; (use-package dap-java
;; ;; ;;   :ensure nil
;; ;; ;;   :after (lsp-java)
;; ;; ;;   ;; The :bind here makes use-package fail to lead the dap-java block!
;; ;; ;;   ;; :bind
;; ;; ;;   ;; (("C-c R" . dap-java-run-test-class)
;; ;; ;;   ;;  ("C-c d" . dap-java-debug-test-method)
;; ;; ;;   ;;  ("C-c r" . dap-java-run-test-method)
;; ;; ;;   ;;  )
;; ;; ;;   :config
;; ;; ;;   (global-set-key (kbd "<f7>") 'dap-step-in)
;; ;; ;;   (global-set-key (kbd "<f8>") 'dap-next)
;; ;; ;;   (global-set-key (kbd "<f9>") 'dap-continue)
;; ;; ;;   )
;; ;; ;; (use-package treemacs
;; ;; ;;   :init
;; ;; ;;   (add-hook 'treemacs-mode-hook
;; ;; ;;             (lambda () (treemacs-resize-icons 15))))




;; ;; ;; TODO Complete later
;; ;; ;; Test and move up later to 'company config section'

;; ;; ;; python
;; ;; ;; (use-package! conda
;; ;; ;;   :ensure t
;; ;; ;;   :init
;; ;; ;;   (setq conda-anaconda-home (expand-file-name "~/opt/anaconda3"))
;; ;; ;;     (setq conda-env-home-directory (expand-file-name "~/opt/anaconda3")))

;; ;; ;; (use-package! projectile
;; ;; ;;   :ensure t)
;; ;; ;; (use-package! lsp-mode
;; ;; ;;   :ensure t)
;; ;; ;; (use-package! lsp-ui
;; ;; ;;   :ensure t
;; ;; ;;   :config
;; ;; ;;   (setq lsp-ui-doc-max-height 20
;; ;; ;;         lsp-ui-doc-max-width 50
;; ;; ;;         lsp-ui-sideline-ignore-duplicate t

;; ;; ;; (use-package! company
;; ;; ;;   :ensure t
;; ;; ;;   :config
;; ;; ;;   (setq company-minimum-prefix-length 1
;; ;; ;;         company-idle-delay 0
;; ;; ;;         company-tooltip-limit 10
;; ;; ;;         company-transformers nil
;; ;; ;;         company-show-numbers t
;; ;; ;;         )
;; ;; ;;   (global-company-mode +1))





;; ;; ;; ;; install LSP company backend for LSP-driven completion
;; ;; ;; ;; (use-package company-lsp
;; ;; ;; ;;   :ensure t
;; ;; ;; ;;   :config
;; ;; ;; ;;   (push 'company-lsp company-backends))

;; ;; ;; (add-hook 'python-mode-hook 'anaconda-mode)
;; ;; ;; (eval-after-load "company"
;; ;; ;;    '(add-to-list 'company-backends '(company-anaconda :with company-capf)))


;; ;; ;; (setq
;; ;; ;;  python-shell-interpreter "ipython"
;; ;; ;;  python-shell-interpreter-args "-i")

;; ;; ;; (setq python-shell-interpreter "jupyter"
;; ;; ;;       python-shell-interpreter-args "console --simple-prompt"
;; ;; ;;       python-shell-prompt-detect-failure-warning nil)
;; ;; ;; (add-to-list 'python-shell-completion-native-disabled-interpreters
;; ;; ;;                           "jupyter")


;; ;; ;; (after! company
;; ;; ;;   (setq company-idle-delay 0.5
;; ;; ;;         company-minimum-prefix-length 2)
;; ;; ;;   (setq company-show-numbers t)
;; ;; ;;   (add-hook 'evil-normal-state-entry-hook #'company-abort)) ;; make aborting less annoying.

;; ;; ;; (set-company-backend! '(text-mode
;; ;; ;;                         markdown-mode
;; ;; ;;                         gfm-mode)
;; ;; ;;   '(:seperate company-ispell
;; ;; ;;     company-files
;; ;; ;;     company-yasnippet))



;; ;; ORG
;; ;; REFS && BIBTEX
(setq
   org_notes (concat (getenv "HOME") "/Dropbox/org/")
   zot_bib (concat (getenv "HOME") "/Dropbox/zotero/refs.bib")
   bib_notes_path (concat (getenv "HOME") "/Dropbox/org/Research-Notes/")
   bib_notes (concat (getenv "HOME") "/Dropbox/org/Research-Notes/bibnotes.org")
   org-directory org_notes
   deft-directory org_notes
   org-roam-directory org_notes)



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




(use-package org-ref
    :config
    (setq
         org-ref-completion-library 'org-ref-helm-cite
         org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex
         org-ref-default-bibliography (list zot_bib)
         org-ref-bibliography-notes bib_notes
         org-ref-note-title-format "* TODO %y - %t\n :PROPERTIES:\n  :Custom_ID: %k\n  :NOTER_DOCUMENT: %F\n :ROAM_KEY: cite:%k\n  :AUTHOR: %9a\n  :JOURNAL: %j\n  :YEAR: %y\n  :VOLUME: %v\n  :PAGES: %p\n  :DOI: %D\n  :URL: %U\n :END:\n\n"
         org-ref-notes-directory bib_notes_path
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
	bibtex-completion-notes-path bib_notes_path
	bibtex-completion-notes-template-one-file
	"\n* ${title} cite:${=key=}\n  :PROPERTIES:\n  :Custom_ID:  :${=key=}\n  :NOTER_DOCUMENT: ${file}\n  :END:\n\n"))


(use-package org-roam
  :hook (org-load . org-roam-mode)
  :commands (org-roam-buffer-toggle-display
             org-roam-find-file
             org-roam-graph
             org-roam-insert
             org-roam-switch-to-buffer
             org-roam-dailies-date
             org-roam-dailies-today
             org-roam-dailies-tomorrow
             org-roam-dailies-yesterday)
  :preface
  ;; Set this to nil so we can later detect whether the user has set a custom
  ;; directory for it, and default to `org-directory' if they haven't.
  (defvar org-roam-directory nil)
  :init
  :config
  (setq org-roam-directory (expand-file-name (or org-roam-directory "roam")
                                             org-directory)
        org-roam-verbose nil  ; https://youtu.be/fn4jIlFwuLU
        org-roam-buffer-window-parameters t
        ;; org-roam-buffer-no-delete-other-windows t ; make org-roam buffer sticky
        org-roam-completion-system 'default)  ;; TODO add company-completion???


  ;; Normally, the org-roam buffer doesn't open until you explicitly call
  ;; `org-roam'. If `+org-roam-open-buffer-on-find-file' is non-nil, the
  ;; org-roam buffer will be opened for you when you use `org-roam-find-file'
  ;; (but not `find-file', to limit the scope of this behavior).
  (add-hook 'find-file-hook
    (defun +org-roam-open-buffer-maybe-h ()
      (and +org-roam-open-buffer-on-find-file
           (memq 'org-roam-buffer--update-maybe post-command-hook)
           (not (window-parameter nil 'window-side)) ; don't proc for popups
           (not (eq 'visible (org-roam-buffer--visibility)))
           (with-current-buffer (window-buffer)
             (org-roam-buffer--get-create)))))

  ;; Hide the mode line in the org-roam buffer, since it serves no purpose. This
  ;; makes it easier to distinguish among other org buffers.
  (add-hook 'org-roam-buffer-prepare-hook #'hide-mode-line-mode))


(use-package org-roam-protocol :after org-protocol)

(let ((org-id-locations org-roam-directory)
      org-agenda-files)
  (org-id-update-id-locations))





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
   org-noter-notes-search-path (list org_notes)))
;; TODO Could be need for change




;; ;; open pdf with system pdf viewer (works on mac)
;; (setq bibtex-completion-pdf-open-function
;;   (lambda (fpath)
;;     (start-process "open" "*open*" "open" fpath)))


;; ;; open pdf with system pdf viewer (works on mac)
;; (setq bibtex-completion-pdf-open-function
;;   (lambda (fpath)
;;     (start-process "open" "*open*" "open" fpath)))

;; (setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f"))


;; TODO


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
   org-noter-notes-search-path  (list org_notes)))


;; ;; LATEX
;; ;;
;; ;;
(setq org-latex-compiler "xelatex")
(setq org-latex-pdf-process '("latexmk -xelatex -quiet -shell-escape -f %f"))
(setq org-preview-latex-image-directory "/tmp/ltximg/")
(setq-default TeX-engine 'xetex)
(setq-default TeX-PDF-mode t)

;; Set org to always preview latex formulas if in side buffer
(add-to-list 'org-roam-buffer-prepare-hook (lambda () (org--latex-preview-region (point-min) (point-max))) t)





;; View PDFs in emacs
;; https://github.com/fuxialexander/org-pdftools
(use-package org-pdftools
  :hook (org-load . org-pdftools-setup-link))


;; (use-package org-noter-pdftools
;; ;   :after org-noter
;;   :config
;;   (with-eval-after-load 'pdf-annot
;;     (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))


;; (use-package org-noter-pdftools
;; :after org-noter
;; :config
;; (with-eval-after-load 'pdf-annot
;; (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))



;; ;; Latex config
(add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer) ;; revert pdf after compile
(setq TeX-view-program-selection '((output-pdf "PDF Tools"))) ;; use pdf-tools for viewing
(setq LaTeX-command "latex --synctex=1") ;; optional: enable synctex

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





;; ;; Org roam and in file completion
;; ;; https://www.ianjones.us/own-your-second-brain
;; (use-package company-org-roam
;;       :when (featurep! :completion company)
;;       :after org-roam
;;       :config
;;       (set-company-backend! 'org-mode '(company-org-roam company-yasnippet company-dabbrev)))




(setq
 bibtex-completion-notes-path bib_notes_path ;(concat (getenv "HOME") "/Dropbox/org/Research-Notes/")
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

(setq doom-line-numbers-style 'relative)
