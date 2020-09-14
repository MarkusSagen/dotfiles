;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")

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
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "/Users/admin/Dropbox/org")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


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



;; Lanch in fullscreen
(add-to-list 'initial-frame-alist '(fullscreen . maximized))


;; Enable searches across all open windows, not just current one
(setq avy-all-windows t)



;; org base config
(setq reftex-default-bibliography '("~/Dropbox/bibliography/references.bib"))
(setq reftex-default-bibliography '("~/Dropbox/bibliography/references.bib"))
;;
(setq ref_file           "~/Dropbox/zotero/refs.bib"

      deft-dictionary    "~/Dropbox/org"
      org-roam-directory "~/Dropbox/org/"
      org-preview-latex-image-directory "/tmp/ltximg/")



;; org - Make orgfiles more beautiful and pure
;; italics and bold render correct
;; (setq org-hide-emphasis-markers t)

;; header bullets
(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; org-headings
;; TODO
;; https://zzamboni.org/post/beautifying-org-mode-in-emacs/
;;
;; (add-hook 'org-mode-hook 'variable-pitch-mode)
;; (add-hook 'org-mode-hook 'visual-line-mode)
;;



;; Using org-present for presenting .org slides
(eval-after-load "org-present"
  '(progn
     (add-hook 'org-present-mode-hook
               (lambda ()
                 (org-present-big)
                 (org-display-inline-images)
                 (org-present-hide-cursor)
                 (org-present-read-only)))
     (add-hook 'org-present-mode-quit-hook
               (lambda ()
                 (org-present-small)
                 (org-remove-inline-images)
                 (org-present-show-cursor)
                 (org-present-read-write)))))






;; `Bibtex-Config'
;; ;; Add Bibtex references search
(autoload 'helm-bibtex "helm-bibtex" "" t)
(setq
 ;; bibtex-completion-notes-path (concat (getenv "HOME") "/org/")
 ;; bibtex-completion-bibliography (concat (getenv "HOME") "/org/refs/refs.bib")
 ;; bibtex-completion-pdf-field "file"
 ;; bibtex-completion-library-path (concat (getenv "HOME") "/Zotero/storage/")
 bibtex-completion-notes-path   "~/Dropbox/org"
 bibtex-completion-bibliography "~/Dropbox/zotero/refs.bib"
 bibtex-completion-pdf-field "file"
 bibtex-completion-library-path "~/Dropbox/zotero/"
 bibtex-completion-pdf-open-function 'org-open-file
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
  ":END:\n\n"
  ))



;; bibtex-completion
(setq bibtex-completion-format-citation-functions
      '((org-mode      . bibtex-completion-format-citation-org-link-to-PDF)
        (latex-mode    . bibtex-completion-format-citation-cite)
        (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
        (default       . bibtex-completion-format-citation-default)))


;; org-ref
;; TODO LOADS really slow for some reason...!
(use-package! org-ref
  :config
  (setq
   org-ref-completion-library 'org-ref-ivy-cite
   org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex
   org-ref-note-title-format "* TODO %y - %t\n :PROPERTIES:\n  :Custom_ID: %k\n  :NOTER_DOCUMENT: %F\n :ROAM_KEY: cite:%k\n  :AUTHOR: %9a\n  :JOURNAL: %j\n  :YEAR: %y\n  :VOLUME: %v\n  :PAGES: %p\n  :DOI: %D\n  :URL: %U\n :END:\n\n"
   org-ref-default-bibliography "~/Dropbox/zotero/refs.bib"
   org-ref-bibliography-notes   "~/Dropbox/org/Research-Notes/notes.org"
   org-ref-notes-directory      "~/Dropbox/org"
   org-ref-notes-function 'orb-edit-notes))



;; ;; org-roam
(use-package! org-roam
  :ensure t
  :custom (org-roam-directory "~/Dropbox/org/")
  :bind (:map org-roam-mode-map
         (("C-c n l" . org-roam)
          ("C-c n f" . org-roam-find-file)
          ("C-c n g" . org-roam-graph-show))
         :map org-mode-map
         (("C-c n i" . org-roam-insert))
         (("C-c n I" . org-roam-insert-immediate)))
)


(after! org-roam
  (setq org-roam-graph-viewer "/usr/bin/open")
  (setq org-roam-ref-capture-templates
        '(("r" "ref" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "websites/${slug}"
           :head "#+TITLE: ${title}
                 #+ROAM_KEY: ${ref}
                 - source :: ${ref}"
           :unnarrowed t)))
  (setq org-roam-capture-templates
        '(("d" "default" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "${slug}"
           :head "#+TITLE: ${title}\n"
           :unnarrowed t)) ))



(after! org-roam
  (map! :leader
        :prefix "n"
        :desc "org-roam" "I" #'org-roam
        :desc "org-roam-insert" "i" #'org-roam-insert
        :desc "org-roam-switch-to-buffer" "b" #'org-roam-switch-to-buffer
        :desc "org-roam-find-file" "f" #'org-roam-find-file
        :desc "org-roam-show-graph" "g" #'org-roam-show-graph
        :desc "org-roam-ref" "B" #'org-roam-find-ref
        :desc "org-roam-capture" "c" #'org-roam-capture))

(use-package! company-org-roam
  :ensure t
  :config
  (push 'company-org-roam company-backends))



;; ;; ;; Capture from websites
;; ;; (after! org-roam
;; ;;   (setq org-roam-capture-ref-templates
;; ;;         '(("r" "ref" plain (function org-roam-capture--get-point)
;; ;;            "%?"
;; ;;            :file-name "websites/${slug}"
;; ;;                           :head "#+TITLE: ${title}
;; ;;     #+ROAM_KEY: ${ref}
;; ;;     - source :: ${ref}"
;; ;;                                          :unnarrowed t))))




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



(use-package! org-roam-bibtex
  :after org-roam
  :ensure t
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :bind (:map org-mode-map
         (("C-c n a" . orb-note-actions)))
  :config
  (setq orb-preformat-keywords
        '(("citekey" . "=key=") "title" "url" "file" "author-or-editor" "keywords"))
  (setq orb-templates
        '(("r" "ref" plain (function org-roam-capture--get-point)
           ""
           :file-name "${slug}"
           :head "#+TITLE: ${title}\n#+ROAM_KEY: ${ref}
- tags ::
- keywords :: ${keywords}
* ${title}
:PROPERTIES:
:Custom_ID: ${citekey}
:URL: ${url}
:AUTHOR: ${author-or-editor}
:NOTER_DOCUMENT: %(orb-process-file-field \"${citekey}\")
:NOTER_PAGE:
:END:"))))



;; bibtex-completion
(map! :ne "SPC n b" #'helm-bibtex)



;; (setq bibtex-completion-bibliography "~/Dropbox/zotero/refs.bib"
;;       bibtex-completion-library-path "~/Dropbox/zotero"
;;       bibtex-completion-notes-path "~/Dropbox/org")

;; ;; open pdf with system pdf viewer (works on mac)
;; (setq bibtex-completion-pdf-open-function
;;   (lambda (fpath)
;;     (start-process "open" "*open*" "open" fpath)))

;; (setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f"))





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
   org-noter-notes-search-path  (list "~/Dropbox/org/")))



;; TODO find how to install .el files
;; ;; Linkage and highlight in pdf files
;; ;; (use-package org-pdftools
;;   ;; :hook (org-load . org-pdftools-setup-link))
;;
;; (use-package org-noter-pdftools
;; :after org-noter
;; :config
;; (with-eval-after-load 'pdf-annot
;; (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))



(use-package! deft
  :after org
  :bind
  ("C-c n d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
        ;; (deft-directory (concat (getenv "HOME") "/org")))
        (deft-directory "~/Dropbox/org"))


;; TODO
;; ;; org-download
;;(require 'org-download)
;; ;; Drag-and-drop to `dired`
;; (add-hook 'dired-mode-hook 'org-download-enable)


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


(setq
 +magit-hub-features t
 css-indent-offset 2
 org-tags-column -80
 web-mode-markup-indent-offset 2
 web-mode-code-indent-offset 2
 web-mode-css-indent-offset 2
 mac-command-modifier 'meta)


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
</script>")



;; Improved buffer, undo and redo
(winner-mode 1)
(global-set-key (kbd "<C-left>") 'winner-undo)
(global-set-key (kbd "<C-right>") 'winner-redo)


;; Pure text editing
(add-hook 'text-mode-hook
          '(lambda ()
             (flyspell-mode)
             (git-gutter+-mode)
             (auto-fill-mode 1)))
(setq longlines-show-hard-newlines t)


;; Compiler setup
(use-package compile
  :init
  (setq compilation-ask-about-save nil
        compilation-scroll-output 'next-error
        ;; Don't stop on info or warnings.
        compilation-skip-threshold 2)
  )
;; Taken from https://emacs.stackexchange.com/questions/31493/print-elapsed-time-in-compilation-buffer/56130#56130
(make-variable-buffer-local 'my-compilation-start-time)
(add-hook 'compilation-start-hook #'my-compilation-start-hook)
(defun my-compilation-start-hook (proc)
  (setq my-compilation-start-time (current-time)))
(add-hook 'compilation-finish-functions #'my-compilation-finish-function)
(defun my-compilation-finish-function (buf why)
  (let* ((elapsed  (time-subtract nil my-compilation-start-time))
         (msg (format "Compilation took: %s" (format-time-string "%T.%N" elapsed t))))
    (save-excursion (goto-char (point-max)) (insert msg))
    (message "Compilation %s: %s" (string-trim-right why) msg)))


;; Auto complete
(use-package company
  :init
  (setq company-idle-delay 0.0
        company-minimum-prefix-length 1))
(global-company-mode 1)
(global-set-key (kbd "<C-return>") 'company-complete)
(use-package company-emoji)
(add-to-list 'company-backends 'company-emoji)


;; Markdown
(use-package emojify)
(add-hook 'markdown-mode-hook 'flyspell-mode)
(add-hook 'markdown-mode-hook 'emojify-mode)


;; Java
(defun tkj-insert-serial-version-uuid()
  (interactive)
  (insert "private static final long serialVersionUID = 1L;"))
(defun tkj-default-code-style-hook()
  (setq c-basic-offset 2
        c-label-offset 0
        tab-width 2
        indent-tabs-mode nil
        compile-command "mvn -q -o -f ~/src/content-engine/engine/engine-core/pom.xml test -DtrimStackTrace=false"
        require-final-newline nil))
(add-hook 'java-mode-hook 'tkj-default-code-style-hook)
(use-package flycheck
  :init
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*Flycheck errors*" eos)
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (side            . bottom)
                 (reusable-frames . visible)
                 (window-height   . 0.15))))
(use-package idle-highlight)
(defun my-java-mode-hook ()
  (auto-fill-mode)
  (flycheck-mode)
  (git-gutter+-mode)
  (gtags-mode)
  (idle-highlight)
  (subword-mode)
  (yas-minor-mode)
  (set-fringe-style '(8 . 0))
  (define-key c-mode-base-map (kbd "C-M-j") 'tkj-insert-serial-version-uuid)
  (define-key c-mode-base-map (kbd "C-m") 'c-context-line-break)
  (define-key c-mode-base-map (kbd "S-<f7>") 'gtags-find-tag-from-here)
  ;; Fix indentation for anonymous classes
  (c-set-offset 'substatement-open 0)
  (if (assoc 'inexpr-class c-offsets-alist)
      (c-set-offset 'inexpr-class 0))
  ;; Indent arguments on the next line as indented body.
  (c-set-offset 'arglist-intro '++))
(add-hook 'java-mode-hook 'my-java-mode-hook)
(use-package projectile :ensure t)
(use-package yasnippet :ensure t)
(use-package lsp-mode :ensure t
  :bind (("\C-\M-b" . lsp-find-implementation)
         ("M-RET" . lsp-execute-code-action))
  :config
  (setq lsp-inhibit-message t
        lsp-eldoc-render-all nil
        lsp-enable-file-watchers nil
        lsp-highlight-symbol-at-point nil)
  ;; Performance tweaks, see
  ;; https://github.com/emacs-lsp/lsp-mode#performance
  (setq gc-cons-threshold 100000000)
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (setq lsp-idle-delay 0.500))
(use-package hydra :ensure t)
(use-package company-lsp :ensure t)
(use-package lsp-ui
  :ensure t
  :config
  (setq lsp-prefer-flymake nil
        lsp-ui-doc-delay 5.0
        lsp-ui-sideline-enable nil
        lsp-ui-sideline-show-symbol nil))
(use-package lsp-java
  :ensure t
  :init
  (setq lsp-java-vmargs
        (list
         "-noverify"
         "-Xmx2G"
         "-XX:+UseG1GC"
         "-XX:+UseStringDeduplication"
         "-javaagent:/Users/admin/.emacs.d/modules/lang/java/lombok.jar"
         )
        ;; Don't organise imports on save
        lsp-java-save-action-organize-imports nil
        ;; Currently (2019-04-24), dap-mode works best with Oracle
        ;; JDK, see https://github.com/emacs-lsp/dap-mode/issues/31
        ;;
        ;; lsp-java-java-path "~/.emacs.d/oracle-jdk-12.0.1/bin/java"
        ;; lsp-java-java-path "/usr/lib/jvm/java-11-openjdk-amd64/bin/java"
        lsp-java-java-path "/usr/bin/java")
  :config
  (add-hook 'java-mode-hook #'lsp))
(use-package dap-mode
  :ensure t
  :after lsp-mode
  :config
  (dap-mode t)
  (dap-ui-mode t)
  (dap-tooltip-mode 1)
  (tooltip-mode 1)
  (dap-register-debug-template
   "localhost:5005"
   (list :type "java"
         :request "attach"
         :hostName "localhost"
         :port 5005))
  (dap-register-debug-template
   "10.186.38.171:5005"
   (list :type "java"
         :request "attach"
         :hostName "10.186.38.171"
         :port 5005)))
;; (use-package dap-java
;;   :ensure nil
;;   :after (lsp-java)
;;   ;; The :bind here makes use-package fail to lead the dap-java block!
;;   ;; :bind
;;   ;; (("C-c R" . dap-java-run-test-class)
;;   ;;  ("C-c d" . dap-java-debug-test-method)
;;   ;;  ("C-c r" . dap-java-run-test-method)
;;   ;;  )
;;   :config
;;   (global-set-key (kbd "<f7>") 'dap-step-in)
;;   (global-set-key (kbd "<f8>") 'dap-next)
;;   (global-set-key (kbd "<f9>") 'dap-continue)
;;   )
;; (use-package treemacs
;;   :init
;;   (add-hook 'treemacs-mode-hook
;;             (lambda () (treemacs-resize-icons 15))))



;; shell
(defun spawn-shell (name)
  "Create a new shell buffer
taken from http://stackoverflow.com/a/4116113/446256"
  (interactive "Name of shell buffer to create: ")
  (pop-to-buffer (get-buffer-create (generate-new-buffer-name name)))
  (shell (current-buffer)))
(defun my-shell-mode-hook ()
  (process-send-string (get-buffer-process (current-buffer))
                       "export PAGER=cat\n")
  (process-send-string (get-buffer-process (current-buffer))
                       "uprompt\n\n\n"))(
  add-hook 'shell-mode-hook 'my-shell-mode-hook)
(setq-default explicit-shell-file-name "/bin/bash")


;; bash settings
(setq sh-basic-offset 2
      sh-indentation 2)
(add-hook 'sh-mode-hook 'yas-minor-mode)
(add-hook 'sh-mode-hook 'flycheck-mode)
(add-hook 'sh-mode-hook 'git-gutter+-mode)
;; Allow functions on the form <word>.<rest>(). Without my change,
;; allowing punctuation characters in the function name,, only
;; <rest>() is allowed.
(setq sh-imenu-generic-expression
      (quote
       ((sh
         (nil "^\\s-*function\\s-+\\([[:alpha:]_][[:alnum:]\\s._]*\\)\\s-*\\(?:()\\)?" 1)
         (nil "^\\s-*\\([[:alpha:]_][[:alnum:]\\s._]*\\)\\s-*()" 1)))))


;; Shell escapes
(defun display-ansi-colors ()
  (interactive)
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))




;; python
(use-package! conda
  :ensure t
  :init
  (setq conda-anaconda-home (expand-file-name "~/opt/anaconda3"))
    (setq conda-env-home-directory (expand-file-name "~/opt/anaconda3")))

(use-package! projectile
  :ensure t)
(use-package! lsp-mode
  :ensure t)
(use-package! lsp-ui
  :ensure t
  :config
  (setq lsp-ui-doc-max-height 20
        lsp-ui-doc-max-width 50
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-peek-always-show t))

(use-package! company
  :ensure t
  :config
  (setq company-minimum-prefix-length 1
        company-idle-delay 0
        company-tooltip-limit 10
        company-transformers nil
        company-show-numbers t
        )
  (global-company-mode +1))


(use-package! company-lsp
  :ensure t
  :commands (company-lsp))


(use-package! company-box
  :ensure t
  :hook (company-mode . company-box-mode))



;; install LSP company backend for LSP-driven completion
;; (use-package company-lsp
;;   :ensure t
;;   :config
;;   (push 'company-lsp company-backends))

(add-hook 'python-mode-hook 'anaconda-mode)
(eval-after-load "company"
   '(add-to-list 'company-backends '(company-anaconda :with company-capf)))


(setq
 python-shell-interpreter "ipython"
 python-shell-interpreter-args "-i")

(setq python-shell-interpreter "jupyter"
      python-shell-interpreter-args "console --simple-prompt"
      python-shell-prompt-detect-failure-warning nil)
(add-to-list 'python-shell-completion-native-disabled-interpreters
                          "jupyter")


(after! company
  (setq company-idle-delay 0.5
        company-minimum-prefix-length 2)
  (setq company-show-numbers t)
  (add-hook 'evil-normal-state-entry-hook #'company-abort)) ;; make aborting less annoying.

(set-company-backend! '(text-mode
                        markdown-mode
                        gfm-mode)
  '(:seperate company-ispell
    company-files
    company-yasnippet))

(setq-default history-length 1000)
(setq-default prescient-history-length 1000)




;; Custom Key mappings
;;
;;
(map! :ne "SPC k" #'save-buffer)
(map! :ne "M-/" #'comment-or-uncomment-region)
(map! :ne "SPC / r" #'deadgrep)
(map! :ne "SPC m m" #'org-latex-preview)

;; Make the '*' character easier to reach
(global-set-key (kbd "M-\\") (kbd "*"))
(global-set-key (kbd "C-|") (kbd "*"))

;; Move between buffers
(map! :n "M-<" #'evil-prev-buffer)
(map! :n "M->" #'evil-next-buffer)
(map! :n "M-(" #'evil-next-close-paren)
(map! :n "M-)" #'evil-previous-open-paren)

;; Move Org-items with Vim keybindings
(after! org (map! :map org-mode-map
                  :n "M-j" #'org-metadown
                  :n "M-k" #'org-metaup))
