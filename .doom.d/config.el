;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Markus Sagen"
      user-mail-address "Markus.Sagen@gmail.com")

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
(setq doom-font (font-spec :family "monospace" :size 14))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one) ;; Toggle: 'Spc h t'
;; (setq doom-theme 'doom-dark+)        ;; Easy to read
;; (setq doom-theme 'doom-dracula)   ;; Very easy at night


 ;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Set FULL SCREEN of Doom Emacs on startup
(add-to-list 'initial-frame-alist '(fullscreen . maximized))


;; Disable read-only FOREVER!
(setq buffer-read-only nil)


;; `ORG-Config'
;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
;; (setq org-directory "~/org/")


;; `ORG-Config'
;; FROM https://rgoswami.me/posts/org-note-workflow/ and https://www.ianjones.us/own-your-second-brain
;; For getting into org-mode roam and bibtex working
;; Set up Org-Roam and Bibtex Reference
(setq
   org_notes (concat (getenv "HOME") "/org")
   zot_bib (concat (getenv "HOME") "/org/refs/refs.bib")
   org-directory org_notes
   deft-directory org_notes
   org-roam-directory org_notes
   )


;; `Bibtex-Config'
;; Add Bibtex references search
(autoload 'helm-bibtex "helm-bibtex" "" t)
(setq
   bibtex-completion-notes-path (concat (getenv "HOME") "/org/")
   bibtex-completion-bibliography (concat (getenv "HOME") "/org/refs/refs.bib")
   bibtex-completion-pdf-field "file"
   bibtex-completion-library-path (concat (getenv "HOME") "/Zotero/storage/")
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


; TODO change to chrome -
; from gtihub org-roam-bibtex
;(setq bibtex-completion-browser-functionwser-function
;  (lambda (url _) (start-process "chromium" "*chromium*" "chromium" url)))

(setq bibtex-completion-format-citation-functions
  '((org-mode      . bibtex-completion-format-citation-org-link-to-PDF)
    (latex-mode    . bibtex-completion-format-citation-cite)
    (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
    (default       . bibtex-completion-format-citation-default)))



;; `ORG-ref-Config'
;; For setting up org citation, linking and default note properties
(use-package org-ref
    :config
    (setq
        org-ref-completion-library 'org-ref-ivy-cite
        org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex
        org-ref-default-bibliography (list (concat (getenv "HOME") "/org/refs/refs.bib"))
        org-ref-bibliography-notes (concat (getenv "HOME") "org/Research-Notes/notes.org")
        org-ref-note-title-format "* TODO %y - %t\n :PROPERTIES:\n  :Custom_ID: %k\n  :NOTER_DOCUMENT: %F\n :ROAM_KEY: cite:%k\n  :AUTHOR: %9a\n  :JOURNAL: %j\n  :YEAR: %y\n  :VOLUME: %v\n  :PAGES: %p\n  :DOI: %D\n  :URL: %U\n :END:\n\n"
        org-ref-notes-directory (concat (getenv "HOME") "/org")
        org-ref-notes-function 'orb-edit-notes
    ))



;; `Org-roam-Config'
;; settings for the slipbox or Zettelkasten method
;; For org-roam
(setq org-roam-directory (concat (getenv "HOME") "/org"))
;;(setq org-roam-completion-system 'default)
(setq org-roam-completion-system 'helm)
;;(setq org-roam-directory "~/org/roam")



;; `Org-journal-Config'
;; Fleeting quick note / Daily notes as in "Roam Research"
;; Provided by org-journal
(use-package org-journal
      :bind
      ("C-c n j" . org-journal-new-entry)
      :custom
      (org-journal-dir (concat (getenv "HOME") "/org/journal"))
      (org-journal-date-prefix "#+TITLE: ")
      (org-journal-file-format "%Y-%m-%d.org")
      (org-journal-date-format "%A, %d %B %Y"))
    (setq org-journal-enable-agenda-integration t)


;; `Org-roam-bibtex-Config'
;; For org-roam-bibtex
(use-package org-roam-bibtex
  ;:after (org-roam)
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









 ;; Mapping helm-bibtex completion
 ;; Mapping bibtex references and how to access
 (map! :ne "SPC n b" #'helm-bibtex)










;; `Org-noter-Config'
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
   org-noter-notes-search-path (list "~/org/")))


;; TODO find how to install .el files
;; Linkage and highlight in pdf files
;; (use-package org-pdftools
  ;; :hook (org-load . org-pdftools-setup-link))

;; (use-package org-noter-pdftools
  ;; :after org-noter
  ;; :config
  ;; (with-eval-after-load 'pdf-annot
    ;; (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))













;; TODO Try
;; Try autocomplete of roam links
;; (use-package company-org-roam
  ;; :ensure t
  ;; :after org-roam
  ;; :config (push 'company-org-roam company-backends))





;; `Org-protocol-Config'
;; Capture info from website and notes
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
                 :kill-buffer t))
)


;; Capture from chrome bookmark
;; TODO Does not work yet...
;; TODO Maybe replace with 'org-downloader'
(after! org-roam
      (setq org-roam-ref-capture-templates
            '(("r" "ref" plain (function org-roam-capture--get-point)
               "%?"
               :file-name "websites/${slug}"
               :head "#+TITLE: ${title}
    #+ROAM_KEY: ${ref}
    - source :: ${ref}"
               :unnarrowed t))))


;; `Deft-Config'
;; Search and lists org mode files and notes
;; Like helm but easier and prettier!
(use-package deft
      :after org
      :bind
      ("C-c n d" . deft)
      :custom
      (deft-recursive t)
      (deft-use-filter-string-for-filename t)
      (deft-default-extension "org")
      (deft-directory (concat (getenv "HOME") "/org")))



;; `Org-Downloader'
;; (use-package! org-download
  ;; :commands
  ;; org-download-dnd
  ;; org-download-yank
  ;; org-download-screenshot
  ;; org-download-dnd-base64
  ;; :init
  ;; (map! :map org-mode-map
        ;; "s-Y" #'org-download-screenshot
        ;; "s-y" #'org-download-yank)
  ;; (pushnew! dnd-protocol-alist
            ;; '("^\\(?:https?\\|ftp\\|file\\|nfs\\):" . +org-dragndrop-download-dnd-fn)
            ;; '("^data:" . org-download-dnd-base64))
  ;; (advice-add #'org-download-enable :override #'ignore)
  ;; :config
  ;; (defun +org/org-download-method (link)
    ;; (let* ((filename
            ;; (file-name-nondirectory
             ;; (car (url-path-and-query
                   ;; (url-generic-parse-url link)))))
           ;; Create folder name with current buffer name, and place in root dir
           ;; (dirname (concat "./img/"
                            ;; (replace-regexp-in-string " " "_"
                                                      ;; (downcase (file-name-base buffer-file-name)))))
           ;; (filename-with-timestamp (format "%s%s.%s"
                                            ;; (file-name-sans-extension filename)
                                            ;; (format-time-string org-download-timestamp)
                                            ;; (file-name-extension filename))))
      ;; (make-directory dirname t)
      ;; (expand-file-name filename-with-timestamp dirname)))
  ;; :config
  ;; (setq org-download-screenshot-method
        ;; (cond (IS-MAC "screencapture -i %s")
              ;; (IS-LINUX
               ;; (cond ((executable-find "maim")  "maim -s %s")
                     ;; ((executable-find "scrot") "scrot -s %s")))))
  ;; (if (memq window-system '(mac ns))
      ;; (setq org-download-screenshot-method "screencapture -i %s")
    ;; (setq org-download-screenshot-method "maim -s %s"))
  ;; (setq org-download-method '+org/org-download-method))


(require 'org-download)

;; Drag-and-drop to `dired`
(add-hook 'dired-mode-hook 'org-download-enable)










;; Enable searches across all open windows, not just current one
(setq avy-all-windows t)

(map! :ne "SPC k" #'save-buffer)
(map! :ne "M-/" #'comment-or-uncomment-region)
(map! :ne "SPC / r" #'deadgrep)
(map! :ne "SPC m m" #'org-latex-preview)




(use-package! mode-icons-mode
  :defer t
  :commands (mode-icons-mode))



;; configure and list agenda after priority and day
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


;; Add Command for moving Org-Headings with Vim commands
(after! org (map! :map org-mode-map
        :n "M-j" #'org-metadown
        :n "M-k" #'org-metaup))


;; Toggle between most two most recent Buffers
(map! :n "\\" "SPC , RET")


;; Map Toggling between
(map! :n "M-<" #'evil-prev-buffer)
(map! :n "M->" #'evil-next-buffer)
(map! :n "M-(" #'evil-next-close-paren)
(map! :n "M-)" #'evil-previous-open-paren)

(global-set-key (kbd "M-\\") (kbd "*"))
(global-set-key (kbd "C-|") (kbd "*"))








;; Org-drill
;; Spaced repetition
;(require 'package)
;(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
;(package-initialize)

;(use-package org-drill
 ; :defer t
 ; :ensure org-plus-contrib
 ; :commands (org-drill)
 ; :config
 ; ;; Config options
;)







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




;; Setting up pdf viewer
;(use-package pdf-view
;  :hook (pdf-tools-enabled . pdf-view-midnight-minor-mode)
;  :hook (pdf-tools-enabled . hide-mode-line-mode)
;  :config
;  (setq pdf-view-midnight-colors '("#ABB2BF" . "#282C35")))

(add-to-list 'mailcap-user-mime-data
               '((type . "application/pdf")
                 (viewer . pdf-view-mode)))



;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.
