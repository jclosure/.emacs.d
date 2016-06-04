
(load-library "support-functions")

;; inspired by see ref:
;; source: http://mescal.imag.fr/membres/arnaud.legrand/misc/init.php
;; source file: http://mescal.imag.fr/membres/arnaud.legrand/misc/init.org

;; good examples:
;; https://pavpanchekha.com/blog/org-mode-publish.html

;; dealing with conflicts, e.g. windmove, yasnippets, etc..
;; ref: http://orgmode.org/manual/Conflicts.html

;; FIX SUBSCRIPT/SUPERSCRIPT
;; http://emacs.stackexchange.com/questions/10549/org-mode-how-to-export-underscore-as-underscore-instead-of-highlight-in-html
;; http://stackoverflow.com/questions/698562/disabling-underscore-to-subscript-in-emacs-org-mode-export


;; setup org mode - http://orgmode.org/worg/org-tutorials/orgtutorial_dto.html
(use-package org
             :defer t
             :init
             (progn
               ;; handling links
               ;; http://orgmode.org/manual/Handling-links.html#Handling-links
               (define-key global-map "\C-cl" 'org-store-link)
               (define-key global-map "\C-ca" 'org-agenda)
               )
             :config
             (progn
               (setq org-todo-keywords
                     '((sequence "TODO" "WAITING" "DONE")))
               (setq org-log-done t)
               (org-babel-do-load-languages
                'org-babel-load-languages
                '((elasticsearch . t)
                  (ruby . t)
                  (python . t)
                  (lisp . t)
                  (scheme . t)
                  (sh . t)
                  (R . t)
                  (ocaml . t)
                  (ditaa . t)
                  (dot . t)
                  (octave . t)
                  (sqlite . t)
                  (perl . t)
                  (org . t)
                  (latex . t)
                  (java . t)
		  (js . t)
		  (C . t)
                  (makefile . t)))
               ;; To create a src block, type <S and then TAB
               (add-to-list 'org-structure-template-alist
                            '("S" "#+begin_src ?\n\n#+end_src" "<src lang=\"?\">\n\n</src>"))
               ;; To create an example block, type <E and then TAB
               (add-to-list 'org-structure-template-alist
                            '("E" "#+begin_example ?\n\n#+end_example" "<src lang=\"text\">\n\n</src>"))
               ;; more convenient link navigation
               (define-key org-mode-map "\C-n" 'org-next-link)
               (define-key org-mode-map "\C-p" 'org-previous-link))
)


;; set default directory and agenda files for org
(setq org-directory "~/org/agenda/")
(setq org-agenda-files (list (concat org-directory "work.org")
                             (concat org-directory  "school.org")
                             (concat org-directory "home.org")))

(ensure-directory org-directory)
(mapcar 'ensure-text-file org-agenda-files)

;; more agenda stuff from arnaud
(setq org-agenda-include-all-todo t)
(setq org-agenda-include-diary t)


;; org cosmetics
;;(setq org-hide-leading-stars t)
(setq org-startup-indented t)      ;; start org mode with identing (outline format instead of book)
(setq org-alphabetical-lists t)
(setq org-src-fontify-natively t)  ;; you want this to activate coloring in blocks
(setq org-src-tab-acts-natively t) ;; you want this to have completion in blocks
;;(setq org-hide-emphasis-markers t) ;; to hide the *,=, or / markers
(setq org-pretty-entities t)       ;; to have \alpha, \to and others display as utf8 http://orgmode.org/manual/Special-symbols.html

;; TESTING OUT!!!
(add-hook 'org-mode-hook 'visual-line-mode)
;;(add-hook 'org-mode-hook 'variable-pitch-mode)



;; enable/disable subscripts and superscripts
;;(setq org-export-with-sub-superscripts t)
;; can be done on a single file with this header:
;; #+OPTIONS: ^:nil


;; if you don't want normal auto src indentation from emacs
;;(setq org-src-preserve-indentation t)

;; seamless use of babel
(setq org-export-babel-evaluate nil)
(setq org-confirm-babel-evaluate nil)

;; fix for lack of coding being detected in windows for org-mode babel results
(defun hide-ctrl-M ()
  "Hides the disturbing '^M' showing up in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(add-hook 'org-mode-hook 'hide-ctrl-M)

 ;; Make windmove work in org-mode:
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

;; list completion tracking - arnaud - look into
;; see http://thread.gmane.org/gmane.emacs.orgmode/42715
(eval-after-load 'org-list
  '(add-hook 'org-checkbox-statistics-hook (function ndk/checkbox-list-complete)))

(defun ndk/checkbox-list-complete ()
  (save-excursion
    (org-back-to-heading t)
    (let ((beg (point)) end)
      (end-of-line)
      (setq end (point))
      (goto-char beg)
      (if (re-search-forward "\\[\\([0-9]*%\\)\\]\\|\\[\\([0-9]*\\)/\\([0-9]*\\)\\]" end t)
            (if (match-end 1)
                (if (equal (match-string 1) "100%")
                    ;; all done - do the state change
                    (org-todo 'done)
                  (org-todo 'todo))
              (if (and (> (match-end 2) (match-beginning 2))
                       (equal (match-string 2) (match-string 3)))
                  (org-todo 'done)
                (org-todo 'todo)))))))


;; DATE HELPER FUNCTIONS

;; insert a date
;;(global-set-key (kbd "C-c d") 'insert-date)
(defun insert-date (prefix)
    "Insert the current date. With prefix-argument, use ISO format. With
   two prefix arguments, write out the day and month name."
    (interactive "P")
    (let ((format (cond
                   ((not prefix) "** %Y-%m-%d")
                   ((equal prefix '(4)) "[%Y-%m-%d]"))))
      (insert (format-time-string format))))

;; insert date with timer
;; (global-set-key (kbd "C-c t") 'insert-time-date)
(defun insert-time-date (prefix)
    "Insert the current date. With prefix-argument, use ISO format. With
   two prefix arguments, write out the day and month name."
    (interactive "P")
    (let ((format (cond
                   ((not prefix) "[%H:%M:%S; %d.%m.%Y]")
                   ((equal prefix '(4)) "[%H:%M:%S; %Y-%m-%d]"))))
      (insert (format-time-string format))))

;; see arnaud's init.el file
;;(global-set-key (kbd "C-c g") 'org-git-insert-link-interactively)



;;;;;;;;;;;;;;;;;;;; LATEX ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; previewing - scale up latex for bigness
(add-hook 'org-mode-hook (lambda ()
                           (setq org-format-latex-options
                                 (plist-put org-format-latex-options :scale 4.0))))


;; toggling preview of latex fragments: http://comments.gmane.org/gmane.emacs.orgmode/88539
;; the new behavior is just to toggle with: C-c C-x C-l
;; org-preview-latex-fragment
;; org-toggle-latex-fragment

;; exporting latex code blocks
;; (setq org-latex-create-formula-image-program 'imagemagick)
;; (setq org-latex-create-formula-image-program 'dvipng)


;;;;;;;;;;;;;;;;;;;;; HTML PUBLISHING SETUP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; RSS: http://emacs-doctor.com/blogging-from-emacs.html
;;;      http://bastibe.de/2013-11-13-blogging-with-emacs.html

;;; the publishing function!!!
(load "ox-publish")
(load "ox-rss")

;; to run execute:
;;(org-export-site)

;; bootstrap publishing setup


(defun org-export-site ()
  "1-click blog publishing"
  ;;(interactive)
  ;;(org-capture nil "b")
  (org-publish "org-site"))


;; custom html
(defun my-org-export-format-drawer (name content)
  (concat "<div class=\"drawer " (downcase name) "\">\n"
          "<h6>" (capitalize name) "</h6>\n"
          content
          "\n</div>"))
(setq org-html-format-drawer-function 'my-org-export-format-drawer)


(setf my-head-extra
      (concat
       "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n"
       "<link rel='stylesheet' href='css/main.css' />"
       "<link rel='alternate' type='application/rss+xml'
              href='http://mydomain.org/org-site.xml'
              title='RSS feed for org-site at mydomain.org'>"))

;; local setup

(setq site-base-directory "~/.emacs.d/org")
(setq site-publishing-directory "~/projects/www/org-site")
(setq site-rss-publishing-directory (concat site-publishing-directory "/rss"))


;; make sure directories are there
(ensure-directory site-base-directory)
(ensure-directory site-publishing-directory)
(ensure-directory site-rss-publishing-directory)

;; initialize
;;(setq org-publish-project-alist (list))
;; todo: use (add-to-list ...)

;; publishing configuration
(setq org-publish-project-alist
      `(
        ("org-site"
         :components ("org-site-docs" "org-site-static" "org-site-rss")
         )
        ("org-site-docs"
         :base-directory ,site-base-directory
         :publishing-directory ,site-publishing-directory
         :publishing-function org-html-publish-to-html
         :table-of-contents nil
         :html-extension "html"
         :body-only nil
         :recursive t
         ;;:with-drawers t

         ;; pretty drawers
         :with-author t
         :with-creator nil
         :headline-level 4
         :section-numbers nil
         :with-toc t
         :with-drawers t
         :html-link-home "./"
         :html-preamble nil
         :html-postamble t
         :html-head-extra ,my-head-extra
         :html-head-include-default-style nil
         :html-head-include-scripts nil

         ;;:exclude "\\^\\([0-9]\\{4\\}-[0-9]+-[0-9]+\\)"
         )
        ("org-site-static"
         :base-directory ,site-base-directory
         :publishing-directory ,site-publishing-directory
         :publishing-function org-publish-attachment
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|gz\\|tar\\|zip\\|bz2\\|xz\\|tex\\|txt\\|html\\|scm\\|key\\|svg"
         :recursive t
         )
        ("org-site-rss"
         :base-directory ,site-base-directory
         :base-extension "org"
         :publishing-directory ,site-rss-publishing-directory
         :publishing-function (org-rss-publish-to-rss)
         :html-link-home "http://mydomain.org/" ;;todo: ...
         :html-link-use-abs-url t
         )
        )
      )


;;;;;;;;;;;;;;;; wp-blog setup

;; documentation of org2blog: https://github.com/punchagan/org2blog/

(require 'auth-source) ;; or nothing if already in the load-path

(let (credentials)
  ;; only required if your auth file is not already in the list of auth-sources
  (add-to-list 'auth-sources "~/org/.wpauth")
  (setq credentials (auth-source-user-and-password "uberpwn"))
  (setq org2blog/wp-blog-alist
        `(("uberpwn"
           :url "http://joelholder.com/xmlrpc.php"
           :username ,(car credentials)
           :password ,(cadr credentials)))))
