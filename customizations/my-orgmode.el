
(load-library "support-functions")

;; inspired by see ref:
;; source: http://mescal.imag.fr/membres/arnaud.legrand/misc/init.php
;; source file: http://mescal.imag.fr/membres/arnaud.legrand/misc/init.org

;; good examples:
;; https://pavpanchekha.com/blog/org-mode-publish.html

;; dealing with conflicts, e.g. windmove, yasnippets, etc..
;; ref: http://orgmode.org/manual/Conflicts.html

;; setup org mode - http://orgmode.org/worg/org-tutorials/orgtutorial_dto.html
(use-package org
             :defer t
             :init
             (progn
               ;; handling links
               ;; http://orgmode.org/manual/Handling-links.html#Handling-links
               (define-key global-map "\C-cl" 'org-store-link)
               (define-key global-map "\C-ca" 'org-agenda)
             :config
             (progn
               (setq org-log-done t)
               (org-babel-do-load-languages
                'org-babel-load-languages
                '((elasticsearch . t)
                  (ruby . t)
                  (python . t)
                  (lisp . t)
                  (sh . t)
                  (R . t)
                  (ocaml . t)
                  (ditaa . t)
                  (dot . t)
                  (octave . t)
                  (sqlite . t)
                  (perl . t)
                  (org . t)
                  (makefile . t)))
               ;; To use this type <S and then TAB
               (add-to-list 'org-structure-template-alist
                            '("S" "#+begin_src ?\n\n#+end_src" "<src lang=\"?\">\n\n</src>")))
             ;; more convenient link navigation
             (define-key org-mode-map "\C-n" 'org-next-link)
             (define-key org-mode-map "\C-p" 'org-previous-link))
)


;; set default directory and agenda files for org
(setq org-directory "~/org/")
(setq org-agenda-files (list "~/org/work.org"
                             "~/org/school.org" 
                             "~/org/home.org"))
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
(setq org-hide-emphasis-markers t) ;; to hide the *,=, or / markers
(setq org-pretty-entities t)       ;; to have \alpha, \to and others display as utf8 http://orgmode.org/manual/Special-symbols.html

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



;;;;;;;;;;;;;;;;;;;;; PUBLISHING SETUP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; the publishing function!!!
(load "ox-publish")

;; to run execute:
;;(org-export-site)

;; bootstrap publishing setup
(progn
  
  (defun org-export-site ()
    "1-click blog publishing"
    (interactive)
    ;;(org-capture nil "b")
    (org-publish "org-site")
    (org-publish "org-site-static")) 

  ;; local setup
  
  (setq blog-base-directory "~/.emacs.d/org")
  (setq blog-publishing-directory "~/projects/www/org-site")

  ;; make sure directories are there
  (ensure-directory blog-base-directory)
  (ensure-directory blog-publishing-directory)


  ;; publishing configuration
  (setq org-publish-project-alist
        `(
          ("org-site"
           :base-directory ,blog-base-directory
           :publishing-directory ,blog-publishing-directory
           :publishing-function org-html-publish-to-html
           :table-of-contents nil
           :html-extension "html"
           :body-only nil
           :recursive t
           ;;:exclude "\\^\\([0-9]\\{4\\}-[0-9]+-[0-9]+\\)"
           )
          ("org-site-static"
           :base-directory ,blog-base-directory
           :publishing-directory ,blog-publishing-directory
           :publishing-function org-publish-attachment
           :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|gz\\|tar\\|zip\\|bz2\\|xz\\|tex\\|txt\\|html\\|scm\\|key\\|svg"
           :recursive t)
          ))
  )
