;; Don't annoy me with those messages about active processes when I exit
(add-hook 'comint-exec-hook 
      (lambda () (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)))

;; A better version of kill-emacs bound to the standard C-x C-c
(defun my-kill-emacs ()
  "save some buffers, then exit unconditionally"
  (interactive)
  (save-some-buffers nil t)
  (kill-emacs))

(global-set-key (kbd "C-x C-c") 'my-kill-emacs)

;; Save here instead of littering current directory with emacs backup files
(setq backup-directory-alist `(("." . "~/.emacs.d/saves")))

;; Save place in files
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file "~/.emacs.d/.saved-places")

;; Add extra to load-path
(add-to-list 'load-path "~/.emacs.d/extra/")


;; Setup package repos
(require 'package)

(add-to-list 'package-archives
	     '("melpa-stable" . "http://stable.melpa.org/packages/") t)

(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

;; Initialize all the ELPA packages (what is installed using the packages commands)    
(package-initialize)

;; Ensure my packages are all installed
(defvar my-packages '(auto-complete
		      cider
		      ac-cider
		      company
		      paredit
		      rainbow-delimiters
		      ir-black-theme
		      ))

(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-install p)))



;; on OSXmake the command key the meta key
;;(setq ns-command-modifier 'meta)
 
;; Read in PATH from .bash_profile - nec to fix and find /usr/local/bin/lein
(if (not (getenv "TERM_PROGRAM"))
     (setenv "PATH"
           (shell-command-to-string "source $HOME/.profile && printf $PATH")))

;; set the path as terminal path [http://lists.gnu.org/archive/html/help-gnu-emacs/2011-10/msg00237.html]
(setq explicit-bash-args (list "--login" "-i"))

;; fix the PATH variable for GUI [http://clojure-doc.org/articles/tutorials/emacs.html#osx] 
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell
         (shell-command-to-string "$SHELL -i -l -c 'echo $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH))

(global-set-key [f7] 'paredit-mode)
(global-set-key [f9] 'cider-jack-in)

;;turn on global goodies
(add-hook 'after-init-hook 'global-company-mode)
;(add-hook 'after-init-hook 'global-auto-complete-mode)

;;extra clojure-mode goodies
;(require 'rainbow-delimiters)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook 'turn-on-eldoc-mode)
(add-hook 'clojure-mode-hook 'paredit-mode)


;;extra cider goodies
(add-hook 'cider-repl-mode-hook 'subword-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
(add-hook 'cider-repl-mode-hook 'auto-complete-mode)

;;
(add-hook 'cider-mode-hook 'ac-flyspell-workaround)
(add-hook 'cider-mode-hook 'ac-cider-setup)
(add-hook 'cider-repl-mode-hook 'ac-cider-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'cider-mode))


;; Show parenthesis mode
(show-paren-mode 1)

;; Turn off toolbar
(tool-bar-mode 0)

;; shell scripts
;;(setq-default sh-basic-offset 2)
;;(setq-default sh-indentation 2)

;; I like white on black
(set-background-color "black")
(set-foreground-color "honeydew")

;; http://emacsthemes.caisah.info/
;; theme
;; (load-theme 'ir-black t) 

;; Set bigger fonts
;; (set-default-font "Courier New-13")

;; Set up natural Undo/Redo (use OSX keys)
;(require 'redo+)
;(when (require 'redo nil 'noerror)
;(global-set-key (kbd "C-S-z") 'redo+)
;)
;(global-set-key (kbd "C-z") 'undo)

;; Undo/Redo
(require 'undo-tree)
(global-undo-tree-mode 1)

(defalias 'redo 'undo-tree-redo)
(global-set-key (kbd "C-z") 'undo) ; [Ctrl+z]
(global-set-key (kbd "C-a") 'redo) ; [Ctrl+a]  Workaround until I can Mac terminal accepting [Ctrl+Shift+z].
;(global-set-key (kbd "C-S-z") 'redo) ; [Ctrl+Shift+z]  Mac style
;(global-set-key (kbd "C-y") 'redo) ; [Ctrl+y] Microsoft Windows style






