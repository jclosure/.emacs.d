;; Don't annoy me with those messages about active processes when I exit
(add-hook 'comint-exec-hook 
      (lambda () (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)))

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
 
;; Read in PATH from .bash_profile - nec to fix shell and find /usr/local/bin/lein
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

;; http://emacsthemes.caisah.info/                                                                                                                                                                                                           
;; theme                                                                                                                                                                                                                                     
;; (load-theme 'ir-black t) 

;; Set bigger fonts
;(set-default-font "Courier New-13")
