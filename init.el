;; Add extra directories to load-path  
(add-to-list 'load-path (expand-file-name "~/.emacs.d/my-lib"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/extra"))

;; load some extra libraries
(load-library "support-functions.el")



;; Start up config
;(setq initial-major-mode (quote text-mode))
(setq inhibit-startup-message t)
(setq initial-scratch-message "")



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


;; TEMPORARY, BACKUPS, AN
;; evaluate temporary-file-directory in scratch to see what it is set to for your os
;; Save all tempfiles in $TMPDIR/emacs-$UID/
(defconst emacs-tmp-dir (format "%s%s%s/" temporary-file-directory "emacs-" (user-login-name)))

(setq backup-directory-alist
      `((".*" . ,(format "%s/%s" emacs-tmp-dir "backed-up-files"))))
(setq auto-save-file-name-transforms
      `((".*" ,(format "%s/%s" emacs-tmp-dir "auto-saved-files") t)))
(setq auto-save-list-file-prefix (format "%s/%s" emacs-tmp-dir "auto-saved-files"))

;; backup file control configuration
(setq make-backup-files t               ; backup of a file the first time it is saved.
      backup-by-copying t               ; don't clobber symlinks
      version-control t                 ; version numbers for backup files
      delete-old-versions t             ; delete excess backup files silently
      delete-by-moving-to-trash t
      kept-old-versions 6               ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions 9               ; newest versions to keep when a new numbered backup is made (default: 2)
      auto-save-default t               ; auto-save every buffer that visits a file
      auto-save-timeout 20              ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 200            ; number of keystrokes between auto-saves (default: 300)
)

;; Save place in files
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (format "%s/%s" emacs-tmp-dir "saved-places"))




;; ------------------------------------------------------
;; START - OSX/LINUX/WINDOWS RELATED - START
;; ------------------------------------------------------

;; (if (or (eq system-type 'darwin) (eq system-type 'gnu) (eq system-type 'gnu/linux) (eq system-type 'cygwin))
;;     (progn
	
;; 		;; EXTEND PATH INTO EMACS
;; 	    ;; Read in PATH from .profile or .bash_profile - nec to fix and find /usr/local/bin/lein
;; 	    (if (not (getenv "TERM_PROGRAM"))
;; 		(setenv "PATH"
;; 			(shell-command-to-string "source $HOME/.profile && printf $PATH")))

;; 	    ;; set the path as terminal path [http://lists.gnu.org/archive/html/help-gnu-emacs/2011-10/msg00237.html]
;; 	    (setq explicit-bash-args (list "--login" "-i"))

;; 	    ;; fix the PATH variable for GUI [http://clojure-doc.org/articles/tutorials/emacs.html#osx]
;; 	    (defun set-exec-path-from-shell-PATH ()
;; 	      (let ((path-from-shell
;; 		     (shell-command-to-string "$SHELL -i -l -c 'echo $PATH'")))
;; 		(setenv "PATH" path-from-shell)
;; 		(setq exec-path (split-string path-from-shell path-separator))))

;; 	    (when window-system (set-exec-path-from-shell-PATH)))
;;  )
 
 ;; wire up the osx pastboard
(if (or (eq system-type 'darwin))
    (progn
	   (global-set-key [?\C-x ?\C-y] 'pt-pbpaste)
	   (global-set-key [?\C-x ?\M-w] 'pt-pbcopy))
)


;; force start in homedirs
;(if (or (eq system-type 'ms-dos) (eq system-type 'windows-nt))
;  (setq default-directory (concat (getenv "USERPROFILE") "/"))
;  (setq default-directory (concat (getenv "HOME") "/")))




;; --------------------------------------------------
;; END - OSX/LINUX/WINDOWS RELATED - END
;; --------------------------------------------------




;; Define package repositories
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
;(add-to-list 'package-archives
;	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

;; Initialize all the ELPA packages (what is installed using the packages commands)    
(package-initialize)

;; Ensure my packages are all installed

(package-refresh-and-install 
	; global packages
		'dash
		'tabbar
		'tabbar-ruler
		'ir-black-theme
		'sublime-themes
		'pretty-mode
		'undo-tree
		'workgroups2
		'auto-complete
		'neotree
		'ido-ubiquitous
		'smex
		'projectile
		'magit
        ;; general
                'yaml-mode
	;; web
		'tagedit
                'web-mode
	; clojure packages
		'cider
		'clojure-mode
		'clojure-mode-extra-font-locking
		'ac-cider
		'company
		'paredit
		'rainbow-delimiters
	; c++ packages
		'yasnippet
		'auto-complete-c-headers
		'iedit
		'flymake-google-cpplint
		'flymake-cursor
		'google-c-style
	; python packages - make sure to run jedi:install-server (requires pip install virtualexnv)
		'elpy
		'jedi
        ; ruby
                'flymake-ruby
                'inf-ruby
                'robe
                'haml-mode
                'projectile-rails
        ; node
                'sws-mode
                'jade-mode
                
)



;; On OS X, an Emacs instance started from the graphical user
;; interface will have a different environment than a shell in a
;; terminal window, because OS X does not run a shell during the
;; login. Obviously this will lead to unexpected results when
;; calling external utilities like make from Emacs.
;; This library works around this problem by copying important
;; environment variables from the user's shell.
;; https://github.com/purcell/exec-path-from-shell
(if (eq system-type 'darwin)
    (package-refresh-and-install 'exec-path-from-shell))



;; show parenthesis mode
(show-paren-mode 1)
;; auto-insert/close bracket pairs
; (electric-pair-mode 1)
; highlight entire bracket expression
;(setq show-paren-style 'expression) 
; display line numbers in margin.
;(global-linum-mode 1) 


; clean wrapping of text
; (global-visual-line-mode 1)


;; turn off toolbar
(if window-system
    (tool-bar-mode 0))

;; shell scripts
;(setq-default sh-basic-offset 2)
;(setq-default sh-indentation 2)



;; Undo/Redo - activate with "C-x u"
(require 'undo-tree)
(global-undo-tree-mode 1)

(defalias 'redo 'undo-tree-redo)
;(global-set-key (kbd "C-z") 'undo) ; [Ctrl+z]
;(global-set-key (kbd "C-S-z") 'redo) ; [Ctrl+Shift+z]  Mac style
;(global-set-key (kbd "C-y") 'redo) ; [Ctrl+y] Microsoft Windows style



; GLOBAL STUFF

(setq scroll-step 1) ;; keyboard scroll one line at a time

(global-set-key (kbd "C-a") 'smart-line-beginning)

;; for use in xterm-256 console mode

(if (equal "xterm-256color" (tty-type))
    (progn
      (define-key input-decode-map "\e[1;2A" [S-up])
      (define-key input-decode-map "\e[1;2B" [S-down])
      (define-key input-decode-map "\e[1;2C" [S-right])
      (define-key input-decode-map "\e[1;2D" [S-left])))


;; for use in putty

(if (eq system-uses-terminfo t)
    (progn                              ;; PuTTY hack - needs to be in Linux or SCO mode
      (define-key key-translation-map [\e] [\M])
      (define-key input-decode-map "\e[H" [home])
      (define-key input-decode-map "\e[F" [end])
      (define-key input-decode-map "\e[D" [S-left])
      (define-key input-decode-map "\e[C" [S-right])
      (define-key input-decode-map "\e[A" [S-up])
      (define-key input-decode-map "\e[B" [S-down])
      (define-key input-decode-map "\e[C" [S-right])
      (define-key input-decode-map "\e[I" [prior])
      (define-key input-decode-map "\e[G" [next])
      (define-key input-decode-map "\e[M" [f1])
      (define-key input-decode-map "\e[Y" [S-f1])
      (define-key input-decode-map "\e[k" [C-f1])
      (define-key input-decode-map "\e\e[M" [M-f1])
      (define-key input-decode-map "\e[N" [f2])
      (define-key input-decode-map "\e[Z" [S-f2])
      (define-key input-decode-map "\e[l" [C-f2])
      (define-key input-decode-map "\e\e[N" [M-f2])
      (define-key input-decode-map "\e[O" [f3])
      (define-key input-decode-map "\e[a" [S-f3])
      (define-key input-decode-map "\e[m" [C-f3])
      (define-key input-decode-map "\e\e[O" [M-f3])
      (define-key input-decode-map "\e[P" [f4])
      (define-key input-decode-map "\e[b" [S-f4])
      (define-key input-decode-map "\e[n" [C-f4])
      (define-key input-decode-map "\e\e[P" [M-f4])
      (define-key input-decode-map "\e[Q" [f5])
      (define-key input-decode-map "\e[c" [S-f5])
      (define-key input-decode-map "\e[o" [C-f5])
      (define-key input-decode-map "\e\e[Q" [M-f5])
      (define-key input-decode-map "\e[R" [f6])
      (define-key input-decode-map "\e[d" [S-f6])
      (define-key input-decode-map "\e[p" [C-f6])
      (define-key input-decode-map "\e\e[R" [M-f6])
      (define-key input-decode-map "\e[S" [f7])
      (define-key input-decode-map "\e[e" [S-f7])
      (define-key input-decode-map "\e[q" [C-f7])
      (define-key input-decode-map "\e\e[S" [M-f7])
      (define-key input-decode-map "\e[T" [f8])
      (define-key input-decode-map "\e[f" [S-f8])
      (define-key input-decode-map "\e[r" [C-f8])
      (define-key input-decode-map "\e\e[T" [M-f8])
      (define-key input-decode-map "\e[U" [f9])
      (define-key input-decode-map "\e[g" [S-f9])
      (define-key input-decode-map "\e[s" [C-f9])
      (define-key input-decode-map "\e\e[U" [M-f9])
      (define-key input-decode-map "\e[V" [f10])
      (define-key input-decode-map "\e[h" [S-f10])
      (define-key input-decode-map "\e[_" [C-f10])
      (define-key input-decode-map "\e\e[V" [M-f10])
      (define-key input-decode-map "\e[W" [f11])
      (define-key input-decode-map "\e[i" [S-f11])
      (define-key input-decode-map "\e[u" [C-f11])
      (define-key input-decode-map "\e\e[W" [M-f11])
      (define-key input-decode-map "\e[X" [f12])
      (define-key input-decode-map "\e[j" [S-f12])
      (define-key input-decode-map "\e[v" [C-f12])
      (define-key input-decode-map "\e\e[X" [M-f12])))


(windmove-default-keybindings)


(setq windmove-wrap-around t)


;;turn on global goodies
(add-hook 'after-init-hook 'global-company-mode)

;(add-hook 'after-init-hook 'global-auto-complete-mode)

;; neotree setup
(add-to-list 'load-path "~/projects/")
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

;; add | between linenumbers
(setq linum-format "%4d \u2502 ")
(linum-mode)

;; THEMES
;; (require 'sublime-themes)
;(load-theme 'hickey 1)

;; GENERAL LANGUAGES
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))


;; WEB
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(require 'haml-mode)


; RUBY
(add-hook 'ruby-mode-hook 'flymake-ruby-load)
(add-hook 'ruby-mode-hook 'robe-mode)
(eval-after-load 'company
  '(push 'company-robe company-backends))
(add-hook 'robe-mode-hook 'ac-robe-setup)
(add-hook 'projectile-mode-hook 'projectile-rails-on)

;; NODE
;; jade
(require 'sws-mode)
(require 'jade-mode)
(add-to-list 'auto-mode-alist '("\\.styl\\'" . sws-mode))



;CLOJURE STUFF

;; global function keys
(global-set-key [f7] 'paredit-mode)
(global-set-key [f9] 'cider-jack-in)

;;extra clojure-mode goodies
;(require 'rainbow-delimiters)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook 'turn-on-eldoc-mode)
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'pretty-mode)


;;extra cider goodies
(add-hook 'cider-repl-mode-hook 'subword-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
(add-hook 'cider-repl-mode-hook 'auto-complete-mode)
;Dont go to REPL buffer when starting Cider:

(setq cider-repl-pop-to-buffer-on-connect nil)


;;
(add-hook 'cider-mode-hook 'ac-flyspell-workaround)
(add-hook 'cider-mode-hook 'ac-cider-setup)
(add-hook 'cider-repl-mode-hook 'ac-cider-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'cider-mode))


; C++ STUFF
; setup ref: https://www.youtube.com/watch?v=HTUE03LnaXA
; .emacs ref: http://barisyuksel.com/cppmode/.emacs

; debugging c++-mode
; (debug (c++-mode t))


; make sure auto-complete package installed
; start auto-complete within emacs
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)



; initialize default snippets directory
(ensure-directory "~/.emacs.d/snippets")
; start yasnippet
(require 'yasnippet)
;; set custom snippets directories
; (setq yas-snippet-dirs '("~/emacs.d/my-snippets"
;  			   "~/Downloads/interesting-snippets"))
(yas-global-mode 1)


; define a function which initializes auto-complete-c-headers
; NOTE: the paths below may change as Xcode evolves.  be prepared to change them
; NOTE: to find c headers in system use this command
; gcc -xc++ -E -v -

; setting up c header awareness here for OSX - Setup Linux and Windows similarly
(if (eq system-type 'darwin)
    (defun my:ac-c-header-init ()
      (require 'auto-complete-c-headers)
      (add-to-list 'ac-sources 'ac-source-c-headers)
      (add-to-list 'achead:include-directories '"/usr/include")
      (add-to-list 'achead:include-directories '"/usr/local/include")
      (add-to-list 'achead:include-directories '"/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1")
      (add-to-list 'achead:include-directories '"/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../lib/clang/6.0/include")
      (add-to-list 'achead:include-directories '"/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include")
      (add-to-list 'achead:include-directories '"/System/Library/Frameworks")
      (add-to-list 'achead:include-directories '"/Library/Frameworks")
      ))



; now lets call it from c/c++ hooks
(add-hook 'c++-mode-hook 'my:ac-c-header-init)
(add-hook 'c-mode-hook 'my:ac-c-header-init)


; ; Fix iedit bug in Mac
(define-key global-map (kbd "C-c ;") 'iedit-mode)


;; need to install flymake-google-cpplint and flymake-cursor packages
;; NOTE: you must install cpplint with: pip install cpplint (note the dir it installs into, must be below)
;; start flymake-google-cpplint-load
;; let's define a function for flymake initialization
(defun my:flymake-google-init () 
  (require 'flymake-google-cpplint)
  (custom-set-variables
   '(flymake-google-cpplint-command "cpplint")) ; cpplint executable in $PATH, e.g. /usr/local/bin/cpplint
  (flymake-google-cpplint-load)
)
(add-hook 'c-mode-hook 'my:flymake-google-init)
(add-hook 'c++-mode-hook 'my:flymake-google-init)

;; install google-c-style package
;; start google-c-style with emacs
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)
;; cedet is built in to emacs so no need to install a package to get semantic ide behavior
;; just enable as follows
;; turn on Semantic
(semantic-mode 1)
;; let's define a function which adds semantic as a suggestion backend to auto complete
;; and hook this function to c-mode-common-hook
(defun my:add-semantic-to-autocomplete() 
  (add-to-list 'ac-sources 'ac-source-semantic)
)
(add-hook 'c-mode-common-hook 'my:add-semantic-to-autocomplete)
;; turn on ede mode 
(global-ede-mode 1)


;; PERSONAL PROJECT SYMBOLS FOR C++
;; create a project for our program. (NOTE: THIS WILL BE PROJECT SPECIFIC.  RUN FROM IN EMACS MAYBE)
;(ede-cpp-root-project
;  "my project"
;  :file "~/projects/demos/cpp/my_program/src/main.cpp"
;  :include-path '("/../my_inc"))


;; you can use system-include-path for setting up the system header file locations.
;; turn on automatic reparsing of open buffers in semantic
(global-semantic-idle-scheduler-mode 1)

;; PYTHON
;; enable repl with "C-c C-p" and transfer buffer to repl with "C-c C-c"
;(require 'python-mode)
;(setq-default py-split-windows-on-execute-function 'split-window-horizontally)

;; setting up IPython
(progn 
  ;; NOTE: set this to the correct path for your python installation in windows
  (if (or (eq system-type 'ms-dos) (eq system-type 'windows-nt))
      (setq
       python-shell-interpreter "C:\\WinPython-64bit-2.7.9.3\\python-2.7.9.amd64\\python.exe"
       python-shell-interpreter-args "-i C:\\WinPython-64bit-2.7.9.3\\python-2.7.9.amd64\\Scripts\\ipython.exe console --pylab"
       ;; turning off emacs warnings in windows because of interactive python warning.  dirty: todo - cleaner solution
       warning-suppress-types '((emacs)))
    (setq
     python-shell-interpreter "ipython"))

  (setq
   ;python-shell-interpreter "ipython"
   ;python-shell-interpreter-args ""
   python-shell-prompt-regexp "In \\[[0-9]+\\]: "
   python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
   python-shell-completion-setup-code
   "from IPython.core.completerlib import module_completion"
   python-shell-completion-module-string-code
   "';'.join(module_completion('''%s'''))\n"
   python-shell-completion-string-code
   "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"))


;(add-hook 'python-mode-hook 'python-shell-switch-to-shell)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:setup-keys t)                      ; optional
(setq jedi:complete-on-dot t)                 ; optional
(elpy-enable) ; http://elpy.readthedocs.org/en/latest/ide.html#interactive-python - keystrokes documentation
;(elpy-use-ipython) ;deprecated



;: WORKGROUPS
;; workgroups2 enables saving window configurations, etc..
;(require 'workgroups2)

;; Change workgroups session file
;(setq wg-session-file "~/.emacs.d/.emacs_workgroups")

;; What to do on Emacs exit / workgroups-mode exit?
;(setq wg-emacs-exit-save-behavior           'save)      ; Options: 'save 'ask nil
;(setq wg-workgroups-mode-exit-save-behavior 'save)      ; Options: 'save 'ask nil

;(workgroups-mode 1)


;; SET DARK
;; run (customize) in scratch and search for what you want to customize
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(frame-background-mode (quote dark))
 '(tabbar-separator (quote (0.5))))



;; TABBAR
(tabbar-mode t)
(global-set-key [(control ?c) (left)] 'tabbar-backward)
(global-set-key [(control ?c) (right)] 'tabbar-forward)
(load-file "~/.emacs.d/extra/init-tabbar.el")
(load-file "~/.emacs.d/extra/my-tabbar-style.el")



;; these don't work in osx terminal becaues of need for C-S-kp-next
(global-set-key (kbd "C-S-p") 'tabbar-backward-group)
(global-set-key (kbd "C-S-n") 'tabbar-forward-group)
(global-set-key (kbd "C-<") 'tabbar-backward)
(global-set-key (kbd "C->") 'tabbar-forward) ;; tabbar.el, put all the buffers on the tabs.




;;;;
;; Customization
;;;;

;; Add a directory to our load path so that when you `load` things
;; below, Emacs knows where to look for the corresponding file.
(add-to-list 'load-path "~/.emacs.d/customizations")

;; Sets up exec-path-from-shell so that Emacs will use the correct
;; environment variables
(load "shell-integration.el")

;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.
(load "navigation.el")

;; These customizations change the way emacs looks and disable/enable
;; some user interface elements
(load "ui.el")

;; These customizations make editing a bit nicer.
(load "editing.el")

;; Hard-to-categorize customizations
(load "misc.el")


;; For editing lisps
(load "elisp-editing.el")

;; Langauage-specific
(load "setup-clojure.el")
(load "setup-js.el")                                     


;; late fly-make support for jade templates
(progn
  (defun flymake-jade-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                 'flymake-create-temp-intemp))
     (local-file (file-relative-name
                  temp-file
                  (file-name-directory buffer-file-name)))
     (arglist (list local-file)))
    (list "jade" arglist)))
(setq flymake-err-line-patterns
       (cons '("\\(.*\\): \\(.+\\):\\([[:digit:]]+\\)$"
              2 3 nil 1)
            flymake-err-line-patterns))
(add-to-list 'flymake-allowed-file-name-masks
         '("\\.jade\\'" flymake-jade-init)))
