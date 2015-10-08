;; Add extra directories to load-path  
(add-to-list 'load-path (expand-file-name "~/.emacs.d/my-lib"))

;; load some extra libraries
(load-library "support-functions")

(add-to-list 'load-path (expand-file-name "~/.emacs.d/extra"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/customizations"))

;; emacs25 workarounds
(load-library "emacs25.hacks")

(delete-selection-mode 1)




;; Start up config
;(setq initial-major-mode (quote text-mode))
(setq inhibit-startup-message t)
(setq initial-scratch-message "")

;; motd
(add-hook 'emacs-startup-hook
          (lambda ()
            (when (string= (buffer-name) "*scratch*")
              (progn 
                (animate-string ";; Your command is my wish..." (/ (frame-height) 2)))
                (goto-char (point-min)))))


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
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/" ) t)            
;(add-to-list 'package-archives
;             '("marmalade" . "http://marmalade-repo.org/packages/") t)
;(add-to-list 'package-archives
;	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))


;; Initialize all the ELPA packages (what is installed using the packages commands)    
(package-initialize)

;; Ensure my packages are all installed

(package-refresh-and-install

        ;; testing

        ;; from org repo because org-20150929 broken
        ;; 
        'org-plus-contrib 
        
 
        ;; global packages
        'dash
        'use-package
        'helm
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
        'flx-ido ;;TESTING
        'smex
        'projectile
        'helm-projectile
        'magit
        'gist
        

        ;; org
        'gntp
        'org-gcal
        
        ;; general
        'yaml-mode

        ;; web
        'tagedit
        'web-mode

        ;; clojure packages
        'cider
        'clojure-mode
        'clojure-mode-extra-font-locking
        'ac-cider
        'company
        'paredit
        'rainbow-delimiters

        ;; c++ packages
        'yasnippet
        'auto-complete-c-headers
        'iedit
        'flymake-google-cpplint
        'flymake-cursor
        'google-c-style

        ;; python packages - make sure to run jedi:install-server (requires pip install virtualexnv)
        'elpy
        'jedi

        ;; ruby
        'flymake-ruby
        'inf-ruby
        'robe
        'haml-mode
        'projectile-rails

        ;; node
        'sws-mode
        'jade-mode

        ;; elasticsearch
        'es-mode
        'logstash-conf

        ;; latex
        'latex-preview-pane
)


(require 'use-package)

;; On OS X, an Emacs instance started from the graphical user
;; interface will have a different environment than a shell in a
;; terminal window, because OS X does not run a shell during the
;; login. Obviously this will lead to unexpected results when
;; calling external utilities like make from Emacs.
;; This library works around this problem by copying important
;; environment variables from the user's shell.
;; https://github.com/purcell/exec-path-from-shell

;; (if (eq system-type 'darwin)
;;     (package-refresh-and-install 'exec-path-from-shell))



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



;;; GLOBAL STUFF

;; turning company-mode on globally
(global-company-mode t)

;; turning projectile on globally
(projectile-global-mode)
;; (setq helm-projectile-fuzzy-match nil)
;; (require 'helm-projectile)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

;; keyboard scroll one line at a time

;; scroll one line at a time (less "jumpy" than defaults)
(setq scroll-step 1) ;; keyboard scroll one line at a time

;; testing mouse setting: 
;;(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
;;(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
;;(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse




;; front of line jump fix
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


;;; windmove configuration
(windmove-default-keybindings)
(setq windmove-wrap-around nil)

;; Here I override the default user-error message because I don't think just because a window isn't there that it's an error.
;; It was annoying me...

;; Selects the window that's hopefully at the location returned by
;; `windmove-other-window-loc', or screams if there's no window there.
(defun windmove-do-window-select (dir &optional arg window)
  "Move to the window at direction DIR.
DIR, ARG, and WINDOW are handled as by `windmove-other-window-loc'.
If no window is at direction DIR, an error is signaled."
  (let ((other-window (windmove-find-other-window dir arg window)))
    (cond ((null other-window)
           (user-error ""))
          ((and (window-minibuffer-p other-window)
                (not (minibuffer-window-active-p other-window)))
           (user-error "Minibuffer is inactive"))
          (t
           (select-window other-window)))))


;;turn on global goodies

; setup global auto complete
(add-hook 'after-init-hook 'global-auto-complete-mode)

;; setup company mode
(add-hook 'after-init-hook 'global-company-mode)

;; neotree setup
(add-to-list 'load-path "~/projects/")
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

;; add | between linenumbers
;;(setq linum-format "%4d \u2502 ")
(setq linum-format "%4d ")
(linum-mode)

;; THEMES
;; (require 'sublime-themes)
;(load-theme 'hickey 1)

;; GENERAL LANGUAGES
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; ALL LISPS
;; turn on pretty mode only for lisps
(defun my/enable-pretty-mode-for-lisps
    (dolist (x '(scheme emacs-lisp lisp))
      (add-hook 
       (intern (concat (symbol-name x) "-mode-hook"))
       'turn-on-pretty-mode)))
;;(my/enable-pretty-mode-for-lisps)

;; COMMON LISP
; ref - https://www.youtube.com/watch?v=VnWVu8VVDbI
; replace "sbcl" with the path to your implementation
; books - https://www.youtube.com/watch?v=YwDpjDZOxF0
(if (executable-find "sbcl")
    (progn 
      (setq inferior-lisp-program "sbcl")
      ; setup slime
      (load (expand-file-name "~/quicklisp/slime-helper.el"))))

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


;; ELASTICSEARCH - note: set to your desired es host url
(add-to-list 'auto-mode-alist '("\\.es$" . es-mode))

; for logstash configuration support
; run logstash-conf-mode on the file

(setq  es-default-url "http://localhost:9200/_search?pretty=true")

;; ;docker
;; (setq  es-default-url "http://192.168.59.103:9200/_search?pretty=true")

;; ;place
;; (setq  es-default-url "http://atlcrmbestgv01:9200/_search?pretty=true") 
;; (setq  es-default-url "http://ausfusppdap00:9200/_search?pretty=true")
;; (setq  es-default-url "http://atlesbdv01:9200/_search?pretty=true")
;; (setq  es-default-url "http://atlesbanlqv01:9200/_search?pretty=true")
;; (setq  es-default-url "http://atlesbanlpv01:9200/_search?pretty=true")




;; RUBY

;; (add-hook 'ruby-mode-hook 'inf-ruby)
(add-hook 'ruby-mode-hook 'flymake-ruby-load)

;;(add-hook 'ruby-mode-hook 'start-robe)

(add-hook 'ruby-mode-hook 'robe-mode)

;; auto-complete from company in robe
;; (eval-after-load 'company
;;   '(push 'company-robe company-backends))
(add-hook 'robe-mode-hook 'ac-robe-setup)

;; rails support in projectile
;; (projectile-global-mode)
(add-hook 'projectile-mode-hook 'projectile-rails-on)

(require 'inf-ruby)
(add-hook 'after-init-hook 'inf-ruby-switch-setup)
(add-hook 'after-init-hook (lambda () (setq inf-ruby-default-implementation "pry"))) 

;; note: to use ruby dev stuff
;; M-x inf-ruby
(global-set-key (kbd "C-c r r") 'inf-ruby)
;; M-x robe-start



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



;; C/C++ ENV LOADER FUNCTION
(defun setup-cpp-env ()
  (progn

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

    
    ))


;; turn on Semantic
(semantic-mode 1)
;; you can use system-include-path for setting up the system header file locations.
;; turn on automatic reparsing of open buffers in semantic
(global-semantic-idle-scheduler-mode 1)

(add-hook 'c++-mode-hook 'setup-cpp-env)
(add-hook 'c-mode-hook 'setup-cpp-env)


;; SETUP PYTHON ENVIRONMENT
(defun setup-python-env ()

    ;; enable repl with "C-c C-p" and transfer buffer to repl with "C-c C-c"
    ;(require 'python-mode)
    ;(setq-default py-split-windows-on-execute-function 'split-window-horizontally)

    ;; setting up IPython
    (progn 
      ;; NOTE: set this to the correct path for your python installation in windows
      (if (or (eq system-type 'ms-dos) (eq system-type 'windows-nt))
          (setq
           python-shell-interpreter "C:\\WinPython-64bit-2.7.10.2\\python-2.7.10.amd64\\python.exe"
           python-shell-interpreter-args "-i C:\\WinPython-64bit-2.7.10.2\\python-2.7.10.amd64\\Scripts\\ipython.exe console"
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
    ;(add-hook 'python-mode-hook 'jedi:setup)
    (setq jedi:setup-keys t)                      ; optional
    (setq jedi:complete-on-dot t)                 ; optional
    (jedi:setup)
    (elpy-enable) ; http://elpy.readthedocs.org/en/latest/ide.html#interactive-python - keystrokes documentation
    ;(elpy-use-ipython) ;deprecated

  
  )

;;(add-hook 'python-mode-hook 'setup-python-env)
(setup-python-env)

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
;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(coffee-tab-width 2)
;;  '(frame-background-mode (quote dark))
;;  '(tabbar-separator (quote (0.5))))



;; TABBAR
(tabbar-mode t)
(load "my-tabbar-style.el")

(global-set-key [(control ?c) (left)] 'tabbar-backward)
(global-set-key [(control ?c) (right)] 'tabbar-forward)

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


;; ;; Sets up exec-path-from-shell so that Emacs will use the correct
;; ;; environment variables
(load "shell-integration.el")

;; ;; These customizations make it easier for you to navigate files,
;; ;; switch buffers, and choose options from the minibuffer.
(load "navigation.el")

;; These customizations change the way emacs looks and disable/enable
;; some user interface elements
(load "ui.el")

;; These customizations make editing a bit nicer.
(load "editing.el")

;; Hard-to-categorize customizations
(load "misc.el")


;; MY ORG-MODE SETUP
(load "setup-orgmode.el")

;; For editing lisps
(load "elisp-editing.el")

;; Langauage-specific
(load "setup-clojure.el")
(load "setup-js.el")                                     


;; late fly-make support for jade templates
(progn
  (require 'flymake)
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


;; set modeline for active buffer to be visually salient
;; (custom-set-faces
;;  '(mode-line ((t (:foreground "white" :background "dark slate gray")))))

; (require 'oneonone)
; (1on1-emacs)


;; configure TRAMP (Transparent Remote (file) Access, Multiple Protocol) 
(require 'tramp)
(setq tramp-default-method "scp")
;; example: C-x C-f /user@your.host.com:/path/to/file
;; If you don't want to enter your password every time you open or save a file consider using Public Key Authentication.



;; nyan mode
;; (defun toggle-nyan-mode (&optional frame)
;;   (when (display-graphic-p frame)
;;       (nyan-mode t)))
;; (add-hook 'after-init-hook 'toggle-nyan-mode)
;; (add-hook 'after-make-frame-functions 'toggle-nyan-mode)



;; comment/uncomment keybindings
;; normally:
;; C-/ for undo
;; C-S-/ for redo
(global-set-key (kbd "C-S-c") 'comment-region)
(global-set-key (kbd "C-S-u") 'uncomment-region)



;;; IRC CONFIGURATION

(use-package erc
  :ensure t
  :defer t
  :config
  (setq erc-autojoin-channels-alist '(("freenode.net"
                                       "#org-mode"
                                       "#hacklabto"
                                       "#emacs"))
        erc-server "irc.freenode.net"
        erc-nick "jclosure"))



;;;; LATEX CONFIGURATION

;; http://www.latexbuch.de/install-latex-windows-7/#x1-160003.4

(load "auctex.el" nil t t)
(require 'tex-mik)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
(setq TeX-PDF-mode t)

(if (or (eq system-type 'ms-dos) (eq system-type 'windows-nt))

    ;; C-c once compiles the latex code
    ;; C-c again View's it
    ;; To enable smooth live previewing in emacs use latex-preview-pane-mode or latex-preview-pane-enable

    ;; note: latex-preview-pane uses doc-view to render pdfs
    ;; 
    ;;       for latex-preview-pane to render pdf's, the following must be the case:
    ;;       1. ghostscript must be installed and gswin64c.exe must be in the path 
    ;;       2. you must be using emacs from the gui
    ;;       3. emacs must be compiled with xpm support
    ;;
    ;;       once all of these criteria have been met the doc-view command becomes available and just works

    ;; docview how to: http://www.gnu.org/software/emacs/manual/html_node/emacs/Document-View.html

    ;; this tells docview what to use for rendering pdf (it's in my path)
    (setq doc-view-ghostscript-program "gswin64c")


    ;;; setup ghostscript as the devault viewer for "View" command, else it defaults to "start <default_prog>"
    ;; (setq TeX-output-view-style '("^pdf$" "." "gswin64.exe %o"))
    ;; (setq TeX-view-program-list
    ;;       '(("GhostScript" "gswin64.exe %o")
    ;;         ))
    ;; (add-hook 'LaTeX-mode-hook
    ;;             (lambda ()
    ;;               (setq TeX-view-program-selection '((output-pdf "GhostScript")
    ;;                                                  (output-dvi "Yap")))))

)



;;;;;;;;;; FLX-IDO (MORE POWERFUL)

(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)

;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

;; If don't want to use the flx's highlights you can turn them off like this:
;;(setq flx-ido-use-faces nil)


;;;;;;;;;;;;; GOOGLE CALENDAR FOR ORG-MODE

;; https://github.com/myuhe/org-gcal.el
(setf my-gcal "~/projects/google/my-gcal.el")
(if (file-exists-p my-gcal)
    (progn
      (defun my/org-gcal-notify (title mes)
        (message "%s - %s" title mes))
      (use-package org-gcal
        :init (fset 'org-gcal-notify 'my/org-gcal-notify)
        :config (load my-gcal))
      ))


;;;;;;;;;;;;;;;;; TIMERS ;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;; agenda ;;;;;;;;;;

;; source: http://orgmode.org/worg/org-hacks.html

;; these function's keep the agenda front and center and up to date.

(defun my/jump-to-org-agenda ()
  (interactive)
  (let ((buf (get-buffer "*Org Agenda*"))
        wind)
    (if buf
        (if (setq wind (get-buffer-window buf))
            (select-window wind)
          (if (called-interactively-p)
              (progn
                (select-window (display-buffer buf t t))
                (org-fit-window-to-buffer)
                ;; (org-agenda-redo)
                )
            (with-selected-window (display-buffer buf)
              (org-fit-window-to-buffer)
              ;; (org-agenda-redo)
              )))
      (call-interactively 'org-agenda-list)))
  ;;(let ((buf (get-buffer "*Calendar*")))
  ;;  (unless (get-buffer-window buf)
  ;;    (org-agenda-goto-calendar)))
  )

;; open agenda on start
(my/jump-to-org-agenda)

;; if its idle for n seconds switch to agenda
(run-with-idle-timer 300 t 'my/jump-to-org-agenda)


(defun my/org-agenda-redo-in-other-window ()
  "Call org-agenda-redo function even in the non-agenda buffer."
  (interactive)
  (let ((agenda-window (get-buffer-window org-agenda-buffer-name t)))
    (when agenda-window
      (with-selected-window agenda-window (org-agenda-redo)))))

;; refresh agenda view every n seconds
(run-at-time nil 300 'my/org-agenda-redo-in-other-window)

;; testing: refresh google calendar with agenda every n seconds
;; create an idle timer that runs org-gcal-sync
(run-at-time nil 300 'org-gcal-sync)

;; helpers

;; try this out!..
(defun my/org-agenda-reschedule-to-today ()
  (interactive)
  (flet ((org-read-date (&rest rest) (current-time)))
    (call-interactively 'org-agenda-schedule)))




;;;;;;;;;;;;;;;; windows tools ;;;;;;;;;;;;;;;;;;;;;;;;;

;; (when (eq system-type 'windows-nt)
;;   (setenv "CYGWIN" "nodosfilewarning")
;;   ;; (setq shell-file-name "C:/emacs/libexec/emacs/24.4/i686-pc-mingw32/cmdproxy.exe")
;;   (add-hook 'comint-output-filter-functions 'shell-strip-ctrl-m nil t)
;;   (add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt nil t))








;;;; recursive grep with helm

;; You could try helm-do-grep. Calling it with a prefix arg gives you a recursive grep, as explained in the Helm Wiki.

;; If you want to launch helm-do-grep recursively without starting helm-find-files, do:

;; C-u helm-command-prefix-key M-g s

;; NOTE: If you forget to hit C-u before M-g s you can do it after file selection.

;; You will be prompted for selecting in which category of files to search: Use the wilcard syntax like *.el for example (search in only .el files).

;; By default, the extension of the file at point is used when the cursor is on a filename. If the cursor is at root of a directory, all the filename extensions found in the directory and not matching grep-find-ignored-files are inserted into the prompt.
;; Try it like so:

;; C-u helm-command-prefix-key M-g s




;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(coffee-tab-width 2)
;;  '(tabbar-separator (quote (0.5))))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(tabbar-modified ((t (:inherit tabbar-default :foreground "red4" :box (:line-width 1 :color "white" :style released-button)))))c)
