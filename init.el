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

(if (or (eq system-type 'darwin) (eq system-type 'gnu) (eq system-type 'gnu/linux) (eq system-type 'cygwin))
    (progn
	
		;; EXTEND PATH INTO EMACS
	    ;; Read in PATH from .profile or .bash_profile - nec to fix and find /usr/local/bin/lein
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

	    (when window-system (set-exec-path-from-shell-PATH)))
 )
 
 ;; wire up the osx pastboard
(if (or (eq system-type 'darwin))
    (progn
	   (global-set-key [?\C-x ?\C-y] 'pt-pbpaste)
	   (global-set-key [?\C-x ?\M-w] 'pt-pbcopy))
)

(if (or (eq system-type 'ms-dos) (eq system-type 'windows-nt))
	(cua-mode 1)
)


;; --------------------------------------------------
;; END - OSX/LINUX/WINDOWS RELATED - END
;; --------------------------------------------------




;; Setup package repos
(require 'package)

(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)

(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

;; Initialize all the ELPA packages (what is installed using the packages commands)    
(package-initialize)

;; Ensure my packages are all installed

(package-refresh-and-install 
	; global packages
		'tabbar
		'tabbar-ruler
		'ir-black-theme
		'sublime-themes
		'pretty-mode
		'undo-tree
		'workgroups2
		'auto-complete
		'neotree
	; clojure packages
		'cider
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
	; python packages - make sure to run jedi:install-server (requires pip install virtualenv)
		'elpy
		'jedi
)





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

;; window movement configuration
;; use Shift+arrow_keys to move cursor around split panes

;; for use in xterm-256 console mode

(if (equal "xterm-256color" (tty-type))
    (progn
      (define-key input-decode-map "\e[1;2A" [S-up])
      (define-key input-decode-map "\e[1;2B" [S-down])
      (define-key input-decode-map "\e[1;2C" [S-right])
      (define-key input-decode-map "\e[1;2D" [S-left])))

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
(require 'sublime-themes)
;(load-theme 'hickey 1)


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


; need to install flymake-google-cpplint and flymake-cursor packages
; NOTE: you must install cpplint with: pip install cpplint (note the dir it installs into, must be below)
; start flymake-google-cpplint-load
; let's define a function for flymake initialization
(defun my:flymake-google-init () 
  (require 'flymake-google-cpplint)
  (custom-set-variables
   '(flymake-google-cpplint-command "C:\\Anaconda3\\Scripts\\cpplint.exe"))
  (flymake-google-cpplint-load)
)
(add-hook 'c-mode-hook 'my:flymake-google-init)
(add-hook 'c++-mode-hook 'my:flymake-google-init)

; install google-c-style package
; start google-c-style with emacs
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)
; cedet is built in to emacs so no need to install a package to get semantic ide behavior
; just enable as follows
; turn on Semantic
(semantic-mode 1)
; let's define a function which adds semantic as a suggestion backend to auto complete
; and hook this function to c-mode-common-hook
(defun my:add-semantic-to-autocomplete() 
  (add-to-list 'ac-sources 'ac-source-semantic)
)
(add-hook 'c-mode-common-hook 'my:add-semantic-to-autocomplete)
; turn on ede mode 
(global-ede-mode 1)


;; PERSONAL PROJECT SYMBOLS FOR C++
; create a project for our program. (NOTE: THIS WILL BE PROJECT SPECIFIC.  RUN FROM IN EMACS MAYBE)
;(ede-cpp-root-project "my project" :file "~/projects/demos/cpp/my_program/src/main.cpp"
;         :include-path '("/../my_inc"))


; you can use system-include-path for setting up the system header file locations.
; turn on automatic reparsing of open buffers in semantic
(global-semantic-idle-scheduler-mode 1)

; PYTHON - enable repl with "C-c C-p" and transfer buffer to repl with "C-c C-c"
;(require 'python-mode)
;(setq-default py-split-windows-on-execute-function 'split-window-horizontally)

;(add-hook 'python-mode-hook 'python-shell-switch-to-shell)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:setup-keys t)                      ; optional
(setq jedi:complete-on-dot t)                 ; optional


;WORKGROUPS
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
 '(frame-background-mode (quote dark))
 '(tabbar-separator (quote (0.5))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;; '(default ((t (:background nil))))
 ;; '(tabbar-button ((t (:inherit tabbar-default :foreground "dark red"))))
 ;; '(tabbar-button-highlight ((t (:inherit tabbar-default))))
 ;; '(tabbar-default ((t (:inherit variable-pitch :background "#959A79" :foreground "black" :weight bold))))
 ;; '(tabbar-highlight ((t (:underline t))))
 ;; '(tabbar-selected ((t (:inherit tabbar-default :background "#95CA59"))))
 ;; '(tabbar-separator ((t (:inherit tabbar-default :background "#95CA59"))))
 ;; '(tabbar-unselected ((t (:inherit tabbar-default))))
 )


(set-default-font "Monaco 14") 
;(load-library  "blackboard-theme.el")
(load-library "afternoon-theme.el")
(load-theme 'afternoon t)
;(set-background-color "black")

;(when (display-graphic-p)
;    (set-background-color "darkgrey"))

;; some manual color settings
;(set-background-color "black")
;(set-foreground-color "honeydew")
;(global-hl-line-mode 1)
;(set-face-background 'hl-line "color-235")
;(set-face-foreground 'highlight nil)
;(set-face-attribute 'region nil :background "color-18")
;; set bigger fonts
; (set-default-font "Courier New-13")





;; ;; TABBAR
(tabbar-mode t)
(global-set-key [(control ?c) (left)] 'tabbar-backward)
(global-set-key [(control ?c) (right)] 'tabbar-forward)
(load-file "~/.emacs.d/extra/init-tabbar.el")
(load-file "~/.emacs.d/extra/my-tabbar-style.el")


;; tab bar setup - keep near bottom of file for gensym func resolution timing
;; (setq tabbar-ruler-global-tabbar 't) ; If you want tabbar
;; ;(setq tabbar-ruler-global-ruler 't) ; if you want a global ruler
;; ;(setq tabbar-ruler-popup-menu 't) ; If you want a popup menu.
;; ;(setq tabbar-ruler-popup-toolbar 't) ; If you want a popup toolbar
;; (require 'tabbar-ruler)
;; ;(load-library "my-tabbar-config.el")
;; ;(load-library "my-tabbar-style.el")
;; (setq tabbar-background-color "#959A79") ;; the color of the tabbar background


;; these don't work in osx terminal becaues of need for C-S-kp-next
(global-set-key (kbd "C-S-p") 'tabbar-backward-group)
(global-set-key (kbd "C-S-n") 'tabbar-forward-group)
(global-set-key (kbd "C-<") 'tabbar-backward)
(global-set-key (kbd "C->") 'tabbar-forward) ;; tabbar.el, put all the buffers on the tabs.
