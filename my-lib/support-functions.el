
(load "memoize.el")

;; memoized version 
(defun memo-package-refresh-contents ()
  (progn
    (package-refresh-contents)
    t))
(memoize 'memo-package-refresh-contents)

(defun package-refresh-and-install (&rest pkgs)
  "Utility function to refresh package contents and install several packages at once"
  (dolist (pkg pkgs)
    (unless (package-installed-p pkg)
      (if  (memo-package-refresh-contents)
          (package-install pkg)))))


(defun ensure-directory (dir)
    "Utility function to create directories in a path if they do not already exist"
    (unless (file-exists-p dir)                                                               
	      (make-directory dir t)))


(defun smart-line-beginning ()
  "Move point to the beginning of text on the current line; if that is already
the current position of point, then move it to the beginning of the line."
  (interactive)
  (let ((pt (point)))
    (beginning-of-line-text)
    (when (eq pt (point))
      (beginning-of-line))))


;; for indenting and unindenting
(defun my-indent-region (N)
  (interactive "p")
  (if (use-region-p)
      (progn (indent-rigidly (region-beginning) (region-end) (* N 4))
             (setq deactivate-mark nil))
    (self-insert-command N)))

(defun my-unindent-region (N)
  (interactive "p")
  (if (use-region-p)
      (progn (indent-rigidly (region-beginning) (region-end) (* N -4))
             (setq deactivate-mark nil))
    (self-insert-command N)))

(global-set-key ">" 'my-indent-region)
(global-set-key "<" 'my-unindent-region)



; if no selection just comment line or comment selection
(defun my-comment-line-or-region ()
  (interactive "*")
  (if (and mark-active transient-mark-mode)
      (comment-or-uncomment-region (region-beginning) (region-end) nil)
    (progn
      (save-excursion
	(move-beginning-of-line nil)
	(set-mark-command nil)
	(move-end-of-line nil)
	(comment-dwim nil)
	))))
(global-set-key (read-kbd-macro "C-;") 'my-comment-line-or-region)
(global-set-key (read-kbd-macro "M-;") 'comment-dwim)


;; Remap goto-line to show temporary the line number.
;; http://whattheemacsd.com//key-bindings.el-01.html
(global-set-key [remap goto-line] 'goto-line-with-feedback)
(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
	(linum-mode 1)
	(goto-line (read-number "Goto line: ")))
        (linum-mode -1)))


; Quickly try a lisp on the web downloading and evaluating it.
(defun my-try-el(url)
  "Quickly try a lisp file downloading and evaluating it"
  (interactive "sEmacs lisp url to retrieve: ")
  (progn
    (switch-to-buffer (url-retrieve-synchronously url))
    (save-excursion
      (goto-char (point-min))
      (delete-region (point) (search-forward "\n\n" nil t)))
    (eval-current-buffer)
    (emacs-lisp-mode)
        (font-lock-fontify-buffer)))

;https://github.com/milkypostman/dotemacs/blob/master/init.el
(defun my-rotate-windows ()
  "Rotate your windows"
  (interactive)
  (cond ((not (> (count-windows)1))
	 (message "You can't rotate a single window!"))
	(t
	 (setq i 1)
	 (setq numWindows (count-windows))
	 (while  (< i numWindows)
	   (let* (
		  (w1 (elt (window-list) i))
		  (w2 (elt (window-list) (+ (% i numWindows) 1)))

		  (b1 (window-buffer w1))
		  (b2 (window-buffer w2))

		  (s1 (window-start w1))
		  (s2 (window-start w2))
		  )
	     (set-window-buffer w1  b2)
	     (set-window-buffer w2 b1)
	     (set-window-start w1 s2)
	     (set-window-start w2 s1)
	                  (setq i (1+ i)))))))


;; Functions to hook into osx pasteboard
;; Use C-x C-y to paste C-x M-w to copy.

(defun pt-pbpaste ()
  "Paste data from pasteboard."
  (interactive)
  (shell-command-on-region
   (point)
   (if mark-active (mark) (point))
   "pbpaste" nil t))

(defun pt-pbcopy ()
  "Copy region to pasteboard."
  (interactive)
  (print (mark))
  (when mark-active
    (shell-command-on-region
     (point) (mark) "pbcopy")
    (kill-buffer "*Shell Command Output*")))
