

(defun ensure-directory (dir)
	"Utility function to create directories in a path if they do not already exist"
    (unless (file-exists-p dir)                                                               
	      (make-directory dir t)))

(defun package-refresh-and-install (&rest pkgs)
  "Utility function to refresh package contents and install several packages at once"
  (dolist (pkg pkgs)
    (unless (package-installed-p pkg)
      (package-refresh-contents)
      (package-install pkg))))


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
