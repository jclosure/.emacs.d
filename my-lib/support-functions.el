

(defun ensure-directory (dir)
    (unless (file-exists-p dir)                                                               
	      (make-directory dir t)))



;; Functions to hook into system clipboard
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
