(defun file-forms-list (file-name)
  (let ((file-forms nil))
    ;; Keep reading Lisp expressions, until we hit EOF,
    ;; and just add one entry for each toplevel form
    ;; to `file-forms'.
    (condition-case err
        (with-temp-buffer
          (insert-file file-name)
          (goto-char (point-min))
          (while (< (point) (point-max))
            (let* ((expr (read (current-buffer)))
                   (form (first expr)))
              (setq file-forms (cons form file-forms)))))
      (end-of-file nil))
    (reverse file-forms)))
 
(defun file-forms-alist (file-name)
  (let ((forms-table (make-hash-table :test #'equal)))
    ;; Build a hash that maps form-name => count for all the
    ;; top-level forms of the `file-name' file.
    (dolist (form (file-forms-list file-name))
      (let ((form-name (format "%s" form)))
        (puthash form-name (1+ (gethash form-name forms-table 0))
                 forms-table)))
    ;; Convert the hash table to an alist of the form:
    ;;    ((form-name . count) (form-name-2 . count-2) ...)
    (let ((forms-alist nil))
      (maphash (lambda (form-name form-count)
                 (setq forms-alist (cons (cons form-name form-count)
                                         forms-alist)))
               forms-table)
      forms-alist)))
 
(progn
  (insert "\n")
  (insert (format "%7s %s\n" "COUNT" "FORM-NAME"))
  (let ((total-forms 0))
    (dolist (fc (sort (file-forms-alist "~/.emacs.d/init.el")
                      (lambda (left right)
                        (> (cdr left) (cdr right)))))
      (insert (format "%7d %s\n" (cdr fc) (car fc)))
      (setq total-forms (+ total-forms (cdr fc))))
    (insert (format "%7d %s\n" total-forms "TOTAL"))))
