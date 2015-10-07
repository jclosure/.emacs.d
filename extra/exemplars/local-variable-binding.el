
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Local-Variables.html


;; let binding
(progn
  
  (setq y 2)

  (let ((y 1)
        (z y))
    (list y z)))


;; let* binding
(progn

  (setq y 2)

  (let* ((y 1)
         (z y))
    (list y z)))





