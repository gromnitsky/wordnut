;; -*- lexical-binding: t -*-

(setq
 vc-handled-backends nil
 argv (cdr argv))		       ; remove '--' from CL arguments

(defun pp-hash (hash)
  "Recursive describe hash func for nested hash-tables"
  (maphash (lambda (key value)
	     (pp key)
	     (princ " => ")
	     (if (hash-table-p value)
		 (progn
		   (princ " { ")
		   (terpri)
		   (describe-hash-descend value)
		   (princ " } "))
	       (pp value))
	     (terpri))
	   hash))


(provide 'tdd-helper)
