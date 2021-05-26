
(in-package :ulubis-lab)

(defun str (&rest body)
  (apply #'concatenate (cons 'string body)))
