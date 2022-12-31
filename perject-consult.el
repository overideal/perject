;;; perject-consult.el --- Consult integration for Perject -*- lexical-binding: t -*-


;;; Commentary:

;; Integrate perject with consult.
;; We define two buffer sources, namely one that shows the buffers belonging to
;; the current project and one that shows the buffers belonging to the current
;; collection.
;; These can be added to `consult-buffer-sources' so that the user
;; can conveniently focus on the buffers of the current project.

;;; Code:

(require 'perject)
(require 'consult)


(defvar perject-consult--source-project-buffer
  `(:name     "Project Buffer"
    :narrow   ?j
    :category buffer
    :face     consult-buffer
    :history  buffer-name-history
    :state    ,#'consult--buffer-state
    :default  t
    :items
    ,(lambda ()
	   (consult--buffer-query
		:sort 'visibility
		:predicate
		(when (perject-current)
		  (lambda (buffer) (perject-is-assoc-with buffer (perject-current))))
        :as #'buffer-name)))
  "Buffer candidate source of buffers belonging to the current project for `consult-buffer'.")

(defvar perject-consult--source-collection-buffer
  `(:name     "Collection Buffer"
    :narrow   ?c
	:hidden   t
	:category buffer
    :face     consult-buffer
    :history  buffer-name-history
    :state    ,#'consult--buffer-state
    :default  t
    :items
    ,(lambda ()
	   (consult--buffer-query
		:sort 'visibility
		:predicate
		(when (perject-current)
		  (lambda (buffer) (perject-is-assoc-with buffer (car (perject-current)))))
        :as #'buffer-name)))
  "Buffer candidate source of buffers belonging to the current collection for `consult-buffer'.")


(provide 'perject-consult)
;;; perject-consult.el ends here
