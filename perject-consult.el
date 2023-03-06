;;; perject-consult.el --- Consult integration for Perject -*- lexical-binding: t -*-

;; Copyright (C) 2022, 2023 overideal

;; This program is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; Integrate perject with consult.
;; We define two buffer sources, namely one that shows the buffers belonging to
;; the current project and one that shows the buffers belonging to the current
;; collection.
;; These can be added to `consult-buffer-sources' so that the user
;; can conveniently focus on the buffers of the current project.

;;; Code:

(require 'perject)

;; Silence the byte compiler.
(declare-function consult--buffer-query "consult")
(declare-function consult--buffer-state "consult")


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
  "Buffer candidate source of buffers belonging to the current project.
This is a buffer candidate source for `consult-buffer'.")

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
  "Buffer candidate source of buffers belonging to the current collection.
This is a buffer candidate source for `consult-buffer'.")


(provide 'perject-consult)
;;; perject-consult.el ends here
