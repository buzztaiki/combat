;;; combat.el --- Company backend transformer        -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Taiki Sugawara

;; Author: Taiki Sugawara <buzz.taiki@gmail.com>
;; Keywords: convenience, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 'pcase)
(require 'seq)
(require 'company)

(defgroup combat nil
  "Company backend transformer."
  :group 'company)

(defcustom combat-transformers nil
  "Transformer functions for `combat-complete'."
  :type '(repeat function))

(defun combat--backend-name (backend)
  (let ((name (symbol-name backend)))
    (if (string-match "^company-" name)
        (replace-match "" nil nil name)
      name)))

;; TODO group backend
(defun combat-backend-name-annotation-transformer (backend)
  (if (not (symbolp backend))
      backend
    (lambda (command &rest args)
      (let ((result (apply backend command args)))
        (if (eq command 'annotation)
            (concat result " " (combat--backend-name backend))
          result)))))
  
(defun combat-combine-backends (backend second-backend method)
  (if (and (consp backend) (memq second-backend backend))
      backend
    (append (if (consp backend) backend (list backend))
            (pcase method
              ((or :with :separate) (list method second-backend))
              (:merge (list second-backend))
              (_ (error "Unknown method %s" method))))))

(defun combat-with-yasnippet-transformer (backend)
  (combat-combine-backends backend 'company-yasnippet :with))

(defun combat--compose (fn &rest fns)
  (seq-reduce (lambda (f g)
                (lambda (&rest args)
                  (funcall f (apply g args))))
              fns fn))

(defun combat--transform-backends (backends)
  (if (null combat-transformers)
      backends
    (mapcar (apply #'combat--compose (reverse combat-transformers))
            backends)))

(defun combat-complete ()
  "Apply all `combat-transformers' to `company-backends' and do `company-complete'."
  (interactive)
  (let ((company-backends (combat--transform-backends company-backends)))
    (company-complete)))

(provide 'combat)

;;; combat.el ends here