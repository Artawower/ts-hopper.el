;;; ts-hopper.el --- ts-package to quick navigation via treesit                     -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Artur Yaroshenko

;; Author: Artur Yaroshenko <artawower@protonmail.com>
;; URL: https://github.com/artawower/ts-hopper.el
;; Package-Requires: ((emacs "29.1"))
;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;; This package provides a way to quick navigate through most necessary nodes
;;; of editable document.

;;; Code:

(defcustom ts-hopper-mode-hops-rules
  '((typescript-ts-mode . ts-hopper--ecmascript-rules))
  "Alist of major modes and functions to get hops rules for them."
  :type '(alist :key-type symbol :value-type function)
  :group 'ts-hopper)

(defcustom ts-hopper--ecmascript-rules
  '((fn          . ("method_definition" "function"))
    (class       . ("class_declaration"))
    (significant . ("class_declaration" "method_definition" "function"))
    (statement   . ("object" "statement_block")))
  "Rules for ecmascript modes."
  :type '(alist :key-type symbol :value-type list)
  :group 'ts-hopper)

(defcustom ts-hopper--highlight-face 'highlight
  "Face to highlight current node."
  :type 'face
  :group 'ts-hopper)

(defcustom ts-hopper--highlight-timeout 0.5
  "Time to highlight current node.
When not provided highlight will be disabled."
  :type 'number
  :group 'ts-hopper)

(defvar ts-hopper-visible-timer nil
  "Timer to hide overlay on the current node.")

(defvar-local ts-hopper--overlay nil
  "Overlay to highlight current node.")

(defvar-local ts-hopper--last-hope-node nil
  "Last node that was hopped to.")


(defun ts-hopper--find-target (node-types direction &optional node)
  "Find target node of NODE-TYPES.

DIRECTION could be `forward' or `backward'.
NODE is a node to start from."
  (let* ((node (or node (treesit-node-on (point) (point))))

         (node (if (eq direction 'forward)
                   (or (treesit-node-child node 0)
                       (treesit-node-next-sibling node)
                       (treesit-node-next-sibling
                        (treesit-parent-until node (lambda (node)
                                                     (treesit-node-next-sibling node)))))
                 (or (treesit-node-child (treesit-node-prev-sibling node)
                                         (- (or (treesit-node-child-count (treesit-node-prev-sibling node)) 0) 1))
                     (treesit-node-prev-sibling node)
                     (treesit-parent-until node (lambda (node)
                                                  (treesit-node-prev-sibling node))))
                 ))
         (node-type (treesit-node-type node))


         (current-node-found-p (and (member node-type node-types) (not (equal (point) (treesit-node-start node))))))

    (cond (current-node-found-p node)
          (node (ts-hopper--find-target node-types direction node))
          (t nil))))


(defun ts-hopper--execute-by-mode (rule direction)
  "Execute command by current major mode and RULE.

DIRECTION could be `forward' or `backward' it will be
passed into function for finding node."

  (let* ((mode-rules (symbol-value (alist-get major-mode ts-hopper-mode-hops-rules)))
         (rules (cdr-safe (assoc rule mode-rules))))
    (if rule (ts-hopper--find-target rules direction)
      (error "No rules [%s] for [%s]" rule major-mode))))

(defun ts-hopper--start-clear-overlay-timer ()
  "Start timer to clear overlay."
  (when ts-hopper-visible-timer
    (cancel-timer ts-hopper-visible-timer))
  (setq ts-hopper-visible-timer
        (run-with-timer ts-hopper--highlight-timeout nil
                        (lambda ()
                          (when ts-hopper--overlay
                            (delete-overlay ts-hopper--overlay))))))

(defun ts-hopper--cancel-timer-cancelation ()
  "Cancel timer to clear overlay."
  (when ts-hopper-visible-timer
    (cancel-timer ts-hopper-visible-timer)
    (setq ts-hopper-visible-timer nil)))


(defun ts-hopper--hop (&optional node)
  "Just hope to treesit NODE.
This function is called hook `ts-hope--after-hoped'."
  ;; (message "hop node: %s" (treesit-node-type node))
  (if (not node)
      (message "No next node")
    (setq-local ts-hopper--last-hope-node node)
    (goto-char (treesit-node-start node))
    (ts-hopper--start-clear-overlay-timer)
    (ts-hopper-highlight-node node)))


;;;###autoload
(defun ts-hopper-hope ()
  "Hop to the next node."
  (interactive))

(defun ts-hopper-highlight-node (&optional node)
  "Highlight NODE."
  (interactive)
  (when ts-hopper--overlay
    (delete-overlay ts-hopper--overlay))

  (when-let ((highlighted-node  (or node ts-hopper--last-hope-node)))
    (setq-local ts-hopper--overlay (make-overlay (treesit-node-start highlighted-node)
                                                 (treesit-node-end highlighted-node)))
    (overlay-put ts-hopper--overlay 'face ts-hopper--highlight-face)))

;;;###autoload
(defun ts-hopper-init ()
  "Initialize ts-hopper."
  (interactive)
  (dolist (item ts-hopper-mode-hops-rules)
    (dolist (rule (symbol-value (cdr item)))
      (let ((rule-name (car rule)))
        ;; (message "rule: %s|%s" rule-name (type-of rule-name))
        ;; (ts-hopper--define-hop-functions rule-name)

        (defalias (intern (concat "ts-hopper-hop-to-next-" (symbol-name rule-name)))
          (lambda ()
            (interactive)
            (let ((target-node (ts-hopper--execute-by-mode rule-name 'forward)))
              (ts-hopper--hop target-node))))

        (defalias (intern (concat "ts-hopper-hop-to-prev-" (symbol-name rule-name)))
          (lambda ()
            (interactive)
            (let ((target-node (ts-hopper--execute-by-mode rule-name 'backward)))
              (ts-hopper--hop target-node))))
        ))))


(defun ts-hopper--mark-node ()
  "Mark current active node."
  (interactive)
  (push-mark (treesit-node-end ts-hopper--last-hope-node) nil t)
  (goto-char (treesit-node-start ts-hopper--last-hope-node))
  (ts-hopper-mode -1))


(defvar ts-hopper-mode-map
  (let ((map (copy-keymap global-map)))
    (set-char-table-range (nth 1 map) t 'ts-hopper-mode)
    (define-key map "c" 'ts-hopper-hop-to-next-class)
    (define-key map "C" 'ts-hopper-hop-to-prev-class)
    (define-key map "f" 'ts-hopper-hop-to-next-fn)
    (define-key map "h" 'ts-hopper-highlight-node)
    (define-key map "F" 'ts-hopper-hop-to-prev-fn)
    (define-key map "s" 'ts-hopper-hop-to-next-significant)
    (define-key map "S" 'ts-hopper-hop-to-prev-significant)
    (define-key map "B" 'ts-hopper-hop-to-prev-statement)
    (define-key map "b" 'ts-hopper-hop-to-next-statement)
    (define-key map "m" 'ts-hopper--mark-node)
    (define-key map "`" 'ts-hopper-mode)
    (define-key map (kbd "<escape>") 'ts-hopper-mode)
    map))

;;;###autoload
(define-minor-mode ts-hopper-mode
  "Minor mode for hopping to treesit nodes."
  :lighter " ts-hopper"
  :init-value nil
  :global nil
  :keymap ts-hopper-mode-map
  :group 'ts-hopper
  (if (and ts-hopper-mode (treesit-available-p) (length> (treesit-parser-list) 0))
      (progn (setq-local overriding-local-map ts-hopper-mode-map)
             (ts-hopper-init))
    (kill-local-variable 'overriding-local-map)
    (when ts-hopper--overlay
      (delete-overlay ts-hopper--overlay))))


(provide 'ts-hopper)
;;; ts-hopper.el ends here
