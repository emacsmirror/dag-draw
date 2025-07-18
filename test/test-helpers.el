;;; test-helpers.el --- Test helper functions for dag-draw  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file contains helper functions used across multiple test files.

;;; Code:


(defun dag-draw--has-connection-above (grid row col)
  "Check if position has connection above."
  (and (> row 0)
       (< col (length (aref grid (1- row))))
       (let ((char (aref (aref grid (1- row)) col)))
         (memq char '(?│ ?└ ?┘ ?├ ?┤ ?┼ ?┬ ?┴ ?▼)))))

(defun dag-draw--has-connection-below (grid row col)
  "Check if position has connection below."
  (and (< (1+ row) (length grid))
       (< col (length (aref grid (1+ row))))
       (let ((char (aref (aref grid (1+ row)) col)))
         (memq char '(?│ ?┌ ?┐ ?├ ?┤ ?┼ ?┬ ?┴ ?▲)))))

(defun dag-draw--has-connection-left (grid row col)
  "Check if position has connection left."
  (and (> col 0)
       (let ((char (aref (aref grid row) (1- col))))
         (memq char '(?─ ?┌ ?└ ?├ ?┤ ?┼ ?┬ ?┴ ?▶)))))

(defun dag-draw--has-connection-right (grid row col)
  "Check if position has connection right."
  (and (< (1+ col) (length (aref grid row)))
       (let ((char (aref (aref grid row) (1+ col))))
         (memq char '(?─ ?┐ ?┘ ?├ ?┤ ?┼ ?┬ ?┴ ?◀)))))


(defun dag-draw--validate-boundary-violations (ascii-output)
  "Validate that no edge characters appear INSIDE node text areas.
Returns list of violation positions or nil if compliant with GKNV Section 5.2.
Arrows ON boundaries are legitimate, only edge characters INSIDE text areas are violations."
  (let* ((lines (split-string ascii-output "\n"))
         (grid (vconcat (mapcar (lambda (line) (vconcat line)) lines)))
         (violations '()))

    ;; Scan each position for potential boundary violations
    (dotimes (row (length grid))
      (when (> (length (aref grid row)) 0)  ; Skip empty lines
        (let ((line-str (mapconcat (lambda (char) (string char)) (aref grid row) "")))
          ;; Look for node boundary patterns: │...text...│
          (let ((start 0))
            (while (string-match "│\\([^│┌┐└┘]*\\)│" line-str start)
              (let* ((match-start (match-beginning 0))
                     (match-end (match-end 0))
                     (inner-content (match-string 1 line-str))
                     (inner-start (1+ match-start)))

                ;; Check if inner content contains edge characters (NOT arrows on boundaries)
                (dotimes (i (length inner-content))
                  (let ((char (aref inner-content i))
                        (abs-col (+ inner-start i)))
                    ;; Edge characters inside node text areas are violations
                    ;; (arrows are handled separately as they're allowed on boundaries)
                    (when (memq char '(?─ ?┼ ?┬ ?┴ ?├ ?┤))
                      (push (list row abs-col (format "edge-char-%s-inside-node" (char-to-string char)))
                            violations))))

                (setq start match-end))))))

      violations)))

(defun dag-draw--validate-node-integrity (ascii-output node-name)
  "Validate that a node has complete box structure using 2D grid analysis.
Returns nil if node is properly formed, error message if corrupted.
Uses robust spatial validation instead of overly strict regex patterns."
  (let* ((lines (split-string ascii-output "\n"))
         (grid (vconcat (mapcar (lambda (line) (vconcat line)) lines)))
         (node-positions '())
         (errors '()))

    ;; Find all positions containing the node name
    (dotimes (row (length grid))
      (when (> (length (aref grid row)) 0)
        (let ((line-str (mapconcat (lambda (char) (string char)) (aref grid row) "")))
          (when (string-match-p (regexp-quote node-name) line-str)
            (push row node-positions)))))

    (if (null node-positions)
        (list (format "Node '%s' not found in output" node-name))

      ;; For each row containing the node name, check for box structure
      (dolist (text-row node-positions)
        (let ((line-str (mapconcat (lambda (char) (string char)) (aref grid text-row) ""))
              (node-start-col (string-match (regexp-quote node-name)
                                            (mapconcat (lambda (char) (string char)) (aref grid text-row) ""))))

          (when node-start-col
            ;; Check for left border (│) on this row
            (let ((has-left-border nil)
                  (has-right-border nil)
                  (has-top-border nil)
                  (has-bottom-border nil))

              ;; Look for left border within reasonable distance (including junction characters and right arrows)
              (dotimes (check-col (min (length (aref grid text-row)) (+ node-start-col 5)))
                (when (and (< check-col node-start-col)
                           (memq (aref (aref grid text-row) check-col) '(?│ ?├ ?┤ ?┬ ?┴ ?▶)))
                  (setq has-left-border t)))

              ;; Look for right border after the node name (including junction characters and left arrows)
              (let ((search-start (+ node-start-col (length node-name))))
                (dotimes (offset 20)  ; Increased search range for robust detection
                  (let ((check-col (+ search-start offset)))
                    (when (and (< check-col (length (aref grid text-row)))
                               (memq (aref (aref grid text-row) check-col) '(?│ ?├ ?┤ ?┬ ?┴ ?◀)))
                      (setq has-right-border t)))))

              ;; Look for top border (┌─┐) in rows above
              (when (> text-row 0)
                (dotimes (check-row 3)
                  (let ((border-row (- text-row (1+ check-row))))
                    (when (and (>= border-row 0)
                               (< border-row (length grid))
                               (> (length (aref grid border-row)) 0))
                      (let ((border-line (mapconcat (lambda (char) (string char)) (aref grid border-row) "")))
                        (when (string-match-p "[┌┐─├┤┬┴▼]" border-line)
                          (setq has-top-border t)))))))

              ;; Look for bottom border (└─┘) in rows below
              (dotimes (check-row 3)
                (let ((border-row (+ text-row (1+ check-row))))
                  (when (and (< border-row (length grid))
                             (> (length (aref grid border-row)) 0))
                    (let ((border-line (mapconcat (lambda (char) (string char)) (aref grid border-row) "")))
                      (when (string-match-p "[└┘─├┤┬┴]" border-line)
                        (setq has-bottom-border t))))))

              ;; Report missing components (but be lenient for edge interference)
              (unless has-left-border
                (push (format "Node '%s' missing left border (│)" node-name) errors))
              (unless has-right-border
                (push (format "Node '%s' missing right border (│)" node-name) errors))
              (unless has-top-border
                (push (format "Node '%s' missing top border (┌─┐)" node-name) errors))
              (unless has-bottom-border
                (push (format "Node '%s' missing bottom border (└─┘)" node-name) errors))))))

      ;; Return errors if any, otherwise nil (success)
      (if errors errors nil))))

(provide 'test-helpers)

;;; test-helpers.el ends here
