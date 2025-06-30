;;; dag-draw-dot.el --- DOT format rendering for dag-draw -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; DOT format (Graphviz) rendering implementation for dag-draw graphs.
;; Exports positioned graphs to Graphviz DOT format for external processing.

;;; Code:

(require 'dag-draw-core)

;;; DOT Format Rendering

(defun dag-draw-render-dot (graph)
  "Render GRAPH in Graphviz DOT format."
  (let ((dot-output "digraph G {\n")
        (node-attrs "  node [shape=box, style=filled, fillcolor=lightgray];\n")
        (edge-attrs "  edge [color=black];\n"))

    ;; Add graph attributes
    (setq dot-output (concat dot-output node-attrs edge-attrs "\n"))

    ;; Add nodes
    (ht-each (lambda (node-id node)
               (let ((label (dag-draw-node-label node)))
                 (setq dot-output
                       (concat dot-output
                               (format "  %s [label=\"%s\"];\n"
                                       (symbol-name node-id)
                                       (dag-draw--escape-dot-string label))))))
             (dag-draw-graph-nodes graph))

    ;; Add edges
    (setq dot-output (concat dot-output "\n"))
    (dolist (edge (dag-draw-graph-edges graph))
      (let ((from (symbol-name (dag-draw-edge-from-node edge)))
            (to (symbol-name (dag-draw-edge-to-node edge)))
            (label (dag-draw-edge-label edge)))
        (setq dot-output
              (concat dot-output
                      (format "  %s -> %s" from to)
                      (if label
                          (format " [label=\"%s\"]" (dag-draw--escape-dot-string label))
                        "")
                      ";\n"))))

    (concat dot-output "}\n")))

(defun dag-draw--escape-dot-string (text)
  "Escape special characters for DOT format."
  ;; Escape backslashes first (each \ becomes \\\\), then quotes (each " becomes \\")
  (replace-regexp-in-string
   "\"" "\\\\\\\\\""
   (replace-regexp-in-string "\\\\" "\\\\\\\\\\\\\\\\" text)))

(provide 'dag-draw-dot)

;;; dag-draw-dot.el ends here