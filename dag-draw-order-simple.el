;;; dag-draw-order-simple.el --- Simple vertex ordering for dag-draw -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Simplified implementation of vertex ordering within ranks.
;; This provides basic ordering functionality without the full complexity
;; of the weighted median heuristic.

;;; Code:

(require 'dash)
(require 'ht)
(require 'dag-draw)
(require 'dag-draw-core)

(defun dag-draw-order-vertices (graph)
  "Order vertices within ranks using a simplified approach.
This is the second pass of the GKNV algorithm."
  
  ;; Simple approach: assign order based on alphabetical/id ordering for now
  ;; This can be enhanced later with the full weighted median heuristic
  (let ((rank-to-nodes (ht-create)))
    
    ;; Group nodes by rank
    (ht-each (lambda (node-id node)
               (let ((rank (or (dag-draw-node-rank node) 0)))
                 (ht-set! rank-to-nodes rank
                          (cons node-id (ht-get rank-to-nodes rank '())))))
             (dag-draw-graph-nodes graph))
    
    ;; Assign order within each rank
    (ht-each (lambda (rank node-list)
               (let ((sorted-nodes (sort node-list (lambda (a b) 
                                                     (string< (symbol-name a) 
                                                             (symbol-name b)))))
                     (order 0))
                 (dolist (node-id sorted-nodes)
                   (let ((node (dag-draw-get-node graph node-id)))
                     (when node
                       (setf (dag-draw-node-order node) order)
                       (setq order (1+ order)))))))
             rank-to-nodes))
  
  graph)

(provide 'dag-draw-order-simple)

;;; dag-draw-order-simple.el ends here