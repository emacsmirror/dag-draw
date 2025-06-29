;;; dag-draw-coincident-ports-test.el --- Tests for handling coincident ports -*- lexical-binding: t -*-

;; Copyright (C) 2024

;;; Commentary:

;; TDD for handling the case where edge ports coincide (adjacent nodes).
;; This is mathematically correct but needs visual enhancement.

;;; Code:

(require 'buttercup)
(require 'dag-draw-render)

(describe "dag-draw coincident ports"
  (describe "visual separation for adjacent nodes"
    (it "should create visible edge connection even when ports coincide"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'A "Node A")
        (dag-draw-add-node graph 'B "Node B")
        (dag-draw-add-edge graph 'A 'B)
        (dag-draw-layout-graph graph)
        
        (let ((result (dag-draw-render-ascii graph)))
          ;; Should have visible edge indicators
          (expect result :to-match "[▼▲▶◀]")  ; Some directional arrow
          ;; Should not have multiple spaces around arrows (disconnected appearance)  
          (expect result :not :to-match "   [▼▲▶◀]   "))))))

(provide 'dag-draw-coincident-ports-test)

;;; dag-draw-coincident-ports-test.el ends here