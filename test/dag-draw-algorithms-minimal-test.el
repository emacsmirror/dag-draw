;;; dag-draw-algorithms-minimal-test.el --- Minimal tests for dag-draw-algorithms.el -*- lexical-binding: t -*-

;; Copyright (C) 2024, 2025

;;; Commentary:

;; Minimal tests for graph algorithms to debug issues.

;;; Code:

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-core)
(require 'dag-draw-algorithms)

(describe "dag-draw-algorithms minimal tests"
  
  (describe "basic DFS"
    (it "should perform DFS on simple graph"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a)
        (dag-draw-add-node graph 'b)
        (dag-draw-add-edge graph 'a 'b)
        
        (let ((result (dag-draw-dfs graph)))
          (expect result :to-be-truthy)))))

)

;;; dag-draw-algorithms-minimal-test.el ends here