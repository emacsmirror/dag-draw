;;; dag-draw-rank-hierarchy-test.el --- Tests for proper GKNV rank assignment -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; TDD tests for fixing the fundamentally broken rank assignment.
;; The current network simplex fails to converge, producing scattered 
;; nodes instead of proper hierarchical DAG structure.

;;; Code:

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-core)
(require 'dag-draw-rank)

(describe "GKNV Rank Assignment for Hierarchical DAG Structure"
  
  (it "should assign proper hierarchical ranks to complex dependency graph"
    (let ((graph (dag-draw-create-graph)))
      ;; Create the exact graph from the failing test
      (dag-draw-add-node graph 'research "Research")
      (dag-draw-add-node graph 'database "Database Design")
      (dag-draw-add-node graph 'api "API Design")
      (dag-draw-add-node graph 'infrastructure "Infrastructure Setup")
      (dag-draw-add-node graph 'backend "Backend Implementation")
      (dag-draw-add-node graph 'frontend "Frontend Implementation")
      (dag-draw-add-node graph 'integration "Integration Testing")
      (dag-draw-add-node graph 'deployment "Deployment")
      
      ;; Add dependency relationships
      (dag-draw-add-edge graph 'research 'database)
      (dag-draw-add-edge graph 'research 'api)
      (dag-draw-add-edge graph 'research 'infrastructure)
      (dag-draw-add-edge graph 'database 'backend)
      (dag-draw-add-edge graph 'api 'backend)
      (dag-draw-add-edge graph 'api 'frontend)
      (dag-draw-add-edge graph 'backend 'integration)
      (dag-draw-add-edge graph 'frontend 'integration)
      (dag-draw-add-edge graph 'integration 'deployment)
      
      ;; Run rank assignment (Pass 1 of GKNV)
      (dag-draw-rank-graph graph)
      
      ;; CRITICAL: Verify proper hierarchical structure
      (let ((research-rank (dag-draw-node-rank (dag-draw-get-node graph 'research)))
            (database-rank (dag-draw-node-rank (dag-draw-get-node graph 'database)))
            (api-rank (dag-draw-node-rank (dag-draw-get-node graph 'api)))
            (infrastructure-rank (dag-draw-node-rank (dag-draw-get-node graph 'infrastructure)))
            (backend-rank (dag-draw-node-rank (dag-draw-get-node graph 'backend)))
            (frontend-rank (dag-draw-node-rank (dag-draw-get-node graph 'frontend)))
            (integration-rank (dag-draw-node-rank (dag-draw-get-node graph 'integration)))
            (deployment-rank (dag-draw-node-rank (dag-draw-get-node graph 'deployment))))
        
        ;; Debug output first
        (message "Rank assignments:")
        (message "  Research: %s" research-rank)
        (message "  Database/API/Infrastructure: %s/%s/%s" database-rank api-rank infrastructure-rank)
        (message "  Backend/Frontend: %s/%s" backend-rank frontend-rank)
        (message "  Integration: %s" integration-rank)
        (message "  Deployment: %s" deployment-rank)
        
        ;; Research should be root (rank 0)
        (expect research-rank :to-equal 0)
        
        ;; First level dependencies should be rank 1
        (expect database-rank :to-equal 1)
        (expect api-rank :to-equal 1)
        (expect infrastructure-rank :to-equal 1)
        
        ;; Second level should be rank 2
        (expect backend-rank :to-equal 2)
        (expect frontend-rank :to-equal 2)
        
        ;; Integration should be rank 3
        (expect integration-rank :to-equal 3)
        
        ;; Deployment should be highest rank (4)
        (expect deployment-rank :to-equal 4)
        
        ;; Debug: Show all edges to understand dependencies
        (message "Edges in graph:")
        (dolist (edge (dag-draw-graph-edges graph))
          (message "  %s -> %s" (dag-draw-edge-from-node edge) (dag-draw-edge-to-node edge))))))
  
  (it "should have network simplex converge for simple dependency chain"
    (let ((graph (dag-draw-create-graph)))
      ;; Simple chain: A -> B -> C
      (dag-draw-add-node graph 'a "A")
      (dag-draw-add-node graph 'b "B")
      (dag-draw-add-node graph 'c "C")
      (dag-draw-add-edge graph 'a 'b)
      (dag-draw-add-edge graph 'b 'c)
      
      ;; Test network simplex convergence directly
      (dag-draw-assign-ranks graph)
      
      ;; Should have proper ranks
      (expect (dag-draw-node-rank (dag-draw-get-node graph 'a)) :to-equal 0)
      (expect (dag-draw-node-rank (dag-draw-get-node graph 'b)) :to-equal 1)
      (expect (dag-draw-node-rank (dag-draw-get-node graph 'c)) :to-equal 2)))
  
  (it "should preserve all edge direction constraints in rank assignment"
    (let ((graph (dag-draw-create-graph)))
      ;; Diamond pattern: A -> {B,C} -> D
      (dag-draw-add-node graph 'a "A")
      (dag-draw-add-node graph 'b "B")
      (dag-draw-add-node graph 'c "C")
      (dag-draw-add-node graph 'd "D")
      (dag-draw-add-edge graph 'a 'b)
      (dag-draw-add-edge graph 'a 'c)
      (dag-draw-add-edge graph 'b 'd)
      (dag-draw-add-edge graph 'c 'd)
      
      (dag-draw-rank-graph graph)
      
      ;; Verify all edge constraints preserved
      (dolist (edge (dag-draw-graph-edges graph))
        (let* ((from-node (dag-draw-get-node graph (dag-draw-edge-from-node edge)))
               (to-node (dag-draw-get-node graph (dag-draw-edge-to-node edge)))
               (from-rank (dag-draw-node-rank from-node))
               (to-rank (dag-draw-node-rank to-node)))
          ;; All edges must go from lower rank to higher rank
          (expect from-rank :to-be-less-than to-rank))))))

(provide 'dag-draw-rank-hierarchy-test)

;;; dag-draw-rank-hierarchy-test.el ends here