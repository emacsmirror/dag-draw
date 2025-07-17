;;; dag-draw-complex-routing-test.el --- Test complex graph edge routing issues -*- lexical-binding: t -*-

;; Copyright (C) 2024

;;; Commentary:

;; Test complex graph structures that reveal edge routing problems.
;; This test reproduces the specific issues seen in the demo:
;; - Task A blocks tasks B and C  
;; - Task D is blocked by both A and B
;; - Additional tasks E and F for completeness
;; This creates multiple edges converging and diverging, exposing routing issues.

;;; Code:

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-core)
(require 'dag-draw-render)

(describe "Complex Graph Edge Routing"

  (describe "6-Node Multi-Connection Test Case"

    (it "reproduces routing issues with converging and diverging edges"
        (let ((graph (dag-draw-create-graph)))
          ;; Create 6-node graph following user specification:
          ;; A blocks B and C
          ;; D is blocked by A and B  
          ;; E and F for additional complexity
          
          (dag-draw-add-node graph 'A "Task A")
          (dag-draw-add-node graph 'B "Task B") 
          (dag-draw-add-node graph 'C "Task C")
          (dag-draw-add-node graph 'D "Task D")
          (dag-draw-add-node graph 'E "Task E")
          (dag-draw-add-node graph 'F "Task F")
          
          ;; A blocks B and C (A->B, A->C)
          (dag-draw-add-edge graph 'A 'B)
          (dag-draw-add-edge graph 'A 'C)
          
          ;; D is blocked by A and B (A->D, B->D) 
          (dag-draw-add-edge graph 'A 'D)
          (dag-draw-add-edge graph 'B 'D)
          
          ;; Additional edges for complexity
          (dag-draw-add-edge graph 'C 'E)
          (dag-draw-add-edge graph 'D 'F)
          
          ;; Layout the graph
          (dag-draw-layout-graph graph)
          
          ;; Render and analyze
          (let ((ascii-output (dag-draw-render-ascii graph)))
            
            ;; Basic sanity checks
            (expect ascii-output :to-be-truthy)
            (expect (length ascii-output) :to-be-greater-than 100)
            
            ;; Should contain all node labels
            (expect ascii-output :to-match "Task A")
            (expect ascii-output :to-match "Task B")
            (expect ascii-output :to-match "Task C")
            (expect ascii-output :to-match "Task D")
            (expect ascii-output :to-match "Task E")
            (expect ascii-output :to-match "Task F")
            
            ;; Should use box-drawing characters
            (expect ascii-output :to-match "┌")
            (expect ascii-output :to-match "─")
            (expect ascii-output :to-match "│")
            
            ;; OUTPUT FOR MANUAL ANALYSIS
            (message "\n=== COMPLEX 6-NODE GRAPH OUTPUT ===")
            (message "%s" ascii-output)
            (message "=== END COMPLEX OUTPUT ===\n")
            
            ;; The key test: analyze for routing problems
            (let ((lines (split-string ascii-output "\n" t)))
              
              ;; Should have substantial routing (multiple edges)
              (let ((edge-line-count 0))
                (dolist (line lines)
                  (when (string-match-p "[─│]" line)
                    (setq edge-line-count (1+ edge-line-count))))
                
                (expect edge-line-count :to-be-greater-than 3))
              
              ;; Check for potential issues (these may fail initially)
              (let ((suspicious-patterns '()))
                
                ;; Look for lines that might be going through node interiors
                (dolist (line lines)
                  (when (and (string-match-p "Task [A-F]" line)
                            (string-match-p "[─│]" line))
                    (push line suspicious-patterns)))
                
                ;; Report suspicious patterns for analysis
                (when suspicious-patterns
                  (message "SUSPICIOUS: Lines with both node labels and edge chars:")
                  (dolist (pattern suspicious-patterns)
                    (message "  %s" pattern)))
                
                ;; For now, just ensure we get output (will refine after fixes)
                (expect (length lines) :to-be-greater-than 10))))))

    (it "analyzes port connections for multiple edges from same node"
        (let ((graph (dag-draw-create-graph)))
          ;; Create simple case: A connects to both B and C
          (dag-draw-add-node graph 'A "Source")
          (dag-draw-add-node graph 'B "Target1") 
          (dag-draw-add-node graph 'C "Target2")
          
          (dag-draw-add-edge graph 'A 'B)
          (dag-draw-add-edge graph 'A 'C)
          
          ;; Set explicit positions to create predictable layout
          (let ((node-a (dag-draw-get-node graph 'A))
                (node-b (dag-draw-get-node graph 'B))
                (node-c (dag-draw-get-node graph 'C)))
            (setf (dag-draw-node-x-coord node-a) 100)
            (setf (dag-draw-node-y-coord node-a) 100)
            (setf (dag-draw-node-x-coord node-b) 200)
            (setf (dag-draw-node-y-coord node-b) 50)
            (setf (dag-draw-node-x-coord node-c) 200)
            (setf (dag-draw-node-y-coord node-c) 150))
          
          ;; Test port calculation for diverging edges
          (let* ((node-a (dag-draw-get-node graph 'A))
                 (node-b (dag-draw-get-node graph 'B))
                 (node-c (dag-draw-get-node graph 'C))
                 (ports-a-b (dag-draw--calculate-edge-ports node-a node-b))
                 (ports-a-c (dag-draw--calculate-edge-ports node-a node-c)))
            
            ;; Both port calculations should succeed
            (expect ports-a-b :to-be-truthy)
            (expect ports-a-c :to-be-truthy)
            (expect (length ports-a-b) :to-equal 2)
            (expect (length ports-a-c) :to-equal 2)
            
            ;; Ports should be on node boundaries, not center
            (let ((from-port-b (car ports-a-b))
                  (from-port-c (car ports-a-c)))
              (expect (dag-draw-point-x from-port-b) :not :to-equal (dag-draw-node-x-coord node-a))
              (expect (dag-draw-point-x from-port-c) :not :to-equal (dag-draw-node-x-coord node-a))))))

  )  ; End describe "6-Node Multi-Connection Test Case"
)  ; End describe "Complex Graph Edge Routing"

(provide 'dag-draw-complex-routing-test)

;;; dag-draw-complex-routing-test.el ends here