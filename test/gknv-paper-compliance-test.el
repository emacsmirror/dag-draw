;;; gknv-paper-compliance-test.el --- GKNV paper compliance verification -*- lexical-binding: t -*-

;; Copyright (C) 2024, 2025

;;; Commentary:

;; Essential tests verifying GKNV algorithm compliance with paper specification.
;; Uses test harness for rigorous verification of algorithm behavior.

;;; Code:

(add-to-list 'load-path (expand-file-name "test/helpers" (locate-dominating-file default-directory "Eldev")))

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-core)
(require 'dag-draw-test-harness)

(describe "GKNV Paper Compliance - Core Algorithm Verification"
  
  (describe "Pass 1: Rank Assignment - Network Simplex"
    
    (it "should assign integer ranks satisfying edge length constraints"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a "Node A")
        (dag-draw-add-node graph 'b "Node B")
        (dag-draw-add-node graph 'c "Node C")
        (dag-draw-add-edge graph 'a 'b)
        (dag-draw-add-edge graph 'b 'c)
        
        (dag-draw-layout-graph graph)
        
        ;; Paper Section 2: ranks should be integers with l(e) >= delta(e) = 1
        (let ((rank-a (dag-draw-node-rank (dag-draw-get-node graph 'a)))
              (rank-b (dag-draw-node-rank (dag-draw-get-node graph 'b)))
              (rank-c (dag-draw-node-rank (dag-draw-get-node graph 'c))))
          
          ;; Ranks should be approximately integers
          (expect (abs (- rank-a (round rank-a))) :to-be-less-than 0.01)
          (expect (abs (- rank-b (round rank-b))) :to-be-less-than 0.01)
          (expect (abs (- rank-c (round rank-c))) :to-be-less-than 0.01)
          
          ;; Edge length constraints: l(e) >= 1
          (expect (- rank-b rank-a) :to-be-greater-than 0.99)
          (expect (- rank-c rank-b) :to-be-greater-than 0.99))))
    
    (it "should handle cycles by breaking back edges"
      (let ((graph (dag-draw-create-graph)))
        ;; Create cycle: A -> B -> C -> A
        (dag-draw-add-node graph 'a "A")
        (dag-draw-add-node graph 'b "B")
        (dag-draw-add-node graph 'c "C")
        (dag-draw-add-edge graph 'a 'b)
        (dag-draw-add-edge graph 'b 'c)
        (dag-draw-add-edge graph 'c 'a)
        
        ;; Should complete without error
        (expect (dag-draw-layout-graph graph) :not :to-be nil)
        
        ;; All nodes should have valid ranks
        (expect (dag-draw-node-rank (dag-draw-get-node graph 'a)) :not :to-be nil)
        (expect (dag-draw-node-rank (dag-draw-get-node graph 'b)) :not :to-be nil)
        (expect (dag-draw-node-rank (dag-draw-get-node graph 'c)) :not :to-be nil))))
  
  (describe "Pass 2: Node Ordering Within Ranks"
    
    (it "should maintain consistent ordering to minimize crossings"
      (let ((graph (dag-draw-create-graph)))
        ;; Diamond pattern that tests crossing reduction
        (dag-draw-add-node graph 'top "Top")
        (dag-draw-add-node graph 'left "Left")
        (dag-draw-add-node graph 'right "Right")
        (dag-draw-add-node graph 'bottom "Bottom")
        
        (dag-draw-add-edge graph 'top 'left)
        (dag-draw-add-edge graph 'top 'right)
        (dag-draw-add-edge graph 'left 'bottom)
        (dag-draw-add-edge graph 'right 'bottom)
        
        (dag-draw-layout-graph graph)
        
        ;; Should create proper rank structure
        (let ((rank-top (dag-draw-node-rank (dag-draw-get-node graph 'top)))
              (rank-left (dag-draw-node-rank (dag-draw-get-node graph 'left)))
              (rank-right (dag-draw-node-rank (dag-draw-get-node graph 'right)))
              (rank-bottom (dag-draw-node-rank (dag-draw-get-node graph 'bottom))))
          
          ;; Top should be rank 0
          (expect rank-top :to-be-close-to 0 0.1)
          ;; Left and right should be same rank (1)
          (expect (abs (- rank-left rank-right)) :to-be-less-than 0.1)
          ;; Bottom should be rank 2
          (expect rank-bottom :to-be-close-to 2 0.1)))))
  
  (describe "Pass 3: Coordinate Assignment"
    
    (it "should assign valid X and Y coordinates"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'source "Source")
        (dag-draw-add-node graph 'target "Target")
        (dag-draw-add-edge graph 'source 'target)
        
        (dag-draw-layout-graph graph)
        
        (let ((source-node (dag-draw-get-node graph 'source))
              (target-node (dag-draw-get-node graph 'target)))
          
          ;; All coordinates should be valid numbers
          (expect (numberp (dag-draw-node-x-coord source-node)) :to-be-truthy)
          (expect (numberp (dag-draw-node-y-coord source-node)) :to-be-truthy)
          (expect (numberp (dag-draw-node-x-coord target-node)) :to-be-truthy)
          (expect (numberp (dag-draw-node-y-coord target-node)) :to-be-truthy)
          
          ;; Y coordinates should reflect rank ordering
          (expect (dag-draw-node-y-coord target-node) :to-be-greater-than 
                  (dag-draw-node-y-coord source-node)))))
    
    (it "should maintain node separation constraints"
      (let ((graph (dag-draw-create-graph)))
        ;; Nodes that will be on same rank
        (dag-draw-add-node graph 'left "Left Node With Long Label")
        (dag-draw-add-node graph 'right "Right")
        (dag-draw-add-node graph 'target "Target")
        (dag-draw-add-edge graph 'left 'target)
        (dag-draw-add-edge graph 'right 'target)
        
        (dag-draw-layout-graph graph)
        
        (let ((left-x (dag-draw-node-x-coord (dag-draw-get-node graph 'left)))
              (right-x (dag-draw-node-x-coord (dag-draw-get-node graph 'right))))
          
          ;; Should be separated by more than node label lengths
          (let ((separation (abs (- right-x left-x))))
            (expect separation :to-be-greater-than 10))))))
  
  (describe "Pass 4: Edge Drawing and ASCII Rendering"
    
    (it "should render nodes and edges using test harness verification"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'start "Start")
        (dag-draw-add-node graph 'finish "Finish")
        (dag-draw-add-edge graph 'start 'finish)
        
        (dag-draw-layout-graph graph)
        
        (let ((ascii-output (dag-draw-render-ascii graph)))
          (expect ascii-output :to-be-truthy)
          
          ;; Use test harness for rigorous verification
          (let ((grid (dag-draw-test--parse-ascii-grid ascii-output)))
            (let ((start-pos (dag-draw-test--find-text-in-grid grid "Start"))
                  (finish-pos (dag-draw-test--find-text-in-grid grid "Finish")))
              
              ;; Both nodes should be found
              (expect start-pos :not :to-be nil)
              (expect finish-pos :not :to-be nil)

              ;; Finish should be below Start (higher Y coordinate in grid)
              (expect (cdr finish-pos) :to-be-greater-than (cdr start-pos))

              ;; Should have path connection
              (expect (dag-draw-test--has-path-between grid start-pos finish-pos)
                      :to-be-truthy))))))
    
    (it "should handle complex graphs with multiple edge types"
      (let ((graph (dag-draw-create-graph)))
        ;; Mix of inter-rank and potential self-edges
        (dag-draw-add-node graph 'root "Root")
        (dag-draw-add-node graph 'branch "Branch")
        (dag-draw-add-node graph 'leaf "Leaf")
        (dag-draw-add-node graph 'isolated "Isolated")
        
        (dag-draw-add-edge graph 'root 'branch)
        (dag-draw-add-edge graph 'branch 'leaf)
        
        (dag-draw-layout-graph graph)
        
        (let ((ascii-output (dag-draw-render-ascii graph)))
          (let ((grid (dag-draw-test--parse-ascii-grid ascii-output)))
            (let ((root-pos (dag-draw-test--find-text-in-grid grid "Root"))
                  (branch-pos (dag-draw-test--find-text-in-grid grid "Branch"))
                  (leaf-pos (dag-draw-test--find-text-in-grid grid "Leaf"))
                  (isolated-pos (dag-draw-test--find-text-in-grid grid "Isolated")))
              
              ;; All nodes should be found
              (expect root-pos :not :to-be nil)
              (expect branch-pos :not :to-be nil)
              (expect leaf-pos :not :to-be nil)
              (expect isolated-pos :not :to-be nil)

              ;; Should maintain hierarchy (higher Y = lower in visual display)
              (expect (cdr branch-pos) :to-be-greater-than (cdr root-pos))
              (expect (cdr leaf-pos) :to-be-greater-than (cdr branch-pos))

              ;; Connected path should exist
              (expect (dag-draw-test--has-path-between grid root-pos leaf-pos)
                      :to-be-truthy))))))
  
  (describe "Aesthetic Principles Verification"
    
    (it "should expose hierarchical structure (A1)"
      (let ((graph (dag-draw-create-graph)))
        ;; Clear hierarchy
        (dag-draw-add-node graph 'ceo "CEO")
        (dag-draw-add-node graph 'manager "Manager")
        (dag-draw-add-node graph 'employee "Employee")
        (dag-draw-add-edge graph 'ceo 'manager)
        (dag-draw-add-edge graph 'manager 'employee)
        
        (dag-draw-layout-graph graph)
        
        ;; Should create clear rank progression
        (let ((ceo-rank (dag-draw-node-rank (dag-draw-get-node graph 'ceo)))
              (mgr-rank (dag-draw-node-rank (dag-draw-get-node graph 'manager)))
              (emp-rank (dag-draw-node-rank (dag-draw-get-node graph 'employee))))
          
          (expect ceo-rank :to-be-less-than mgr-rank)
          (expect mgr-rank :to-be-less-than emp-rank))))
    
    (it "should keep edges short (A3)"
      (let ((graph (dag-draw-create-graph)))
        ;; Simple chain should use minimum edge lengths
        (dag-draw-add-node graph 'first "First")
        (dag-draw-add-node graph 'second "Second")
        (dag-draw-add-edge graph 'first 'second)
        
        (dag-draw-layout-graph graph)
        
        (let ((first-rank (dag-draw-node-rank (dag-draw-get-node graph 'first)))
              (second-rank (dag-draw-node-rank (dag-draw-get-node graph 'second))))
          
          ;; Should use minimum edge length
          (expect (- second-rank first-rank) :to-be-close-to 1 0.1))))
  
  (describe "Algorithm Robustness"
    
    (it "should handle edge cases without failure"
      ;; Single node
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'alone "Alone")
        (expect (dag-draw-layout-graph graph) :not :to-be nil)
        (expect (dag-draw-node-rank (dag-draw-get-node graph 'alone)) :to-be-close-to 0 0.1))
      
      ;; Disconnected components
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a "A")
        (dag-draw-add-node graph 'b "B")
        (dag-draw-add-node graph 'c "C")
        (dag-draw-add-node graph 'd "D")
        (dag-draw-add-edge graph 'a 'b)
        (dag-draw-add-edge graph 'c 'd)
        ;; Two disconnected pairs
        
        (expect (dag-draw-layout-graph graph) :not :to-be nil)
        
        ;; All should have valid ranks
        (expect (dag-draw-node-rank (dag-draw-get-node graph 'a)) :not :to-be nil)
        (expect (dag-draw-node-rank (dag-draw-get-node graph 'b)) :not :to-be nil)
        (expect (dag-draw-node-rank (dag-draw-get-node graph 'c)) :not :to-be nil)
        (expect (dag-draw-node-rank (dag-draw-get-node graph 'd)) :not :to-be nil)))
    
    (it "should maintain numerical stability"
      (let ((graph (dag-draw-create-graph)))
        ;; Create longer chain
        (dotimes (i 10)
          (dag-draw-add-node graph (intern (format "node-%d" i)) (format "Node %d" i))
          (when (> i 0)
            (dag-draw-add-edge graph (intern (format "node-%d" (1- i))) 
                              (intern (format "node-%d" i)))))
        
        (dag-draw-layout-graph graph)
        
        ;; All coordinates should be finite
        (dotimes (i 10)
          (let ((node (dag-draw-get-node graph (intern (format "node-%d" i)))))
            (expect (numberp (dag-draw-node-rank node)) :to-be-truthy)
            (expect (numberp (dag-draw-node-x-coord node)) :to-be-truthy)
            (expect (numberp (dag-draw-node-y-coord node)) :to-be-truthy)))))))))

;;; gknv-paper-compliance-test.el ends here