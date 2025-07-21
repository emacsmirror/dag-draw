;;; dag-draw-dfs-edge-classification-test.el --- Tests for GKNV DFS edge classification -*- lexical-binding: t -*-

;; Copyright (C) 2024

;;; Commentary:

;; Tests to ensure proper GKNV DFS edge classification per Section 2.1.
;; Validates proper tree, forward, cross, and back edge identification.
;;
;; GKNV Reference: Section 2.1, lines 374-386  
;; Ubiquitous Language: DFS Edge Classification - proper GKNV taxonomy

;;; Code:

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-core)
(require 'dag-draw-cycle-breaking)

(describe "GKNV DFS Edge Classification"
  
  (describe "proper edge taxonomy per Section 2.1"
    
    (it "should classify tree edges correctly"
      ;; RED TEST: Current implementation doesn't have proper edge classification
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a "A")
        (dag-draw-add-node graph 'b "B")
        (dag-draw-add-node graph 'c "C")
        
        ;; Create tree structure: a -> b -> c
        (dag-draw-add-edge graph 'a 'b)
        (dag-draw-add-edge graph 'b 'c)
        
        (let ((classification (dag-draw--classify-edges-gknv graph)))
          ;; Should identify tree edges per GKNV Section 2.1
          (let ((tree-edges (ht-get classification 'tree-edges)))
            (expect tree-edges :not :to-be nil)
            (expect (length tree-edges) :to-equal 2)
            ;; Tree edges should form DFS tree structure
            (expect (cl-some (lambda (e) 
                               (and (eq (dag-draw-edge-from-node e) 'a)
                                    (eq (dag-draw-edge-to-node e) 'b))) 
                             tree-edges) :to-be t)))))
    
    (it "should classify forward edges correctly" 
      ;; RED TEST: Should identify forward edges (ancestor -> descendant, non-tree)
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'root "Root")
        (dag-draw-add-node graph 'child "Child") 
        (dag-draw-add-node graph 'grandchild "Grandchild")
        
        ;; Tree edges: root -> child -> grandchild
        (dag-draw-add-edge graph 'root 'child)
        (dag-draw-add-edge graph 'child 'grandchild)
        
        ;; Forward edge: root -> grandchild (ancestor to descendant)
        (dag-draw-add-edge graph 'root 'grandchild)
        
        (let ((classification (dag-draw--classify-edges-gknv graph)))
          ;; Should identify forward edge per GKNV Section 2.1, line 380
          (let ((forward-edges (ht-get classification 'forward-edges)))
            (expect forward-edges :not :to-be nil)
            (expect (cl-some (lambda (e)
                               (and (eq (dag-draw-edge-from-node e) 'root)
                                    (eq (dag-draw-edge-to-node e) 'grandchild)))
                             forward-edges) :to-be t)))))
    
    (it "should classify back edges correctly"
      ;; RED TEST: Should identify back edges (descendant -> ancestor, causes cycles)
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a "A")
        (dag-draw-add-node graph 'b "B")
        (dag-draw-add-node graph 'c "C")
        
        ;; Tree structure with cycle
        (dag-draw-add-edge graph 'a 'b)
        (dag-draw-add-edge graph 'b 'c) 
        (dag-draw-add-edge graph 'c 'a)  ; Back edge creating cycle
        
        (let ((classification (dag-draw--classify-edges-gknv graph)))
          ;; Should identify back edge per GKNV Section 2.1, line 381  
          (let ((back-edges (ht-get classification 'back-edges)))
            (expect back-edges :not :to-be nil)
            ;; Back edge should be the one creating the cycle
            (expect (length back-edges) :to-be-greater-than 0)))))
            
    (it "should classify cross edges correctly"
      ;; GREEN TEST: Should identify cross edges (unrelated nodes)  
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'root "Root")
        (dag-draw-add-node graph 'left "Left")
        (dag-draw-add-node graph 'right "Right") 
        (dag-draw-add-node graph 'bottom "Bottom")
        
        ;; Tree structure: root -> left, root -> right -> bottom
        (dag-draw-add-edge graph 'root 'left)
        (dag-draw-add-edge graph 'root 'right)
        (dag-draw-add-edge graph 'right 'bottom)
        
        ;; Cross edge: left -> bottom (unrelated in tree)
        (dag-draw-add-edge graph 'left 'bottom)
        
        (let ((classification (dag-draw--classify-edges-gknv graph)))
          ;; Should identify cross edge per GKNV Section 2.1, line 379
          (let ((cross-edges (ht-get classification 'cross-edges)))
            (expect cross-edges :not :to-be nil)
            (expect (cl-some (lambda (e)
                               (and (eq (dag-draw-edge-from-node e) 'left) 
                                    (eq (dag-draw-edge-to-node e) 'bottom)))
                             cross-edges) :to-be t)))))
    
    (it "should integrate with cycle breaking per GKNV Section 2.1"
      ;; GREEN TEST: Back edges should be reversed to break cycles
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'x "X")
        (dag-draw-add-node graph 'y "Y")
        
        ;; Create cycle: x -> y -> x
        (dag-draw-add-edge graph 'x 'y)
        (dag-draw-add-edge graph 'y 'x)
        
        ;; Should detect and break cycles using GKNV classification
        (expect (dag-draw-has-cycles graph) :to-be t)
        (dag-draw--break-cycles-using-gknv-classification graph)
        (expect (dag-draw-has-cycles graph) :to-be nil)))))

(provide 'dag-draw-dfs-edge-classification-test)

;;; dag-draw-dfs-edge-classification-test.el ends here