;;; dag-draw-edge-label-nodes-test.el --- Tests for GKNV edge label virtual nodes -*- lexical-binding: t -*-

;; Copyright (C) 2024

;;; Commentary:

;; Tests for GKNV Section 5.3 edge label implementation.
;; "In dot, edge labels on inter-rank edges are represented as off-center
;; virtual nodes. This guarantees that labels never overlap other nodes,
;; edges or labels."

;;; Code:

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-pass4-splines)

(describe "GKNV Edge Label Virtual Nodes (Section 5.3)"
  
  (describe "edge label virtual node creation"
    
    (it "should create virtual nodes for edges with labels"
      ;; RED TEST: This should fail - virtual node creation doesn't exist yet
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a "A")
        (dag-draw-add-node graph 'b "B")
        
        ;; Add edge with label per GKNV Section 5.3
        (let ((edge (dag-draw-add-edge graph 'a 'b 1 "label-text")))
          (setf (dag-draw-edge-label edge) "test-label")
          
          ;; GKNV: "edge labels on inter-rank edges are represented as off-center virtual nodes"
          (dag-draw--create-edge-label-virtual-nodes graph)
          
          ;; Should create a virtual node for the label
          (let ((label-nodes (dag-draw--get-label-virtual-nodes graph)))
            (expect (length label-nodes) :to-equal 1)
            (expect (dag-draw-node-label (car label-nodes)) :to-equal "test-label")))))
    
    (it "should not create virtual nodes for edges without labels"
      ;; GREEN TEST: Edges without labels should not create virtual nodes
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a "A")
        (dag-draw-add-node graph 'b "B")
        (dag-draw-add-edge graph 'a 'b)
        
        (dag-draw--create-edge-label-virtual-nodes graph)
        
        (let ((label-nodes (dag-draw--get-label-virtual-nodes graph)))
          (expect (length label-nodes) :to-equal 0))))
    
    (it "should handle multiple labeled edges"
      ;; GREEN TEST: Multiple labels should create multiple virtual nodes
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a "A")
        (dag-draw-add-node graph 'b "B")
        (dag-draw-add-node graph 'c "C")
        
        (let ((edge1 (dag-draw-add-edge graph 'a 'b))
              (edge2 (dag-draw-add-edge graph 'b 'c)))
          (setf (dag-draw-edge-label edge1) "label1")
          (setf (dag-draw-edge-label edge2) "label2")
          
          (dag-draw--create-edge-label-virtual-nodes graph)
          
          (let ((label-nodes (dag-draw--get-label-virtual-nodes graph)))
            (expect (length label-nodes) :to-equal 2))))))

  (describe "rank separation compensation per GKNV Section 5.3"
    
    (it "should double minimum edge lengths for labeled edges"
      ;; RED TEST: GKNV Section 5.3 - "Setting the minimum edge length to 2"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a "A")
        (dag-draw-add-node graph 'b "B")
        
        (let ((edge (dag-draw-add-edge graph 'a 'b)))
          (setf (dag-draw-edge-label edge) "test-label")
          
          ;; GKNV: "Setting the minimum edge length to 2 (effectively doubling the ranks)"
          (dag-draw--apply-label-edge-length-compensation graph)
          
          (expect (dag-draw-edge-min-length edge) :to-equal 2))))
    
    (it "should not modify edge lengths for unlabeled edges"
      ;; GREEN TEST: Unlabeled edges should retain original minimum length
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a "A")
        (dag-draw-add-node graph 'b "B")
        
        (let ((edge (dag-draw-add-edge graph 'a 'b)))
          (dag-draw--apply-label-edge-length-compensation graph)
          
          (expect (dag-draw-edge-min-length edge) :to-equal 1))))))

(provide 'dag-draw-edge-label-nodes-test)

;;; dag-draw-edge-label-nodes-test.el ends here