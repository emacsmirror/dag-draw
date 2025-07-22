;;; dag-draw-edge-label-integration-test.el --- Integration tests for edge labels -*- lexical-binding: t -*-

;; Copyright (C) 2024

;;; Commentary:

;; Integration tests for edge label functionality in the mainline pipeline.
;; Tests that edge labels are processed automatically during graph layout.

;;; Code:

(require 'buttercup)
(require 'dag-draw)

(describe "Edge Label Integration with Mainline Pipeline"
  
  (it "should automatically process edge labels during layout"
    ;; Test that dag-draw-layout-graph automatically handles edge labels
    (let ((graph (dag-draw-create-graph)))
      (dag-draw-add-node graph 'a "A")
      (dag-draw-add-node graph 'b "B")
      
      ;; Add edge with label
      (let ((edge (dag-draw-add-edge graph 'a 'b)))
        (setf (dag-draw-edge-label edge) "test-label")
        
        ;; Layout graph - this should automatically process edge labels
        (dag-draw-layout-graph graph)
        
        ;; Check that virtual node was created
        (let ((label-nodes (dag-draw--get-label-virtual-nodes graph)))
          (expect (length label-nodes) :to-equal 1)
          (expect (dag-draw-node-label (car label-nodes)) :to-equal "test-label"))
        
        ;; Check that edge length was adjusted
        (expect (dag-draw-edge-δ edge) :to-equal 2))))
  
  (it "should skip processing when no edge labels exist"
    ;; Test that graphs without labels work normally
    (let ((graph (dag-draw-create-graph)))
      (dag-draw-add-node graph 'a "A")  
      (dag-draw-add-node graph 'b "B")
      (dag-draw-add-edge graph 'a 'b)
      
      ;; Layout graph without labels
      (dag-draw-layout-graph graph)
      
      ;; Should have no virtual nodes
      (let ((label-nodes (dag-draw--get-label-virtual-nodes graph)))
        (expect (length label-nodes) :to-equal 0))))
  
  (it "should handle mixed labeled and unlabeled edges"
    ;; Test proper handling of mixed edge types
    (let ((graph (dag-draw-create-graph)))
      (dag-draw-add-node graph 'a "A")
      (dag-draw-add-node graph 'b "B") 
      (dag-draw-add-node graph 'c "C")
      
      (let ((labeled-edge (dag-draw-add-edge graph 'a 'b))
            (unlabeled-edge (dag-draw-add-edge graph 'b 'c)))
        (setf (dag-draw-edge-label labeled-edge) "labeled")
        
        ;; Layout the graph
        (dag-draw-layout-graph graph)
        
        ;; Check that only labeled edge was modified
        (expect (dag-draw-edge-δ labeled-edge) :to-equal 2)
        (expect (dag-draw-edge-δ unlabeled-edge) :to-equal 1)
        
        ;; Check that one virtual node was created
        (let ((label-nodes (dag-draw--get-label-virtual-nodes graph)))
          (expect (length label-nodes) :to-equal 1))))))

(provide 'dag-draw-edge-label-integration-test)

;;; dag-draw-edge-label-integration-test.el ends here