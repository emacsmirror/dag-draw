;;; dag-draw-auxiliary-integration-test.el --- Test auxiliary node creation during network simplex -*- lexical-binding: t -*-

;; Copyright (C) 2024

;;; Commentary:

;; Test to verify that auxiliary nodes are actually being created and cleaned up
;; during the network simplex process, as required by GKNV Figure 2-2.

;;; Code:

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-core)
(require 'dag-draw-pass1-ranking)

(describe "Auxiliary Node Integration with Network Simplex"
  
  (it "should create auxiliary nodes during network simplex and clean them up afterward"
    ;; This test will fail if network simplex doesn't actually use auxiliary nodes
    (let ((graph (dag-draw-create-graph)))
      ;; Create a graph that requires network simplex
      (dag-draw-add-node graph 'a "A")
      (dag-draw-add-node graph 'b "B") 
      (dag-draw-add-node graph 'c "C")
      (dag-draw-add-edge graph 'a 'b 2)
      (dag-draw-add-edge graph 'b 'c 3)
      
      ;; Store original node count
      (let ((original-node-count (length (dag-draw-get-node-ids graph)))
            (aux-nodes-detected nil))
        
        ;; Spy on auxiliary cleanup to see if it's called with actual auxiliary nodes
        (spy-on 'dag-draw--cleanup-auxiliary-elements 
                :and-call-fake (lambda (g)
                                 (let ((aux-nodes (dag-draw--find-auxiliary-nodes g)))
                                   (when aux-nodes
                                     (setq aux-nodes-detected t))
                                   ;; Do cleanup manually to avoid recursion
                                   (let ((aux-nodes (dag-draw--find-auxiliary-nodes g))
                                         (aux-edges (dag-draw--find-auxiliary-edges g)))
                                     (dolist (edge aux-edges)
                                       (dag-draw-remove-edge g 
                                                             (dag-draw-edge-from-node edge)
                                                             (dag-draw-edge-to-node edge)))
                                     (dolist (node-id aux-nodes)
                                       (when (dag-draw-get-node g node-id)
                                         (dag-draw-remove-node g node-id)))))))
        
        ;; Run network simplex 
        (dag-draw--assign-ranks-network-simplex graph)
        
        ;; Verify that auxiliary nodes were actually created and cleaned up
        (expect aux-nodes-detected :to-be t)
        
        ;; Verify final state has no auxiliary nodes
        (expect (length (dag-draw-get-node-ids graph)) :to-equal original-node-count)
        (expect (dag-draw--find-auxiliary-nodes graph) :to-equal '())))))

(provide 'dag-draw-auxiliary-integration-test)

;;; dag-draw-auxiliary-integration-test.el ends here