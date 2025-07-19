;;; dag-draw-network-simplex-edge-weights-test.el --- TDD tests for enhanced edge weight system -*- lexical-binding: t -*-

;; Copyright (C) 2024

;;; Commentary:

;; TDD Phase 1.4: Enhanced edge weight system for network simplex algorithm.
;; This implements proper edge weight handling, user constraints, and auxiliary
;; node management as described in GKNV paper sections 2.1 and 2.2.

;;; Code:

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-core)
(require 'dag-draw-pass2-ordering)
(require 'cl-lib)
(require 'dag-draw-pass1-ranking)
(require 'test-helpers)
(require 'dag-draw-test-helpers)

(describe "Network Simplex Enhanced Edge Weight System"
  (describe "edge weight constraints and priorities"

    (it "should implement GKNV minimum edge length constraints"
        ;; RED phase: This test will fail because min-length constraints don't exist yet
        (let ((graph (dag-draw-create-graph)))
          (dag-draw-add-node graph 'layer1 "Layer1")
          (dag-draw-add-node graph 'layer2 "Layer2") 
          (dag-draw-add-node graph 'layer3 "Layer3")
          
          ;; Edge with minimum length constraint > 1
          (dag-draw-add-edge graph 'layer1 'layer2 1)     ; min-length = 1 (default)
          (dag-draw-add-edge graph 'layer2 'layer3 1)     ; min-length = 1 (default)
          (dag-draw-add-edge graph 'layer1 'layer3 5 nil (ht ('min-length 3)))  ; min-length = 3 via attributes
          
          (let* ((spanning-tree (dag-draw--create-feasible-spanning-tree graph))
                 (ranking (dag-draw--spanning-tree-to-ranking graph spanning-tree)))
            
            ;; Should respect minimum length constraints in ranking
            (expect (ht-get ranking 'layer3) :to-be-greater-than (+ (ht-get ranking 'layer1) 2))
            
            ;; Should properly handle constraints in spanning tree
            (expect (dag-draw--tree-respects-min-length-constraints-p spanning-tree graph) :to-be t)))))

  (describe "auxiliary node and edge management"
    (it "should create auxiliary nodes for network simplex optimization"
        ;; RED phase: This test will fail because auxiliary node creation doesn't exist yet
        (let ((graph (dag-draw-create-graph)))
          (dag-draw-add-node graph 'start "Start")
          (dag-draw-add-node graph 'end "End")
          (dag-draw-add-edge graph 'start 'end 1)
          
          (let* ((aux-graph (dag-draw--create-auxiliary-network-simplex-graph graph))
                 (aux-nodes (dag-draw--get-auxiliary-nodes aux-graph)))
            
            ;; Should create auxiliary source and sink nodes
            (expect (dag-draw--has-auxiliary-source-p aux-graph) :to-be t)
            (expect (dag-draw--has-auxiliary-sink-p aux-graph) :to-be t)
            
            ;; Auxiliary nodes should have proper connectivity
            (expect (length aux-nodes) :to-be-greater-than 0)
            (expect (dag-draw--auxiliary-nodes-properly-connected-p aux-graph aux-nodes) :to-be t))))

    (it "should handle long edges with virtual nodes"
        ;; RED phase: This test will fail because virtual node insertion doesn't exist yet
        (let ((graph (dag-draw-create-graph)))
          ;; Create nodes that will be far apart in ranking
          (dag-draw-add-node graph 'top "Top")
          (dag-draw-add-node graph 'middle1 "Middle1")
          (dag-draw-add-node graph 'middle2 "Middle2") 
          (dag-draw-add-node graph 'bottom "Bottom")
          
          ;; Create edges that force ranks to be far apart
          (dag-draw-add-edge graph 'top 'middle1 1)
          (dag-draw-add-edge graph 'middle1 'middle2 1)
          (dag-draw-add-edge graph 'middle2 'bottom 1)
          (dag-draw-add-edge graph 'top 'bottom 1)  ; This will span multiple ranks
          
          ;; First assign ranks
          (dag-draw-assign-ranks graph)
          
          (let* ((virtual-graph (dag-draw--create-virtual-nodes graph))
                 ;; Extract virtual nodes (IDs starting with "virtual_")
                 (virtual-nodes (cl-remove-if-not 
                                (lambda (node-id) 
                                  (string-match "^virtual_" (symbol-name node-id)))
                                (ht-keys (dag-draw-graph-nodes virtual-graph)))))
            
            ;; Should create virtual nodes for long edges
            (expect (length virtual-nodes) :to-be-greater-than 0)
            
            ;; Virtual nodes should be properly ranked
            (expect (dag-draw--virtual-nodes-properly-ranked-p virtual-graph virtual-nodes) :to-be t)
            
            ;; No edge should span more than 1 rank after virtual node insertion
            (expect (dag-draw--all-edges-unit-length-p virtual-graph) :to-be t)))))

  (describe "edge weight optimization and cost calculation"
    (it "should calculate total network cost using edge weights"
        ;; RED phase: This test will fail because cost calculation doesn't exist yet
        (let ((graph (dag-draw-create-graph)))
          (dag-draw-add-node graph 'a "A")
          (dag-draw-add-node graph 'b "B")
          (dag-draw-add-node graph 'c "C")
          (dag-draw-add-edge graph 'a 'b 3)  ; weight = 3
          (dag-draw-add-edge graph 'b 'c 5)  ; weight = 5
          (dag-draw-add-edge graph 'a 'c 2)  ; weight = 2
          
          (let* ((spanning-tree (dag-draw--create-feasible-spanning-tree graph))
                 (network-cost (dag-draw--calculate-network-cost graph spanning-tree)))
            
            ;; Should calculate cost based on tree edge weights and lengths
            (expect (numberp network-cost) :to-be t)
            (expect network-cost :to-be-greater-than 0)
            
            ;; Cost should reflect the weight-distance product
            (expect (dag-draw--cost-reflects-weight-distance-product-p graph spanning-tree network-cost) :to-be t))))

    (it "should optimize edge weights during network simplex iterations"
        ;; RED phase: This test will fail because weight optimization doesn't exist yet
        (let ((graph (dag-draw-create-graph)))
          ;; Create suboptimal initial configuration
          (dag-draw-add-node graph 'hub "Hub")
          (dag-draw-add-node graph 'spoke1 "Spoke1")
          (dag-draw-add-node graph 'spoke2 "Spoke2")
          (dag-draw-add-node graph 'connector "Connector")
          
          (dag-draw-add-edge graph 'hub 'spoke1 1)
          (dag-draw-add-edge graph 'hub 'spoke2 1)
          (dag-draw-add-edge graph 'spoke1 'connector 10)  ; High weight - should be optimized
          (dag-draw-add-edge graph 'spoke2 'connector 1)
          
          ;; Test end-to-end: call the actual public API that users would call
          (dag-draw-assign-ranks graph)
          
          ;; Verify that network simplex optimization occurred and ranks are assigned
          (let ((hub-rank (dag-draw-node-rank (dag-draw-get-node graph 'hub)))
                (spoke1-rank (dag-draw-node-rank (dag-draw-get-node graph 'spoke1)))
                (spoke2-rank (dag-draw-node-rank (dag-draw-get-node graph 'spoke2)))
                (connector-rank (dag-draw-node-rank (dag-draw-get-node graph 'connector))))
            
            ;; All nodes should have ranks assigned
            (expect hub-rank :to-be-truthy)
            (expect spoke1-rank :to-be-truthy)
            (expect spoke2-rank :to-be-truthy)
            (expect connector-rank :to-be-truthy)
            
            ;; Network simplex should have optimized to avoid high-weight edge (spoke1->connector=10)
            ;; Optimal path should prefer spoke2->connector (weight=1) over spoke1->connector (weight=10)
            (expect (< hub-rank connector-rank) :to-be t)  ; Hub should be before connector
            (expect (< spoke2-rank connector-rank) :to-be t)  ; Spoke2 should be before connector
            ))))

)

(provide 'dag-draw-network-simplex-edge-weights-test)

;;; dag-draw-network-simplex-edge-weights-test.el ends here