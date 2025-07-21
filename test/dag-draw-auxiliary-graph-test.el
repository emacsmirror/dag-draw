;;; dag-draw-auxiliary-graph-test.el --- Tests for GKNV auxiliary graph construction -*- lexical-binding: t -*-

;; Copyright (C) 2024

;;; Commentary:

;; Tests to ensure proper GKNV auxiliary graph construction per Section 4.2.
;; Validates proper n_e edge cost nodes and (n_e, u), (n_e, v) cost encoding edges.
;;
;; GKNV Reference: Section 4.2, lines 1413-1422
;; Ubiquitous Language: Auxiliary Graph Method - proper cost encoding implementation

;;; Code:

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-core)
(require 'dag-draw-pass3-positioning)

(describe "GKNV Auxiliary Graph Construction"
  
  (describe "proper cost encoding per Section 4.2"
    
    (it "should create n_e edge cost nodes for each original edge"
      ;; RED TEST: Current implementation uses aux_edge naming instead of n_e
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a "A")
        (dag-draw-add-node graph 'b "B") 
        
        ;; Add edge that should get n_e cost node
        (dag-draw-add-edge graph 'a 'b 2)  ; ω(e) = 2
        
        (let ((aux-graph (dag-draw--build-constraint-auxiliary-graph graph)))
          ;; Should create n_e cost node with GKNV naming convention
          (let ((ne-node (dag-draw-get-node aux-graph 'n_e_a_b)))
            (expect ne-node :not :to-be nil)
            ;; Should be labeled as edge cost node per GKNV terminology
            (expect (dag-draw-node-label ne-node) :to-match "n_e")))))
    
    (it "should create cost encoding edges (n_e, u) and (n_e, v)"
      ;; RED TEST: Verify proper edge directions per GKNV Section 4.2, line 1420
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'u "U")
        (dag-draw-add-node graph 'v "V")
        (dag-draw-add-edge graph 'u 'v 3)
        
        ;; Set different ranks to avoid separation edge conflicts
        (let ((u-node (dag-draw-get-node graph 'u))
              (v-node (dag-draw-get-node graph 'v)))
          (setf (dag-draw-node-rank u-node) 0)
          (setf (dag-draw-node-rank v-node) 1))
        
        (let ((aux-graph (dag-draw--build-constraint-auxiliary-graph graph)))
          ;; Should find cost encoding edges (n_e, u) and (n_e, v)
          (let ((ne-to-u-edge (cl-find-if 
                               (lambda (e)
                                 (and (eq (dag-draw-edge-from-node e) 'n_e_u_v)
                                      (eq (dag-draw-edge-to-node e) 'u)))
                               (dag-draw-graph-edges aux-graph)))
                (ne-to-v-edge (cl-find-if
                               (lambda (e)
                                 (and (eq (dag-draw-edge-from-node e) 'n_e_u_v)
                                      (eq (dag-draw-edge-to-node e) 'v)))
                               (dag-draw-graph-edges aux-graph))))
            
            (expect ne-to-u-edge :not :to-be nil)
            (expect ne-to-v-edge :not :to-be nil)
            
            ;; Cost encoding edges should have δ = 0 per GKNV
            (expect (dag-draw-edge-δ ne-to-u-edge) :to-equal 0)
            (expect (dag-draw-edge-δ ne-to-v-edge) :to-equal 0)
            
            ;; Should have ω = ω(e) × Ω(e) weight
            (expect (> (dag-draw-edge-ω ne-to-u-edge) 0) :to-be t)
            (expect (> (dag-draw-edge-ω ne-to-v-edge) 0) :to-be t)))))
    
    (it "should create separation encoding edges with proper constraints"
      ;; GREEN TEST: Test separation edges for adjacent nodes
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'left "Left")
        (dag-draw-add-node graph 'right "Right")
        
        ;; Set same rank for both nodes  
        (let ((left-node (dag-draw-get-node graph 'left))
              (right-node (dag-draw-get-node graph 'right)))
          (setf (dag-draw-node-rank left-node) 0)
          (setf (dag-draw-node-rank right-node) 0)
          (setf (dag-draw-node-x-coord left-node) 0.0)
          (setf (dag-draw-node-x-coord right-node) 100.0))
        
        (let ((aux-graph (dag-draw--build-constraint-auxiliary-graph graph)))
          ;; Should have separation edge (left, right)
          (let ((sep-edge (cl-find-if
                           (lambda (e)
                             (and (eq (dag-draw-edge-from-node e) 'left)
                                  (eq (dag-draw-edge-to-node e) 'right)))
                           (dag-draw-graph-edges aux-graph))))
            (expect sep-edge :not :to-be nil)
            ;; Separation edge should have ω = 0 and δ = ρ(left,right) per GKNV
            (expect (dag-draw-edge-ω sep-edge) :to-equal 0)
            (expect (> (dag-draw-edge-δ sep-edge) 0) :to-be t)))))))

(provide 'dag-draw-auxiliary-graph-test)

;;; dag-draw-auxiliary-graph-test.el ends here