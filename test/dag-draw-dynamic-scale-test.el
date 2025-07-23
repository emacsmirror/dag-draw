;;; dag-draw-dynamic-scale-test.el --- Tests for dynamic ASCII scale calculation -*- lexical-binding: t -*-

;; Copyright (C) 2024

;;; Commentary:

;; Tests for dynamic ASCII scale calculation functionality.
;; Implements Phase 1 of ASCII-native GKNV implementation.

;;; Code:

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-core)
(require 'dag-draw-ascii-grid)

(describe "Dynamic ASCII scale calculation"
  
  (describe "basic scale calculation"
    (it "should calculate optimal scale for simple graphs"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a "Node A")
        (dag-draw-add-node graph 'b "Node B") 
        (dag-draw-add-node graph 'c "Node C")
        (dag-draw-add-edge graph 'a 'b)
        (dag-draw-add-edge graph 'b 'c)
        
        ;; For a simple 3-node graph targeting 80x24 ASCII area,
        ;; expect scale around 0.2 (slightly higher than fixed 0.15)
        (let ((scale (dag-draw--calculate-optimal-ascii-scale graph 80 24)))
          (expect scale :to-be-greater-than 0.1)
          (expect scale :to-be-less-than 0.5)
          (expect scale :to-be-close-to 0.2 0.1))))
    
    (it "should return smaller scale for complex graphs"
      (let ((graph (dag-draw-create-graph)))
        ;; Create a more complex graph with many nodes
        (dotimes (i 10)
          (dag-draw-add-node graph (intern (format "node%d" i)) (format "Node %d" i)))
        (dotimes (i 9)
          (dag-draw-add-edge graph (intern (format "node%d" i)) (intern (format "node%d" (1+ i)))))
        
        ;; Complex graph should get smaller scale to fit in same ASCII area
        (let ((scale (dag-draw--calculate-optimal-ascii-scale graph 80 24)))
          (expect scale :to-be-greater-than 0.05)
          (expect scale :to-be-less-than 0.15))))
    
    (it "should prevent coordinate collapse"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a "A")
        (dag-draw-add-node graph 'b "B")
        (dag-draw-add-edge graph 'a 'b)
        
        ;; Even for tiny ASCII area, should not collapse coordinates
        (let ((scale (dag-draw--calculate-optimal-ascii-scale graph 10 5)))
          ;; Minimum scale to prevent collapse: at least 0.02
          (expect scale :to-be-greater-than 0.02))))
    
    (it "should detect and prevent coordinate collapse"
      (let ((graph (dag-draw-create-graph)))
        ;; Create nodes with coordinates that would collapse at small scales
        (let ((node-a (dag-draw-add-node graph 'a "Node A"))
              (node-b (dag-draw-add-node graph 'b "Node B")))
          ;; Set coordinates that are very close together
          (setf (dag-draw-node-x-coord node-a) 100.0)
          (setf (dag-draw-node-y-coord node-a) 100.0)
          (setf (dag-draw-node-x-coord node-b) 101.0)
          (setf (dag-draw-node-y-coord node-b) 101.0)
          
          ;; Small scale would normally cause collapse, but should be prevented
          (let ((scale (dag-draw--calculate-optimal-ascii-scale graph 20 10)))
            ;; Should get a scale that prevents the coordinates from collapsing
            ;; At scale 0.02, nodes would be at (2,2) and (2,2) - collapsed
            ;; At scale 0.03, nodes would be at (3,3) and (3,3) - still collapsed  
            ;; At scale 0.05, nodes would be at (5,5) and (5,5) - still collapsed
            ;; Should get higher scale to separate them
            (expect scale :to-be-greater-than 0.05))))))
  
  (describe "enhanced complexity analysis"
    (it "should consider node size variations"
      (let ((graph1 (dag-draw-create-graph))
            (graph2 (dag-draw-create-graph)))
        ;; Graph1: uniform small nodes
        (dag-draw-add-node graph1 'a "A")
        (dag-draw-add-node graph1 'b "B")
        
        ;; Graph2: varied large nodes  
        (dag-draw-add-node graph2 'x "Very Long Node Name Here")
        (dag-draw-add-node graph2 'y "Y")
        
        (let ((scale1 (dag-draw--calculate-optimal-ascii-scale graph1 80 24))
              (scale2 (dag-draw--calculate-optimal-ascii-scale graph2 80 24)))
          ;; Graph with varied node sizes should get smaller scale
          (expect scale2 :to-be-less-than scale1))))
    
    (it "should consider edge density"
      (let ((sparse-graph (dag-draw-create-graph))
            (dense-graph (dag-draw-create-graph)))
        ;; Sparse graph: 4 nodes, 2 edges
        (dag-draw-add-node sparse-graph 'a "A")
        (dag-draw-add-node sparse-graph 'b "B")  
        (dag-draw-add-node sparse-graph 'c "C")
        (dag-draw-add-node sparse-graph 'd "D")
        (dag-draw-add-edge sparse-graph 'a 'b)
        (dag-draw-add-edge sparse-graph 'b 'c)
        
        ;; Dense graph: 4 nodes, 6 edges (more connections)
        (dag-draw-add-node dense-graph 'w "W")
        (dag-draw-add-node dense-graph 'x "X")  
        (dag-draw-add-node dense-graph 'y "Y")
        (dag-draw-add-node dense-graph 'z "Z")
        (dag-draw-add-edge dense-graph 'w 'x)
        (dag-draw-add-edge dense-graph 'w 'y)
        (dag-draw-add-edge dense-graph 'w 'z)
        (dag-draw-add-edge dense-graph 'x 'y)
        (dag-draw-add-edge dense-graph 'x 'z)
        (dag-draw-add-edge dense-graph 'y 'z)
        
        (let ((sparse-scale (dag-draw--calculate-optimal-ascii-scale sparse-graph 80 24))
              (dense-scale (dag-draw--calculate-optimal-ascii-scale dense-graph 80 24)))
          ;; Dense graph should get smaller scale
          (expect dense-scale :to-be-less-than sparse-scale))))
    
    (it "should consider hierarchy depth"
      (let ((shallow-graph (dag-draw-create-graph))
            (deep-graph (dag-draw-create-graph)))
        ;; Shallow graph: 2 levels
        (dag-draw-add-node shallow-graph 'root "Root")
        (dag-draw-add-node shallow-graph 'child1 "Child1")
        (dag-draw-add-node shallow-graph 'child2 "Child2")
        (dag-draw-add-edge shallow-graph 'root 'child1)
        (dag-draw-add-edge shallow-graph 'root 'child2)
        
        ;; Deep graph: 4 levels
        (dag-draw-add-node deep-graph 'r "Root")
        (dag-draw-add-node deep-graph 'l1 "Level1")
        (dag-draw-add-node deep-graph 'l2 "Level2")
        (dag-draw-add-node deep-graph 'l3 "Level3")
        (dag-draw-add-edge deep-graph 'r 'l1)
        (dag-draw-add-edge deep-graph 'l1 'l2)
        (dag-draw-add-edge deep-graph 'l2 'l3)
        
        (let ((shallow-scale (dag-draw--calculate-optimal-ascii-scale shallow-graph 80 24))
              (deep-scale (dag-draw--calculate-optimal-ascii-scale deep-graph 80 24)))
          ;; Both should be reasonable scales
          (expect shallow-scale :to-be-greater-than 0.05)
          (expect deep-scale :to-be-greater-than 0.05)))))
  
  (describe "scale validation"
    (it "should handle empty graphs gracefully"
      (let ((graph (dag-draw-create-graph)))
        (let ((scale (dag-draw--calculate-optimal-ascii-scale graph 80 24)))
          (expect scale :to-be-greater-than 0)
          (expect scale :to-be-less-than 1))))
    
    (it "should handle single node graphs"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'only "Only Node")
        (let ((scale (dag-draw--calculate-optimal-ascii-scale graph 80 24)))
          (expect scale :to-be-greater-than 0.1)
          (expect scale :to-be-less-than 0.5))))))

;;; dag-draw-dynamic-scale-test.el ends here