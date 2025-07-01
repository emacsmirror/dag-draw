;;; dag-draw-node-ports-test.el --- Unit tests for node port calculation -*- lexical-binding: t -*-

;; Copyright (C) 2024

;;; Commentary:

;; Unit tests for calculating where edges should connect to rectangular nodes.
;; This implements the "port" concept from GKNV algorithm where edges connect
;; to node boundaries rather than centers.

;;; Code:

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-core)
(require 'dag-draw-splines)
(require 'dag-draw-render)

(describe "Node Port Calculation"

  (describe "Basic port calculation for rectangular nodes"
    
    (it "should calculate top port for upward connections"
      (let ((node (dag-draw-node-create 
                   :id 'test
                   :x-coord 100 :y-coord 100
                   :x-size 60 :y-size 40)))
        
        ;; Top port should be at center-top of rectangle
        ;; Rectangle spans from (70,80) to (130,120)  
        ;; Top port should be at (100, 80)
        (let ((port (dag-draw--get-node-port node 'top)))
          (expect (dag-draw-point-x port) :to-equal 100.0)
          (expect (dag-draw-point-y port) :to-equal 80.0))))
    
    (it "should calculate bottom port for downward connections"
      (let ((node (dag-draw-node-create
                   :id 'test
                   :x-coord 100 :y-coord 100
                   :x-size 60 :y-size 40)))
        
        ;; Bottom port should be at center-bottom of rectangle
        ;; Bottom port should be at (100, 120)
        (let ((port (dag-draw--get-node-port node 'bottom)))
          (expect (dag-draw-point-x port) :to-equal 100.0)
          (expect (dag-draw-point-y port) :to-equal 120.0))))
    
    (it "should calculate left port for leftward connections"
      (let ((node (dag-draw-node-create
                   :id 'test
                   :x-coord 100 :y-coord 100
                   :x-size 60 :y-size 40)))
        
        ;; Left port should be at center-left of rectangle
        ;; Left port should be at (70, 100)
        (let ((port (dag-draw--get-node-port node 'left)))
          (expect (dag-draw-point-x port) :to-equal 70.0)
          (expect (dag-draw-point-y port) :to-equal 100.0))))
    
    (it "should calculate right port for rightward connections" 
      (let ((node (dag-draw-node-create
                   :id 'test
                   :x-coord 100 :y-coord 100
                   :x-size 60 :y-size 40)))
        
        ;; Right port should be at center-right of rectangle
        ;; Right port should be at (130, 100)
        (let ((port (dag-draw--get-node-port node 'right)))
          (expect (dag-draw-point-x port) :to-equal 130.0)
          (expect (dag-draw-point-y port) :to-equal 100.0)))))

  (describe "Edge direction-based port selection"
    
    (it "should select correct ports for vertical edges"
      (let ((from-node (dag-draw-node-create
                        :id 'from
                        :x-coord 100 :y-coord 50
                        :x-size 40 :y-size 20))
            (to-node (dag-draw-node-create
                      :id 'to
                      :x-coord 100 :y-coord 150
                      :x-size 40 :y-size 20)))
        
        ;; For vertical edge going down, should use bottom port of from-node
        ;; and top port of to-node
        (let ((ports (dag-draw--calculate-edge-ports from-node to-node)))
          (expect (dag-draw-point-x (car ports)) :to-equal 100.0)   ; from bottom port x
          (expect (dag-draw-point-y (car ports)) :to-equal 60.0)    ; from bottom port y
          (expect (dag-draw-point-x (cadr ports)) :to-equal 100.0)  ; to top port x
          (expect (dag-draw-point-y (cadr ports)) :to-equal 140.0)))) ; to top port y
    
    (it "should select correct ports for horizontal edges"
      (let ((from-node (dag-draw-node-create
                        :id 'from
                        :x-coord 50 :y-coord 100
                        :x-size 40 :y-size 20))
            (to-node (dag-draw-node-create
                      :id 'to
                      :x-coord 150 :y-coord 100
                      :x-size 40 :y-size 20)))
        
        ;; For horizontal edge going right, should use right port of from-node
        ;; and left port of to-node
        (let ((ports (dag-draw--calculate-edge-ports from-node to-node)))
          (expect (dag-draw-point-x (car ports)) :to-equal 70.0)    ; from right port x
          (expect (dag-draw-point-y (car ports)) :to-equal 100.0)   ; from right port y
          (expect (dag-draw-point-x (cadr ports)) :to-equal 130.0)  ; to left port x
          (expect (dag-draw-point-y (cadr ports)) :to-equal 100.0)))) ; to left port y
    
    (it "should handle diagonal edges by choosing primary direction"
      (let ((from-node (dag-draw-node-create
                        :id 'from
                        :x-coord 50 :y-coord 50
                        :x-size 40 :y-size 20))
            (to-node (dag-draw-node-create
                      :id 'to
                      :x-coord 150 :y-coord 150
                      :x-size 40 :y-size 20)))
        
        ;; For diagonal edge (down-right), should prefer vertical direction
        ;; Bottom port of from-node, top port of to-node
        (let ((ports (dag-draw--calculate-edge-ports from-node to-node)))
          (expect (dag-draw-point-y (car ports)) :to-equal 60.0)    ; from bottom port
          (expect (dag-draw-point-y (cadr ports)) :to-equal 140.0)))) ; to top port

  (describe "Grid coordinate conversion"
    
    (it "should convert world coordinates to grid coordinates"
      ;; Test the coordinate transformation used in ASCII rendering
      (let ((world-point (dag-draw-point-create :x 100 :y 50))
            (min-x 0) (min-y 0) (scale 2))  ; Note: Uses global coordinate scale
        
        (let ((grid-point (dag-draw--world-point-to-grid world-point min-x min-y scale)))
          ;; ASCII-GKNV SCALING: Uses dag-draw-ascii-coordinate-scale (0.6)
          (expect (dag-draw-point-x grid-point) :to-equal 60.0)  ; 100 * 0.6 = 60
          (expect (dag-draw-point-y grid-point) :to-equal 30.0)))) ; 50 * 0.6 = 30
    
    (it "should handle coordinate offsets correctly"
      (let ((world-point (dag-draw-point-create :x 150 :y 100))
            (min-x 50) (min-y 25) (scale 2))  ; Note: Uses global coordinate scale
        
        (let ((grid-point (dag-draw--world-point-to-grid world-point min-x min-y scale)))
          ;; ASCII-GKNV SCALING: Uses dag-draw-ascii-coordinate-scale (0.6)
          ;; (150-50) * 0.6 = 60, (100-25) * 0.6 = 45
          (expect (dag-draw-point-x grid-point) :to-equal 60.0)
          (expect (dag-draw-point-y grid-point) :to-equal 45.0)))))

  (describe "Integration with edge routing"
    
    (it "should provide port coordinates for ASCII edge drawing"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'source "Source")
        (dag-draw-add-node graph 'target "Target")
        (dag-draw-add-edge graph 'source 'target)
        
        ;; Set coordinates
        (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'source)) 100)
        (setf (dag-draw-node-y-coord (dag-draw-get-node graph 'source)) 50)
        (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'target)) 100)
        (setf (dag-draw-node-y-coord (dag-draw-get-node graph 'target)) 150)
        
        ;; Should be able to get connection points for this edge
        (let ((edge (car (dag-draw-graph-edges graph))))
          (let ((connection-points (dag-draw--get-edge-connection-points graph edge)))
            (expect (length connection-points) :to-equal 2)
            ;; First point should be bottom of source, second should be top of target
            (expect (dag-draw-point-y (car connection-points)) :to-be-less-than
                    (dag-draw-point-y (cadr connection-points))))))))))

;;; dag-draw-node-ports-test.el ends here