;;; dag-draw-splines-test.el --- Tests for dag-draw-splines.el -*- lexical-binding: t -*-

;; Copyright (C) 2024

;;; Commentary:

;; Tests for spline edge drawing functionality.

;;; Code:

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-core)
(require 'dag-draw-rank)
(require 'dag-draw-order-simple)
(require 'dag-draw-position)
(require 'dag-draw-splines)

(describe "dag-draw-splines"
  
  (describe "point and curve structures"
    (it "should create points correctly"
      (let ((point (dag-draw-point-create :x 10 :y 20)))
        (expect (dag-draw-point-x point) :to-equal 10)
        (expect (dag-draw-point-y point) :to-equal 20)))
    
    (it "should create Bézier curves correctly"
      (let* ((p0 (dag-draw-point-create :x 0 :y 0))
             (p1 (dag-draw-point-create :x 10 :y 0))
             (p2 (dag-draw-point-create :x 10 :y 10))
             (p3 (dag-draw-point-create :x 20 :y 10))
             (curve (dag-draw-bezier-curve-create :p0 p0 :p1 p1 :p2 p2 :p3 p3)))
        
        (expect (dag-draw-bezier-curve-p0 curve) :to-equal p0)
        (expect (dag-draw-bezier-curve-p3 curve) :to-equal p3))))

  (describe "edge classification"
    (it "should classify inter-rank edges"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a)
        (dag-draw-add-node graph 'b)
        (dag-draw-add-edge graph 'a 'b)
        
        ;; Set different ranks
        (setf (dag-draw-node-rank (dag-draw-get-node graph 'a)) 0)
        (setf (dag-draw-node-rank (dag-draw-get-node graph 'b)) 1)
        
        (let ((edge (car (dag-draw-graph-edges graph))))
          (expect (dag-draw--classify-edge graph edge) :to-equal 'inter-rank-edge))))
    
    (it "should classify flat edges"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a)
        (dag-draw-add-node graph 'b)
        (dag-draw-add-edge graph 'a 'b)
        
        ;; Set same rank
        (setf (dag-draw-node-rank (dag-draw-get-node graph 'a)) 1)
        (setf (dag-draw-node-rank (dag-draw-get-node graph 'b)) 1)
        
        (let ((edge (car (dag-draw-graph-edges graph))))
          (expect (dag-draw--classify-edge graph edge) :to-equal 'flat-edge))))
    
    (it "should classify self edges"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a)
        (dag-draw-add-edge graph 'a 'a)  ; Self-edge
        
        (let ((edge (car (dag-draw-graph-edges graph))))
          (expect (dag-draw--classify-edge graph edge) :to-equal 'self-edge)))))

  (describe "node port calculation"
    (it "should calculate top port correctly"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a)
        (let ((node (dag-draw-get-node graph 'a)))
          (setf (dag-draw-node-x-coord node) 100)
          (setf (dag-draw-node-y-coord node) 200)
          (setf (dag-draw-node-x-size node) 80)
          (setf (dag-draw-node-y-size node) 60)
          
          (let ((port (dag-draw--get-node-port node 'top)))
            (expect (dag-draw-point-x port) :to-equal 100)
            (expect (dag-draw-point-y port) :to-equal 170)))))  ; 200 - 60/2
    
    (it "should calculate all four ports correctly"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a)
        (let ((node (dag-draw-get-node graph 'a)))
          (setf (dag-draw-node-x-coord node) 100)
          (setf (dag-draw-node-y-coord node) 200)
          (setf (dag-draw-node-x-size node) 80)
          (setf (dag-draw-node-y-size node) 60)
          
          (let ((top (dag-draw--get-node-port node 'top))
                (bottom (dag-draw--get-node-port node 'bottom))
                (left (dag-draw--get-node-port node 'left))
                (right (dag-draw--get-node-port node 'right)))
            
            (expect (dag-draw-point-y top) :to-equal 170)     ; top
            (expect (dag-draw-point-y bottom) :to-equal 230)  ; bottom
            (expect (dag-draw-point-x left) :to-equal 60)     ; left
            (expect (dag-draw-point-x right) :to-equal 140))))))  ; right

  (describe "Bézier curve mathematics"
    (it "should calculate points on curve correctly"
      (let* ((p0 (dag-draw-point-create :x 0 :y 0))
             (p1 (dag-draw-point-create :x 0 :y 10))
             (p2 (dag-draw-point-create :x 10 :y 10))
             (p3 (dag-draw-point-create :x 10 :y 0))
             (curve (dag-draw-bezier-curve-create :p0 p0 :p1 p1 :p2 p2 :p3 p3)))
        
        ;; Test endpoints
        (let ((start (dag-draw--bezier-point-at curve 0.0))
              (end (dag-draw--bezier-point-at curve 1.0)))
          (expect (dag-draw-point-x start) :to-equal 0)
          (expect (dag-draw-point-y start) :to-equal 0)
          (expect (dag-draw-point-x end) :to-equal 10)
          (expect (dag-draw-point-y end) :to-equal 0))
        
        ;; Test midpoint
        (let ((mid (dag-draw--bezier-point-at curve 0.5)))
          (expect (dag-draw-point-x mid) :to-equal 5.0)  ; Should be 5 at midpoint
          (expect (dag-draw-point-y mid) :to-equal 7.5))))  ; Should be above line
    
    (it "should sample curve points correctly"
      (let* ((p0 (dag-draw-point-create :x 0 :y 0))
             (p1 (dag-draw-point-create :x 10 :y 0))
             (p2 (dag-draw-point-create :x 10 :y 10))
             (p3 (dag-draw-point-create :x 20 :y 10))
             (curve (dag-draw-bezier-curve-create :p0 p0 :p1 p1 :p2 p2 :p3 p3))
             (points (dag-draw--sample-spline curve 4)))
        
        (expect (length points) :to-equal 5)  ; 4 segments = 5 points
        
        ;; First and last points should match curve endpoints
        (let ((first (car points))
              (last (car (last points))))
          (expect (dag-draw-point-x first) :to-equal 0)
          (expect (dag-draw-point-y first) :to-equal 0)
          (expect (dag-draw-point-x last) :to-equal 20)
          (expect (dag-draw-point-y last) :to-equal 10)))))

  (describe "spline creation"
    (it "should create downward inter-rank splines"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a)
        (dag-draw-add-node graph 'b)
        
        ;; Set coordinates and ranks
        (let ((node-a (dag-draw-get-node graph 'a))
              (node-b (dag-draw-get-node graph 'b)))
          (setf (dag-draw-node-x-coord node-a) 50)
          (setf (dag-draw-node-y-coord node-a) 50)
          (setf (dag-draw-node-rank node-a) 0)
          
          (setf (dag-draw-node-x-coord node-b) 100)
          (setf (dag-draw-node-y-coord node-b) 150)
          (setf (dag-draw-node-rank node-b) 1)
          
          (let ((splines (dag-draw--create-downward-spline graph node-a node-b)))
            (expect (length splines) :to-equal 1)
            
            (let ((curve (car splines)))
              ;; Start should be at bottom of node A
              (expect (dag-draw-point-x (dag-draw-bezier-curve-p0 curve)) :to-equal 50)
              ;; End should be at top of node B  
              (expect (dag-draw-point-x (dag-draw-bezier-curve-p3 curve)) :to-equal 100))))))
    
    (it "should create horizontal flat splines"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a)
        (dag-draw-add-node graph 'b)
        
        ;; Set coordinates on same rank
        (let ((node-a (dag-draw-get-node graph 'a))
              (node-b (dag-draw-get-node graph 'b)))
          (setf (dag-draw-node-x-coord node-a) 50)
          (setf (dag-draw-node-y-coord node-a) 100)
          (setf (dag-draw-node-rank node-a) 1)
          
          (setf (dag-draw-node-x-coord node-b) 150)
          (setf (dag-draw-node-y-coord node-b) 100)
          (setf (dag-draw-node-rank node-b) 1)
          
          (let ((splines (dag-draw--create-horizontal-spline node-a node-b 'right 'left)))
            (expect (length splines) :to-equal 1)
            
            (let ((curve (car splines)))
              ;; Y coordinates should be same (horizontal)
              (expect (dag-draw-point-y (dag-draw-bezier-curve-p0 curve))
                      :to-equal (dag-draw-point-y (dag-draw-bezier-curve-p3 curve))))))))
    
    (it "should create self-edge loops"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a)
        (dag-draw-add-edge graph 'a 'a)
        
        (let ((node (dag-draw-get-node graph 'a)))
          (setf (dag-draw-node-x-coord node) 100)
          (setf (dag-draw-node-y-coord node) 100)
          
          (let ((splines (dag-draw--create-self-spline graph (car (dag-draw-graph-edges graph)))))
            (expect (length splines) :to-equal 2)  ; Two curves for complete loop
            
            ;; Both curves should start and end at same point (loop)
            (let ((curve1 (nth 0 splines))
                  (curve2 (nth 1 splines)))
              (expect (dag-draw-point-x (dag-draw-bezier-curve-p0 curve1))
                      :to-equal (dag-draw-point-x (dag-draw-bezier-curve-p3 curve2)))))))))

  (describe "full spline generation"
    (it "should generate splines for all edge types"
      (let ((graph (dag-draw-create-graph)))
        ;; Create various edge types
        (dag-draw-add-node graph 'a)
        (dag-draw-add-node graph 'b)
        (dag-draw-add-node graph 'c)
        (dag-draw-add-edge graph 'a 'b)  ; inter-rank
        (dag-draw-add-edge graph 'b 'c)  ; flat 
        (dag-draw-add-edge graph 'c 'c)  ; self
        
        ;; Set up coordinates and ranks
        (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'a)) 50)
        (setf (dag-draw-node-y-coord (dag-draw-get-node graph 'a)) 50)
        (setf (dag-draw-node-rank (dag-draw-get-node graph 'a)) 0)
        
        (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'b)) 100)
        (setf (dag-draw-node-y-coord (dag-draw-get-node graph 'b)) 150)
        (setf (dag-draw-node-rank (dag-draw-get-node graph 'b)) 1)
        
        (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'c)) 200)
        (setf (dag-draw-node-y-coord (dag-draw-get-node graph 'c)) 150)
        (setf (dag-draw-node-rank (dag-draw-get-node graph 'c)) 1)
        
        ;; Generate splines
        (dag-draw-generate-splines graph)
        
        ;; All edges should have spline points
        (dolist (edge (dag-draw-graph-edges graph))
          (expect (dag-draw-edge-spline-points edge) :to-be-truthy)
          (expect (length (dag-draw-edge-spline-points edge)) :to-be-greater-than 0))))

  (describe "integration with full pipeline"
    (it "should work with complete layout pipeline"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a "Node A")
        (dag-draw-add-node graph 'b "Node B")
        (dag-draw-add-node graph 'c "Node C")
        (dag-draw-add-edge graph 'a 'b)
        (dag-draw-add-edge graph 'b 'c)
        
        ;; Run complete pipeline
        (dag-draw-rank-graph graph)
        (dag-draw-order-vertices graph)
        (dag-draw-position-nodes graph)
        (dag-draw-generate-splines graph)
        
        ;; All edges should have splines
        (dolist (edge (dag-draw-graph-edges graph))
          (expect (dag-draw-edge-spline-points edge) :to-be-truthy)
          (expect (length (dag-draw-edge-spline-points edge)) :to-be-greater-than 0))))
    
    (it "should handle complex graph with cycles"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a)
        (dag-draw-add-node graph 'b)
        (dag-draw-add-node graph 'c)
        (dag-draw-add-edge graph 'a 'b)
        (dag-draw-add-edge graph 'b 'c)
        (dag-draw-add-edge graph 'c 'a)  ; Creates cycle
        
        ;; Run complete pipeline (should handle cycle breaking)
        (expect (dag-draw-layout-graph graph) :not :to-throw)
        
        ;; Should have splines for all edges
        (expect (length (dag-draw-graph-edges graph)) :to-equal 3)
        (dolist (edge (dag-draw-graph-edges graph))
          (expect (dag-draw-edge-spline-points edge) :to-be-truthy))))

  (describe "utility functions"
    (it "should calculate spline bounds correctly"
      (let* ((p0 (dag-draw-point-create :x 0 :y 0))
             (p1 (dag-draw-point-create :x 10 :y 5))
             (p2 (dag-draw-point-create :x 20 :y 5))
             (p3 (dag-draw-point-create :x 30 :y 0))
             (curve (dag-draw-bezier-curve-create :p0 p0 :p1 p1 :p2 p2 :p3 p3))
             (bounds (dag-draw--spline-bounds (list curve))))
        
        (expect (dag-draw-box-x-min bounds) :to-be-less-than-or-equal 0)
        (expect (dag-draw-box-x-max bounds) :to-be-greater-than-or-equal 30)
        (expect (dag-draw-box-y-min bounds) :to-be-less-than-or-equal 0)
        (expect (dag-draw-box-y-max bounds) :to-be-greater-than-or-equal 0)))
    
    (it "should calculate approximate spline length"
      (let* ((p0 (dag-draw-point-create :x 0 :y 0))
             (p1 (dag-draw-point-create :x 10 :y 0))
             (p2 (dag-draw-point-create :x 10 :y 10))
             (p3 (dag-draw-point-create :x 20 :y 10))
             (curve (dag-draw-bezier-curve-create :p0 p0 :p1 p1 :p2 p2 :p3 p3))
             (length (dag-draw--spline-length (list curve))))
        
        ;; Length should be greater than straight-line distance
        (expect length :to-be-greater-than 22.0)  ; sqrt(20^2 + 10^2) ≈ 22.36
        (expect length :to-be-less-than 40.0))))  ; Should be reasonable

;;; dag-draw-splines-test.el ends here