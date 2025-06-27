;;; dag-draw-render-test.el --- Tests for dag-draw-render.el -*- lexical-binding: t -*-

;; Copyright (C) 2024

;;; Commentary:

;; Tests for graph rendering functionality.

;;; Code:

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-core)
(require 'dag-draw-rank)
(require 'dag-draw-order-simple)
(require 'dag-draw-position)
(require 'dag-draw-splines)
(require 'dag-draw-render)

(describe "dag-draw-render"
  
  (describe "SVG rendering"
    (it "should produce valid SVG output"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a "Node A")
        (dag-draw-add-node graph 'b "Node B")
        (dag-draw-add-edge graph 'a 'b)
        
        ;; Set up basic positioning
        (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'a)) 50)
        (setf (dag-draw-node-y-coord (dag-draw-get-node graph 'a)) 50)
        (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'b)) 150)
        (setf (dag-draw-node-y-coord (dag-draw-get-node graph 'b)) 150)
        
        (let ((svg-output (dag-draw-render-svg graph)))
          (expect svg-output :to-match "<svg")
          (expect svg-output :to-match "</svg>")
          (expect svg-output :to-match "xmlns=\"http://www.w3.org/2000/svg\"")
          (expect svg-output :to-match "<rect")
          (expect svg-output :to-match "Node A")
          (expect svg-output :to-match "Node B"))))
    
    (it "should include arrow markers in SVG"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a)
        (dag-draw-add-node graph 'b)
        (dag-draw-add-edge graph 'a 'b)
        
        (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'a)) 0)
        (setf (dag-draw-node-y-coord (dag-draw-get-node graph 'a)) 0)
        (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'b)) 100)
        (setf (dag-draw-node-y-coord (dag-draw-get-node graph 'b)) 100)
        
        (let ((svg-output (dag-draw-render-svg graph)))
          (expect svg-output :to-match "<marker")
          (expect svg-output :to-match "id=\"arrowhead\"")
          (expect svg-output :to-match "marker-end"))))
    
    (it "should handle edge labels in SVG"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a)
        (dag-draw-add-node graph 'b)
        (dag-draw-add-edge graph 'a 'b 1 "test-label")
        
        (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'a)) 0)
        (setf (dag-draw-node-y-coord (dag-draw-get-node graph 'a)) 0)
        (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'b)) 100)
        (setf (dag-draw-node-y-coord (dag-draw-get-node graph 'b)) 100)
        
        ;; Set edge label position
        (let ((edge (car (dag-draw-graph-edges graph))))
          (setf (dag-draw-edge-label-position edge)
                (dag-draw-point-create :x 50 :y 50)))
        
        (let ((svg-output (dag-draw-render-svg graph)))
          (expect svg-output :to-match "test-label"))))
    
    (it "should escape XML special characters"
      (expect (dag-draw--escape-xml "A&B<C>D\"E'F") 
              :to-equal "A&amp;B&lt;C&gt;D&quot;E&apos;F")))

  (describe "ASCII rendering"
    (it "should produce ASCII art output"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a "A")
        (dag-draw-add-node graph 'b "B")
        (dag-draw-add-edge graph 'a 'b)
        
        ;; Set up positioning
        (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'a)) 50)
        (setf (dag-draw-node-y-coord (dag-draw-get-node graph 'a)) 50)
        (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'b)) 150)
        (setf (dag-draw-node-y-coord (dag-draw-get-node graph 'b)) 150)
        
        (let ((ascii-output (dag-draw-render-ascii graph)))
          (expect ascii-output :to-be-truthy)
          (expect (length ascii-output) :to-be-greater-than 10)
          (expect ascii-output :to-match "A")
          (expect ascii-output :to-match "B"))))
    
    (it "should use box-drawing characters"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'test "Test")
        
        (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'test)) 100)
        (setf (dag-draw-node-y-coord (dag-draw-get-node graph 'test)) 100)
        
        (let ((ascii-output (dag-draw-render-ascii graph)))
          ;; Should contain box-drawing characters
          (expect ascii-output :to-match "[┌┐└┘│─]"))))
    
    (it "should handle empty graphs gracefully"
      (let ((graph (dag-draw-create-graph)))
        (let ((ascii-output (dag-draw-render-ascii graph)))
          (expect ascii-output :to-be-truthy)
          (expect (length ascii-output) :to-be-greater-than 0)))))

  (describe "DOT format rendering"
    (it "should produce valid DOT output"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a "Node A")
        (dag-draw-add-node graph 'b "Node B")
        (dag-draw-add-edge graph 'a 'b)
        
        (let ((dot-output (dag-draw-render-dot graph)))
          (expect dot-output :to-match "digraph G")
          (expect dot-output :to-match "a \\[label=\"Node A\"\\]")
          (expect dot-output :to-match "b \\[label=\"Node B\"\\]")
          (expect dot-output :to-match "a -> b"))))
    
    (it "should handle edge labels in DOT"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a)
        (dag-draw-add-node graph 'b)
        (dag-draw-add-edge graph 'a 'b 1 "edge-label")
        
        (let ((dot-output (dag-draw-render-dot graph)))
          (expect dot-output :to-match "a -> b \\[label=\"edge-label\"\\]"))))
    
    (it "should escape DOT special characters"
      (expect (dag-draw--escape-dot-string "test\"quote\\slash")
              :to-equal "test\\\\\"quote\\\\\\\\slash")))

  (describe "graph bounds calculation"
    (it "should calculate correct bounds for positioned nodes"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a)
        (dag-draw-add-node graph 'b)
        
        ;; Set coordinates and sizes
        (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'a)) 100)
        (setf (dag-draw-node-y-coord (dag-draw-get-node graph 'a)) 200)
        (setf (dag-draw-node-x-size (dag-draw-get-node graph 'a)) 80)
        (setf (dag-draw-node-y-size (dag-draw-get-node graph 'a)) 60)
        
        (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'b)) 300)
        (setf (dag-draw-node-y-coord (dag-draw-get-node graph 'b)) 400)
        (setf (dag-draw-node-x-size (dag-draw-get-node graph 'b)) 100)
        (setf (dag-draw-node-y-size (dag-draw-get-node graph 'b)) 40)
        
        (let ((bounds (dag-draw-get-graph-bounds graph)))
          (expect (nth 0 bounds) :to-equal 60.0)   ; min-x (100 - 80/2)
          (expect (nth 1 bounds) :to-equal 170.0)  ; min-y (200 - 60/2)
          (expect (nth 2 bounds) :to-equal 350.0)  ; max-x (300 + 100/2)
          (expect (nth 3 bounds) :to-equal 420.0))))  ; max-y (400 + 40/2)

  (describe "integration with full pipeline"
    (it "should render complete laid-out graphs"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'root "Root")
        (dag-draw-add-node graph 'left "Left")
        (dag-draw-add-node graph 'right "Right")
        (dag-draw-add-node graph 'leaf "Leaf")
        (dag-draw-add-edge graph 'root 'left)
        (dag-draw-add-edge graph 'root 'right)
        (dag-draw-add-edge graph 'left 'leaf)
        (dag-draw-add-edge graph 'right 'leaf)
        
        ;; Run complete layout pipeline
        (dag-draw-layout-graph graph)
        
        ;; Test all rendering formats
        (let ((svg-output (dag-draw-render-svg graph))
              (ascii-output (dag-draw-render-ascii graph))
              (dot-output (dag-draw-render-dot graph)))
          
          ;; SVG should be well-formed
          (expect svg-output :to-match "<svg")
          (expect svg-output :to-match "</svg>")
          (expect svg-output :to-match "Root")
          (expect svg-output :to-match "Leaf")
          
          ;; ASCII should contain node labels
          (expect ascii-output :to-match "Root")
          (expect ascii-output :to-match "Leaf")
          
          ;; DOT should contain all edges
          (expect dot-output :to-match "root -> left")
          (expect dot-output :to-match "root -> right")
          (expect dot-output :to-match "left -> leaf")
          (expect dot-output :to-match "right -> leaf"))))
    
    (it "should handle graphs with splines"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a)
        (dag-draw-add-node graph 'b)
        (dag-draw-add-edge graph 'a 'b)
        
        ;; Run layout to generate splines
        (dag-draw-layout-graph graph)
        
        ;; Check that edges have spline points
        (let ((edge (car (dag-draw-graph-edges graph))))
          (expect (dag-draw-edge-spline-points edge) :to-be-truthy))
        
        ;; SVG should render splines as paths
        (let ((svg-output (dag-draw-render-svg graph)))
          (expect svg-output :to-match "<path"))))

  (describe "utility functions"
    (it "should handle file saving"
      ;; Mock file saving since we can't write to arbitrary locations
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'test)
        
        ;; Test format detection from filename
        (expect (dag-draw-render-graph graph 'svg) :to-match "<svg")
        (expect (dag-draw-render-graph graph 'ascii) :to-be-truthy)
        (expect (dag-draw-render-graph graph 'dot) :to-match "digraph")))
    
    (it "should handle buffer display"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'test "Test Node")
        
        ;; Test buffer creation
        (let ((buffer (dag-draw-display-in-buffer graph "*Test Graph*" 'ascii)))
          (expect (bufferp buffer) :to-be-truthy)
          (with-current-buffer buffer
            (expect (buffer-string) :to-match "Test Node"))
          (kill-buffer buffer)))))

  (describe "error handling and edge cases"
    (it "should handle empty graphs"
      (let ((graph (dag-draw-create-graph)))
        (expect (dag-draw-render-svg graph) :to-match "<svg")
        (expect (dag-draw-render-ascii graph) :to-be-truthy)
        (expect (dag-draw-render-dot graph) :to-match "digraph")))
    
    (it "should handle nodes without coordinates"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'test)
        ;; Don't set coordinates - should use defaults
        
        (expect (dag-draw-render-svg graph) :to-match "<svg")
        (expect (dag-draw-render-ascii graph) :to-be-truthy)))
    
    (it "should handle graphs with only nodes"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a "Node A")
        (dag-draw-add-node graph 'b "Node B")
        ;; No edges
        
        (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'a)) 50)
        (setf (dag-draw-node-y-coord (dag-draw-get-node graph 'a)) 50)
        (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'b)) 150)
        (setf (dag-draw-node-y-coord (dag-draw-get-node graph 'b)) 150)
        
        (let ((svg-output (dag-draw-render-svg graph)))
          (expect svg-output :to-match "Node A")
          (expect svg-output :to-match "Node B")
          ;; Should not have any paths (edges)
          (expect svg-output :not :to-match "<path"))))
    
    (it "should handle self-loops"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a "Node A")
        (dag-draw-add-edge graph 'a 'a)  ; Self-loop
        
        (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'a)) 100)
        (setf (dag-draw-node-y-coord (dag-draw-get-node graph 'a)) 100)
        
        (dag-draw-generate-splines graph)  ; Generate splines for self-loop
        
        (let ((svg-output (dag-draw-render-svg graph)))
          (expect svg-output :to-match "Node A")))))

;;; dag-draw-render-test.el ends here