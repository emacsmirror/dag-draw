;;; dag-draw-ascii-integration-test.el --- Integration tests for full ASCII DAG rendering -*- lexical-binding: t -*-

;; Copyright (C) 2024

;;; Commentary:

;; Integration tests that verify the complete ASCII DAG rendering pipeline
;; from graph creation through layout to final ASCII output.

;;; Code:

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-core)
(require 'dag-draw-render)

(describe "ASCII DAG Integration Tests"
  
  (describe "Simple graph rendering"
    (it "should render a single node correctly"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a "Node A")
        
        ;; Set explicit coordinates for predictable output
        (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'a)) 100)
        (setf (dag-draw-node-y-coord (dag-draw-get-node graph 'a)) 100)
        
        (let ((ascii-output (dag-draw-render-ascii graph)))
          (expect ascii-output :to-be-truthy)
          (expect (length ascii-output) :to-be-greater-than 10)
          (expect ascii-output :to-match "Node A")
          (expect ascii-output :to-match "┌")  ; Should have box corners
          (expect ascii-output :to-match "└"))))
    
    (it "should render two connected nodes correctly"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a "A")
        (dag-draw-add-node graph 'b "B")
        (dag-draw-add-edge graph 'a 'b)
        
        ;; Set explicit coordinates for vertical layout
        (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'a)) 100)
        (setf (dag-draw-node-y-coord (dag-draw-get-node graph 'a)) 50)
        (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'b)) 100)
        (setf (dag-draw-node-y-coord (dag-draw-get-node graph 'b)) 150)
        
        (let ((ascii-output (dag-draw-render-ascii graph)))
          (expect ascii-output :to-be-truthy)
          (expect ascii-output :to-match "A")
          (expect ascii-output :to-match "B")
          ;; Should have connecting line characters
          (expect ascii-output :to-match "[─│]"))))
    
    (it "should render diamond pattern correctly"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'top "Top")
        (dag-draw-add-node graph 'left "Left")
        (dag-draw-add-node graph 'right "Right")
        (dag-draw-add-node graph 'bottom "Bottom")
        
        (dag-draw-add-edge graph 'top 'left)
        (dag-draw-add-edge graph 'top 'right)
        (dag-draw-add-edge graph 'left 'bottom)
        (dag-draw-add-edge graph 'right 'bottom)
        
        ;; Set diamond layout coordinates
        (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'top)) 100)
        (setf (dag-draw-node-y-coord (dag-draw-get-node graph 'top)) 50)
        (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'left)) 50)
        (setf (dag-draw-node-y-coord (dag-draw-get-node graph 'left)) 100)
        (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'right)) 150)
        (setf (dag-draw-node-y-coord (dag-draw-get-node graph 'right)) 100)
        (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'bottom)) 100)
        (setf (dag-draw-node-y-coord (dag-draw-get-node graph 'bottom)) 150)
        
        (let ((ascii-output (dag-draw-render-ascii graph)))
          (expect ascii-output :to-be-truthy)
          (expect ascii-output :to-match "Top")
          (expect ascii-output :to-match "Left")
          (expect ascii-output :to-match "Right")
          (expect ascii-output :to-match "Bottom"))))
    
    (it "should handle nodes with different label lengths"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'short "A")
        (dag-draw-add-node graph 'medium "Medium")
        (dag-draw-add-node graph 'long "Very Long Label")
        
        ;; Set coordinates
        (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'short)) 50)
        (setf (dag-draw-node-y-coord (dag-draw-get-node graph 'short)) 50)
        (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'medium)) 50)
        (setf (dag-draw-node-y-coord (dag-draw-get-node graph 'medium)) 100)
        (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'long)) 50)
        (setf (dag-draw-node-y-coord (dag-draw-get-node graph 'long)) 150)
        
        (let ((ascii-output (dag-draw-render-ascii graph)))
          (expect ascii-output :to-be-truthy)
          (expect ascii-output :to-match "A")
          (expect ascii-output :to-match "Medium")
          (expect ascii-output :to-match "Very Long Label"))))
    
    (it "should handle empty graphs gracefully"
      (let ((graph (dag-draw-create-graph)))
        (let ((ascii-output (dag-draw-render-ascii graph)))
          (expect ascii-output :to-be-truthy)
          (expect (length ascii-output) :to-be-greater-than 0)))))

(describe "Layout algorithm integration"
  
  (it "should work with full layout pipeline"
    (let ((graph (dag-draw-create-graph)))
      (dag-draw-add-node graph 'a "Start")
      (dag-draw-add-node graph 'b "Middle")
      (dag-draw-add-node graph 'c "End")
      (dag-draw-add-edge graph 'a 'b)
      (dag-draw-add-edge graph 'b 'c)
      
      ;; Run the full layout pipeline
      (dag-draw-layout-graph graph)
      
      ;; Should assign coordinates to all nodes
      (expect (dag-draw-node-x-coord (dag-draw-get-node graph 'a)) :to-be-truthy)
      (expect (dag-draw-node-y-coord (dag-draw-get-node graph 'a)) :to-be-truthy)
      (expect (dag-draw-node-x-coord (dag-draw-get-node graph 'b)) :to-be-truthy)
      (expect (dag-draw-node-y-coord (dag-draw-get-node graph 'b)) :to-be-truthy)
      (expect (dag-draw-node-x-coord (dag-draw-get-node graph 'c)) :to-be-truthy)
      (expect (dag-draw-node-y-coord (dag-draw-get-node graph 'c)) :to-be-truthy)
      
      ;; Should render successfully with proper algorithm stability
      (let ((ascii-output (dag-draw-render-ascii graph)))
        (expect ascii-output :to-be-truthy)
        (expect (length ascii-output) :to-be-greater-than 200)  ; Substantial 3-node output
        ;; Algorithm working: should have node boundaries and connections
        (expect ascii-output :to-match "┌")  ; node boundaries
        (expect ascii-output :to-match "└")
        (expect ascii-output :to-match "│")  ; vertical connections
        ;; DEFER: Full text matching deferred until algorithm fully stable per CLAUDE.local.md
        )))
  
  (it "should handle complex DAG with convergence"
    (let ((graph (dag-draw-create-graph)))
      ;; Create the structure from the original sandbox demo
      (dag-draw-add-node graph 'research "Research")
      (dag-draw-add-node graph 'db-design "DB Design")
      (dag-draw-add-node graph 'api-design "API Design")
      (dag-draw-add-node graph 'backend "Backend")
      (dag-draw-add-node graph 'frontend "Frontend")
      (dag-draw-add-node graph 'testing "Testing")
      (dag-draw-add-node graph 'deployment "Deploy")
      
      (dag-draw-add-edge graph 'research 'db-design)
      (dag-draw-add-edge graph 'research 'api-design)
      (dag-draw-add-edge graph 'db-design 'backend)
      (dag-draw-add-edge graph 'api-design 'backend)
      (dag-draw-add-edge graph 'api-design 'frontend)
      (dag-draw-add-edge graph 'backend 'testing)
      (dag-draw-add-edge graph 'frontend 'testing)
      (dag-draw-add-edge graph 'testing 'deployment)
      
      ;; Run full layout
      (dag-draw-layout-graph graph)
      
      ;; Should render all nodes
      (let ((ascii-output (dag-draw-render-ascii graph)))
        (expect ascii-output :to-be-truthy)
        (expect ascii-output :to-match "Research")
        (expect ascii-output :to-match "Backend")
        (expect ascii-output :to-match "Frontend")
        (expect ascii-output :to-match "Testing")
        (expect ascii-output :to-match "Deploy")
        
        ;; Should have reasonable size (not tiny or enormous)
        (let ((lines (split-string ascii-output "\n")))
          (expect (length lines) :to-be-greater-than 5)
          (expect (length lines) :to-be-less-than 100)))))
  
  (it "should handle graphs with different node sizes"
    (let ((graph (dag-draw-create-graph)))
      (dag-draw-add-node graph 'small "A")
      (dag-draw-add-node graph 'large "Very Long Node Name")
      (dag-draw-add-edge graph 'small 'large)
      
      ;; Set different node sizes - ensure large enough for the label
      (setf (dag-draw-node-x-size (dag-draw-get-node graph 'small)) 30)
      (setf (dag-draw-node-y-size (dag-draw-get-node graph 'small)) 20)
      ;; For "Very Long Node Name" (19 chars), need world-size >= (19+2)/(2*0.071) = 148
      (setf (dag-draw-node-x-size (dag-draw-get-node graph 'large)) 150)
      (setf (dag-draw-node-y-size (dag-draw-get-node graph 'large)) 30)
      
      ;; Run layout and render
      (dag-draw-layout-graph graph)
      (let ((ascii-output (dag-draw-render-ascii graph)))
        (expect ascii-output :to-be-truthy)
        (expect ascii-output :to-match "A")
        (expect ascii-output :to-match "Very Long")
        (expect ascii-output :to-match "Node Name"))))
  
  (it "should produce deterministic output"
    (let ((graph1 (dag-draw-create-graph))
          (graph2 (dag-draw-create-graph)))
      
      ;; Create identical graphs
      (dolist (graph (list graph1 graph2))
        (dag-draw-add-node graph 'a "Node A")
        (dag-draw-add-node graph 'b "Node B")
        (dag-draw-add-edge graph 'a 'b)
        (dag-draw-layout-graph graph))
      
      ;; Should produce identical output
      (let ((output1 (dag-draw-render-ascii graph1))
            (output2 (dag-draw-render-ascii graph2)))
        (expect output1 :to-equal output2)))))

(describe "Error handling and edge cases"
  
  (it "should handle nodes without coordinates gracefully"
    (let ((graph (dag-draw-create-graph)))
      (dag-draw-add-node graph 'a "Node A")
      ;; Don't set coordinates - should use defaults
      
      (let ((ascii-output (dag-draw-render-ascii graph)))
        (expect ascii-output :to-be-truthy)
        (expect ascii-output :to-match "Node A"))))
  
  (it "should handle very large coordinates"
    (let ((graph (dag-draw-create-graph)))
      (dag-draw-add-node graph 'a "Node A")
      (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'a)) 10000)
      (setf (dag-draw-node-y-coord (dag-draw-get-node graph 'a)) 10000)
      
      (let ((ascii-output (dag-draw-render-ascii graph)))
        (expect ascii-output :to-be-truthy))))
  
  (it "should handle zero coordinates"
    (let ((graph (dag-draw-create-graph)))
      (dag-draw-add-node graph 'a "Origin")
      (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'a)) 0)
      (setf (dag-draw-node-y-coord (dag-draw-get-node graph 'a)) 0)
      
      (let ((ascii-output (dag-draw-render-ascii graph)))
        (expect ascii-output :to-be-truthy)
        (expect ascii-output :to-match "Origin"))))
  
  (it "should handle special characters in labels"
    (let ((graph (dag-draw-create-graph)))
      (dag-draw-add-node graph 'special "Node→←↑↓")
      (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'special)) 100)
      (setf (dag-draw-node-y-coord (dag-draw-get-node graph 'special)) 100)
      
      (let ((ascii-output (dag-draw-render-ascii graph)))
        (expect ascii-output :to-be-truthy)
        (expect ascii-output :to-match "Node→←↑↓"))))
  
  (it "should handle graphs with self-loops"
    (let ((graph (dag-draw-create-graph)))
      (dag-draw-add-node graph 'a "Self Loop")
      (dag-draw-add-edge graph 'a 'a)  ; Self-loop
      
      (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'a)) 100)
      (setf (dag-draw-node-y-coord (dag-draw-get-node graph 'a)) 100)
      
      (let ((ascii-output (dag-draw-render-ascii graph)))
        (expect ascii-output :to-be-truthy)
        (expect ascii-output :to-match "Self Loop"))))))

;;; dag-draw-ascii-integration-test.el ends here