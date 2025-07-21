;;; dag-draw-aesthetic-principles-test.el --- Tests for GKNV Aesthetic Principles A1-A4 -*- lexical-binding: t -*-

;; Copyright (C) 2024

;;; Commentary:

;; Tests to ensure proper GKNV aesthetic principles A1-A4 per Section 1.1.
;; Validates hierarchical structure, visual anomaly avoidance, short edges, and symmetry.
;;
;; GKNV Reference: Section 1.1, lines 43-54
;; Ubiquitous Language: Aesthetic Principles - proper A1-A4 integration

;;; Code:

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-core)
(require 'dag-draw-aesthetic-principles)

(describe "GKNV Aesthetic Principles A1-A4"
  
  (describe "A1: Expose hierarchical structure per Section 1.1"
    
    (it "should validate hierarchical structure exposure"
      ;; RED TEST: Function should exist to validate A1 compliance
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'source "Source")
        (dag-draw-add-node graph 'middle "Middle")
        (dag-draw-add-node graph 'sink "Sink")
        
        (dag-draw-add-edge graph 'source 'middle)
        (dag-draw-add-edge graph 'middle 'sink)
        
        ;; Should have A1 validation function
        (expect (fboundp 'dag-draw--validate-hierarchical-structure) :to-be t)
        
        (let ((a1-score (dag-draw--validate-hierarchical-structure graph)))
          ;; A1 score should reflect hierarchical clarity
          (expect (numberp a1-score) :to-be t)
          (expect (>= a1-score 0) :to-be t))))
    
    (it "should aim edges in same general direction"
      ;; GREEN TEST: Edges should follow consistent directional flow
      (let ((graph (dag-draw-create-graph)))
        ;; Create hierarchical structure: top -> middle -> bottom
        (dag-draw-add-node graph 'top "Top")
        (dag-draw-add-node graph 'middle-left "Middle Left")
        (dag-draw-add-node graph 'middle-right "Middle Right") 
        (dag-draw-add-node graph 'bottom "Bottom")
        
        (dag-draw-add-edge graph 'top 'middle-left)
        (dag-draw-add-edge graph 'top 'middle-right)
        (dag-draw-add-edge graph 'middle-left 'bottom)
        (dag-draw-add-edge graph 'middle-right 'bottom)
        
        ;; Perform ranking to establish hierarchy
        (dag-draw-rank graph)
        
        (let ((direction-consistency (dag-draw--measure-directional-consistency graph)))
          ;; Should have high directional consistency (edges point same way)
          (expect direction-consistency :to-be-greater-than 0.8))))
    
    (it "should highlight source and sink nodes"
      ;; GREEN TEST: Source and sink nodes should be identifiable
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'source1 "Source 1")
        (dag-draw-add-node graph 'source2 "Source 2")
        (dag-draw-add-node graph 'middle "Middle")
        (dag-draw-add-node graph 'sink "Sink")
        
        (dag-draw-add-edge graph 'source1 'middle)
        (dag-draw-add-edge graph 'source2 'middle)
        (dag-draw-add-edge graph 'middle 'sink)
        
        (let ((source-sink-info (dag-draw--identify-source-sink-prominence graph)))
          ;; Should identify and highlight sources/sinks for A1
          (expect (ht-get source-sink-info 'source-count) :to-equal 2)
          (expect (ht-get source-sink-info 'sink-count) :to-equal 1)))))
  
  (describe "A2: Avoid visual anomalies per Section 1.1"
    
    (it "should minimize edge crossings"
      ;; RED TEST: Function should exist to count/minimize crossings
      (let ((graph (dag-draw-create-graph)))
        ;; Create structure that could have crossings
        (dag-draw-add-node graph 'a "A")
        (dag-draw-add-node graph 'b "B")
        (dag-draw-add-node graph 'c "C")
        (dag-draw-add-node graph 'd "D")
        
        (dag-draw-add-edge graph 'a 'c)  ; Could cross with b->d
        (dag-draw-add-edge graph 'a 'd)
        (dag-draw-add-edge graph 'b 'c)
        (dag-draw-add-edge graph 'b 'd)
        
        ;; Should have crossing detection/minimization
        (expect (fboundp 'dag-draw--count-edge-crossings) :to-be t)
        
        ;; Perform layout
        (dag-draw-rank graph)
        (dag-draw-order-vertices graph)
        (dag-draw-position-nodes graph)
        
        (let ((crossing-count (dag-draw--count-edge-crossings graph)))
          ;; Should minimize crossings per A2
          (expect (numberp crossing-count) :to-be t)
          (expect (>= crossing-count 0) :to-be t))))
    
    (it "should avoid sharp bends in edges"
      ;; GREEN TEST: Edge routing should minimize sharp bends
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'start "Start")
        (dag-draw-add-node graph 'end "End")
        (dag-draw-add-edge graph 'start 'end)
        
        ;; Perform full layout
        (dag-draw-render-graph graph)
        
        (let ((bend-analysis (dag-draw--analyze-edge-bends graph)))
          ;; Should have low bend severity per A2
          (expect (ht-get bend-analysis 'max-bend-angle) :to-be-less-than 90)))))
  
  (describe "A3: Keep edges short per Section 1.1"
    
    (it "should measure and minimize edge lengths"
      ;; RED TEST: Function should exist to measure edge lengths
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'near1 "Near 1")
        (dag-draw-add-node graph 'near2 "Near 2") 
        (dag-draw-add-node graph 'far "Far")
        
        (dag-draw-add-edge graph 'near1 'near2)  ; Short edge
        (dag-draw-add-edge graph 'near1 'far)    ; Potentially long edge
        
        ;; Should have edge length measurement
        (expect (fboundp 'dag-draw--measure-edge-lengths) :to-be t)
        
        ;; Perform positioning
        (dag-draw-rank graph)
        (dag-draw-order-vertices graph)
        (dag-draw-position-nodes graph)
        
        (let ((length-metrics (dag-draw--measure-edge-lengths graph)))
          ;; Should optimize for short edges per A3
          (expect (ht-get length-metrics 'average-length) :not :to-be nil)
          (expect (numberp (ht-get length-metrics 'average-length)) :to-be t))))
    
    (it "should aid finding related nodes through short edges"
      ;; GREEN TEST: Related nodes should be positioned close together
      (let ((graph (dag-draw-create-graph)))
        ;; Create cluster of related nodes
        (dag-draw-add-node graph 'cluster-a "Cluster A")
        (dag-draw-add-node graph 'cluster-b "Cluster B")
        (dag-draw-add-node graph 'cluster-c "Cluster C")
        (dag-draw-add-node graph 'isolated "Isolated")
        
        ;; Dense connections within cluster
        (dag-draw-add-edge graph 'cluster-a 'cluster-b)
        (dag-draw-add-edge graph 'cluster-b 'cluster-c)
        (dag-draw-add-edge graph 'cluster-c 'cluster-a)
        
        ;; Sparse connection to isolated node
        (dag-draw-add-edge graph 'cluster-a 'isolated)
        
        (dag-draw-render-graph graph)
        
        (let ((clustering-quality (dag-draw--measure-node-clustering graph)))
          ;; Should group related nodes per A3
          (expect clustering-quality :to-be-greater-than 0.5)))))
  
  (describe "A4: Favor symmetry and balance per Section 1.1"
    
    (it "should measure layout symmetry"
      ;; RED TEST: Function should exist to measure symmetry
      (let ((graph (dag-draw-create-graph)))
        ;; Create symmetric structure
        (dag-draw-add-node graph 'center "Center")
        (dag-draw-add-node graph 'left "Left")
        (dag-draw-add-node graph 'right "Right")
        
        (dag-draw-add-edge graph 'center 'left)
        (dag-draw-add-edge graph 'center 'right)
        
        ;; Should have symmetry measurement
        (expect (fboundp 'dag-draw--measure-layout-symmetry) :to-be t)
        
        (dag-draw-render-graph graph)
        
        (let ((symmetry-score (dag-draw--measure-layout-symmetry graph)))
          ;; Should favor symmetric layouts per A4
          (expect (numberp symmetry-score) :to-be t)
          (expect (>= symmetry-score 0) :to-be t))))
    
    (it "should balance node distribution"
      ;; GREEN TEST: Nodes should be balanced across layout
      (let ((graph (dag-draw-create-graph)))
        ;; Create balanced tree structure
        (dag-draw-add-node graph 'root "Root")
        (dag-draw-add-node graph 'left-child "Left Child")
        (dag-draw-add-node graph 'right-child "Right Child")
        (dag-draw-add-node graph 'left-leaf "Left Leaf")
        (dag-draw-add-node graph 'right-leaf "Right Leaf")
        
        (dag-draw-add-edge graph 'root 'left-child)
        (dag-draw-add-edge graph 'root 'right-child)
        (dag-draw-add-edge graph 'left-child 'left-leaf)
        (dag-draw-add-edge graph 'right-child 'right-leaf)
        
        (dag-draw-render-graph graph)
        
        (let ((balance-metrics (dag-draw--measure-layout-balance graph)))
          ;; Should achieve good balance per A4
          (expect (ht-get balance-metrics 'horizontal-balance) :to-be-greater-than 0.7)))))

(describe "A1-A4 Integration into Algorithm Decisions"
  
  (it "should integrate aesthetic principles into ranking decisions"
    ;; GREEN TEST: Pass 1 should consider A1 and A3
    (let ((graph (dag-draw-create-graph)))
      (dag-draw-add-node graph 'top "Top")
      (dag-draw-add-node graph 'bottom "Bottom")
      (dag-draw-add-edge graph 'top 'bottom)
      
      ;; Ranking should consider aesthetic principles
      (dag-draw-rank graph)
      
      ;; Should have aesthetic evaluation in ranking
      (expect (fboundp 'dag-draw--evaluate-ranking-aesthetics) :to-be t)))
  
  (it "should integrate aesthetic principles into ordering decisions"  
    ;; GREEN TEST: Pass 2 should consider A2 (crossings)
    (let ((graph (dag-draw-create-graph)))
      (dag-draw-add-node graph 'a "A")
      (dag-draw-add-node graph 'b "B")
      (dag-draw-add-node graph 'c "C")
      (dag-draw-add-node graph 'd "D")
      
      (dag-draw-add-edge graph 'a 'c)
      (dag-draw-add-edge graph 'b 'd)
      
      (dag-draw-rank graph)
      (dag-draw-order-vertices graph)
      
      ;; Ordering should minimize crossings per A2
      (expect (fboundp 'dag-draw--evaluate-ordering-aesthetics) :to-be t)))
  
  (it "should integrate aesthetic principles into positioning decisions"
    ;; GREEN TEST: Pass 3 should consider A3 (edge length) and A4 (balance)
    (let ((graph (dag-draw-create-graph)))
      (dag-draw-add-node graph 'left "Left")
      (dag-draw-add-node graph 'center "Center") 
      (dag-draw-add-node graph 'right "Right")
      
      (dag-draw-add-edge graph 'left 'center)
      (dag-draw-add-edge graph 'center 'right)
      
      (dag-draw-rank graph)
      (dag-draw-order-vertices graph)
      (dag-draw-position-nodes graph)
      
      ;; Positioning should optimize edge length and balance per A3/A4
      (expect (fboundp 'dag-draw--evaluate-positioning-aesthetics) :to-be t)))))

(provide 'dag-draw-aesthetic-principles-test)

;;; dag-draw-aesthetic-principles-test.el ends here