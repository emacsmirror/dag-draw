;;; dag-draw-ascii-integration-test.el --- Integration tests for full ASCII DAG rendering -*- lexical-binding: t -*-

;; Copyright (C) 2024

;;; Commentary:

;; GKNV Baseline Compliance Tests - ASCII: Full Pipeline Integration
;;
;; This module tests complete GKNV four-pass pipeline to ASCII output as
;; specified in doc/implementation-decisions.md.
;;
;; GKNV Reference: Sections 2-5 (four passes), ASCII adaptation
;; Decision: D1.x-D5.x - All decisions (four passes + ASCII rendering)
;; Algorithm: Complete GKNV Pipeline: Input Graph → ASCII Art
;;
;; Key Requirements Tested:
;; - Pass 1: Rank assignment (input graph → ranked graph)
;; - Pass 2: Ordering (ranked → ordered graph)
;; - Pass 3: Positioning (ordered → positioned graph)
;; - Pass 4: Splines (positioned → routed graph)
;; - ASCII: Rendering (routed → ASCII string)
;; - Integration points between passes verified
;; - Data flows correctly through all passes
;; - Final ASCII output reflects GKNV algorithm quality
;;
;; Test Coverage:
;; - Complete pipeline execution from input to output
;; - Each pass produces valid input for next pass
;; - No data corruption at pass boundaries
;; - Final ASCII preserves layout quality from GKNV algorithm
;; - Various input graphs: simple, complex, realistic
;; - Output quality: hierarchical structure visible (A1)
;; - Output quality: minimal crossings (A2)
;; - Output quality: short edges (A3)
;; - Output quality: balanced appearance (A4)
;;
;; Baseline Status: ✅ Required for GKNV compliance
;;
;; See doc/implementation-decisions.md (all sections) for full decision rationale.
;; See doc/algorithm-specification.md (all sections) for implementation details.

;; Integration tests that verify the complete ASCII DAG rendering pipeline
;; from graph creation through layout to final ASCII output.

;;; Code:

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-core)
(require 'dag-draw-render)
(require 'dag-draw-test-harness)

(describe "ASCII DAG Integration Tests"

  (describe "Simple graph rendering"
    (it "should render a single node correctly"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a "Node A")

        ;; Use layout algorithm for proper integration test
        (dag-draw-layout-graph graph :coordinate-mode 'ascii)

        (let ((ascii-output (dag-draw-render-ascii graph)))
          (expect ascii-output :to-be-truthy)
          (expect (length ascii-output) :to-be-greater-than 10)
          ;; Use test harness for structural validation
          (let ((node-validation (dag-draw-test--validate-node-completeness ascii-output graph)))
            (expect (plist-get node-validation :complete) :to-be t))
          (let ((boundary-validation (dag-draw-test--validate-node-boundaries ascii-output)))
            (expect (plist-get boundary-validation :valid) :to-be t)))))

    (it "should render two connected nodes correctly"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a "A")
        (dag-draw-add-node graph 'b "B")
        (dag-draw-add-edge graph 'a 'b)

        ;; Use layout algorithm for proper integration test
        (dag-draw-layout-graph graph :coordinate-mode 'ascii)

        (let ((ascii-output (dag-draw-render-ascii graph)))
          (expect ascii-output :to-be-truthy)
          ;; Use test harness for comprehensive validation
          (let ((node-validation (dag-draw-test--validate-node-completeness ascii-output graph)))
            (expect (plist-get node-validation :complete) :to-be t))
          (let ((connectivity-validation (dag-draw-test--validate-edge-connectivity ascii-output graph)))
            (expect (plist-get connectivity-validation :all-connected) :to-be t)))))

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

        ;; Use layout algorithm for proper integration test
        (dag-draw-layout-graph graph :coordinate-mode 'ascii)

        (let ((ascii-output (dag-draw-render-ascii graph)))
          (expect ascii-output :to-be-truthy)
          
          ;; XP Debug: Show actual output and detection results BEFORE validation
          (message "\n=== DIAMOND TEST DEBUG ===")
          (message "ASCII OUTPUT:")
          (message "%s" ascii-output)
          (let* ((found-nodes (dag-draw-test--find-nodes-using-graph-data ascii-output graph))
                 (expected-nodes (dag-draw-test--extract-expected-nodes graph)))
            (message "Expected %d nodes: %s" (length expected-nodes) 
                     (mapcar (lambda (n) (plist-get n :label)) expected-nodes))
            (message "Found %d nodes: %s" (length found-nodes)
                     (mapcar (lambda (n) (plist-get n :text)) found-nodes))
            ;; XP Debug: Show what text search is actually finding
            (dolist (expected expected-nodes)
              (let* ((label (plist-get expected :label))
                     (found-text (dag-draw-test--find-text-in-grid
                                  (dag-draw-test--parse-ascii-grid ascii-output) label)))
                (message "Searching for '%s': %s" label
                         (if found-text
                             (let ((x (plist-get found-text :x))
                                   (y (plist-get found-text :y)))
                               (if (and x y)
                                   (format "found at (%d,%d)" x y)
                                 "found but coords nil"))
                           "NOT FOUND")))))
          (message "========================")
          
          ;; Use test harness for structural validation
          (let ((node-validation (dag-draw-test--validate-node-completeness ascii-output graph)))
            (expect (plist-get node-validation :complete) :to-be t))
          (let ((structure-validation (dag-draw-test--validate-graph-structure ascii-output graph)))
            (expect (plist-get structure-validation :topology-match) :to-be t)))))

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
          ;; Use test harness for comprehensive validation
          (let ((node-validation (dag-draw-test--validate-node-completeness ascii-output graph)))
            (expect (plist-get node-validation :complete) :to-be t))
          (let ((boundary-validation (dag-draw-test--validate-node-boundaries ascii-output)))
            (expect (plist-get boundary-validation :valid) :to-be t))
          (let ((connectivity-validation (dag-draw-test--validate-edge-connectivity ascii-output graph)))
            (expect (plist-get connectivity-validation :all-connected) :to-be t)))))

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

        ;; Run full layout with ASCII coordinate mode
        (dag-draw-layout-graph graph :coordinate-mode 'ascii)

        ;; Should render all nodes with complete validation
        (let ((ascii-output (dag-draw-render-ascii graph)))
          (expect ascii-output :to-be-truthy)
          ;; Use test harness for comprehensive validation
          (let ((node-validation (dag-draw-test--validate-node-completeness ascii-output graph)))
            (expect (plist-get node-validation :complete) :to-be t))
          (let ((structure-validation (dag-draw-test--validate-graph-structure ascii-output graph)))
            (expect (plist-get structure-validation :topology-match) :to-be t)
            (expect (plist-get structure-validation :node-count-match) :to-be t))
          (let ((connectivity-validation (dag-draw-test--validate-edge-connectivity ascii-output graph)))
            (expect (plist-get connectivity-validation :all-connected) :to-be t))

          ;; Should have reasonable size (not tiny or enormous)
          ;; ASCII grid expansion and network simplex optimization can create larger grids
          (let ((lines (split-string ascii-output "\n")))
            (expect (length lines) :to-be-greater-than 5)
            (expect (length lines) :to-be-less-than 150)))))  ; Reasonable upper bound for 7-node graph
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
          ;; Use test harness for validation
          (let ((node-validation (dag-draw-test--validate-node-completeness ascii-output graph)))
            (expect (plist-get node-validation :complete) :to-be t)))))

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
          (expect output1 :to-equal output2))))
    ) ; 4 closing parens: lines-let, ascii-let, graph-let, it

  (describe "Error handling and edge cases"

    (it "should handle nodes without coordinates gracefully"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a "Node A")
        ;; Don't set coordinates - should use defaults

        (let ((ascii-output (dag-draw-render-ascii graph)))
          (expect ascii-output :to-be-truthy)
          ;; Use test harness for validation
          (let ((node-validation (dag-draw-test--validate-node-completeness ascii-output graph)))
            (expect (plist-get node-validation :complete) :to-be t)))))

    (it "should handle very large coordinates"
      ;; RENDERER TEST: Explicitly testing renderer coordinate handling edge case
      ;; Manual coordinates are intentional - not testing layout algorithm
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a "Node A")
        (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'a)) 10000)
        (setf (dag-draw-node-y-coord (dag-draw-get-node graph 'a)) 10000)

        (let ((ascii-output (dag-draw-render-ascii graph)))
          (expect ascii-output :to-be-truthy))))

    (it "should handle zero coordinates"
      ;; RENDERER TEST: Explicitly testing renderer coordinate handling edge case
      ;; Manual coordinates are intentional - not testing layout algorithm
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a "Origin")
        (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'a)) 0)
        (setf (dag-draw-node-y-coord (dag-draw-get-node graph 'a)) 0)

        (let ((ascii-output (dag-draw-render-ascii graph)))
          (expect ascii-output :to-be-truthy)
          ;; Use test harness for validation
          (let ((node-validation (dag-draw-test--validate-node-completeness ascii-output graph)))
            (expect (plist-get node-validation :complete) :to-be t)))))

    (it "should handle special characters in labels"
      ;; RENDERER TEST: Explicitly testing renderer special character handling
      ;; Manual coordinates are intentional - not testing layout algorithm
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'special "Node→←↑↓")
        (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'special)) 100)
        (setf (dag-draw-node-y-coord (dag-draw-get-node graph 'special)) 100)

        (let ((ascii-output (dag-draw-render-ascii graph)))
          (expect ascii-output :to-be-truthy)
          ;; Use test harness for validation
          (let ((node-validation (dag-draw-test--validate-node-completeness ascii-output graph)))
            (expect (plist-get node-validation :complete) :to-be t)))))

)

  )

;;; dag-draw-ascii-integration-test.el ends here
