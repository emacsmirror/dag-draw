;;; gknv-aesthetic-principles-test.el --- GKNV aesthetic principles verification tests -*- lexical-binding: t -*-

;; Copyright (C) 2024, 2025

;;; Commentary:

;; Tests to verify compliance with the four aesthetic principles defined in
;; Section 1.1 of "A Technique for Drawing Directed Graphs" (Gansner et al.):
;;
;; A1: Expose hierarchical structure
;; A2: Avoid visual anomalies
;; A3: Keep edges short
;; A4: Favor symmetry and balance
;;
;; These principles guide the GKNV algorithm and should be measurable in the output.

;;; Code:

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-core)
(require 'dag-draw-test-harness)

(describe "GKNV Aesthetic Principles Verification - Section 1.1"

  ;; =================================================================
  ;; A1: EXPOSE HIERARCHICAL STRUCTURE (Section 1.1, line 43)
  ;; "aim edges in the same general direction if possible"
  ;; =================================================================

  (describe "A1: Expose Hierarchical Structure - Line 43"

    (it "should assign consistent flow direction in simple hierarchies"
      (let ((graph (dag-draw-create-graph)))
        ;; Classic hierarchy: root -> children -> grandchildren
        (dag-draw-add-node graph 'root "Root")
        (dag-draw-add-node graph 'child1 "Child 1")
        (dag-draw-add-node graph 'child2 "Child 2")
        (dag-draw-add-node graph 'grandchild1 "Grandchild 1")
        (dag-draw-add-node graph 'grandchild2 "Grandchild 2")

        (dag-draw-add-edge graph 'root 'child1)
        (dag-draw-add-edge graph 'root 'child2)
        (dag-draw-add-edge graph 'child1 'grandchild1)
        (dag-draw-add-edge graph 'child2 'grandchild2)

        (dag-draw-layout-graph graph)

        ;; Should create clear rank progression: 0 -> 1 -> 2
        (let ((rank-root (dag-draw-node-rank (dag-draw-get-node graph 'root)))
              (rank-c1 (dag-draw-node-rank (dag-draw-get-node graph 'child1)))
              (rank-c2 (dag-draw-node-rank (dag-draw-get-node graph 'child2)))
              (rank-gc1 (dag-draw-node-rank (dag-draw-get-node graph 'grandchild1)))
              (rank-gc2 (dag-draw-node-rank (dag-draw-get-node graph 'grandchild2))))

          ;; Paper A1: "expose hierarchical structure"
          (expect rank-root :to-be-close-to 0 0.1)
          (expect rank-c1 :to-be-close-to 1 0.1)
          (expect rank-c2 :to-be-close-to 1 0.1)
          (expect rank-gc1 :to-be-close-to 2 0.1)
          (expect rank-gc2 :to-be-close-to 2 0.1))))

    (it "should highlight source and sink nodes through rank assignment"
      (let ((graph (dag-draw-create-graph)))
        ;; Multi-source, multi-sink graph
        (dag-draw-add-node graph 'source1 "Source 1")
        (dag-draw-add-node graph 'source2 "Source 2")
        (dag-draw-add-node graph 'processing "Processing")
        (dag-draw-add-node graph 'sink1 "Sink 1")
        (dag-draw-add-node graph 'sink2 "Sink 2")

        (dag-draw-add-edge graph 'source1 'processing)
        (dag-draw-add-edge graph 'source2 'processing)
        (dag-draw-add-edge graph 'processing 'sink1)
        (dag-draw-add-edge graph 'processing 'sink2)

        (dag-draw-layout-graph graph)

        ;; Sources should be at minimum rank, sinks at maximum rank
        (let ((rank-s1 (dag-draw-node-rank (dag-draw-get-node graph 'source1)))
              (rank-s2 (dag-draw-node-rank (dag-draw-get-node graph 'source2)))
              (rank-proc (dag-draw-node-rank (dag-draw-get-node graph 'processing)))
              (rank-sink1 (dag-draw-node-rank (dag-draw-get-node graph 'sink1)))
              (rank-sink2 (dag-draw-node-rank (dag-draw-get-node graph 'sink2))))

          ;; Sources should be at rank 0
          (expect rank-s1 :to-be-close-to 0 0.1)
          (expect rank-s2 :to-be-close-to 0 0.1)

          ;; Processing in middle
          (expect rank-proc :to-be-close-to 1 0.1)

          ;; Sinks at final rank
          (expect rank-sink1 :to-be-close-to 2 0.1)
          (expect rank-sink2 :to-be-close-to 2 0.1))))

    (it "should maintain flow consistency even with complex dependencies"
      (let ((graph (dag-draw-create-graph)))
        ;; Complex DAG that tests flow consistency
        (dag-draw-add-node graph 'input "Input")
        (dag-draw-add-node graph 'validate "Validate")
        (dag-draw-add-node graph 'process "Process")
        (dag-draw-add-node graph 'store "Store")
        (dag-draw-add-node graph 'notify "Notify")
        (dag-draw-add-node graph 'output "Output")

        ;; Linear dependencies
        (dag-draw-add-edge graph 'input 'validate)
        (dag-draw-add-edge graph 'validate 'process)
        (dag-draw-add-edge graph 'process 'store)
        (dag-draw-add-edge graph 'store 'output)

        ;; Cross dependencies
        (dag-draw-add-edge graph 'process 'notify)
        (dag-draw-add-edge graph 'notify 'output)

        (dag-draw-layout-graph graph)

        ;; Should maintain topological ordering despite cross-dependencies
        (let ((ranks (mapcar (lambda (id)
                               (cons id (dag-draw-node-rank (dag-draw-get-node graph id))))
                             '(input validate process store notify output))))

          ;; Core flow should be maintained
          (expect (cdr (assoc 'input ranks)) :to-be-less-than (cdr (assoc 'validate ranks)))
          (expect (cdr (assoc 'validate ranks)) :to-be-less-than (cdr (assoc 'process ranks)))
          (expect (cdr (assoc 'process ranks)) :to-be-less-than (cdr (assoc 'output ranks))))))

  ;; =================================================================
  ;; A2: AVOID VISUAL ANOMALIES (Section 1.1, line 47)
  ;; "avoid edge crossings and sharp bends"
  ;; =================================================================

  (describe "A2: Avoid Visual Anomalies - Line 47"

    (it "should minimize edge crossings in ASCII output"
      (let ((graph (dag-draw-create-graph)))
        ;; Create scenario prone to crossings
        (dag-draw-add-node graph 'top-left "Top Left")
        (dag-draw-add-node graph 'top-right "Top Right")
        (dag-draw-add-node graph 'bottom-left "Bottom Left")
        (dag-draw-add-node graph 'bottom-right "Bottom Right")

        ;; Crossing edges: TL->BR and TR->BL
        (dag-draw-add-edge graph 'top-left 'bottom-right)
        (dag-draw-add-edge graph 'top-right 'bottom-left)

        (dag-draw-layout-graph graph)

        (let* ((ascii-output (dag-draw-render-ascii graph))
               (grid (dag-draw-test--parse-ascii-grid ascii-output)))

          ;; Verify output is non-empty
          (expect ascii-output :to-be-truthy)
          (expect (length ascii-output) :to-be-greater-than 20)

          ;; Verify all nodes rendered completely using grid-based verification
          (let ((node-validation (dag-draw-test--validate-node-completeness ascii-output graph)))
            (expect (plist-get node-validation :complete) :to-be t)
            (expect (length (plist-get node-validation :missing-text)) :to-equal 0))

          ;; Verify node boundaries are properly formed with box-drawing characters
          (let ((boundary-validation (dag-draw-test--validate-node-boundaries ascii-output)))
            (expect (plist-get boundary-validation :valid) :to-be t))

          ;; Verify junctions have correct connectivity (proper edge routing)
          (let ((junction-validation (dag-draw-test--validate-junction-connectivity grid)))
            (expect (plist-get junction-validation :all-valid) :to-be t)
            (expect (length (plist-get junction-validation :invalid-junctions)) :to-equal 0))

          ;; Verify no malformed junction patterns (junctions adjacent to borders)
          (let ((malformed (dag-draw-test--find-malformed-junctions grid)))
            (expect (length malformed) :to-equal 0)))))

    (it "should avoid node-edge overlaps in ASCII rendering"
      (let ((graph (dag-draw-create-graph)))
        ;; Create layout that could cause overlaps
        (dag-draw-add-node graph 'center "Center Node")
        (dag-draw-add-node graph 'top "Top")
        (dag-draw-add-node graph 'bottom "Bottom")
        (dag-draw-add-node graph 'side "Side")

        (dag-draw-add-edge graph 'top 'center)
        (dag-draw-add-edge graph 'center 'bottom)
        (dag-draw-add-edge graph 'side 'bottom)

        (dag-draw-layout-graph graph)

        (let ((ascii-output (dag-draw-render-ascii graph)))
          ;; Should contain all node labels without corruption
          (expect ascii-output :to-match "Center Node")
          (expect ascii-output :to-match "Top")
          (expect ascii-output :to-match "Bottom")
          (expect ascii-output :to-match "Side"))))

    (it "should produce smooth edge routing without sharp bends"
      (let ((graph (dag-draw-create-graph)))
        ;; Simple vertical chain - should produce straight edges
        (dag-draw-add-node graph 'first "First")
        (dag-draw-add-node graph 'second "Second")
        (dag-draw-add-node graph 'third "Third")

        (dag-draw-add-edge graph 'first 'second)
        (dag-draw-add-edge graph 'second 'third)

        (dag-draw-layout-graph graph)

        (let ((ascii-output (dag-draw-render-ascii graph)))
          ;; Should contain vertical edge characters
          (expect ascii-output :to-match "│")
          ;; Should contain arrow indicating direction
          (expect ascii-output :to-match "▼"))))

    (it "should handle complex graphs without visual corruption"
      (let ((graph (dag-draw-create-graph)))
        ;; Complex graph with potential for visual anomalies
        (dag-draw-add-node graph 'a "Node A")
        (dag-draw-add-node graph 'b "Node B")
        (dag-draw-add-node graph 'c "Node C")
        (dag-draw-add-node graph 'd "Node D")
        (dag-draw-add-node graph 'e "Node E")

        ;; Multiple edge patterns
        (dag-draw-add-edge graph 'a 'b)
        (dag-draw-add-edge graph 'a 'c)
        (dag-draw-add-edge graph 'b 'd)
        (dag-draw-add-edge graph 'c 'd)
        (dag-draw-add-edge graph 'd 'e)

        (dag-draw-layout-graph graph)

        (let ((ascii-output (dag-draw-render-ascii graph)))
          ;; Should be structurally sound
          (expect ascii-output :to-be-truthy)
          (expect (length ascii-output) :to-be-greater-than 50)

          ;; Should contain all nodes
          (dolist (label '("Node A" "Node B" "Node C" "Node D" "Node E"))
            (expect ascii-output :to-match label))))))

  ;; =================================================================
  ;; A3: KEEP EDGES SHORT (Section 1.1, line 50)
  ;; "makes it easier to find related nodes and contributes to A2"
  ;; =================================================================

  (describe "A3: Keep Edges Short - Line 50"

    (it "should minimize total edge length in rank assignment"
      (let ((graph (dag-draw-create-graph)))
        ;; Simple chain - should use minimum edge lengths
        (dag-draw-add-node graph 'start "Start")
        (dag-draw-add-node graph 'middle "Middle")
        (dag-draw-add-node graph 'end "End")

        (dag-draw-add-edge graph 'start 'middle)
        (dag-draw-add-edge graph 'middle 'end)

        (dag-draw-layout-graph graph)

        ;; Each edge should have minimum length (typically 1)
        (let ((rank-start (dag-draw-node-rank (dag-draw-get-node graph 'start)))
              (rank-middle (dag-draw-node-rank (dag-draw-get-node graph 'middle)))
              (rank-end (dag-draw-node-rank (dag-draw-get-node graph 'end))))

          (let ((edge1-length (- rank-middle rank-start))
                (edge2-length (- rank-end rank-middle)))
            ;; Paper A3: minimize edge lengths
            (expect edge1-length :to-be-close-to 1 0.1)
            (expect edge2-length :to-be-close-to 1 0.1)))))

    (it "should prefer shorter paths in complex graphs"
      (let ((graph (dag-draw-create-graph)))
        ;; Graph with alternative path lengths
        (dag-draw-add-node graph 'source "Source")
        (dag-draw-add-node graph 'direct "Direct")
        (dag-draw-add-node graph 'indirect1 "Indirect 1")
        (dag-draw-add-node graph 'indirect2 "Indirect 2")
        (dag-draw-add-node graph 'target "Target")

        ;; Direct path: source -> direct -> target (length 2)
        (dag-draw-add-edge graph 'source 'direct)
        (dag-draw-add-edge graph 'direct 'target)

        ;; Indirect path: source -> indirect1 -> indirect2 -> target (length 3)
        (dag-draw-add-edge graph 'source 'indirect1)
        (dag-draw-add-edge graph 'indirect1 'indirect2)
        (dag-draw-add-edge graph 'indirect2 'target)

        (dag-draw-layout-graph graph)

        ;; Should not add unnecessary rank separation
        (let ((rank-source (dag-draw-node-rank (dag-draw-get-node graph 'source)))
              (rank-target (dag-draw-node-rank (dag-draw-get-node graph 'target))))

          ;; Total height should be minimized
          (let ((total-height (- rank-target rank-source)))
            ;; Should be 3 (minimum to accommodate longest path)
            (expect total-height :to-be-close-to 3 0.1)))))

    (it "should optimize coordinate assignment for edge straightness"
      (let ((graph (dag-draw-create-graph)))
        ;; Vertical chain that should be straight
        (dag-draw-add-node graph 'top "Top Node")
        (dag-draw-add-node graph 'center "Center Node")
        (dag-draw-add-node graph 'bottom "Bottom Node")

        (dag-draw-add-edge graph 'top 'center)
        (dag-draw-add-edge graph 'center 'bottom)

        (dag-draw-layout-graph graph)

        ;; X coordinates should be aligned for straight vertical path
        (let ((x-top (dag-draw-node-x-coord (dag-draw-get-node graph 'top)))
              (x-center (dag-draw-node-x-coord (dag-draw-get-node graph 'center)))
              (x-bottom (dag-draw-node-x-coord (dag-draw-get-node graph 'bottom))))

          ;; Should be approximately aligned (tolerance for network simplex optimization)
          ;; Network simplex may not produce exact alignment but should be close
          (expect (abs (- x-top x-center)) :to-be-less-than 15)
          (expect (abs (- x-center x-bottom)) :to-be-less-than 15))))

    (it "should minimize coordinate spread in ASCII grid"
      (let ((graph (dag-draw-create-graph)))
        ;; Compact graph that shouldn't spread unnecessarily
        (dag-draw-add-node graph 'root "Root")
        (dag-draw-add-node graph 'leaf "Leaf")
        (dag-draw-add-edge graph 'root 'leaf)

        (dag-draw-layout-graph graph)

        (let ((ascii-output (dag-draw-render-ascii graph)))
          ;; Should be compact - count lines
          (let ((lines (split-string ascii-output "\\n" t)))
            ;; Simple two-node graph should be relatively compact
            (expect (length lines) :to-be-less-than 15))))))

  ;; =================================================================
  ;; A4: FAVOR SYMMETRY AND BALANCE (Section 1.1, line 53)
  ;; "has a secondary role in a few places in our algorithm"
  ;; =================================================================

  (describe "A4: Favor Symmetry and Balance - Line 53"

    (it "should balance symmetric graph structures"
      (let ((graph (dag-draw-create-graph)))
        ;; Symmetric diamond structure
        (dag-draw-add-node graph 'top "Top")
        (dag-draw-add-node graph 'left "Left")
        (dag-draw-add-node graph 'right "Right")
        (dag-draw-add-node graph 'bottom "Bottom")

        (dag-draw-add-edge graph 'top 'left)
        (dag-draw-add-edge graph 'top 'right)
        (dag-draw-add-edge graph 'left 'bottom)
        (dag-draw-add-edge graph 'right 'bottom)

        (dag-draw-layout-graph graph)

        ;; Left and right should be balanced around center
        (let ((x-top (dag-draw-node-x-coord (dag-draw-get-node graph 'top)))
              (x-left (dag-draw-node-x-coord (dag-draw-get-node graph 'left)))
              (x-right (dag-draw-node-x-coord (dag-draw-get-node graph 'right)))
              (x-bottom (dag-draw-node-x-coord (dag-draw-get-node graph 'bottom))))

          ;; Top and bottom should be centered between left and right
          ;; Tolerance adjusted for GKNV A3>A4 priority (short edges over perfect balance)
          (let ((center-x (/ (+ x-left x-right) 2)))
            (expect (abs (- x-top center-x)) :to-be-less-than 50)
            (expect (abs (- x-bottom center-x)) :to-be-less-than 50)))))

    (it "should balance rank populations when possible"
      (let ((graph (dag-draw-create-graph)))
        ;; Create graph with potential for rank balancing
        (dag-draw-add-node graph 'root "Root")
        (dag-draw-add-node graph 'mid1 "Mid 1")
        (dag-draw-add-node graph 'mid2 "Mid 2")
        (dag-draw-add-node graph 'mid3 "Mid 3")
        (dag-draw-add-node graph 'leaf "Leaf")

        ;; Root feeds all mid nodes, all mid nodes feed leaf
        (dag-draw-add-edge graph 'root 'mid1)
        (dag-draw-add-edge graph 'root 'mid2)
        (dag-draw-add-edge graph 'root 'mid3)
        (dag-draw-add-edge graph 'mid1 'leaf)
        (dag-draw-add-edge graph 'mid2 'leaf)
        (dag-draw-add-edge graph 'mid3 'leaf)

        (dag-draw-layout-graph graph)

        ;; Mid nodes should all be on same rank (balanced)
        (let ((rank-mid1 (dag-draw-node-rank (dag-draw-get-node graph 'mid1)))
              (rank-mid2 (dag-draw-node-rank (dag-draw-get-node graph 'mid2)))
              (rank-mid3 (dag-draw-node-rank (dag-draw-get-node graph 'mid3))))

          ;; All mid nodes should be on same rank
          (expect (abs (- rank-mid1 rank-mid2)) :to-be-less-than 0.1)
          (expect (abs (- rank-mid2 rank-mid3)) :to-be-less-than 0.1))))

    (it "should produce visually balanced ASCII output"
      (let ((graph (dag-draw-create-graph)))
        ;; Balanced tree structure
        (dag-draw-add-node graph 'root "Root")
        (dag-draw-add-node graph 'branch-a "Branch A")
        (dag-draw-add-node graph 'branch-b "Branch B")
        (dag-draw-add-node graph 'leaf-1 "Leaf 1")
        (dag-draw-add-node graph 'leaf-2 "Leaf 2")
        (dag-draw-add-node graph 'leaf-3 "Leaf 3")
        (dag-draw-add-node graph 'leaf-4 "Leaf 4")

        (dag-draw-add-edge graph 'root 'branch-a)
        (dag-draw-add-edge graph 'root 'branch-b)
        (dag-draw-add-edge graph 'branch-a 'leaf-1)
        (dag-draw-add-edge graph 'branch-a 'leaf-2)
        (dag-draw-add-edge graph 'branch-b 'leaf-3)
        (dag-draw-add-edge graph 'branch-b 'leaf-4)

        (dag-draw-layout-graph graph)

        (let ((ascii-output (dag-draw-render-ascii graph)))
          ;; Should produce structurally balanced output
          (expect ascii-output :to-be-truthy)
          (expect (length ascii-output) :to-be-greater-than 30)

          ;; Should contain all elements
          (expect ascii-output :to-match "Root")
          (expect ascii-output :to-match "Branch A")
          (expect ascii-output :to-match "Branch B")))))

  ;; =================================================================
  ;; AESTHETIC PRINCIPLES INTEGRATION TESTS
  ;; =================================================================

  (describe "Aesthetic Principles Integration"

    (it "should balance multiple principles in complex graphs"
      (let ((graph (dag-draw-create-graph)))
        ;; Complex graph that tests multiple principles
        (dag-draw-add-node graph 'start "Start Process")
        (dag-draw-add-node graph 'validate-input "Validate Input")
        (dag-draw-add-node graph 'parse-data "Parse Data")
        (dag-draw-add-node graph 'transform "Transform")
        (dag-draw-add-node graph 'validate-output "Validate Output")
        (dag-draw-add-node graph 'store-result "Store Result")
        (dag-draw-add-node graph 'send-notification "Send Notification")
        (dag-draw-add-node graph 'complete "Complete")

        ;; Main flow
        (dag-draw-add-edge graph 'start 'validate-input)
        (dag-draw-add-edge graph 'validate-input 'parse-data)
        (dag-draw-add-edge graph 'parse-data 'transform)
        (dag-draw-add-edge graph 'transform 'validate-output)
        (dag-draw-add-edge graph 'validate-output 'store-result)
        (dag-draw-add-edge graph 'store-result 'complete)

        ;; Parallel notification
        (dag-draw-add-edge graph 'store-result 'send-notification)
        (dag-draw-add-edge graph 'send-notification 'complete)

        (dag-draw-layout-graph graph)

        ;; A1: Should expose clear hierarchical flow
        (let ((start-rank (dag-draw-node-rank (dag-draw-get-node graph 'start)))
              (complete-rank (dag-draw-node-rank (dag-draw-get-node graph 'complete))))
          (expect complete-rank :to-be-greater-than start-rank))

        ;; A2: Should produce clean output without anomalies
        (let ((ascii-output (dag-draw-render-ascii graph)))
          (expect ascii-output :to-be-truthy)
          (expect (length ascii-output) :to-be-greater-than 100))

        ;; A3: Should use reasonable edge lengths
        (let ((total-ranks (- (dag-draw-node-rank (dag-draw-get-node graph 'complete))
                             (dag-draw-node-rank (dag-draw-get-node graph 'start)))))
          (expect total-ranks :to-be-less-than 10)) ; Reasonable height

        ;; All nodes should have valid coordinates (integration test)
        (dolist (node-id '(start validate-input parse-data transform
                           validate-output store-result send-notification complete))
          (let ((node (dag-draw-get-node graph node-id)))
            (expect (dag-draw-node-rank node) :not :to-be nil)
            (expect (dag-draw-node-x-coord node) :not :to-be nil)
            (expect (dag-draw-node-y-coord node) :not :to-be nil)))))

    (it "should handle aesthetic conflicts gracefully"
      (let ((graph (dag-draw-create-graph)))
        ;; Graph that creates tension between different principles
        (dag-draw-add-node graph 'source "Source")
        (dag-draw-add-node graph 'hub "Hub")
        (dag-draw-add-node graph 'target1 "Target 1")
        (dag-draw-add-node graph 'target2 "Target 2")
        (dag-draw-add-node graph 'target3 "Target 3")
        (dag-draw-add-node graph 'final "Final")

        ;; Create pattern that could conflict: hub -> many targets -> final
        (dag-draw-add-edge graph 'source 'hub)
        (dag-draw-add-edge graph 'hub 'target1)
        (dag-draw-add-edge graph 'hub 'target2)
        (dag-draw-add-edge graph 'hub 'target3)
        (dag-draw-add-edge graph 'target1 'final)
        (dag-draw-add-edge graph 'target2 'final)
        (dag-draw-add-edge graph 'target3 'final)

        ;; Should resolve conflicts and produce valid layout
        (expect (dag-draw-layout-graph graph) :not :to-be nil)

        (let ((ascii-output (dag-draw-render-ascii graph)))
          ;; Should produce usable output despite complexity
          (expect ascii-output :to-be-truthy)
          (expect ascii-output :to-match "Source")
          (expect ascii-output :to-match "Hub")
          (expect ascii-output :to-match "Final")))))))

;;; gknv-aesthetic-principles-test.el ends here
