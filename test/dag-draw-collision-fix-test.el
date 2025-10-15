;;; dag-draw-collision-fix-test.el --- Test for edge-node collision fixes -*- lexical-binding: t -*-

;;; Commentary:

;; FUTURE ENHANCEMENT - Beyond GKNV Baseline
;;
;; Enhancement Category: Quality
;; Baseline Status: ⏳ Deferred (Quality improvements beyond baseline)
;;
;; This test verifies:
;; - Edge-node collision fixes in ASCII output
;; - Text corruption prevention (edges not overwriting node text)
;; - Complex multi-node convergence pattern rendering
;;
;; Related Baseline Decisions: D5.3 (Edge Routing), D5.5 (Arrow Placement)
;; Enhancement Source: ASCII rendering quality validation
;;
;; These tests ensure rendering quality beyond baseline correctness.
;; See doc/test-suite-analysis.md (Category B3) for categorization rationale.

;;; Code:

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-render)
(require 'dag-draw-test-harness)

(describe "Edge-Node Collision Prevention"
  
  (describe "Text corruption prevention"
    (it "should not corrupt node text with edge characters"
      ;; RENDERER STRESS TEST: Artificially creates text corruption scenario
      ;; Manual coordinates intentional - testing renderer text preservation
      (let ((graph (dag-draw-create-graph)))
        ;; Create nodes that are likely to have edge routing through them
        (dag-draw-add-node graph 'task-d "Task D")
        (dag-draw-add-node graph 'task-e "Task E")
        (dag-draw-add-edge graph 'task-d 'task-e)

        ;; Set vertical coordinates to create vertical edge routing
        (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'task-d)) 100)
        (setf (dag-draw-node-y-coord (dag-draw-get-node graph 'task-d)) 50)
        (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'task-e)) 100)
        (setf (dag-draw-node-y-coord (dag-draw-get-node graph 'task-e)) 150)
        
        (let ((ascii-output (dag-draw-render-ascii graph)))
          ;; Debug: print the output to see what we get
          (message "=== COLLISION TEST OUTPUT ===")
          (message "%s" ascii-output)
          (message "=== END OUTPUT ===")
          
          ;; Use test harness for comprehensive validation
          (let ((node-validation (dag-draw-test--validate-node-completeness ascii-output graph)))
            (expect (plist-get node-validation :complete) :to-be t))
          (let ((boundary-validation (dag-draw-test--validate-node-boundaries ascii-output)))
            (expect (plist-get boundary-validation :valid) :to-be t))
          (let ((connectivity-validation (dag-draw-test--validate-edge-connectivity ascii-output graph)))
            (expect (plist-get connectivity-validation :all-connected) :to-be t)))))
    
    (it "should handle complex multi-node convergence without corruption"
      ;; This test replicates the failing test pattern more closely
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'task-a "Task A")
        (dag-draw-add-node graph 'task-b "Task B") 
        (dag-draw-add-node graph 'task-c "Task C")
        (dag-draw-add-node graph 'task-d "Task D")
        (dag-draw-add-node graph 'task-e "Task E")
        (dag-draw-add-node graph 'task-f "Task F")
        
        ;; Create convergence pattern like the failing tests
        (dag-draw-add-edge graph 'task-a 'task-b)
        (dag-draw-add-edge graph 'task-a 'task-c)
        (dag-draw-add-edge graph 'task-b 'task-d)
        (dag-draw-add-edge graph 'task-c 'task-d)
        (dag-draw-add-edge graph 'task-d 'task-e)
        (dag-draw-add-edge graph 'task-d 'task-f)
        
        ;; Run layout 
        (dag-draw-layout-graph graph :coordinate-mode 'ascii)
        
        (let ((ascii-output (dag-draw-render-ascii graph)))
          ;; Debug: print the output
          (message "=== COMPLEX CONVERGENCE TEST OUTPUT ===")
          (message "%s" ascii-output)
          (message "=== END COMPLEX OUTPUT ===")
          
          ;; Use test harness for comprehensive validation
          (let ((node-validation (dag-draw-test--validate-node-completeness ascii-output graph)))
            (expect (plist-get node-validation :complete) :to-be t))
          (let ((structure-validation (dag-draw-test--validate-graph-structure ascii-output graph)))
            (expect (plist-get structure-validation :topology-match) :to-be t)
            (expect (plist-get structure-validation :node-count-match) :to-be t))
          (let ((boundary-validation (dag-draw-test--validate-node-boundaries ascii-output)))
            (expect (plist-get boundary-validation :valid) :to-be t)))))
    
    (it "should connect simple two-node graph with visible edges"
      ;; RENDERER STRESS TEST: Artificially creates boundary connection scenario
      ;; Manual coordinates intentional - testing renderer edge-to-boundary connection
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'source "Source")
        (dag-draw-add-node graph 'target "Target")
        (dag-draw-add-edge graph 'source 'target)

        ;; Set vertical coordinates for boundary connection test
        (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'source)) 100)
        (setf (dag-draw-node-y-coord (dag-draw-get-node graph 'source)) 50)
        (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'target)) 100)
        (setf (dag-draw-node-y-coord (dag-draw-get-node graph 'target)) 150)
        
        (let ((ascii-output (dag-draw-render-ascii graph)))
          ;; Debug: print the output
          (message "=== BOUNDARY CONNECTION TEST OUTPUT ===")
          (message "%s" ascii-output)
          (message "=== END BOUNDARY OUTPUT ===")
          
          ;; Use test harness for validation
          (let ((node-validation (dag-draw-test--validate-node-completeness ascii-output graph)))
            (expect (plist-get node-validation :complete) :to-be t))
          (let ((connectivity-validation (dag-draw-test--validate-edge-connectivity ascii-output graph)))
            (expect (plist-get connectivity-validation :all-connected) :to-be t)))))
    
    (it "should produce correct arrow character based on layout direction"
      ;; RENDERER STRESS TEST: Artificially creates arrow direction scenario
      ;; Manual coordinates intentional - testing renderer arrow character selection
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'node-a "Node A")
        (dag-draw-add-node graph 'node-b "Node B")
        (dag-draw-add-edge graph 'node-a 'node-b)

        ;; Set coordinates for vertical layout (should produce downward arrow)
        (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'node-a)) 100)
        (setf (dag-draw-node-y-coord (dag-draw-get-node graph 'node-a)) 50)
        (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'node-b)) 100)
        (setf (dag-draw-node-y-coord (dag-draw-get-node graph 'node-b)) 150)
        
        (let ((ascii-output (dag-draw-render-ascii graph)))
          ;; Debug: print the output  
          (message "=== ARROW DIRECTION TEST OUTPUT ===")
          (message "%s" ascii-output)
          (message "=== END ARROW OUTPUT ===")
          
          ;; Use test harness for validation
          (let ((node-validation (dag-draw-test--validate-node-completeness ascii-output graph)))
            (expect (plist-get node-validation :complete) :to-be t))
          (let ((arrow-validation (dag-draw-test--validate-arrows ascii-output)))
            (expect (plist-get arrow-validation :valid-arrows) :to-be-greater-than 0)))))
    
    (it "should preserve box drawing characters in node borders"
      ;; RENDERER STRESS TEST: Testing renderer border character preservation
      ;; Manual coordinates intentional - testing renderer box drawing quality
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'test "Test")

        (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'test)) 100)
        (setf (dag-draw-node-y-coord (dag-draw-get-node graph 'test)) 100)
        
        (let ((ascii-output (dag-draw-render-ascii graph)))
          ;; Use test harness for validation
          (let ((node-validation (dag-draw-test--validate-node-completeness ascii-output graph)))
            (expect (plist-get node-validation :complete) :to-be t))
          (let ((boundary-validation (dag-draw-test--validate-node-boundaries ascii-output)))
            (expect (plist-get boundary-validation :valid) :to-be t)))))
    
    (it "should handle complex graph without text corruption"
      ;; RENDERER STRESS TEST: Artificially creates complex diamond scenario
      ;; Manual coordinates intentional - testing renderer complex pattern handling
      (let ((graph (dag-draw-create-graph)))
        ;; Create a diamond pattern that has been problematic
        (dag-draw-add-node graph 'top "Top")
        (dag-draw-add-node graph 'left "Left")
        (dag-draw-add-node graph 'right "Right")
        (dag-draw-add-node graph 'bottom "Bottom")

        (dag-draw-add-edge graph 'top 'left)
        (dag-draw-add-edge graph 'top 'right)
        (dag-draw-add-edge graph 'left 'bottom)
        (dag-draw-add-edge graph 'right 'bottom)

        ;; Set diamond coordinates
        (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'top)) 100)
        (setf (dag-draw-node-y-coord (dag-draw-get-node graph 'top)) 50)
        (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'left)) 50)
        (setf (dag-draw-node-y-coord (dag-draw-get-node graph 'left)) 100)
        (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'right)) 150)
        (setf (dag-draw-node-y-coord (dag-draw-get-node graph 'right)) 100)
        (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'bottom)) 100)
        (setf (dag-draw-node-y-coord (dag-draw-get-node graph 'bottom)) 150)
        
        (let ((ascii-output (dag-draw-render-ascii graph)))
          ;; Use test harness for comprehensive validation
          (let ((node-validation (dag-draw-test--validate-node-completeness ascii-output graph)))
            (expect (plist-get node-validation :complete) :to-be t))
          (let ((structure-validation (dag-draw-test--validate-graph-structure ascii-output graph)))
            (expect (plist-get structure-validation :topology-match) :to-be t))
          (let ((boundary-validation (dag-draw-test--validate-node-boundaries ascii-output)))
            (expect (plist-get boundary-validation :valid) :to-be t))))))

)

;;; dag-draw-collision-fix-test.el ends here