;;; dag-draw-root-cause-debug-test.el --- Debug root causes of visual issues -*- lexical-binding: t -*-

;;; Commentary:

;; FUTURE ENHANCEMENT - Development/Debugging Tool
;;
;; Enhancement Category: Debug
;; Baseline Status: 🔧 Development Tool (Not part of baseline algorithm)
;;
;; This test verifies:
;; - Root cause analysis for visual issues
;; - Pattern origin detection (node boxes vs edges)
;; - Coordinate conversion debugging
;;
;; Related Baseline Decisions: D5.x (ASCII Rendering)
;; Enhancement Source: Development debugging tools
;;
;; This is a development/debugging test, not part of the baseline specification.
;; May be useful for troubleshooting rendering issues during implementation.
;; See doc/test-suite-analysis.md for categorization rationale.
;;
;; [Original commentary: Pinpoint the exact source of the ────── and floating arrow issues...]
;;
;; Pinpoint the exact source of the ──────  and floating arrow issues

;;; Code:

(require 'buttercup)
(require 'dag-draw)

(describe
 "Root Cause Analysis"

 (describe
  "Node box horizontal line investigation"
    (it "should identify if ────── comes from node boxes vs edges"
        (let ((graph (dag-draw-create-graph)))
          ;; Single node with NO edges - pure node rendering test
          (dag-draw-add-node graph 'test "TestNode")

          (dag-draw-layout-graph graph)
          (let ((output (dag-draw-render-ascii graph)))
            (message "\n=== SINGLE NODE TEST (NO EDGES) ===")
            (message "%s" output)
            (message "====================================")

            ;; If ────── appears here, it's from node box rendering
            ;; If it doesn't appear, it's from edge rendering
            (if (string-match-p "──────" output)
                (message "DIAGNOSIS: ────── pattern comes from NODE BOX rendering!")
              (message "DIAGNOSIS: ────── pattern NOT from node boxes - must be edge rendering"))
            ))))

 (describe
  "Edge vs Node boundary pattern analysis"
    (it "should show where exactly the excessive lines appear"
        (let ((graph (dag-draw-create-graph)))
          (dag-draw-add-node graph 'a "A")
          (dag-draw-add-node graph 'b "B")
          (dag-draw-add-edge graph 'a 'b)

          (dag-draw-layout-graph graph)
          (let* ((output (dag-draw-render-ascii graph))
                 (lines (split-string output "\n"))
                 (line-num 0))
            (message "\n=== LINE-BY-LINE ANALYSIS ===")
            (dolist (line lines)
              (setq line-num (1+ line-num))
              (when (string-match-p "──────" line)
                (message "Line %d contains ──────: '%s'" line-num line))
              (when (string-match-p "[◀▶▲▼]" line)
                (message "Line %d contains arrows: '%s'" line-num line)))
            (message "==============================")
            ))))

 (describe
  "Coordinate conversion debugging"
    (it "should show world vs grid coordinate mapping"
        (let ((graph (dag-draw-create-graph)))
          (dag-draw-add-node graph 'source "Source")
          (dag-draw-add-node graph 'dest1 "Dest1")
          (dag-draw-add-node graph 'dest2 "Dest2")
          (dag-draw-add-edge graph 'source 'dest1)
          (dag-draw-add-edge graph 'source 'dest2)

          (dag-draw-layout-graph graph)

          ;; Debug coordinate conversion
          (let* ((edges (dag-draw-graph-edges graph))
                 (edge1 (car edges))
                 (edge2 (cadr edges))
                 (min-x 0) (min-y 0) (scale 1.0))

            ;; Test world coordinates
            (require 'dag-draw-ports)
            (let ((conn1 (dag-draw--get-edge-connection-points graph edge1 min-x min-y scale))
                  (conn2 (dag-draw--get-edge-connection-points graph edge2 min-x min-y scale)))

              (message "\n=== COORDINATE CONVERSION DEBUG ===")
              (when conn1
                (message "Edge 1 - World coords: from(%.2f,%.2f) to(%.2f,%.2f)"
                         (dag-draw-point-x (car conn1)) (dag-draw-point-y (car conn1))
                         (dag-draw-point-x (cadr conn1)) (dag-draw-point-y (cadr conn1)))
                (message "Edge 1 - Grid coords: from(%d,%d) to(%d,%d)"
                         (round (dag-draw-point-x (car conn1))) (round (dag-draw-point-y (car conn1)))
                         (round (dag-draw-point-x (cadr conn1))) (round (dag-draw-point-y (cadr conn1)))))

              (when conn2
                (message "Edge 2 - World coords: from(%.2f,%.2f) to(%.2f,%.2f)"
                         (dag-draw-point-x (car conn2)) (dag-draw-point-y (car conn2))
                         (dag-draw-point-x (cadr conn2)) (dag-draw-point-y (cadr conn2)))
                (message "Edge 2 - Grid coords: from(%d,%d) to(%d,%d)"
                         (round (dag-draw-point-x (car conn2))) (round (dag-draw-point-y (car conn2)))
                         (round (dag-draw-point-x (cadr conn2))) (round (dag-draw-point-y (cadr conn2)))))

              (when (and conn1 conn2)
                (let ((grid1-from-x (round (dag-draw-point-x (car conn1))))
                      (grid1-from-y (round (dag-draw-point-y (car conn1))))
                      (grid2-from-x (round (dag-draw-point-x (car conn2))))
                      (grid2-from-y (round (dag-draw-point-y (car conn2)))))
                  (if (and (= grid1-from-x grid2-from-x) (= grid1-from-y grid2-from-y))
                      (message "PROBLEM: Both edges map to SAME grid coordinates (%d,%d)!" grid1-from-x grid1-from-y)
                    (message "SUCCESS: Edges map to DIFFERENT grid coordinates"))))
              (message "====================================")))))))

(provide 'dag-draw-root-cause-debug-test)

;;; dag-draw-root-cause-debug-test.el ends here
