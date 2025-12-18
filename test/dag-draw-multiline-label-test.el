;;; dag-draw-multiline-label-test.el --- Tests for multi-line label rendering -*- lexical-binding: t -*-

;; Copyright (C) 2024, 2025

;;; Commentary:

;; Tests to verify that nodes with long labels correctly render
;; wrapped text across multiple lines in ASCII output.

;;; Code:

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-core)

(describe "Multi-line label rendering"

  (it "should render wrapped labels on multiple lines"
    (let ((graph (dag-draw-create-from-spec
                  :nodes '((task :label "Make orthopedist appointment"))
                  :edges '())))
      ;; Verify the label was wrapped during node creation
      (let* ((node (dag-draw-get-node graph 'task))
             (label (dag-draw-node-label node)))
        (expect (string-match-p "\n" label) :to-be-truthy)
        (expect (length (split-string label "\n")) :to-equal 2))

      ;; Layout and render
      (dag-draw-layout-graph graph)
      (let ((output (dag-draw-render-ascii graph)))
        ;; Both parts of the label should appear in output
        (expect output :to-match "orthopedist")
        (expect output :to-match "appointment"))))

  (it "should size box height to fit multiple text lines"
    (let ((graph (dag-draw-create-from-spec
                  :nodes '((task :label "Get diagnosis from orthopedist"))
                  :edges '())))
      (dag-draw-layout-graph graph)

      ;; Node should have height > 3 to fit 2 text lines + 2 borders
      (let* ((node (dag-draw-get-node graph 'task))
             (height (dag-draw-node-y-size node)))
        ;; Height should be at least 4 (2 text rows + 2 border rows)
        (expect height :to-be-greater-than 3))))

  (it "should render all lines of a task dependency graph"
    (let ((graph (dag-draw-create-from-spec
                  :nodes '((find-mri :label "Find place to get MRI"
                                     :ascii-marker "→ "
                                     :ascii-highlight t)
                           (make-mri :label "Make MRI appointment"
                                     :ascii-marker "○ ")
                           (make-ortho :label "Make orthopedist appointment"
                                       :ascii-marker "○ ")
                           (get-diagnosis :label "Get diagnosis from orthopedist"
                                          :ascii-marker "○ ")
                           (fix-shoulder :label "Fix Shoulder Pain [0/4][0%]"
                                         :ascii-marker "✓ "))
                  :edges '((find-mri make-mri)
                           (make-mri make-ortho)
                           (make-ortho get-diagnosis)
                           (get-diagnosis fix-shoulder)))))
      (dag-draw-layout-graph graph)
      (let ((output (dag-draw-render-ascii graph)))
        ;; All long labels should have both parts visible
        (expect output :to-match "orthopedist")
        (expect output :to-match "appointment")
        (expect output :to-match "diagnosis")
        ;; The [0/4][0%] part should also be visible
        (expect output :to-match "\\[0/4\\]")))))

(provide 'dag-draw-multiline-label-test)

;;; dag-draw-multiline-label-test.el ends here
