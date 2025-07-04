;;; dag-draw-quality-foundation-test.el --- Minimal tests for quality assessment foundation -*- lexical-binding: t -*-

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-core)
(require 'dag-draw-quality)

(describe "Quality Assessment Foundation - TDD Fix"

  (describe "Basic Quality Metrics Creation"
    (it "should create quality metrics without crashing"
      (let ((graph (dag-draw-create-graph)))
        ;; Create minimal graph with just one node
        (dag-draw-add-node graph 'a "A")

        ;; Ensure node has minimal required properties
        (let ((node-a (dag-draw-get-node graph 'a)))
          (setf (dag-draw-node-x-coord node-a) 0.0)
          (setf (dag-draw-node-y-coord node-a) 0.0))

        ;; This should not crash
        (let ((metrics (dag-draw-quality-assess-layout graph)))
          (expect (dag-draw-quality-metrics-p metrics) :to-be-truthy)
          (expect (numberp (dag-draw-quality-metrics-node-overlaps metrics)) :to-be-truthy)
          (expect (numberp (dag-draw-quality-metrics-edge-crossings metrics)) :to-be-truthy))))))

(provide 'dag-draw-quality-foundation-test)

;;; dag-draw-quality-foundation-test.el ends here
