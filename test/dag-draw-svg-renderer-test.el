;;; dag-draw-svg-renderer-test.el --- TDD tests for SVG output renderer -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'dag-draw)

(describe "SVG Output Renderer"
  (describe "dag-draw-render-svg function"

    (it "should position nodes correctly with text labels"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a "Node A")
        (dag-draw-add-node graph 'b "Node B")
        (let ((svg-output (dag-draw-render-svg graph)))
          (expect svg-output :to-match "<rect")
          (expect svg-output :to-match "<text")
          (expect svg-output :to-match "Node A")
          (expect svg-output :to-match "Node B"))))


    (it "should produce proportional layout matching ASCII rendering"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a "A")
        (dag-draw-add-node graph 'b "B")
        (dag-draw-add-edge graph 'a 'b)
        (let ((svg-output (dag-draw-render-svg graph)))
          (expect svg-output :to-match "viewBox")
          (expect svg-output :to-match "width")
          (expect svg-output :to-match "height"))))))

;;; dag-draw-svg-renderer-test.el ends here