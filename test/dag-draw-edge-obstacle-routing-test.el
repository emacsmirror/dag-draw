;;; dag-draw-edge-obstacle-routing-test.el --- ASCII edge routing regressions -*- lexical-binding: t; -*-

;;; Commentary:
;; Regression tests for two ASCII renderer defects:
;;  1. Rank-skipping ("transitive") edges were drawn straight through an
;;     intervening node box, overwriting its label (e.g. "Build" -> "Bui|d").
;;  2. The source-port junction (a node's bottom border where an edge leaves)
;;     was never converted to a T-junction character.

;;; Code:

(require 'buttercup)
(require 'dag-draw)

(defun dd-test--render-triangle ()
  "Render design->build->test plus the rank-skipping design->test edge."
  (let ((g (dag-draw-create-graph)))
    (dag-draw-add-node g 'design "Design")
    (dag-draw-add-node g 'build "Build")
    (dag-draw-add-node g 'test "Test")
    (dag-draw-add-edge g 'design 'build)
    (dag-draw-add-edge g 'build 'test)
    (dag-draw-add-edge g 'design 'test)
    (dag-draw-layout-graph g)
    (dag-draw-render-graph g 'ascii)))

(describe "ASCII edge routing - obstacle avoidance"
  (it "does not draw a rank-skipping edge through an intervening node box"
    (let ((ascii (dd-test--render-triangle)))
      ;; The Build label must remain intact (was corrupted to "Bui|d").
      (expect ascii :to-match "Build")
      ;; No vertical edge glyph may appear between the letters of a node label.
      (expect ascii :not :to-match "Bui[│|]d")))

  (it "still renders all three node labels"
    (let ((ascii (dd-test--render-triangle)))
      (expect ascii :to-match "Design")
      (expect ascii :to-match "Build")
      (expect ascii :to-match "Test"))))

(describe "ASCII edge routing - port junctions"
  (it "emits a T-junction where an edge leaves a node's bottom border"
    (let ((g (dag-draw-create-graph)))
      (dag-draw-add-node g 'a "Alpha")
      (dag-draw-add-node g 'b "Beta")
      (dag-draw-add-edge g 'a 'b)
      (dag-draw-layout-graph g)
      (let ((ascii (dag-draw-render-graph g 'ascii)))
        ;; Alpha's bottom border should carry a downward T-junction at the port.
        (expect ascii :to-match "┬")))))

(provide 'dag-draw-edge-obstacle-routing-test)
;;; dag-draw-edge-obstacle-routing-test.el ends here
