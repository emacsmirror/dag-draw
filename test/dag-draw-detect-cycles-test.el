;;; dag-draw-detect-cycles-test.el --- Tests for dag-draw-detect-cycles -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the public cycle-detection helper that returns the back-edges
;; (the edges that close cycles), used in the README troubleshooting section.

;;; Code:

(require 'buttercup)
(require 'dag-draw)

(describe "dag-draw-detect-cycles"

  (it "returns nil for an acyclic graph"
    (let ((graph (dag-draw-create-graph)))
      (dag-draw-add-node graph 'a "A")
      (dag-draw-add-node graph 'b "B")
      (dag-draw-add-node graph 'c "C")
      (dag-draw-add-edge graph 'a 'b)
      (dag-draw-add-edge graph 'b 'c)
      (expect (dag-draw-detect-cycles graph) :to-be nil)))

  (it "returns the back-edge that closes a cycle"
    (let ((graph (dag-draw-create-graph)))
      (dag-draw-add-node graph 'a "A")
      (dag-draw-add-node graph 'b "B")
      (dag-draw-add-node graph 'c "C")
      (dag-draw-add-edge graph 'a 'b)
      (dag-draw-add-edge graph 'b 'c)
      (dag-draw-add-edge graph 'c 'a)   ; closes the cycle a -> b -> c -> a
      (let ((back (dag-draw-detect-cycles graph)))
        (expect (length back) :to-equal 1)
        (expect (dag-draw-edge-from-node (car back)) :to-equal 'c)
        (expect (dag-draw-edge-to-node (car back)) :to-equal 'a))))

  (it "returns dag-draw-edge structures"
    (let ((graph (dag-draw-create-graph)))
      (dag-draw-add-node graph 'x "X")
      (dag-draw-add-node graph 'y "Y")
      (dag-draw-add-edge graph 'x 'y)
      (dag-draw-add-edge graph 'y 'x)
      (expect (dag-draw-edge-p (car (dag-draw-detect-cycles graph))) :to-be t))))

(provide 'dag-draw-detect-cycles-test)
;;; dag-draw-detect-cycles-test.el ends here
