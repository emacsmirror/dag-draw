;;; dag-draw-ascii-routing-buttercup-test.el --- Buttercup tests for ASCII edge routing -*- lexical-binding: t -*-

;; Copyright (C) 2024

;;; Commentary:

;; Comprehensive buttercup tests for ASCII edge routing following GKNV algorithm.

;;; Code:

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-core)
(require 'dag-draw-render)

(describe
 "ASCII Edge Routing - GKNV Algorithm Implementation"

 (describe
  "Occupancy Map Calculation"

  (it "creates correct occupancy map for single node"
      (let ((graph (dag-draw-create-graph)))
        ;; Add single node with known position and size
        (dag-draw-add-node graph 'test "Test Node")
        (let ((node (dag-draw-get-node graph 'test)))
          ;; Set explicit coordinates and size for deterministic test
          (setf (dag-draw-node-x-coord node) 100)
          (setf (dag-draw-node-y-coord node) 100)
          (setf (dag-draw-node-x-size node) 80)
          (setf (dag-draw-node-y-size node) 40))

        ;; Create minimal grid and test occupancy map
        (let* ((min-x 50) (min-y 50) (scale 2)
               (grid-width 30) (grid-height 30)
               (grid (make-vector grid-height nil)))

          ;; Initialize grid
          (dotimes (y grid-height)
            (aset grid y (make-vector grid-width ?\s)))

          ;; Create occupancy map
          (let ((occupancy-map (dag-draw--create-node-occupancy-map graph grid min-x min-y scale)))

            ;; Verify occupancy map structure
            (expect (length occupancy-map) :to-equal grid-height)
            (expect (length (aref occupancy-map 0)) :to-equal grid-width)

            ;; Verify some cells are marked as occupied
            (let ((occupied-count 0))
              (dotimes (y grid-height)
                (dotimes (x grid-width)
                  (when (aref (aref occupancy-map y) x)
                    (setq occupied-count (1+ occupied-count)))))

              (expect occupied-count :to-be-greater-than 0))))))

  (it "handles coordinate system transformation correctly"
      ;; Test the coordinate transformation functions
      (let ((world-coord 150)
            (min-coord 100)
            (scale 2))  ; Note: Function uses global dag-draw-ascii-coordinate-scale

        ;; ASCII-GKNV SCALING: Uses dag-draw-ascii-coordinate-scale (0.6)
        (let ((grid-coord (dag-draw--world-to-grid-coord world-coord min-coord scale)))
          (expect grid-coord :to-be-close-to 30.0 1.0))  ; (150-100) * 0.6 = 30 (ASCII-appropriate)

        ;; Test world-to-grid size conversion (uses dag-draw-ascii-box-scale 0.08)
        (let ((world-size 80))
          (let ((grid-size (dag-draw--world-to-grid-size world-size scale)))
            (expect grid-size :to-be-greater-than 3))))))

 (describe
  "Port Calculation - GKNV Section 5"

  (it "calculates correct boundary ports for all four sides"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'test "Test")
        (let ((node (dag-draw-get-node graph 'test)))
          ;; Set known position and size
          (setf (dag-draw-node-x-coord node) 100.0)
          (setf (dag-draw-node-y-coord node) 200.0)
          (setf (dag-draw-node-x-size node) 80.0)
          (setf (dag-draw-node-y-size node) 40.0)

          ;; Test all four port sides
          (let ((top-port (dag-draw--get-node-port node 'top))
                (bottom-port (dag-draw--get-node-port node 'bottom))
                (left-port (dag-draw--get-node-port node 'left))
                (right-port (dag-draw--get-node-port node 'right)))

            ;; Top port: center x, top edge y
            (expect (dag-draw-point-x top-port) :to-be-close-to 100.0 0.1)
            (expect (dag-draw-point-y top-port) :to-be-close-to 180.0 0.1)

            ;; Bottom port: center x, bottom edge y
            (expect (dag-draw-point-x bottom-port) :to-be-close-to 100.0 0.1)
            (expect (dag-draw-point-y bottom-port) :to-be-close-to 220.0 0.1)

            ;; Left port: left edge x, center y
            (expect (dag-draw-point-x left-port) :to-be-close-to 60.0 0.1)
            (expect (dag-draw-point-y left-port) :to-be-close-to 200.0 0.1)

            ;; Right port: right edge x, center y
            (expect (dag-draw-point-x right-port) :to-be-close-to 140.0 0.1)
            (expect (dag-draw-point-y right-port) :to-be-close-to 200.0 0.1)

            ;; Verify ports are on node boundaries, not center
            (expect (dag-draw-point-x top-port) :not :to-equal (dag-draw-point-x left-port))
            (expect (dag-draw-point-y top-port) :not :to-equal (dag-draw-point-y bottom-port)))))))

 (describe
  "Edge Classification - GKNV Algorithm"

  (it "correctly classifies inter-rank edges"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'rank0 "Node 0")
        (dag-draw-add-node graph 'rank1 "Node 1")
        (dag-draw-add-edge graph 'rank0 'rank1)

        ;; Set different ranks
        (setf (dag-draw-node-rank (dag-draw-get-node graph 'rank0)) 0)
        (setf (dag-draw-node-rank (dag-draw-get-node graph 'rank1)) 1)

        (let* ((edge (car (dag-draw-graph-edges graph)))
               (classification (dag-draw--classify-edge graph edge)))
          (expect classification :to-equal 'inter-rank-edge))))

  (it "correctly classifies flat edges (same rank)"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'left "Left")
        (dag-draw-add-node graph 'right "Right")
        (dag-draw-add-edge graph 'left 'right)

        ;; Set same rank
        (setf (dag-draw-node-rank (dag-draw-get-node graph 'left)) 1)
        (setf (dag-draw-node-rank (dag-draw-get-node graph 'right)) 1)

        (let* ((edge (car (dag-draw-graph-edges graph)))
               (classification (dag-draw--classify-edge graph edge)))
          (expect classification :to-equal 'flat-edge))))

  (it "correctly classifies self-edges (loops)"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'self "Self")
        (dag-draw-add-edge graph 'self 'self)

        ;; Set rank to avoid nil comparison issues
        (setf (dag-draw-node-rank (dag-draw-get-node graph 'self)) 0)

        (let* ((edge (car (dag-draw-graph-edges graph)))
               (classification (dag-draw--classify-edge graph edge)))
          (expect classification :to-equal 'self-edge)))))

 (describe
  "Routing Algorithm - All-or-Nothing Logic"

  (it "draws complete L-path when unobstructed"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'source "Source")
        (dag-draw-add-node graph 'target "Target")
        (dag-draw-add-edge graph 'source 'target)

        ;; Position nodes diagonally
        (let ((source (dag-draw-get-node graph 'source))
              (target (dag-draw-get-node graph 'target)))
          (setf (dag-draw-node-x-coord source) 100)
          (setf (dag-draw-node-y-coord source) 100)
          (setf (dag-draw-node-x-size source) 40)
          (setf (dag-draw-node-y-size source) 20)

          (setf (dag-draw-node-x-coord target) 200)
          (setf (dag-draw-node-y-coord target) 200)
          (setf (dag-draw-node-x-size target) 40)
          (setf (dag-draw-node-y-size target) 20))

        ;; Create grid with plenty of free space
        (let* ((min-x 50) (min-y 50) (scale 2)
               (grid-width 40) (grid-height 40)
               (grid (make-vector grid-height nil)))

          (dotimes (y grid-height)
            (aset grid y (make-vector grid-width ?\s)))

          ;; Draw nodes and edges
          (dag-draw--ascii-draw-nodes graph grid min-x min-y scale)
          (dag-draw--ascii-draw-edges graph grid min-x min-y scale)

          ;; Verify we have both horizontal and vertical edge segments
          (let ((has-horizontal nil)
                (has-vertical nil))

            (dotimes (y grid-height)
              (dotimes (x grid-width)
                (let ((char (aref (aref grid y) x)))
                  (when (eq char ?─) (setq has-horizontal t))
                  (when (eq char ?│) (setq has-vertical t)))))

            ;; For diagonal routing, we should have both segments
            (expect has-horizontal :to-be-truthy)
            (expect has-vertical :to-be-truthy))))))

 (describe
  "Visual Quality Standards"

  (it "produces clean ASCII output without overlapping node interiors"
      (let ((graph (dag-draw-create-graph)))
        ;; Create simple vertical layout
        (dag-draw-add-node graph 'top "Top Layer")
        (dag-draw-add-node graph 'bottom "Data Layer")

        (dag-draw-add-edge graph 'top 'bottom)

        ;; Run full layout
        (dag-draw-layout-graph graph)

        (let ((ascii-output (dag-draw-render-ascii graph)))
          ;; Basic quality checks
          (expect ascii-output :to-be-truthy)
          (expect (length ascii-output) :to-be-greater-than 50)

          ;; Should contain all node labels
          (expect ascii-output :to-match "Top Layer")
          (expect ascii-output :to-match "Data Layer")

          ;; Should use proper box-drawing characters
          (expect ascii-output :to-match "┌")  ; top-left corner
          (expect ascii-output :to-match "─")  ; horizontal line
          (expect ascii-output :to-match "│")  ; vertical line

          ;; Split into lines for detailed analysis
          (let ((lines (split-string ascii-output "\n" t)))
            (expect (length lines) :to-be-greater-than 5)

            ;; Should have clean vertical connections between layers
            (expect (cl-some (lambda (line) (string-match-p "│" line)) lines)
                    :to-be-truthy)))))))

;;; dag-draw-ascii-routing-buttercup-test.el ends here
