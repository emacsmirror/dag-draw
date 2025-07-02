;;; dag-draw-coordinate-debug-test.el --- Debug coordinate system issues -*- lexical-binding: t -*-

;; Tests to trace coordinates through the complete pipeline and identify where lower-level edges disappear

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-core)
(require 'dag-draw-render)

(describe "Coordinate System Debug Tests"
  (describe "Simple 2-node vs Complex 8-node Coordinate Comparison"
    (it "should have similar coordinate patterns for both simple and complex graphs"
      (let ((simple-graph (dag-draw-create-graph))
            (complex-graph (dag-draw-create-graph)))

        ;; Simple graph: Backend → Integration (WORKS)
        (dag-draw-add-node simple-graph 'backend "Backend Implementation")
        (dag-draw-add-node simple-graph 'integration "Integration Testing")
        (dag-draw-add-edge simple-graph 'backend 'integration)
        (dag-draw-layout-graph simple-graph)

        ;; Complex graph: Full dependency tree (FAILS)
        (dag-draw-add-node complex-graph 'research "Research")
        (dag-draw-add-node complex-graph 'database "Database Design")
        (dag-draw-add-node complex-graph 'api "API Design")
        (dag-draw-add-node complex-graph 'infrastructure "Infrastructure Setup")
        (dag-draw-add-node complex-graph 'backend "Backend Implementation")
        (dag-draw-add-node complex-graph 'frontend "Frontend Implementation")
        (dag-draw-add-node complex-graph 'integration "Integration Testing")
        (dag-draw-add-node complex-graph 'deployment "Deployment")

        (dag-draw-add-edge complex-graph 'research 'database)
        (dag-draw-add-edge complex-graph 'research 'api)
        (dag-draw-add-edge complex-graph 'research 'infrastructure)
        (dag-draw-add-edge complex-graph 'database 'backend)
        (dag-draw-add-edge complex-graph 'api 'backend)
        (dag-draw-add-edge complex-graph 'api 'frontend)
        (dag-draw-add-edge complex-graph 'backend 'integration)
        (dag-draw-add-edge complex-graph 'frontend 'integration)
        (dag-draw-add-edge complex-graph 'integration 'deployment)
        (dag-draw-layout-graph complex-graph)

        ;; Compare coordinates
        (let* ((simple-backend (ht-get (dag-draw-graph-nodes simple-graph) 'backend))
               (simple-integration (ht-get (dag-draw-graph-nodes simple-graph) 'integration))
               (complex-backend (ht-get (dag-draw-graph-nodes complex-graph) 'backend))
               (complex-integration (ht-get (dag-draw-graph-nodes complex-graph) 'integration))
               (simple-bounds (dag-draw-get-graph-bounds simple-graph))
               (complex-bounds (dag-draw-get-graph-bounds complex-graph)))

          ;; Log coordinate information for debugging
          (message "\n=== COORDINATE COMPARISON ===")
          (message "SIMPLE GRAPH:")
          (message "  Backend: world-y=%.1f, rank=%s"
                   (dag-draw-node-y-coord simple-backend) (dag-draw-node-rank simple-backend))
          (message "  Integration: world-y=%.1f, rank=%s"
                   (dag-draw-node-y-coord simple-integration) (dag-draw-node-rank simple-integration))
          (message "  Bounds: min-y=%.1f, max-y=%.1f" (nth 1 simple-bounds) (nth 3 simple-bounds))

          (message "COMPLEX GRAPH:")
          (message "  Backend: world-y=%.1f, rank=%s"
                   (dag-draw-node-y-coord complex-backend) (dag-draw-node-rank complex-backend))
          (message "  Integration: world-y=%.1f, rank=%s"
                   (dag-draw-node-y-coord complex-integration) (dag-draw-node-rank complex-integration))
          (message "  Bounds: min-y=%.1f, max-y=%.1f" (nth 1 complex-bounds) (nth 3 complex-bounds))

          ;; Basic coordinate sanity checks
          (expect (dag-draw-node-y-coord simple-backend) :to-be-less-than
                  (dag-draw-node-y-coord simple-integration))
          (expect (dag-draw-node-y-coord complex-backend) :to-be-less-than
                  (dag-draw-node-y-coord complex-integration))

          ;; The coordinate systems should be reasonable
          (expect (dag-draw-node-y-coord complex-backend) :to-be-greater-than 0)
          (expect (dag-draw-node-y-coord complex-integration) :to-be-greater-than 0)))))

  (describe "Grid Conversion Analysis"
    (it "should convert world coordinates to valid grid coordinates for all nodes"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'research "Research")
        (dag-draw-add-node graph 'database "Database Design")
        (dag-draw-add-node graph 'backend "Backend Implementation")
        (dag-draw-add-node graph 'integration "Integration Testing")
        (dag-draw-add-node graph 'deployment "Deployment")

        (dag-draw-add-edge graph 'research 'database)
        (dag-draw-add-edge graph 'database 'backend)
        (dag-draw-add-edge graph 'backend 'integration)
        (dag-draw-add-edge graph 'integration 'deployment)
        (dag-draw-layout-graph graph)

        (let* ((bounds (dag-draw-get-graph-bounds graph))
               (min-x (nth 0 bounds))
               (min-y (nth 1 bounds))
               (scale dag-draw-ascii-coordinate-scale)
               (collision-spacing-buffer 10) ; From render logic
               (adjusted-min-y (- min-y collision-spacing-buffer))
               (nodes '(research database backend integration deployment)))

          (message "\n=== GRID CONVERSION ANALYSIS ===")
          (message "Scale: %s, Adjusted min-y: %.2f" scale adjusted-min-y)

          (dolist (node-id nodes)
            (let* ((node (ht-get (dag-draw-graph-nodes graph) node-id))
                   (world-y (dag-draw-node-y-coord node))
                   (grid-center-y (dag-draw--world-to-grid-coord world-y adjusted-min-y scale))
                   (node-height (dag-draw--world-to-grid-size (dag-draw-node-y-size node) scale))
                   (grid-top-y (round (- grid-center-y (/ node-height 2)))))

              (message "  %s: world-y=%.1f → grid-center-y=%.1f → grid-top-y=%d"
                       node-id world-y grid-center-y grid-top-y)

              ;; TEST: Grid coordinates should be positive and reasonable
              (expect grid-center-y :to-be-greater-than-or-equal 0)
              (expect grid-top-y :to-be-greater-than-or-equal 0)))

          ;; Generate output and check grid size
          (let* ((output (dag-draw-render-ascii graph))
                 (output-lines (split-string output "\n"))
                 (grid-height (length output-lines)))

            (message "  Final grid height: %d lines" grid-height)
            (message "=== END CONVERSION ANALYSIS ===\n")

            ;; TEST: Grid should be large enough for all nodes
            (expect grid-height :to-be-greater-than 10)
            (expect output :to-match "Backend")
            (expect output :to-match "Integration")
            (expect output :to-match "Deployment"))))))

  (describe "Edge Coordinate Tracing"
    (it "should trace edge endpoints through the coordinate pipeline"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'backend "Backend Implementation")
        (dag-draw-add-node graph 'integration "Integration Testing")
        (dag-draw-add-edge graph 'backend 'integration)
        (dag-draw-layout-graph graph)

        (let* ((edge (car (dag-draw-graph-edges graph)))
               (spline-points (dag-draw-edge-spline-points edge))
               (bounds (dag-draw-get-graph-bounds graph))
               (min-x (nth 0 bounds))
               (min-y (nth 1 bounds))
               (scale dag-draw-ascii-coordinate-scale))

          (message "\n=== EDGE COORDINATE TRACING ===")
          (message "Edge spline points: %d" (length spline-points))

          (when (>= (length spline-points) 2)
            (let* ((start-point (nth 0 spline-points))
                   (end-point (nth (1- (length spline-points)) spline-points))
                   (start-world-x (dag-draw-point-x start-point))
                   (start-world-y (dag-draw-point-y start-point))
                   (end-world-x (dag-draw-point-x end-point))
                   (end-world-y (dag-draw-point-y end-point)))

              (message "  Start: world(%.1f, %.1f)" start-world-x start-world-y)
              (message "  End: world(%.1f, %.1f)" end-world-x end-world-y)

              ;; Convert to grid coordinates (same logic as rendering)
              (let* ((collision-spacing-buffer 10)
                     (adjusted-min-x (- min-x collision-spacing-buffer))
                     (adjusted-min-y (- min-y collision-spacing-buffer))
                     (start-grid-x (round (dag-draw--world-to-grid-coord start-world-x adjusted-min-x scale)))
                     (start-grid-y (round (dag-draw--world-to-grid-coord start-world-y adjusted-min-y scale)))
                     (end-grid-x (round (dag-draw--world-to-grid-coord end-world-x adjusted-min-x scale)))
                     (end-grid-y (round (dag-draw--world-to-grid-coord end-world-y adjusted-min-y scale))))

                (message "  Start: grid(%d, %d)" start-grid-x start-grid-y)
                (message "  End: grid(%d, %d)" end-grid-x end-grid-y)

                ;; TEST: Grid coordinates should be reasonable
                (expect start-grid-x :to-be-greater-than-or-equal 0)
                (expect start-grid-y :to-be-greater-than-or-equal 0)
                (expect end-grid-x :to-be-greater-than-or-equal 0)
                (expect end-grid-y :to-be-greater-than-or-equal 0)

                ;; TEST: Should show vertical separation
                (expect end-grid-y :to-be-greater-than start-grid-y)))

          (message "=== END EDGE TRACING ===\n")))))))

;;; dag-draw-coordinate-debug-test.el ends here
