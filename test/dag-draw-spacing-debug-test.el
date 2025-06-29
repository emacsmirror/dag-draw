;;; dag-draw-spacing-debug-test.el --- Debug spacing issues -*- lexical-binding: t -*-

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-core)
(require 'dag-draw-render)

(describe "Spacing Debug Tests"
  
  (it "should debug simple graph coordinates and rendering"
    (let ((graph (dag-draw-create-graph)))
      ;; Create simple graph exactly like the failing test
      (dag-draw-add-node graph 'research "Research")
      (dag-draw-add-node graph 'database "Database")
      (dag-draw-add-edge graph 'research 'database)
      
      ;; Run FULL pipeline
      (dag-draw-layout-graph graph)
      
      ;; Print debug info
      (let ((research-node (dag-draw-get-node graph 'research))
            (database-node (dag-draw-get-node graph 'database)))
        (message "\n=== DEBUG COORDINATES ===")
        (message "Research coords: (%s, %s) size: (%s, %s)"
                 (dag-draw-node-x-coord research-node)
                 (dag-draw-node-y-coord research-node)
                 (dag-draw-node-x-size research-node)
                 (dag-draw-node-y-size research-node))
        (message "Database coords: (%s, %s) size: (%s, %s)"
                 (dag-draw-node-x-coord database-node)
                 (dag-draw-node-y-coord database-node)
                 (dag-draw-node-x-size database-node)
                 (dag-draw-node-y-size database-node)))
      
      ;; Get bounds info
      (let ((bounds (dag-draw-get-graph-bounds graph)))
        (message "Graph bounds: min-x=%s min-y=%s max-x=%s max-y=%s"
                 (nth 0 bounds) (nth 1 bounds) (nth 2 bounds) (nth 3 bounds))
        
        ;; Debug grid coordinate calculation
        (let* ((min-x (nth 0 bounds))
               (min-y (nth 1 bounds))
               (scale 2)
               (research-node (dag-draw-get-node graph 'research))
               (research-y (dag-draw-node-y-coord research-node))
               (research-height (dag-draw-node-y-size research-node))
               (grid-center-y (dag-draw--world-to-grid-coord research-y min-y scale))
               (grid-height (dag-draw--world-to-grid-size research-height scale))
               (grid-y (- grid-center-y (/ grid-height 2))))
          (message "Research Y calc: world-y=%s grid-center-y=%s grid-height=%s grid-y=%s"
                   research-y grid-center-y grid-height grid-y)))
      
      ;; Get ASCII output
      (let ((output (dag-draw-render-ascii graph)))
        (message "\n=== ACTUAL OUTPUT ===")
        (message "%s" output)
        (message "=====================\n")
        
        ;; Basic checks still apply
        (expect output :to-match "Research")
        (expect output :to-match "Database")))))

;;; dag-draw-spacing-debug-test.el ends here