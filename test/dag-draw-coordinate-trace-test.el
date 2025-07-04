;;; dag-draw-coordinate-trace-test.el --- Trace coordinate evolution for debugging -*- lexical-binding: t -*-

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-core)
(require 'dag-draw-ascii-grid)

(describe "Coordinate Evolution Trace"
  (it "should trace coordinate transformations for Research->Database edge"
    (let ((graph (dag-draw-create-graph)))
      
      ;; Create minimal test case focusing on Research->Database
      (dag-draw-add-node graph 'research "Research")
      (dag-draw-add-node graph 'database "Database Design")
      (dag-draw-add-edge graph 'research 'database)
      
      (message "\n=== COORDINATE TRACE: Research->Database ===")
      
      ;; STEP 1: Initial positioning after layout
      (dag-draw-layout-graph graph)
      
      (let* ((research-node (dag-draw-get-node graph 'research))
             (database-node (dag-draw-get-node graph 'database))
             (research-edge (car (dag-draw-graph-edges graph))))
        
        (message "\n--- STEP 1: Post-Layout World Coordinates ---")
        (message "Research: x=%.1f y=%.1f width=%.1f height=%.1f" 
                 (dag-draw-node-x-coord research-node)
                 (dag-draw-node-y-coord research-node)
                 (dag-draw-node-x-size research-node)
                 (dag-draw-node-y-size research-node))
        (message "Database: x=%.1f y=%.1f width=%.1f height=%.1f"
                 (dag-draw-node-x-coord database-node)
                 (dag-draw-node-y-coord database-node)
                 (dag-draw-node-x-size database-node)
                 (dag-draw-node-y-size database-node))
        
        ;; STEP 2: Get graph bounds before ASCII context
        (let* ((raw-bounds (dag-draw-get-graph-bounds graph))
               (min-x (nth 0 raw-bounds))
               (min-y (nth 1 raw-bounds))
               (max-x (nth 2 raw-bounds))
               (max-y (nth 3 raw-bounds)))
          
          (message "\n--- STEP 2: Raw Graph Bounds ---")
          (message "Raw bounds: min-x=%.1f min-y=%.1f max-x=%.1f max-y=%.1f" min-x min-y max-x max-y)
          
          ;; STEP 3: ASCII coordinate context creation
          (let* ((ascii-context (dag-draw--create-ascii-coordinate-context graph))
                 (offset-x (ht-get ascii-context 'offset-x))
                 (offset-y (ht-get ascii-context 'offset-y))
                 (ascii-bounds (ht-get ascii-context 'ascii-bounds)))
            
            (message "\n--- STEP 3: ASCII Context Normalization ---")
            (message "ASCII offsets: offset-x=%.1f offset-y=%.1f" offset-x offset-y)
            (message "ASCII bounds: %.1f,%.1f,%.1f,%.1f" (nth 0 ascii-bounds) (nth 1 ascii-bounds) (nth 2 ascii-bounds) (nth 3 ascii-bounds))
            
            ;; STEP 4: ASCII-normalized node positions
            (let* ((research-ascii-pos (dag-draw--ascii-get-node-position research-node ascii-context dag-draw-ascii-coordinate-scale))
                   (database-ascii-pos (dag-draw--ascii-get-node-position database-node ascii-context dag-draw-ascii-coordinate-scale)))
              
              (message "\n--- STEP 4: ASCII-Normalized Positions ---")
              (message "Research ASCII: x=%.1f y=%.1f" (nth 0 research-ascii-pos) (nth 1 research-ascii-pos))
              (message "Database ASCII: x=%.1f y=%.1f" (nth 0 database-ascii-pos) (nth 1 database-ascii-pos))
              
              ;; STEP 5: Check adjusted positions if they exist
              (let ((adjusted-positions (dag-draw-graph-adjusted-positions graph)))
                (message "\n--- STEP 5: Adjusted Positions Check ---")
                (if adjusted-positions
                    (progn
                      (message "Research adjusted: %s" (ht-get adjusted-positions 'research))
                      (message "Database adjusted: %s" (ht-get adjusted-positions 'database)))
                  (message "No adjusted positions found")))
              
              ;; STEP 6: Render ASCII and check final output
              (let ((output (dag-draw-render-ascii graph)))
                
                (message "\n--- STEP 6: Port Analysis from Splines ---")
                (when (dag-draw-edge-spline-points research-edge)
                  (let* ((spline-points (dag-draw-edge-spline-points research-edge))
                         (start-point (nth 0 spline-points))
                         (end-point (nth (1- (length spline-points)) spline-points)))
                    (message "Spline start: x=%.1f y=%.1f" (dag-draw-point-x start-point) (dag-draw-point-y start-point))
                    (message "Spline end: x=%.1f y=%.1f" (dag-draw-point-x end-point) (dag-draw-point-y end-point))))
                
                (message "\n--- ASCII OUTPUT ---")
                (message "%s" output)
                (message "=== END COORDINATE TRACE ===\n")
                
                ;; Basic validation that coordinates exist
                (expect (dag-draw-node-x-coord research-node) :to-be-truthy)
                (expect (dag-draw-node-x-coord database-node) :to-be-truthy)))))))))

(provide 'dag-draw-coordinate-trace-test)

;;; dag-draw-coordinate-trace-test.el ends here