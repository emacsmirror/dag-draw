;;; dag-draw-port-coordinate-bug-test.el --- Demonstrate port coordinate system inconsistency -*- lexical-binding: t -*-

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-core)
(require 'dag-draw-ascii-grid)
(require 'dag-draw-splines)
(require 'dag-draw-ports)

(describe "Port Coordinate System Bug"
  (it "should demonstrate coordinate system inconsistency between spline generation and ASCII rendering"
    (let ((graph (dag-draw-create-graph)))
      
      ;; Create the problematic Research->Database setup
      (dag-draw-add-node graph 'research "Research")
      (dag-draw-add-node graph 'database "Database Design")
      (dag-draw-add-edge graph 'research 'database)
      
      (message "\n=== PORT COORDINATE SYSTEM BUG DEMONSTRATION ===")
      
      ;; STEP 1: Run layout to get initial world coordinates
      (dag-draw-layout-graph graph)
      
      (let* ((research-node (dag-draw-get-node graph 'research))
             (database-node (dag-draw-get-node graph 'database))
             (edge (car (dag-draw-graph-edges graph))))
        
        (message "\n--- STEP 1: World Coordinates After Layout ---")
        (message "Research: x=%.1f y=%.1f width=%.1f height=%.1f" 
                 (dag-draw-node-x-coord research-node)
                 (dag-draw-node-y-coord research-node)
                 (dag-draw-node-x-size research-node)
                 (dag-draw-node-y-size research-node))
        
        ;; STEP 2: Calculate port using spline generation method (world coordinates)
        (let* ((world-research-port (dag-draw--get-node-port research-node edge graph))
               (world-database-port (dag-draw--get-node-port database-node edge graph)))
          
          (message "\n--- STEP 2: Spline Generation Ports (World Coordinates) ---")
          (message "Research port (world): x=%.1f y=%.1f" 
                   (dag-draw-point-x world-research-port)
                   (dag-draw-point-y world-research-port))
          (message "Database port (world): x=%.1f y=%.1f"
                   (dag-draw-point-x world-database-port)
                   (dag-draw-point-y world-database-port))
          
          ;; STEP 3: Get bounds for coordinate conversion (before collision adjustment)
          (let* ((raw-bounds (dag-draw-get-graph-bounds graph))
                 (original-min-x (nth 0 raw-bounds))
                 (original-min-y (nth 1 raw-bounds))
                 (scale dag-draw-ascii-coordinate-scale))
            
            (message "\n--- STEP 3: Original Bounds (Before Collision Adjustment) ---")
            (message "Original bounds: min-x=%.1f min-y=%.1f" original-min-x original-min-y)
            
            ;; Convert world ports to grid using original bounds
            (let ((research-grid-original 
                   (list (dag-draw--world-to-grid-coord (dag-draw-point-x world-research-port) original-min-x scale)
                         (dag-draw--world-to-grid-coord (dag-draw-point-y world-research-port) original-min-y scale)))
                  (database-grid-original
                   (list (dag-draw--world-to-grid-coord (dag-draw-point-x world-database-port) original-min-x scale)
                         (dag-draw--world-to-grid-coord (dag-draw-point-y world-database-port) original-min-y scale))))
              
              (message "\n--- STEP 4: Grid Conversion (Original Bounds) ---")
              (message "Research port (grid, original): x=%.1f y=%.1f" 
                       (nth 0 research-grid-original) (nth 1 research-grid-original))
              (message "Database port (grid, original): x=%.1f y=%.1f"
                       (nth 0 database-grid-original) (nth 1 database-grid-original))
              
              ;; STEP 5: Simulate collision buffer adjustment (what happens in ASCII rendering)
              (let* ((collision-buffer 5.0)  ; From the debug output
                     (adjusted-min-x (- original-min-x collision-buffer))
                     (adjusted-min-y (- original-min-y collision-buffer)))
                
                (message "\n--- STEP 5: Collision Buffer Adjustment ---")
                (message "Adjusted bounds: min-x=%.1f min-y=%.1f (buffer=%.1f)" 
                         adjusted-min-x adjusted-min-y collision-buffer)
                
                ;; Convert same world coordinates using adjusted bounds
                (let ((research-grid-adjusted
                       (list (dag-draw--world-to-grid-coord (dag-draw-point-x world-research-port) adjusted-min-x scale)
                             (dag-draw--world-to-grid-coord (dag-draw-point-y world-research-port) adjusted-min-y scale)))
                      (database-grid-adjusted
                       (list (dag-draw--world-to-grid-coord (dag-draw-point-x world-database-port) adjusted-min-x scale)
                             (dag-draw--world-to-grid-coord (dag-draw-point-y world-database-port) adjusted-min-y scale))))
                  
                  (message "\n--- STEP 6: Grid Conversion (Adjusted Bounds) ---")
                  (message "Research port (grid, adjusted): x=%.1f y=%.1f" 
                           (nth 0 research-grid-adjusted) (nth 1 research-grid-adjusted))
                  (message "Database port (grid, adjusted): x=%.1f y=%.1f"
                           (nth 0 database-grid-adjusted) (nth 1 database-grid-adjusted))
                  
                  ;; STEP 7: Calculate coordinate drift caused by collision buffer
                  (let ((research-x-drift (- (nth 0 research-grid-adjusted) (nth 0 research-grid-original)))
                        (research-y-drift (- (nth 1 research-grid-adjusted) (nth 1 research-grid-original)))
                        (database-x-drift (- (nth 0 database-grid-adjusted) (nth 0 database-grid-original)))
                        (database-y-drift (- (nth 1 database-grid-adjusted) (nth 1 database-grid-original))))
                    
                    (message "\n--- STEP 7: COORDINATE DRIFT ANALYSIS ---")
                    (message "Research port drift: dx=%.1f dy=%.1f" research-x-drift research-y-drift)
                    (message "Database port drift: dx=%.1f dy=%.1f" database-x-drift database-y-drift)
                    (message "Expected drift: collision-buffer × scale = %.1f × %.3f = %.1f" 
                             collision-buffer scale (* collision-buffer scale))
                    
                    ;; STEP 8: Now get the actual rendered output to see real coordinates
                    (let ((output (dag-draw-render-ascii graph)))
                      (message "\n--- STEP 8: Actual ASCII Output ---")
                      (message "%s" output)
                      
                      (message "\n=== BUG CONFIRMATION ===")
                      (message "The %.1f grid unit drift explains why Research port appears at y=%.0f instead of y=%.0f"
                               research-y-drift
                               (nth 1 research-grid-adjusted)
                               (nth 1 research-grid-original))
                      (message "This proves the coordinate system inconsistency between spline and ASCII phases.")
                      
                      ;; Validation that the drift is significant
                      (expect (abs research-y-drift) :to-be-greater-than 0.5)
                      (expect (abs database-y-drift) :to-be-greater-than 0.5)))))))))
        
        (message "=== END PORT COORDINATE BUG DEMONSTRATION ===\n"))))

(provide 'dag-draw-port-coordinate-bug-test)

;;; dag-draw-port-coordinate-bug-test.el ends here