;;; dag-draw-gknv-compliance-test.el --- Test GKNV-compliant vs broken rendering -*- lexical-binding: t -*-

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-core)
(require 'dag-draw-render-gknv-compliant)

(describe "GKNV Compliance Comparison"
  (it "should demonstrate the difference between GKNV-compliant and broken rendering"
    (let ((graph (dag-draw-create-graph)))
      
      ;; Create the Research->Database test case
      (dag-draw-add-node graph 'research "Research")
      (dag-draw-add-node graph 'database "Database Design")
      (dag-draw-add-edge graph 'research 'database)
      
      (message "\n=== GKNV COMPLIANCE COMPARISON ===")
      
      ;; Run GKNV layout ONCE
      (dag-draw-layout-graph graph)
      
      (let* ((research-node (dag-draw-get-node graph 'research))
             (database-node (dag-draw-get-node graph 'database))
             (edge (car (dag-draw-graph-edges graph))))
        
        (message "\n--- GKNV Final Coordinates (After 4-Pass Algorithm) ---")
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
        
        ;; Show GKNV spline endpoints
        (when (dag-draw-edge-spline-points edge)
          (let* ((spline-points (dag-draw-edge-spline-points edge))
                 (start-point (car spline-points))
                 (end-point (car (last spline-points))))
            (message "GKNV Spline: start(%.1f,%.1f) end(%.1f,%.1f)"
                     (dag-draw-point-x start-point) (dag-draw-point-y start-point)
                     (dag-draw-point-x end-point) (dag-draw-point-y end-point))))
        
        ;; Render with GKNV-compliant method
        (message "\n--- GKNV-COMPLIANT RENDERING ---")
        (let ((gknv-output (dag-draw-render-ascii-gknv-compliant graph)))
          (message "GKNV-Compliant Output:")
          (message "%s" gknv-output)
          
          ;; Render with broken method for comparison
          (message "\n--- BROKEN RENDERING (Current Implementation) ---")
          (let ((broken-output (dag-draw-render-ascii graph)))
            (message "Broken Output:")
            (message "%s" broken-output)
            
            (message "\n=== ANALYSIS ===")
            (message "GKNV-compliant rendering:")
            (message "- Uses GKNV final coordinates directly")
            (message "- No coordinate system changes")
            (message "- No spline regeneration")
            (message "- Ports calculated from actual node boundaries")
            
            (message "\nBroken rendering:")
            (message "- Changes coordinate system during ASCII phase")
            (message "- Regenerates splines with different coordinates")
            (message "- Ports extracted from wrong coordinate system")
            (message "- Violates GKNV algorithm authority")
            
            ;; Basic validation
            (expect (stringp gknv-output) :to-be-truthy)
            (expect (stringp broken-output) :to-be-truthy)
            (expect gknv-output :to-match "Research")
            (expect gknv-output :to-match "Database")))))
    
    (message "=== END GKNV COMPLIANCE COMPARISON ===\n")))

(provide 'dag-draw-gknv-compliance-test)

;;; dag-draw-gknv-compliance-test.el ends here