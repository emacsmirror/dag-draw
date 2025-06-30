;;; dag-draw-ascii-coordinate-preservation-test.el --- Tests for ASCII coordinate preservation -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; TDD tests for ensuring ASCII rendering preserves the hierarchical
;; structure created by the GKNV positioning algorithm.
;; Problem: Perfect coordinates (Y: 0, 25, 50, 75, 100) are being
;; distorted by collision buffers, creating scattered output.

;;; Code:

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-core)
(require 'dag-draw-render)

(describe "ASCII Coordinate Preservation"
  
  (it "should preserve hierarchical Y-coordinates in ASCII output"
    (let ((graph (dag-draw-create-graph)))
      ;; Create simple vertical chain to test Y-coordinate preservation
      (dag-draw-add-node graph 'top "Top")
      (dag-draw-add-node graph 'middle "Middle")
      (dag-draw-add-node graph 'bottom "Bottom")
      (dag-draw-add-edge graph 'top 'middle)
      (dag-draw-add-edge graph 'middle 'bottom)
      
      ;; Run full GKNV layout
      (dag-draw-layout-graph graph)
      
      ;; Get the positioned coordinates
      (let ((top-y (dag-draw-node-y-coord (dag-draw-get-node graph 'top)))
            (middle-y (dag-draw-node-y-coord (dag-draw-get-node graph 'middle)))
            (bottom-y (dag-draw-node-y-coord (dag-draw-get-node graph 'bottom))))
        
        (message "GKNV coordinates: Top=%s, Middle=%s, Bottom=%s" top-y middle-y bottom-y)
        
        ;; Generate ASCII output
        (let ((ascii-output (dag-draw-render-ascii graph)))
          (message "\n=== ASCII OUTPUT ===")
          (message "%s" ascii-output)
          (message "==================\n")
          
          ;; CRITICAL: Check that hierarchical structure is preserved in ASCII
          (let ((lines (split-string ascii-output "\n")))
            
            ;; Find which lines contain each node
            (let ((top-line nil)
                  (middle-line nil) 
                  (bottom-line nil))
              
              (dotimes (i (length lines))
                (let ((line (nth i lines)))
                  (when (string-match-p "Top" line)
                    (setq top-line i))
                  (when (string-match-p "Middle" line)
                    (setq middle-line i))
                  (when (string-match-p "Bottom" line)
                    (setq bottom-line i))))
              
              (message "ASCII line positions: Top=%s, Middle=%s, Bottom=%s" top-line middle-line bottom-line)
              
              ;; CRITICAL TEST: ASCII should preserve hierarchical order
              (expect top-line :not :to-be nil)
              (expect middle-line :not :to-be nil)
              (expect bottom-line :not :to-be nil)
              
              ;; Top should appear before Middle, Middle before Bottom
              (expect top-line :to-be-less-than middle-line)
              (expect middle-line :to-be-less-than bottom-line)
              
              ;; The relative spacing should be roughly proportional
              ;; If GKNV gives spacing of 25 units, ASCII should reflect that
              (let ((ascii-top-to-middle (- middle-line top-line))
                    (ascii-middle-to-bottom (- bottom-line middle-line)))
                ;; ASCII spacing should be roughly equal (hierarchical levels are equally spaced)
                (expect ascii-top-to-middle :to-be-close-to ascii-middle-to-bottom 3)))))))) ; Allow some tolerance
  
  (it "should maintain proportional spacing for complex hierarchy"
    (let ((graph (dag-draw-create-graph)))
      ;; Create the dependency graph that was failing
      (dag-draw-add-node graph 'research "Research")
      (dag-draw-add-node graph 'database "Database")
      (dag-draw-add-node graph 'api "API")
      (dag-draw-add-node graph 'backend "Backend")
      (dag-draw-add-node graph 'deployment "Deployment")
      
      ;; Simple linear chain to test proportional spacing
      (dag-draw-add-edge graph 'research 'database)
      (dag-draw-add-edge graph 'database 'api)
      (dag-draw-add-edge graph 'api 'backend)
      (dag-draw-add-edge graph 'backend 'deployment)
      
      ;; Run layout
      (dag-draw-layout-graph graph)
      
      ;; Get ASCII output
      (let ((ascii-output (dag-draw-render-ascii graph)))
        (let ((lines (split-string ascii-output "\n")))
          
          ;; Find node positions in ASCII
          (let ((research-line nil)
                (database-line nil)
                (api-line nil)
                (backend-line nil)
                (deployment-line nil))
            
            (dotimes (i (length lines))
              (let ((line (nth i lines)))
                (when (string-match-p "Research" line) (setq research-line i))
                (when (string-match-p "Database" line) (setq database-line i))
                (when (string-match-p "API" line) (setq api-line i))
                (when (string-match-p "Backend" line) (setq backend-line i))
                (when (string-match-p "Deployment" line) (setq deployment-line i))))
            
            ;; CRITICAL: All nodes should be found and in correct order
            (expect research-line :not :to-be nil)
            (expect database-line :not :to-be nil)
            (expect api-line :not :to-be nil)
            (expect backend-line :not :to-be nil)
            (expect deployment-line :not :to-be nil)
            
            ;; Should maintain hierarchical order
            (expect research-line :to-be-less-than database-line)
            (expect database-line :to-be-less-than api-line)
            (expect api-line :to-be-less-than backend-line)
            (expect backend-line :to-be-less-than deployment-line)
            
            (message "Complex hierarchy ASCII positions: R=%s D=%s A=%s B=%s Dep=%s" 
                     research-line database-line api-line backend-line deployment-line))))))
  
  (it "should not apply excessive coordinate distortion"
    (let ((graph (dag-draw-create-graph)))
      ;; Single node test - simplest case
      (dag-draw-add-node graph 'single "Single")
      
      ;; Run layout
      (dag-draw-layout-graph graph)
      
      ;; Get GKNV coordinate
      (let ((gknv-y (dag-draw-node-y-coord (dag-draw-get-node graph 'single))))
        
        ;; Should be at Y=0 for single node at rank 0
        (expect gknv-y :to-equal 0)
        
        ;; Generate ASCII and check it's reasonable
        (let ((ascii-output (dag-draw-render-ascii graph)))
          ;; ASCII should be compact for single node, not massively expanded
          (let ((line-count (length (split-string ascii-output "\n"))))
            ;; Should not create a huge grid for a single node
            (expect line-count :to-be-less-than 20)  ; Reasonable limit
            (message "Single node ASCII has %d lines" line-count)))))))

(provide 'dag-draw-ascii-coordinate-preservation-test)

;;; dag-draw-ascii-coordinate-preservation-test.el ends here