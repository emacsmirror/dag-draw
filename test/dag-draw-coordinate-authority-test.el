;;; dag-draw-coordinate-authority-test.el --- GKNV Pass 3 coordinate authority tests -*- lexical-binding: t -*-

;; Copyright (C) 2024

;;; Commentary:

;; Tests to ensure GKNV Pass 3 (coordinate assignment) has sole authority over node positioning.
;; Validates removal of manual/adjusted coordinate precedence violations per compliance report.
;; 
;; GKNV Reference: Section 4 - "The third pass finds optimal coordinates for nodes"
;; Ubiquitous Language: Pass 3 Context - Coordinate Assignment (Section 4)

;;; Code:

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-ascii-grid)  ; For dag-draw--world-to-grid-size
(require 'dag-draw-render)      ; For dag-draw-render-ascii

(describe "GKNV Pass 3 Coordinate Authority"
  
  (describe "coordinate system single source of truth"
    
    (it "should store GKNV coordinates in protected adjusted-positions"
      ;; GREEN TEST: Verifies GKNV Pass 3 authority through adjusted-positions storage
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'source "Source")
        (dag-draw-add-node graph 'target "Target") 
        (dag-draw-add-edge graph 'source 'target)
        
        ;; GKNV Pass 1-3: Algorithm assigns coordinates
        (dag-draw-layout-graph graph)
        
        ;; Verify GKNV coordinates are stored in protected adjusted-positions
        (let* ((adjusted-positions (dag-draw-graph-adjusted-positions graph))
               (source-adjusted (ht-get adjusted-positions 'source))
               (target-adjusted (ht-get adjusted-positions 'target)))
          
          ;; GKNV Pass 3 should store final coordinates in adjusted-positions
          (expect adjusted-positions :not :to-be nil)
          (expect source-adjusted :not :to-be nil)
          (expect target-adjusted :not :to-be nil)
          
          ;; Manual coordinate override (this should not affect protected coordinates)
          (let* ((source-node (dag-draw-get-node graph 'source))
                 (target-node (dag-draw-get-node graph 'target)))
            (setf (dag-draw-node-x-coord source-node) 999.0)
            (setf (dag-draw-node-x-coord target-node) 888.0)
            
            ;; Adjusted-positions should be unchanged (GKNV authority preserved)
            (expect (ht-get adjusted-positions 'source) :to-equal source-adjusted)
            (expect (ht-get adjusted-positions 'target) :to-equal target-adjusted)
            
            ;; Adjusted positions should contain GKNV coordinates, not manual overrides
            (expect (nth 0 (ht-get adjusted-positions 'source)) :not :to-equal 999.0)
            (expect (nth 0 (ht-get adjusted-positions 'target)) :not :to-equal 888.0))))))

(describe "test fixture replacement"
  
  (it "should use GKNV parameters instead of manual coordinates for test setup"
    ;; Demonstrate proper way to test specific layouts without violating Pass 3 authority
    (let ((graph (dag-draw-create-graph)))
      (dag-draw-add-node graph 'center "Center")
      (dag-draw-add-node graph 'left "Left") 
      (dag-draw-add-edge graph 'center 'left)
      
      ;; Instead of manual coordinate setting, use GKNV parameters to influence layout
      (setf (dag-draw-graph-node-separation graph) 40)  ; Wider horizontal spacing
      (setf (dag-draw-graph-rank-separation graph) 30)  ; Taller vertical spacing
      
      ;; GKNV algorithm determines final coordinates
      (dag-draw-layout-graph graph)
      
      ;; Verify GKNV assigned coordinates through adjusted-positions
      (let ((adjusted-positions (dag-draw-graph-adjusted-positions graph)))
        (expect adjusted-positions :not :to-be nil)
        (expect (ht-get adjusted-positions 'center) :not :to-be nil)
        (expect (ht-get adjusted-positions 'left) :not :to-be nil))))))

(provide 'dag-draw-coordinate-authority-test)

;;; dag-draw-coordinate-authority-test.el ends here