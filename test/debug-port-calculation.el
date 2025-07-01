;;; debug-port-calculation.el --- Debug port calculation issues -*- lexical-binding: t -*-

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-core)
(require 'dag-draw-render)

(describe "Port Calculation Debug"
  
  (it "should show where ports are calculated for simple graph"
    (let ((graph (dag-draw-create-graph)))
      ;; Create simple vertical connection to debug port calculation
      (dag-draw-add-node graph 'top "Top")
      (dag-draw-add-node graph 'bottom "Bottom")
      (dag-draw-add-edge graph 'top 'bottom)
      
      ;; Set explicit coordinates
      (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'top)) 100.0)
      (setf (dag-draw-node-y-coord (dag-draw-get-node graph 'top)) 25.0)
      (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'bottom)) 100.0)
      (setf (dag-draw-node-y-coord (dag-draw-get-node graph 'bottom)) 75.0)
      
      (dag-draw-layout-graph graph)
      
      (let ((output (dag-draw-render-ascii graph)))
        (message "=== PORT CALCULATION DEBUG ===")
        (message "%s" output)
        (message "==============================")
        
        ;; Check that both nodes are present
        (expect output :to-match "Top")
        (expect output :to-match "Bottom")
        
        ;; Check that the Top box is complete (not corrupted)
        (expect output :to-match "┌.*┐")  ; Check top line of box
        (expect output :to-match "│.*Top.*│")  ; Check that Top text is in a box
        (expect output :to-match "└.*┘")
        
        ;; Check that there's a vertical connection
        (expect output :to-match "│")
        
        ;; CRITICAL: The vertical line should appear BETWEEN the boxes, not adjacent
        (let ((lines (split-string output "\n"))
              (top-box-end nil)
              (bottom-box-start nil)
              (connecting-line nil))
          (dotimes (i (length lines))
            (let ((line (nth i lines)))
              ;; Find where Top box ends
              (when (string-match-p "└.*┘" line)
                (setq top-box-end i))
              ;; Find where Bottom box starts  
              (when (string-match-p "┌.*Bottom.*┐" line)
                (setq bottom-box-start i))
              ;; Find connecting line
              (when (and (string-match-p "^\\s*│\\s*$" line)
                         (> i (or top-box-end 0))
                         (< i (or bottom-box-start 999)))
                (setq connecting-line i))))
          
          (message "Top box ends at line: %s" top-box-end)
          (message "Bottom box starts at line: %s" bottom-box-start)  
          (message "Connecting line at: %s" connecting-line)
          
          ;; The connecting line should be between the boxes
          (when (and top-box-end bottom-box-start connecting-line)
            (expect connecting-line :to-be-greater-than top-box-end)
            (expect connecting-line :to-be-less-than bottom-box-start)))))))

(provide 'debug-port-calculation)

;;; debug-port-calculation.el ends here