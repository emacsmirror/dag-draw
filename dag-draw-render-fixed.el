(defun dag-draw--ascii-draw-boundary-aware-path (grid x1 y1 x2 y2 occupancy-map)
  "Draw clean orthogonal path that avoids node interiors using occupancy map."
  (let* ((grid-height (length grid))
         (grid-width (if (> grid-height 0) (length (aref grid 0)) 0)))

    ;; Fix: Use all-or-nothing routing with alternative paths
    ;; Don't draw partial segments that create disconnected lines

    ;; Try horizontal-first path (x1->x2 at y1, then y1->y2 at x2)
    (let ((horizontal-blocked nil)
          (vertical-blocked nil))

      ;; Check if horizontal segment would go through occupied areas
      (let ((start-x (min x1 x2))
            (end-x (max x1 x2)))
        (dotimes (i (1+ (- end-x start-x)))
          (let ((x (+ start-x i)))
            (when (and (>= x 0) (< x grid-width) (>= y1 0) (< y1 grid-height))
              (when (aref (aref occupancy-map y1) x)
                (setq horizontal-blocked t))))))

      ;; Check if vertical segment would go through occupied areas
      (let ((start-y (min y1 y2))
            (end-y (max y1 y2)))
        (dotimes (i (1+ (- end-y start-y)))
          (let ((y (+ start-y i)))
            (when (and (>= x2 0) (< x2 grid-width) (>= y 0) (< y grid-height))
              (when (aref (aref occupancy-map y) x2)
                (setq vertical-blocked t))))))

      ;; Fix: Only draw complete connected paths, never partial segments
      (cond
       ;; If horizontal-first L-path is completely clear, draw it
       ((not (or horizontal-blocked vertical-blocked))
        ;; Draw horizontal segment: x1 to x2 at y1
        (let ((start-x (min x1 x2))
              (end-x (max x1 x2)))
          (dotimes (i (1+ (- end-x start-x)))
            (let ((x (+ start-x i)))
              (when (and (>= x 0) (< x grid-width) (>= y1 0) (< y1 grid-height))
                (when (= (aref (aref grid y1) x) ?\s)
                  (aset (aref grid y1) x ?─))))))
        
        ;; Draw vertical segment: y1 to y2 at x2
        (let ((start-y (min y1 y2))
              (end-y (max y1 y2)))
          (dotimes (i (1+ (- end-y start-y)))
            (let ((y (+ start-y i)))
              (when (and (>= x2 0) (< x2 grid-width) (>= y 0) (< y grid-height))
                (when (= (aref (aref grid y) x2) ?\s)
                  (aset (aref grid y) x2 ?│)))))))
       
       ;; If horizontal-first is blocked, try vertical-first as alternative
       (t
        (dag-draw--try-vertical-first-path grid x1 y1 x2 y2 occupancy-map))))))

(defun dag-draw--try-vertical-first-path (grid x1 y1 x2 y2 occupancy-map)
  "Try vertical-first L-path as alternative when horizontal-first is blocked."
  (let* ((grid-height (length grid))
         (grid-width (if (> grid-height 0) (length (aref grid 0)) 0))
         (vertical-blocked nil)
         (horizontal-blocked nil))

    ;; Check if vertical segment (y1->y2 at x1) would go through occupied areas
    (let ((start-y (min y1 y2))
          (end-y (max y1 y2)))
      (dotimes (i (1+ (- end-y start-y)))
        (let ((y (+ start-y i)))
          (when (and (>= x1 0) (< x1 grid-width) (>= y 0) (< y grid-height))
            (when (aref (aref occupancy-map y) x1)
              (setq vertical-blocked t))))))

    ;; Check if horizontal segment (x1->x2 at y2) would go through occupied areas
    (let ((start-x (min x1 x2))
          (end-x (max x1 x2)))
      (dotimes (i (1+ (- end-x start-x)))
        (let ((x (+ start-x i)))
          (when (and (>= x 0) (< x grid-width) (>= y2 0) (< y2 grid-height))
            (when (aref (aref occupancy-map y2) x)
              (setq horizontal-blocked t))))))

    ;; If vertical-first path is clear, draw it
    (if (not (or vertical-blocked horizontal-blocked))
        (progn
          ;; Draw vertical segment: y1 to y2 at x1
          (let ((start-y (min y1 y2))
                (end-y (max y1 y2)))
            (dotimes (i (1+ (- end-y start-y)))
              (let ((y (+ start-y i)))
                (when (and (>= x1 0) (< x1 grid-width) (>= y 0) (< y grid-height))
                  (when (= (aref (aref grid y) x1) ?\s)
                    (aset (aref grid y) x1 ?│))))))
          
          ;; Draw horizontal segment: x1 to x2 at y2
          (let ((start-x (min x1 x2))
                (end-x (max x1 x2)))
            (dotimes (i (1+ (- end-x start-x)))
              (let ((x (+ start-x i)))
                (when (and (>= x 0) (< x grid-width) (>= y2 0) (< y2 grid-height))
                  (when (= (aref (aref grid y2) x) ?\s)
                    (aset (aref grid y2) x ?─)))))))
      
      ;; If both L-paths are blocked, draw direct line as fallback
      (dag-draw--draw-direct-fallback-line grid x1 y1 x2 y2))))

(defun dag-draw--draw-direct-fallback-line (grid x1 y1 x2 y2)
  "Draw direct line as fallback when both L-paths are blocked."
  (let* ((grid-height (length grid))
         (grid-width (if (> grid-height 0) (length (aref grid 0)) 0)))
    
    ;; Simple direct line - better than disconnected segments
    (if (= x1 x2)
        ;; Pure vertical line
        (let ((start-y (min y1 y2))
              (end-y (max y1 y2)))
          (dotimes (i (1+ (- end-y start-y)))
            (let ((y (+ start-y i)))
              (when (and (>= x1 0) (< x1 grid-width) (>= y 0) (< y grid-height))
                (when (= (aref (aref grid y) x1) ?\s)
                  (aset (aref grid y) x1 ?│))))))
      
      (if (= y1 y2)
          ;; Pure horizontal line
          (let ((start-x (min x1 x2))
                (end-x (max x1 x2)))
            (dotimes (i (1+ (- end-x start-x)))
              (let ((x (+ start-x i)))
                (when (and (>= x 0) (< x grid-width) (>= y1 0) (< y1 grid-height))
                  (when (= (aref (aref grid y1) x) ?\s)
                    (aset (aref grid y1) x ?─))))))
        
        ;; Diagonal line - draw horizontal-first as best effort
        (progn
          ;; Draw horizontal segment
          (let ((start-x (min x1 x2))
                (end-x (max x1 x2)))
            (dotimes (i (1+ (- end-x start-x)))
              (let ((x (+ start-x i)))
                (when (and (>= x 0) (< x grid-width) (>= y1 0) (< y1 grid-height))
                  (when (= (aref (aref grid y1) x) ?\s)
                    (aset (aref grid y1) x ?─))))))
          
          ;; Draw vertical segment
          (let ((start-y (min y1 y2))
                (end-y (max y1 y2)))
            (dotimes (i (1+ (- end-y start-y)))
              (let ((y (+ start-y i)))
                (when (and (>= x2 0) (< x2 grid-width) (>= y 0) (< y grid-height))
                  (when (= (aref (aref grid y) x2) ?\s)
                    (aset (aref grid y) x2 ?│)))))))))))