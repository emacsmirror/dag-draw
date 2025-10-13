# ASCII Rendering Strategy for dag-draw.el

## Overview

This document specifies the complete strategy for rendering GKNV-algorithm graph layouts as ASCII art using Unicode box-drawing characters. It covers coordinate scaling, character selection, edge routing, junction rules, and all edge types.

## Table of Contents

1. [Coordinate Scaling Strategy](#coordinate-scaling-strategy)
2. [Character Set](#character-set)
3. [Node Rendering](#node-rendering)
4. [Edge Routing Strategy](#edge-routing-strategy)
5. [Junction Character Rules](#junction-character-rules)
6. [Arrow Placement](#arrow-placement)
7. [Edge Type Handling](#edge-type-handling)
8. [Label Rendering](#label-rendering)
9. [Examples](#examples)

---

## Coordinate Scaling Strategy

### Problem

The GKNV algorithm operates in continuous coordinate space with arbitrary units (typically 72 per inch). ASCII rendering requires discrete character grid positions with fixed aspect ratio (~2:1 width:height).

### Solution

#### Step 1: Find Coordinate Bounds

```elisp
(defun dag-draw--find-bounds (graph)
  "Find min/max X and Y coordinates in GRAPH.
Returns (min-x max-x min-y max-y)."
  (let ((min-x most-positive-fixnum)
        (max-x most-negative-fixnum)
        (min-y most-positive-fixnum)
        (max-y most-negative-fixnum))
    (maphash (lambda (_id node)
               (setq min-x (min min-x (dag-draw-node-x node))
                     max-x (max max-x (dag-draw-node-x node))
                     min-y (min min-y (dag-draw-node-y node))
                     max-y (max max-y (dag-draw-node-y node))))
             (dag-draw-graph-nodes graph))
    (list min-x max-x min-y max-y)))
```

#### Step 2: Compute Target Grid Size

```elisp
(defun dag-draw--compute-grid-size (graph max-width max-height)
  "Compute grid dimensions fitting GRAPH within MAX-WIDTH x MAX-HEIGHT.
Returns (width height) respecting aspect ratio."
  (let* ((bounds (dag-draw--find-bounds graph))
         (min-x (nth 0 bounds))
         (max-x (nth 1 bounds))
         (min-y (nth 2 bounds))
         (max-y (nth 3 bounds))
         (coord-width (- max-x min-x))
         (coord-height (- max-y min-y)))
    ;; Add margin
    (let* ((margin-x (* 0.1 coord-width))
           (margin-y (* 0.1 coord-height))
           (total-width (+ coord-width (* 2 margin-x)))
           (total-height (+ coord-height (* 2 margin-y)))
           ;; Compute scale factors
           (x-scale (/ max-width total-width))
           (y-scale (/ max-height total-height))
           ;; Use smaller scale to fit both dimensions
           (scale (min x-scale y-scale))
           ;; Compute actual grid size
           (grid-width (ceiling (* total-width scale)))
           (grid-height (ceiling (* total-height scale))))
      (list grid-width grid-height))))
```

#### Step 3: Scale Coordinates

```elisp
(defun dag-draw--scale-coordinates (graph grid-width grid-height)
  "Scale all node coordinates to fit GRID-WIDTH x GRID-HEIGHT.
Modifies node x and y fields to integer grid positions.
Returns hash table mapping original coords to grid coords."
  (let* ((bounds (dag-draw--find-bounds graph))
         (min-x (nth 0 bounds))
         (max-x (nth 1 bounds))
         (min-y (nth 2 bounds))
         (max-y (nth 3 bounds))
         (coord-width (- max-x min-x))
         (coord-height (- max-y min-y))
         (margin 5)  ; Character cells margin
         (usable-width (- grid-width (* 2 margin)))
         (usable-height (- grid-height (* 2 margin)))
         (x-scale (/ (float usable-width) coord-width))
         (y-scale (/ (float usable-height) coord-height)))
    ;; Scale all nodes
    (maphash (lambda (_id node)
               (let ((scaled-x (+ margin (round (* (- (dag-draw-node-x node) min-x)
                                                   x-scale))))
                     (scaled-y (+ margin (round (* (- (dag-draw-node-y node) min-y)
                                                   y-scale)))))
                 (setf (dag-draw-node-x node) scaled-x
                       (dag-draw-node-y node) scaled-y)))
             (dag-draw-graph-nodes graph))))
```

### Key Principles

1. **Preserve Relative Positions**: Maintain topological relationships from GKNV layout
2. **Uniform Scaling**: Use same scale factor for X and Y to preserve graph shape
3. **Margin Handling**: Add margin around graph for visual breathing room
4. **Integer Rounding**: Round to nearest character position (prefer rounding toward center for symmetry)

---

## Character Set

### Unicode Box-Drawing Characters (U+2500 block)

#### Lines
- `─` (U+2500): Horizontal line
- `│` (U+2502): Vertical line

#### Corners
- `┌` (U+250C): Down-right corner
- `┐` (U+2510): Down-left corner
- `└` (U+2514): Up-right corner
- `┘` (U+2518): Up-left corner

#### T-Junctions
- `├` (U+251C): Vertical line with right branch
- `┤` (U+2524): Vertical line with left branch
- `┬` (U+252C): Horizontal line with down branch
- `┴` (U+2534): Horizontal line with up branch

#### Cross
- `┼` (U+253C): Four-way intersection

#### Arrows (Unicode)
- `▲` (U+25B2): Up arrow
- `▼` (U+25BC): Down arrow
- `◀` (U+25C0): Left arrow
- `▶` (U+25B6): Right arrow

#### ASCII Fallback
If Unicode unavailable:
- Lines: `-`, `|`
- Corners: `+`
- T-junctions: `+`
- Cross: `+`
- Arrows: `^`, `v`, `<`, `>`

---

## Node Rendering

### Node Box Structure

```
┌─────────────┐
│  Node Label │
└─────────────┘
```

### Size Calculation

```elisp
(defun dag-draw--compute-node-size (node)
  "Compute character dimensions for NODE box.
Returns (width . height)."
  (let* ((label (dag-draw-node-label node))
         (label-length (length label))
         (padding 2)  ; 1 space on each side
         (width (max 5 (+ label-length padding)))  ; Minimum width 5
         (height 3))  ; Top border, label, bottom border
    (cons width height)))
```

### Rendering Algorithm

```elisp
(defun dag-draw--render-node (node grid)
  "Render NODE on GRID at its grid position."
  (let* ((x (dag-draw-node-x node))
         (y (dag-draw-node-y node))
         (size (dag-draw--compute-node-size node))
         (width (car size))
         (height (cdr size))
         (label (dag-draw-node-label node))
         ;; Center node box on position
         (left (- x (/ width 2)))
         (top y))
    ;; Top border
    (dag-draw--set-grid grid left top ?┌)
    (cl-loop for i from 1 below (1- width)
             do (dag-draw--set-grid grid (+ left i) top ?─))
    (dag-draw--set-grid grid (+ left width -1) top ?┐)
    ;; Middle (label)
    (dag-draw--set-grid grid left (1+ top) ?│)
    (let* ((label-pad (- width 2 (length label)))
           (left-pad (/ label-pad 2))
           (start-x (+ left 1 left-pad)))
      (cl-loop for i from 0 below (length label)
               do (dag-draw--set-grid grid (+ start-x i) (1+ top)
                                     (aref label i))))
    (dag-draw--set-grid grid (+ left width -1) (1+ top) ?│)
    ;; Bottom border
    (dag-draw--set-grid grid left (+ top 2) ?└)
    (cl-loop for i from 1 below (1- width)
             do (dag-draw--set-grid grid (+ left i) (+ top 2) ?─))
    (dag-draw--set-grid grid (+ left width -1) (+ top 2) ?┘)
    ;; Store node box boundaries for port calculations
    (setf (dag-draw-node-box-left node) left
          (dag-draw-node-box-right node) (+ left width -1)
          (dag-draw-node-box-top node) top
          (dag-draw-node-box-bottom node) (+ top 2))))
```

### Virtual Node Rendering

Virtual nodes are typically not rendered as boxes, but as routing points. However, they need bounding boxes for routing calculations:

```elisp
(defun dag-draw--compute-virtual-node-size (vnode edges-through)
  "Compute size needed for virtual node based on edges routing through it.
EDGES-THROUGH: number of edges at this virtual node."
  (let ((width (max 1 (ceiling (/ edges-through 2.0))))
        (height 1))
    (cons width height)))
```

---

## Edge Routing Strategy

### Overview

GKNV Pass 4 computes splines (Bezier curves) in continuous space. ASCII rendering must:
1. Sample the spline at regular intervals
2. Convert sample points to grid coordinates
3. Connect points with orthogonal (horizontal/vertical) segments
4. Record which edges pass through each grid cell

### Spline Sampling

```elisp
(defun dag-draw--sample-spline (control-points num-samples)
  "Sample Bezier spline defined by CONTROL-POINTS.
Returns list of NUM-SAMPLES (x . y) points along spline."
  (let ((samples '()))
    (dotimes (i num-samples)
      (let ((t-val (/ (float i) (1- num-samples))))
        (push (dag-draw--bezier-point control-points t-val) samples)))
    (nreverse samples)))

(defun dag-draw--bezier-point (control-points t-val)
  "Evaluate Bezier curve at parameter T-VAL.
CONTROL-POINTS is list of (x . y) control points.
Returns (x . y) point on curve."
  (let ((n (1- (length control-points))))
    (cl-loop for i from 0 to n
             for point = (nth i control-points)
             for coeff = (* (dag-draw--binomial n i)
                           (expt t-val i)
                           (expt (- 1 t-val) (- n i)))
             sum (* coeff (car point)) into x
             sum (* coeff (cdr point)) into y
             finally return (cons x y))))
```

### Grid Path Construction

```elisp
(defun dag-draw--spline-to-grid-path (spline grid-scale)
  "Convert sampled SPLINE to grid path with orthogonal segments.
GRID-SCALE: coordinate-to-grid scaling info.
Returns list of (x . y) grid positions."
  (let* ((sampled (dag-draw--sample-spline spline 50))
         (grid-points (mapcar (lambda (pt)
                               (dag-draw--scale-point-to-grid pt grid-scale))
                             sampled))
         ;; Remove duplicate consecutive points
         (unique-points (dag-draw--remove-consecutive-dups grid-points)))
    unique-points))

(defun dag-draw--connect-grid-points (points edge grid)
  "Connect POINTS with orthogonal path, rendering on GRID.
Records EDGE in each visited cell."
  (cl-loop for i from 0 below (1- (length points))
           for p1 = (nth i points)
           for p2 = (nth (1+ i) points)
           do (dag-draw--draw-segment p1 p2 edge grid)))

(defun dag-draw--draw-segment (p1 p2 edge grid)
  "Draw segment from P1 to P2, recording EDGE in visited cells.
Uses Manhattan routing: horizontal then vertical, or vice versa."
  (let ((x1 (car p1)) (y1 (cdr p1))
        (x2 (car p2)) (y2 (cdr p2)))
    (cond
     ;; Same point: no-op
     ((and (= x1 x2) (= y1 y2)) nil)
     ;; Vertical only
     ((= x1 x2)
      (dag-draw--draw-vertical x1 y1 y2 edge grid))
     ;; Horizontal only
     ((= y1 y2)
      (dag-draw--draw-horizontal y1 x1 x2 edge grid))
     ;; Diagonal: route via intermediate point
     ;; Choose horizontal-first or vertical-first based on distance
     (t
      (if (> (abs (- x2 x1)) (abs (- y2 y1)))
          ;; Horizontal dominates: go horizontal first
          (progn
            (dag-draw--draw-horizontal y1 x1 x2 edge grid)
            (dag-draw--draw-vertical x2 y1 y2 edge grid))
        ;; Vertical dominates: go vertical first
        (progn
          (dag-draw--draw-vertical x1 y1 y2 edge grid)
          (dag-draw--draw-horizontal y2 x1 x2 edge grid)))))))

(defun dag-draw--draw-horizontal (y x1 x2 edge grid)
  "Draw horizontal segment from (x1,y) to (x2,y)."
  (let ((start (min x1 x2))
        (end (max x1 x2)))
    (cl-loop for x from start to end
             do (dag-draw--add-edge-to-cell x y edge :horizontal grid))))

(defun dag-draw--draw-vertical (x y1 y2 edge grid)
  "Draw vertical segment from (x,y1) to (x,y2)."
  (let ((start (min y1 y2))
        (end (max y1 y2)))
    (cl-loop for y from start to end
             do (dag-draw--add-edge-to-cell x y edge :vertical grid))))
```

### Grid Cell Structure

Each grid cell tracks:
- Character to display
- Edges passing through (list)
- Direction(s) of each edge (:horizontal, :vertical, or both)

```elisp
(cl-defstruct dag-draw-grid-cell
  char              ; Character to display
  edges             ; List of edges through this cell
  edge-directions   ; Hash table: edge -> direction (:h, :v, or :both)
  node              ; Node if this cell is part of node box
  )

(defun dag-draw--add-edge-to-cell (x y edge direction grid)
  "Add EDGE to cell at (X,Y) with DIRECTION."
  (let ((cell (or (dag-draw--get-cell grid x y)
                  (make-dag-draw-grid-cell
                   :char ?\s
                   :edges '()
                   :edge-directions (make-hash-table)))))
    ;; Add edge if not already present
    (unless (member edge (dag-draw-grid-cell-edges cell))
      (push edge (dag-draw-grid-cell-edges cell)))
    ;; Record direction
    (let ((existing-dir (gethash edge (dag-draw-grid-cell-edge-directions cell))))
      (puthash edge
               (cond
                ((null existing-dir) direction)
                ((eq existing-dir direction) direction)
                (t :both))  ; Edge goes both ways through this cell
               (dag-draw-grid-cell-edge-directions cell)))
    ;; Update character (will be refined by junction algorithm)
    (setf (dag-draw-grid-cell-char cell)
          (dag-draw--initial-edge-char direction))
    (dag-draw--set-cell grid x y cell)))

(defun dag-draw--initial-edge-char (direction)
  "Return initial character for edge segment in DIRECTION."
  (pcase direction
    (:horizontal ?─)
    (:vertical ?│)
    (:both ?┼)))
```

---

## Junction Character Rules

### Overview

Junction characters are applied at five types of locations:
1. **Port boundaries**: Where edges leave/enter nodes
2. **Direction changes**: Corners where edge changes from H to V or vice versa
3. **Merge/split points**: Where multiple edges share a path segment
4. **Cross points**: Where edges cross perpendicularly
5. **T-junctions**: Where an edge branches

### Algorithm

Walk each edge's grid path and analyze local context at each cell:

```elisp
(defun dag-draw--apply-junction-characters (graph grid)
  "Apply junction characters to all edges in GRAPH on GRID."
  (maphash (lambda (_id edge)
             (dag-draw--apply-edge-junctions edge grid))
           (dag-draw-graph-edges graph)))

(defun dag-draw--apply-edge-junctions (edge grid)
  "Walk EDGE path and set junction characters."
  (let ((path (dag-draw--get-edge-grid-path edge grid)))
    (cl-loop for i from 0 below (length path)
             for point = (nth i path)
             for prev = (when (> i 0) (nth (1- i) path))
             for next = (when (< i (1- (length path))) (nth (1+ i) path))
             do (dag-draw--set-junction-at-point
                 point prev next edge grid))))

(defun dag-draw--set-junction-at-point (point prev next edge grid)
  "Determine and set junction character at POINT for EDGE.
PREV and NEXT are adjacent points in edge path (or nil)."
  (let* ((x (car point))
         (y (cdr point))
         (cell (dag-draw--get-cell grid x y))
         (node (dag-draw-grid-cell-node cell)))
    (cond
     ;; Inside node box: don't modify
     (node nil)
     ;; Port boundary: check if at node boundary
     ((dag-draw--at-node-boundary-p x y grid)
      (dag-draw--set-port-junction point prev next edge grid))
     ;; Direction change (corner)
     ((and prev next (dag-draw--is-direction-change-p prev point next))
      (dag-draw--set-corner-junction prev point next cell grid))
     ;; Multiple edges at this point
     ((> (length (dag-draw-grid-cell-edges cell)) 1)
      (dag-draw--set-multi-edge-junction cell grid))
     ;; Single edge, straight segment: leave as is
     (t nil))))
```

### Port Boundaries

Port junctions occur where an edge exits or enters a node box:

```elisp
(defun dag-draw--set-port-junction (point prev next edge grid)
  "Set junction character at port boundary POINT."
  (let* ((x (car point))
         (y (cdr point))
         (cell (dag-draw--get-cell grid x y))
         (node (dag-draw--find-node-at-boundary x y grid)))
    (when node
      (let* ((box-left (dag-draw-node-box-left node))
             (box-right (dag-draw-node-box-right node))
             (box-top (dag-draw-node-box-top node))
             (box-bottom (dag-draw-node-box-bottom node))
             (at-top (= y box-top))
             (at-bottom (= y box-bottom))
             (at-left (= x box-left))
             (at-right (= x box-right))
             ;; Determine edge direction relative to node
             (edge-dir (dag-draw--edge-direction-at-port
                       point prev next)))
        ;; Select junction character based on position and direction
        (setf (dag-draw-grid-cell-char cell)
              (dag-draw--port-junction-char
               at-top at-bottom at-left at-right edge-dir))
        (dag-draw--set-cell grid x y cell)))))

(defun dag-draw--port-junction-char (at-top at-bottom at-left at-right
                                            edge-dir)
  "Select port junction character based on position and edge direction.
EDGE-DIR is one of :up, :down, :left, :right."
  (cond
   ;; Top edge
   (at-top
    (pcase edge-dir
      (:up ?┬)    ; Edge goes up from top border
      (:down ?─)  ; Edge comes down through top border (shouldn't happen)
      (:left ?┐)  ; Edge goes left from top border
      (:right ?┌) ; Edge goes right from top border
      (_ ?┬)))
   ;; Bottom edge
   (at-bottom
    (pcase edge-dir
      (:down ?┴)  ; Edge goes down from bottom border
      (:up ?─)    ; Edge comes up through bottom border (shouldn't happen)
      (:left ?┘)  ; Edge goes left from bottom border
      (:right ?└) ; Edge goes right from bottom border
      (_ ?┴)))
   ;; Left edge
   (at-left
    (pcase edge-dir
      (:left ?├)  ; Edge goes left from left border
      (:right ?│) ; Edge comes right through left border
      (:up ?└)    ; Edge goes up from left border
      (:down ?┌)  ; Edge goes down from left border
      (_ ?├)))
   ;; Right edge
   (at-right
    (pcase edge-dir
      (:right ?┤) ; Edge goes right from right border
      (:left ?│)  ; Edge comes left through right border
      (:up ?┘)    ; Edge goes up from right border
      (:down ?┐)  ; Edge goes down from right border
      (_ ?┤)))
   ;; Default
   (t ?┼)))
```

### Direction Changes (Corners)

```elisp
(defun dag-draw--is-direction-change-p (prev curr next)
  "Return t if PREV-CURR-NEXT forms a corner."
  (let ((dx-in (- (car curr) (car prev)))
        (dy-in (- (cdr curr) (cdr prev)))
        (dx-out (- (car next) (car curr)))
        (dy-out (- (cdr next) (cdr curr))))
    ;; Direction change if incoming and outgoing directions differ
    (not (or (and (zerop dy-in) (zerop dy-out))   ; Both horizontal
             (and (zerop dx-in) (zerop dx-out)))))) ; Both vertical

(defun dag-draw--set-corner-junction (prev curr next cell grid)
  "Set corner character at CURR based on PREV and NEXT."
  (let* ((dx-in (- (car curr) (car prev)))
         (dy-in (- (cdr curr) (cdr prev)))
         (dx-out (- (car next) (car curr)))
         (dy-out (- (cdr next) (cdr curr)))
         (char (dag-draw--select-corner-char dx-in dy-in dx-out dy-out)))
    (setf (dag-draw-grid-cell-char cell) char)
    (dag-draw--set-cell grid (car curr) (cdr curr) cell)))

(defun dag-draw--select-corner-char (dx-in dy-in dx-out dy-out)
  "Select appropriate corner character for direction change.
DX-IN, DY-IN: incoming direction (sign indicates direction)
DX-OUT, DY-OUT: outgoing direction"
  (let ((from-left (> dx-in 0))
        (from-right (< dx-in 0))
        (from-above (> dy-in 0))
        (from-below (< dy-in 0))
        (to-left (< dx-out 0))
        (to-right (> dx-out 0))
        (to-above (< dy-out 0))
        (to-below (> dy-out 0)))
    (cond
     ;; From horizontal to vertical
     ((and from-left to-below) ?┐)   ; ─┐
                                      ;  │
     ((and from-left to-above) ?┘)   ;  │
                                      ; ─┘
     ((and from-right to-below) ?┌)  ; ┌─
                                      ; │
     ((and from-right to-above) ?└)  ; │
                                      ; └─
     ;; From vertical to horizontal
     ((and from-above to-right) ?└)  ; │
                                      ; └─
     ((and from-above to-left) ?┘)   ;  │
                                      ; ─┘
     ((and from-below to-right) ?┌)  ; ┌─
                                      ; │
     ((and from-below to-left) ?┐)   ; ─┐
                                      ;  │
     ;; Shouldn't happen: default to cross
     (t ?┼))))
```

### Multi-Edge Junctions

When multiple edges pass through the same cell:

```elisp
(defun dag-draw--set-multi-edge-junction (cell grid)
  "Set junction character for cell with multiple edges.
Analyzes all edges to determine correct junction type."
  (let ((edges (dag-draw-grid-cell-edges cell))
        (directions (dag-draw-grid-cell-edge-directions cell))
        (has-h nil)
        (has-v nil)
        (h-count 0)
        (v-count 0))
    ;; Count edges in each direction
    (dolist (edge edges)
      (let ((dir (gethash edge directions)))
        (when (memq dir '(:horizontal :both))
          (setq has-h t)
          (cl-incf h-count))
        (when (memq dir '(:vertical :both))
          (setq has-v t)
          (cl-incf v-count))))
    ;; Select character
    (setf (dag-draw-grid-cell-char cell)
          (cond
           ;; Cross: edges in both directions
           ((and has-h has-v) ?┼)
           ;; T-junction variations detected by checking neighbors
           ((and has-h (> v-count 0))
            (dag-draw--detect-t-junction cell grid))
           ((and has-v (> h-count 0))
            (dag-draw--detect-t-junction cell grid))
           ;; Straight line with multiple edges overlapping
           (has-h ?─)
           (has-v ?│)
           ;; Default
           (t ?┼)))))

(defun dag-draw--detect-t-junction (cell grid)
  "Detect specific T-junction type by examining neighbors.
Returns appropriate T-junction character."
  (let* ((x (dag-draw-grid-cell-x cell))
         (y (dag-draw-grid-cell-y cell))
         (up (dag-draw--has-edge-p grid x (1- y)))
         (down (dag-draw--has-edge-p grid x (1+ y)))
         (left (dag-draw--has-edge-p grid (1- x) y))
         (right (dag-draw--has-edge-p grid (1+ x) y)))
    (cond
     ((and up down right (not left)) ?├)  ; ├ (missing left)
     ((and up down left (not right)) ?┤)  ; ┤ (missing right)
     ((and left right down (not up)) ?┬)  ; ┬ (missing up)
     ((and left right up (not down)) ?┴)  ; ┴ (missing down)
     (t ?┼))))  ; Full cross
```

### Cross Points

Crosses are detected when edges pass through the same cell in perpendicular directions:

```elisp
(defun dag-draw--is-cross-point-p (cell)
  "Return t if CELL has edges crossing perpendicularly."
  (let ((directions (dag-draw-grid-cell-edge-directions cell))
        (has-h nil)
        (has-v nil))
    (maphash (lambda (_edge dir)
               (when (memq dir '(:horizontal :both)) (setq has-h t))
               (when (memq dir '(:vertical :both)) (setq has-v t)))
             directions)
    (and has-h has-v)))
```

---

## Arrow Placement

### Overview

Arrows indicate edge direction and are placed at the head (destination) of each edge, adjacent to the node boundary.

### Algorithm

```elisp
(defun dag-draw--place-arrows (graph grid)
  "Place directional arrows at head of all edges."
  (maphash (lambda (_id edge)
             (unless (dag-draw-edge-reversed-p edge)
               ;; Place arrow at head
               (dag-draw--place-arrow edge :head grid))
             (when (dag-draw-edge-reversed-p edge)
               ;; For reversed edges, show arrow at original direction
               (dag-draw--place-arrow edge :tail grid)))
           (dag-draw-graph-edges graph)))

(defun dag-draw--place-arrow (edge end-type grid)
  "Place arrow for EDGE at END-TYPE (:head or :tail)."
  (let* ((node (if (eq end-type :head)
                  (dag-draw-edge-head edge)
                (dag-draw-edge-tail edge)))
         (path (dag-draw--get-edge-grid-path edge grid))
         ;; Find point where edge touches node boundary
         (boundary-point (if (eq end-type :head)
                           (car (last path))
                         (car path)))
         (prev-point (if (eq end-type :head)
                        (nth (- (length path) 2) path)
                      (nth 1 path)))
         (x (car boundary-point))
         (y (cdr boundary-point))
         ;; Determine arrow direction from path direction
         (arrow-char (dag-draw--arrow-char prev-point boundary-point)))
    ;; Place arrow
    (let ((cell (dag-draw--get-cell grid x y)))
      (setf (dag-draw-grid-cell-char cell) arrow-char)
      (dag-draw--set-cell grid x y cell))))

(defun dag-draw--arrow-char (from to)
  "Determine arrow character based on direction from FROM to TO."
  (let ((dx (- (car to) (car from)))
        (dy (- (cdr to) (cdr from))))
    (cond
     ((> dy 0) ?▼)   ; Going down
     ((< dy 0) ?▲)   ; Going up
     ((> dx 0) ?▶)   ; Going right
     ((< dx 0) ?◀)   ; Going left
     (t ?▼))))       ; Default down
```

### Arrow Position Refinement

Arrows should be placed just inside the node boundary when possible, or just outside if inside conflicts with node border:

```elisp
(defun dag-draw--refine-arrow-position (arrow-pos node edge grid)
  "Adjust arrow position to be optimally placed relative to node boundary.
Returns (x . y) for arrow placement."
  (let* ((box-left (dag-draw-node-box-left node))
         (box-right (dag-draw-node-box-right node))
         (box-top (dag-draw-node-box-top node))
         (box-bottom (dag-draw-node-box-bottom node))
         (x (car arrow-pos))
         (y (cdr arrow-pos)))
    ;; If arrow is on border, move it one cell toward edge
    (cond
     ((= y box-top) (cons x (1- y)))       ; Top border: arrow above
     ((= y box-bottom) (cons x (1+ y)))    ; Bottom border: arrow below
     ((= x box-left) (cons (1- x) y))      ; Left border: arrow left
     ((= x box-right) (cons (1+ x) y))     ; Right border: arrow right
     (t arrow-pos))))  ; Inside box: keep position
```

---

## Edge Type Handling

### Inter-Rank Edges (Standard)

Most common edge type. Routing follows spline approximation through virtual nodes if needed.

**Example:**
```
┌─────┐
│  A  │
└──┬──┘
   │
   │
   ▼
┌─────┐
│  B  │
└─────┘
```

### Flat Edges (Same Rank)

Edges between nodes on the same rank:

```elisp
(defun dag-draw--route-flat-edge (edge grid)
  "Route flat edge (between nodes on same rank)."
  (let* ((tail (dag-draw-edge-tail edge))
         (head (dag-draw-edge-head edge))
         ;; Check if adjacent
         (tail-order (dag-draw-node-order tail))
         (head-order (dag-draw-node-order head)))
    (if (= (abs (- tail-order head-order)) 1)
        ;; Adjacent: simple horizontal line above nodes
        (dag-draw--route-adjacent-flat-edge edge grid)
      ;; Non-adjacent: route around nodes
      (dag-draw--route-non-adjacent-flat-edge edge grid))))

(defun dag-draw--route-adjacent-flat-edge (edge grid)
  "Route flat edge between adjacent nodes."
  (let* ((tail (dag-draw-edge-tail edge))
         (head (dag-draw-edge-head edge))
         (y (1- (dag-draw-node-box-top tail)))  ; Above nodes
         (x1 (dag-draw-node-x tail))
         (x2 (dag-draw-node-x head)))
    ;; Draw horizontal line
    (dag-draw--draw-horizontal y x1 x2 edge grid)
    ;; Add downward connections to nodes
    (dag-draw--draw-vertical x1 y (dag-draw-node-box-top tail) edge grid)
    (dag-draw--draw-vertical x2 y (dag-draw-node-box-top head) edge grid)))
```

**Example:**
```
  ┌────┐    ┌────┐
  │ A  ├────┤ B  │
  └────┘    └────┘
```

### Self-Loops

Edges from a node to itself:

```elisp
(defun dag-draw--route-self-loop (edge grid)
  "Route self-loop (edge from node to itself)."
  (let* ((node (dag-draw-edge-tail edge))
         (x (dag-draw-node-box-right node))
         (y (dag-draw-node-y node))
         (loop-width 3))
    ;; Draw loop on right side of node
    ;; Upward segment
    (dag-draw--draw-vertical x y (- y 1) edge grid)
    ;; Top horizontal
    (dag-draw--draw-horizontal (- y 1) x (+ x loop-width) edge grid)
    ;; Downward segment
    (dag-draw--draw-vertical (+ x loop-width) (- y 1) (+ y 1) edge grid)
    ;; Bottom horizontal back to node
    (dag-draw--draw-horizontal (+ y 1) (+ x loop-width) x edge grid)
    ;; Connection to node
    (dag-draw--draw-vertical x (+ y 1) y edge grid)))
```

**Example:**
```
  ┌────┐─┐
  │ A  │ │
  └────┘─┘
```

### Multi-Edges

Multiple edges between the same pair of nodes:

```elisp
(defun dag-draw--route-multi-edges (edges grid)
  "Route multiple edges between same pair of nodes.
EDGES: list of edges with same tail and head."
  (let ((spacing (dag-draw-graph-nodesep graph)))
    (cl-loop for edge in edges
             for offset from 0
             do (dag-draw--route-edge-with-offset
                 edge (* offset spacing) grid))))

(defun dag-draw--route-edge-with-offset (edge offset grid)
  "Route EDGE with horizontal OFFSET for multi-edge spacing."
  (let ((spline (dag-draw-edge-control-points edge)))
    ;; Add offset to all X coordinates
    (let ((offset-spline (mapcar (lambda (pt)
                                  (cons (+ (car pt) offset) (cdr pt)))
                                spline)))
      (dag-draw--route-spline offset-spline edge grid))))
```

**Example:**
```
  ┌────┐
  │ A  │
  └─┬┬─┘
    ││
    ││
    ▼▼
  ┌────┐
  │ B  │
  └────┘
```

---

## Label Rendering

### Node Labels

Rendered inside node boxes (as shown in node rendering section above).

### Edge Labels

Edge labels are handled as special virtual nodes per GKNV Section 5.3:

```elisp
(defun dag-draw--render-edge-label (label-node edge grid)
  "Render edge label as virtual node."
  (let* ((x (dag-draw-node-x label-node))
         (y (dag-draw-node-y label-node))
         (label (dag-draw-node-label label-node))
         (len (length label)))
    ;; Place label text without box
    (cl-loop for i from 0 below len
             do (dag-draw--set-grid grid (+ x i) y (aref label i)))))
```

**Example:**
```
┌─────┐
│  A  │
└──┬──┘
   │
 label
   │
   ▼
┌─────┐
│  B  │
└─────┘
```

---

## Examples

### Simple Tree

```
     ┌───────┐
     │ Root  │
     └───┬───┘
         │
    ┌────┴────┐
    │         │
    ▼         ▼
┌───────┐ ┌───────┐
│ Left  │ │ Right │
└───────┘ └───────┘
```

### Diamond

```
  ┌────┐
  │ A  │
  └─┬──┘
    │
  ┌─┴─┐
  │   │
  ▼   ▼
┌───┐ ┌───┐
│ B │ │ C │
└─┬─┘ └─┬─┘
  │     │
  └──┬──┘
     │
     ▼
  ┌────┐
  │ D  │
  └────┘
```

### Complex Routing

```
┌───┐     ┌───┐
│ A │     │ B │
└─┬─┘     └─┬─┘
  │         │
  │    ┌────┘
  │    │
  └────┼───┐
       │   │
       ▼   ▼
     ┌───┐ ┌───┐
     │ C │ │ D │
     └───┘ └───┘
```

With junction characters:
```
┌───┐     ┌───┐
│ A │     │ B │
└─┬─┘     └─┬─┘
  │         │
  │    ┌────┘
  │    │
  └────┼───┐
       │   │
       ▼   ▼
     ┌───┐ ┌───┐
     │ C │ │ D │
     └───┘ └───┘
```

### Self-Loop

```
  ┌────┐─┐
  │ A  │ │
  └──┬─┘─┘
     │
     ▼
  ┌────┐
  │ B  │
  └────┘
```

---

## Implementation Checklist

- [ ] Coordinate scaling (bounds, scale factors, integer rounding)
- [ ] Grid data structure (cells with char, edges, directions)
- [ ] Node rendering (boxes with borders and labels)
- [ ] Virtual node sizing
- [ ] Spline sampling
- [ ] Grid path construction
- [ ] Orthogonal segment drawing
- [ ] Edge tracking in cells
- [ ] Junction detection (5 types)
- [ ] Port junction characters
- [ ] Corner junction characters
- [ ] Multi-edge junction characters
- [ ] T-junction detection
- [ ] Cross detection
- [ ] Arrow placement
- [ ] Arrow direction calculation
- [ ] Inter-rank edge routing
- [ ] Flat edge routing (adjacent and non-adjacent)
- [ ] Self-loop routing
- [ ] Multi-edge offset routing
- [ ] Edge label rendering
- [ ] Grid-to-string conversion
- [ ] Unicode character output
- [ ] ASCII fallback mode

---

**Document Version**: 1.0
**Date**: 2025-10-13
**Author**: dag-draw.el development team
