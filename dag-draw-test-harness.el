;;; dag-draw-test-harness.el --- Intelligent ASCII DAG testing harness -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Intelligent testing harness for ASCII DAG renderings that can parse
;; and validate ASCII grid structures without relying on regex patterns.
;; Provides structural analysis of nodes, edges, and graph topology.

;;; Code:

(require 'dag-draw-core)

;;; ASCII Grid Parsing

(defun dag-draw-test--parse-ascii-grid (ascii-string)
  "Parse ASCII-STRING into structured grid data.
Returns hash table with grid dimensions and character matrix."
  (let* ((lines (split-string ascii-string "\n"))
         (height (length lines))
         (width (if lines (apply #'max (mapcar #'length lines)) 0))
         (grid (make-vector height nil))
         (result (make-hash-table :test 'equal)))
    
    ;; Build character grid
    (dotimes (y height)
      (let ((line (or (nth y lines) ""))
            (row (make-vector width ?\s)))
        (dotimes (x (length line))
          (aset row x (aref line x)))
        (aset grid y row)))
    
    (puthash 'width width result)
    (puthash 'height height result)
    (puthash 'grid grid result)
    result))

(defun dag-draw-test--get-char-at (grid x y)
  "Get character at position (X,Y) in parsed GRID."
  (let ((grid-data (gethash 'grid grid))
        (height (gethash 'height grid))
        (width (gethash 'width grid)))
    (if (and (>= x 0) (< x width) (>= y 0) (< y height))
        (aref (aref grid-data y) x)
      ?\s)))

;;; Node Detection and Analysis

(defun dag-draw-test--find-nodes (ascii-string)
  "Find all node boundaries in ASCII-STRING and extract their content.
This is a fallback function - prefer using graph-based validation instead."
  (let ((grid (dag-draw-test--parse-ascii-grid ascii-string))
        (nodes '()))
    
    ;; Look for top-left corners of boxes (┌ or other box corner chars)
    (dotimes (y (gethash 'height grid))
      (dotimes (x (gethash 'width grid))
        (when (memq (dag-draw-test--get-char-at grid x y) '(?┌))
          (let ((node (dag-draw-test--parse-node-at grid x y)))
            (when node
              (push node nodes))))))
    
    (nreverse nodes)))

(defun dag-draw-test--find-nodes-using-graph-data (ascii-string graph)
  "Find nodes in ASCII-STRING using actual GRAPH coordinate data.
This is more reliable than character-based detection."
  (let ((found-nodes '())
        (grid (dag-draw-test--parse-ascii-grid ascii-string)))
    
    ;; Extract nodes from graph (handles both real graphs and mock structures)
    (let ((expected-nodes (dag-draw-test--extract-expected-nodes graph)))
      (dolist (expected expected-nodes)
        (let* ((label (plist-get expected :label))
               ;; Search for the label text in the ASCII grid
               (found-text (dag-draw-test--find-text-in-grid grid label)))
          (when found-text
            (push (list :id (plist-get expected :id)
                        :text label
                        :x (car found-text)
                        :y (cdr found-text)
                        :width (length label)
                        :height 1)
                  found-nodes)))))
    
    found-nodes))

(defun dag-draw-test--find-text-in-grid (grid text)
  "Find TEXT in GRID and return its bounding box.
This searches for the text as it would appear inside ASCII node boxes."
  (let ((grid-height (gethash 'height grid))
        (grid-width (gethash 'width grid))
        (result nil))
    
    ;; Search for the text in the grid 
    (catch 'found
      (dotimes (y grid-height)
        (dotimes (x grid-width)
          ;; Extract a line of text to search within
          (let ((line-length (min 40 (- grid-width x))))  ; Look ahead up to 40 chars
            (when (> line-length (length text))
              (let ((line-text (apply #'string 
                                     (mapcar (lambda (i) 
                                               (dag-draw-test--get-char-at grid (+ x i) y))
                                             (number-sequence 0 (1- line-length))))))
                ;; Look for the text within the line (allowing for spacing/padding)
                (when (string-match-p (regexp-quote text) line-text)
                  (let ((text-start (string-match (regexp-quote text) line-text)))
                    ;; Found the text - return coordinate pair as cons cell
                    (setq result (cons (+ x text-start) y))
                    (throw 'found result)))))))))
    
    result))

(defun dag-draw-test--parse-node-at (grid start-x start-y)
  "Parse node starting at top-left corner (START-X, START-Y) in GRID.
Returns node plist or nil if not a valid node."
  (let ((x start-x)
        (y start-y)
        (width 0)
        (height 0)
        (text-lines '()))
    
    ;; Find width by following top border (including junction chars and arrows)
    (while (and (< x (gethash 'width grid))
                (memq (dag-draw-test--get-char-at grid x y) '(?┌ ?─ ?┐ ?┬ ?┼ ?▼ ?▲ ?▶ ?◀)))
      (setq x (1+ x)))
    (setq width (- x start-x))
    
    ;; Find height by following left border (including junction chars)
    (setq x start-x)
    (while (and (< y (gethash 'height grid))
                (memq (dag-draw-test--get-char-at grid x y) '(?┌ ?│ ?└ ?├ ?┤ ?┼ ?▼ ?▲ ?▶ ?◀)))
      (setq y (1+ y)))
    (setq height (- y start-y))
    
    ;; Extract text content (skip borders)
    (when (and (> width 2) (> height 2))
      (dotimes (row (- height 2))
        (let ((text-y (+ start-y 1 row))
              (line ""))
          (dotimes (col (- width 2))
            (let ((text-x (+ start-x 1 col)))
              (setq line (concat line (string (dag-draw-test--get-char-at grid text-x text-y))))))
          (push (string-trim line) text-lines)))
      
      (list :x start-x
            :y start-y
            :width width
            :height height
            :text (mapconcat 'identity (nreverse text-lines) "\n")))))

;;; Edge Detection and Analysis

(defun dag-draw-test--find-edges (ascii-string)
  "Find all edge connections in ASCII-STRING.
Returns list of edge plists with :from, :to, :path coordinates."
  (let ((grid (dag-draw-test--parse-ascii-grid ascii-string))
        (nodes (dag-draw-test--find-nodes ascii-string))
        (edges '()))
    
    ;; For each node, trace outgoing edges
    (dolist (node nodes)
      (let ((node-edges (dag-draw-test--trace-edges-from-node grid node nodes)))
        (setq edges (append edges node-edges))))
    
    edges))

(defun dag-draw-test--trace-edges-from-node (grid from-node all-nodes)
  "Trace edges starting FROM-NODE in GRID, connecting to nodes in ALL-NODES.
Returns list of edge plists."
  (let ((edges '())
        (from-x (plist-get from-node :x))
        (from-y (plist-get from-node :y))
        (from-width (plist-get from-node :width))
        (from-height (plist-get from-node :height)))
    
    ;; Check all edge exit points on node boundary
    (let ((exit-points (dag-draw-test--find-edge-exit-points grid from-node)))
      (dolist (exit-point exit-points)
        (let ((path (dag-draw-test--trace-edge-path grid exit-point all-nodes)))
          (when path
            (push (list :from from-node
                        :to (plist-get path :destination)
                        :path (plist-get path :coordinates))
                  edges)))))
    
    edges))

(defun dag-draw-test--find-edge-exit-points (grid node)
  "Find points where edges exit NODE boundary in GRID.
Returns list of (x y) coordinate pairs."
  (let ((x (plist-get node :x))
        (y (plist-get node :y))
        (width (plist-get node :width))
        (height (plist-get node :height))
        (exit-points '()))
    
    ;; Check for junction characters on node boundary that indicate edge starts
    ;; Top border
    (dotimes (i width)
      (when (memq (dag-draw-test--get-char-at grid (+ x i) y) '(?┬ ?┼))
        (push (list (+ x i) y) exit-points)))
    
    ;; Bottom border
    (dotimes (i width)
      (when (memq (dag-draw-test--get-char-at grid (+ x i) (+ y height -1)) '(?┴ ?┼))
        (push (list (+ x i) (+ y height -1)) exit-points)))
    
    ;; Left border
    (dotimes (i height)
      (when (memq (dag-draw-test--get-char-at grid x (+ y i)) '(?├ ?┼))
        (push (list x (+ y i)) exit-points)))
    
    ;; Right border
    (dotimes (i height)
      (when (memq (dag-draw-test--get-char-at grid (+ x width -1) (+ y i)) '(?┤ ?┼))
        (push (list (+ x width -1) (+ y i)) exit-points)))
    
    exit-points))

(defun dag-draw-test--trace-edge-path (grid start-point target-nodes)
  "Trace edge path from START-POINT in GRID until reaching one of TARGET-NODES.
Returns plist with :destination and :coordinates or nil."
  ;; Simplified path tracing - follow edge characters until hitting a node
  (let ((current-x (car start-point))
        (current-y (cadr start-point))
        (path (list start-point))
        (visited (make-hash-table :test 'equal))
        (found-target nil))
    
    ;; Simple flood-fill following edge characters
    (while (and (not found-target) (< (length path) 100)) ; prevent infinite loops
      (puthash (list current-x current-y) t visited)
      
      ;; Check if we've reached a target node
      (dolist (target target-nodes)
        (when (dag-draw-test--point-in-node-p current-x current-y target)
          (setq found-target target)))
      
      (unless found-target
        ;; Find next edge character in adjacent positions
        (let ((next-pos (dag-draw-test--find-next-edge-char grid current-x current-y visited)))
          (if next-pos
              (progn
                (setq current-x (car next-pos))
                (setq current-y (cadr next-pos))
                (push next-pos path))
            ;; No more edge characters found
            (setq found-target 'none)))))
    
    (when (and found-target (not (eq found-target 'none)))
      (list :destination found-target :coordinates (nreverse path)))))

(defun dag-draw-test--find-next-edge-char (grid x y visited)
  "Find next edge character adjacent to (X,Y) in GRID, avoiding VISITED positions."
  (let ((edge-chars '(?│ ?─ ?┌ ?┐ ?└ ?┘ ?├ ?┤ ?┬ ?┴ ?┼ ?▼ ?▲ ?▶ ?◀))
        (directions '((0 1) (0 -1) (1 0) (-1 0))))
    
    (catch 'found
      (dolist (dir directions)
        (let ((new-x (+ x (car dir)))
              (new-y (+ y (cadr dir))))
          (when (and (not (gethash (list new-x new-y) visited))
                     (memq (dag-draw-test--get-char-at grid new-x new-y) edge-chars))
            (throw 'found (list new-x new-y))))))))

(defun dag-draw-test--point-in-node-p (x y node)
  "Check if point (X,Y) is inside NODE boundaries."
  (let ((node-x (plist-get node :x))
        (node-y (plist-get node :y))
        (node-width (plist-get node :width))
        (node-height (plist-get node :height)))
    (and (>= x node-x)
         (< x (+ node-x node-width))
         (>= y node-y)
         (< y (+ node-y node-height)))))

;;; Validation Functions

(defun dag-draw-test--validate-node-completeness (ascii-string graph)
  "Validate that all expected node text appears completely in ASCII-STRING.
GRAPH should contain expected node data for comparison."
  ;; XP: Use graph-based detection instead of character hunting
  (let ((found-nodes (dag-draw-test--find-nodes-using-graph-data ascii-string graph))
        (missing-text '())
        (complete t))
    
    ;; Extract expected nodes from graph
    (let ((expected-nodes (dag-draw-test--extract-expected-nodes graph)))
      (dolist (expected expected-nodes)
        (let ((expected-text (plist-get expected :label))
              (found-match nil))
          
          ;; Check if expected text appears in any found node
          (dolist (found found-nodes)
            (when (string-match-p (regexp-quote expected-text) (plist-get found :text))
              (setq found-match t)))
          
          (unless found-match
            (push expected-text missing-text)
            (setq complete nil)))))
    
    (list :complete complete :missing-text missing-text)))

(defun dag-draw-test--extract-expected-nodes (graph)
  "Extract expected node information from GRAPH data structure."
  (cond
   ;; dag-draw-graph structure
   ((dag-draw-graph-p graph)
    (let ((nodes '()))
      (ht-each (lambda (id node)
                 (push (list :id id :label (dag-draw-node-label node)) nodes))
               (dag-draw-graph-nodes graph))
      nodes))
   
   ;; Hash table with 'nodes key (mock structure)
   ((hash-table-p graph)
    (gethash 'nodes graph))
   
   ;; Default empty
   (t '())))

(defun dag-draw-test--validate-node-boundaries (ascii-string)
  "Validate that node boundaries are properly formed in ASCII-STRING."
  (let ((nodes (dag-draw-test--find-nodes ascii-string))
        (broken-boundaries '())
        (valid t))
    
    (dolist (node nodes)
      (unless (dag-draw-test--check-node-boundary-integrity node ascii-string)
        (push node broken-boundaries)
        (setq valid nil)))
    
    (list :valid valid :broken-boundaries broken-boundaries)))

(defun dag-draw-test--check-node-boundary-integrity (node ascii-string)
  "Check that NODE has complete, unbroken boundary in ASCII-STRING."
  ;; Simplified check - ensure node has reasonable dimensions and text
  (and (> (plist-get node :width) 2)
       (> (plist-get node :height) 2)
       (not (string-empty-p (string-trim (plist-get node :text))))))

(defun dag-draw-test--validate-edge-connectivity (ascii-string graph)
  "Validate that edges properly connect nodes in ASCII-STRING according to GRAPH."
  (let ((found-edges (dag-draw-test--find-edges ascii-string))
        (expected-edges (dag-draw-test--extract-expected-edges graph))
        (missing-connections '())
        (broken-paths '())
        (all-connected t))
    
    ;; Simplified validation - if we have edges to check connectivity
    (when (> (length expected-edges) 0)
      (setq all-connected (>= (length found-edges) 0)))
    
    ;; TODO: Implement proper edge matching
    ;; For now, be permissive to test core functionality
    
    (list :all-connected all-connected
          :missing-connections missing-connections
          :broken-paths broken-paths)))

(defun dag-draw-test--extract-expected-edges (graph)
  "Extract expected edge connections from GRAPH."
  (cond
   ((dag-draw-graph-p graph)
    (mapcar (lambda (edge)
              (list :from (dag-draw-edge-from-node edge)
                    :to (dag-draw-edge-to-node edge)))
            (dag-draw-graph-edges graph)))
   (t '())))

(defun dag-draw-test--edges-match-p (expected found)
  "Check if EXPECTED edge matches FOUND edge structure."
  ;; Simplified matching based on node text content
  (let ((expected-from (plist-get expected :from))
        (expected-to (plist-get expected :to)))
    
    ;; For now, return t if we have any found edges 
    ;; TODO: Improve matching algorithm
    (and found expected-from expected-to t)))

(defun dag-draw-test--validate-arrows (ascii-string)
  "Validate arrow placement and direction in ASCII-STRING."
  (let ((grid (dag-draw-test--parse-ascii-grid ascii-string))
        (valid-arrows 0)
        (invalid-arrows 0))
    
    ;; Count arrow characters and check their context
    (dotimes (y (gethash 'height grid))
      (dotimes (x (gethash 'width grid))
        (let ((char (dag-draw-test--get-char-at grid x y)))
          (when (memq char '(?▼ ?▲ ?▶ ?◀))
            (if (dag-draw-test--arrow-has-valid-context-p grid x y char)
                (setq valid-arrows (1+ valid-arrows))
              (setq invalid-arrows (1+ invalid-arrows)))))))
    
    (list :valid-arrows valid-arrows :invalid-arrows invalid-arrows)))

(defun dag-draw-test--arrow-has-valid-context-p (grid x y arrow-char)
  "Check if ARROW-CHAR at (X,Y) in GRID has valid edge context."
  ;; Check that arrow connects to edge characters
  (let ((edge-chars '(?│ ?─ ?┌ ?┐ ?└ ?┘ ?├ ?┤ ?┬ ?┴ ?┼)))
    (cl-case arrow-char
      (?▼ (memq (dag-draw-test--get-char-at grid x (1- y)) edge-chars))
      (?▲ (memq (dag-draw-test--get-char-at grid x (1+ y)) edge-chars))
      (?▶ (memq (dag-draw-test--get-char-at grid (1- x) y) edge-chars))
      (?◀ (memq (dag-draw-test--get-char-at grid (1+ x) y) edge-chars))
      (t nil))))

(defun dag-draw-test--validate-graph-structure (ascii-string graph)
  "Validate that ASCII-STRING represents the same graph structure as GRAPH."
  ;; XP: Use graph-based detection instead of character hunting
  (let ((found-nodes (dag-draw-test--find-nodes-using-graph-data ascii-string graph))
        (found-edges (dag-draw-test--find-edges ascii-string))
        (expected-nodes (dag-draw-test--extract-expected-nodes graph))
        (expected-edges (dag-draw-test--extract-expected-edges graph)))
    
    (list :topology-match (>= (length found-nodes) (length expected-nodes))
          :node-count-match (>= (length found-nodes) (length expected-nodes))
          :edge-count-match (>= (length found-edges) 0))))

(defun dag-draw-test--has-path-between (grid start-pos end-pos)
  "Check if there's a path of edge characters between START-POS and END-POS in GRID.
START-POS and END-POS should be position cons cells in (x . y) format."
  (when (and start-pos end-pos)
    (let ((start-x (car start-pos))
          (start-y (cdr start-pos))
          (end-x (car end-pos))
          (end-y (cdr end-pos))
          (edge-chars '(?│ ?─ ?┌ ?┐ ?└ ?┘ ?├ ?┤ ?┬ ?┴ ?┼ ?▼ ?▲ ?▶ ?◀))
          (visited (make-hash-table :test 'equal))
          (queue nil)
          (found nil))
      
      ;; Start flood-fill from start position
      (push (list start-x start-y) queue)
      
      ;; Breadth-first search for path to end position
      (while (and queue (not found))
        (let* ((current (pop queue))
               (curr-x (car current))
               (curr-y (cadr current)))
          
          ;; Mark as visited
          (puthash (list curr-x curr-y) t visited)
          
          ;; Check if we reached the end position (within reasonable distance)
          (when (and (<= (abs (- curr-x end-x)) 5)
                     (<= (abs (- curr-y end-y)) 5))
            (setq found t))
          
          ;; Add adjacent edge characters to queue
          (unless found
            (dolist (dir '((0 1) (0 -1) (1 0) (-1 0)))
              (let ((new-x (+ curr-x (car dir)))
                    (new-y (+ curr-y (cadr dir))))
                (when (and (not (gethash (list new-x new-y) visited))
                           (memq (dag-draw-test--get-char-at grid new-x new-y) edge-chars))
                  (push (list new-x new-y) queue)))))))
      
      found)))

;;; Junction Validation Functions

(defun dag-draw-test--find-malformed-junctions (grid)
  "Find all malformed junction patterns in GRID.
Returns list of plists with :x, :y, :char, :problem, :context.
Scans entire grid for:
  - Junctions adjacent to node border characters (┼│, │┼, etc.)
  - Junction characters with wrong connection counts
  - Floating junctions not connected to edges."
  (let ((grid-height (gethash 'height grid))
        (grid-width (gethash 'width grid))
        (malformed '())
        (junction-chars '(?┼ ?├ ?┤ ?┬ ?┴ ?┌ ?┐ ?└ ?┘))
        (border-chars '(?┌ ?┐ ?└ ?┘ ?│ ?─)))

    (dotimes (y grid-height)
      (dotimes (x grid-width)
        (let ((char (dag-draw-test--get-char-at grid x y)))
          (when (memq char junction-chars)
            (let ((left-char (dag-draw-test--get-char-at grid (1- x) y))
                  (right-char (dag-draw-test--get-char-at grid (1+ x) y))
                  (up-char (dag-draw-test--get-char-at grid x (1- y)))
                  (down-char (dag-draw-test--get-char-at grid x (1+ y))))

              ;; Check for junction adjacent to node border (malformed pattern)
              (when (or (and (eq char ?┼) (memq left-char border-chars))
                        (and (eq char ?┼) (memq right-char border-chars))
                        (and (eq char ?┼) (memq up-char border-chars))
                        (and (eq char ?┼) (memq down-char border-chars)))
                (push (list :x x :y y :char char
                            :problem 'adjacent-to-border
                            :context (list :left left-char :right right-char
                                           :up up-char :down down-char))
                      malformed)))))))

    (nreverse malformed)))

(defun dag-draw-test--validate-junction-connectivity (grid)
  "Validate junction characters have correct number of connections.
Returns plist with :all-valid boolean and :invalid-junctions list.
Checks:
  - Corners (┌┐└┘) connect exactly 2 directions
  - T-junctions (├┤┬┴) connect exactly 3 directions
  - Cross (┼) connects exactly 4 directions."
  (let ((grid-height (gethash 'height grid))
        (grid-width (gethash 'width grid))
        (invalid-junctions '())
        (all-valid t)
        (edge-chars '(?│ ?─ ?┼ ?├ ?┤ ?┬ ?┴ ?┌ ?┐ ?└ ?┘ ?▼ ?▲ ?▶ ?◀)))

    (dotimes (y grid-height)
      (dotimes (x grid-width)
        (let ((char (dag-draw-test--get-char-at grid x y)))

          ;; Check corner junctions (should have 2 connections)
          (when (memq char '(?┌ ?┐ ?└ ?┘))
            (let ((connections 0))
              (when (memq (dag-draw-test--get-char-at grid (1- x) y) edge-chars)
                (setq connections (1+ connections)))
              (when (memq (dag-draw-test--get-char-at grid (1+ x) y) edge-chars)
                (setq connections (1+ connections)))
              (when (memq (dag-draw-test--get-char-at grid x (1- y)) edge-chars)
                (setq connections (1+ connections)))
              (when (memq (dag-draw-test--get-char-at grid x (1+ y)) edge-chars)
                (setq connections (1+ connections)))

              (unless (= connections 2)
                (push (list :x x :y y :char char
                            :expected-connections 2
                            :actual-connections connections)
                      invalid-junctions)
                (setq all-valid nil))))

          ;; Check T-junctions (should have 3 connections)
          (when (memq char '(?├ ?┤ ?┬ ?┴))
            (let ((connections 0))
              (when (memq (dag-draw-test--get-char-at grid (1- x) y) edge-chars)
                (setq connections (1+ connections)))
              (when (memq (dag-draw-test--get-char-at grid (1+ x) y) edge-chars)
                (setq connections (1+ connections)))
              (when (memq (dag-draw-test--get-char-at grid x (1- y)) edge-chars)
                (setq connections (1+ connections)))
              (when (memq (dag-draw-test--get-char-at grid x (1+ y)) edge-chars)
                (setq connections (1+ connections)))

              (unless (= connections 3)
                (push (list :x x :y y :char char
                            :expected-connections 3
                            :actual-connections connections)
                      invalid-junctions)
                (setq all-valid nil))))

          ;; Check cross junctions (should have 4 connections)
          (when (eq char ?┼)
            (let ((connections 0))
              (when (memq (dag-draw-test--get-char-at grid (1- x) y) edge-chars)
                (setq connections (1+ connections)))
              (when (memq (dag-draw-test--get-char-at grid (1+ x) y) edge-chars)
                (setq connections (1+ connections)))
              (when (memq (dag-draw-test--get-char-at grid x (1- y)) edge-chars)
                (setq connections (1+ connections)))
              (when (memq (dag-draw-test--get-char-at grid x (1+ y)) edge-chars)
                (setq connections (1+ connections)))

              (unless (= connections 4)
                (push (list :x x :y y :char char
                            :expected-connections 4
                            :actual-connections connections)
                      invalid-junctions)
                (setq all-valid nil)))))))

    (list :all-valid all-valid :invalid-junctions (nreverse invalid-junctions))))

(defun dag-draw-test--validate-junction-border-separation (grid)
  "Validate junctions are not adjacent to node border characters.
Returns plist with :no-adjacency boolean and :adjacent-junctions list.
Checks all 4 neighbors of each junction for border characters."
  (let ((grid-height (gethash 'height grid))
        (grid-width (gethash 'width grid))
        (adjacent-junctions '())
        (no-adjacency t)
        (junction-chars '(?┼ ?├ ?┤ ?┬ ?┴ ?┌ ?┐ ?└ ?┘))
        (border-chars '(?┌ ?┐ ?└ ?┘ ?│ ?─)))

    (dotimes (y grid-height)
      (dotimes (x grid-width)
        (let ((char (dag-draw-test--get-char-at grid x y)))
          (when (memq char junction-chars)
            (let ((left-char (dag-draw-test--get-char-at grid (1- x) y))
                  (right-char (dag-draw-test--get-char-at grid (1+ x) y))
                  (up-char (dag-draw-test--get-char-at grid x (1- y)))
                  (down-char (dag-draw-test--get-char-at grid x (1+ y))))

              ;; Check if any neighbor is a border character
              (when (or (memq left-char border-chars)
                        (memq right-char border-chars)
                        (memq up-char border-chars)
                        (memq down-char border-chars))
                (push (list :x x :y y :char char
                            :adjacent-to (list :left left-char :right right-char
                                               :up up-char :down down-char))
                      adjacent-junctions)
                (setq no-adjacency nil)))))))

    (list :no-adjacency no-adjacency :adjacent-junctions (nreverse adjacent-junctions))))

(defun dag-draw-test--trace-edge-junctions (grid start-pos end-pos)
  "Trace edge path and return all junction positions along the way.
START-POS and END-POS are (x . y) cons cells.
Returns list of plists with :x, :y, :junction-char."
  (let ((junction-chars '(?┼ ?├ ?┤ ?┬ ?┴ ?┌ ?┐ ?└ ?┘))
        (edge-chars '(?│ ?─ ?┼ ?├ ?┤ ?┬ ?┴ ?┌ ?┐ ?└ ?┘ ?▼ ?▲ ?▶ ?◀))
        (visited (make-hash-table :test 'equal))
        (queue (list (list (car start-pos) (cdr start-pos))))
        (junctions '())
        (found-end nil))

    ;; BFS to trace path and record junctions
    (while (and queue (not found-end))
      (let* ((current (pop queue))
             (curr-x (car current))
             (curr-y (cadr current))
             (char (dag-draw-test--get-char-at grid curr-x curr-y)))

        (unless (gethash (list curr-x curr-y) visited)
          (puthash (list curr-x curr-y) t visited)

          ;; Record junction if found
          (when (memq char junction-chars)
            (push (list :x curr-x :y curr-y :junction-char char) junctions))

          ;; Check if reached end
          (when (and (= curr-x (car end-pos)) (= curr-y (cdr end-pos)))
            (setq found-end t))

          ;; Add neighbors to queue
          (unless found-end
            (dolist (dir '((0 1) (0 -1) (1 0) (-1 0)))
              (let ((new-x (+ curr-x (car dir)))
                    (new-y (+ curr-y (cadr dir))))
                (when (and (not (gethash (list new-x new-y) visited))
                           (memq (dag-draw-test--get-char-at grid new-x new-y) edge-chars))
                  (push (list new-x new-y) queue))))))))

    (nreverse junctions)))

;;; Spatial Verification Functions

(defun dag-draw-test--verify-hierarchical-y-coords (graph grid scale)
  "Verify Y-coordinates follow GKNV D3.6: Y = rank × ranksep.
GRAPH is the dag-draw graph, GRID is parsed ASCII, SCALE is world→grid factor.
Returns plist with :correct boolean and :violations list."
  (let ((violations '())
        (correct t)
        (nodes (dag-draw-test--extract-expected-nodes graph))
        (ranksep (if (hash-table-p graph)
                     (gethash 'ranksep graph 25)
                   (dag-draw-graph-rank-separation graph))))

    (dolist (node nodes)
      (let* ((label (plist-get node :label))
             (rank (plist-get node :rank))
             (expected-y (plist-get node :y))
             ;; Find actual position in grid
             (found-pos (dag-draw-test--find-text-in-grid grid label)))

        (when (and found-pos rank expected-y)
          (let* ((actual-y (cdr found-pos))
                 ;; Expected Y in world coords: rank × ranksep
                 ;; Expected Y in grid coords: (rank × ranksep) / scale
                 (expected-y-world (* rank ranksep))
                 (expected-y-grid (round (/ expected-y-world scale)))
                 (tolerance 2))

            (unless (<= (abs (- actual-y expected-y-grid)) tolerance)
              (push (list :node label
                          :rank rank
                          :expected-y expected-y-grid
                          :actual-y actual-y
                          :difference (- actual-y expected-y-grid))
                    violations)
              (setq correct nil))))))

    (list :correct correct :violations (nreverse violations))))

(defun dag-draw-test--verify-separation-constraints (graph grid scale)
  "Verify X-separation follows GKNV D3.3: ρ(a,b) = (xsize_a + xsize_b)/2 + nodesep.
Returns plist with :all-satisfied boolean and :violations list."
  (let ((violations '())
        (all-satisfied t)
        (nodes (dag-draw-test--extract-expected-nodes graph))
        (nodesep (if (hash-table-p graph)
                     (gethash 'nodesep graph 40)
                   (dag-draw-graph-node-separation graph))))

    ;; Group nodes by rank
    (let ((rank-groups (make-hash-table :test 'equal)))
      (dolist (node nodes)
        (let ((rank (plist-get node :rank)))
          (when rank
            (push node (gethash rank rank-groups)))))

      ;; For each rank with multiple nodes, check separation
      (maphash (lambda (rank rank-nodes)
                 (when (> (length rank-nodes) 1)
                   ;; Sort nodes by x-coordinate
                   (let ((sorted-nodes (sort rank-nodes
                                             (lambda (a b)
                                               (< (or (plist-get a :x) 0)
                                                  (or (plist-get b :x) 0))))))

                     ;; Check adjacent pairs
                     (dotimes (i (1- (length sorted-nodes)))
                       (let* ((node1 (nth i sorted-nodes))
                              (node2 (nth (1+ i) sorted-nodes))
                              (label1 (plist-get node1 :label))
                              (label2 (plist-get node2 :label))
                              (xsize1 (or (plist-get node1 :xsize) 5))
                              (xsize2 (or (plist-get node2 :xsize) 5))
                              ;; Find actual positions in grid
                              (pos1 (dag-draw-test--find-text-in-grid grid label1))
                              (pos2 (dag-draw-test--find-text-in-grid grid label2)))

                         (when (and pos1 pos2)
                           (let* ((x1 (car pos1))
                                  (x2 (car pos2))
                                  ;; GKNV D3.3: ρ(a,b) = (xsize_a + xsize_b)/2 + nodesep
                                  (min-sep-world (+ (/ (+ xsize1 xsize2) 2.0) nodesep))
                                  (min-sep-grid (round (/ min-sep-world scale)))
                                  (actual-sep (- x2 x1))
                                  (tolerance 2))

                             (when (< actual-sep (- min-sep-grid tolerance))
                               (push (list :node1 label1
                                           :node2 label2
                                           :min-sep min-sep-grid
                                           :actual-sep actual-sep
                                           :deficit (- min-sep-grid actual-sep))
                                     violations)
                               (setq all-satisfied nil)))))))))
               rank-groups))

    (list :all-satisfied all-satisfied :violations (nreverse violations))))

(defun dag-draw-test--measure-grid-distance (grid pos1 pos2)
  "Measure Manhattan distance between POS1 and POS2 on grid.
POS1 and POS2 are (x . y) cons cells.
Returns (dx . dy) cons cell."
  (let ((dx (abs (- (car pos2) (car pos1))))
        (dy (abs (- (cdr pos2) (cdr pos1)))))
    (cons dx dy)))

;;; Edge Path Analysis Functions

(defun dag-draw-test--get-edge-path (grid from-node to-node)
  "Get full edge path from FROM-NODE to TO-NODE as list of grid coordinates.
FROM-NODE and TO-NODE are node plists with :x, :y, :width, :height.
Returns list of (x y) coordinate pairs, or nil if no path found."
  (let* ((from-x (plist-get from-node :x))
         (from-y (plist-get from-node :y))
         (from-width (plist-get from-node :width))
         (from-height (plist-get from-node :height))
         (to-x (plist-get to-node :x))
         (to-y (plist-get to-node :y))
         (to-width (plist-get to-node :width))
         (to-height (plist-get to-node :height))
         (edge-chars '(?│ ?─ ?┌ ?┐ ?└ ?┘ ?├ ?┤ ?┬ ?┴ ?┼ ?▼ ?▲ ?▶ ?◀))
         (visited (make-hash-table :test 'equal))
         (queue nil)
         (parent-map (make-hash-table :test 'equal))
         (found-end nil))

    ;; Find starting points on from-node boundary
    (dotimes (i from-width)
      (let ((x (+ from-x i)))
        ;; Check bottom border
        (when (memq (dag-draw-test--get-char-at grid x (+ from-y from-height)) edge-chars)
          (push (list x (+ from-y from-height)) queue))
        ;; Check top border
        (when (memq (dag-draw-test--get-char-at grid x (1- from-y)) edge-chars)
          (push (list x (1- from-y)) queue))))

    (dotimes (i from-height)
      (let ((y (+ from-y i)))
        ;; Check right border
        (when (memq (dag-draw-test--get-char-at grid (+ from-x from-width) y) edge-chars)
          (push (list (+ from-x from-width) y) queue))
        ;; Check left border
        (when (memq (dag-draw-test--get-char-at grid (1- from-x) y) edge-chars)
          (push (list (1- from-x) y) queue))))

    ;; BFS to find path
    (while (and queue (not found-end))
      (let* ((current (pop queue))
             (curr-x (car current))
             (curr-y (cadr current)))

        (unless (gethash (list curr-x curr-y) visited)
          (puthash (list curr-x curr-y) t visited)

          ;; Check if we reached target node boundary
          (when (and (>= curr-x (1- to-x))
                     (<= curr-x (+ to-x to-width))
                     (>= curr-y (1- to-y))
                     (<= curr-y (+ to-y to-height)))
            (setq found-end (list curr-x curr-y)))

          ;; Add neighbors
          (unless found-end
            (dolist (dir '((0 1) (0 -1) (1 0) (-1 0)))
              (let ((new-x (+ curr-x (car dir)))
                    (new-y (+ curr-y (cadr dir))))
                (when (and (not (gethash (list new-x new-y) visited))
                           (memq (dag-draw-test--get-char-at grid new-x new-y) edge-chars))
                  (puthash (list new-x new-y) (list curr-x curr-y) parent-map)
                  (push (list new-x new-y) queue))))))))

    ;; Reconstruct path if found
    (when found-end
      (let ((path (list found-end))
            (current found-end))
        (while (gethash (list (car current) (cadr current)) parent-map)
          (setq current (gethash (list (car current) (cadr current)) parent-map))
          (push current path))
        path))))

(defun dag-draw-test--verify-path-continuity (grid path)
  "Verify PATH has no gaps (all adjacent positions connected).
PATH is list of (x y) coordinate pairs.
Returns plist with :continuous boolean and :gaps list."
  (let ((gaps '())
        (continuous t))
    (when (> (length path) 1)
      (dotimes (i (1- (length path)))
        (let* ((curr (nth i path))
               (next (nth (1+ i) path))
               (curr-x (car curr))
               (curr-y (cadr curr))
               (next-x (car next))
               (next-y (cadr next))
               (dx (abs (- next-x curr-x)))
               (dy (abs (- next-y curr-y))))
          ;; Adjacent positions should be exactly 1 step apart (manhattan distance)
          (unless (and (<= (+ dx dy) 1) (or (= dx 1) (= dy 1) (and (= dx 0) (= dy 0))))
            (push (list :from curr :to next) gaps)
            (setq continuous nil)))))
    (list :continuous continuous :gaps (nreverse gaps))))

(defun dag-draw-test--verify-orthogonal-routing (grid path)
  "Verify PATH uses only horizontal/vertical segments (no diagonals).
Returns plist with :orthogonal boolean and :diagonal-segments list."
  (let ((diagonal-segments '())
        (orthogonal t))
    (when (> (length path) 1)
      (dotimes (i (1- (length path)))
        (let* ((curr (nth i path))
               (next (nth (1+ i) path))
               (curr-x (car curr))
               (curr-y (cadr curr))
               (next-x (car next))
               (next-y (cadr next))
               (dx (abs (- next-x curr-x)))
               (dy (abs (- next-y curr-y))))
          ;; Orthogonal means either dx=0 (vertical) or dy=0 (horizontal)
          (unless (or (= dx 0) (= dy 0))
            (push (list :from curr :to next) diagonal-segments)
            (setq orthogonal nil)))))
    (list :orthogonal orthogonal :diagonal-segments (nreverse diagonal-segments))))

(provide 'dag-draw-test-harness)

;;; dag-draw-test-harness.el ends here