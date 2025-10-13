# Algorithm Specification for dag-draw.el

## Overview

This document provides a detailed specification of all four passes of the GKNV algorithm adapted for Emacs Lisp implementation. It includes pseudocode, data structures, function signatures, and integration points.

## Table of Contents

1. [Data Structures](#data-structures)
2. [Pass 1: Ranking](#pass-1-ranking)
3. [Pass 2: Ordering](#pass-2-ordering)
4. [Pass 3: Positioning](#pass-3-positioning)
5. [Pass 4: Edge Drawing](#pass-4-edge-drawing)
6. [ASCII Rendering](#ascii-rendering)
7. [Integration Points](#integration-points)

---

## Data Structures

### Core Graph Structures

```elisp
;; Node structure
(cl-defstruct dag-draw-node
  id              ; unique identifier (symbol or string)
  label           ; display label (string)
  xsize           ; width of node box (integer, character units)
  ysize           ; height of node box (integer, character units)
  rank            ; assigned rank (integer)
  order           ; position within rank (integer)
  x               ; X coordinate (float, will be scaled to characters)
  y               ; Y coordinate (float, will be scaled to characters)
  in-edges        ; list of edge structs where this is head
  out-edges       ; list of edge structs where this is tail
  virtual-p       ; t if virtual node, nil if real
  original-edge   ; if virtual, the original edge this belongs to
  )

;; Edge structure
(cl-defstruct dag-draw-edge
  id              ; unique identifier
  tail            ; tail node (dag-draw-node)
  head            ; head node (dag-draw-node)
  weight          ; user weight (positive integer, default 1)
  delta           ; minimum length (positive integer, default 1)
  omega           ; internal weight for positioning (1, 2, or 8)
  reversed-p      ; t if reversed for cycle breaking
  virtual-chain   ; list of virtual nodes if edge spans multiple ranks
  tail-port       ; X offset from tail center (float, optional)
  head-port       ; X offset from head center (float, optional)
  label           ; edge label (string, optional)
  label-node      ; virtual node for label if present
  control-points  ; list of (x . y) for spline/route
  )

;; Graph structure
(cl-defstruct dag-draw-graph
  nodes           ; hash table: id -> dag-draw-node
  edges           ; hash table: id -> dag-draw-edge
  nodesep         ; minimum horizontal separation (integer, characters)
  ranksep         ; minimum vertical separation (integer, characters)
  max-rank        ; maximum rank number (integer)
  ranks           ; vector of lists: rank -> ordered list of nodes
  s-min           ; set of nodes forced to minimum rank (list)
  s-max           ; set of nodes forced to maximum rank (list)
  s-same          ; list of sets of nodes forced to same rank
  )

;; Tree edge (for network simplex)
(cl-defstruct dag-draw-tree-edge
  tail            ; tail node
  head            ; head node
  cut-value       ; cut value (integer)
  )

;; Spanning tree (for network simplex)
(cl-defstruct dag-draw-spanning-tree
  edges           ; list of dag-draw-tree-edge
  parent          ; hash table: node -> parent tree edge
  low             ; hash table: node -> low number
  lim             ; hash table: node -> lim number
  )
```

---

## Pass 1: Ranking

### Objective

Assign each node to an integer rank such that for every edge (v,w), rank(w) - rank(v) >= delta(e), minimizing the sum of weighted edge lengths.

### Algorithm Overview

```
1. Break cycles by reversing back edges (DFS)
2. Merge constraint sets temporarily
3. Construct initial feasible ranking
4. Build initial feasible spanning tree
5. Iterate network simplex until optimal
6. Normalize ranks (min rank = 0)
7. Balance ranks greedily
```

### Function Specifications

#### Main Ranking Function

```elisp
(defun dag-draw-rank (graph)
  "Assign ranks to all nodes in GRAPH using network simplex.
Returns the modified GRAPH with ranks assigned."
  (dag-draw--break-cycles graph)
  (dag-draw--merge-constraint-sets graph)
  (dag-draw--init-rank graph)
  (let ((tree (dag-draw--feasible-tree graph)))
    (while (when-let ((leave-edge (dag-draw--find-leave-edge tree graph)))
             (let ((enter-edge (dag-draw--find-enter-edge leave-edge tree graph)))
               (dag-draw--exchange-edges leave-edge enter-edge tree graph))
             t)))
  (dag-draw--normalize-ranks graph)
  (dag-draw--balance-ranks graph)
  (dag-draw--unmerge-constraint-sets graph)
  graph)
```

#### Cycle Breaking

```elisp
(defun dag-draw--break-cycles (graph)
  "Break cycles in GRAPH by reversing back edges found via DFS.
Modifies edges in place, setting reversed-p flag."
  (let ((visited (make-hash-table :test 'eq))
        (on-stack (make-hash-table :test 'eq)))
    (dolist (node (dag-draw--get-source-nodes graph))
      (dag-draw--dfs-break-cycles node visited on-stack graph))))

(defun dag-draw--dfs-break-cycles (node visited on-stack graph)
  "DFS helper for cycle breaking starting at NODE.
VISITED: nodes completely processed
ON-STACK: nodes currently on DFS stack"
  (puthash node t on-stack)
  (dolist (edge (dag-draw-node-out-edges node))
    (let ((head (dag-draw-edge-head edge)))
      (cond
       ;; Back edge - creates cycle, reverse it
       ((gethash head on-stack)
        (dag-draw--reverse-edge edge))
       ;; Unvisited - continue DFS
       ((not (gethash head visited))
        (dag-draw--dfs-break-cycles head visited on-stack graph)))))
  (remhash node on-stack)
  (puthash node t visited))

(defun dag-draw--reverse-edge (edge)
  "Reverse EDGE direction, setting reversed-p flag."
  (let ((old-tail (dag-draw-edge-tail edge))
        (old-head (dag-draw-edge-head edge)))
    (setf (dag-draw-edge-tail edge) old-head)
    (setf (dag-draw-edge-head edge) old-tail)
    (setf (dag-draw-edge-reversed-p edge) t)
    ;; Update node edge lists
    (setf (dag-draw-node-out-edges old-tail)
          (delete edge (dag-draw-node-out-edges old-tail)))
    (setf (dag-draw-node-in-edges old-tail)
          (cons edge (dag-draw-node-in-edges old-tail)))
    (setf (dag-draw-node-in-edges old-head)
          (delete edge (dag-draw-node-in-edges old-head)))
    (setf (dag-draw-node-out-edges old-head)
          (cons edge (dag-draw-node-out-edges old-head)))))
```

#### Initial Ranking

```elisp
(defun dag-draw--init-rank (graph)
  "Compute initial feasible ranking using topological sort.
Assigns rank to each node respecting delta constraints."
  (let ((in-degree (make-hash-table :test 'eq))
        (queue '()))
    ;; Initialize in-degrees
    (maphash (lambda (_id node)
               (puthash node (length (dag-draw-node-in-edges node)) in-degree))
             (dag-draw-graph-nodes graph))
    ;; Queue nodes with no in-edges
    (maphash (lambda (_id node)
               (when (zerop (gethash node in-degree))
                 (push node queue)
                 (setf (dag-draw-node-rank node) 0)))
             (dag-draw-graph-nodes graph))
    ;; Process queue
    (while queue
      (let ((node (pop queue)))
        (dolist (edge (dag-draw-node-out-edges node))
          (let* ((head (dag-draw-edge-head edge))
                 (min-rank (+ (dag-draw-node-rank node)
                             (dag-draw-edge-delta edge)))
                 (current-rank (dag-draw-node-rank head)))
            ;; Update rank if necessary
            (when (or (null current-rank) (< current-rank min-rank))
              (setf (dag-draw-node-rank head) min-rank))
            ;; Decrement in-degree
            (let ((new-degree (1- (gethash head in-degree))))
              (puthash head new-degree in-degree)
              (when (zerop new-degree)
                (push head queue)))))))))
```

#### Feasible Tree Construction

```elisp
(defun dag-draw--feasible-tree (graph)
  "Construct initial feasible spanning tree for GRAPH.
Returns a dag-draw-spanning-tree structure."
  (let ((tree (make-dag-draw-spanning-tree
               :edges '()
               :parent (make-hash-table :test 'eq)
               :low (make-hash-table :test 'eq)
               :lim (make-hash-table :test 'eq)))
        (in-tree (make-hash-table :test 'eq))
        (node-list (dag-draw--get-all-nodes graph)))
    ;; Start with one node
    (let ((start-node (car node-list)))
      (puthash start-node t in-tree))
    ;; Grow tree by adding minimal slack edges
    (while (< (hash-table-count in-tree) (length node-list))
      (let* ((incident (dag-draw--find-min-slack-incident-edge
                       in-tree graph))
             (edge (car incident))
             (is-head-outside (cdr incident))
             (outside-node (if is-head-outside
                              (dag-draw-edge-head edge)
                            (dag-draw-edge-tail edge))))
        ;; Adjust ranks to make edge tight
        (dag-draw--adjust-ranks-for-tight-edge edge is-head-outside in-tree)
        ;; Add edge to tree
        (push (dag-draw--make-tree-edge edge) (dag-draw-spanning-tree-edges tree))
        (puthash outside-node t in-tree)))
    ;; Compute cut values
    (dag-draw--init-cut-values tree graph)
    ;; Compute low/lim for efficient queries
    (dag-draw--compute-postorder tree graph)
    tree))

(defun dag-draw--find-min-slack-incident-edge (in-tree graph)
  "Find edge with minimal slack incident to tree.
Returns cons (edge . head-is-outside)."
  (let ((min-slack most-positive-fixnum)
        (best-edge nil)
        (head-outside nil))
    (maphash (lambda (_id node)
               (when (gethash node in-tree)
                 ;; Check out-edges
                 (dolist (edge (dag-draw-node-out-edges node))
                   (let ((head (dag-draw-edge-head edge)))
                     (when (not (gethash head in-tree))
                       (let ((slack (dag-draw--compute-slack edge)))
                         (when (< slack min-slack)
                           (setq min-slack slack
                                 best-edge edge
                                 head-outside t))))))
                 ;; Check in-edges
                 (dolist (edge (dag-draw-node-in-edges node))
                   (let ((tail (dag-draw-edge-tail edge)))
                     (when (not (gethash tail in-tree))
                       (let ((slack (dag-draw--compute-slack edge)))
                         (when (< slack min-slack)
                           (setq min-slack slack
                                 best-edge edge
                                 head-outside nil))))))))
             (dag-draw-graph-nodes graph))
    (cons best-edge head-outside)))

(defun dag-draw--compute-slack (edge)
  "Compute slack of EDGE: length - delta."
  (let ((length (- (dag-draw-node-rank (dag-draw-edge-head edge))
                   (dag-draw-node-rank (dag-draw-edge-tail edge))))
        (delta (dag-draw-edge-delta edge)))
    (- length delta)))
```

#### Network Simplex Iteration

```elisp
(defun dag-draw--find-leave-edge (tree graph)
  "Find tree edge with negative cut value.
Returns edge or nil if all non-negative (optimal)."
  ;; Use cyclic search for better performance
  (let ((start-pos (or (dag-draw-spanning-tree-search-pos tree) 0))
        (edges (dag-draw-spanning-tree-edges tree))
        (n (length (dag-draw-spanning-tree-edges tree))))
    (cl-loop for i from 0 below n
             for pos = (mod (+ start-pos i) n)
             for edge = (nth pos edges)
             when (< (dag-draw-tree-edge-cut-value edge) 0)
             do (setf (dag-draw-spanning-tree-search-pos tree) pos)
             and return edge
             finally return nil)))

(defun dag-draw--find-enter-edge (leave-edge tree graph)
  "Find non-tree edge to replace LEAVE-EDGE.
Returns edge with minimum slack from head to tail component."
  ;; Break tree at leave-edge to get components
  (let* ((components (dag-draw--break-tree-at-edge leave-edge tree))
         (head-comp (car components))
         (tail-comp (cdr components))
         (min-slack most-positive-fixnum)
         (best-edge nil))
    ;; Find minimum slack edge from head to tail
    (dolist (head-node head-comp)
      (dolist (edge (dag-draw-node-out-edges head-node))
        (when (memq (dag-draw-edge-head edge) tail-comp)
          (let ((slack (dag-draw--compute-slack edge)))
            (when (< slack min-slack)
              (setq min-slack slack
                    best-edge edge))))))
    best-edge))

(defun dag-draw--exchange-edges (leave-edge enter-edge tree graph)
  "Exchange LEAVE-EDGE with ENTER-EDGE in TREE.
Updates tree structure and cut values."
  ;; Adjust ranks to make enter-edge tight
  (dag-draw--adjust-ranks-for-exchange leave-edge enter-edge tree graph)
  ;; Update tree edges
  (setf (dag-draw-spanning-tree-edges tree)
        (cons (dag-draw--make-tree-edge-from-graph-edge enter-edge)
              (delete leave-edge (dag-draw-spanning-tree-edges tree))))
  ;; Update cut values along path from enter-edge endpoints
  (dag-draw--update-cut-values-after-exchange enter-edge tree graph)
  ;; Update postorder numbering
  (dag-draw--compute-postorder tree graph))
```

#### Cut Value Computation

```elisp
(defun dag-draw--init-cut-values (tree graph)
  "Compute initial cut values for all tree edges.
Uses incremental method from leaves inward."
  (let* ((edges (dag-draw-spanning-tree-edges tree))
         (visited (make-hash-table :test 'eq))
         (leaves (dag-draw--find-tree-leaves tree)))
    ;; Process from leaves inward
    (while leaves
      (let ((node (pop leaves)))
        (unless (gethash node visited)
          (puthash node t visited)
          ;; Find the tree edge incident to this node with unknown cut value
          (let ((unknown-edge (dag-draw--find-unknown-cut-value-edge
                               node edges visited)))
            (when unknown-edge
              (dag-draw--compute-cut-value-incremental
               unknown-edge tree graph visited)
              ;; Add other endpoint to queue
              (let ((other (dag-draw--other-endpoint unknown-edge node)))
                (push other leaves)))))))))

(defun dag-draw--compute-cut-value-incremental (edge tree graph visited)
  "Compute cut value for EDGE using local information.
VISITED contains nodes whose incident edges have known cut values."
  ;; Formula from GKNV Section 2.4
  ;; Cut value = sum of known incident edge cut values
  ;;           + sum of non-tree edge weights (tail->head)
  ;;           - sum of non-tree edge weights (head->tail)
  (let ((cut-val 0)
        (node (dag-draw-tree-edge-tail edge)))
    ;; Add known cut values
    (dolist (other-edge (dag-draw--incident-tree-edges node tree))
      (unless (eq other-edge edge)
        (when (dag-draw-tree-edge-cut-value other-edge)
          (let ((sign (if (eq node (dag-draw-tree-edge-tail other-edge)) 1 -1)))
            (cl-incf cut-val (* sign (dag-draw-tree-edge-cut-value other-edge)))))))
    ;; Add non-tree edge contributions
    (cl-incf cut-val (dag-draw--non-tree-edge-weight-sum node graph tree))
    (setf (dag-draw-tree-edge-cut-value edge) cut-val)))
```

#### Rank Normalization and Balancing

```elisp
(defun dag-draw--normalize-ranks (graph)
  "Normalize ranks so minimum rank is 0."
  (let ((min-rank most-positive-fixnum))
    ;; Find minimum
    (maphash (lambda (_id node)
               (setq min-rank (min min-rank (dag-draw-node-rank node))))
             (dag-draw-graph-nodes graph))
    ;; Adjust all ranks
    (when (> min-rank 0)
      (maphash (lambda (_id node)
                 (cl-decf (dag-draw-node-rank node) min-rank))
               (dag-draw-graph-nodes graph)))))

(defun dag-draw--balance-ranks (graph)
  "Balance nodes with equal in/out weight across ranks greedily."
  (maphash (lambda (_id node)
             (when (dag-draw--can-move-rank-p node)
               (let ((best-rank (dag-draw--find-least-crowded-rank
                                node graph)))
                 (setf (dag-draw-node-rank node) best-rank))))
           (dag-draw-graph-nodes graph)))

(defun dag-draw--can-move-rank-p (node)
  "Return t if NODE has equal in/out weight and can be moved."
  (let ((in-weight (cl-reduce #'+
                              (mapcar #'dag-draw-edge-weight
                                     (dag-draw-node-in-edges node))
                              :initial-value 0))
        (out-weight (cl-reduce #'+
                               (mapcar #'dag-draw-edge-weight
                                      (dag-draw-node-out-edges node))
                               :initial-value 0)))
    (= in-weight out-weight)))
```

---

## Pass 2: Ordering

### Objective

Order nodes within each rank to minimize edge crossings.

### Algorithm Overview

```
1. Create virtual nodes for multi-rank edges
2. Compute initial ordering (DFS from sources and sinks)
3. For max_iterations (24):
   a. Apply weighted median heuristic
   b. Apply transpose heuristic
   c. If better, save ordering
4. Return best ordering found
```

### Function Specifications

#### Main Ordering Function

```elisp
(defun dag-draw-ordering (graph)
  "Order nodes within ranks to minimize crossings.
Returns modified GRAPH with order field set for all nodes."
  (dag-draw--create-virtual-nodes graph)
  (let* ((order (dag-draw--init-order graph))
         (best-order (copy-sequence order))
         (best-crossings (dag-draw--count-crossings order graph)))
    (dotimes (iter 24)
      (dag-draw--wmedian order iter graph)
      (dag-draw--transpose order graph)
      (let ((crossings (dag-draw--count-crossings order graph)))
        (when (< crossings best-crossings)
          (setq best-crossings crossings
                best-order (copy-sequence order)))))
    ;; Apply best ordering
    (dag-draw--apply-ordering best-order graph)
    graph))
```

#### Virtual Node Creation

```elisp
(defun dag-draw--create-virtual-nodes (graph)
  "Create virtual nodes for edges spanning multiple ranks.
Modifies GRAPH in place, adding virtual nodes and splitting edges."
  (maphash (lambda (_id edge)
             (let* ((tail (dag-draw-edge-tail edge))
                    (head (dag-draw-edge-head edge))
                    (tail-rank (dag-draw-node-rank tail))
                    (head-rank (dag-draw-node-rank head))
                    (span (- head-rank tail-rank)))
               (when (> span 1)
                 (dag-draw--split-edge-with-virtuals edge span graph))))
           (dag-draw-graph-edges graph)))

(defun dag-draw--split-edge-with-virtuals (edge span graph)
  "Split EDGE into chain of edges through virtual nodes."
  (let ((virtuals '())
        (tail (dag-draw-edge-tail edge))
        (head (dag-draw-edge-head edge))
        (tail-rank (dag-draw-node-rank tail)))
    ;; Create virtual nodes
    (dotimes (i (1- span))
      (let ((vnode (make-dag-draw-node
                    :id (dag-draw--gen-virtual-id edge i)
                    :label ""
                    :xsize 1
                    :ysize 1
                    :rank (+ tail-rank i 1)
                    :virtual-p t
                    :original-edge edge)))
        (push vnode virtuals)
        (puthash (dag-draw-node-id vnode) vnode
                (dag-draw-graph-nodes graph))))
    ;; Create chain of edges
    (setf virtuals (nreverse virtuals))
    (let ((current-tail tail))
      (dolist (vnode virtuals)
        (let ((vedge (dag-draw--make-virtual-edge current-tail vnode edge)))
          (setf current-tail vnode))))
    ;; Final edge to head
    (let ((vedge (dag-draw--make-virtual-edge current-tail head edge)))
      (setf (dag-draw-edge-virtual-chain edge) virtuals))))
```

#### Initial Ordering

```elisp
(defun dag-draw--init-order (graph)
  "Compute initial ordering using DFS.
Returns vector: rank -> ordered list of nodes."
  ;; Try both forward and backward, keep better
  (let* ((forward-order (dag-draw--dfs-order graph :forward))
         (backward-order (dag-draw--dfs-order graph :backward))
         (forward-cross (dag-draw--count-crossings forward-order graph))
         (backward-cross (dag-draw--count-crossings backward-order graph)))
    (if (< forward-cross backward-cross)
        forward-order
      backward-order)))

(defun dag-draw--dfs-order (graph direction)
  "Compute ordering via DFS in DIRECTION (:forward or :backward)."
  (let ((order (make-vector (1+ (dag-draw-graph-max-rank graph)) '()))
        (visited (make-hash-table :test 'eq))
        (start-nodes (if (eq direction :forward)
                        (dag-draw--get-source-nodes graph)
                      (dag-draw--get-sink-nodes graph))))
    (dolist (node start-nodes)
      (dag-draw--dfs-order-visit node visited order graph direction))
    ;; Reverse each rank list (DFS builds backward)
    (dotimes (i (length order))
      (setf (aref order i) (nreverse (aref order i))))
    order))

(defun dag-draw--dfs-order-visit (node visited order graph direction)
  "DFS visit for ordering, appending to ORDER."
  (unless (gethash node visited)
    (puthash node t visited)
    (let ((neighbors (if (eq direction :forward)
                        (mapcar #'dag-draw-edge-head
                               (dag-draw-node-out-edges node))
                      (mapcar #'dag-draw-edge-tail
                             (dag-draw-node-in-edges node)))))
      (dolist (neighbor neighbors)
        (dag-draw--dfs-order-visit neighbor visited order graph direction)))
    ;; Add to rank list
    (let ((rank (dag-draw-node-rank node)))
      (push node (aref order rank)))))
```

#### Weighted Median Heuristic

```elisp
(defun dag-draw--wmedian (order iter graph)
  "Apply weighted median heuristic to ORDER.
ITER determines direction (even = down, odd = up)."
  (if (evenp iter)
      ;; Forward: rank 1 to max
      (cl-loop for rank from 1 to (dag-draw-graph-max-rank graph)
               do (dag-draw--wmedian-rank order rank (1- rank) graph))
    ;; Backward: rank max-1 to 0
    (cl-loop for rank from (1- (dag-draw-graph-max-rank graph)) downto 0
             do (dag-draw--wmedian-rank order rank (1+ rank) graph))))

(defun dag-draw--wmedian-rank (order rank adj-rank graph)
  "Sort nodes in RANK by weighted median position in ADJ-RANK."
  (let ((nodes (aref order rank))
        (medians (make-hash-table :test 'eq)))
    ;; Compute median for each node
    (dolist (node nodes)
      (puthash node (dag-draw--median-value node adj-rank order) medians))
    ;; Sort by median (stable for -1 values)
    (setf (aref order rank)
          (sort nodes
                (lambda (a b)
                  (let ((ma (gethash a medians))
                        (mb (gethash b medians)))
                    (cond
                     ((and (= ma -1.0) (= mb -1.0)) nil) ; keep order
                     ((= ma -1.0) nil)  ; a stays put
                     ((= mb -1.0) t)    ; b stays put
                     (t (< ma mb)))))))))

(defun dag-draw--median-value (node adj-rank order)
  "Compute weighted median position for NODE based on ADJ-RANK.
Returns -1.0 if no adjacent nodes (keep current position)."
  (let* ((positions (dag-draw--adjacent-positions node adj-rank order))
         (p (length positions)))
    (cond
     ;; No adjacent nodes
     ((= p 0) -1.0)
     ;; Odd number: return middle
     ((oddp p) (float (nth (/ p 2) positions)))
     ;; Even = 2: return average
     ((= p 2) (/ (+ (nth 0 positions) (nth 1 positions)) 2.0))
     ;; Even > 2: weighted interpolation
     (t (let* ((m (/ p 2))
               (left-pos (nth (1- m) positions))
               (right-pos (nth m positions))
               (left-spread (- left-pos (nth 0 positions)))
               (right-spread (- (nth (1- p) positions) right-pos)))
          (if (zerop (+ left-spread right-spread))
              (/ (+ left-pos right-pos) 2.0)
            (/ (+ (* left-pos right-spread)
                  (* right-pos left-spread))
               (float (+ left-spread right-spread)))))))))

(defun dag-draw--adjacent-positions (node adj-rank order)
  "Return sorted list of positions of nodes adjacent to NODE in ADJ-RANK."
  (let ((adj-nodes (if (< adj-rank (dag-draw-node-rank node))
                      ;; Adjacent rank above: look at in-edges
                      (mapcar #'dag-draw-edge-tail
                             (dag-draw-node-in-edges node))
                    ;; Adjacent rank below: look at out-edges
                    (mapcar #'dag-draw-edge-head
                           (dag-draw-node-out-edges node))))
        (rank-list (aref order adj-rank))
        (positions '()))
    ;; Find positions in rank-list
    (dolist (adj-node adj-nodes)
      (let ((pos (cl-position adj-node rank-list)))
        (when pos (push pos positions))))
    (sort positions #'<)))
```

#### Transpose Heuristic

```elisp
(defun dag-draw--transpose (order graph)
  "Apply transpose heuristic to reduce crossings in ORDER."
  (let ((improved t))
    (while improved
      (setq improved nil)
      (dotimes (rank (length order))
        (let ((rank-list (aref order rank)))
          (dotimes (i (1- (length rank-list)))
            (let ((v (nth i rank-list))
                  (w (nth (1+ i) rank-list)))
              (when (> (dag-draw--crossing-vw v w order graph)
                      (dag-draw--crossing-vw w v order graph))
                ;; Swap v and w
                (setf (nth i rank-list) w
                      (nth (1+ i) rank-list) v)
                (setq improved t)))))))))

(defun dag-draw--crossing-vw (v w order graph)
  "Count crossings if V appears left of W in their rank.
Considers all edges incident to V and W."
  (let ((crossings 0))
    ;; Check crossings with upper rank
    (when (> (dag-draw-node-rank v) 0)
      (cl-incf crossings
               (dag-draw--count-crossings-between
                v w (1- (dag-draw-node-rank v)) order :in-edges)))
    ;; Check crossings with lower rank
    (when (< (dag-draw-node-rank v) (dag-draw-graph-max-rank graph))
      (cl-incf crossings
               (dag-draw--count-crossings-between
                v w (1+ (dag-draw-node-rank v)) order :out-edges)))
    crossings))
```

---

## Pass 3: Positioning

### Objective

Assign X and Y coordinates to all nodes, minimizing weighted horizontal edge length while respecting node sizes and separation constraints.

### Algorithm Overview

```
1. Assign Y coordinates by rank (fixed ranksep)
2. Build auxiliary graph for X coordinate assignment
3. Construct initial feasible tree for auxiliary graph
4. Run network simplex on auxiliary graph
5. Extract X coordinates from solution
```

### Function Specifications

#### Main Positioning Function

```elisp
(defun dag-draw-position (graph)
  "Assign X and Y coordinates to all nodes.
Returns modified GRAPH with x and y fields set."
  (dag-draw--assign-y-coordinates graph)
  (let ((aux-graph (dag-draw--build-auxiliary-graph graph)))
    (dag-draw-rank aux-graph)  ; Use network simplex
    (dag-draw--extract-x-coordinates graph aux-graph))
  graph)
```

#### Y Coordinate Assignment

```elisp
(defun dag-draw--assign-y-coordinates (graph)
  "Assign Y coordinates based on rank and ranksep."
  (let ((ranksep (dag-draw-graph-ranksep graph))
        (y 0))
    (dotimes (rank (1+ (dag-draw-graph-max-rank graph)))
      (dolist (node (aref (dag-draw-graph-ranks graph) rank))
        (setf (dag-draw-node-y node) (float y)))
      (cl-incf y ranksep))))
```

#### Auxiliary Graph Construction

```elisp
(defun dag-draw--build-auxiliary-graph (graph)
  "Build auxiliary graph for X coordinate optimization.
Returns new dag-draw-graph structure."
  (let ((aux-graph (make-dag-draw-graph
                    :nodes (make-hash-table :test 'eq)
                    :edges (make-hash-table :test 'eq)
                    :nodesep (dag-draw-graph-nodesep graph)
                    :ranksep (dag-draw-graph-ranksep graph))))
    ;; Copy all original nodes
    (maphash (lambda (id node)
               (puthash id node (dag-draw-graph-nodes aux-graph)))
             (dag-draw-graph-nodes graph))

    ;; For each edge e=(u,v), create node n_e and edges (n_e,u) and (n_e,v)
    (maphash (lambda (id edge)
               (let* ((n-e (make-dag-draw-node
                           :id (dag-draw--gen-aux-node-id edge)
                           :label ""
                           :xsize 0
                           :ysize 0
                           :rank 0  ; Auxiliary nodes can be at any position
                           :virtual-p t))
                      (u (dag-draw-edge-tail edge))
                      (v (dag-draw-edge-head edge))
                      (omega (dag-draw--compute-omega edge))
                      (weight (* omega (dag-draw-edge-weight edge))))
                 ;; Add n_e to auxiliary graph
                 (puthash (dag-draw-node-id n-e) n-e
                         (dag-draw-graph-nodes aux-graph))
                 ;; Create edge (n_e, u) with delta based on tail-port
                 (let ((delta-u (if (dag-draw-edge-tail-port edge)
                                   (abs (dag-draw-edge-tail-port edge))
                                 0)))
                   (dag-draw--add-aux-edge aux-graph n-e u delta-u weight))
                 ;; Create edge (n_e, v) with delta based on head-port
                 (let ((delta-v (if (dag-draw-edge-head-port edge)
                                   (+ (abs (dag-draw-edge-head-port edge))
                                      (abs (or (dag-draw-edge-tail-port edge) 0)))
                                 0)))
                   (dag-draw--add-aux-edge aux-graph n-e v delta-v weight))))
             (dag-draw-graph-edges graph))

    ;; For each pair of adjacent nodes in same rank, create separation edge
    (dotimes (rank (1+ (dag-draw-graph-max-rank graph)))
      (let ((rank-nodes (aref (dag-draw-graph-ranks graph) rank)))
        (dotimes (i (1- (length rank-nodes)))
          (let* ((left (nth i rank-nodes))
                 (right (nth (1+ i) rank-nodes))
                 (rho (dag-draw--compute-rho left right graph)))
            (dag-draw--add-aux-edge aux-graph left right rho 0)))))

    aux-graph))

(defun dag-draw--compute-omega (edge)
  "Compute internal weight omega for EDGE based on endpoint types.
Returns 1, 2, or 8."
  (let ((tail-virtual (dag-draw-node-virtual-p (dag-draw-edge-tail edge)))
        (head-virtual (dag-draw-node-virtual-p (dag-draw-edge-head edge))))
    (cond
     ((and (not tail-virtual) (not head-virtual)) 1)  ; Both real
     ((or tail-virtual head-virtual) 2)               ; One virtual
     ((and tail-virtual head-virtual) 8))))           ; Both virtual

(defun dag-draw--compute-rho (left right graph)
  "Compute minimum separation between LEFT and RIGHT nodes.
Formula: (xsize(left) + xsize(right))/2 + nodesep"
  (+ (/ (+ (dag-draw-node-xsize left)
           (dag-draw-node-xsize right))
        2)
     (dag-draw-graph-nodesep graph)))
```

#### X Coordinate Extraction

```elisp
(defun dag-draw--extract-x-coordinates (graph aux-graph)
  "Extract X coordinates from solved AUX-GRAPH and apply to GRAPH."
  (maphash (lambda (id node)
             (let ((aux-node (gethash id (dag-draw-graph-nodes aux-graph))))
               (setf (dag-draw-node-x node)
                     (float (dag-draw-node-rank aux-node)))))
           (dag-draw-graph-nodes graph)))
```

---

## Pass 4: Edge Drawing

### Objective

Compute control points for drawing edges as smooth splines (or in ASCII, as routed paths).

### Algorithm Overview

```
1. Compute splines in order (shortest edges first)
2. For each edge:
   a. Determine region (boxes where spline can go)
   b. Compute linear path through region
   c. Generate Bezier spline fitting path
   d. Update virtual node sizes
3. Clip splines to node boundaries
```

### Function Specifications

#### Main Spline Function

```elisp
(defun dag-draw-make-splines (graph)
  "Compute control points for all edges in GRAPH.
Returns modified GRAPH with control-points set for edges."
  (let ((edges-by-length (dag-draw--sort-edges-by-length graph)))
    (dolist (edge edges-by-length)
      (dag-draw--compute-edge-spline edge graph)))
  graph)

(defun dag-draw--sort-edges-by-length (graph)
  "Return list of edges sorted by length (shortest first)."
  (let ((edges '()))
    (maphash (lambda (_id edge)
               (push edge edges))
             (dag-draw-graph-edges graph))
    (sort edges
          (lambda (e1 e2)
            (< (dag-draw--edge-length e1)
               (dag-draw--edge-length e2))))))

(defun dag-draw--edge-length (edge)
  "Compute Euclidean length of EDGE."
  (let ((tail (dag-draw-edge-tail edge))
        (head (dag-draw-edge-head edge)))
    (sqrt (+ (expt (- (dag-draw-node-x head) (dag-draw-node-x tail)) 2)
             (expt (- (dag-draw-node-y head) (dag-draw-node-y tail)) 2)))))
```

#### Region Construction

```elisp
(defun dag-draw--compute-edge-spline (edge graph)
  "Compute spline control points for EDGE."
  (let* ((boxes (dag-draw--build-edge-region edge graph))
         (q (dag-draw--edge-start-point edge))
         (r (dag-draw--edge-end-point edge))
         (theta-q (dag-draw--edge-start-angle edge))
         (theta-r (dag-draw--edge-end-angle edge))
         (spline (dag-draw--compute-spline-in-region
                 boxes q r theta-q theta-r)))
    (setf (dag-draw-edge-control-points edge) spline)
    (dag-draw--update-virtual-box-sizes edge spline graph)))

(defun dag-draw--build-edge-region (edge graph)
  "Build list of boxes defining region for EDGE spline.
Returns list of box structures: ((x-min y-min x-max y-max) ...)."
  (let ((boxes '())
        (tail (dag-draw-edge-tail edge))
        (head (dag-draw-edge-head edge)))
    ;; Add port boxes at tail
    (push (dag-draw--make-port-box tail edge :tail) boxes)
    ;; Add inter-rank and virtual node boxes
    (when (dag-draw-edge-virtual-chain edge)
      (dolist (vnode (dag-draw-edge-virtual-chain edge))
        ;; Inter-rank box before virtual
        (push (dag-draw--make-inter-rank-box vnode :above graph) boxes)
        ;; Virtual node box
        (push (dag-draw--make-virtual-node-box vnode graph) boxes)))
    ;; Add final inter-rank box
    (push (dag-draw--make-inter-rank-box head :above graph) boxes)
    ;; Add port box at head
    (push (dag-draw--make-port-box head edge :head) boxes)
    (nreverse boxes)))
```

#### Spline Computation

```elisp
(defun dag-draw--compute-spline-in-region (boxes q r theta-q theta-r)
  "Compute spline from Q to R within BOXES with angles THETA-Q and THETA-R.
Returns list of control points: ((x . y) ...)."
  (let* ((L-array (dag-draw--compute-L-segments boxes))
         (p-array (dag-draw--compute-path q r boxes L-array))
         (vector-q (dag-draw--angle-to-vector theta-q))
         (vector-r (dag-draw--angle-to-vector theta-r))
         (s-array (dag-draw--compute-spline-from-path
                  p-array vector-q vector-r boxes L-array)))
    s-array))

(defun dag-draw--compute-L-segments (boxes)
  "Compute line segments L_i at intersections between boxes.
Returns list of ((x1 y1) (x2 y2)) segments."
  (let ((segments '()))
    (dotimes (i (1- (length boxes)))
      (push (dag-draw--box-intersection (nth i boxes) (nth (1+ i) boxes))
            segments))
    (nreverse segments)))

(defun dag-draw--compute-path (q r boxes L-array)
  "Compute piecewise linear path from Q to R through BOXES.
Returns list of points: ((x . y) ...)."
  (if (dag-draw--line-fits-p q r boxes L-array)
      (list q r)
    ;; Recursive subdivision
    (let* ((split-info (dag-draw--find-line-split q r boxes L-array))
           (split-point (car split-info))
           (split-idx (cdr split-info))
           (boxes-1 (cl-subseq boxes 0 (1+ split-idx)))
           (boxes-2 (cl-subseq boxes split-idx))
           (L-array-1 (cl-subseq L-array 0 split-idx))
           (L-array-2 (cl-subseq L-array split-idx)))
      (append (dag-draw--compute-path q split-point boxes-1 L-array-1)
              (cdr (dag-draw--compute-path split-point r boxes-2 L-array-2))))))

(defun dag-draw--compute-spline-from-path (p-array vector-q vector-r
                                                  boxes L-array)
  "Generate Bezier spline approximating P-ARRAY.
VECTOR-Q and VECTOR-R are tangent vectors at endpoints.
Returns list of control points."
  (let ((spline (dag-draw--generate-bezier-spline p-array vector-q vector-r)))
    ;; Check if spline fits
    (if (dag-draw--spline-fits-p spline boxes L-array)
        spline
      ;; Try straightening or subdividing
      (if (= (length p-array) 2)
          ;; Single segment: straighten
          (dag-draw--straighten-spline spline boxes L-array)
        ;; Multiple segments: subdivide
        (let* ((split-info (dag-draw--find-spline-split spline p-array))
               (split-point (car split-info))
               (split-idx (cdr split-info)))
          (append
           (dag-draw--compute-spline-from-path
            (cl-subseq p-array 0 (1+ split-idx))
            vector-q
            (dag-draw--tangent-at-point split-point)
            (cl-subseq boxes 0 (1+ split-idx))
            (cl-subseq L-array 0 split-idx))
           (cdr (dag-draw--compute-spline-from-path
                 (cl-subseq p-array split-idx)
                 (dag-draw--reverse-vector
                  (dag-draw--tangent-at-point split-point))
                 vector-r
                 (cl-subseq boxes split-idx)
                 (cl-subseq L-array split-idx)))))))))
```

---

## ASCII Rendering

### Objective

Convert continuous coordinates and splines to ASCII character grid with proper junction characters.

### Algorithm Overview

```
1. Scale coordinates to character grid
2. Render node boxes with borders
3. Route edges on grid (approximating splines)
4. Apply junction character algorithm
5. Place arrows at edge endpoints
```

### Function Specifications

#### Main ASCII Rendering

```elisp
(defun dag-draw-render-ascii (graph)
  "Render GRAPH as ASCII art.
Returns multi-line string."
  (let* ((grid (dag-draw--create-grid graph))
         (scaled-graph (dag-draw--scale-to-grid graph grid)))
    ;; Render nodes
    (dag-draw--render-nodes scaled-graph grid)
    ;; Route edges
    (dag-draw--route-edges scaled-graph grid)
    ;; Apply junction characters
    (dag-draw--apply-junctions scaled-graph grid)
    ;; Place arrows
    (dag-draw--place-arrows scaled-graph grid)
    ;; Convert grid to string
    (dag-draw--grid-to-string grid)))
```

#### Coordinate Scaling

```elisp
(defun dag-draw--scale-to-grid (graph grid)
  "Scale continuous coordinates to character grid positions.
Returns new graph with integer grid coordinates."
  (let* ((min-x most-positive-fixnum)
         (max-x most-negative-fixnum)
         (min-y most-positive-fixnum)
         (max-y most-negative-fixnum))
    ;; Find bounds
    (maphash (lambda (_id node)
               (setq min-x (min min-x (dag-draw-node-x node))
                     max-x (max max-x (dag-draw-node-x node))
                     min-y (min min-y (dag-draw-node-y node))
                     max-y (max max-y (dag-draw-node-y node))))
             (dag-draw-graph-nodes graph))
    ;; Compute scale factors
    (let* ((width (dag-draw-grid-width grid))
           (height (dag-draw-grid-height grid))
           (x-scale (/ (- width 2.0) (- max-x min-x)))
           (y-scale (/ (- height 2.0) (- max-y min-y))))
      ;; Scale all nodes
      (maphash (lambda (_id node)
                 (setf (dag-draw-node-x node)
                       (round (* (- (dag-draw-node-x node) min-x) x-scale)))
                 (setf (dag-draw-node-y node)
                       (round (* (- (dag-draw-node-y node) min-y) y-scale))))
               (dag-draw-graph-nodes graph)))
    graph))
```

#### Edge Routing

```elisp
(defun dag-draw--route-edges (graph grid)
  "Route all edges on character GRID.
Approximates splines with orthogonal paths."
  (maphash (lambda (_id edge)
             (dag-draw--route-edge edge grid))
           (dag-draw-graph-edges graph)))

(defun dag-draw--route-edge (edge grid)
  "Route single EDGE on GRID.
Samples spline control points and creates orthogonal path."
  (let* ((cpoints (dag-draw-edge-control-points edge))
         (sampled (dag-draw--sample-spline cpoints 20))
         (grid-points (mapcar (lambda (pt)
                               (cons (round (car pt)) (round (cdr pt))))
                             sampled)))
    ;; Connect points with orthogonal segments
    (dag-draw--draw-orthogonal-path grid-points edge grid)))

(defun dag-draw--draw-orthogonal-path (points edge grid)
  "Draw orthogonal (horizontal/vertical) path through POINTS on GRID."
  (dotimes (i (1- (length points)))
    (let ((p1 (nth i points))
          (p2 (nth (1+ i) points)))
      (if (= (car p1) (car p2))
          ;; Vertical segment
          (dag-draw--draw-vertical-segment (car p1) (cdr p1) (cdr p2)
                                          edge grid)
        ;; Horizontal segment
        (dag-draw--draw-horizontal-segment (cdr p1) (car p1) (car p2)
                                          edge grid)))))

(defun dag-draw--draw-vertical-segment (x y1 y2 edge grid)
  "Draw vertical line segment on GRID."
  (let ((start (min y1 y2))
        (end (max y1 y2)))
    (cl-loop for y from start to end
             do (dag-draw--set-grid grid x y
                                   (dag-draw--make-grid-cell
                                    :char ?│
                                    :edges (list edge))))))

(defun dag-draw--draw-horizontal-segment (y x1 x2 edge grid)
  "Draw horizontal line segment on GRID."
  (let ((start (min x1 x2))
        (end (max x1 x2)))
    (cl-loop for x from start to end
             do (dag-draw--set-grid grid x y
                                   (dag-draw--make-grid-cell
                                    :char ?─
                                    :edges (list edge))))))
```

#### Junction Character Algorithm

```elisp
(defun dag-draw--apply-junctions (graph grid)
  "Walk all edges and apply correct junction characters at:
- Port boundaries (edge starts/ends)
- Direction changes (corners)
- Merge/split points
- Crossings"
  (maphash (lambda (_id edge)
             (dag-draw--walk-edge-for-junctions edge grid))
           (dag-draw-graph-edges graph)))

(defun dag-draw--walk-edge-for-junctions (edge grid)
  "Walk EDGE path and set junction characters."
  (let ((path (dag-draw--get-edge-grid-path edge grid)))
    ;; Check each point in path
    (dotimes (i (length path))
      (let* ((point (nth i path))
             (x (car point))
             (y (cdr point))
             (prev (when (> i 0) (nth (1- i) path)))
             (next (when (< i (1- (length path))) (nth (1+ i) path)))
             (cell (dag-draw--get-grid grid x y)))
        ;; Determine junction type
        (cond
         ;; Starting port
         ((= i 0)
          (dag-draw--set-port-junction edge :start x y grid))
         ;; Ending port
         ((= i (1- (length path)))
          (dag-draw--set-port-junction edge :end x y grid))
         ;; Direction change
         ((and prev next (dag-draw--is-corner-p prev point next))
          (dag-draw--set-corner-junction prev point next cell grid x y))
         ;; Multiple edges at this point
         ((> (length (dag-draw-grid-cell-edges cell)) 1)
          (dag-draw--set-multi-edge-junction cell grid x y)))))))

(defun dag-draw--is-corner-p (prev curr next)
  "Return t if PREV-CURR-NEXT forms a corner (direction change)."
  (not (or (and (= (car prev) (car curr))
                (= (car curr) (car next)))    ; All same X (vertical)
           (and (= (cdr prev) (cdr curr))
                (= (cdr curr) (cdr next)))))) ; All same Y (horizontal)

(defun dag-draw--set-corner-junction (prev curr next cell grid x y)
  "Set appropriate corner character at CURR based on PREV and NEXT directions."
  (let* ((dx-in (- (car curr) (car prev)))
         (dy-in (- (cdr curr) (cdr prev)))
         (dx-out (- (car next) (car curr)))
         (dy-out (- (cdr next) (cdr curr)))
         (char (dag-draw--corner-char dx-in dy-in dx-out dy-out)))
    (setf (dag-draw-grid-cell-char cell) char)
    (dag-draw--set-grid grid x y cell)))

(defun dag-draw--corner-char (dx-in dy-in dx-out dy-out)
  "Return appropriate corner character for given direction changes.
DX-IN, DY-IN: incoming direction
DX-OUT, DY-OUT: outgoing direction"
  (cond
   ;; From left, going down
   ((and (> dx-in 0) (= dy-in 0) (= dx-out 0) (> dy-out 0)) ?┐)
   ;; From left, going up
   ((and (> dx-in 0) (= dy-in 0) (= dx-out 0) (< dy-out 0)) ?┘)
   ;; From right, going down
   ((and (< dx-in 0) (= dy-in 0) (= dx-out 0) (> dy-out 0)) ?┌)
   ;; From right, going up
   ((and (< dx-in 0) (= dy-in 0) (= dx-out 0) (< dy-out 0)) ?└)
   ;; From above, going right
   ((and (= dx-in 0) (> dy-in 0) (> dx-out 0) (= dy-out 0)) ?└)
   ;; From above, going left
   ((and (= dx-in 0) (> dy-in 0) (< dx-out 0) (= dy-out 0)) ?┘)
   ;; From below, going right
   ((and (= dx-in 0) (< dy-in 0) (> dx-out 0) (= dy-out 0)) ?┌)
   ;; From below, going left
   ((and (= dx-in 0) (< dy-in 0) (< dx-out 0) (= dy-out 0)) ?┐)
   ;; Default: shouldn't happen
   (t ?+)))

(defun dag-draw--set-multi-edge-junction (cell grid x y)
  "Set junction character for point where multiple edges meet/cross."
  (let ((edges (dag-draw-grid-cell-edges cell))
        (has-vertical nil)
        (has-horizontal nil))
    ;; Check what directions edges go
    (dolist (edge edges)
      (let ((dirs (dag-draw--edge-directions-at-point edge x y)))
        (when (member :up dirs) (setq has-vertical t))
        (when (member :down dirs) (setq has-vertical t))
        (when (member :left dirs) (setq has-horizontal t))
        (when (member :right dirs) (setq has-horizontal t))))
    ;; Set appropriate junction character
    (setf (dag-draw-grid-cell-char cell)
          (cond
           ((and has-vertical has-horizontal) ?┼)  ; Cross
           (has-vertical ?│)                        ; Vertical line
           (has-horizontal ?─)                      ; Horizontal line
           (t (dag-draw-grid-cell-char cell))))    ; Keep current
    (dag-draw--set-grid grid x y cell)))
```

---

## Integration Points

### Between Pass 1 and Pass 2

**Output of Pass 1:**
- All nodes have `rank` field set
- Graph is acyclic (some edges reversed)
- Ranks are normalized (min = 0)

**Input to Pass 2:**
- Nodes with rank assignments
- Original graph structure

**Integration:**
```elisp
(defun dag-draw-passes-1-2 (graph)
  (dag-draw-rank graph)
  (when (dag-draw--verify-ranking graph)
    (dag-draw-ordering graph)))
```

### Between Pass 2 and Pass 3

**Output of Pass 2:**
- Virtual nodes created for long edges
- All nodes have `order` field set (position within rank)
- Graph structure includes virtual nodes

**Input to Pass 3:**
- Nodes with rank and order
- Edges possibly split into chains through virtual nodes

**Integration:**
```elisp
(defun dag-draw-passes-2-3 (graph)
  (dag-draw-ordering graph)
  (when (dag-draw--verify-ordering graph)
    (dag-draw-position graph)))
```

### Between Pass 3 and Pass 4

**Output of Pass 3:**
- All nodes have `x` and `y` coordinates
- Coordinates are continuous (float) values

**Input to Pass 4:**
- Positioned nodes
- Full edge structure (including virtual nodes)

**Integration:**
```elisp
(defun dag-draw-passes-3-4 (graph)
  (dag-draw-position graph)
  (when (dag-draw--verify-positioning graph)
    (dag-draw-make-splines graph)))
```

### Between Pass 4 and ASCII Rendering

**Output of Pass 4:**
- All edges have `control-points` set
- Control points are continuous coordinates

**Input to ASCII Rendering:**
- Complete graph with coordinates and control points

**Integration:**
```elisp
(defun dag-draw-complete (graph)
  (dag-draw-rank graph)
  (dag-draw-ordering graph)
  (dag-draw-position graph)
  (dag-draw-make-splines graph)
  (dag-draw-render-ascii graph))
```

---

## Verification Functions

Each pass should have verification to catch errors early:

```elisp
(defun dag-draw--verify-ranking (graph)
  "Verify all edges respect rank constraints."
  (let ((valid t))
    (maphash (lambda (_id edge)
               (let* ((tail (dag-draw-edge-tail edge))
                      (head (dag-draw-edge-head edge))
                      (length (- (dag-draw-node-rank head)
                                (dag-draw-node-rank tail)))
                      (delta (dag-draw-edge-delta edge)))
                 (when (< length delta)
                   (message "Edge %s violates delta constraint: length=%d delta=%d"
                           (dag-draw-edge-id edge) length delta)
                   (setq valid nil))))
             (dag-draw-graph-edges graph))
    valid))

(defun dag-draw--verify-ordering (graph)
  "Verify all nodes have valid order assignments."
  (let ((valid t))
    (dotimes (rank (1+ (dag-draw-graph-max-rank graph)))
      (let* ((nodes (aref (dag-draw-graph-ranks graph) rank))
             (orders (mapcar #'dag-draw-node-order nodes)))
        (unless (equal orders (cl-sort (copy-sequence orders) #'<))
          (message "Rank %d has invalid ordering" rank)
          (setq valid nil))))
    valid))

(defun dag-draw--verify-positioning (graph)
  "Verify all nodes have coordinates and respect separation."
  (let ((valid t))
    (dotimes (rank (1+ (dag-draw-graph-max-rank graph)))
      (let ((nodes (aref (dag-draw-graph-ranks graph) rank)))
        (dotimes (i (1- (length nodes)))
          (let* ((left (nth i nodes))
                 (right (nth (1+ i) nodes))
                 (sep (- (dag-draw-node-x right) (dag-draw-node-x left)))
                 (min-sep (dag-draw--compute-rho left right graph)))
            (when (< sep min-sep)
              (message "Nodes %s and %s violate separation: %f < %f"
                      (dag-draw-node-id left) (dag-draw-node-id right)
                      sep min-sep)
              (setq valid nil))))))
    valid))
```

---

**Document Version**: 1.0
**Date**: 2025-10-13
**Author**: dag-draw.el development team
