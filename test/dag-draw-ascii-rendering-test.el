;;; dag-draw-ascii-rendering-test.el --- Unit tests for ASCII rendering components -*- lexical-binding: t -*-

;; Copyright (C) 2024

;;; Commentary:

;; Comprehensive unit tests for ASCII rendering functionality in dag-draw.
;; These tests focus on the low-level components: grid creation, coordinate
;; mapping, box drawing, and text placement.

;;; Code:

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-core)
(require 'dag-draw-render)

(describe "ASCII Grid Creation"
  
  (describe "dag-draw--create-ascii-grid"
    (it "should create grid with correct dimensions"
      (let ((grid (dag-draw--create-ascii-grid 5 3)))
        (expect (length grid) :to-equal 3)
        (expect (length (aref grid 0)) :to-equal 5)
        (expect (length (aref grid 1)) :to-equal 5)
        (expect (length (aref grid 2)) :to-equal 5)))
    
    (it "should initialize grid with spaces"
      (let ((grid (dag-draw--create-ascii-grid 3 2)))
        (expect (aref (aref grid 0) 0) :to-equal ?\s)
        (expect (aref (aref grid 0) 2) :to-equal ?\s)
        (expect (aref (aref grid 1) 1) :to-equal ?\s)))
    
    (it "should handle minimal grid sizes"
      (let ((grid (dag-draw--create-ascii-grid 1 1)))
        (expect (length grid) :to-equal 1)
        (expect (length (aref grid 0)) :to-equal 1)
        (expect (aref (aref grid 0) 0) :to-equal ?\s)))
    
    (it "should handle zero dimensions gracefully"
      (let ((grid (dag-draw--create-ascii-grid 0 0)))
        (expect (length grid) :to-equal 0)))
    
    (it "should handle large grids efficiently"
      (let ((grid (dag-draw--create-ascii-grid 100 50)))
        (expect (length grid) :to-equal 50)
        (expect (length (aref grid 0)) :to-equal 100)
        (expect (length (aref grid 49)) :to-equal 100))))

(describe "Coordinate System and Bounds Calculation"
  
  (describe "dag-draw-get-graph-bounds"
    (it "should calculate correct bounds for positioned nodes"
      (let ((graph (dag-draw-create-graph)))
        ;; Add nodes with specific coordinates and sizes
        (dag-draw-add-node graph 'a "Node A")
        (dag-draw-add-node graph 'b "Node B")
        
        ;; Set coordinates and sizes
        (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'a)) 100)
        (setf (dag-draw-node-y-coord (dag-draw-get-node graph 'a)) 200)
        (setf (dag-draw-node-x-size (dag-draw-get-node graph 'a)) 80)
        (setf (dag-draw-node-y-size (dag-draw-get-node graph 'a)) 60)
        
        (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'b)) 300)
        (setf (dag-draw-node-y-coord (dag-draw-get-node graph 'b)) 400)
        (setf (dag-draw-node-x-size (dag-draw-get-node graph 'b)) 100)
        (setf (dag-draw-node-y-size (dag-draw-get-node graph 'b)) 40)
        
        (let ((bounds (dag-draw-get-graph-bounds graph)))
          ;; min-x = 100 - 80/2 = 60
          (expect (nth 0 bounds) :to-equal 60.0)
          ;; min-y = 200 - 60/2 = 170
          (expect (nth 1 bounds) :to-equal 170.0)
          ;; max-x = 300 + 100/2 = 350
          (expect (nth 2 bounds) :to-equal 350.0)
          ;; max-y = 400 + 40/2 = 420
          (expect (nth 3 bounds) :to-equal 420.0))))
    
    (it "should handle empty graphs"
      (let ((graph (dag-draw-create-graph)))
        (let ((bounds (dag-draw-get-graph-bounds graph)))
          (expect (length bounds) :to-equal 4)
          ;; Should return default bounds
          (expect (nth 0 bounds) :to-equal 0)
          (expect (nth 1 bounds) :to-equal 0)
          (expect (nth 2 bounds) :to-equal 100)
          (expect (nth 3 bounds) :to-equal 100))))
    
    (it "should handle single node"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'single "Single")
        (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'single)) 50)
        (setf (dag-draw-node-y-coord (dag-draw-get-node graph 'single)) 75)
        (setf (dag-draw-node-x-size (dag-draw-get-node graph 'single)) 30)
        (setf (dag-draw-node-y-size (dag-draw-get-node graph 'single)) 20)
        
        (let ((bounds (dag-draw-get-graph-bounds graph)))
          (expect (nth 0 bounds) :to-equal 35.0)  ; 50 - 30/2
          (expect (nth 1 bounds) :to-equal 65.0)  ; 75 - 20/2
          (expect (nth 2 bounds) :to-equal 65.0)  ; 50 + 30/2
          (expect (nth 3 bounds) :to-equal 85.0)))) ; 75 + 20/2
    
    (it "should handle nodes with zero coordinates"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'origin "Origin")
        (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'origin)) 0)
        (setf (dag-draw-node-y-coord (dag-draw-get-node graph 'origin)) 0)
        (setf (dag-draw-node-x-size (dag-draw-get-node graph 'origin)) 50)
        (setf (dag-draw-node-y-size (dag-draw-get-node graph 'origin)) 30)
        
        (let ((bounds (dag-draw-get-graph-bounds graph)))
          (expect (nth 0 bounds) :to-equal -25.0)
          (expect (nth 1 bounds) :to-equal -15.0)
          (expect (nth 2 bounds) :to-equal 25.0)
          (expect (nth 3 bounds) :to-equal 15.0)))))

(describe "Grid Coordinate Mapping"
  
  (it "should map world coordinates to grid coordinates correctly"
    ;; Test the coordinate transformation used in ASCII rendering
    (let* ((min-x 0)
           (min-y 0)
           (scale 2)
           (world-x 100)
           (world-y 200)
           ;; Use the new helper function for consistent calculation
           (grid-x (dag-draw--world-to-grid-coord world-x min-x scale))
           (grid-y (dag-draw--world-to-grid-coord world-y min-y scale)))
      
      (expect grid-x :to-equal 16)  ; 100 * 2 * 0.08 = 16  
      (expect grid-y :to-equal 32)  ; 200 * 2 * 0.08 = 32
      ))
  
  (it "should handle negative coordinates"
    (let* ((min-x -50)
           (min-y -25)
           (scale 2)
           (world-x 0)
           (world-y 0)
           (grid-x (dag-draw--world-to-grid-coord world-x min-x scale))
           (grid-y (dag-draw--world-to-grid-coord world-y min-y scale)))
      
      (expect grid-x :to-equal 8)  ; (0 - (-50)) * 2 * 0.08 = 8
      (expect grid-y :to-equal 4)   ; (0 - (-25)) * 2 * 0.08 = 4
      ))
  
  (it "should scale node sizes appropriately"
    ;; Test node size scaling used in dag-draw--ascii-draw-nodes
    (let* ((scale 2)
           (node-width 50)
           (node-height 30)
           ;; Use the new helper function for consistent calculation
           (grid-width (dag-draw--world-to-grid-size node-width scale))
           (grid-height (dag-draw--world-to-grid-size node-height scale)))
      
      ;; With dag-draw-ascii-box-scale = 0.071: max(3, ceil(50 * 2 * 0.071)) = max(3, 8) = 8
      (expect grid-width :to-equal 8)  
      ;; max(3, ceil(30 * 2 * 0.071)) = max(3, 5) = 5
      (expect grid-height :to-equal 5)
      )))

(describe "ASCII Grid Utilities"
  
  (describe "grid manipulation"
    (it "should safely set characters within bounds"
      (let ((grid (dag-draw--create-ascii-grid 5 3)))
        ;; Set a character and verify
        (aset (aref grid 1) 2 ?X)
        (expect (aref (aref grid 1) 2) :to-equal ?X)
        ;; Other positions should remain spaces
        (expect (aref (aref grid 0) 0) :to-equal ?\s)
        (expect (aref (aref grid 2) 4) :to-equal ?\s)))
    
    (it "should handle grid boundaries correctly"
      (let ((grid (dag-draw--create-ascii-grid 3 2)))
        ;; Test boundary positions
        (aset (aref grid 0) 0 ?A)  ; top-left
        (aset (aref grid 0) 2 ?B)  ; top-right
        (aset (aref grid 1) 0 ?C)  ; bottom-left
        (aset (aref grid 1) 2 ?D)  ; bottom-right
        
        (expect (aref (aref grid 0) 0) :to-equal ?A)
        (expect (aref (aref grid 0) 2) :to-equal ?B)
        (expect (aref (aref grid 1) 0) :to-equal ?C)
        (expect (aref (aref grid 1) 2) :to-equal ?D))))

(describe "Grid to String Conversion"
  
  (describe "dag-draw--ascii-grid-to-string"
    (it "should convert simple grid to string"
      (let ((grid (dag-draw--create-ascii-grid 3 2)))
        (aset (aref grid 0) 0 ?A)
        (aset (aref grid 0) 1 ?B)
        (aset (aref grid 0) 2 ?C)
        (aset (aref grid 1) 0 ?D)
        (aset (aref grid 1) 1 ?E)
        (aset (aref grid 1) 2 ?F)
        
        (let ((result (dag-draw--ascii-grid-to-string grid)))
          (expect result :to-equal "ABC\nDEF"))))
    
    (it "should handle empty grid"
      (let ((grid (dag-draw--create-ascii-grid 0 0)))
        (let ((result (dag-draw--ascii-grid-to-string grid)))
          (expect result :to-equal ""))))
    
    (it "should handle single row"
      (let ((grid (dag-draw--create-ascii-grid 5 1)))
        (aset (aref grid 0) 0 ?H)
        (aset (aref grid 0) 1 ?E)
        (aset (aref grid 0) 2 ?L)
        (aset (aref grid 0) 3 ?L)
        (aset (aref grid 0) 4 ?O)
        
        (let ((result (dag-draw--ascii-grid-to-string grid)))
          (expect result :to-equal "HELLO"))))
    
    (it "should preserve spaces in output"
      (let ((grid (dag-draw--create-ascii-grid 3 2)))
        ;; Leave some positions as spaces
        (aset (aref grid 0) 0 ?X)
        (aset (aref grid 0) 2 ?Y)
        (aset (aref grid 1) 1 ?Z)
        
        (let ((result (dag-draw--ascii-grid-to-string grid)))
          (expect result :to-equal "X Y\n Z")))))))))

;;; dag-draw-ascii-rendering-test.el ends here