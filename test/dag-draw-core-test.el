;;; dag-draw-core-test.el --- Tests for dag-draw-core.el -*- lexical-binding: t -*-

;; Copyright (C) 2024

;;; Commentary:

;; Tests for core graph utility functions.

;;; Code:

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-core)

(describe "dag-draw-core utilities"
  

  (describe "graph traversal"
    (let ((graph))
      (before-each
        (setq graph (dag-draw-create-graph))
        (dag-draw-add-node graph 'a)
        (dag-draw-add-node graph 'b)
        (dag-draw-add-node graph 'c)
        (dag-draw-add-edge graph 'a 'b)
        (dag-draw-add-edge graph 'a 'c)
        (dag-draw-add-edge graph 'b 'c))
      
      (it "should get node by ID"
        (let ((node (dag-draw-get-node graph 'a)))
          (expect (dag-draw-node-id node) :to-equal 'a)))
      
      (it "should return nil for non-existent node"
        (expect (dag-draw-get-node graph 'nonexistent) :to-be nil))
      
      (it "should get outgoing edges"
        (let ((edges (dag-draw-get-edges-from graph 'a)))
          (expect (length edges) :to-equal 2)
          (expect (mapcar #'dag-draw-edge-to-node edges) 
                  :to-contain 'b)
          (expect (mapcar #'dag-draw-edge-to-node edges) 
                  :to-contain 'c)))
      
      (it "should get incoming edges"
        (let ((edges (dag-draw-get-edges-to graph 'c)))
          (expect (length edges) :to-equal 2)
          (expect (mapcar #'dag-draw-edge-from-node edges) 
                  :to-contain 'a)
          (expect (mapcar #'dag-draw-edge-from-node edges) 
                  :to-contain 'b)))
      
      (it "should get successors"
        (let ((successors (dag-draw-get-successors graph 'a)))
          (expect successors :to-contain 'b)
          (expect successors :to-contain 'c)
          (expect (length successors) :to-equal 2)))
      
      (it "should get predecessors"
        (let ((predecessors (dag-draw-get-predecessors graph 'c)))
          (expect predecessors :to-contain 'a)
          (expect predecessors :to-contain 'b)
          (expect (length predecessors) :to-equal 2)))))

  (describe "graph properties"
    (it "should find source nodes"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'root)
        (dag-draw-add-node graph 'child)
        (dag-draw-add-node graph 'isolated)
        (dag-draw-add-edge graph 'root 'child)
        (let ((sources (dag-draw-get-source-nodes graph)))
          (expect sources :to-contain 'root)
          (expect sources :to-contain 'isolated)
          (expect sources :not :to-contain 'child)))))

  (describe "graph modification"
    (let ((graph))
      (before-each
        (setq graph (dag-draw-create-graph))
        (dag-draw-add-node graph 'a)
        (dag-draw-add-node graph 'b)
        (dag-draw-add-node graph 'c)
        (dag-draw-add-edge graph 'a 'b)
        (dag-draw-add-edge graph 'b 'c))
      
      (it "should remove nodes and connected edges"
        (dag-draw-remove-node graph 'b)
        (expect (dag-draw-get-node graph 'b) :to-be nil)
        (expect (dag-draw-edge-count graph) :to-equal 0))
      
      (it "should remove specific edges"
        (dag-draw-remove-edge graph 'a 'b)
        (expect (dag-draw-edge-count graph) :to-equal 1))))

  (describe "graph copying"
    (it "should create deep copies"
      (let ((original (dag-draw-create-graph)))
        (dag-draw-add-node original 'a "Node A")
        (dag-draw-add-node original 'b "Node B")
        (dag-draw-add-edge original 'a 'b 2 "Edge Label")
        
        (let ((copy (dag-draw-copy-graph original)))
          ;; Should have same content but be different objects
          (expect (dag-draw-node-count copy) :to-equal (dag-draw-node-count original))
          (expect (dag-draw-edge-count copy) :to-equal (dag-draw-edge-count original))
          (expect copy :not :to-be original)
          
          ;; Modifying copy shouldn't affect original
          (dag-draw-add-node copy 'c)
          (expect (dag-draw-node-count original) :to-equal 2)
          (expect (dag-draw-node-count copy) :to-equal 3)))))

  (describe "debugging utilities"
    (it "should generate graph summaries"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a)
        (dag-draw-add-node graph 'b)
        (dag-draw-add-edge graph 'a 'b)
        (let ((summary (dag-draw-graph-summary graph)))
          (expect summary :to-match "2 nodes")
          (expect summary :to-match "1 edges"))))))

;;; dag-draw-core-test.el ends here