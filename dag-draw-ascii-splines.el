;;; dag-draw-ascii-splines.el --- ASCII-specific spline implementation -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; ASCII-specific spline implementation for dag-draw graphs.
;; This module provides simplified spline computation optimized for
;; discrete ASCII grid rendering, following GKNV Section 5.2 principles
;; but adapted for ASCII constraints.

;;; Code:

(require 'dag-draw-core)
(require 'dag-draw-ascii-grid)
(require 'dag-draw-ports)

;;; ASCII Spline Data Structure

;; Reuse the point structure from main splines module
(cl-defstruct (dag-draw-ascii-path
               (:constructor dag-draw-ascii-path-create)
               (:copier nil))
  "ASCII path represented as sequence of grid points."
  points)

;;; ASCII Spline Generation

(defun dag-draw-ascii-generate-edge-path (graph edge)
  "Generate ASCII path for EDGE following GKNV principles.
Returns list of grid points suitable for ASCII rendering."
  (let* ((from-node (dag-draw-get-node graph (dag-draw-edge-from-node edge)))
         (to-node (dag-draw-get-node graph (dag-draw-edge-to-node edge)))
         (edge-type (dag-draw-ascii--classify-edge graph edge)))
    
    (pcase edge-type
      ('inter-rank (dag-draw-ascii--create-inter-rank-path graph edge from-node to-node))
      ('flat-edge (dag-draw-ascii--create-flat-path graph edge from-node to-node))
      ('self-edge (dag-draw-ascii--create-self-path graph edge from-node))
      (_ (dag-draw-ascii--create-simple-path from-node to-node)))))

(defun dag-draw-ascii--classify-edge (graph edge)
  "Classify edge type for ASCII rendering."
  (let* ((from-node (dag-draw-get-node graph (dag-draw-edge-from-node edge)))
         (to-node (dag-draw-get-node graph (dag-draw-edge-to-node edge)))
         (from-rank (dag-draw-node-rank from-node))
         (to-rank (dag-draw-node-rank to-node)))

    (cond
     ((eq (dag-draw-edge-from-node edge) (dag-draw-edge-to-node edge))
      'self-edge)
     ((and from-rank to-rank (= from-rank to-rank))
      'flat-edge)
     (t 'inter-rank))))

;;; Inter-rank ASCII Paths

(defun dag-draw-ascii--create-inter-rank-path (graph edge from-node to-node)
  "Create ASCII path between different ranks.
Uses GKNV Section 5.1.1 port routing principles."
  (let* ((ports (dag-draw--get-edge-connection-points graph edge))
         (from-port (car ports))
         (to-port (cadr ports)))
    
    (if (and from-port to-port)
        ;; Create L-shaped path (ASCII standard)
        (dag-draw-ascii--create-l-shaped-path from-port to-port)
      ;; Fallback to simple path
      (dag-draw-ascii--create-simple-path from-node to-node))))

(defun dag-draw-ascii--create-l-shaped-path (from-port to-port)
  "Create L-shaped ASCII path between two ports.
This is the standard ASCII routing strategy."
  (let ((from-x (dag-draw-point-x from-port))
        (from-y (dag-draw-point-y from-port))
        (to-x (dag-draw-point-x to-port))
        (to-y (dag-draw-point-y to-port)))
    
    (dag-draw-ascii-path-create
     :points (list
              (dag-draw-point-create :x from-x :y from-y)
              (dag-draw-point-create :x to-x :y from-y)  ; Corner point
              (dag-draw-point-create :x to-x :y to-y)))))

;;; Flat Edge ASCII Paths

(defun dag-draw-ascii--create-flat-path (graph edge from-node to-node)
  "Create ASCII path for flat edges (same rank)."
  (let* ((from-x (dag-draw-node-x-coord from-node))
         (to-x (dag-draw-node-x-coord to-node))
         (ports (dag-draw--get-edge-connection-points graph edge))
         (from-port (car ports))
         (to-port (cadr ports)))
    
    (if (and from-port to-port)
        ;; Use port-based routing
        (dag-draw-ascii--create-horizontal-path from-port to-port)
      ;; Fallback to simple path
      (dag-draw-ascii--create-simple-path from-node to-node))))

(defun dag-draw-ascii--create-horizontal-path (from-port to-port)
  "Create horizontal ASCII path between ports."
  (dag-draw-ascii-path-create
   :points (list from-port to-port)))

;;; Self-edge ASCII Paths

(defun dag-draw-ascii--create-self-path (graph edge from-node)
  "Create ASCII path for self-edges."
  (let* ((center-x (dag-draw-node-x-coord from-node))
         (center-y (dag-draw-node-y-coord from-node))
         (width (dag-draw-node-x-size from-node))
         (height (dag-draw-node-y-size from-node))
         ;; Create loop around node
         (loop-size 3))
    
    (dag-draw-ascii-path-create
     :points (list
              (dag-draw-point-create :x (+ center-x (/ width 2)) :y center-y)
              (dag-draw-point-create :x (+ center-x (/ width 2) loop-size) :y center-y)
              (dag-draw-point-create :x (+ center-x (/ width 2) loop-size) :y (- center-y loop-size))
              (dag-draw-point-create :x (- center-x (/ width 2)) :y (- center-y loop-size))
              (dag-draw-point-create :x (- center-x (/ width 2)) :y center-y)))))

;;; Simple ASCII Paths

(defun dag-draw-ascii--create-simple-path (from-node to-node)
  "Create simple direct ASCII path between nodes."
  (let ((from-x (or (dag-draw-node-x-coord from-node) 0))
        (from-y (or (dag-draw-node-y-coord from-node) 0))
        (to-x (or (dag-draw-node-x-coord to-node) 0))
        (to-y (or (dag-draw-node-y-coord to-node) 0)))
    
    (dag-draw-ascii-path-create
     :points (list
              (dag-draw-point-create :x from-x :y from-y)
              (dag-draw-point-create :x to-x :y to-y)))))

;;; ASCII Path Utilities

(defun dag-draw-ascii-path-to-points (ascii-path)
  "Convert ASCII path to simple list of points for rendering."
  (dag-draw-ascii-path-points ascii-path))


(provide 'dag-draw-ascii-splines)

;;; dag-draw-ascii-splines.el ends here