;;; visual-properties-test.el --- Tests for visual properties feature -*- lexical-binding: t -*-

;; Copyright (C) 2025

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Tests for visual properties stored in node attributes:
;; - ASCII visual attributes (:ascii-highlight, :ascii-marker)
;; - SVG visual attributes (:svg-fill, :svg-stroke, etc.)

;;; Code:

(require 'buttercup)
(require 'ht)
(require 'dag-draw)
(require 'dag-draw-render)

;;; ASCII Visual Properties Tests

(describe "ASCII visual properties"

  (describe ":ascii-highlight attribute"

    (it "should render node with double-line box chars when :ascii-highlight is t"
      (let* ((graph (dag-draw-create-graph))
             (attrs (ht (:ascii-highlight t))))
        (dag-draw-add-node graph 'task1 "Task" attrs)
        (dag-draw-layout-graph graph)
        (let ((output (dag-draw-render-graph graph 'ascii)))
          ;; Double-line box drawing characters: ╔ ╗ ╚ ╝ ═ ║
          (expect output :to-match "╔")
          (expect output :to-match "╗")
          (expect output :to-match "╚")
          (expect output :to-match "╝"))))

    (it "should render node with single-line box chars when :ascii-highlight is not set"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'task1 "Task")
        (dag-draw-layout-graph graph)
        (let ((output (dag-draw-render-graph graph 'ascii)))
          ;; Single-line box drawing characters: ┌ ┐ └ ┘ ─ │
          (expect output :to-match "┌")
          (expect output :to-match "┐")
          (expect output :to-match "└")
          (expect output :to-match "┘")))))

  (describe ":ascii-marker attribute"

    (it "should prepend marker character to node label"
      (let* ((graph (dag-draw-create-graph))
             (attrs (ht (:ascii-marker "!"))))
        (dag-draw-add-node graph 'task1 "Task" attrs)
        (dag-draw-layout-graph graph)
        (let ((output (dag-draw-render-graph graph 'ascii)))
          ;; Should contain "!Task" in the output
          (expect output :to-match "!Task"))))

    (it "should work with multi-character markers"
      (let* ((graph (dag-draw-create-graph))
             (attrs (ht (:ascii-marker "[X]"))))
        (dag-draw-add-node graph 'task1 "Done" attrs)
        (dag-draw-layout-graph graph)
        (let ((output (dag-draw-render-graph graph 'ascii)))
          ;; Should contain "[X]Done" in the output
          (expect output :to-match "\\[X\\]Done"))))))

;;; SVG Visual Properties Tests

(describe "SVG visual properties"

  (describe ":svg-fill attribute"

    (it "should apply custom fill color to node"
      (let* ((graph (dag-draw-create-graph))
             (attrs (ht (:svg-fill "#ff5733"))))
        (dag-draw-add-node graph 'task1 "Task" attrs)
        (dag-draw-layout-graph graph)
        (let ((output (dag-draw-render-graph graph 'svg)))
          ;; Should contain style="fill: #ff5733;" in the rect element
          (expect output :to-match "style=\"fill: #ff5733;\"")))))

  (describe ":svg-stroke attribute"

    (it "should apply custom stroke color to node"
      (let* ((graph (dag-draw-create-graph))
             (attrs (ht (:svg-stroke "#0000ff"))))
        (dag-draw-add-node graph 'task1 "Task" attrs)
        (dag-draw-layout-graph graph)
        (let ((output (dag-draw-render-graph graph 'svg)))
          ;; Should contain stroke: #0000ff in style attribute
          (expect output :to-match "stroke: #0000ff")))))

  (describe ":svg-stroke-width attribute"

    (it "should apply custom stroke width to node"
      (let* ((graph (dag-draw-create-graph))
             (attrs (ht (:svg-stroke-width 3))))
        (dag-draw-add-node graph 'task1 "Task" attrs)
        (dag-draw-layout-graph graph)
        (let ((output (dag-draw-render-graph graph 'svg)))
          ;; Should contain stroke-width: 3 in style attribute
          (expect output :to-match "stroke-width: 3")))))

  (describe "combined SVG attributes"

    (it "should apply multiple SVG attributes together"
      (let* ((graph (dag-draw-create-graph))
             (attrs (ht (:svg-fill "#ff0000")
                        (:svg-stroke "#0000ff")
                        (:svg-stroke-width 2))))
        (dag-draw-add-node graph 'task1 "Task" attrs)
        (dag-draw-layout-graph graph)
        (let ((output (dag-draw-render-graph graph 'svg)))
          (expect output :to-match "fill: #ff0000")
          (expect output :to-match "stroke: #0000ff")
          (expect output :to-match "stroke-width: 2"))))))

(provide 'visual-properties-test)
;;; visual-properties-test.el ends here
