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

(provide 'visual-properties-test)
;;; visual-properties-test.el ends here
