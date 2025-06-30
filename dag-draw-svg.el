;;; dag-draw-svg.el --- SVG rendering for dag-draw -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; SVG rendering implementation for dag-draw graphs.
;; Converts positioned graphs into scalable vector graphics with 
;; smooth splines, proper arrow markers, and clean styling.

;;; Code:

(require 'dag-draw-core)

;;; Customization

(defcustom dag-draw-render-svg-node-fill "#f0f0f0"
  "Default fill color for SVG nodes."
  :type 'string
  :group 'dag-draw-render)

(defcustom dag-draw-render-svg-node-stroke "#000000"
  "Default stroke color for SVG nodes."
  :type 'string
  :group 'dag-draw-render)

(defcustom dag-draw-render-svg-edge-stroke "#666666"
  "Default stroke color for SVG edges."
  :type 'string
  :group 'dag-draw-render)

;;; SVG Rendering

(defun dag-draw-render-svg (graph)
  "Render GRAPH as SVG string with positioned nodes and smooth spline edges."
  (let* ((bounds (dag-draw-get-graph-bounds graph))
         (min-x (nth 0 bounds))
         (min-y (nth 1 bounds))
         (max-x (nth 2 bounds))
         (max-y (nth 3 bounds))
         (width (- max-x min-x))
         (height (- max-y min-y))
         (margin 20)
         (svg-width (+ width (* 2 margin)))
         (svg-height (+ height (* 2 margin))))

    (concat
     (dag-draw--svg-header svg-width svg-height (- min-x margin) (- min-y margin) width height)
     (dag-draw--svg-defs)
     (dag-draw--svg-render-edges graph)
     (dag-draw--svg-render-nodes graph)
     (dag-draw--svg-footer))))

(defun dag-draw--svg-header (svg-width svg-height view-x view-y view-width view-height)
  "Generate SVG header with dimensions and viewBox."
  (format "<svg width=\"%.1f\" height=\"%.1f\" viewBox=\"%.1f %.1f %.1f %.1f\" xmlns=\"http://www.w3.org/2000/svg\">\n"
          svg-width svg-height view-x view-y view-width view-height))

(defun dag-draw--svg-defs ()
  "Generate SVG definitions for arrow markers and styles."
  (concat
   "  <defs>\n"
   "    <marker id=\"arrowhead\" markerWidth=\"10\" markerHeight=\"7\" refX=\"9\" refY=\"3.5\" orient=\"auto\">\n"
   "      <polygon points=\"0 0, 10 3.5, 0 7\" fill=\"" dag-draw-render-svg-edge-stroke "\" />\n"
   "    </marker>\n"
   "    <style><![CDATA[\n"
   "      .node { fill: " dag-draw-render-svg-node-fill "; stroke: " dag-draw-render-svg-node-stroke "; stroke-width: 1; }\n"
   "      .node-label { font-family: Arial, sans-serif; font-size: 12px; text-anchor: middle; dominant-baseline: central; }\n"
   "      .edge { fill: none; stroke: " dag-draw-render-svg-edge-stroke "; stroke-width: 2; marker-end: url(#arrowhead); }\n"
   "      .edge-label { font-family: Arial, sans-serif; font-size: 10px; text-anchor: middle; }\n"
   "    ]]></style>\n"
   "  </defs>\n"))

(defun dag-draw--svg-render-nodes (graph)
  "Render all nodes as SVG rectangles with labels."
  (let ((node-svg "  <g class=\"nodes\">\n"))
    (ht-each (lambda (node-id node)
               (let* ((x (or (dag-draw-node-x-coord node) 0))
                      (y (or (dag-draw-node-y-coord node) 0))
                      (width (dag-draw-node-x-size node))
                      (height (dag-draw-node-y-size node))
                      (label (dag-draw-node-label node))
                      (rect-x (- x (/ width 2.0)))
                      (rect-y (- y (/ height 2.0))))

                 (setq node-svg
                       (concat node-svg
                               (format "    <rect class=\"node\" x=\"%.1f\" y=\"%.1f\" width=\"%.1f\" height=\"%.1f\" rx=\"3\" />\n"
                                       rect-x rect-y width height)
                               (format "    <text class=\"node-label\" x=\"%.1f\" y=\"%.1f\">%s</text>\n"
                                       x y (dag-draw--escape-xml label))))))
             (dag-draw-graph-nodes graph))

    (concat node-svg "  </g>\n")))

(defun dag-draw--svg-render-edges (graph)
  "Render all edges as SVG paths with smooth splines."
  (let ((edge-svg "  <g class=\"edges\">\n"))
    (dolist (edge (dag-draw-graph-edges graph))
      ;; Render edge path if spline points exist
      (when (dag-draw-edge-spline-points edge)
        (let ((path-data (dag-draw--svg-path-from-spline edge)))
          (setq edge-svg
                (concat edge-svg
                        (format "    <path class=\"edge\" d=\"%s\" />\n" path-data)))))

      ;; Render edge label if present (independent of spline points)
      (when (dag-draw-edge-label edge)
        (let ((label-pos (dag-draw-edge-label-position edge)))
          (when label-pos
            (setq edge-svg
                  (concat edge-svg
                          (format "    <text class=\"edge-label\" x=\"%.1f\" y=\"%.1f\">%s</text>\n"
                                  (dag-draw-point-x label-pos)
                                  (dag-draw-point-y label-pos)
                                  (dag-draw--escape-xml (dag-draw-edge-label edge)))))))))

    (concat edge-svg "  </g>\n")))

(defun dag-draw--svg-path-from-spline (edge)
  "Convert edge spline points to SVG path data."
  (let ((points (dag-draw-edge-spline-points edge)))
    (when points
      (let ((path-data (format "M %.1f,%.1f"
                               (dag-draw-point-x (car points))
                               (dag-draw-point-y (car points)))))

        ;; Add line segments for all remaining points
        (dolist (point (cdr points))
          (setq path-data
                (concat path-data
                        (format " L %.1f,%.1f"
                                (dag-draw-point-x point)
                                (dag-draw-point-y point)))))

        path-data))))

(defun dag-draw--svg-footer ()
  "Generate SVG footer."
  "</svg>\n")

(defun dag-draw--escape-xml (text)
  "Escape XML special characters in TEXT."
  ;; Escape & first, then other characters (avoiding double-escaping)
  (let ((escaped-ampersand (replace-regexp-in-string "&" "&amp;" text)))
    (replace-regexp-in-string
     "'" "&apos;"
     (replace-regexp-in-string
      "\"" "&quot;"
      (replace-regexp-in-string
       ">" "&gt;"
       (replace-regexp-in-string "<" "&lt;" escaped-ampersand))))))

(provide 'dag-draw-svg)

;;; dag-draw-svg.el ends here