;;; test-visual-ascii-properties.el --- Visual test for ASCII properties -*- lexical-binding: t -*-

;; This script demonstrates the new ASCII visual properties.
;; Run with: ~/bin/eldev exec emacs --batch -l test-visual-ascii-properties.el

(require 'dag-draw)
(require 'ht)

(message "\n=== ASCII VISUAL PROPERTIES DEMO ===\n")

;; Test 1: Highlight attribute
(message "TEST 1: :ascii-highlight attribute")
(message "--------------------------------------")
(let ((graph (dag-draw-create-graph)))
  (dag-draw-add-node graph 'normal "Normal Node")
  (dag-draw-add-node graph 'highlighted "Highlighted" (ht (:ascii-highlight t)))
  (dag-draw-add-edge graph 'normal 'highlighted)
  (dag-draw-layout-graph graph)
  (message "%s" (dag-draw-render-graph graph 'ascii)))

(message "\nExpected: 'Normal Node' should have single-line borders (┌┐└┘─│)")
(message "         'Highlighted' should have double-line borders (╔╗╚╝═║)\n")

;; Test 2: Marker attribute
(message "\nTEST 2: :ascii-marker attribute")
(message "--------------------------------------")
(let ((graph (dag-draw-create-graph)))
  (dag-draw-add-node graph 'plain "Plain Task")
  (dag-draw-add-node graph 'starred "Important" (ht (:ascii-marker "* ")))
  (dag-draw-add-node graph 'checked "Completed" (ht (:ascii-marker "[X] ")))
  (dag-draw-add-edge graph 'plain 'starred)
  (dag-draw-add-edge graph 'plain 'checked)
  (dag-draw-layout-graph graph)
  (message "%s" (dag-draw-render-graph graph 'ascii)))

(message "\nExpected: First node has no marker")
(message "         Second node shows '* Important'")
(message "         Third node shows '[X] Completed'\n")

;; Test 3: Combined attributes
(message "\nTEST 3: Combined :ascii-highlight and :ascii-marker")
(message "----------------------------------------------------")
(let ((graph (dag-draw-create-graph)))
  (dag-draw-add-node graph 'critical "Critical Task"
                     (ht (:ascii-highlight t) (:ascii-marker "! ")))
  (dag-draw-add-node graph 'normal "Normal Task")
  (dag-draw-add-edge graph 'critical 'normal)
  (dag-draw-layout-graph graph)
  (message "%s" (dag-draw-render-graph graph 'ascii)))

(message "\nExpected: 'Critical Task' has double-line borders AND '! ' prefix")
(message "         'Normal Task' has single-line borders and no prefix\n")

(message "=== END OF ASCII VISUAL PROPERTIES DEMO ===\n")

;;; test-visual-ascii-properties.el ends here
