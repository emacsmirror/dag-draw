;;; test-harness-selection-test.el --- Tests for selection test harness functions -*- lexical-binding: t; -*-

;; Tests for test harness extensions that support node selection visualization testing

(add-to-list 'load-path (expand-file-name "test/helpers" (locate-dominating-file default-directory "Eldev")))

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-test-harness)

(describe "Test Harness - ASCII Box Style Detection"
  (describe "dag-draw-test--get-node-box-style"
    (it "detects single-line box style"
      (let ((ascii-grid "┌───┐\n│ A │\n└───┘"))
        (expect (dag-draw-test--get-node-box-style ascii-grid 0 0)
                :to-equal 'single-line)))

    (it "detects double-line box style"
      (let ((ascii-grid "╔═══╗\n║ A ║\n╚═══╝"))
        (expect (dag-draw-test--get-node-box-style ascii-grid 0 0)
                :to-equal 'double-line)))

    (it "handles invalid coordinates gracefully"
      (let ((ascii-grid "┌───┐\n│ A │\n└───┘"))
        (expect (dag-draw-test--get-node-box-style ascii-grid 100 100)
                :to-equal nil)))

    (it "handles non-box characters"
      (let ((ascii-grid "     \n  A  \n     "))
        (expect (dag-draw-test--get-node-box-style ascii-grid 0 0)
                :to-equal nil)))))

(describe "Test Harness - Selection Validation"
  (describe "dag-draw-test--validate-selection-visualization"
    (it "validates all selected nodes have double-line style"
      (let* ((ascii-grid "╔═══╗\n║ A ║\n╚═══╝\n\n┌───┐\n│ B │\n└───┘")
             (all-nodes (list (list :id 'node-a :x 0 :y 0 :label "A")
                             (list :id 'node-b :x 0 :y 4 :label "B")))
             (selected-ids '(node-a))
             (result (dag-draw-test--validate-selection-visualization
                      ascii-grid all-nodes selected-ids)))
        (expect (plist-get result :valid) :to-be t)
        (expect (plist-get result :issues) :to-equal nil)))

    (it "detects when selected node doesn't have double-line style"
      (let* ((ascii-grid "┌───┐\n│ A │\n└───┘\n\n┌───┐\n│ B │\n└───┘")
             (all-nodes (list (list :id 'node-a :x 0 :y 0 :label "A")
                             (list :id 'node-b :x 0 :y 4 :label "B")))
             (selected-ids '(node-a))
             (result (dag-draw-test--validate-selection-visualization
                      ascii-grid all-nodes selected-ids)))
        (expect (plist-get result :valid) :to-be nil)
        (expect (length (plist-get result :issues)) :to-be-greater-than 0)))

    (it "validates unselected nodes have single-line style"
      (let* ((ascii-grid "╔═══╗\n║ A ║\n╚═══╝\n\n┌───┐\n│ B │\n└───┘")
             (all-nodes (list (list :id 'node-a :x 0 :y 0 :label "A")
                             (list :id 'node-b :x 0 :y 4 :label "B")))
             (selected-ids '(node-a))
             (result (dag-draw-test--validate-selection-visualization
                      ascii-grid all-nodes selected-ids)))
        (expect (plist-get result :valid) :to-be t)))

    (it "detects when unselected node incorrectly has double-line style"
      (let* ((ascii-grid "╔═══╗\n║ A ║\n╚═══╝\n\n╔═══╗\n║ B ║\n╚═══╝")
             (all-nodes (list (list :id 'node-a :x 0 :y 0 :label "A")
                             (list :id 'node-b :x 0 :y 4 :label "B")))
             (selected-ids '(node-a))
             (result (dag-draw-test--validate-selection-visualization
                      ascii-grid all-nodes selected-ids)))
        (expect (plist-get result :valid) :to-be nil)
        (expect (length (plist-get result :issues)) :to-be-greater-than 0)))

    (it "handles empty selection list"
      (let* ((ascii-grid "┌───┐\n│ A │\n└───┘")
             (all-nodes (list (list :id 'node-a :x 0 :y 0 :label "A")))
             (selected-ids '())
             (result (dag-draw-test--validate-selection-visualization
                      ascii-grid all-nodes selected-ids)))
        (expect (plist-get result :valid) :to-be t)))))

(provide 'test-harness-selection-test)
;;; test-harness-selection-test.el ends here
