;;; dag-draw-specific-pattern-test.el --- Test specific problematic patterns -*- lexical-binding: t -*-

(require 'buttercup)
(require 'dag-draw-render)

(describe
 "Specific Problematic Patterns"

 (it "should not place junction characters directly adjacent to box borders"
     ;; Test for the specific issues: ┼│ Research │ and │└───────────┘─┼
     (let ((graph (dag-draw-create-graph)))
       (dag-draw-add-node graph 'research "Research")
       (dag-draw-add-node graph 'db-design "Database Design")
       (dag-draw-add-node graph 'api-design "API Design")
       (dag-draw-add-node graph 'backend "Backend")
       (dag-draw-add-edge graph 'research 'db-design)
       (dag-draw-add-edge graph 'research 'api-design)
       (dag-draw-add-edge graph 'db-design 'backend)
       (dag-draw-add-edge graph 'api-design 'backend)
       (dag-draw-layout-graph graph)

       (let ((ascii-output (dag-draw-render-ascii graph)))
         (message "=== SPECIFIC PATTERN TEST ===")
         (message "%s" ascii-output)

         ;; Should NOT have junction characters directly adjacent to box borders
         (expect ascii-output :not :to-match "┼│")           ; Junction left of box side
         (expect ascii-output :not :to-match "│┼")           ; Box side with junction right
         (expect ascii-output :not :to-match "┼┐")           ; Junction left of top-right corner
         (expect ascii-output :not :to-match "┼┌")           ; Junction left of top-left corner
         (expect ascii-output :not :to-match "┼└")           ; Junction left of bottom-left corner
         (expect ascii-output :not :to-match "┼┘")           ; Junction left of bottom-right corner
         (expect ascii-output :not :to-match "┐┼")           ; Top-right corner with junction right
         (expect ascii-output :not :to-match "┌┼")           ; Top-left corner with junction right
         (expect ascii-output :not :to-match "└┼")           ; Bottom-left corner with junction right
         (expect ascii-output :not :to-match "┘┼")           ; Bottom-right corner with junction right
         (expect ascii-output :not :to-match "┘─┼")          ; Specific pattern: corner-line-junction

         ;; Should have all nodes visible
         (expect ascii-output :to-match "Research")
         (expect ascii-output :to-match "Database Design")
         (expect ascii-output :to-match "API Design")
         (expect ascii-output :to-match "Backend")

         (message "============================")))))

(provide 'dag-draw-specific-pattern-test)

;;; dag-draw-specific-pattern-test.el ends here
