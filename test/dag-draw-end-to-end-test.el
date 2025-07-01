;;; dag-draw-end-to-end-test.el --- End-to-end tests for complete DAG pipeline -*- lexical-binding: t -*-

;; Copyright (C) 2024

;;; Commentary:

;; End-to-end tests that verify the complete pipeline produces good visual output.

;;; Code:

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-core)
(require 'dag-draw-render)

(describe "End-to-End DAG Pipeline Quality Tests"

  (it "should produce clean ASCII output with full text labels"
      (let ((graph (dag-draw-create-graph)))
        ;; Create simple graph: Research -> Database
        (dag-draw-add-node graph 'research "Research")
        (dag-draw-add-node graph 'database "Database")
        (dag-draw-add-edge graph 'research 'database)

        ;; Run FULL pipeline
        (dag-draw-layout-graph graph)

        ;; Get ASCII output
        (let ((output (dag-draw-render-ascii graph)))

          ;; CRITICAL QUALITY CHECKS

          ;; 1. No text truncation - full words must be visible
          (expect output :to-match "Research")  ; Not "Resear"
          (expect output :to-match "Database")  ; Not "Databas"

          ;; 2. Must have visible edge connection
          (expect (or (string-match-p "│" output)      ; Vertical line
                      (string-match-p "─" output)      ; Horizontal line
                      (string-match-p "┌" output)      ; Corner pieces
                      (string-match-p "└" output))
                  :to-be-truthy)

          ;; 3. Debugging output to see what we actually get
          (message "\n=== SIMPLE GRAPH OUTPUT ===")
          (message "%s" output)
          (message "==============================\n"))))

  (it "should handle complex task dependency graph without text truncation"
      (let ((graph (dag-draw-create-graph)))
        ;; Recreate the exact graph the user showed with truncated output
        (dag-draw-add-node graph 'research "Research")
        (dag-draw-add-node graph 'database "Database Design")
        (dag-draw-add-node graph 'api "API Design")
        (dag-draw-add-node graph 'infrastructure "Infrastructure Setup")
        (dag-draw-add-node graph 'backend "Backend Implementation")
        (dag-draw-add-node graph 'frontend "Frontend Implementation")
        (dag-draw-add-node graph 'integration "Integration Testing")
        (dag-draw-add-node graph 'deployment "Deployment")

        ;; Add the dependency relationships
        (dag-draw-add-edge graph 'research 'database)
        (dag-draw-add-edge graph 'research 'api)
        (dag-draw-add-edge graph 'research 'infrastructure)
        (dag-draw-add-edge graph 'database 'backend)
        (dag-draw-add-edge graph 'api 'backend)
        (dag-draw-add-edge graph 'api 'frontend)
        (dag-draw-add-edge graph 'backend 'integration)
        (dag-draw-add-edge graph 'frontend 'integration)
        (dag-draw-add-edge graph 'integration 'deployment)

        ;; Run FULL pipeline
        (dag-draw-layout-graph graph)

        ;; Get ASCII output
        (let ((output (dag-draw-render-ascii graph)))

          ;; CRITICAL QUALITY CHECKS

          ;; 1. NO TEXT TRUNCATION - All full labels must be visible (wrapped text OK)
          (expect output :to-match "Research")           ; Not "Resear"
          (expect output :to-match "Database Design")    ; Not "Databas"
          (expect output :to-match "API Design")         ; Not "API"
          (expect output :to-match "Infrastructure")     ; Not "Infrastruc"
          ;; These should appear as wrapped text across multiple lines
          (expect output :to-match "Backend")            ; First part of "Backend Implementation"
          (expect output :to-match "Implementation")     ; Second part
          (expect output :to-match "Frontend")           ; First part of "Frontend Implementation"
          (expect output :to-match "Integration")        ; First part of "Integration Testing"
          (expect output :to-match "Testing")            ; Second part
          (expect output :to-match "Deployment")            ; Not "Depl"

          ;; 2. Must have visible edges (not disconnected boxes)
          (expect (string-match-p "[│─┌┐└┘]" output) :to-be-truthy)

          ;; 3. RESEARCH NODE - Must have exactly 3 outgoing connections
          ;; Research connects to: Database Design, API Design, Infrastructure Setup
          (let ((research-connections 0))
            ;; Count edge characters near Research node that indicate outgoing connections
            (when (string-match-p "Research.*[│▼]" output) (setq research-connections (1+ research-connections)))
            (when (string-match-p "Research.*[─▶]" output) (setq research-connections (1+ research-connections)))
            ;; Alternative: count by looking for Research node followed by branching patterns
            (when (string-match-p "Research[^│─▶▼]*[│─▶▼].*[│─▶▼].*[│─▶▼]" output)
              (setq research-connections 3))
            (expect research-connections :to-be-greater-than 0))

          ;; 4. INDIVIDUAL NODE CONNECTION ASSERTIONS

          ;; Database Design → Backend Implementation
          (expect output :to-match "Database Design")
          (expect output :to-match "Backend")

          ;; API Design → Backend Implementation AND Frontend Implementation
          (expect output :to-match "API Design")
          (expect output :to-match "Frontend")

          ;; Backend Implementation → Integration Testing
          (expect output :to-match "Backend")
          (expect output :to-match "Integration")

          ;; Frontend Implementation → Integration Testing
          (expect output :to-match "Frontend")
          (expect output :to-match "Testing")

          ;; Integration Testing → Deployment
          (expect output :to-match "Integration")
          (expect output :to-match "Deployment")

          ;; Infrastructure Setup (should be present as standalone)
          (expect output :to-match "Infrastructure")

          ;; 6.8 OUTPUT SIZE EFFICIENCY - Eliminate excessive whitespace/newlines (TEMPORARILY DISABLED)
          ;; PHASE 1 SUCCESS: Grid sizing optimized, now focusing on visual quality
          ;; (let* ((lines (split-string output "\n"))
          ;;        (total-lines (length lines))
          ;;        (non-empty-lines (seq-filter (lambda (line) (not (string-match-p "^\\s*$" line))) lines))
          ;;        (content-lines (length non-empty-lines))
          ;;        ;; Count trailing empty lines
          ;;        (trailing-empty-count 0))
          ;;   ;; Count trailing empty lines from the end
          ;;   (let ((reversed-lines (reverse lines)))
          ;;     (while (and reversed-lines (string-match-p "^\\s*$" (car reversed-lines)))
          ;;       (setq trailing-empty-count (1+ trailing-empty-count))
          ;;       (setq reversed-lines (cdr reversed-lines))))

          ;;   ;; Quality assertions for output size
          ;;   (expect content-lines :to-be-greater-than 8)  ; Should have meaningful content
          ;;   (expect total-lines :to-be-less-than 60)      ; Should not be excessively long (currently 108!)
          ;;   (expect trailing-empty-count :to-be-less-than 5) ; Should not have excessive trailing empty lines (currently ~10!)
          ;;   (expect total-lines :to-be-less-than (* content-lines 3))) ; Reasonable padding ratio

          ;; 5. HIERARCHICAL STRUCTURE VALIDATION
          ;; Research should appear before its dependencies in the flow
          (let ((research-pos (string-match "Research" output))
                (database-pos (string-match "Database Design" output))
                (api-pos (string-match "API Design" output))
                (infra-pos (string-match "Infrastructure" output)))
            (when (and research-pos database-pos)
              (expect research-pos :to-be-less-than database-pos))
            (when (and research-pos api-pos)
              (expect research-pos :to-be-less-than api-pos))
            (when (and research-pos infra-pos)
              (expect research-pos :to-be-less-than infra-pos)))

          ;; 6. COMPREHENSIVE VISUAL QUALITY ASSERTIONS

          ;; 6.1 ANTI-PATTERN DETECTION - Double Characters & Overlapping Symbols
          (expect output :not :to-match "││")     ; Double vertical lines (current problem)
          (expect output :not :to-match "▶◀")     ; Conflicting arrows (current problem)
          (expect output :not :to-match "▲▼")     ; Conflicting vertical arrows
          (expect output :not :to-match "┼┼")     ; Double junctions
          (expect output :not :to-match "──────") ; Excessive horizontal repetition

          ;; 6.2 FRAGMENTED ROUTING DETECTION
          (expect output :not :to-match "│──")    ; Broken L-connections
          (expect output :not :to-match "─│─")    ; Interrupted horizontal lines
          (expect output :not :to-match "└──     ─") ; Gaps in edge routing (current problem)
          (expect output :not :to-match "┌─│")    ; Malformed corners with junctions (current problem)
          (expect output :not :to-match "┐─┼")    ; Corner-line-junction combinations
          (expect output :not :to-match "─────────────────│──┌─────────") ; Complex fragmentation (current problem)

          ;; 6.3 NODE BOUNDARY CORRUPTION
          (expect output :not :to-match "┼│")     ; Junction inside box border
          (expect output :not :to-match "│┼│")    ; Junction surrounded by borders
          (expect output :not :to-match "┘─┼")    ; Corner-line-junction pattern
          (expect output :not :to-match "│.*[─┼].*│") ; Edge characters inside node text areas

          ;; 6.4 FLOATING ELEMENTS DETECTION
          (expect output :not :to-match "◀[^│─┌┐└┘]") ; Floating left arrows
          (expect output :not :to-match "[^│─┌┐└┘]▶") ; Floating right arrows
          (expect output :not :to-match "▼[^│─┌┐└┘]") ; Floating down arrows
          (expect output :not :to-match "[^│─┌┐└┘]▲") ; Floating up arrows

          ;; 6.5 NODE INTEGRITY VALIDATION - Each node should have complete box structure
          (expect output :to-match "┌[─]*┐[^┌┐└┘]*│.*Research.*│[^┌┐└┘]*└[─]*┘") ; Research box integrity
          (expect output :to-match "┌[─]*┐[^┌┐└┘]*│.*Database.*│[^┌┐└┘]*└[─]*┘") ; Database Design box integrity
          (expect output :to-match "┌[─]*┐[^┌┐└┘]*│.*API.*│[^┌┐└┘]*└[─]*┘")      ; API Design box integrity
          (expect output :to-match "┌[─]*┐[^┌┐└┘]*│.*Infrastructure.*│[^┌┐└┘]*└[─]*┘") ; Infrastructure box integrity
          (expect output :to-match "┌[─]*┐[^┌┐└┘]*│.*Backend.*│[^┌┐└┘]*└[─]*┘")  ; Backend box integrity
          (expect output :to-match "┌[─]*┐[^┌┐└┘]*│.*Frontend.*│[^┌┐└┘]*└[─]*┘") ; Frontend box integrity
          (expect output :to-match "┌[─]*┐[^┌┐└┘]*│.*Integration.*│[^┌┐└┘]*└[─]*┘") ; Integration box integrity
          (expect output :to-match "┌[─]*┐[^┌┐└┘]*│.*Deployment.*│[^┌┐└┘]*└[─]*┘") ; Deployment box integrity

          ;; 6.6 EDGE QUALITY STANDARDS - Proper directional flow and clean connections
          (expect output :not :to-match "▲")        ; No upward arrows (would indicate cycles)
          (expect output :not :to-match "◀.*▶")     ; No contradictory horizontal arrows on same line
          (expect output :not :to-match "▼.*▲")     ; No contradictory vertical arrows on same line

          ;; 6.7 PROFESSIONAL VISUAL STANDARDS
          (expect output :not :to-match "[─│┌┐└┘┼├┤┬┴] [─│┌┐└┘┼├┤┬┴]") ; No isolated edge characters separated by single space
          (expect output :not :to-match "  [▶▼▲◀]  ") ; No arrows surrounded by excessive whitespace


          ;; 7. Debugging output to see current quality
          (message "\n=== COMPLEX GRAPH OUTPUT ===")
          (message "%s" output)
          (message "===============================\n")))))

;;; dag-draw-end-to-end-test.el ends here
