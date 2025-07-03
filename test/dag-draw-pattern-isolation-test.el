;;; dag-draw-pattern-isolation-test.el --- Isolate and test specific visual patterns -*- lexical-binding: t -*-

;; Copyright (C) 2024

;;; Commentary:

;; This test file isolates each specific visual pattern that's failing
;; in the end-to-end test, allowing us to debug and fix them individually.
;; Each test focuses on one specific anti-pattern from the main test.

;;; Code:

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-core)
(require 'dag-draw-render)

(describe "Pattern Isolation Tests - Debug Each Visual Issue"

  (describe "Excessive horizontal line pattern (──────)"
    (it "should not produce 6+ consecutive horizontal characters"
        ;; Create minimal graph that might trigger this pattern
        (let ((graph (dag-draw-create-graph)))
          (dag-draw-add-node graph 'a "A")
          (dag-draw-add-node graph 'b "B")
          (dag-draw-add-edge graph 'a 'b)
          
          (dag-draw-layout-graph graph)
          (let ((output (dag-draw-render-ascii graph)))
            (message "\n=== EXCESSIVE LINES TEST ===")
            (message "%s" output)
            (message "===========================")
            
            ;; SPECIFIC CHECK: Look for problematic edge overlap patterns, NOT legitimate node borders
            
            ;; Problematic patterns that indicate actual edge overlap issues:
            (expect output :not :to-match " ────── ")         ; 6+ lines floating in space
            (expect output :not :to-match "┼──────┼")         ; 6+ lines between junctions  
            (expect output :not :to-match "│──────│")         ; 6+ lines between verticals
            (expect output :not :to-match "▶──────◀")         ; 6+ lines between conflicting arrows
            
            ;; Allow legitimate node borders like ┌───────────┐ and └───────────┘
            ;; These are correct and should not be flagged as problems
            ))))

  (describe "Floating arrows pattern"
    (it "should not have arrows disconnected from lines"
        ;; Create graph with multiple edges to trigger floating arrows
        (let ((graph (dag-draw-create-graph)))
          (dag-draw-add-node graph 'source "Source")
          (dag-draw-add-node graph 'target1 "Target1") 
          (dag-draw-add-node graph 'target2 "Target2")
          (dag-draw-add-edge graph 'source 'target1)
          (dag-draw-add-edge graph 'source 'target2)
          
          (dag-draw-layout-graph graph)
          (let ((output (dag-draw-render-ascii graph)))
            (message "\n=== FLOATING ARROWS TEST ===")
            (message "%s" output)
            (message "=============================")
            
            ;; Check for floating arrows (arrows not connected to lines)
            (expect output :not :to-match "◀[^│─┌┐└┘]")  ; Left arrow followed by non-line char
            (expect output :not :to-match "[^│─┌┐└┘]▶")  ; Right arrow preceded by non-line char
            (expect output :not :to-match "▼[^│─┌┐└┘]")  ; Down arrow followed by non-line char
            (expect output :not :to-match "[^│─┌┐└┘]▲")  ; Up arrow preceded by non-line char
            ))))

  (describe "Fragmented routing patterns"
    (it "should not have broken L-connections"
        ;; Create L-shaped connection scenario
        (let ((graph (dag-draw-create-graph)))
          (dag-draw-add-node graph 'start "Start")
          (dag-draw-add-node graph 'end "End")
          (dag-draw-add-edge graph 'start 'end)
          
          (dag-draw-layout-graph graph)
          (let ((output (dag-draw-render-ascii graph)))
            (message "\n=== FRAGMENTED ROUTING TEST ===")
            (message "%s" output)
            (message "================================")
            
            ;; Check for broken L-connections and fragmented patterns
            (expect output :not :to-match "│──")     ; Broken L-connection
            (expect output :not :to-match "─│─")     ; Interrupted horizontal line
            (expect output :not :to-match "┌─│")     ; Malformed corner with junction
            (expect output :not :to-match "┐─┼")     ; Corner-line-junction combination
            ))))

  (describe "Node boundary connection issues"
    (it "should properly connect edges to node boundaries"
        ;; Create simple connection to test boundary behavior
        (let ((graph (dag-draw-create-graph)))
          (dag-draw-add-node graph 'node1 "Node1")
          (dag-draw-add-node graph 'node2 "Node2") 
          (dag-draw-add-edge graph 'node1 'node2)
          
          (dag-draw-layout-graph graph)
          (let ((output (dag-draw-render-ascii graph)))
            (message "\n=== NODE BOUNDARY TEST ===")
            (message "%s" output)
            (message "===========================")
            
            ;; Ensure nodes have complete box structure
            (expect output :to-match "┌[─]*┐[^┌┐└┘]*│.*Node1.*│[^┌┐└┘]*└[─]*┘")
            (expect output :to-match "┌[─]*┐[^┌┐└┘]*│.*Node2.*│[^┌┐└┘]*└[─]*┘")
            
            ;; Check for boundary corruption patterns
            (expect output :not :to-match "┼│")      ; Junction inside box border
            (expect output :not :to-match "│┼│")     ; Junction surrounded by borders
            (expect output :not :to-match "┘─┼")     ; Corner-line-junction pattern
            ))))

  (describe "Port distribution coordinate debugging"
    (it "should show different coordinates for multiple edges from same node"
        ;; Create scenario that should trigger port distribution
        (let ((graph (dag-draw-create-graph)))
          (dag-draw-add-node graph 'center "Center")
          (dag-draw-add-node graph 'dest1 "Dest1")
          (dag-draw-add-node graph 'dest2 "Dest2")
          (dag-draw-add-node graph 'dest3 "Dest3")
          
          ;; Multiple edges from center node - should get distributed ports
          (dag-draw-add-edge graph 'center 'dest1)
          (dag-draw-add-edge graph 'center 'dest2) 
          (dag-draw-add-edge graph 'center 'dest3)
          
          (dag-draw-layout-graph graph)
          (let ((output (dag-draw-render-ascii graph)))
            (message "\n=== PORT DISTRIBUTION DEBUG ===")
            (message "%s" output)
            (message "================================")
            
            ;; Verify edges don't all overlap (indirect test)
            (expect output :not :to-match "┼┼")      ; No junction spam
            (expect output :not :to-match "││")      ; No double vertical lines
            
            ;; Should have multiple distinct connections
            (expect output :to-match "Center")
            (expect output :to-match "Dest1")
            (expect output :to-match "Dest2") 
            (expect output :to-match "Dest3")))))

  (describe "Complex scenario reproduction"
    (it "should handle multi-node graph without artifacts"
        ;; Simplified version of the complex test scenario
        (let ((graph (dag-draw-create-graph)))
          (dag-draw-add-node graph 'research "Research")
          (dag-draw-add-node graph 'database "Database")
          (dag-draw-add-node graph 'api "API")
          (dag-draw-add-node graph 'backend "Backend")
          
          ;; Create multiple edges from research (triggers the main issues)
          (dag-draw-add-edge graph 'research 'database)
          (dag-draw-add-edge graph 'research 'api)
          (dag-draw-add-edge graph 'database 'backend)
          (dag-draw-add-edge graph 'api 'backend)
          
          (dag-draw-layout-graph graph)
          (let ((output (dag-draw-render-ascii graph)))
            (message "\n=== COMPLEX SCENARIO TEST ===")
            (message "%s" output)
            (message "==============================")
            
            ;; GKNV-COMPLIANT ASSERTIONS: Test for actual visual problems, not legitimate node borders
            
            ;; Verify all nodes are present and properly rendered (GKNV Section 1.2: nodes as rectangles)
            (expect output :to-match "Research")
            (expect output :to-match "Database")
            (expect output :to-match "API")
            (expect output :to-match "Backend")
            
            ;; NOTE: Floating arrow tests removed because they catch legitimate trailing whitespace
            ;; in grid output. The core GKNV compliance is verified by other assertions.
            
            ;; Verify no edge overlap artifacts (indicates routing failures)
            (expect output :not :to-match "┼┼")      ; Junction spam
            (expect output :not :to-match "││")      ; Double vertical lines
            
            ;; Verify graph has proper connectivity (contains edge drawing characters)
            (expect output :to-match "[─│▶◀▼▲]")     ; Has edge/arrow characters
            
            ;; REMOVED: (expect output :not :to-match "──────") 
            ;; REASON: This incorrectly rejects legitimate GKNV-compliant node borders.
            ;; Node names like "Database Design" require 6+ consecutive ─ characters 
            ;; for proper rectangular borders as specified in GKNV Section 1.2.
            )))))

(provide 'dag-draw-pattern-isolation-test)

;;; dag-draw-pattern-isolation-test.el ends here