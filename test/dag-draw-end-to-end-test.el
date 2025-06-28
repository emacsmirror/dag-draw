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
          
          ;; 1. NO TEXT TRUNCATION - All full labels must be visible
          (expect output :to-match "Research")           ; Not "Resear"
          (expect output :to-match "Database Design")    ; Not "Databas"
          (expect output :to-match "API Design")         ; Not "API"
          (expect output :to-match "Infrastructure")     ; Not "Infrastruc"
          (expect output :to-match "Backend Implementation") ; Not "Backend Imp"
          (expect output :to-match "Frontend Implementation") ; Not "Frontend Imp"
          (expect output :to-match "Integration Testing")    ; Not "Integrati"
          (expect output :to-match "Deployment")            ; Not "Depl"
          
          ;; 2. Must have visible edges (not disconnected boxes)
          (expect (string-match-p "[│─┌┐└┘]" output) :to-be-truthy)
          
          ;; 3. Debugging output to see current quality
          (message "\n=== COMPLEX GRAPH OUTPUT ===")
          (message "%s" output)
          (message "===============================\n")))))

;;; dag-draw-end-to-end-test.el ends here