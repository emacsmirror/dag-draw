;;; dag-draw-org-validation.el --- Validation utilities for org-mode DAG structures -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; This module provides validation utilities for org-mode files used with
;; dag-draw. It helps users verify that their org-mode task dependencies
;; form valid DAG structures and provides debugging information.

;;; Code:

(require 'org)
(require 'dag-draw)
(require 'dag-draw-core)

;;; Org parsing utilities

(defun dag-draw-org--parse-tasks-from-file (org-file)
  "Parse ORG-FILE and extract tasks with ID and BLOCKED_BY properties."
  (let ((tasks '()))
    (with-temp-buffer
      (insert-file-contents org-file)
      (org-mode)
      
      (org-map-entries
       (lambda ()
         (let* ((heading (org-get-heading t t t t))
                (id (org-entry-get nil "ID"))
                (blocked-by-str (org-entry-get nil "BLOCKED_BY"))
                (blocked-by (when blocked-by-str
                              (split-string blocked-by-str))))
           (when id
             (push (list :id (intern id)
                         :title heading
                         :blocked-by (mapcar #'intern blocked-by))
                   tasks))))
       nil nil)
      
      (nreverse tasks))))

(defun dag-draw-org--create-graph-from-tasks (tasks)
  "Create a dag-draw graph from TASKS list."
  (let ((graph (dag-draw-create-graph)))
    
    ;; Add nodes
    (dolist (task tasks)
      (let ((id (plist-get task :id))
            (title (plist-get task :title)))
        (dag-draw-add-node graph id title)))
    
    ;; Add edges
    (dolist (task tasks)
      (let ((id (plist-get task :id))
            (blocked-by (plist-get task :blocked-by)))
        (dolist (blocker blocked-by)
          (dag-draw-add-edge graph blocker id))))
    
    graph))

;;; Validation functions

(defun dag-draw-org-validate-structure (org-file)
  "Validate the DAG structure of ORG-FILE and return validation report.
Returns a plist with validation results and any issues found."
  (interactive "fOrg file to validate: ")
  
  (let* ((tasks (dag-draw-org--parse-tasks-from-file org-file))
         (graph (dag-draw-org--create-graph-from-tasks tasks))
         (validation-report (list :valid t :errors '() :warnings '() :info '())))
    
    ;; Basic parsing validation
    (if (zerop (length tasks))
        (progn
          (setf (plist-get validation-report :valid) nil)
          (push "No tasks with ID properties found in file" 
                (plist-get validation-report :errors)))
      
      ;; Add basic info
      (push (format "Parsed %d tasks successfully" (length tasks))
            (plist-get validation-report :info))
      (push (format "Created graph with %d edges" (length (dag-draw-graph-edges graph)))
            (plist-get validation-report :info)))
    
    ;; Check for missing dependencies
    (dolist (task tasks)
      (let ((blocked-by (plist-get task :blocked-by)))
        (dolist (blocker blocked-by)
          (unless (cl-find-if (lambda (t) (eq (plist-get t :id) blocker)) tasks)
            (push (format "Task '%s' references missing dependency '%s'" 
                          (plist-get task :id) blocker)
                  (plist-get validation-report :warnings))))))
    
    ;; Check for self-dependencies
    (dolist (task tasks)
      (let ((id (plist-get task :id))
            (blocked-by (plist-get task :blocked-by)))
        (when (member id blocked-by)
          (setf (plist-get validation-report :valid) nil)
          (push (format "Task '%s' depends on itself (self-cycle)" id)
                (plist-get validation-report :errors)))))
    
    ;; Basic cycle detection (simplified)
    (let ((potential-cycles (dag-draw-org--detect-simple-cycles tasks)))
      (when potential-cycles
        (setf (plist-get validation-report :valid) nil)
        (dolist (cycle potential-cycles)
          (push (format "Potential cycle detected: %s" cycle)
                (plist-get validation-report :errors)))))
    
    ;; Try to validate the graph structure
    (condition-case err
        (dag-draw-validate-graph graph)
      (error
       (setf (plist-get validation-report :valid) nil)
       (push (format "Graph validation failed: %s" (error-message-string err))
             (plist-get validation-report :errors))))
    
    ;; Display results if interactive
    (when (called-interactively-p 'interactive)
      (dag-draw-org--display-validation-report org-file validation-report))
    
    validation-report))

(defun dag-draw-org--detect-simple-cycles (tasks)
  "Simple cycle detection for TASKS. Returns list of potential cycles."
  (let ((cycles '())
        (task-deps (make-hash-table :test 'eq)))
    
    ;; Build dependency hash
    (dolist (task tasks)
      (let ((id (plist-get task :id))
            (blocked-by (plist-get task :blocked-by)))
        (puthash id blocked-by task-deps)))
    
    ;; Check for direct cycles (A->B->A)
    (dolist (task tasks)
      (let ((id (plist-get task :id)))
        (dag-draw-org--check-cycle-from id id task-deps '() cycles)))
    
    cycles))

(defun dag-draw-org--check-cycle-from (start current deps-hash path cycles &optional max-depth)
  "Check for cycles starting from START, currently at CURRENT.
PATH tracks visited nodes. MAX-DEPTH limits recursion (default 10)."
  (let ((max-depth (or max-depth 10)))
    (when (and (> max-depth 0) (not (member current path)))
      (let ((new-path (cons current path))
            (dependencies (gethash current deps-hash)))
        (dolist (dep dependencies)
          (cond
           ((eq dep start)
            ;; Found cycle back to start
            (push (reverse (cons dep new-path)) cycles))
           ((not (member dep path))
            ;; Continue search
            (dag-draw-org--check-cycle-from start dep deps-hash new-path cycles (1- max-depth)))))))))

(defun dag-draw-org--display-validation-report (org-file report)
  "Display validation REPORT for ORG-FILE in a buffer."
  (let ((buffer (get-buffer-create "*DAG Validation Report*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert (format "DAG Validation Report for: %s\n" org-file))
      (insert (make-string 60 ?=))
      (insert "\n\n")
      
      ;; Status
      (if (plist-get report :valid)
          (insert "✅ VALIDATION PASSED\n\n")
        (insert "❌ VALIDATION FAILED\n\n"))
      
      ;; Info
      (let ((info-list (plist-get report :info)))
        (when info-list
          (insert "📋 Information:\n")
          (dolist (info (reverse info-list))
            (insert (format "   • %s\n" info)))
          (insert "\n")))
      
      ;; Warnings
      (let ((warnings (plist-get report :warnings)))
        (when warnings
          (insert "⚠️  Warnings:\n")
          (dolist (warning (reverse warnings))
            (insert (format "   • %s\n" warning)))
          (insert "\n")))
      
      ;; Errors
      (let ((errors (plist-get report :errors)))
        (when errors
          (insert "❌ Errors:\n")
          (dolist (error (reverse errors))
            (insert (format "   • %s\n" error)))
          (insert "\n")))
      
      ;; Recommendations
      (insert "💡 Recommendations:\n")
      (insert "   • Ensure all tasks have unique ID properties\n")
      (insert "   • Verify BLOCKED_BY references point to existing task IDs\n")
      (insert "   • Check for circular dependencies\n")
      (insert "   • Use dag-draw-org-debug-structure for detailed analysis\n")
      
      (goto-char (point-min))
      (text-mode))
    
    (display-buffer buffer)
    buffer))

;;; Interactive debugging functions

(defun dag-draw-org-debug-structure (org-file)
  "Provide detailed debugging information for ORG-FILE DAG structure."
  (interactive "fOrg file to debug: ")
  
  (let* ((tasks (dag-draw-org--parse-tasks-from-file org-file))
         (graph (dag-draw-org--create-graph-from-tasks tasks))
         (buffer (get-buffer-create "*DAG Structure Debug*")))
    
    (with-current-buffer buffer
      (erase-buffer)
      (insert (format "DAG Structure Debug for: %s\n" org-file))
      (insert (make-string 60 ?=))
      (insert "\n\n")
      
      ;; Tasks summary
      (insert (format "📋 Tasks (%d total):\n" (length tasks)))
      (insert (make-string 30 ?-))
      (insert "\n")
      (dolist (task tasks)
        (let ((id (plist-get task :id))
              (title (plist-get task :title))
              (blocked-by (plist-get task :blocked-by)))
          (insert (format "Task %s: \"%s\"\n" id title))
          (if blocked-by
              (insert (format "   Blocked by: %s\n" 
                             (mapconcat #'symbol-name blocked-by ", ")))
            (insert "   Blocked by: (none - root task)\n"))
          (insert "\n")))
      
      ;; Dependency graph
      (insert "\n🔗 Dependency Graph:\n")
      (insert (make-string 30 ?-))
      (insert "\n")
      (dolist (edge (dag-draw-graph-edges graph))
        (insert (format "%s → %s\n" 
                        (dag-draw-edge-from-node edge)
                        (dag-draw-edge-to-node edge))))
      
      (insert (format "\nTotal edges: %d\n" (length (dag-draw-graph-edges graph))))
      
      ;; Root and leaf tasks
      (let ((root-tasks '())
            (leaf-tasks '())
            (task-ids (mapcar (lambda (task) (plist-get task :id)) tasks)))
        
        (dolist (task tasks)
          (let ((id (plist-get task :id))
                (blocked-by (plist-get task :blocked-by)))
            (when (null blocked-by)
              (push id root-tasks))))
        
        (dolist (task-id task-ids)
          (let ((blocks-others (cl-some (lambda (edge)
                                         (eq (dag-draw-edge-from-node edge) task-id))
                                       (dag-draw-graph-edges graph))))
            (unless blocks-others
              (push task-id leaf-tasks))))
        
        (insert (format "\n🌱 Root tasks (no dependencies): %s\n"
                        (if root-tasks
                            (mapconcat #'symbol-name root-tasks ", ")
                          "(none)")))
        (insert (format "🍃 Leaf tasks (don't block others): %s\n"
                        (if leaf-tasks
                            (mapconcat #'symbol-name leaf-tasks ", ")
                          "(none)"))))
      
      (goto-char (point-min))
      (text-mode))
    
    (display-buffer buffer)
    buffer))

(defun dag-draw-org-check-file (org-file)
  "Quick check if ORG-FILE is suitable for DAG visualization.
Returns t if valid, nil if issues found."
  (interactive "fOrg file to check: ")
  
  (let ((report (dag-draw-org-validate-structure org-file)))
    (when (called-interactively-p 'interactive)
      (if (plist-get report :valid)
          (message "✅ %s is valid for DAG visualization" org-file)
        (message "❌ %s has validation issues - check *DAG Validation Report*" org-file)))
    
    (plist-get report :valid)))

(provide 'dag-draw-org-validation)

;;; dag-draw-org-validation.el ends here