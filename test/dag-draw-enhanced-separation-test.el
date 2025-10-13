;;; dag-draw-enhanced-separation-test.el --- Test enhanced separation algorithm -*- lexical-binding: t -*-

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-core)
(require 'dag-draw-render)
(require 'dag-draw-test-harness)
(require 'org)

(defun test-create-parallel-path-graph ()
  "Create a test graph with parallel paths that could cause visual confusion."
  (let ((graph (dag-draw-create-graph)))

    ;; Add nodes
    (dag-draw-add-node graph 'research "Research")
    (dag-draw-add-node graph 'db-design "Database Design")
    (dag-draw-add-node graph 'api-design "API Design")
    (dag-draw-add-node graph 'backend "Backend")

    ;; Add edges that create parallel paths
    (dag-draw-add-edge graph 'research 'db-design)
    (dag-draw-add-edge graph 'research 'api-design)
    (dag-draw-add-edge graph 'db-design 'backend)
    (dag-draw-add-edge graph 'api-design 'backend)

    graph))

(describe
 "Enhanced Separation Algorithm"
 (it "should provide adequate separation for parallel dependency paths"
     (let* ((graph (test-create-parallel-path-graph))
            ;; Configure for compact layout
            (old-node-sep (dag-draw-graph-node-separation graph))
            (old-rank-sep (dag-draw-graph-rank-separation graph)))

       ;; Set up enhanced separation settings
       (setf (dag-draw-graph-node-separation graph) 40)
       (setf (dag-draw-graph-rank-separation graph) 35)

       ;; Layout the graph
       (dag-draw-layout-graph graph)

       ;; Check that parallel nodes have adequate separation
       (let* ((db-node (dag-draw-get-node graph 'db-design))
              (api-node (dag-draw-get-node graph 'api-design))
              (db-x (dag-draw-node-x-coord db-node))
              (api-x (dag-draw-node-x-coord api-node))
              (separation (abs (- db-x api-x))))

         ;; With enhanced separation, parallel nodes should be spaced further apart
         (expect separation :to-be-greater-than 80)

         ;; Both should be at the same rank (y-coordinate)
         (expect (dag-draw-node-y-coord db-node)
                 :to-equal (dag-draw-node-y-coord api-node))))))

(describe
 "Visual Confusion Prevention"
 (it "should generate ASCII output that clearly shows parallel paths"
     (let* ((graph (test-create-parallel-path-graph)))

       ;; Configure for enhanced separation
       (setf (dag-draw-graph-node-separation graph) 50)
       (setf (dag-draw-graph-rank-separation graph) 40)

       ;; Layout and render
       (dag-draw-layout-graph graph)
       (let ((ascii-output (dag-draw-render-ascii graph)))

         ;; Validate basic requirements: nodes exist and are positioned with adequate separation
         (let ((node-validation (dag-draw-test--validate-node-completeness ascii-output graph)))
           (expect (plist-get node-validation :complete) :to-be t))
         
         ;; Visual quality check: verify parallel nodes have adequate separation (core business requirement)
         (let* ((db-node (dag-draw-get-node graph 'db-design))
                (api-node (dag-draw-get-node graph 'api-design))
                (db-x (dag-draw-node-x-coord db-node))
                (api-x (dag-draw-node-x-coord api-node))
                (separation (abs (- db-x api-x))))
           ;; Enhanced separation should create visual clarity between parallel paths
           (expect separation :to-be-greater-than 80))

         ;; Should have proper vertical structure (multiple lines)
         (expect (length (split-string ascii-output "\n")) :to-be-greater-than 10)

         ;; Print output for manual inspection
         (message "\n=== ENHANCED SEPARATION ASCII OUTPUT ===")
         (message "%s" ascii-output)
         (message "==========================================")))))

;; Also add a function to test with the actual tasks.org file if it exists
(describe
 "Real Task Dependencies"
 (it "should handle tasks.org with enhanced separation"
     (let ((tasks-file "sandbox/tasks.org"))
       (when (file-exists-p tasks-file)
         (let ((tasks '())
               (graph (dag-draw-create-graph)))

           ;; Parse tasks.org
           (with-temp-buffer
             (insert-file-contents tasks-file)
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
              nil nil))

           ;; Create graph
           (dolist (task tasks)
             (let ((id (plist-get task :id))
                   (title (plist-get task :title)))
               (dag-draw-add-node graph id title)))

           (dolist (task tasks)
             (let ((id (plist-get task :id))
                   (blocked-by (plist-get task :blocked-by)))
               (dolist (blocker blocked-by)
                 (dag-draw-add-edge graph blocker id))))

           ;; Configure enhanced separation
           (setf (dag-draw-graph-node-separation graph) 50)
           (setf (dag-draw-graph-rank-separation graph) 40)

           ;; Layout and test
           (dag-draw-layout-graph graph :coordinate-mode 'ascii)

           (let* ((db-node (dag-draw-get-node graph 'db-design))
                  (api-node (dag-draw-get-node graph 'api-design)))

             (when (and db-node api-node)
               (let ((separation (abs (- (dag-draw-node-x-coord db-node)
                                         (dag-draw-node-x-coord api-node)))))

                 (message "\n=== REAL TASKS.ORG WITH ENHANCED SEPARATION ===")
                 (message "Parsed %d tasks successfully" (length tasks))
                 (message "DB Design position: (%.1f, %.1f)"
                          (dag-draw-node-x-coord db-node)
                          (dag-draw-node-y-coord db-node))
                 (message "API Design position: (%.1f, %.1f)"
                          (dag-draw-node-x-coord api-node)
                          (dag-draw-node-y-coord api-node))
                 (message "Separation: %.1f" separation)
                 (message "==============================================")

                 ;; Should have good separation for parallel paths
                 (expect separation :to-be-greater-than 80)

                 ;; Both should be at same rank (same Y coordinate)
                 (expect (dag-draw-node-y-coord db-node)
                         :to-equal (dag-draw-node-y-coord api-node))

                 ;; Verify we have all expected tasks
                 (expect (length tasks) :to-equal 8)))))))))

(provide 'dag-draw-enhanced-separation-test)

;;; dag-draw-enhanced-separation-test.el ends here
