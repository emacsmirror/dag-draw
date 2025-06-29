;;; dag-draw-validation-integration-test.el --- Integration tests for validation utilities -*- lexical-binding: t -*-

(require 'buttercup)
(require 'dag-draw-org-validation)

(describe "Validation Utilities Integration"
  
  (it "should validate tasks.org successfully"
    (let ((tasks-file "sandbox/tasks.org"))
      (when (file-exists-p tasks-file)
        (let ((report (dag-draw-org-validate-structure tasks-file)))
          
          ;; Should be valid
          (expect (plist-get report :valid) :to-be t)
          
          ;; Should have no errors
          (expect (length (plist-get report :errors)) :to-equal 0)
          
          ;; Should have info about parsed tasks
          (expect (length (plist-get report :info)) :to-be-greater-than 0)
          
          ;; Quick check should return true
          (expect (dag-draw-org-check-file tasks-file) :to-be t)))))
  
  (it "should detect missing dependencies"
    (let ((test-content "#+TITLE: Test
* Task A
  :PROPERTIES:
  :ID: task-a
  :BLOCKED_BY: missing-task
  :END:"))
      
      (with-temp-file "/tmp/test-missing-deps.org"
        (insert test-content))
      
      (let ((report (dag-draw-org-validate-structure "/tmp/test-missing-deps.org")))
        
        ;; Should have warnings about missing dependencies
        (expect (length (plist-get report :warnings)) :to-be-greater-than 0)
        
        ;; Warning should mention the missing task
        (expect (format "%s" (plist-get report :warnings)) :to-match "missing-task"))
      
      (delete-file "/tmp/test-missing-deps.org")))
  
  (it "should detect self-dependencies"
    (let ((test-content "#+TITLE: Test
* Task A
  :PROPERTIES:
  :ID: task-a
  :BLOCKED_BY: task-a
  :END:"))
      
      (with-temp-file "/tmp/test-self-dep.org"
        (insert test-content))
      
      (let ((report (dag-draw-org-validate-structure "/tmp/test-self-dep.org")))
        
        ;; Should be invalid due to self-dependency
        (expect (plist-get report :valid) :to-be nil)
        
        ;; Should have error about self-cycle
        (expect (length (plist-get report :errors)) :to-be-greater-than 0)
        (expect (format "%s" (plist-get report :errors)) :to-match "self-cycle"))
      
      (delete-file "/tmp/test-self-dep.org")))
  
  (it "should handle empty org files gracefully"
    (let ((test-content "#+TITLE: Empty Test
No tasks here"))
      
      (with-temp-file "/tmp/test-empty.org"
        (insert test-content))
      
      (let ((report (dag-draw-org-validate-structure "/tmp/test-empty.org")))
        
        ;; Should be invalid due to no tasks
        (expect (plist-get report :valid) :to-be nil)
        
        ;; Should have error about no tasks
        (expect (length (plist-get report :errors)) :to-be-greater-than 0)
        (expect (format "%s" (plist-get report :errors)) :to-match "No tasks"))
      
      (delete-file "/tmp/test-empty.org")))
  
  (it "should create debug structure without errors"
    (let ((tasks-file "sandbox/tasks.org"))
      (when (file-exists-p tasks-file)
        ;; Actually call the function
        (let ((result 
               (condition-case err
                   (prog1 'success (dag-draw-org-debug-structure tasks-file))
                 (error (format "error: %s" (error-message-string err))))))
          
          ;; Should not error
          (expect result :to-equal 'success)
          
          ;; Should create the debug buffer
          (expect (get-buffer "*DAG Structure Debug*") :not :to-be nil))))))

(provide 'dag-draw-validation-integration-test)

;;; dag-draw-validation-integration-test.el ends here