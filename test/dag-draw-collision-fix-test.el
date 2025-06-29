;;; dag-draw-collision-fix-test.el --- Test for edge-node collision fixes -*- lexical-binding: t -*-

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-render)

(describe "Edge-Node Collision Prevention"
  
  (describe "Text corruption prevention"
    (it "should not corrupt node text with edge characters"
      (let ((graph (dag-draw-create-graph)))
        ;; Create nodes that are likely to have edge routing through them
        (dag-draw-add-node graph 'task-d "Task D")
        (dag-draw-add-node graph 'task-e "Task E")
        (dag-draw-add-edge graph 'task-d 'task-e)
        
        ;; Run layout to set ranks and positions  
        (dag-draw-layout-graph graph)
        
        ;; Override coordinates to create vertical edge routing
        (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'task-d)) 100)
        (setf (dag-draw-node-y-coord (dag-draw-get-node graph 'task-d)) 50)
        (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'task-e)) 100)
        (setf (dag-draw-node-y-coord (dag-draw-get-node graph 'task-e)) 150)
        
        (let ((ascii-output (dag-draw-render-ascii graph)))
          ;; Debug: print the output to see what we get
          (message "=== COLLISION TEST OUTPUT ===")
          (message "%s" ascii-output)
          (message "=== END OUTPUT ===")
          
          ;; The output should contain the original text
          (expect ascii-output :to-match "Task D")
          (expect ascii-output :to-match "Task E")
          ;; The output should NOT contain corrupted text
          (expect ascii-output :not :to-match "Task─D")
          (expect ascii-output :not :to-match "Task│D")
          (expect ascii-output :not :to-match "Task┼D")
          (expect ascii-output :not :to-match "Task[─│┼]"))))
    
    (it "should handle complex multi-node convergence without corruption"
      ;; This test replicates the failing test pattern more closely
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'task-a "Task A")
        (dag-draw-add-node graph 'task-b "Task B") 
        (dag-draw-add-node graph 'task-c "Task C")
        (dag-draw-add-node graph 'task-d "Task D")
        (dag-draw-add-node graph 'task-e "Task E")
        (dag-draw-add-node graph 'task-f "Task F")
        
        ;; Create convergence pattern like the failing tests
        (dag-draw-add-edge graph 'task-a 'task-b)
        (dag-draw-add-edge graph 'task-a 'task-c)
        (dag-draw-add-edge graph 'task-b 'task-d)
        (dag-draw-add-edge graph 'task-c 'task-d)
        (dag-draw-add-edge graph 'task-d 'task-e)
        (dag-draw-add-edge graph 'task-d 'task-f)
        
        ;; Run layout 
        (dag-draw-layout-graph graph)
        
        (let ((ascii-output (dag-draw-render-ascii graph)))
          ;; Debug: print the output
          (message "=== COMPLEX CONVERGENCE TEST OUTPUT ===")
          (message "%s" ascii-output)
          (message "=== END COMPLEX OUTPUT ===")
          
          ;; All task names should be clean
          (expect ascii-output :to-match "Task A")
          (expect ascii-output :to-match "Task B")
          (expect ascii-output :to-match "Task C")
          (expect ascii-output :to-match "Task D")
          (expect ascii-output :to-match "Task E")
          (expect ascii-output :to-match "Task F")
          
          ;; No task names should be corrupted
          (expect ascii-output :not :to-match "Task [A-F][─│┼]"))))
    
    (it "should connect simple two-node graph with visible edges"
      ;; This replicates the failing boundary connection test
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'source "Source")
        (dag-draw-add-node graph 'target "Target")
        (dag-draw-add-edge graph 'source 'target)
        
        ;; Set coordinates like the failing test
        (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'source)) 100)
        (setf (dag-draw-node-y-coord (dag-draw-get-node graph 'source)) 50)
        (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'target)) 100)
        (setf (dag-draw-node-y-coord (dag-draw-get-node graph 'target)) 150)
        
        (let ((ascii-output (dag-draw-render-ascii graph)))
          ;; Debug: print the output
          (message "=== BOUNDARY CONNECTION TEST OUTPUT ===")
          (message "%s" ascii-output)
          (message "=== END BOUNDARY OUTPUT ===")
          
          ;; Should have both nodes
          (expect ascii-output :to-match "Source")
          (expect ascii-output :to-match "Target")
          ;; Should have vertical connection
          (expect ascii-output :to-match "│"))))
    
    (it "should produce correct arrow character based on layout direction"
      ;; This replicates the failing arrow direction test
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'node-a "Node A")
        (dag-draw-add-node graph 'node-b "Node B")
        (dag-draw-add-edge graph 'node-a 'node-b)
        
        ;; Set coordinates for vertical layout (should produce 'v' arrow)
        (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'node-a)) 100)
        (setf (dag-draw-node-y-coord (dag-draw-get-node graph 'node-a)) 50)
        (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'node-b)) 100)
        (setf (dag-draw-node-y-coord (dag-draw-get-node graph 'node-b)) 150)
        
        (let ((ascii-output (dag-draw-render-ascii graph)))
          ;; Debug: print the output  
          (message "=== ARROW DIRECTION TEST OUTPUT ===")
          (message "%s" ascii-output)
          (message "=== END ARROW OUTPUT ===")
          
          ;; Should have both nodes
          (expect ascii-output :to-match "Node A")
          (expect ascii-output :to-match "Node B")
          ;; For vertical layout, should have downward arrow 'v'
          (expect ascii-output :to-match "v"))))
    
    (it "should preserve box drawing characters in node borders"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'test "Test")
        
        ;; Run layout to set ranks
        (dag-draw-layout-graph graph)
        
        (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'test)) 100)
        (setf (dag-draw-node-y-coord (dag-draw-get-node graph 'test)) 100)
        
        (let ((ascii-output (dag-draw-render-ascii graph)))
          ;; Should have proper box corners
          (expect ascii-output :to-match "┌")
          (expect ascii-output :to-match "└")
          (expect ascii-output :to-match "┐")
          (expect ascii-output :to-match "┘")
          ;; Should have node content
          (expect ascii-output :to-match "Test"))))
    
    (it "should handle complex graph without text corruption"
      (let ((graph (dag-draw-create-graph)))
        ;; Create a diamond pattern that has been problematic
        (dag-draw-add-node graph 'top "Top")
        (dag-draw-add-node graph 'left "Left")  
        (dag-draw-add-node graph 'right "Right")
        (dag-draw-add-node graph 'bottom "Bottom")
        
        (dag-draw-add-edge graph 'top 'left)
        (dag-draw-add-edge graph 'top 'right)
        (dag-draw-add-edge graph 'left 'bottom)
        (dag-draw-add-edge graph 'right 'bottom)
        
        ;; Run layout to set ranks
        (dag-draw-layout-graph graph)
        
        ;; Set diamond coordinates
        (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'top)) 100)
        (setf (dag-draw-node-y-coord (dag-draw-get-node graph 'top)) 50)
        (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'left)) 50)
        (setf (dag-draw-node-y-coord (dag-draw-get-node graph 'left)) 100)
        (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'right)) 150)
        (setf (dag-draw-node-y-coord (dag-draw-get-node graph 'right)) 100)
        (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'bottom)) 100)
        (setf (dag-draw-node-y-coord (dag-draw-get-node graph 'bottom)) 150)
        
        (let ((ascii-output (dag-draw-render-ascii graph)))
          ;; All nodes should have clean text
          (expect ascii-output :to-match "Top")
          (expect ascii-output :to-match "Left")
          (expect ascii-output :to-match "Right") 
          (expect ascii-output :to-match "Bottom")
          ;; No text should be corrupted with edge characters
          (expect ascii-output :not :to-match "Top[─│┼]")
          (expect ascii-output :not :to-match "Left[─│┼]")
          (expect ascii-output :not :to-match "Right[─│┼]")
          (expect ascii-output :not :to-match "Bottom[─│┼]")))))

  (describe "Occupancy map accuracy"
    (it "should create occupancy map that matches node drawing coordinates"
      ;; This test will help debug coordinate mismatches
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'test "Test Node")
        
        ;; Run layout to set ranks
        (dag-draw-layout-graph graph)
        
        (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'test)) 100)
        (setf (dag-draw-node-y-coord (dag-draw-get-node graph 'test)) 100)
        
        ;; The test should pass without throwing errors
        (expect (dag-draw-render-ascii graph) :to-be-truthy)
        ;; Basic check that rendering produces expected content
        (let ((output (dag-draw-render-ascii graph)))
          (expect output :to-match "Test Node")
          (expect output :to-match "┌"))))))

;;; dag-draw-collision-fix-test.el ends here