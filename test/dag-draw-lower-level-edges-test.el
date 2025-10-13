;;; dag-draw-lower-level-edges-test.el --- Tests for edges reaching lower graph levels -*- lexical-binding: t -*-

;; Tests to verify that edges successfully reach Backend, Frontend, Integration, and Deployment nodes

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-core)
(require 'dag-draw-render)
(require 'dag-draw-test-harness)

(describe "Lower Level Edge Visibility Tests"
  (describe "Backend Implementation Edge Visibility"
    (it "should show visible edge from Database Design to Backend Implementation"
      (let ((graph (dag-draw-create-graph)))
        ;; Create minimal graph to isolate the Backend connection issue
        (dag-draw-add-node graph 'database "Database Design")
        (dag-draw-add-node graph 'backend "Backend Implementation")
        (dag-draw-add-edge graph 'database 'backend)
        (dag-draw-layout-graph graph :coordinate-mode 'ascii)
        
        (let ((output (dag-draw-render-ascii graph)))
          ;; Use test harness for comprehensive validation
          (let ((node-validation (dag-draw-test--validate-node-completeness output graph)))
            (expect (plist-get node-validation :complete) :to-be t))
          (let ((connectivity-validation (dag-draw-test--validate-edge-connectivity output graph)))
            (expect (plist-get connectivity-validation :all-connected) :to-be t)))))

    (it "should show visible edge from API Design to Backend Implementation"
      (let ((graph (dag-draw-create-graph)))
        ;; Create 3-node chain to test API→Backend connection
        (dag-draw-add-node graph 'api "API Design")
        (dag-draw-add-node graph 'backend "Backend Implementation")
        (dag-draw-add-edge graph 'api 'backend)
        (dag-draw-layout-graph graph :coordinate-mode 'ascii)
        
        (let ((output (dag-draw-render-ascii graph)))
          ;; Use test harness for comprehensive validation
          (let ((node-validation (dag-draw-test--validate-node-completeness output graph)))
            (expect (plist-get node-validation :complete) :to-be t))
          (let ((connectivity-validation (dag-draw-test--validate-edge-connectivity output graph)))
            (expect (plist-get connectivity-validation :all-connected) :to-be t))))))

  (describe "Frontend Implementation Edge Visibility" 
    (it "should show visible edge from API Design to Frontend Implementation"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'api "API Design")
        (dag-draw-add-node graph 'frontend "Frontend Implementation")
        (dag-draw-add-edge graph 'api 'frontend)
        (dag-draw-layout-graph graph :coordinate-mode 'ascii)
        
        (let ((output (dag-draw-render-ascii graph)))
          ;; Use test harness for comprehensive validation
          (let ((node-validation (dag-draw-test--validate-node-completeness output graph)))
            (expect (plist-get node-validation :complete) :to-be t))
          (let ((connectivity-validation (dag-draw-test--validate-edge-connectivity output graph)))
            (expect (plist-get connectivity-validation :all-connected) :to-be t))))))

  (describe "Integration Testing Edge Visibility"
    (it "should show visible edge from Backend to Integration Testing"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'backend "Backend Implementation")
        (dag-draw-add-node graph 'integration "Integration Testing")
        (dag-draw-add-edge graph 'backend 'integration)
        (dag-draw-layout-graph graph :coordinate-mode 'ascii)
        
        (let ((output (dag-draw-render-ascii graph)))
          ;; Use test harness for comprehensive validation
          (let ((node-validation (dag-draw-test--validate-node-completeness output graph)))
            (expect (plist-get node-validation :complete) :to-be t))
          (let ((connectivity-validation (dag-draw-test--validate-edge-connectivity output graph)))
            (expect (plist-get connectivity-validation :all-connected) :to-be t)))))

    (it "should show visible edge from Frontend to Integration Testing"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'frontend "Frontend Implementation")
        (dag-draw-add-node graph 'integration "Integration Testing") 
        (dag-draw-add-edge graph 'frontend 'integration)
        (dag-draw-layout-graph graph :coordinate-mode 'ascii)
        
        (let ((output (dag-draw-render-ascii graph)))
          ;; Use test harness for comprehensive validation
          (let ((node-validation (dag-draw-test--validate-node-completeness output graph)))
            (expect (plist-get node-validation :complete) :to-be t))
          (let ((connectivity-validation (dag-draw-test--validate-edge-connectivity output graph)))
            (expect (plist-get connectivity-validation :all-connected) :to-be t))))))

  (describe "Deployment Edge Visibility"
    (it "should show visible edge from Integration Testing to Deployment"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'integration "Integration Testing")
        (dag-draw-add-node graph 'deployment "Deployment")
        (dag-draw-add-edge graph 'integration 'deployment)
        (dag-draw-layout-graph graph :coordinate-mode 'ascii)
        
        (let ((output (dag-draw-render-ascii graph)))
          ;; Use test harness for comprehensive validation
          (let ((node-validation (dag-draw-test--validate-node-completeness output graph)))
            (expect (plist-get node-validation :complete) :to-be t))
          (let ((connectivity-validation (dag-draw-test--validate-edge-connectivity output graph)))
            (expect (plist-get connectivity-validation :all-connected) :to-be t))))))

  (describe "Complex Multi-Level Edge Visibility"
    (it "should show all edges in the full dependency graph"
      (let ((graph (dag-draw-create-graph)))
        ;; Recreate the exact failing scenario
        (dag-draw-add-node graph 'research "Research")
        (dag-draw-add-node graph 'database "Database Design")
        (dag-draw-add-node graph 'api "API Design")
        (dag-draw-add-node graph 'infrastructure "Infrastructure Setup")
        (dag-draw-add-node graph 'backend "Backend Implementation")
        (dag-draw-add-node graph 'frontend "Frontend Implementation")
        (dag-draw-add-node graph 'integration "Integration Testing")
        (dag-draw-add-node graph 'deployment "Deployment")
        
        (dag-draw-add-edge graph 'research 'database)
        (dag-draw-add-edge graph 'research 'api)
        (dag-draw-add-edge graph 'research 'infrastructure)
        (dag-draw-add-edge graph 'database 'backend)
        (dag-draw-add-edge graph 'api 'backend)
        (dag-draw-add-edge graph 'api 'frontend)
        (dag-draw-add-edge graph 'backend 'integration)
        (dag-draw-add-edge graph 'frontend 'integration)
        (dag-draw-add-edge graph 'integration 'deployment)
        
        (dag-draw-layout-graph graph :coordinate-mode 'ascii)
        (let ((output (dag-draw-render-ascii graph)))
          ;; Use test harness for comprehensive validation of complex graph
          (let ((node-validation (dag-draw-test--validate-node-completeness output graph)))
            (expect (plist-get node-validation :complete) :to-be t))
          (let ((structure-validation (dag-draw-test--validate-graph-structure output graph)))
            (expect (plist-get structure-validation :topology-match) :to-be t)
            (expect (plist-get structure-validation :node-count-match) :to-be t)
            (expect (plist-get structure-validation :edge-count-match) :to-be t))
          (let ((connectivity-validation (dag-draw-test--validate-edge-connectivity output graph)))
            (expect (plist-get connectivity-validation :all-connected) :to-be t))
          (let ((arrow-validation (dag-draw-test--validate-arrows output)))
            (expect (plist-get arrow-validation :valid-arrows) :to-be-greater-than 0))))))

  (describe "Coordinate Range Validation"
    (it "should place all nodes within ASCII grid bounds"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'backend "Backend Implementation")
        (dag-draw-add-node graph 'integration "Integration Testing")
        (dag-draw-add-node graph 'deployment "Deployment")
        (dag-draw-add-edge graph 'backend 'integration)
        (dag-draw-add-edge graph 'integration 'deployment)
        (dag-draw-layout-graph graph :coordinate-mode 'ascii)
        
        ;; Get the actual grid bounds and node coordinates
        (let* ((bounds (dag-draw-get-graph-bounds graph))
               (backend-node (ht-get (dag-draw-graph-nodes graph) 'backend))
               (integration-node (ht-get (dag-draw-graph-nodes graph) 'integration))
               (deployment-node (ht-get (dag-draw-graph-nodes graph) 'deployment))
               (output (dag-draw-render-ascii graph))
               (output-lines (split-string output "\n")))
          
          ;; TEST: Grid should be large enough to contain all coordinates
          (expect (length output-lines) :to-be-greater-than 10)
          
          ;; TEST: Nodes should have reasonable Y-coordinate separation
          (expect (dag-draw-node-y-coord backend-node) :not :to-equal 
                  (dag-draw-node-y-coord integration-node))
          (expect (dag-draw-node-y-coord integration-node) :not :to-equal
                  (dag-draw-node-y-coord deployment-node)))))))

;;; dag-draw-lower-level-edges-test.el ends here