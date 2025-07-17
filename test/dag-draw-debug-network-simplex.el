;;; dag-draw-debug-network-simplex.el --- Debug network simplex behavior -*- lexical-binding: t -*-

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-core)
(require 'dag-draw-pass1-ranking)

(describe "Debug Network Simplex"
  
  (it "should show what our network simplex actually does vs topological"
    (let ((graph (dag-draw-create-graph)))
      (dag-draw-add-node graph 'a "A")
      (dag-draw-add-node graph 'b "B") 
      (dag-draw-add-node graph 'c "C")
      (dag-draw-add-node graph 'd "D")
      
      (dag-draw-add-edge graph 'a 'b 10)
      (dag-draw-add-edge graph 'b 'd 10)
      (dag-draw-add-edge graph 'a 'c 1)
      (dag-draw-add-edge graph 'c 'd 1)
      
      ;; Test network simplex
      (dag-draw--assign-ranks-network-simplex graph)
      (message "SIMPLEX RANKS:")
      (ht-each (lambda (node-id node)
                 (message "  %s: rank %s" node-id (dag-draw-node-rank node)))
               (dag-draw-graph-nodes graph))
      
      ;; Test topological
      (let ((topo-graph (dag-draw-copy-graph graph)))
        (dag-draw--assign-ranks-topological topo-graph)
        (message "TOPOLOGICAL RANKS:")
        (ht-each (lambda (node-id node)
                   (message "  %s: rank %s" node-id (dag-draw-node-rank node)))
                 (dag-draw-graph-nodes topo-graph)))
      
      ;; Always pass - this is just for debugging
      (expect t :to-be t))))

(provide 'dag-draw-debug-network-simplex)
;;; dag-draw-debug-network-simplex.el ends here