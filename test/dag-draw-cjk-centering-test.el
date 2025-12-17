;;; dag-draw-cjk-centering-test.el --- Tests for CJK text centering -*- lexical-binding: t -*-

;;; Commentary:
;; Tests for proper centering of CJK text in ASCII boxes.

;;; Code:

(require 'buttercup)
(require 'dag-draw)

(describe "CJK text centering in grid"

  (it "should produce same display width for content lines with equivalent text"
    ;; This tests that the CONTENT line (with text) has the same display width
    ;; "ABCD" and "阅读" both display as 4 columns, so the full line should match
    (let ((graph1 (dag-draw-create-graph))
          (graph2 (dag-draw-create-graph)))
      (dag-draw-add-node graph1 'en "ABCD")
      (dag-draw-add-node graph2 'cn "阅读")
      (dag-draw-layout-graph graph1)
      (dag-draw-layout-graph graph2)
      (let* ((ascii1 (dag-draw-render-graph graph1 'ascii))
             (ascii2 (dag-draw-render-graph graph2 'ascii))
             ;; Find the content line (contains │ text │)
             (content1 (seq-find (lambda (l) (string-match "│.*ABCD.*│" l))
                                 (split-string ascii1 "\n")))
             (content2 (seq-find (lambda (l) (string-match "│.*阅读.*│" l))
                                 (split-string ascii2 "\n"))))
        (when (and content1 content2)
          ;; Extract just the box content (from first │ to last │)
          (string-match "│[^│]*│" content1)
          (let ((line1 (match-string 0 content1)))
            (string-match "│[^│]*│" content2)
            (let ((line2 (match-string 0 content2)))
              ;; Both content lines should have the same DISPLAY width
              (expect (string-width line1) :to-equal (string-width line2)))))))))

(provide 'dag-draw-cjk-centering-test)
;;; dag-draw-cjk-centering-test.el ends here
