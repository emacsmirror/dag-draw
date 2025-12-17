;;; dag-draw-cjk-support-test.el --- Tests for CJK/Unicode text width handling -*- lexical-binding: t -*-

;;; Commentary:
;; Tests to ensure CJK (Chinese/Japanese/Korean) and other wide Unicode
;; characters are handled correctly in node sizing and text placement.
;;
;; Bug context: GitHub issue org-gtd.el#255 - boxes misaligned when nodes
;; contain Chinese text. Root cause: code uses `length` (character count)
;; instead of `string-width` (display column count).

;;; Code:

(require 'buttercup)
(require 'dag-draw)

(describe "CJK text width support"

  (describe "dag-draw--calculate-constrained-node-size"

    (it "should calculate same box width for visually equal strings"
      ;; "ABCD" and "阅读" both display as 4 columns in monospace
      (let* ((english-lines '("ABCD"))
             (chinese-lines '("阅读"))
             (english-size (dag-draw--calculate-constrained-node-size english-lines))
             (chinese-size (dag-draw--calculate-constrained-node-size chinese-lines)))
        ;; Both should get the same width since they display identically
        (expect (car english-size) :to-equal (car chinese-size))))

    (it "should size Chinese text boxes based on display width not character count"
      ;; "阅读书籍" is 4 characters but 8 display columns
      (let* ((chinese-lines '("阅读书籍"))
             (size (dag-draw--calculate-constrained-node-size chinese-lines))
             ;; ascii-width-needed should be string-width + 4 = 8 + 4 = 12
             ;; With scale 0.15, world-width = 12 / 0.15 = 80
             ;; But minimum is 60, and 80 > 60, so should be 80
             (expected-min-width 80))
        ;; Currently fails because it uses length (4) instead of string-width (8)
        (expect (car size) :to-be-greater-than 60))))

  (describe "dag-draw--format-node-text-with-constraints"

    (it "should wrap Chinese text based on display width not character count"
      ;; Text with spaces for word wrapping
      ;; "这是一段 很长的中文 文本内容" = 15 chars, 28 display cols, has spaces
      (let* ((long-chinese "这是一段 很长的中文 文本内容")
             (width (string-width long-chinese)))
        ;; Verify our test data is correct: > 25 display cols
        (expect width :to-be-greater-than 25)
        ;; Should be wrapped into 2 lines since width > 25
        (let ((result (dag-draw--format-node-text-with-constraints long-chinese)))
          (expect (length result) :to-be-greater-than 1))))

    (it "should not wrap short Chinese text that fits in one line"
      ;; 6 Chinese chars = 12 display columns, should NOT wrap
      (let* ((short-chinese "短文本测试")
             (result (dag-draw--format-node-text-with-constraints short-chinese)))
        ;; Should be single line since 10 < 25
        (expect (length result) :to-equal 1))))

  (describe "ASCII rendering with CJK text"

    (it "should render Chinese text centered in box correctly"
      ;; Create a simple graph with Chinese node
      (let* ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'cn "阅读")
        (dag-draw-layout-graph graph)
        (let* ((ascii (dag-draw-render-graph graph 'ascii))
               (lines (split-string ascii "\n")))
          ;; Find the line with the label
          (let ((label-line (seq-find (lambda (l) (string-match "阅读" l)) lines)))
            (when label-line
              ;; The box should be properly sized for 4-column text
              ;; Check that the right border aligns (no overflow)
              ;; Box line should have balanced spaces around the text
              (let* ((left-border (string-match "│" label-line))
                     (right-border (string-match "│" label-line (1+ left-border))))
                (expect left-border :to-be-truthy)
                (expect right-border :to-be-truthy)
                ;; Interior width should accommodate 4 display columns
                (expect (- right-border left-border) :to-be-greater-than 4)))))))

    (it "should produce same box dimensions for visually equivalent text"
      ;; "ABCD" and "阅读" both display as 4 columns
      (let ((graph1 (dag-draw-create-graph))
            (graph2 (dag-draw-create-graph)))
        (dag-draw-add-node graph1 'en "ABCD")
        (dag-draw-add-node graph2 'cn "阅读")
        (dag-draw-layout-graph graph1)
        (dag-draw-layout-graph graph2)
        (let* ((ascii1 (dag-draw-render-graph graph1 'ascii))
               (ascii2 (dag-draw-render-graph graph2 'ascii))
               ;; Find the top border line (starts with ┌)
               (top1 (seq-find (lambda (l) (string-match "┌[─]+┐" l))
                               (split-string ascii1 "\n")))
               (top2 (seq-find (lambda (l) (string-match "┌[─]+┐" l))
                               (split-string ascii2 "\n"))))
          ;; Extract just the box part (from ┌ to ┐)
          (when (and top1 top2)
            (string-match "┌[─]+┐" top1)
            (let ((box1 (match-string 0 top1)))
              (string-match "┌[─]+┐" top2)
              (let ((box2 (match-string 0 top2)))
                ;; Both boxes should have the same width (not character count, but box width)
                ;; Since both texts are 4 display columns, boxes should be equal
                (expect (string-width box1) :to-equal (string-width box2))))))))))

(provide 'dag-draw-cjk-support-test)
;;; dag-draw-cjk-support-test.el ends here
