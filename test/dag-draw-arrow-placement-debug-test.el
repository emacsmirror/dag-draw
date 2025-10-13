;;; dag-draw-arrow-placement-debug-test.el --- Debug arrow placement coordinates -*- lexical-binding: t -*-

;;; Commentary:

;; FUTURE ENHANCEMENT - Development/Debugging Tool
;;
;; Enhancement Category: Debug
;; Baseline Status: 🔧 Development Tool (Not part of baseline algorithm)
;;
;; This test verifies:
;; - Arrow placement coordinate debugging
;; - Edge line endpoint analysis
;; - Arrow-to-edge connectivity verification
;;
;; Related Baseline Decisions: D5.5 (Arrow Placement)
;; Enhancement Source: Development debugging tools
;;
;; This is a development/debugging test, not part of the baseline specification.
;; May be useful for troubleshooting arrow placement during implementation.
;; See doc/test-suite-analysis.md for categorization rationale.
;;
;; [Original commentary: Debug exactly where arrows are being placed...]
;;
;; Debug exactly where arrows are being placed vs where edge lines end

;;; Code:

(require 'buttercup)
(require 'dag-draw)

(describe "Arrow Placement Debugging"

          (describe "Simple two-node connection"
                    (it "should trace edge line endpoints vs arrow positions"
                        (let ((graph (dag-draw-create-graph)))
                          (dag-draw-add-node graph 'source "Source")
                          (dag-draw-add-node graph 'target "Target")
                          (dag-draw-add-edge graph 'source 'target)

                          (dag-draw-layout-graph graph)
                          (let* ((output (dag-draw-render-ascii graph))
                                 (lines (split-string output "\n")))

                            (message "\n=== ARROW PLACEMENT DEBUG ===")
                            (message "%s" output)
                            (message "==============================")

                            ;; Find where arrows appear
                            (let ((arrow-line-num 0)
                                  (arrow-positions '()))
                              (dolist (line lines)
                                (setq arrow-line-num (1+ arrow-line-num))
                                (let ((pos 0))
                                  (while (string-match "[◀▶▲▼]" line pos)
                                    (let ((arrow-char (match-string 0 line))
                                          (arrow-pos (match-beginning 0)))
                                      (push (list arrow-line-num arrow-pos arrow-char line) arrow-positions)
                                      (setq pos (1+ (match-beginning 0)))))))

                              ;; Find where edge lines end
                              (let ((edge-line-num 0)
                                    (edge-positions '()))
                                (dolist (line lines)
                                  (setq edge-line-num (1+ edge-line-num))
                                  (let ((pos 0))
                                    (while (string-match "[│─┼┌┐└┘]" line pos)
                                      (let ((edge-char (match-string 0 line))
                                            (edge-pos (match-beginning 0)))
                                        (push (list edge-line-num edge-pos edge-char line) edge-positions)
                                        (setq pos (1+ (match-beginning 0)))))))

                                (message "\nArrows found:")
                                (dolist (arrow arrow-positions)
                                  (message "Line %d, pos %d: '%s' in '%s'"
                                           (nth 0 arrow) (nth 1 arrow) (nth 2 arrow) (nth 3 arrow)))

                                (message "\nEdge characters found:")
                                (dolist (edge edge-positions)
                                  (message "Line %d, pos %d: '%s' in '%s'"
                                           (nth 0 edge) (nth 1 edge) (nth 2 edge) (nth 3 edge)))

                                ;; Check for connectivity issues (GKNV-compliant logic)
                                ;; GKNV requires arrows to show direction (Line 372-373) and avoid visual anomalies (A2)
                                (let ((disconnected-arrows '()))
                                  (dolist (arrow arrow-positions)
                                    (let* ((arrow-line (nth 0 arrow))
                                           (arrow-pos (nth 1 arrow))
                                           (arrow-char (nth 2 arrow))
                                           (arrow-line-str (nth 3 arrow))
                                           (connected nil))

                                      ;; Check if arrow is adjacent to any edge character
                                      (dolist (edge edge-positions)
                                        (let ((edge-line (nth 0 edge))
                                              (edge-pos (nth 1 edge)))
                                          (when (and (= edge-line arrow-line)
                                                     (<= (abs (- edge-pos arrow-pos)) 1))
                                            (setq connected t))))

                                      ;; IMPROVED: Also allow arrows that are part of legitimate boundary patterns
                                      ;; Check if arrow is followed/preceded by boundary characters (node borders)
                                      (when (and (not connected) arrow-line-str)
                                        (let ((after-char (when (< (1+ arrow-pos) (length arrow-line-str))
                                                            (string (aref arrow-line-str (1+ arrow-pos)))))
                                              (before-char (when (> arrow-pos 0)
                                                             (string (aref arrow-line-str (1- arrow-pos))))))
                                          ;; Allow arrows that are part of node boundary patterns
                                          (when (or (and after-char (string-match-p "[─┘┐└┌]" after-char))
                                                    (and before-char (string-match-p "[─┘┐└┌]" before-char)))
                                            (setq connected t))))

                                      (unless connected
                                        (push arrow disconnected-arrows))))

                                  (if disconnected-arrows
                                      (progn
                                        (message "\nDISCONNECTED ARROWS FOUND:")
                                        (dolist (arrow disconnected-arrows)
                                          (message "Floating arrow: %s at line %d, pos %d"
                                                   (nth 2 arrow) (nth 0 arrow) (nth 1 arrow))))
                                    (message "\nSUCCESS: All arrows are properly connected to edge lines")))))
                            )))))

(provide 'dag-draw-arrow-placement-debug-test)

;;; dag-draw-arrow-placement-debug-test.el ends here
