;;; dag-draw-ascii-whitespace-test.el --- ASCII output has no trailing whitespace -*- lexical-binding: t; -*-

;;; Commentary:
;; The ASCII renderer used to pad every row out to a uniform width and append
;; blank padding rows, leaving trailing whitespace on every line and a block of
;; blank lines at the bottom.  Rendered output should be clean.

;;; Code:

(require 'buttercup)
(require 'dag-draw)

(defun dd-ws--render ()
  (let ((g (dag-draw-create-from-spec
            :nodes '((a :label "Alpha") (b :label "Beta") (c :label "Gamma"))
            :edges '((a b) (a c)))))
    (dag-draw-layout-graph g)
    (dag-draw-render-graph g 'ascii)))

(describe "ASCII output whitespace"
  (it "leaves no trailing whitespace on any line"
    (dolist (line (split-string (dd-ws--render) "\n"))
      (expect line :not :to-match "[ \t]+\\'")))

  (it "has no trailing blank lines and no final newline"
    (let ((out (dd-ws--render)))
      (expect out :not :to-match "\n[ \t]*\\'")))

  (it "still renders the graph content"
    (let ((out (dd-ws--render)))
      (expect out :to-match "Alpha")
      (expect out :to-match "Beta")
      (expect out :to-match "Gamma"))))

(provide 'dag-draw-ascii-whitespace-test)
;;; dag-draw-ascii-whitespace-test.el ends here
