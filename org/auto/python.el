(TeX-add-style-hook
 "python"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "11pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("inputenc" "latin1") ("fontenc" "T1") ("ulem" "normalem")))
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art11"
    "inputenc"
    "fontenc"
    "fixltx2e"
    "graphicx"
    "grffile"
    "longtable"
    "wrapfig"
    "rotating"
    "ulem"
    "amsmath"
    "textcomp"
    "amssymb"
    "capt-of"
    "hyperref")
   (LaTeX-add-labels
    "sec:orgheadline7"
    "sec:orgheadline1"
    "tab:orgtable1"
    "sec:orgheadline2"
    "tab:orgtable2"
    "sec:orgheadline3"
    "sec:orgheadline4"
    "tab:orgtable3"
    "sec:orgheadline5"
    "sec:orgheadline6")))

