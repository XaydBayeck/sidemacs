
emacs-lisp-mode

(sct ";; " n ";; (@* \"" p "\" )" n ";;" n n r)

org-mode latex-mode ;; For math edit

(lb "\\begin{" (s env) "}" n> p n> "\\end{" env "}" n r)
(eqt "\\begin{equation}" n> p n> "\\end{equation}" n r)
(mt "\\begin{" (s type) "matrix}" n> p n> "\\end{" type "matrix}" n r)
(alg "\\begin{align*}" n> p n> "\\end{align*}" n r)
(arr "\\begin{array}{" p "}" n> p n> "\\end{array}" n r)
(fr "\\frac{" p "}" "{" p "}" q)
(im "$ " p " $" r)
(mm "\\[" n> p "." n> "\\]" n r)
(sum "\\sum_{" p "}^{" p "}" r)
(prd "\\product_{" p "}^{" p "}" r)
(lm0 "\\lim_{" p "\\to 0}" r)
(lmi "\\lim_{" p "\\to \\infty}" r)
(lim "\\lim_{" p "\\to" p "}" r)
(cc "\\subset")
(map "\\mapsto")
(oo "\\infty")
(dd "\\mathrm{d}")
(drv "\\frac{\\mathrm{d}" p "}{\\mathrm{d}" p "}" r)
(prv "\\frac{\\partial " p "}{\\partial " p "}" r)
(^ "^{" p "}" r)
(_ "_{" p "}" r)
