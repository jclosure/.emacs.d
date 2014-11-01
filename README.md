.emacs.d
========

My Sweet Emacs Setup

Optimized for Clojure Development

![alt tag](https://github.com/jclosure/.emacs.d/blob/master/extra/screenshot.png)


	F10 - Toggle Top Menu Activation
	F9  - Runs cider-jack-in
	F7  - Toggle Paraedit

---

[Keys]

	CIDER IDE:

		C-c C-k  => Build
		C-c ,    => Run Tests
		C-c M-n  => Change Namespace
        C-c M-e  => cider-eval-last-sexp-to-repl
		C-c M-p  => cider-insert-last-sexp-in-repl
		C-c C-w  => cider-eval-last-expression-and-replace (This on is crazy powerful for inplace computed value replacement of the source with output from the source itself)
		 
	REPL:

		C-c, M-o => Clear REPL
		M-p      => Previous Command
		M-n      => Next Command


	Standard:

		M-:      => Eval in minibuff
		M-x      => Execute in minibuff

		C-x C-e  => Run prev s-expression in Minibuff
		C-x C-f  => Find/Create File
		C-x C-s  => Save buffer to File
		C-x C-w  => Save As buffer to File
		C-x C-c  => Exit

		C-v      => Page Up
		M-v      => Page Down

		C-x <-   => Prev Buffer
		C-x ->   => Next Buffer
		C-x b    => Pop Buffer

	Cheat Sheet:

	      http://www.cs.colostate.edu/helpdocs/emacs.html
---

[Features]

	Removes annoying nags screens on exit
	Consolidated saves directory
	Saves place in files
	Corrects Emacs shell path to one specified in .profile (or .bash_profile)
	Shows matching parens

---

[Packages]

	Melpa Stable:

		auto-complete
		cider
		ac-cider
		company
		paredit
		rainbow-delimiters
		ir-black-theme
		pretty-mode
