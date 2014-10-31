.emacs.d
========

My Sweet Emacs Setup

Optimized for Clojure Development

	F10 - Toggle Top Menu Activation
	F9  - Runs cider-jack-in
	F7  - Toggle Paraedit






---

[Keys]

	IDE:

		C-c C-k  => Build
		C-c ,    => Run Tests
		C-c, M-n => Change Namespace

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
