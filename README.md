.emacs.d
========

##Manicured Emacs Configuration

###Optimized for Clojure, C++, and Python  Development
  
####Global Features

	Removes annoying nags screens on exit
	Moves saves and backups to system temp directory
	Saves place in files
	Corrects Emacs shell path to one specified in .profile (or .bash_profile)
	Shows matching parens
	Windmove easy moving between windows
	Sublime themes

---

####Installation

* Basic Installation:
  * cd ~
  * git clone https://github.com/jclosure/.emacs.d.git
  * emacs

* Setup External Dependencies:
  * install python
  * install cpplint
    * pip install cpplint
    * set the path to cpplint in init.el
  * install jedi server
    * pip install virtualenv
    * (fron in emacs) run M-x jedi:install-server
    

---

####Python Development

![alt tag](https://github.com/jclosure/.emacs.d/blob/master/extra/python-dev-screenshot.png)


	C-c C-p Active REPL
	C-c C-c Send buffer to REPL

---

####Clojure Development

![alt tag](https://github.com/jclosure/.emacs.d/blob/master/extra/clojure-dev-screenshot.png)


	F10 - Toggle Top Menu Activation
	F9  - Runs cider-jack-in
	F7  - Toggle Paraedit


####Keys

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


---

Resources:

[Emacs Cheat Sheet](https://www.gnu.org/software/emacs/refcards/pdf/refcard.pdf)

[Emacs Survival Guide](https://www.gnu.org/software/emacs/refcards/pdf/survival.pdf)


