;;; -*- auto-recompile: t -*-

;;; keykit-mode.el --- editing and evaluating Keykit code within Emacs

;; Keywords: keykit, geomaestro, language

;; This file is not part of GNU Emacs.
;; 
;; keykit-mode.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; keykit-mode.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.


;;; Commentary:
;;-------------

;; Provides indentation and fontification (based on a hack of c-mode.el adapted
;; for Keykit)
;;
;; + Includes an on-line help browser
;;
;; + Can generate HTML documentation from specific comments in the sources. 
;;
;; + Handles lowkey invocations
;;
;; + Also able to use a TCP/IP session in order to have a full-featured Keykit console 
;; directly within an Emacs buffer, and also to evaluate Keykit code directly from
;; the source file.
;; (this requires from Keykit side the code in GeoMaestro/lib/tcpip.k)
;;
;;
;; NOTE: some code here is useless because it comes from the original c-mode and
;; I didn't remove it by fear of breaking something. Quite ugly, sorry...
;;
;;                                                      Stef (hepta@zogotounga.net)
;;
;; support Font Lock and Outline minor modes
;;
;; last version and more detailed documentation at:
;; http://www.zogotounga.net/GM/GMemacs.html


;; last modified March 30, 2005
;; (requires Emacs 21.1)
;;; --------------------------------------------------------------------------------

;;; Code:

(require 'cl)
(require 'kk_utils)
(require 'query-sheet)


;;; --------------------------- Customization Macros  -----------------------------


(defgroup keykit nil
  "Major mode for editing Keykit code."
  :group 'languages
  :prefix "kk-")

(defgroup keykit-doc nil
  "HTML auto-documentation of Keykit libraries."
  :group 'keykit
  :prefix "kk-HTML-")

(defgroup keykit-util nil
  "Interfacing Key and Lowkey with Emacs."
  :group 'keykit
  :prefix "kk-")


(defcustom kk-default-directory "c:/keykit/"
  "Keykit root directory"
  :type 'directory
  :group 'spfa-paths
  :group 'keykit)

(defun kk-default-directory ()
  (substitute-in-file-name kk-default-directory))

(defcustom kk-case-fold-search t
  "If not nil the searches do ignore case"
  :type 'boolean
  :group 'keykit)

(defcustom kk-lowkey-executable "lowkey.exe"
  "Valid name for lowkey executable (with or without full path)"
  :type 'file
  :group 'spfa-paths
  :group 'keykit-util)

(defun kk-lowkey-executable ()
  (substitute-in-file-name kk-lowkey-executable))

(defcustom kk-keypath  '("c:/keykit/contrib/GeoMaestro/lib/"
			 "c:/keykit/contrib/GeoMaestro/userlib/"
			 "c:/keykit/lib/"
			 "c:/keykit/liblocal/")
  "List of directories constituting Keypath"
  :type  '(repeat directory)
  :group 'spfa-paths
  :group 'keykit)

(defun kk-keypath ()
  (mapcar 'substitute-in-file-name kk-keypath))

(defcustom kk-HTML-directory "c:/keykit/doc/"
  "Directory where auto HTML documentation is written"
  :type 'directory
  :group 'spfa-paths
  :group 'keykit-doc)

(defun kk-HTML-directory ()
  (substitute-in-file-name kk-HTML-directory))

(defcustom kk-HTML-title "Library of Keykit functions"
  "Title to be given to the HTML documentation"
  :type 'string
  :group 'keykit-doc)

(defcustom kk-HTML-external-link ""
  "URL pointing wherever you want to escape the HTML documentation"
  :type 'string
  :group 'keykit-doc)

(defcustom kk-HTML-external-link-label ""
  "The text/label used when linking to kk-HTML-external-link (default is \"back\")"
  :type 'string
  :group 'keykit-doc)

(defcustom kk-HTML-topics-links '(("GVARS" "../GMtips.html#GVARS")
				  ("csound" "../GMcsound.html"))
  "Links to places where topics are defined/explained/illustrated"
  :type '(repeat (list string string))
  :group 'keykit-doc)

(defcustom kk-HTML-topic-link-label "More about this topic here: "
  "The text/label used when linking from a topic page"
  :type 'string
  :group 'keykit-doc)

(defcustom kk-HTML-background-picture ""
  "Optional background picture for the HTML documents.\nThe name can be absolute or relative to the `kk-HTML-directory'."
  :type 'string
  :group 'keykit-doc)

(defcustom kk-HTML-background-color "white"
  "Background color for the HTML documents navigation links."
  :type 'color
  :group 'keykit-doc)

(defcustom kk-HTML-doc-chunk  20
  "Number of functions described per page in the HTML documentation"
  :type 'integer
  :group 'keykit-doc)

(defcustom kk-HTML-doc-directory ""
  "Keykit root directory as for HTML documentation.
if \"\", it is defaulted as `kk-default-directory'. This is so that we can document an instance of Keykit installation while working on another one. If your `kk-HTML-doc-keypath' is a subset of `kk-HTML-keypath', don't worry about this. "
  :type 'directory
  :group 'keykit-doc)

(defun kk-HTML-doc-directory ()
  (substitute-in-file-name kk-HTML-doc-directory))

(defcustom kk-HTML-doc-keypath 
  '("c:/keykit/contrib/GeoMaestro/lib/"
    "c:/keykit/contrib/GeoMaestro/userlib/")
  "List of directories analog to `kk-keypath' used by the HTML documentation system.\nIt can be a subset of ``kk-keypath', or a completely different set of directories: the HTML doc can be created from another version or installation of Keykit than the one you are working on."
  :type  '(repeat directory)
  :group 'spfa-paths
  :group 'keykit-doc)

(defun kk-HTML-doc-keypath ()
  (mapcar 'substitute-in-file-name kk-HTML-doc-keypath))

(defcustom GeoMaestro-manual-URL "http://www.zogotounga.net/GM/eGM0.html"
  "URL for the index page of GeoMaestro HTML manual.
this is only used when creating HTML autodocumentation. If you have a local copy of the manual on your computer, you should point to it here. If you intend to publish the documentation on the web, then keep the default URL (except if the link is dead by the time..)"
  :type 'string
  :group 'spfa-paths
  :group 'keykit-doc)

(defcustom kk-HTML-electric-link-comment "<!--doc-->"
  "HTML comment marking a HREF dependent on the HTML documentation."
  :type 'string
  :group 'keykit-doc)

(defcustom kk-HTML-dependent-pages '()
  "List of directories that will be scanned for HTML pages.
all links in these pages from keywords corresponding to a documented function and marked with `kk-HTML-electric-link-comment' will be automatically updated to the current location of the function documentation."
  :type  '(repeat directory)
  :group 'keykit-doc)

(defcustom kk-indent-level 4
  "Indentation of Keykit statements with respect to containing block."
  :type 'integer
  :group 'keykit)

(defcustom kk-indent-method 0
  "Indentation of methods with respect to containing class."
  :type 'integer
  :group 'keykit)

(defcustom kk-auto-newline t
  "Non-nil means automatic newline after braces and semicolons."
  :type 'boolean
  :group 'keykit)

(defcustom kk-indent-tabs-mode nil
  "Indentation can insert tabs \(instead of spaces) if this is non-nil."
  :type 'boolean
  :group 'keykit)

(defcustom kk-geomaestro-items t
  "Non-nil adds specific GeoMaestro items to the Keykit menu.
Change only takes place at the next session \(or keykit-mode.el evaluation)"
  :type 'boolean
  :group 'keykit)

(defcustom kk-geomaestro-dvlp-items nil
  "Non-nil adds items to the Keykit menu for developing and debugging the GeoMaestro system. 
Also toggle Emacs keykit-mode itself in debug mode. Change only takes place at the next session \(or keykit-mode.el evaluation)"
  :type 'boolean
  :group 'keykit)


(defvar kk-tab-always-indent t
  "Non-nil means TAB in Keykit mode should always reindent the current line,
 regardless of where in the line point is when the TAB command is used.")


;;; --------------------------- Keymap & menus -----------------------------

(defvar keykit-mode-abbrev-table nil
  "Abbrev table in use in KK mode.")

(define-abbrev-table 'keykit-mode-abbrev-table ())

(defvar keykit-mode-map (make-sparse-keymap)
  "Keymap used in KK mode.")

(define-key keykit-mode-map "{" 'electric-kk-semi)
(define-key keykit-mode-map "}" 'electric-kk-semi)
(define-key keykit-mode-map ";" 'electric-kk-semi)
(define-key keykit-mode-map "\r" 'electric-kk-return)
(define-key keykit-mode-map "\e\C-h" 'mark-kk-function)
(define-key keykit-mode-map "\e\C-q" 'indent-kk-exp)
(define-key keykit-mode-map "\ea" 'kk-beginning-of-statement)
(define-key keykit-mode-map "\ee" 'kk-end-of-statement)
(define-key keykit-mode-map "\177" 'backward-delete-char-untabify)
(define-key keykit-mode-map "\t" 'kk-indent-command)
(define-key keykit-mode-map "\e?" 'kk-insert-testonnotes)
(define-key keykit-mode-map "\et" 'kk-insert-tab)
(define-key keykit-mode-map "\em" 'kk-man)
(define-key keykit-mode-map "\ec" 'kk-visit-code)
(define-key keykit-mode-map "\ed" 'kk-insert-doc-comments)

(when kk-geomaestro-dvlp-items
  (define-key keykit-mode-map "\ef" 'kk-add-GMblah-from-french)
  (define-key keykit-mode-map "\eb" 'kk-add-GMblah-from-english)
  (define-key keykit-mode-map "\ew" 'kk-what-blah))


(easy-menu-define keykit-menu keykit-mode-map
  "Menu provided by keykit-mode"
  `("Keykit"
      ["Customize Keykit mode"        (customize-group 'keykit) t]  
      ["Toggle minor mode Outline"    outline-mode t]
      "--"
      ["Run lowkey on this file"      kk-run-lowkey t]
      ["TCP/IP console"               kk-tcpip t]
      ["Evaluate region"              kk-send-region-through-back-door t]
      "--"
      ["Functions, class, methods"    (occur "^\\(function\\|class\\|method\\)") t]
      ["Forward Statement"            kk-end-of-statement t]
      ["Backward Statement"           kk-beginning-of-statement t]
      ["Indent Line"                  kk-indent-command t]
      ["Indent Region"                indent-region t]
      ["Indent Expression"            indent-kk-exp t]
      ["Beginning of function"        beginning-of-defun t]
      ["End of function"              end-of-defun t]
      ["Select function"              mark-kk-function t]
      ["Unfold next array"            kk-unfold-next-array-in-line t]
      "--"
      ["Comment Out Region"           comment-region t]
      ["Uncomment Region"             kk-uncomment-region t] ;; pourquoi pas uncomment-region ?
      "--"
      ["Insert  TAB"                  (insert "\t") t]
      ["Untabify buffer"              kk-untabify-buffer t]
      "--"
      ["Insert  {}"                   (progn (insert "{}") (backward-char 1)) t]
      ,@(when kk-geomaestro-items
	  '(["Insert projector template"           kk-insert-projector-template t]
	    ["Insert distortion function template" kk-insert-df-template t]
	    ["Insert GUI plug-in template"         kk-insert-plugin-template t]
	    ["Insert Compositor plug-in template"  kk-insert-compoplugin-template t]))
      ,@(when kk-geomaestro-dvlp-items
	  '("--"
	    ["GMblah_ ?"                           kk-what-blah t]
	    [".. to EnglishBlahBlah()"             kk-add-GMblah-from-english t]
	    [".. to FrenchBlahBlah()"              kk-add-GMblah-from-french t]))))


(easy-menu-define kkdoc-menu keykit-mode-map
  "Menu provided by keykit-mode"
  `("KK-doc"
      ["Insert documentation fields"        kk-insert-doc-comments t] 
      "--"
      ["Customize HTML man pages"           kk-customize-HTML-group t]
      ["Make HTML man pages"                kk-make-HTML-man t]
      ["Browse HTML man pages"              (browse-url 
                                             (expand-file-name "index.html" (kk-HTML-directory)))  t]
      ["Update all dependent HTML files"    kk-HTML-scan-and-update-all t]
      ["Update dependent HTML buffers"      kk-scan-buffer-and-update t]
      "--"
      ["man"                                kk-man t]
      ["visit code"                         kk-visit-code t]
      ["regexp search on topics"            kk-regexp-search-topics t]
      "--"
      ["search Keypath"                     kk-search-keypath t]
      ["regexp search Keypath"              kk-regexp-search-keypath t]))


;;; --------------------------- Syntax Table  -----------------------------

(defvar keykit-mode-syntax-table nil 
  "Syntax table in use in KK-mode buffers.")

(if keykit-mode-syntax-table
    ()
  (setq keykit-mode-syntax-table (copy-syntax-table c-mode-syntax-table))
  (modify-syntax-entry ?\\ "\\" keykit-mode-syntax-table)
  (modify-syntax-entry ?/ "." keykit-mode-syntax-table)
  (modify-syntax-entry ?. "'" keykit-mode-syntax-table)
  (modify-syntax-entry ?* "." keykit-mode-syntax-table)
  (modify-syntax-entry ?+ "." keykit-mode-syntax-table)
  (modify-syntax-entry ?- "." keykit-mode-syntax-table)
  (modify-syntax-entry ?= "." keykit-mode-syntax-table)
  (modify-syntax-entry ?% "." keykit-mode-syntax-table)
  (modify-syntax-entry ?< "." keykit-mode-syntax-table)
  (modify-syntax-entry ?> "." keykit-mode-syntax-table)
  (modify-syntax-entry ?& "." keykit-mode-syntax-table)
  (modify-syntax-entry ?| "." keykit-mode-syntax-table)
  (modify-syntax-entry ?# "<" keykit-mode-syntax-table)
  (modify-syntax-entry ?\n ">" keykit-mode-syntax-table)
  (modify-syntax-entry ?\r ">" keykit-mode-syntax-table)
;  (modify-syntax-entry ?\' "$" keykit-mode-syntax-table)
  (modify-syntax-entry ?\" "\"" keykit-mode-syntax-table)  

  (modify-syntax-entry ?( "()" keykit-mode-syntax-table)
  (modify-syntax-entry ?) ")(" keykit-mode-syntax-table)
)


;;; --------------------------- Core code  -----------------------------

;;; --------- ... with remnants of the original c-mode  -----------------


(defvar kk-brace-imaginary-offset 0
  "*Imagined indentation of a C open brace that actually follows a statement.")
(defvar kk-brace-offset 0
  "*Extra indentation for braces, compared with other text in same context.")
(defvar kk-argdecl-indent 5
  "*Indentation level of declarations of C function arguments.")
(defvar kk-label-offset -2
  "*Offset of C label lines and case statements relative to usual indentation.")
(defvar kk-continued-statement-offset 0
  "*Extra indent for lines not starting new statements.")
(defvar kk-continued-brace-offset 0
  "*Extra indent for substatements that start with open-braces.
This is in addition to `kk-continued-statement-offset'.")

;;; Regular expression used internally to recognize labels in switch
;;; statements. Hacked for Keykit arrays declaration on several lines (??)
; (defconst kk-switch-label-regexp "case[ \t'/(]\\|default[ \t]*:")
(defconst kk-array-item-declaration-qui-marche-pas "^.*=.*,[ \t]*$")


(defun keykit-mode ()
  "Major mode for editing Keykit code, based on the so-called \"boring old\" c-mode.

Tab indents for Keykit code, by default with space characters only.
Comments are delimited with # ... \\n.


\\[keykit-mode-map]

Turning on Keykit mode calls the value of the variable `keykit-mode-hook' with no args,
if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (use-local-map keykit-mode-map)
  (setq major-mode 'keykit-mode)
  (setq mode-name "KK")
  (setq local-abbrev-table keykit-mode-abbrev-table)
  (set-syntax-table keykit-mode-syntax-table)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'kk-indent-line)
  (make-local-variable 'indent-region-function)
  (setq indent-region-function 'kk-indent-region)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)
  (make-local-variable 'outline-regexp)
  (setq outline-regexp "^fun\\|^cla\\|[ \t]*method ")

;     (make-local-variable 'outline-level)
;     (setq outline-level 'kk-outline-level)

  (make-local-variable 'defun-prompt-regexp)
  (setq defun-prompt-regexp "\\(^function\\|class\\|[ \t]*method\\)[ \t]+[ a-zA-Z0-9_]*\\(([^)]*)\\)*[ \t]*")
  (make-local-variable 'comment-start)
  (setq comment-start "#")
  (make-local-variable 'comment-end)
  (setq comment-end "")
  (make-local-variable 'comment-column)
  (setq comment-column 32)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "#\\([^\"\n\r]*\\|\\(\"[^\"\n\r]*\"\\)\\)*$")
  (make-local-variable 'comment-indent-function)
  (setq comment-indent-function 'kk-comment-indent)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)
  (setq indent-tabs-mode kk-indent-tabs-mode)
  (keykit-fontify)
  (run-hooks 'keykit-mode-hook))


(defun kk-untabify-on-saving ()
  (setq local-write-file-hooks '(kk-untabify-buffer)))


;; This is used by indent-for-comment
;; to decide how much to indent a comment in Keykit code
(defun kk-comment-indent ()
  (current-column))


(defun electric-kk-semi (arg)
  "Insert character and correct line's indentation."
  (interactive "P")
  (if kk-auto-newline
      (electric-kk-terminator arg)
    (self-insert-command (prefix-numeric-value arg))))


(defun electric-kk-return (arg)
  "Correct line's indentation and insert newline. 
If in a #desc field, add a new #desc line."
  (interactive "P")
  (progn 
    (kk-indent-line)
    (newline)
    (if (save-excursion
          (previous-line 1)
          (beginning-of-line)
          (looking-at "^#desc\t"))
        (insert "#desc\t"))))


(defun electric-kk-terminator (arg)
  "Insert character and correct line's indentation."
  (interactive "P")
  (let (insertpos (end (point)))
    (if (and (not arg) (eolp)
             (not (save-excursion
                    (beginning-of-line)
                    (skip-chars-forward " \t")
                    (or (= (following-char) ?#)
                        ;; Colon is special only after a label, or case ....
                        ;; So quickly rule out most other uses of colon
                        ;; and do no indentation for them.
                        (and (eq last-command-char ?:)
                             (not (looking-at kk-array-item-declaration-qui-marche-pas))
                             (save-excursion
                               (skip-chars-forward "a-zA-Z0-9_$")
                               (skip-chars-forward " \t")
                               (< (point) end)))
                        (progn
                          (beginning-of-defun)
                          (let ((pps (parse-partial-sexp (point) end)))
                            (or (nth 3 pps) (nth 4 pps) (nth 5 pps))))))))
        (progn
          (insert last-command-char)
          (kk-indent-line)
          (and kk-auto-newline
               (not (kk-inside-parens-p))
               (progn
                 (newline)
                 ;; (newline) may have done auto-fill
                 (setq insertpos (- (point) 2))
                 (kk-indent-line)))
          (save-excursion
            (if insertpos (goto-char (1+ insertpos)))
            (delete-char -1))))
    (if insertpos
        (save-excursion
          (goto-char insertpos)
          (self-insert-command (prefix-numeric-value arg)))
      (self-insert-command (prefix-numeric-value arg)))))


(defun kk-inside-parens-p ()
  (condition-case ()
      (save-excursion
        (save-restriction
          (narrow-to-region (point)
                            (progn (beginning-of-defun) (point)))
          (goto-char (point-max))
          (= (char-after (or (scan-lists (point) -1 1) (point-min))) ?\()))
    (error nil)))


(defun kk-indent-command (&optional whole-exp)
  "Indent current line as Keykit code, or in some cases insert a tab character.
If `kk-tab-always-indent' is non-nil \(the default), always indent current line.
Otherwise, indent the current line only if point is at the left margin or
in the line's indentation; otherwise insert a tab.

Also insert a tab if the point is in a comment line or before a comment at the
end of a line of code. 

A numeric argument, regardless of its value, means indent rigidly all the
lines of the expression starting after point so that this line becomes
properly indented.  The relative indentation among the lines of the
expression are preserved."
  (interactive "P")

  (if (or (looking-at "[\t ]*#")
          (save-excursion
            (beginning-of-line)
            (looking-at "[\t ]*#")))
      (insert-tab)

    (if whole-exp
        ;; If arg, always indent this line as C
        ;; and shift remaining lines of expression the same amount.
        (let ((shift-amt (kk-indent-line))
              beg end)
          (save-excursion
            (if kk-tab-always-indent
                (beginning-of-line))
            ;; Find beginning of following line.
            (save-excursion
              (forward-line 1) (setq beg (point)))
            ;; Find first beginning-of-sexp for sexp extending past this line.
            (while (< (point) beg)
              (forward-sexp 1)
              (setq end (point))
              (skip-chars-forward " \t\n")))
          (if (> end beg)
              (indent-code-rigidly beg end shift-amt "#")))
      (if (and (not kk-tab-always-indent)
               (save-excursion
                 (skip-chars-backward " \t")
                 (not (bolp))))
          (insert-tab)
        (kk-indent-line)))))


(defun kk-special-previous-line (nb-lines)
  "Used by `kk-indent-line' in order to take into account
if, else, for and while constructs with no braces \(one-ligne
long blocks). 
Return an integer which will be added to indent."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (looking-at "[\t ]$\\|[\t ]*[{}]")
        0
      (kk-previous-interesting-line)
      (if (looking-at "[\t ]$\\|[\t ]*[{}]")
          0
        (if (eq nb-lines 2) (kk-previous-interesting-line))
        (beginning-of-line)
        (if (save-excursion
              (or (and (looking-at "[\t ]*\\(if\\|while\\|else if\\|for\\) *(")
                       (progn
                         (kk-end-of-statement 1)
                         (looking-at "[\t ]*\\($\\|#\\)")))
                  (looking-at "[\t ]*else[\t ]*\\($\\|#\\)")))
            kk-indent-level
          0)))))


(defun kk-previous-interesting-line ()
  (if (and (equal (what-line)
                  (save-excursion
                    (kk-beginning-of-statement 1)
                    (what-line)))
           (not (equal (what-line)
             (save-excursion
               (goto-char (point-min))
               (what-line)))))
      (progn
        (previous-line 1)
        (end-of-line))
    (kk-beginning-of-statement 1)))


(defun kk-indent-line ()
  "Indent current line as Keykit code.
Return the amount the indentation changed by."

  (if (or (looking-at "[\t ]*#")
          (save-excursion
            (beginning-of-line)
            (looking-at "[\t ]*#")))
      0

    (let ((indent (calculate-kk-indent nil))
          beg shift-amt
          (case-fold-search nil)
          (pos (- (point-max) (point))))
    
      (beginning-of-line)
      (setq beg (point))
      (cond ((eq indent nil)
             (setq indent (current-indentation)))

            ((eq indent t)
             (setq indent (current-indentation)))

            (t
             (skip-chars-forward " \t")
             (if (listp indent) (setq indent (car indent)))
             (cond 
            
              ((and (looking-at "while\\b")
                    (not (looking-at "while\\s_"))
                    (save-excursion
                      (kk-backward-to-start-of-do)))
               ;; This is a `while' that ends a do-while.
               (setq indent (save-excursion
                              (kk-backward-to-start-of-do)
                              (current-indentation))))

              ((= (following-char) ?})
               (setq indent (- indent kk-indent-level)))
              
              ((= (following-char) ?{)
               (setq indent (+ indent kk-brace-offset))))))
      
      (skip-chars-forward " \t")
      (setq shift-amt (- indent (current-column)))

      (progn 
        (delete-region beg (point))

        ;; are we just after a complex array declaration ?
	(let (array-declaration column)
	  (save-excursion
	    (beginning-of-line)
	    (let ((prev-pos (point)))
	      (kk-beginning-of-statement 1)
	      (while (and (looking-at "\\[")
			  (< (point) prev-pos))
		(kk-beginning-of-statement 1)
		(setq prev-pos (point))))
	    (setq array-declaration (looking-at "\\[\\|.*=[^][]*\\[")
		  column (current-column)))
	  (if (and (/= (following-char) ?}) 
		   array-declaration
		   (> (save-excursion
			(end-of-line)
			(count-lines 1 (point)))
		      (save-excursion
			(beginning-of-line)
			(kk-beginning-of-statement 1)
			(kk-end-of-statement 1)
			(count-lines 1 (point)))))
	      (setq indent column)))

        (indent-to (+ indent (calculate-kk-special-offset)))
	
        ;; If initial point was within line's indentation,
        ;; position after the indentation.  Else stay at same point in text.
        (if (> (- (point-max) pos) (point))
            (goto-char (- (point-max) pos))))
     shift-amt)))


(defun calculate-kk-special-offset ()
  "...see `kk-special-previous-line'"
  (let* ((is-a-comment-line (save-excursion
                              (beginning-of-line)
                              (if (looking-at "[\t ]*#")
                                  t
                                nil)))
         (offset (if is-a-comment-line
                     0
                   (kk-special-previous-line 1))))      
    (if (and (zerop offset)
             (null is-a-comment-line)
             (null (zerop (kk-special-previous-line 2))))
        (save-excursion
          (while (null (zerop (kk-special-previous-line 2)))
            (setq offset (- offset (kk-special-previous-line 2)))
            (beginning-of-line)
            (kk-beginning-of-statement 1))))
    offset))


(defun calculate-kk-indent (&optional parse-start)
  "Return appropriate indentation for current line as Keykit code.
In usual case returns an integer: the column to indent to.
Returns nil if line starts inside a string, t if in a comment.
...this is not quite ready yet..."
   (save-excursion
    (beginning-of-line)
    (let ((indent-point (point))
          (case-fold-search nil)
          state
          containing-sexp)
      (if parse-start
          (goto-char parse-start)
        (beginning-of-defun))
      (while (< (point) indent-point)
        (setq parse-start (point))
        (setq state (parse-partial-sexp (point) indent-point 0))
        (setq containing-sexp (car (cdr state))))
      (cond ((or (nth 3 state) (nth 4 state))
             ;; return nil or t if should not change this line
             (nth 4 state))

            ((null containing-sexp)
             0)

            ((looking-at "[ \t]*method[ \t]")
             ;; user-defined indentation for methods
             kk-indent-method)

            ((/= (char-after containing-sexp) ?{)
             ;; line is expression, not statement:
             ;; indent to just after the surrounding open.
             (goto-char (1+ containing-sexp))
             (current-column))

            (t
             ;; Statement level.  Is it a continuation or a new statement?
             ;; Find previous non-comment character.
             (goto-char indent-point)
             (kk-backward-to-noncomment containing-sexp)


             ;; (...) Now we get the answer.
             (if (and (not (memq (preceding-char) '(0 ?\, ?\; ?\} ?\{)))
                      ;; But don't treat a line with a close-brace
                      ;; as a continuation.  It is probably the
                      ;; end of an enum type declaration.
                      (save-excursion
                        (goto-char indent-point)
                        (skip-chars-forward " \t")
                        (not (= (following-char) ?}))))
                 ;; This line is continuation of preceding line's statement;
                 ;; indent  kk-continued-statement-offset  more than the
                 ;; previous line of the statement.
                 (progn
                   (kk-backward-to-start-of-continued-exp containing-sexp)
                   (+ kk-continued-statement-offset (current-column)
                      (if (save-excursion (goto-char indent-point)
                                          (skip-chars-forward " \t")
                                          (eq (following-char) ?{))
                          kk-continued-brace-offset 0)))
               ;; This line starts a new statement.
               ;; Position following last unclosed open.
               (goto-char containing-sexp)
               ;; Is line first statement after an open-brace?
               (or
                 ;; If no, find that first statement and indent like it.
                 (save-excursion
                   (forward-char 1)
                   (let ((colon-line-end 0))
                     (while (progn (skip-chars-forward " \t\n")
                                   (looking-at "#\\|/\\*\\|case[ \t\n'/(].*:\\|[a-zA-Z0-9_$]*:"))
                       ;; Skip over comments and labels following openbrace.
                       (cond ((= (following-char) ?#)
                              (forward-line 1))
                             ((= (following-char) ?\/)
                              (forward-char 2)
                              (search-forward "*/" nil 'move))
                             ;; case or label:
                             (t
                              (save-excursion (end-of-line)
                                              (setq colon-line-end (point)))
                              (search-forward ":"))))
                     ;; The first following code counts
                     ;; if it is before the line we want to indent.
                     (and (< (point) indent-point)
                          (-
                           (if (> colon-line-end (point))
                               (- (current-indentation) kk-label-offset)
                             (current-column))
                           ;; If prev stmt starts with open-brace, that
                           ;; open brace was offset by kk-brace-offset.
                           ;; Compensate to get the column where
                           ;; an ordinary statement would start.
                           (if (= (following-char) ?\{) kk-brace-offset 0)))))
                 ;; If no previous statement,
                 ;; indent it relative to line brace is on.
                 (calculate-kk-indent-after-brace))))))))


(defun calculate-kk-indent-after-brace ()
  "Return the proper indent for the first line after an open-brace.
This function is called with point before the brace."
  ;; For open brace in column zero, don't let statement
  ;; start there too.  If kk-indent-level is zero,
  ;; use kk-brace-offset + kk-continued-statement-offset instead.
  ;; For open-braces not the first thing in a line,
  ;; add in kk-brace-imaginary-offset.
  (+ (if (and (bolp) (zerop kk-indent-level))
         (+ kk-brace-offset kk-continued-statement-offset)
       kk-indent-level)
     ;; Move back over whitespace before the openbrace.
     ;; If openbrace is not first nonwhite thing on the line,
     ;; add the kk-brace-imaginary-offset.
     (progn (skip-chars-backward " \t")
            (if (bolp) 0 kk-brace-imaginary-offset))
     ;; If the openbrace is preceded by a parenthesized exp,
     ;; move to the beginning of that;
     ;; possibly a different line
     (progn
       (if (eq (preceding-char) ?\))
           (forward-sexp -1))
       ;; Get initial indentation of the line we are on.
       (current-indentation))))

(defun kk-uncomment-region  (beg end)
  (interactive "r")
  (if (> beg end) (let (mid) (setq mid beg beg end end mid)))
  (save-excursion
    (goto-char beg)
    (beginning-of-line)
    (while (< (point) end)
      (if (looking-at "# ")
	  (progn
	    (delete-char 2)
	    (setq end (- end 2))))
      (next-line 1))))

 (defun kk-backward-to-noncomment (lim)
   (let (opoint stop)
     (while (not stop)
       (skip-chars-backward " \t\n\f" lim)
       (setq opoint (point))
        (if (and (>= (point) (+ 1 lim))
               (save-excursion
                 (forward-char -1)
                 (looking-at "#")))
          (search-backward "#" lim 'move)
        (setq stop (or (<= (point) lim)
                       (save-excursion
                         (beginning-of-line)
                         (skip-chars-forward " \t")
                         (not (looking-at "#")))))
        (or stop (beginning-of-line))))
     (kk-skip-comment-lines -1)
     (if (looking-at "#")
         (skip-chars-backward " \t"))))


(defun kk-backward-to-start-of-continued-exp (lim)
  (if (memq (preceding-char) '(?\) ?\"))
      (forward-sexp -1))
  (beginning-of-line)
  (if (<= (point) lim)
      (goto-char (1+ lim)))
  (skip-chars-forward " \t"))


(defun kk-backward-to-start-of-do (&optional limit)
  "If point follows a `do' statement, move to beginning of it and return t.
Otherwise return nil and don't move point."
  (or limit (setq limit (save-excursion (beginning-of-defun) (point))))
  (let ((first t)
        (startpos (point))
        (done nil))
    (while (not done)
      (let ((next-start (point)))
        (condition-case nil
            ;; Move back one token or one brace or paren group.
            (backward-sexp 1)
          ;; If we find an open-brace, we lose.
          (error (setq done 'fail)))
        (if done
            nil
          ;; If we reached a `do', we win.
          (if (looking-at "do\\b")
              (setq done 'succeed)
            ;; Otherwise, if we skipped a semicolon, we lose.
            ;; (Exception: we can skip one semicolon before getting
            ;; to a the last token of the statement, unless that token
            ;; is a close brace.)
            (if (save-excursion
                  (forward-sexp 1)
                  (or (and (not first) (= (preceding-char) ?}))
                      (search-forward ";" next-start t
                                      (if (and first
                                               (/= (preceding-char) ?}))
                                          2 1))))
                (setq done 'fail)
              (setq first nil)
              ;; If we go too far back in the buffer, we lose.
              (if (< (point) limit)
                  (setq done 'fail)))))))
    (if (eq done 'succeed)
        t
      (goto-char startpos)
      nil)))


(defun kk-beginning-of-statement (count)
  "Go to the beginning of the innermost Keykit statement.
With prefix arg, go back N - 1 statements.  If already at the beginning of a
statement then go to the beginning of the preceding one. Ignore all comments."
  (interactive "p")
  (let (still-within-comments)
    (while (> count 0)
      (setq still-within-comments 1)
      (while still-within-comments
        (while (kk-skip-comment-lines -1))
        (kk-beginning-of-statement-1)
        (setq still-within-comments  (kk-skip-comment-lines -1)))
      (setq count (1- count)))
    (while (< count 0)
      (setq still-within-comments 1)
      (while still-within-comments
        (while (kk-skip-comment-lines 1))
        (kk-end-of-statement-1)
        (setq still-within-comments (kk-skip-comment-lines 1)))
      (setq count (1+ count)))))


(defun kk-end-of-statement (count)
  "Go to the end of the innermost Keykit statement.
With prefix arg, go forward N - 1 statements.
Move forward to end of the next statement if already at end."
  (interactive "p")
  (kk-beginning-of-statement (- count)))


(defun kk-skip-comment-lines (direction)
  "Used exclusively by `kk-beginning-of-statement', allowing it to
ignore comments"
  (if (save-excursion
        (beginning-of-line)
        (looking-at "[ \t]*#\\|[ \t]*$"))
      ;; case 1: full comment line
      (if (eq 1 direction)
          (progn
            (if (equal (what-line)
                       (save-excursion
                         (goto-char (point-max))
                         (what-line)))
                nil
              (next-line 1)  
              (kk-skip-comment-lines 1)
              (beginning-of-line)
              t))
        (if (equal "Line 1" (what-line) )
            nil
          (previous-line 1)
          (kk-skip-comment-lines -1)
          (end-of-line)
          t))
    ;; case 2: comment at the end of a line
    (if  (and
          (null (looking-at "\\([^\"\n\r]\\|\\(\"[^\"\n\r]*\"\\)\\)*#"))
          (save-excursion
            (beginning-of-line)
            (looking-at "\\([^\"\n\r]\\|\\(\"[^\"\n\r]*\"\\)\\)*#"))) 
        (if (eq 1 direction)
              (progn
                (next-line 1)  
                (beginning-of-line)
                (kk-skip-comment-lines 1)             
                t)
              (re-search-backward "#")
              t)
      nil)))


(defun kk-beginning-of-statement-1 ()
  (let ((last-begin (point))
        (first t))
    (condition-case ()
        (progn
          (while (and (not (bobp))
                      (progn
                        (backward-sexp 1)
                        (or first
                            (not (re-search-forward "[][;{}()\n\r]" last-begin t)))))
            (setq last-begin (point) first nil))
          (goto-char last-begin))
      (error (if first (backward-up-list 1) (goto-char last-begin))))))


(defun kk-end-of-statement-1 ()
  (condition-case ()
      (progn
        (while (and (not (eobp))
                    (let ((beg (point)))
                      (forward-sexp 1)
                      (let ((end (point)))
                        (save-excursion
                          (goto-char beg)
                          (not (re-search-forward "[][;{}()\n\r\"]" end t)))))))
        (re-search-backward "[];})\n\r\"]")
        (forward-char 1))
    (error
     (let ((beg (point)))
       (backward-up-list -1)
       (let ((end (point)))
         (goto-char beg)
         (search-forward ";\n\r" end 'move))))))


(defun mark-kk-function ()
  "Put mark at end of Keykit function/class, point at beginning."
  (interactive)
  (push-mark (point))
  (end-of-defun)
  (push-mark (point) nil t)
  (beginning-of-defun)
  (if (looking-at "{")
      (progn
        (kk-beginning-of-statement 1)
        (beginning-of-line))))


(defun indent-kk-exp ()
  "Indent the innermost statement the point is in"
  (interactive)
  (save-excursion
    (kk-indent-region (progn
                        (kk-beginning-of-statement 1)
                        (point))
                      (progn
                        (kk-end-of-statement 1)
                        (point)))))


(defun kk-indent-region (start end)
  (let (end-line)
    (save-excursion
      (goto-char end)
      (setq end-line (what-line))
      (goto-char start)
      (while (not (equal (what-line) end-line))
        (beginning-of-line)
        (kk-indent-line)
        (next-line 1)))))


;;; --------------------------- Font Lock Mode  -----------------------------

(defconst keykit-font-lock-keywords
  (list
   '("^[\t ]*\\(if\\|else if\\|while\\|for\\)[ (]\\|^[\t ]*else[\t \r\n]*"
     0 font-lock-keyword-face t)
   '("^\\(function\\|class\\|[ \t]*method\\)[ \t]+[ a-zA-Z0-9_]*\\(([^)]*)\\)*" 
     0 font-lock-function-name-face t)
   '("^[ \t]*\\(return\\|error\\)[ \t]*([^)]*" 
     1 font-lock-builtin-face t)
   '("^[ \t]*\\(break\\|continue\\)[\t ]*$" 
     1 font-lock-builtin-face t)
   '("\\(system\\)[ (]" 
     1 font-lock-warning-face t)
   '("-\\*[^*]*\\*-" 
     0 font-lock-warning-face t)
   '("^\\([^\"\n\r#]\\|\\(\"[^\"\n\r]*\"\\)\\)*\\(\\(#.*\\)\\)$"
     4 font-lock-comment-face t)
   '("^\\([^\"\n\r#]\\|\\(\"[^\"\n\r]*\"\\)\\)*\\(\\(#+\\)\\)"
     4 font-lock-type-face t)
   '("#+$"
     0 font-lock-type-face t)
   '("^#\\(name\\|usage\\|desc\\|see\\|topics\\)\t" 
     0 font-lock-type-face t)
   '("^[ \t]*#\\(include\\|define\\|library\\).*" 
     0 font-lock-keyword-face t)
   '("^[ \t]*#\\(include\\|define\\|library\\)"
     0 font-lock-warning-face t))
  "Keykit-mode fontification")

(defun keykit-fontify ()
  "Loads the keykit-font-lock-keywords into a local version of font-lock-keywords."
  (set (make-local-variable 'font-lock-defaults)
       '(keykit-font-lock-keywords
         t t nil nil))
  (font-lock-mode 1))


;;; --------------------------- Insertion & templates  -----------------------------


(defun kk-untabify-buffer ()
  "Replace all TAB characters with spaces, except in documentation fields."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (beginning-of-line)
      (if (not (or (looking-at "#name\t")
                   (looking-at "#usage\t")
                   (looking-at "#desc\t")
                   (looking-at "#see\t")
                   (looking-at "#topics\t")))
          (untabify (point)
                    (save-excursion
                      (end-of-line)
                      (point)))  
        (untabify (save-excursion
                    (re-search-forward "\t")
                    (point))
                  (save-excursion
                    (end-of-line)
                    (point))))
      (next-line 1))))


(defun kk-insert-doc-comments()
  "Insert the comment keywords for documenting a function."
  (interactive)
  (let ((fname (kk-function-at-point))
	(fname-with-args
	 (if (save-excursion
	       (beginning-of-line)
	       (looking-at "[ \t]*class"))
	     ()
	   (progn
	     (thing-at-point-looking-at "^\\(function\\|method\\) \\(.+)\\){*")
	     (match-string 2)))))
    (beginning-of-line)
    (if (null fname)
        (insert "#name\t\n#usage\t\n")
      (insert "#name\t" fname "\n")
      (if fname-with-args
	  (insert "#usage\t" fname-with-args "\n")))
    (insert "#desc\t\n#see\t\n#topics\t\n\n")
    (forward-line -6)
    (if (null fname)
        (search-forward "#name\t")
       (search-forward "#desc\t"))))


(defun kk-insert-projector-template ()
  "..." 
  (interactive)
  (let (beg end)
    (newline)
    (insert "function -*SomeProjector*-( -*args*- ,...)\n{\n")
    (setq beg (point))
    (insert "duree = OptArgs(...)[\"duree\"]\n")
    (insert "t0 = OptArgs(...)[\"t0\"]\n")
    (insert "region = OptArgs(...)[\"region\"]\n")
    (insert "df = OptArgs(...)[\"df\"]\n\n")
    (insert "li = NewLigne(\"-*SomeName*-\", t0)\n\n")
    (insert "-*...*-\n\n")
    (insert "return(li)\n")
    (setq end (point))
    (insert "}\n\n")
    (indent-region beg end kk-indent-level)
    (search-backward "function -*")))


(defun kk-insert-df-template ()
  "..."
  (interactive)
  (let (beg end)
    (newline)
    (insert "function -*DFname*-(ph, side, dist, ch, theta, t)\n{\n")
    (setq beg (point))
    (insert "return(-*...*-)\n")
    (setq end (point))
    (insert "}\n\n")
    (indent-region beg end kk-indent-level)
    (search-backward "function -*")))


(defun kk-insert-plugin-template (plug-name)
  "..."
  (interactive "MPlug-in name: ")
  (let (beg)
    (newline)
    (insert "function " plug-name "Down(xA, yA, target, ch)\n{\n\n}\n")
    (insert "function " plug-name "Drag(xD, yD, target, ...)\n{\n")
    (setq beg (point))
    (insert "return(-*mode*-)\n")
    (indent-region beg (point) kk-indent-level)
    (insert "}\n")
    (insert "function " plug-name "Up(xB,yB,target,p1,p2,p3)\n{\n\n}\n")
    (insert "function " plug-name "Undo(...)\n{\n")
    (setq beg (point))
    (insert "return(-*undotype*-)\n")
    (indent-region beg (point) kk-indent-level)
    (insert "}\n\n")
    (search-backward (concat "function " plug-name "Down"))))


(defun kk-insert-compoplugin-template (plug-name)
  "..."
  (interactive "MPlug-in name: ")
  (let (beg)
    (newline)
    (insert  "function " plug-name "Down(box, m, i)\n{\n\n}\n")
    (insert  "function " plug-name "Drag(box, ...)\n{\n")
    (setq beg (point))
    (insert "return(-*mode*-)\n")
    (indent-region beg (point) kk-indent-level)
    (insert "}\n")
    (insert  "function " plug-name "Up(box,p1,p2,p3,p4)\n{\n\n}\n")
    (insert  "function " plug-name "Undo() {}\n")
    (search-backward (concat "function " plug-name "Down"))))


;;; ------------------ GeoMaestro development utilities -----------------------------


(defun kk-what-blah ()
  (interactive)
  (let (blah
        info
        tmp-buffer)
    (if (thing-at-point-looking-at "GMblah_[A-Za-z0-9]*")
        (progn
          (setq blah (match-string 0))
          (save-excursion
            (setq tmp-buffer (get-buffer-create " *kk-temp*"))
            (set-buffer tmp-buffer)
            (unwind-protect
                (insert-file-contents (expand-file-name "contrib/GeoMaestro/lib/franglish.k"
                                                        (kk-default-directory)))
              (search-forward "EnglishBlahBlah")
              (search-forward blah)
              (search-forward-regexp "=[\t ]")
              (looking-at ".*")
              (setq info (match-string 0))
              (kill-buffer tmp-buffer)))
          (princ info t)))))


(defun kk-add-GMblah-from-english (french-text)
  (interactive "MFrench text: ")
    (setq french-text (concat "\"" french-text "\""))
    (if (thing-at-point-looking-at "\".*\"")
        (replace-match (kk-new-GMblah french-text (match-string-no-properties 0) "F"))))


(defun kk-add-GMblah-from-french (english-text)
  (interactive "MEnglish text: ")
    (setq english-text (concat "\"" english-text "\""))
    (if (thing-at-point-looking-at "\".*\"")
        (replace-match (kk-new-GMblah (match-string-no-properties 0) english-text "E"))))


(defun kk-new-GMblah (french-text english-text end-here)
  (let (blah-num)
    (save-excursion
      (save-match-data
        (find-file-other-window (expand-file-name
                                 "contrib/GeoMaestro/lib/franglish.k" 
                                 (kk-default-directory)))
        (goto-char (point-min))
        (search-forward "#F begin Emacs")
        (search-forward-regexp "[0-9]+") 
        (setq blah-num (concat "GMblah_E" 
                               (int-to-string (1+ (string-to-int (match-string 0))))))
        (beginning-of-line)
        (insert (make-string kk-indent-level ? ) blah-num " = " french-text "\n")
        (search-forward "#E begin Emacs")
        (next-line 1)
        (beginning-of-line)
        (insert (make-string kk-indent-level ? ) blah-num " = " english-text "\n")
        (goto-char (point-min))
        (search-forward (concat "#" end-here " begin Emacs"))
        (select-window (previous-window))
        blah-num))))


;;; ---------------query sheet support (cf. query-sheet.el) ------------------------

(defvar keykit-mode-query-sheet
  (make-querysheet
   :buffer-name "*Keypath search*"
   :sources 'make-keykit-source-list
   :modes '(qsheet-search-keypath           ; -> kk-search-keypath
	    qsheet-regexp-search-keypath    ; -> kk-regexp-search-keypath
	    qsheet-regexp-search-topics)))  ; -> kk-regexp-search-topics

(qsheet-make-query-functions keykit-mode-query-sheet "kk-")

(defvar qsheet-search-keypath 
  (make-querymode
   :label "Search in Keypath for a word/function"
   :what-i-did "Searched for"
   :case t
   :interactive (lambda () (read-from-minibuffer "String: " (kk-function-at-point)))
   :get
   (lambda (string source) 
     (when (search-forward string nil t)
       (save-excursion
	 (backward-char 1)
	 (append (list :source source)
		 (list :found (kk-function-at-point))
		 (list :in-line (propertize (thing-at-point 'line)
					    'face 'font-lock-string-face))
		 (list :whatline (count-lines (point-min) (point)))))))
   :display
   (lambda (result)
     (insert (plist-get result :in-line)
	     (propertize (plist-get result :found) 
			 'face (if (string= (plist-get result :found) string) 
				   'font-lock-warning-face
				 'font-lock-keyword-face))
	     " --> "
	     (propertize (file-relative-name (plist-get result :source)
					     (kk-default-directory))
			 'mouse-face 'highlight
			 'local-map (qsheet-local-map
				     `(lambda ()
					(interactive)
					(qsheet-goto-source ,(plist-get result :source)
							    ,(plist-get result :whatline)))))
	     " (Line " (number-to-string (plist-get result :whatline)) ")\n"))))

(defvar qsheet-regexp-search-keypath  
  (make-querymode
   :label "Search in Keypath for a regexp"
   :what-i-did "Searched for regexp"
   :case t
   :interactive (lambda () (read-from-minibuffer "search for regexp: "))
   :get
   (lambda (reg source) 
     (when (re-search-forward reg nil t)
       (append (list :source source)
	       (list :found (match-string 0))
	       (list :in-line (propertize (thing-at-point 'line)
					  'face 'font-lock-string-face))
	       (list :whatline (count-lines (point-min) (point))))))
   :display
   (lambda (result)
     (insert (propertize (plist-get result :in-line)
			 'face 'font-lock-string-face)
	     (propertize (plist-get result :found) 
			 'face 'bold)
	     " --> "
	     (propertize (file-relative-name (plist-get result :source)
					     (kk-default-directory))
			 'mouse-face 'highlight
			 'local-map (qsheet-local-map
				     `(lambda ()
					(interactive)
					(qsheet-goto-source ,(plist-get result :source)
							    ,(plist-get result :whatline)))))
	     " (Line " (number-to-string (plist-get result :whatline)) ")\n"))))

(defvar qsheet-regexp-search-topics 
  (make-querymode
   :label "Search topics for a regexp"
   :what-i-did "Searched topics matching"
   :case t
   :interactive (lambda () (read-from-minibuffer "search topics for regexp: "))
   :get
   (lambda (reg source) 
     (when (re-search-forward (concat "^#topics[ \t]*.*" reg) nil t)
       (append (list :source source)
	       (list :topic-found (propertize (thing-at-point 'word)
					      'face 'font-lock-keyword-face))
	       (list :found
		     (save-excursion
		       (forward-line 1)
		       (skip-chars-forward " \t\n\r")
		       (cond
			((looking-at
			  "^\\(function\\|class\\)[ \t]+\\([a-zA-Z0-9_]*\\)[ \t]*(*")
			 (match-string 2))
			((looking-at
			  "^[ \t]*method[ \t]+\\([a-zA-Z0-9_]*\\)[ \t]*(")
			 (let ((method (match-string 1)))
			   (concat
			    (save-excursion
			      (re-search-backward "^[ \t]*class[ \t]+\\([a-zA-Z0-9_]*\\)")
			      (match-string 1))
			    "." method)))
			(t
			 "??"))))
	       (list :whatline (count-lines (point-min) (point))))))
   :display
   (lambda (result)
     (insert (propertize (plist-get result :found) 
			 'face 'bold
			 'mouse-face 'highlight
			 'local-map (qsheet-local-map 'kk-man-at-point))
	     " --> "
	     (propertize (file-relative-name (plist-get result :source)
					     (kk-default-directory))
			 'mouse-face 'highlight
			 'local-map (qsheet-local-map
				     `(lambda () 
					(interactive)
					(qsheet-goto-source ,(plist-get result :source)
							    ,(plist-get result :whatline)))))
	     " (Line " (number-to-string (plist-get result :whatline)) ") "
	     (plist-get result :topic-found) "\n"))))


;;; ---------------------- On-line help browser -----------------------------


(defmacro kk-with-temp-buffer-reading (file &rest BODY)
  `(with-temp-buffer
     (if (and (stringp ,file)
	      (file-readable-p ,file))
	 (progn
	   (insert-file-contents ,file)
	   ,@BODY))))


(defun kk-man-at-point (&optional class)
  (interactive)
  (if (and class
	   (equal (substring (kk-function-at-point) 0 1) "."))
      (kk-man (concat class (kk-function-at-point)))
    (kk-man (kk-function-at-point))))


(defun kk-man-method-at-point (class)
  (interactive)
  (kk-man (concat class "." (kk-function-at-point))))


(defun kk-search-topic-at-point ()
  (interactive)
  (kk-regexp-search-topics (thing-at-point 'word)))


(defun make-keykit-source-list ()
  (let ((dirs (kk-keypath))
        (ldir ()))
    (while dirs
      (setq ldir 
            (append ldir
                    (directory-files (car dirs) t "\\(\\.k$\\|\\.tyk$\\)")))
      (setq dirs (cdr dirs)))
    ldir))


(defun kk-source-of-function (function-name dirs)
  "Go through all the keylib.k in the Keypath and returns the name of
the file containing the code for the function given as argument, or nil
if it is not found."
  (interactive)
  (let (source-filename)
    (dolist (dir dirs)
      (kk-with-temp-buffer-reading (concat dir "keylib.k")
	 (if (search-forward-regexp (concat "\.k " function-name "[ \t]*$") nil t)
	     (progn
	       (search-backward ".k")
	       (setq source-filename 
		     (concat dir (thing-at-point 'filename)))))))
    source-filename))


(defun kk-get-function-description (function-name dirs)
  "...used by kk-man"
  (let* ((description nil)
	 (function-or-class (car (split-string function-name "\\." t)))
	 (function-or-method (or (cadr (split-string function-name "\\." t))
				 function-or-class)))
    (kk-with-temp-buffer-reading (kk-source-of-function function-or-class dirs)
     (if (search-forward-regexp (concat "#name[\t ]*" function-or-method "[ \t]*$") nil t)
	 (progn
	   (beginning-of-line)
	   (while (looking-at "#[a-z]*")
	     (setq description 
		   (cons (list
			  (match-string 0)
			  (substring (thing-at-point 'line) (1+ (length (match-string 0)))))
			 description))
	     (forward-line 1))))
     
     (skip-chars-forward " \t\n\r")
     (if (looking-at "^class[ \t]")
	 (let ((methods ()))
	   (while (and (search-forward-regexp "#name[\t ]*" nil t)
		       (progn
			 (beginning-of-line)
			 (while (looking-at "^#")
			   (forward-line 1))
			 (skip-chars-forward " \t\n\r")
			 (looking-at "^method[ \t]+\\([a-zA-Z0-9_]+\\)[ \t]*(")))
	     (setq methods (cons (match-string 1) methods)))
	   (if methods
	       (setq description (cons (list "#methods" methods) description))))))
    (nreverse description)))


(defun kk-visit-code (function-name)
  "Display the source code for the function given as argument in a specific buffer."
  (interactive "P")
  (if (equal function-name nil)
      (setq function-name (read-from-minibuffer
                           "Keykit function/class: "
                           (kk-function-at-point))))
  (let (source pos-point)
    (if (setq pos-point (string-match "\\." function-name))
        (let ((class-name (substring function-name 0 pos-point))
              (method-name (substring function-name (1+ pos-point))))

	  ;; pourrait être considérablement amélioré (par recherche des méthodes héritées):
	  (if (string-match "\\$" class-name)
	      (setq class-name (save-excursion (re-search-backward "^class[ \t]+\\([^ {]*\\)")
					       (match-string 1))))

          (if (null (setq source (kk-source-of-function class-name (kk-keypath))))
              (format "Couldn't find this class (%s ?) code, sorry" class-name)

	    (if (string= (buffer-name) (file-name-nondirectory source))
		(select-window (display-buffer (current-buffer) t))
	      (find-file-other-window source))

            (goto-char (point-min))
            (search-forward-regexp (concat "class[\t ]*" class-name "[\t {$]"))
            (search-forward-regexp (concat "method[\t ]*" method-name "[\t ($]")))))
    (if (null (setq source (kk-source-of-function function-name (kk-keypath))))
        (format "Couldn't find this function (%s ?) code, sorry" function-name)
      (find-file-other-window source)
      (goto-char (point-min))
      (search-forward-regexp (concat "\\(function\\|class\\)[\t ]*" 
				     function-name "[\t ({]")))))


(defun kk-function-at-point ()
  (if (thing-at-point-looking-at "[$a-zA-Z1-90_.]+")
      (match-string 0)
    ""))


(defun kk-man (function-name)
  "Display the documentation for the function given as argument in a specific buffer.
If no documentation available, return nil"
  (interactive "P")
  (let ((desc-list (kk-get-function-description 
		    (or function-name 
			(setq function-name (read-from-minibuffer
					     "Keykit function/class: "
					     (kk-function-at-point))))
			(kk-keypath)))
	(need-a-n t))
    (if (null desc-list)
        (prin1 (concat "This function (" function-name "?) is not documented, sorry"))
      (pop-to-buffer (get-buffer-create "*Keykit help*"))
      (setq buffer-read-only nil)
      (erase-buffer)
      (qsheet-insert-button "Visit code" `(lambda () 
					    (interactive) 
					    (kk-visit-code ,function-name)))
      (insert "   ")
      (qsheet-insert-button "(bury)" 'bury-buffer)
      (insert "\n\n")
      (if (cadr (split-string function-name "\\." t))
	  (insert (propertize "Method of class " 'face 'bold)
		  (propertize (setq function-name
				    (car (split-string function-name "\\." t)))
			      'face 'bold
			      'mouse-face 'highlight
			      'local-map (qsheet-local-map 'kk-man-at-point))
		  "\n"))

      (dolist (item desc-list)
        (cond
         ((equal "#name" (car item))
          (insert #("Name: " 0 5 (face bold)) (cadr item)))
         ((equal "#usage" (car item))
          (insert #("Usage: " 0 6 (face bold)) (cadr item) "\n")
	  (setq need-a-n nil))
         ((equal "#desc" (car item))
	  (if need-a-n (progn (insert ?\n) (setq need-a-n nil)))
          (if (cadr item)
	      (insert " " (cadr item))
	    (insert "\n")))
	 ((equal "#see" (car item))
          (insert #("\nSee: " 0 4 (face bold))
		  (kk-make-clickable-string (split-string (cadr item) "[;, \t\n\r()]+" t)
                                            `(lambda ()
					       (interactive)
					       (kk-man-at-point ,function-name)))
		  ?\n))
         ((equal "#topics" (car item))
          (insert #("\nRelated topics: " 0 15 (face bold))
		  (kk-make-clickable-string (split-string (cadr item) "[;, \t\n\r]+" t) 
                                            'kk-search-topic-at-point)))
	 ((equal "#methods" (car item))
	  (insert (propertize "\n\nDocumented methods: " 'face 'bold)
		  (kk-make-clickable-string (cadr item)
					    `(lambda ()
					       (interactive)
					       (kk-man-method-at-point ,function-name))
					    t)))))
      (goto-char (point-min))
      (end-of-line)
      (setq buffer-read-only t))))


(defun kk-make-clickable-string (keywords-list action &optional vertical)
  (let ((c-string ""))
    (dolist (keyword (delete "" keywords-list))
      (setq c-string
	    (concat c-string (if vertical "\n" " ") 
		    (propertize keyword
				'mouse-face 'highlight
				'local-map (qsheet-local-map action)))))
    (concat c-string " ")))



;;; --------------------------- HTML documentation -----------------------------

(defvar kk-HTML-topics ()
   "List of topics")

(defvar kk-HTML-buffers '()
  "Buffers containing doc pages")

(defun kk-HTML-doc-header (title)
  (concat
   "<html>\n"
   "<head>\n"
   "    <title>" title "</title>\n"
   "</head>\n"
   (if (equal kk-HTML-background-picture "")
       "<body>"
     (concat
      "<body background=\""
      kk-HTML-background-picture
      "\">"))))
  
(defun kk-HTML-doc-footer()
  "</body></html>")
   
(defvar kk-HTML-f-URLs (makehash 'equal)
  "hash table associating its link in the docs to each documented function")

(defvar kk-HTML-f-topics (makehash 'equal)
  "")

(defvar kk-HTML-f-descriptions (makehash 'equal)
  "")

(defvar kk-HTML-m-descriptions (makehash 'equal)
  "")

;;;;;;; debug utilities:
(defun kk-check-f-URLs ()
  (interactive)
  (pop-to-buffer (get-buffer-create "*keykit-mode debugging*"))
  (erase-buffer)
  (maphash (lambda (key val) (insert key ?\n val ?\n ?\n)) kk-HTML-f-URLs))
(defun kk-check-f-topics ()
  (interactive)
  (pop-to-buffer (get-buffer-create "*keykit-mode debugging*"))
  (erase-buffer)
  (maphash (lambda (key val) 
	     (insert key ?\n)
	     (princ val (current-buffer))
	     (insert ?\n ?\n)) kk-HTML-f-topics))
(defun kk-check-f-descriptions ()
  (interactive)
  (pop-to-buffer (get-buffer-create "*keykit-mode debugging*"))
  (erase-buffer)
  (maphash (lambda (key val) 
	     (insert key ?\n)
	     (princ val (current-buffer))
	     (insert ?\n ?\n))
	   kk-HTML-f-descriptions))
(defun kk-check-m-descriptions ()
  (interactive)
  (pop-to-buffer (get-buffer-create "*keykit-mode debugging*"))
  (erase-buffer)
  (maphash (lambda (key val) 
	     (insert key ?\n)
	     (princ val (current-buffer))
	     (insert ?\n ?\n))
	   kk-HTML-m-descriptions))
;;;;;;;;;;;;;;;;;;
;;;;;;;
;;


(defun kk-HTML-color (emacs-color)
  (concat "#" (mapconcat '(lambda (nc) (kk-int255-to-hexa (/ nc 256)))
			 (color-values emacs-color) "")))

(defun kk-int255-to-hexa (int)
  (let* ((big (/ int 16))
	 (small (- int (* 16 big))))
   (concat (kk-int15-to-hexa big) (kk-int15-to-hexa small))))

(defun kk-int15-to-hexa (int)
  (if (< int 10)
      (number-to-string int)
    (string (+ ?A (- int 10))))) 


(defun kk-make-HTML-man ()
  "Create HTML documentation from the #desc fields from all code in the
part of the Keypath defined in kk-HTML-doc-keypath. 
It is written in the kk-HTML-directory: see the \"customize\" item in the 
main Keykit menu."
  (interactive)
  (if (yes-or-no-p "This will override the current documentation. Continue ? ")
      (let ((lib-num 0)
            (f-list ())
            f-URL topic-page)
        (save-excursion
          (if (equal "" kk-HTML-doc-directory)
              (setq kk-HTML-doc-directory kk-default-directory))
          (clrhash kk-HTML-f-URLs)
          (clrhash kk-HTML-f-topics)
	  (setq kk-HTML-topics ())
          (setq kk-HTML-buffers ())
	  ;;
	  ;; index page:
	  ;;
	  (with-temp-buffer
	    ;;
	    ;; (index page) header:
	    ;;
	    (insert (kk-HTML-doc-header kk-HTML-title)
		    "<H1>" kk-HTML-title "</H1>"
		    "<I>This documentation has been created by <A HREF=\""
		    (file-name-directory GeoMaestro-manual-URL)
		    "GMemacs.html\">keykit-mode for Emacs</A>"
		    "<BR> on " (current-time-string) "</I><BR><BR>")
	    ;;
	    ;; (index page) links to libraries:
	    ;;
	    (insert "<H2>Libraries: </H2><UL>")
	    (dolist (dir (kk-HTML-doc-keypath))
	      (setq lib-num (1+ lib-num))
	      (kk-make-HTML-man-for-library dir kk-HTML-doc-chunk
					    (expand-file-name 
                                             (concat  "lib" (number-to-string lib-num) "_") 
                                             (kk-HTML-directory)))
	      (insert "<LI> <H2><A HREF=\""
                      (concat (kk-HTML-directory) "lib" (number-to-string lib-num) "_all.html")
		      "\">"
		      (file-relative-name dir (kk-HTML-doc-directory))
		      "</A></H2>"))
	    (insert "</UL>")
	    ;;
	    ;; (index page) links to topics:
	    ;;
	    (insert "<BR><H2>Topics: <UL></H2>")
	    (dolist (topic (sort kk-HTML-topics 'string<))
	      (setq topic-page (expand-file-name (concat "topic_" topic ".html")
                                                 (kk-HTML-directory)))
	      (insert "<LI><A HREF=\""
		      (file-relative-name topic-page (kk-HTML-directory))
		      "\">" topic "</A>")
	      (kk-HTML-make-topic-page topic-page topic))
	    (insert "</UL>")
	    ;;
	    ;; (index page) links to alphabetically sorted functions:
	    ;;
	    (insert "<BR><H2>Documented functions & classes: </H2>"
		    "<I>Unix-style alphabetic order</I><BR><BR>")
	    (maphash (lambda (f url)
		       (setq f-list (append f-list (list (list f url)))))
		     kk-HTML-f-URLs)
	    (setq f-list (sort f-list (lambda (l1 l2) (string< (car l1) (car l2)))))
	    (dolist (f-url f-list)
	      (if (null (string-match "\\." (car f-url)))
		  (insert "- <A HREF=\"" (cadr f-url) "\">" (car f-url) "</A>")))
	    ;;
	    ;; (index page) links to alphabetically sorted methods:
	    ;;
	    (insert "<BR><BR><BR><H2>Documented methods: </H2>"
		    "<I>Unix-style alphabetic order</I><BR><BR>")
	    (dolist (f-url f-list) 
	      (if (string-match "\\." (car f-url))
		  (insert "- <A HREF=\"" (cadr f-url) "\">" (car f-url) "</A>")))
	    ;;
	    ;; (index page) back-to-where-we-came-from link:
	    ;;
	    (if (not (equal "" kk-HTML-external-link))
		(insert "<BR><BR><BR><BR><A HREF=\"" kk-HTML-external-link "\">"
			(if (not (equal "" kk-HTML-external-link-label))
			    kk-HTML-external-link-label
			  "back")
			"</A><BR><BR><BR>"))
	    ;;
	    ;; footer:
	    ;;
	    (insert (kk-HTML-doc-footer))
	    (write-file (expand-file-name "index.html" (kk-HTML-directory))))
	  
          ;; +
          ;; second pass into the lib/class htmls, to link all -f#...#f-
          ;;
          (dolist (this-buffer kk-HTML-buffers)
            (set-buffer this-buffer)
            (goto-char (point-min))
	    (let ((maybe-class (save-excursion
				 (if (re-search-forward "class \\([a-zA-Z0-9_]*\\)" nil t)
				     (match-string 1)
				   ""))))
	      (while (re-search-forward "-f#\\([._a-zA-Z0-9]*\\)#f-" nil t)
		(let* ((keyword (match-string 1))
		       (full-keyword (if (equal (substring keyword 0 1) ".")
					 (concat maybe-class keyword)
				       keyword)))
		  (if (setq f-URL (gethash full-keyword kk-HTML-f-URLs))
		      (replace-match (concat
				      "<A HREF=\"" (gethash full-keyword kk-HTML-f-URLs) "\">"
				      keyword "</A>")
				     nil nil)
		    (replace-match keyword nil nil))))
	      (save-buffer)
 	      (if (file-exists-p (concat buffer-file-name "~"))
 		  (delete-file (concat buffer-file-name "~")))
	      (kill-buffer this-buffer)))

          ;; +
          ;; scanning the dependent HTML
          ;;
	  (if (yes-or-no-p "Update dependent HTML files ? ")
	      (kk-HTML-scan-and-update-all)) 
	  ;;
          ;; done.
          (message-box "HTML documentation is done !")))))


(defun kk-HTML-scan-and-update-all ()
  (interactive)
  (dolist (dir kk-HTML-dependent-pages)
    (dolist (html-file (directory-files dir t "\\.html*$"))
      (if (get-file-buffer html-file)
	  (save-excursion
	    (set-buffer (get-file-buffer html-file))
	    (kk-scan-buffer-and-update html-file))
	(kk-with-temp-buffer-reading html-file
          (kk-scan-buffer-and-update html-file))))))


(defun kk-scan-buffer-and-update (&optional html-file)
  (interactive)
  (if (equal 0 (hash-table-count kk-HTML-f-URLs))
      (message "the database seems empty... stopped")
    (if (null html-file)
	;; on scanne tous les HTML ouverts
	(dolist (buffer (buffer-list))
	  (let ((file (buffer-file-name buffer)))
	    (if (and (stringp file)
		     (string-match "\\.html*$" file)
		     (member (file-name-directory file) kk-HTML-dependent-pages))
		(save-excursion
		  (set-buffer buffer)
		  (kk-scan-buffer-and-update file)))))
      ;; on scanne seulement html-file
      (let ((num 0))
	(save-excursion               
	  (goto-char (point-min))
	  (while (search-forward kk-HTML-electric-link-comment nil t)
	    (if (and (or (looking-at "[ ]*<A HREF=\\([^<>]*\\)>\\([._a-zA-Z0-9]+\\)\\((?)?\\)</A>")
			 (search-forward-regexp "\\(\\([._a-zA-Z0-9]+\\)\\)\\((?)?\\)" nil t))
		     (gethash (match-string 2) kk-HTML-f-URLs))
		(progn
		  (replace-match (concat 
				  "<A HREF=\""
				  (file-relative-name 
				   (expand-file-name (gethash (match-string 2) kk-HTML-f-URLs)
						     (kk-HTML-directory))
				   (file-name-directory html-file))
				  "\">" (match-string 2) (match-string 3) "</A>")
				 nil nil)
		  (setq num (1+ num)))))
	  (if (> num 0)
	      (progn
		(message 
		 (concat "Updated " (number-to-string num) " occurence(s) in " html-file))
		(write-file html-file))))))))


(defun kk-make-HTML-man-for-library (lib max-per-page HTML-root-name)
  ""
  (let ((tmp-kl-buffr (get-buffer-create "*kk-make-HTML-man-for-library*"))
        (html-buffer (get-buffer-create "*HTML-temp*"))
        (html-lib-buffer (get-buffer-create "*HTML-temp2*"))
        f-candidat links
        f-URL
        not-the-end
        (per-page 0)
        (page-num 0))
    (kk-get-all-descriptions-in-library lib)
    (save-excursion
      (set-buffer html-lib-buffer)
      (erase-buffer)
      (insert-file-contents (concat lib "keylib.k"))
      (set-buffer tmp-kl-buffr)
      (erase-buffer)
      (insert-file-contents (concat lib "keylib.k"))
      (sort-fields 3 1 (save-excursion (goto-char (point-max)) (point)))
      (goto-char (point-min))
      (while (progn
               (setq not-the-end (search-forward-regexp "[a-zA-Z0-9_]+$" nil t))
               (setq f-candidat (match-string 0))
               ;;
               ;; completing/writing a lib*.html page:
               ;;
               (if (or (= per-page max-per-page)
                       (null not-the-end))
                   (progn
                     (setq per-page 0)
                     (setq page-num (1+ page-num))
                     (save-excursion
                       (set-buffer html-buffer)
                       (goto-char (point-min))
                       (insert (kk-HTML-doc-header 
				(concat lib 
					" : page " (int-to-string page-num))))
                       (setq links 
			     (concat
			      "<TABLE BORDER=\"0\" CELLSPACING=\"0\">"
			      "<TR><TD BGCOLOR=\""
			      (kk-HTML-color kk-HTML-background-color)
			      "\" WIDTH=\"30\"> </TD>"
			      "<TD BGCOLOR=\""
			      (kk-HTML-color kk-HTML-background-color) "\"><BR>"
			      (if (> page-num 1)
				  (concat "&lt; <A HREF=\""
					  (file-relative-name
					   (concat HTML-root-name 
						   (int-to-string (- page-num 1)) 
						   ".html")
					   (kk-HTML-directory))
					  "\">previous page</A> ]")
				"")
			      (if not-the-end
				  (concat "[ <A HREF=\""
					  (file-relative-name
					   (concat HTML-root-name 
						   (int-to-string (1+ page-num)) 
						   ".html")
					   (kk-HTML-directory))
					  "\">next page</A> &gt;")
				"")
			      "<CODE>  [</CODE>"
			      "<A HREF=\"index.html\">back</A>"
			      "<CODE>]  [</CODE>"
			      "<A HREF=\""
			      (file-relative-name
			       (concat HTML-root-name "all.html")
			       (kk-HTML-directory))
			      "\">back to #library listing</A>"
			      "<CODE>] </CODE>"
			      "<BR><BR>"
			      "</TD><TD BGCOLOR=\""
			      (kk-HTML-color kk-HTML-background-color)
			      "\" WIDTH=\"30\"> </TD>"
			      "</TR> </TABLE>"))
                       (insert links)
                       (goto-char (point-max))
                       (insert "<HR>")
                       (insert links)
                       (insert (kk-HTML-doc-footer))
                       (write-file (concat HTML-root-name (int-to-string page-num) ".html"))
                       (setq kk-HTML-buffers (cons (current-buffer) kk-HTML-buffers))
                       (setq html-buffer (get-buffer-create "*HTML-temp*"))))
                 not-the-end)
               not-the-end)
        ;;
        ;; collecting/writing functions documentations:
        ;; (up to max-per-page per html page)
        ;;
        (if (kk-HTML-man f-candidat html-buffer)
            (progn
              (setq per-page (1+ per-page))
              (save-excursion
                (set-buffer html-lib-buffer)
                (goto-char (point-min))
                (search-forward-regexp (concat " " f-candidat "$"))
                (insert "</A>")
                (search-backward " ")
                (forward-char 1) 
                (setq f-URL (concat 
                             (file-relative-name
                              (concat HTML-root-name (int-to-string (1+ page-num)) ".html")
                              (kk-HTML-directory))
                             "#" f-candidat))
                (setf (gethash f-candidat kk-HTML-f-URLs) f-URL)
                (insert "<A HREF=\"" f-URL "\">")))))
      (set-buffer html-lib-buffer)
      (goto-char (point-min))
      (insert (kk-HTML-doc-header (concat "Functions in: "
                                          (file-relative-name
                                           lib 
                                           (kk-HTML-doc-directory)))))
      (insert "<H2>Functions defined in:<BR> "
              (file-relative-name
               lib 
               (kk-HTML-doc-directory)) 
              "</H2>")
      (if (> page-num 0)
          (insert "<H2>Proceed to an <A HREF=\""
                  (file-relative-name
                   (concat HTML-root-name "1.html")
                   (kk-HTML-directory))
                  "\">alphabetic listing</A> or click in the following:</H2>")
        (insert "<H2>None of these functions is documented, sorry</H2>"))
      (while (search-forward "\n" nil t)
	(replace-match "\n<BR>" nil nil))
      (insert "<BR><BR><A HREF=\"index.html\">back</A><BR>")
      (insert (kk-HTML-doc-footer))
      (write-file (concat HTML-root-name "all.html"))
      (kill-buffer (current-buffer))
      (kill-buffer html-buffer)
      (kill-buffer tmp-kl-buffr))))


(defun kk-HTML-make-topic-page (filename topic)
  "Write index page for topic."
  (save-excursion
    (let ((f-list (gethash topic kk-HTML-f-topics))
          (tmp-buffer (get-buffer-create "*kk-HTML-topic*")))
      (setq f-list (sort f-list 'string<))
      (set-buffer tmp-buffer)
      (insert (kk-HTML-doc-header (concat "Functions with topic: " topic)))
      (insert "<H1>Functions with topic: " topic "</H1>")
      (dolist (topic-link kk-HTML-topics-links)
	(if (equal (car topic-link) topic)
	    (insert "<H3>" kk-HTML-topic-link-label 
		    "<A HREF=\"" (cadr topic-link) "\">" (cadr topic-link) "</A></H3>")))
      (insert "<A HREF=\"index.html\">back</A><BR><BR><BR>")
      (while f-list
        (insert "<A HREF=\""
                (gethash (car f-list) kk-HTML-f-URLs)
                "\">"
                (car f-list)
                "</A><BR>")
        (setq f-list (cdr f-list)))
      (insert "<BR><BR><A HREF=\"index.html\">back</A><BR>")
      (insert (kk-HTML-doc-footer))
      (write-file filename)
      (kill-buffer (current-buffer)))))



(defun kk-HTML-man (function-name buffer)
  "Insert the documentation for the function given as argument in HTML format,
in buffer, then returns t. If no documentation available \(in `kk-HTML-f-descriptions'),
 returns nil"
  (let (end
        name)
    (save-excursion
      (set-buffer buffer)
      (if (null (gethash function-name kk-HTML-f-descriptions))
          nil
        (dolist (item (gethash function-name kk-HTML-f-descriptions))
          (cond
           ((equal "#name" (car item))
            (insert "<HR><A NAME=\""   
                    (setq name (car (split-string (cadr item) "[ \t\n]+" t)))
                    "\"></A>\n" )
            (insert "<PRE><B>" 
                    (car (split-string (cadr item) "[ \t\n]+" t)) 
                    "</B><BR>\n" ))
           ((equal "#usage" (car item))
	    (forward-line -1)
	    (beginning-of-line)
	    (kill-line)
            (insert "<PRE><B>" 
                    (car (split-string (cadr item) "\n" t)) 
                    "</B><BR>\n" ))
           ((equal "#desc" (car item))
            (if (null (cadr item))
                (insert "\n")            
              (insert "  "  (cadr item))))   
           ((equal "#see" (car item))
            (insert "</PRE><CODE><U>See</U>:\n") 
            (mapcar (lambda (s)
                      (if (not (equal s ""))
                          (insert " -f#" s "#f-\n")))
                    (split-string (cadr item) "[, ;\t\n]" t))
            (insert "<CODE><PRE>"))
           ((equal "#topics" (car item))
            (insert "\n<U>Related topics</U>:")
            (mapcar (lambda (s)
                      (if (equal s "")
                          nil
                        (insert " <A HREF=\""
                                 (concat "topic_" s ".html")
                                "\">" s "</A>")
                        (setf (gethash s kk-HTML-f-topics) 
                              (union (gethash s kk-HTML-f-topics) (list name) :test #'equal))
                        (setq kk-HTML-topics (union kk-HTML-topics (list s) :test #'equal))))
                    (split-string (cadr item) "[, ;\t\n]" t))
            (insert "\n"))
	   ((equal "-methods-" (car item))
	    (kk-HTML-class-page function-name)
            (insert "\n<A HREF=\"class_" 
		    function-name ".html\">See documented methods</A>\n"))))
        (insert "</PRE><BR>\n")
        t))))



(defun kk-HTML-class-page (function-name)
  "Insert the documentation for the class given as argument in HTML format,
in buffer"
  (let (end name)
    (save-excursion
      (set-buffer (get-buffer-create "*HTML-temp-class*"))
      (insert (kk-HTML-doc-header (concat "class " function-name)))
      (dolist (item (gethash function-name kk-HTML-f-descriptions))
	(cond
	 ((equal "#name" (car item))
	  (insert "-f#" function-name "#f- [<A HREF=\"index.html\">main</A>]<HR>\n"
		  "<H2>class " 
		  (car (split-string (cadr item) "\n" t)) 
		  "</H2><PRE>\n" ))
	 ((equal "#desc" (car item))
	  (if (null (cadr item))
	      (insert "\n")            
	    (insert "  "  (cadr item))))   
	 ((equal "#see" (car item))
	  (insert "</PRE><CODE><U>See</U>:\n") 
	  (mapcar (lambda (s)
		    (if (not (equal s ""))
			(insert " -f#" s "#f-\n")))
		  (split-string (cadr item) "[, ;\t\n]" t))
	  (insert "<CODE><PRE>"))
	 ((equal "#topics" (car item))
	  (insert "\n<U>Related topics</U>:")
	  (mapcar (lambda (s)
		    (if (equal s "")
			nil
		      (insert " <A HREF=\""
			       (concat "topic_" s ".html")
                               "\">" s "</A>")
 		      (setq kk-HTML-topics (union kk-HTML-topics (list s) :test #'equal))))
		  (split-string (cadr item) "[, ;\t\n]" t))
	  (insert "\n"))))

	  (insert "\n<U>Documented methods</U>: ")
	  (maphash (lambda (class-method description) 
		     (if (equal function-name
				(car (split-string class-method "\\." t)))
			 (insert "<br><A HREF=\"#"
				 (cadr (split-string class-method "\\." t))
				 "\">"
				 (cadr (split-string class-method "\\." t))
				 "</A> ")))
		   kk-HTML-m-descriptions)
	  (insert "\n\n")

	  (maphash (lambda (class-method description)
		     (if (equal function-name
				(car (split-string class-method "\\." t)))
			 (dolist (item description)
			   (cond
			    ((equal "#name" (car item))
			     (insert "<A NAME=\""
				     (cadr (split-string class-method "\\." t))
				     "\"></A>\n" )
			     (puthash class-method
				      (concat
				       "class_" function-name ".html"
				       "#" (cadr (split-string class-method "\\." t)))
				      kk-HTML-f-URLs))
			    ((equal "#usage" (car item))
			     (insert "<PRE><B>method ."
				     (car (split-string (cadr item) "\n" t)) 
				     "</B><BR>\n" ))
			    ((equal "#desc" (car item))
			     (if (null (cadr item))
				 (insert "\n")
			       (insert "  "  (cadr item))))
			    ((equal "#see" (car item))
			     (insert "</PRE><CODE><U>See</U>:\n")
			     (mapcar (lambda (s)
				       (if (not (equal s ""))
					   (insert " -f#" s "#f-\n")))
				     (split-string (cadr item) "[, ;\t\n]" t))
			     (insert "<CODE><PRE>"))
			    ((equal "#topics" (car item))
			     (insert "\n<U>Related topics</U>:")
			     (mapcar (lambda (s)
				       (if (equal s "")
					   nil
					 (insert " <A HREF=\""
						  (concat "topic_" s ".html")
						 "\">" s "</A>")
					 (setf (gethash s kk-HTML-f-topics)
					       (union (gethash s kk-HTML-f-topics) (list class-method) :test #'equal))
					 (setq kk-HTML-topics (union kk-HTML-topics (list s) :test #'equal))))
				     (split-string (cadr item) "[, ;\t\n]" t))
			     (insert "\n"))))))
		   kk-HTML-m-descriptions)

      (insert "</PRE><BR>\n"
	      "<HR>-f#" function-name "#f- [<A HREF=\"index.html\">main</A>]<br>"
	      (kk-HTML-doc-footer))
      (write-file (expand-file-name (concat "class_" function-name ".html") (kk-HTML-directory)))
      (setq kk-HTML-buffers (cons (current-buffer) kk-HTML-buffers)))))



(defun kk-get-all-descriptions-in-library (lib)
  "Put all documented functions \(names and descriptions) in directory lib 
within the hash table `kk-HTML-f-descriptions'. For internal use only \(called by
`kk-make-HTML-man-for-library')."
  (clrhash kk-HTML-f-descriptions)
  (clrhash kk-HTML-m-descriptions)
  (save-excursion
    (dolist (source-filename (directory-files lib t "\\.k$"))     
      (kk-with-temp-buffer-reading source-filename
	(let ((current-class "")
	      (current-class-description ()))
	  (while (search-forward-regexp (concat "^#name[\t ]*\\(.*\\)[ \t]*$") nil t)
	    (let ((function-name (match-string 1))
		  description)
	      (beginning-of-line)
	      (while (looking-at "#[a-z]*")
		(setq description
		      (cons
		       (list
			(match-string 0)
			(replace-regexp-in-string 
			 ">" "&gt;"
			 (replace-regexp-in-string 
			  "<" "&lt;"
			  (substring (thing-at-point 'line) (1+ (length (match-string 0)))))))
		       description))
		(forward-line 1))
	      (skip-chars-forward " \t\n\r")
	      (cond
	       ((looking-at "function")
		(puthash function-name (nreverse description) kk-HTML-f-descriptions))
	       ((looking-at "class")
		(setq current-class-description (copy-list description))		
		(puthash function-name (nreverse description) kk-HTML-f-descriptions)
		(setq current-class function-name))
	       ((looking-at "method")
 		(if current-class-description 
 		    (puthash current-class
 			     (nreverse (cons '("-methods-") current-class-description))
			     kk-HTML-f-descriptions))
 		(setq current-class-description ())
		(puthash (concat current-class "." function-name)
			 (nreverse description) kk-HTML-m-descriptions))))))))))


;;; --------------------------- End  -----------------------------

(provide 'keykit-mode)

;;; keykit-mode.el ends here









