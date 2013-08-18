;;; -*- auto-recompile: t -*-

;;; keykit-mode.el --- editing and evaluating KeyKit code within Emacs

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

;; 
;; Provides indentation and fontification (based on a hack of c-mode.el adapted
;; for KeyKit)
;;
;; + Can generate HTML documentation from specific comments in the sources. 
;;
;; + Handles lowkey invocations
;;
;; + Also able to use a TCP/IP session in order to have a full-featured KeyKit console 
;; directly within an Emacs telnet buffer, and to evaluate KeyKit code directly from
;; the source file.
;; (this requires from KeyKit side the code in GeoMaestro/lib/tcpip.k)
;;
;;
;; WARNING: this is a beta release, and certainly not a very good elisp code
;; feel free to comment and complain, and help me make it better...
;;
;; ... some code here is useless because it comes from the original c-mode and
;; I didn't remove it by fear of breaking something. Quite ugly, sorry...
;;
;;                                                      Stef (hepta@zogotounga.net)
;;
;; support Font Lock and Outline minor modes
;;
;; last version and more detailed documentation at:
;; http://www.zogotounga.net/GM/GMemacs.html


;; last modified September 6, 2001
;;; --------------------------------------------------------------------------------

;;; Code:

(require 'thingatpt)
(require 'kk_utils)


;;; --------------------------- Customization Macros  -----------------------------


(defgroup keykit nil
  "Major mode for editing KeyKit code."
  :group 'languages
  :prefix "kk-")

(defgroup keykit-doc nil
  "HTML auto-documentation of KeyKit libraries."
  :group 'keykit
  :prefix "kk-HTML-")

(defgroup keykit-util nil
  "Interfacing Key and Lowkey with Emacs."
  :group 'keykit
  :prefix "kk-")


(defcustom kk-default-directory "c:/keykit/"
  "KeyKit root directory"
  :type 'directory
  :group 'keykit)

(defcustom kk-lowkey-executable "lowkey.exe"
  "Valid name for lowkey executable (with or without full path)"
  :type 'file
  :group 'keykit-util)

(defcustom kk-keypath 
  '("c:/keykit/contrib/GeoMaestro/lib/"
    "c:/keykit/contrib/GeoMaestro/userlib/"
    "c:/keykit/lib/"
    "c:/keykit/liblocal/")
  "...a list of directories constituting Keypath"
  :type  '(repeat directory)
  :group 'keykit)

(defcustom kk-HTML-directory "c:/keykit/doc/"
  "Directory where auto HTML documentation is written"
  :type 'directory
  :group 'keykit-doc)

(defcustom kk-HTML-title "Library of KeyKit functions"
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

(defcustom kk-HTML-background-picture ""
  "Optional background picture for the HTML documents. 
The name can be absolute or relative to the kk-HTML-directory."
  :type 'string
  :group 'keykit-doc)

(defcustom kk-HTML-doc-chunk  20
  "Number of functions described per page in the HTML documentation"
  :type 'integer
  :group 'keykit-doc)

(defcustom kk-HTML-doc-directory ""
  "KeyKit root directory as for HTML documentation. 
If \"\", it is defaulted as kk-default-directory. This is so that we can document an instance of KeyKit installation while working on another one. If your kk-HTML-doc-keypath is a subset of kk-HTML-keypath, don't worry about this. "
  :type 'directory
  :group 'keykit-doc)

(defcustom kk-HTML-doc-keypath 
  '("c:/keykit/contrib/GeoMaestro/lib/"
    "c:/keykit/contrib/GeoMaestro/userlib/")
  "...a list of directories analog to kk-keypath used by the HTML documentation system. It can be a subset of kk-keypath, or a completely different set of directories: the HTML doc can be created from another version or installation of KeyKit than the one you are working on."
  :type  '(repeat directory)
  :group 'keykit-doc)

(defcustom GeoMaestro-manual-URL "http://perso.infonie.fr/hepta/GM/eGM0.html"
  "URL for the index page of GeoMaestro HTML manual.
This is only used when creating HTML autodocumentation. If you have a local copy of the manual on your computer, you should point to it here. If you intend to publish the documentation on the web, then keep the default URL (except if the link is dead by the time..)"
  :type 'string
  :group 'keykit-doc)

(defcustom kk-indent-level 4
  "Indentation of KeyKit statements with respect to containing block."
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
  "Indentation can insert tabs (instead of spaces) if this is non-nil."
  :type 'boolean
  :group 'keykit)

(defcustom kk-geomaestro-items t
  "Non-nil adds specific GeoMaestro items to the KeyKit menu. 
Change only takes place at the next session (or keykit-mode.el evaluation)"
  :type 'boolean
  :group 'keykit)

(defcustom kk-geomaestro-dvlp-items nil
  "Non-nil adds items to the KeyKit menu for developing and debugging the GeoMaestro system. 
Also toggles Emacs keykit-mode itself in debug mode.
Change only takes place at the next session (or keykit-mode.el evaluation)"
  :type 'boolean
  :group 'keykit)

(defun kk-customize-group ()
  (interactive)
  (customize-group 'keykit))

(defun kk-customize-HTML-group ()
  (interactive)
  (customize-group 'keykit-doc))


; (defcustom kk-tab-always-indent t
;   "Non-nil means TAB in KeyKit mode should always reindent the current line,
; regardless of where in the line point is when the TAB command is used."
;   :type 'boolean
;   :group 'keykit)

(defvar kk-tab-always-indent t
  "Non-nil means TAB in KeyKit mode should always reindent the current line,
 regardless of where in the line point is when the TAB command is used.")


;;; --------------------------- Keymap & menu -----------------------------

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

(define-key keykit-mode-map [menu-bar] (make-sparse-keymap))

(if kk-geomaestro-dvlp-items
    (progn
      (define-key keykit-mode-map [menu-bar kkdebug]
        (cons "DEBUG" (make-sparse-keymap "KK-mode")))
      (define-key keykit-mode-map [menu-bar kkdebug test3]
        '("test 3" . kk-test3))
      (define-key keykit-mode-map [menu-bar kkdebug test2]
        '("test 2" . kk-test2))
      (define-key keykit-mode-map [menu-bar kkdebug test1]
        '("test 1" . kk-test1))))


(define-key keykit-mode-map [menu-bar kk2]
  (cons "KK-doc" (make-sparse-keymap "KK-mode")))
(define-key keykit-mode-map [menu-bar kk2 kkregsearch]
  '("regexp search Keypath" . kk-regexp-search-keypath))
(define-key keykit-mode-map [menu-bar kk2 kksearch]
  '("search Keypath" . kk-search-keypath))
(define-key keykit-mode-map [menu-bar kk2 sep01]
  '("--"))

(define-key keykit-mode-map [menu-bar kk2 kktopics]
  '("regexp search on topics" . kk-regexp-search-topics))
(define-key keykit-mode-map [menu-bar kk2 kkseecode]
  '("visit code" . kk-visit-code))
(define-key keykit-mode-map [menu-bar kk2 kkman]
  '("man" . kk-man))
(define-key keykit-mode-map [menu-bar kk2 sep02]
  '("--"))
(define-key keykit-mode-map [menu-bar kk2 makehtmlman]
  '("Make HTML man pages" . kk-make-HTML-man))
(define-key keykit-mode-map [menu-bar kk2 customhtml]
  '("Customize HTML man pages" . kk-customize-HTML-group))
(define-key keykit-mode-map [menu-bar kk2 sep1]
  '("--"))
(define-key keykit-mode-map [menu-bar kk2 insertdoc]
  '("Insert documentation fields" . kk-insert-doc-comments))


(define-key keykit-mode-map [menu-bar kk]
  (cons "KeyKit" (make-sparse-keymap "KK-mode")))

(if kk-geomaestro-dvlp-items
    (progn
      (define-key keykit-mode-map "\ef" 'kk-add-GMblah-from-french)
      (define-key keykit-mode-map "\eb" 'kk-add-GMblah-from-english)
      (define-key keykit-mode-map "\ew" 'kk-what-blah)
      (define-key keykit-mode-map [menu-bar kk frenchblahblah]
        '(".. to FrenchBlahBlah()" . kk-add-GMblah-from-french))
      (define-key keykit-mode-map [menu-bar kk englishblahblah]
        '(".. to EnglishBlahBlah()" . kk-add-GMblah-from-english))
      (define-key keykit-mode-map [menu-bar kk whatblahblah]
        '("GMblah_ ?" . kk-what-blah))
      (define-key keykit-mode-map [menu-bar kk sep0]
        '("--"))))


(if kk-geomaestro-items
    (progn
      (define-key keykit-mode-map [menu-bar kk insertcompoplug]
        '("Insert Compositor plug-in template" . kk-insert-compoplugin-template))
      (define-key keykit-mode-map [menu-bar kk insertguiplug]
        '("Insert GUI plug-in template" . kk-insert-plugin-template))
      (define-key keykit-mode-map [menu-bar kk insertdf]
        '("Insert distortion function template" . kk-insert-df-template))
      (define-key keykit-mode-map [menu-bar kk insertproj]
        '("Insert projector template" . kk-insert-projector-template))))

(define-key keykit-mode-map [menu-bar kk insert2b]
  '("Insert  {}" . kk-insert-2braces))
(define-key keykit-mode-map [menu-bar kk sep2]
  '("--"))

(define-key keykit-mode-map [menu-bar kk untab]
  '("Untabify buffer" . kk-untabify-buffer))
(define-key keykit-mode-map [menu-bar kk inserttab]
  '("Insert  TAB" . kk-insert-tab))
(define-key keykit-mode-map [menu-bar kk sep21]
  '("--"))

(define-key keykit-mode-map [menu-bar kk uncomment-region]
  '("Uncomment Region" . (lambda (beg end)
                           (interactive "r")
                           (comment-region beg end '(4)))))
(define-key keykit-mode-map [menu-bar kk comment-region]
  '("Comment Out Region" . comment-region))
(define-key keykit-mode-map [menu-bar kk sep22]
  '("--"))

(define-key keykit-mode-map [menu-bar kk kk-function]
  '("Select function" . mark-kk-function))
(define-key keykit-mode-map [menu-bar kk endoff]
  '("End of function" . end-of-defun))
(define-key keykit-mode-map [menu-bar kk begoff]
  '("Beginning of function" . beginning-of-defun))
(define-key keykit-mode-map [menu-bar kk indent-exp]
  '("Indent Expression" . indent-kk-exp))
(define-key keykit-mode-map [menu-bar kk indent-reg]
  '("Indent Region" . indent-region))
(define-key keykit-mode-map [menu-bar kk indent-line]
  '("Indent Line" . kk-indent-command))
(define-key keykit-mode-map [menu-bar kk backward-stmt]
  '("Backward Statement" . kk-beginning-of-statement))
(define-key keykit-mode-map [menu-bar kk forward-stmt]
  '("Forward Statement" . kk-end-of-statement))
(define-key keykit-mode-map [menu-bar kk sep3]
  '("--"))

; (define-key keykit-mode-map [menu-bar kk mutable-region]
;   '("Set region as mutable" . kk-mark-region-as-mutable))
(define-key keykit-mode-map [menu-bar kk kk-tcpip]
  '("Evaluate region (via TCP/IP) " . kk-tcpip-send-region))
(define-key keykit-mode-map [menu-bar kk kk-tcpip-go]
  '("TCP/IP console" . kk-tcpip))
(define-key keykit-mode-map [menu-bar kk run-lowkey]
  '("Run lowkey on this file" . kk-run-lowkey))
(define-key keykit-mode-map [menu-bar kk sep4]
  '("--"))

(define-key keykit-mode-map [menu-bar kk kkoutline]
  '("Toggle minor mode Outline" . outline-minor-mode ))
(define-key keykit-mode-map [menu-bar kk kkcustom]
  '("Customize KeyKit major mode" . kk-customize-group))


;(put 'comment-region 'menu-enable 'mark-active)


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
;;; statements. Hacked for KeyKit arrays declaration on several lines (??)
; (defconst kk-switch-label-regexp "case[ \t'/(]\\|default[ \t]*:")
(defconst kk-array-item-declaration-qui-marche-pas "^.*=.*,[ \t]*$")



(defun keykit-mode ()
  "Major mode for editing KeyKit code, based on the so-called \"boring old\" c-mode.

Tab indents for KeyKit code, by default with space characters only.
Comments are delimited with # ... \\n.


\\{keykit-mode-map}

Turning on KeyKit mode calls the value of the variable keykit-mode-hook with no args,
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
  (setq outline-regexp "^fun\\|^cla\\|[ \t]*method")

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
  (run-hooks 'keykit-mode-hook))


(defun kk-untabify-on-saving ()
  (setq local-write-file-hooks '(kk-untabify-buffer)))


;; This is used by indent-for-comment
;; to decide how much to indent a comment in KeyKit code
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
          (looking-at "[\t ]*#desc\t"))
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
  "Indent current line as KeyKit code, or in some cases insert a tab character.
If `kk-tab-always-indent' is non-nil (the default), always indent current line.
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
  "Used by kk-indent-line in order to take into account
if, else, for and while constructs with no braces (one-ligne
long blocks). 
Returns an integer which will be added to indent."
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
               (beginning-of-buffer)
               (what-line)))))
      (progn
        (previous-line 1)
        (end-of-line))
    (kk-beginning-of-statement 1)))


(defun kk-indent-line ()
  "Indent current line as KeyKit code.
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
        (if (and (/= (following-char) ?}) 
                 (save-excursion
                   (beginning-of-line)
                   (kk-beginning-of-statement 1)
                   (while (looking-at "\\[")
                     (kk-beginning-of-statement 1))
                   (looking-at "\\[\\|.*=[^][]*\\["))
                 (> (save-excursion
                      (end-of-line)
                      (count-lines 1 (point)))
                    (save-excursion
                      (beginning-of-line)
                      (kk-beginning-of-statement 1)
                      (kk-end-of-statement 1)
                      (count-lines 1 (point)))))
            (setq indent (save-excursion
                           (beginning-of-line)
                           (kk-beginning-of-statement 1)
                           (while (looking-at "\\[")
                             (kk-beginning-of-statement 1))
                           (current-column))))

        (indent-to (+ indent (calculate-kk-special-offset)))

        ;; If initial point was within line's indentation,
        ;; position after the indentation.  Else stay at same point in text.
        (if (> (- (point-max) pos) (point))
            (goto-char (- (point-max) pos))))
     shift-amt)))


(defun calculate-kk-special-offset ()
  "...see kk-special-previous-line"
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
  "Return appropriate indentation for current line as KeyKit code.
In usual case returns an integer: the column to indent to.
Returns nil if line starts inside a string, t if in a comment.
(this is not quite ready yet)"
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
  "Return the proper C indent for the first line after an open-brace.
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
  "Go to the beginning of the innermost KeyKit statement.
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
  "Go to the end of the innermost KeyKit statement.
With prefix arg, go forward N - 1 statements.
Move forward to end of the next statement if already at end."
  (interactive "p")
  (kk-beginning-of-statement (- count)))


(defun kk-skip-comment-lines (direction)
  "Used exclusively by kk-beginning-of-statement, allowing it to
ignore comments"
  (if (save-excursion
        (beginning-of-line)
        (looking-at "[ \t]*#\\|[ \t]*$"))
      ;; case 1: full comment line
      (if (eq 1 direction)
          (progn
            (if (equal (what-line)
                       (save-excursion
                         (end-of-buffer)
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
  "Put mark at end of KeyKit function/class, point at beginning."
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
   '("^[ \t]*\\(return\\)[ \t]*([^)]*" 
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
   '("#\\(name\\|usage\\|desc\\|see\\|topics\\)\t" 
     0 font-lock-type-face t)
   '("^[ \t]*#\\(include\\|define\\|library\\).*" 
     0 font-lock-keyword-face t)
   '("^[ \t]*#\\(include\\|define\\|library\\)"
     0 font-lock-warning-face t))
  "KeyKit-mode fontification")

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
    (beginning-of-buffer)
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


(defun kk-insert-tab ()
  (interactive)
  (insert "\t"))


(defun kk-insert-2braces ()
  "Simply insert the string \"{}\". This is useful when `{' and `}' are electric."
  (interactive)
  (insert "{}")
  (backward-char 1))


(defun kk-insert-doc-comments()
  "Insert the comment keywords for documenting a function."
  (interactive)
  (let ((fname (thing-at-point 'word)))
    (beginning-of-line)
    (if (null fname)
        (insert "\n#name\t\n#usage\t\n")
      (insert "\n#name\t" fname "\n")
      (insert "#usage\t" fname "()\n"))
    (insert "#desc\t\n#see\t\n#topics\t\n\n")
    (previous-line 6)
    (if (null fname)
        (search-forward "#name\t")
    (search-forward "("))))


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
                (insert-file-contents (concat kk-default-directory 
                                              "contrib/GeoMaestro/lib/franglish.k"))
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
        (find-file-other-window (concat kk-default-directory 
                                        "contrib/GeoMaestro/lib/franglish.k"))
        (beginning-of-buffer)
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
        (beginning-of-buffer)
        (search-forward (concat "#" end-here " begin Emacs"))
        (select-window (previous-window))
        blah-num))))


;;; --------------------------- On-line help  -----------------------------


(defun kk-insert-button (text action)
  (let* ((beg (point))
         (end (progn (insert "[" text "]") (point))))
    (put-text-property beg end 'mouse-face 'highlight)
    (let ((map (make-sparse-keymap)))
      (define-key map [mouse-1] action)
      (put-text-property beg end 'local-map map))))


(defun kk-regexp-search-topics (reg)
  (interactive "Msearch topics for regexp: ") 
  (if (not (equal reg ""))
      (let (begin-list beg end
            results-buffer tmp-buffer
            sources source
            found topic-found result-line)
        (save-excursion
          (setq results-buffer (get-buffer-create "*Keypath search*"))
          (set-buffer results-buffer)
          (setq buffer-read-only nil)
          (goto-char (point-max))
          (setq begin-list (point))
          (insert #("Searched topics matching: " 0 25 (face bold))
                  reg 
                  #("\nResults are:\n" 0 14 (face bold)))
          (kk-insert-button "Refresh" 'kk-refresh-search)
          (insert "\n\n")
          (setq tmp-buffer (get-buffer-create " *kk-temp*"))
          (setq sources (make-keykit-source-list))
          (while sources
            (setq source (car sources))
            (setq sources (cdr sources))
            (set-buffer tmp-buffer)
            (erase-buffer)
            (unwind-protect
                (insert-file-contents source)
              (beginning-of-buffer)
              (while (re-search-forward reg nil t)
                (setq topic-found (thing-at-point 'word))
                (if (and (> (current-column) 7)
                         (save-excursion
                           (beginning-of-line)
                           (looking-at "#topics")))
                    (save-excursion                     
                      (re-search-backward "^#name[ \t]*\\(.*\\)$") 
                      (setq found (match-string 1))
                      (add-text-properties 0 (length found) 
                                           '(face bold) found)
                      (add-text-properties 0 (length topic-found)
                                           '(face font-lock-keyword-face) topic-found)
                      (setq result-line 
                            (concat found 
                                    " --> " 
                                    (file-relative-name source kk-default-directory)
                                    " (" (what-line) ") " 
                                    topic-found))
                      (set-buffer results-buffer)
                      (insert result-line)
                      (put-text-property 
                       (save-excursion
                         (beginning-of-line)
                         (setq beg (point)))
                       (save-excursion
                         (end-of-line)
                         (search-backward " --> ")
                         (setq end (point)))                                        
                       'mouse-face 'highlight)
                      (let ((map (make-sparse-keymap)))
                        (define-key map [mouse-1] 'kk-man-at-point)
                        (put-text-property beg end 'local-map map))
                      (put-text-property 
                       (save-excursion
                         (beginning-of-line)
                         (search-forward " --> ")
                         (setq beg (point)))
                       (save-excursion
                         (beginning-of-line)
                         (search-forward ")")
                         (setq end (point)))                         
                       'mouse-face 'highlight)
                      (let ((map (make-sparse-keymap)))
                        (define-key map [mouse-1] 'kk-visit-searched-source)
                        (put-text-property beg end 'local-map map))
                      (insert "\n"))))))
          (kill-buffer tmp-buffer))
    (set-buffer results-buffer)
    (goto-char (point-max))
    (insert "\n\n\n")
    (goto-char begin-list)
    (switch-to-buffer results-buffer)
    (setq buffer-read-only t))))


(defun kk-make-clickable-string (topics-list action)
  (let ((c-string "")
        (t-list topics-list)
        new-topic)
    (while t-list
      (setq new-topic (car t-list))
      (setq t-list (cdr t-list))
      (put-text-property 0 (length new-topic) 'mouse-face 'highlight new-topic)
      (let ((map (make-sparse-keymap)))
        (define-key map [mouse-1] action)
        (put-text-property 0 (length new-topic) 'local-map map new-topic))
      (setq c-string (concat c-string " " new-topic)))
    c-string))


(defun kk-regexp-search-keypath (reg)
  "Go through all the *.k files in the Keypath and returns a clickable list of
all matches of argument regexp in the sources, in a specific buffer."
  (interactive "Msearch for regexp: ")
  (let (beg begin-list
        results-buffer tmp-buffer
        sources source
        found result-line)
    (if (not (equal reg ""))
      (save-excursion
        (setq results-buffer (get-buffer-create "*Keypath search*"))
        (set-buffer results-buffer)
        (setq buffer-read-only nil)
        (goto-char (point-max))
        (setq begin-list (point))
        (insert  #("Searched (regexp) for: " 0 23 (face bold)) 
                 reg 
                 #("\nResults are:\n" 0 14 (face bold)))
        (kk-insert-button "Refresh" 'kk-refresh-search)
        (insert "\n\n")
        (setq tmp-buffer (get-buffer-create " *kk-temp*"))
        (setq sources (make-keykit-source-list))
        (while sources
          (setq source (car sources))
          (setq sources (cdr sources))
          (set-buffer tmp-buffer)
          (erase-buffer)
          (unwind-protect
              (insert-file-contents source)
            (beginning-of-buffer)
            (while (re-search-forward reg nil t)
              (save-excursion   
                (backward-char 1) 
                (setq found (match-string 0))
                (add-text-properties 0 (length found) '(face bold) found)
                (setq result-line (concat found 
                                          " --> " 
                                          (file-relative-name source kk-default-directory)       
                                          " (" (what-line) ")"))
                (set-buffer results-buffer)
                (insert result-line)
                (put-text-property 
                 (save-excursion
                   (beginning-of-line)
                   (search-forward " --> ")
                   (setq beg (point)))
                 (point)                           
                 'mouse-face 'highlight)
                (let ((map (make-sparse-keymap)))
                  (define-key map [mouse-1] 'kk-visit-searched-source)
                  (put-text-property beg (point) 'local-map map))
                (insert "\n")))))
        (kill-buffer tmp-buffer)))
    (set-buffer results-buffer)
    (goto-char (point-max))
    (insert "\n\n\n")
    (goto-char begin-list)
    (switch-to-buffer results-buffer)
    (setq buffer-read-only t)))


(defun kk-search-keypath (string)
  "Go through all the *.k files in the Keypath and returns a clickable list of
all occurence of argument string in the sources, in a specific buffer."
  (interactive "P")
  (let ((results-buffer (get-buffer-create "*Keypath search*"))
        (tmp-buffer (get-buffer-create " *kk-temp*"))
        (sources (make-keykit-source-list))
        source found in-line result-line beg end begin-list)
    (if (equal string nil)
        (setq string (read-from-minibuffer
                      "String: "
                      (thing-at-point 'word))))
    (save-excursion
      (set-buffer results-buffer)
      (setq buffer-read-only nil)
      (goto-char (point-max))
      (setq begin-list (point))
      (insert  #("Searched for: " 0 14 (face bold)) 
               string 
               #("\nResults are:\n" 0 14 (face bold)))
      (kk-insert-button "Refresh" 'kk-refresh-search)
      (insert "\n\n")
      (while sources
        (setq source (car sources))
        (setq sources (cdr sources))
        (set-buffer tmp-buffer)
        (erase-buffer)
        (unwind-protect
            (insert-file-contents source)
          (beginning-of-buffer)
          (while (search-forward string nil t)
            (save-excursion     
              (backward-char 1) 
              (setq found (thing-at-point 'word))
              (setq in-line (thing-at-point 'line))
              (add-text-properties 0 (length in-line) '(face font-lock-string-face) in-line)
              (if (equal found string)
                  (add-text-properties 0 (length found) '(face font-lock-warning-face) found)
                  (add-text-properties 0 (length found) '(face font-lock-keyword-face) found))
              (setq result-line (concat in-line
                                        found 
                                        " --> " 
                                        (file-relative-name source kk-default-directory)        
                                        " (" (what-line) ")"))
              (set-buffer results-buffer)
              (insert result-line)
              (put-text-property 
               (save-excursion
                 (beginning-of-line)
                 (search-forward " --> ")
                 (setq beg (point)))
               (point)                           
               'mouse-face 'highlight)
              (let ((map (make-sparse-keymap)))
                (define-key map [mouse-1] 'kk-visit-searched-source)
                (put-text-property beg (point) 'local-map map))
              (insert "\n")))))
    (kill-buffer tmp-buffer))
    (set-buffer results-buffer)
    (goto-char (point-max))
    (insert "\n\n\n")
    (goto-char begin-list)
    (switch-to-buffer results-buffer)
    (setq buffer-read-only t)))


(defun kk-refresh-search ()
  (interactive)
  (let (type string)
      (previous-line 2)
      (beginning-of-line)
      ;; check the environment
      (re-search-forward "^Searched .")
      (setq type (thing-at-point 'word))
      (re-search-forward ": \\(.*\\)$")
      (setq string (match-string 1))
      ;; delete the last results
      (beginning-of-line)
      (setq buffer-read-only nil)
      (delete-region (point)
                     (save-excursion
                       (next-line 4)
                       (if (null (re-search-forward "^Searched" nil t))
                           (goto-char (point-max))
                         (match-beginning 0))))
      ;; call for a new search
      (cond
       ((equal type "regexp")
        (kk-regexp-search-keypath string))
        ((equal type "for")
        (kk-search-keypath string))
        ((equal type "topics")
        (kk-regexp-search-topics string)))))


(defun kk-visit-searched-source ()
  (interactive)
  (let (line)
    (end-of-line) 
    (re-search-backward " [0-9]*)")
    (setq line (string-to-int (match-string 0)))
    (beginning-of-line)
    (search-forward "--> ")
    (find-file (expand-file-name (thing-at-point 'filename) kk-default-directory))
    (goto-line line)))


(defun kk-man-at-point ()
  (interactive)
  (kk-man (thing-at-point 'word)))


(defun kk-search-topic-at-point ()
  (interactive)
  (kk-regexp-search-topics (thing-at-point 'word)))


(defun make-keykit-source-list ()
  (let ((dirs kk-keypath)
        (ldir ()))
    (while dirs
      (setq ldir 
            (append ldir
                    (directory-files (car dirs) t "\.k$")))
      (setq dirs (cdr dirs)))
    ldir))


(defun kk-source-of-function (function-name dirs)
  "Go through all the keylib.k in the Keypath and returns the name of
the file containing the code for the function given as argument, or nil
if it is not found."
  (interactive)
  (let ((source-filename "")
        (tmp-buffer (get-buffer-create " *kk-source-of-function*"))
        keylib dir)
    (save-excursion
      (while dirs
        (setq dir (car dirs))
        (setq dirs (cdr dirs))
        (setq keylib (concat dir "keylib.k"))
        (set-buffer tmp-buffer)
        (erase-buffer)
        (unwind-protect
            (insert-file-contents keylib)
          (beginning-of-buffer)
          (if (search-forward-regexp (concat "\.k " function-name "[ \t]*$") nil t)
              (progn                
                (search-backward ".k")
                (setq source-filename 
                      (concat dir (thing-at-point 'filename))))))))
    (kill-buffer tmp-buffer)
    (if (equal source-filename "")
        nil
      source-filename)))


(defun kk-get-function-description (function-name dirs)
  "...used by kk-man"
  (let ((description nil)
        source-filename
        tmp-buffer)
  (save-excursion
    (setq source-filename (kk-source-of-function function-name dirs))
    (if (null source-filename)
        nil
      (set-buffer (setq tmp-buffer (get-buffer-create " *kk-get-function-description*")))
      (unwind-protect
          (insert-file-contents source-filename)
        (beginning-of-buffer)
        (if (search-forward-regexp (concat "#name[\t ]*" function-name "[ \t]*$") nil t)
            (progn
              (beginning-of-line)
              (while (looking-at "#[a-z]*")
              (setq description 
                    (cons 
                     (list 
                      (match-string 0)
                      (substring (thing-at-point 'line) (1+ (length (match-string 0)))))
                     description))
              (next-line 1))))
        (kill-buffer tmp-buffer))))
  (nreverse description)))


(defun kk-visit-code (function-name)
  "Displays the source code for the function given as argument in a specific buffer."
  (interactive "P")
  (if (equal function-name nil)
      (setq function-name (read-from-minibuffer
                           "KeyKit function/class: "
                           (thing-at-point 'word))))
  (let (source pos-point)
    (if (setq pos-point (string-match "\\." function-name))
        (let ((class-name (substring function-name 0 pos-point))
              (method-name (substring function-name (1+ pos-point))))
          (if (null (setq source (kk-source-of-function class-name kk-keypath)))
              (format "Couldn't find this class (%s ?) code, sorry" class-name)
            (find-file source)
            (beginning-of-buffer)
            (search-forward-regexp (concat "class[\t ]*" class-name "[\t ]*{"))
            (search-forward-regexp (concat "method[\t ]*" method-name "[\t ]*(")))))
    (if (null (setq source (kk-source-of-function function-name kk-keypath)))
        (format "Couldn't find this function (%s ?) code, sorry" function-name)
      (find-file source)
      (beginning-of-buffer)
      (search-forward-regexp (concat "\\(function\\|class\\)[\t ]*" function-name "[\t ({]")))))


(defun kk-man (function-name)
  "Displays the documentation for the function given as argument in a specific buffer.
If no documentation available, returns nil"
  (interactive "P")
  (let (kk-man-buffer
        desc-list
        item)
    (if (equal function-name nil)
        (setq function-name (read-from-minibuffer
                             "KeyKit function/class: "
                             (thing-at-point 'word))))
    (if (null (setq desc-list (kk-get-function-description function-name kk-keypath)))
        (prin1 (concat "This function (" function-name "?) is not documented, sorry"))
      (setq kk-man-buffer (get-buffer-create "*KeyKit help*"))
      (if (null (equal (current-buffer) kk-man-buffer))
          (switch-to-buffer-other-window kk-man-buffer))
      (setq buffer-read-only nil)
      (erase-buffer)
      (while desc-list
        (setq item (car desc-list))
        (setq desc-list (cdr desc-list))
        (cond
         ((equal "#name" (car item))
          (insert #("Name: " 0 5 (face bold)) (cadr item)))
         ((equal "#usage" (car item))
          (insert #("Usage: " 0 6 (face bold)) (cadr item) "\n"))
         ((equal "#desc" (car item))
          (if (null (cadr item))
              (insert "\n")
            (insert "  " (cadr item))))
         ((equal "#see" (car item))
          (insert #("\nSee: " 0 4 (face bold)))
          (insert (kk-make-clickable-string (split-string (cadr item) "[;, \t()]+")
                                            'kk-man-at-point)))
         ((equal "#topics" (car item))
          (insert #("\nRelated topics: " 0 15 (face bold)))
          (insert (kk-make-clickable-string (split-string (cadr item) "[;, \t]+") 
                                            'kk-search-topic-at-point)))))
    (beginning-of-buffer)
    (next-line 2)
    (setq buffer-read-only t))))



;;; --------------------------- HTML help  -----------------------------

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
   

(defun kk-make-HTML-man ()
  "Creates HTML documentation from the #desc fields from all code in the
part of the Keypath defined in kk-HTML-doc-keypath. 
It is written in the kk-HTML-directory (see the \"customize\" item in the 
main KeyKit menu)."
  (interactive)
  (if (yes-or-no-p "This will override the current documentation. Continue ?")
      (let ((dirs kk-HTML-doc-keypath)
            (lib-num 0)
            (f-list ())
            (index-buffer (get-buffer-create "*kk-make-HTML-man*"))
            f-URL top-page)
        (save-excursion
          (if (equal "" kk-HTML-doc-directory)
              (setq kk-HTML-doc-directory kk-default-directory))
          (setq kk-HTML-f-URLs (make-hash-table :test #'equal))
          (setq kk-HTML-f-topics (make-hash-table :test #'equal))
          (setq kk-HTML-buffers '())
          (set-buffer index-buffer)
          ;;
          ;; (index page) header:
          ;;
          (insert (kk-HTML-doc-header kk-HTML-title))
          (insert "<H1>" kk-HTML-title "</H1>")
          (insert "<I>This documentation has been created by <A HREF=\""
                  (file-name-directory GeoMaestro-manual-URL)
                  "GMemacs.html\">keykit-mode for Emacs</A>")
          (insert "<BR> on " (current-time-string) "</I><BR><BR>")
          ;;
          ;; (index page) links to libraries:
          ;;
          (insert "<H2>Libraries: </H2><UL>")
          (while dirs
            (setq lib-num (1+ lib-num))
            (kk-make-HTML-man-for-library (car dirs) 
                                          kk-HTML-doc-chunk 
                                          (concat kk-HTML-directory "lib" lib-num "_"))
            (insert "<LI> <H2><A HREF=\"" 
                    (file-relative-name
                     (concat kk-HTML-directory "lib" lib-num "_all.html")
                     kk-HTML-directory)
                    "\">"
                    (file-relative-name
                     (car dirs) 
                     kk-HTML-doc-directory)
                    "</A></H2>")
            (setq dirs (cdr dirs)))
          (insert "</UL>")
          ;;
          ;; (index page) links to topics:
          ;;
          (insert "<BR><H2>Topics: <UL></H2>")
          (setq kk-HTML-topics (sort kk-HTML-topics 'string<))
          (while kk-HTML-topics
            (setq top-page (concat kk-HTML-directory
                                   "topic_"
                                   (car kk-HTML-topics)
                                   ".html"))
            (insert "<LI><A HREF=\""
                    (file-relative-name
                     top-page 
                     kk-HTML-directory)
                    "\">" (car kk-HTML-topics) "</A>")
            (kk-HTML-make-topic-page top-page (car kk-HTML-topics))
            (setq kk-HTML-topics (cdr kk-HTML-topics)))
          (insert "</UL>")
          ;;
          ;; (index page) links to alphabetically sorted functions:
          ;;     
          (insert "<BR><H2>Documented functions (alphabetic order): </H2>")
          (maphash #'(lambda (f url)
                       (setq f-list (append f-list (list (list f url)))))
                       kk-HTML-f-URLs)
          (setq f-list (sort f-list #'(lambda (l1 l2) (string< (car l1) (car l2)))))
          (while f-list
            (insert "- <A HREF=\"" (cadar f-list) "\">" (caar f-list) "</A>")
            (setq f-list (cdr f-list)))
          (if (not (equal "" kk-HTML-external-link))
              (insert "<BR><BR><BR><BR><A HREF=\""
                      kk-HTML-external-link
                      "\">"
                      (if (not (equal "" kk-HTML-external-link-label))
                          kk-HTML-external-link-label
                        "back")
                      "</A><BR><BR><BR>"))
          ;;
          ;; footer:
          ;;
          (insert (kk-HTML-doc-footer))
          (write-file (concat kk-HTML-directory "index.html"))
          (kill-buffer (current-buffer))
          ;;
          ;; second pass into the lib*.html, to link the "see" items
          ;;
          (while kk-HTML-buffers
            (set-buffer (car kk-HTML-buffers))
            (beginning-of-buffer)
            (while (re-search-forward "^ -f#\\([_a-zA-Z0-9]*\\)#f-$" nil t)
              (if (setq f-URL (gethash (match-string 1) kk-HTML-f-URLs))
                  (replace-match (concat
                                  "<A HREF=\""
                                  (gethash (match-string 1) kk-HTML-f-URLs)
                                  "\">"
                                  (match-string 1)
                                  "</A>")
                                 nil nil)
                (replace-match (match-string 1) nil nil)))
            (save-buffer)
            (if (file-exists-p (concat buffer-file-name "~"))
                (delete-file (concat buffer-file-name "~")))
            (kill-buffer (current-buffer))
            (setq kk-HTML-buffers (cdr kk-HTML-buffers)))
          ;;
          ;; done.
          ;;
          (message-box "HTML documentation is done !")))))


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
      (sort-fields 3 1 (save-excursion (end-of-buffer) (point)))
      (beginning-of-buffer)
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
                       (beginning-of-buffer)
                       (insert (kk-HTML-doc-header (concat lib 
                                                           " : page " (int-to-string page-num))))
                       (setq links 
                             (concat (if (> page-num 1)
                                         (concat "&lt; <A HREF=\""
                                                 (file-relative-name
                                                  (concat HTML-root-name 
                                                          (int-to-string (- page-num 1)) 
                                                          ".html")
                                                  kk-HTML-directory)
                                                 "\">previous page</A> ]")
                                       "")
                                     (if not-the-end
                                         (concat "[ <A HREF=\""
                                                 (file-relative-name
                                                  (concat HTML-root-name 
                                                          (int-to-string (1+ page-num)) 
                                                          ".html")
                                                  kk-HTML-directory)
                                                 "\">next page</A> &gt;")
                                       "")
                                     "<BR><BR><A HREF=\"index.html\">back</A><BR>"
                                     "<A HREF=\""
                                     (file-relative-name
                                      (concat HTML-root-name "all.html")
                                      kk-HTML-directory)
                                     "\">back to #library listing</A><BR><BR>"))
                       (insert links)
                       (end-of-buffer)
                       (insert "<HR><BR><BR>")
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
                (beginning-of-buffer)
                (search-forward-regexp (concat " " f-candidat "$"))
                (insert "</A>")
                (search-backward " ")
                (forward-char 1) 
                (setq f-URL (concat 
                             (file-relative-name
                              (concat HTML-root-name (int-to-string (1+ page-num)) ".html")
                              kk-HTML-directory)
                             "#" f-candidat))
                (setf (gethash f-candidat kk-HTML-f-URLs) f-URL)
                (insert "<A HREF=\"" f-URL "\">")))))
      (set-buffer html-lib-buffer)
      (beginning-of-buffer)
      (insert (kk-HTML-doc-header (concat "Functions in: "
                                          (file-relative-name
                                           lib 
                                           kk-HTML-doc-directory))))
      (insert "<H2>Functions defined in:<BR> "
              (file-relative-name
               lib 
               kk-HTML-doc-directory) 
              "</H2>")
      (if (> page-num 0)
          (insert "<H2>Proceed to an <A HREF=\""
                  (file-relative-name
                   (concat HTML-root-name "1.html")
                   kk-HTML-directory)
                  "\">alphabetic listing</A> or click in the following:</H2>")
        (insert "<H2>None of these functions is documented, sorry</H2>"))
      (perform-replace "\n" "\n<BR>" nil nil nil)
      (insert "<BR><BR><A HREF=\"index.html\">back</A><BR>")
      (insert (kk-HTML-doc-footer))
      (write-file (concat HTML-root-name "all.html"))
      (kill-buffer (current-buffer))
      (kill-buffer html-buffer)
      (kill-buffer tmp-kl-buffr))))


(defun kk-HTML-make-topic-page (filename topic)
  "Writes index page for topic."
  (save-excursion
    (let ((f-list (gethash topic kk-HTML-f-topics))
          (tmp-buffer (get-buffer-create "*kk-HTML-topic*")))
      (setq f-list (sort f-list 'string<))
      (set-buffer tmp-buffer)
      (insert (kk-HTML-doc-header (concat "Functions with topic: " topic)))
      (insert "<H1>Functions with topic: " topic "</H1>")
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
  "Inserts the documentation for the function given as argument in HTML format,
in buffer, then returns t. If no documentation available (in kk-HTML-f-descriptions),
 returns nil"
  (let (end
        desc-list
        item
        name)
    (save-excursion
      (set-buffer buffer)
      (if (null (setq desc-list (gethash function-name kk-HTML-f-descriptions)))
          nil
        (while desc-list
          (setq item (car desc-list))
          (setq desc-list (cdr desc-list))
          (cond
           ((equal "#name" (car item))
            (insert "<HR><A NAME=\""   
                    (setq name (car (split-string (cadr item) "[ \t\n]+")))
                    "\"></A>\n" ))
           ((equal "#usage" (car item))
            (insert "<PRE><B>" 
                    (car (split-string (cadr item) "\n")) 
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
                    (split-string (cadr item) "[, ;\t\n]"))
            (insert "<CODE><PRE>"))
           ((equal "#topics" (car item))
            (insert "\n<U>Related topics</U>:")
            (mapcar (lambda (s)
                      (if (equal s "")
                          nil
                        (insert " <A HREF=\""
                                (file-relative-name
                                 (concat kk-HTML-directory "topic_" s ".html")
                                 kk-HTML-directory)
                                "\">" s "</A>")
                        (setf (gethash s kk-HTML-f-topics) 
                              (union (gethash s kk-HTML-f-topics) (list name) :test #'equal))
                        (setq kk-HTML-topics (union kk-HTML-topics (list s) :test #'equal))))
                    (split-string (cadr item) "[, ;\t\n]"))
            (insert "\n"))))
        (insert "</PRE><BR>\n")
        t))))


(defun kk-get-all-descriptions-in-library (lib)
  "Puts all documented functions (names and descriptions) in directory lib 
within the hash table kk-HTML-f-descriptions. For internal use only (called by
kk-make-HTML-man-for-library)."
  (let ((description nil)
        source-filename 
        function-name
        (sources (directory-files lib t "\\.k$"))
        tmp-buffer)
    (setq kk-HTML-f-descriptions (make-hash-table :test #'equal))
    (save-excursion
      (while sources      
        (setq source-filename (car sources))
        (setq sources (cdr sources))
        (set-buffer (setq tmp-buffer (get-buffer-create " *kk-get-all-descriptions*")))
        (erase-buffer)
        (unwind-protect
            (insert-file-contents source-filename)
          (beginning-of-buffer)
          (while (search-forward-regexp (concat "^#name[\t ]*\\(.*\\)[ \t]*$") nil t)
            (progn
              (setq function-name (match-string 1))
              (beginning-of-line)
              (while (looking-at "#[a-z]*")
                (setq description
                      (cons 
                       (list 
                        (match-string 0)
                        (kk-replace-in-string 
                         (substring (thing-at-point 'line) (1+ (length (match-string 0))))
                         '((">" "&gt;")
                           ("<" "&lt;"))))
                       description))
                (next-line 1))          
              (setf (gethash function-name kk-HTML-f-descriptions) (nreverse description))  
              (setq description nil))))
        (kill-buffer tmp-buffer)))))


;;; --------------------------- End  -----------------------------

(provide 'keykit-mode)

;;; keykit-mode.el ends here









