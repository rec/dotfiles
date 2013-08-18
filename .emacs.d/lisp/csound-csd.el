;; Mode for editing Csound CSD files
;; John Fitch 18 Dec 2003

;; Copyright (C) 1996/97 by Codemist Ltd

;; Author: John Fitch <jpff@codemist.co.uk>
;; Keywords: Csound
;; Version: 0.1
(defconst csound-csd-version " 0.1")

;;; This file is not part of GNU Emacs.

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; I N S T A L L A T I O N
;;; In order to arrange for autoloading add the following to .emacs
;;; (setq auto-mode-alist (cons '("\\.csd$" . csound-csd-mode) auto-mode-alist))
;;; (autoload 'csound-csd-mode "csound-csd" "Csound CSD major mode." t)


(defvar csound-csd-mode-build-new-buffer t
  "*If not nil, then new buffers start with standard components.")

(defvar csd-verbose t
  "Non nil to show a counter during server script matching")

;; Csound-csd basic keymap
(defvar csound-csd-mode-map (make-sparse-keymap) "Keymap for csound-csd-mode")

;; special mode keys
(define-key csound-csd-mode-map "\C-c\C-c" 'csound-csd-quick)
(define-key csound-csd-mode-map "\C-c\C-o" 'csound-csd-edit-orch)
(define-key csound-csd-mode-map "\C-c\C-s" 'csound-csd-edit-score)
(define-key csound-csd-mode-map "\C-c\C-l" 'csound-csd-edit-options)
(define-key csound-csd-mode-map "\C-co"    'csound-csd-insert-orch-file)
(define-key csound-csd-mode-map "\C-cO"    'csound-csd-insert-orch-buffer)
(define-key csound-csd-mode-map "\C-cs"    'csound-csd-insert-score-file)
(define-key csound-csd-mode-map "\C-cS"    'csound-csd-insert-score-buffer)
(define-key csound-csd-mode-map "\C-cm"    'csound-csd-insert-midi-file)
(define-key csound-csd-mode-map "\C-cM"    'csound-csd-delete-midi-file)
(define-key csound-csd-mode-map "\C-cw"    'csound-csd-insert-sample-file)
(define-key csound-csd-mode-map "\C-cW"    'csound-csd-delete-sample-file)
(defvar csound-csd-mode-syntax-table nil "Syntax table")

(if csound-csd-mode-syntax-table
    ()
  (setq csound-csd-mode-syntax-table
        (make-syntax-table text-mode-syntax-table))
  (modify-syntax-entry ?<  "(>  " csound-csd-mode-syntax-table)
  (modify-syntax-entry ?>  ")<  " csound-csd-mode-syntax-table)
  (modify-syntax-entry ?\" ".   " csound-csd-mode-syntax-table)
  (modify-syntax-entry ?\\ ".   " csound-csd-mode-syntax-table)
  (modify-syntax-entry ?'  "w   " csound-csd-mode-syntax-table))

;;; An attempt at font-lock support
(require 'font-lock)
(make-local-variable 'font-lock-defaults)
(setq font-lock-defaults '(csd-font-lock-keywords t t))

(defvar csd-font-lock-keywords nil
  "Additional expressions to highlight in CSD helper mode.")
(setq csd-font-lock-keywords
    (list		
     '("^\<CsoundSynthesizer\>" . font-lock-csd-synth)
     '("^\</CsoundSynthesizer\>" . font-lock-csd-synth)
     '("^\<CsInstruments\>" . font-lock-csd-inst)
     '("^\</CsInstruments\>" . font-lock-csd-inst)
     '("^\<CsScore\>" . font-lock-csd-score)
     '("^\</CsScore\>" . font-lock-csd-score)
;;     '("<CsInstruments>\\([^<]*\\)</CsInstruments>" 1 csd-bold-face t)
;;     '("<CsScore>\\([^<]*\\)</CsScore>" 1 csd-italic-face t)
     '("[=(&]?[ \t\n]*\\(\"[^\"\n]*<%[^\"\n]*\\(\"[^\"\n]*\"\\)[^\"\n]*%>[^\"\n]*\\)" 1 font-lock-csd-string t)
     '("[=(&]?[ \t\n]*\\(\"[^\"\n]*\"\\)"  1 font-lock-csd-string t)
;     '("\\(<%=\\w\\)" 1 csd-builtin-face t)
;     '("\\(\")[^\"\n]*%>\\)" 1 csd-builtin-face t)
;     '("\\(<%=[^%]*%>\\)" 1 csd-builtin-face t)
;     '("\\(<\\?=\\w\\)" 1 csd-builtin-face t)
;     '("\\(\")[^\"\n]*\\?>\\)" 1 csd-builtin-face t)
;     '("\\(<\\?=[^%]*\\?>\\)" 1 csd-builtin-face t)
     ))
 

(defun csd-fontify-region (beg end verbose)
  (let ((loudly (and verbose
		     (> (- end beg) (/ (buffer-size) 2)))))
    (setq csd-verbose loudly)
    (font-lock-default-fontify-region beg end loudly)))

(set (make-local-variable font-lock-fontify-region-function) 
     'csd-fontify-region)

(defun csd-fontify-buffer ()
  (setq csd-verbose (if (numberp font-lock-verbose)
				(> (buffer-size) font-lock-verbose)
			      font-lock-verbose))
  (font-lock-default-fontify-buffer))

(set (make-local-variable font-lock-fontify-buffer-function) 
     'csd-fontify-buffer)

;; csound-csd-mode

(defun csound-csd-mode ()
  "Mode for editing Csound Csd files

\\{csound-csd-mode-map}
Major mode for editing Csound Csd files
Written by John Fitch (jpff@cs.bath.ac.uk)"
  (interactive)
  (setq font-lock-maximum-decoration t)
  (turn-on-font-lock)
  (kill-all-local-variables)

  (use-local-map csound-csd-mode-map)
  (set-syntax-table csound-csd-mode-syntax-table)

  (setq mode-name "Csound Csd")
  (setq major-mode 'csound-csd-mode)

;; Font lock
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '((csd-font-lock-keywords)
                             nil nil nil beginning-of-defun
                             (font-lock-comment-start-regexp . ";")
                             (font-lock-mark-block-function mark-defun)))
  (let ((colour-faces
         '(;; Format is:
           ;; face                              "colour"        bold
           (font-lock-csd-string                "magenta"       nil)
           (font-lock-csd-comment               "grey50"        nil)
           (font-lock-csd-synth                 "DarkGoldenrod" t)
           (font-lock-csd-inst                  "RoyalBlue"     t)
           (font-lock-csd-score                 "red"           t)
           ))
        face colour colour-face colour-bdbg)
    (while colour-faces
      (setq face (nth 0 (car colour-faces))
            colour (nth 1 (car colour-faces))
            colour-face (intern (concat (symbol-name face) "-colour"))
            colour-bdbg (nth 2 (car colour-faces))
            colour-faces (cdr colour-faces))
      (make-face colour-face)
      (if colour-bdbg (make-face-bold colour-face nil t))
      (set-face-foreground colour-face colour)
      (set face colour-face)))
  (font-lock-mode 1)
  (csound-install-csd-menu "Csound/Csd")
  (run-hooks 'csound-csd-mode-hook)

  (if (and csound-csd-mode-build-new-buffer (zerop (buffer-size)))
        (csound-csd-insert-new-buffer-strings)))

(defun csound-csd-insert-new-buffer-strings ()
  "Insert csound-csd-mode strings."
  (insert "<CsoundSynthesizer>

<CsInstruments>

</CsInstruments>

<CsScore>

</CsScore>

</CsoundSynthesizer>
"))

;; To-do
;; insert sample
;; insert midi

(defun csound-csd-quick ()
  (interactive)
  (widen)
  (csound-csd-mode))

(defun csound-csd-edit-score ()
  (interactive)
  (let (spoint epoint)
    (goto-char 0)
    (if (re-search-forward "^<CsScore>" (point-max) t)
        (progn
          (forward-line 1)
          (setq spoint (point))
          (re-search-forward "^</CsScore>" (point-max) t)
          (forward-line -1)
          (setq epoint (point)))
      (re-search-forward "</CsoundSynthesizer>" (point-max) t)
      (insert "<CsScore>

</CsScore>
")
      (forward-line -2)
      (setq spoint (setq epoint (point))))
    (save-excursion
      (csound-sco-mode)
      (font-lock-fontify-region spoint epoint)
      (narrow-to-region spoint epoint))))

(defun csound-csd-insert-score-file (name)
  (interactive "fInsert Score file: ")
  (let (spoint epoint)
    (goto-char 0)
    (if (re-search-forward "^<CsScore>" (point-max) t)
        (progn
          (forward-line 1)
          (setq spoint (point))
          (re-search-forward "^</CsScore>" (point-max) t)
          (forward-line -1)
          (setq epoint (point))
          (delete-region spoint epoint))
      (re-search-forward "</CsoundSynthesizer>" (point-max) t)
      (insert "<CsScore>

</CsScore>
")
      (forward-line -2))
    (insert-file-contents name)
    (goto-char spoint)))

(defun csound-csd-insert-score-buffer (name)
  (interactive "bInsert Score buffer: ")
  (let (spoint epoint)
    (goto-char 0)
    (if (re-search-forward "^<CsScore>" (point-max) t)
        (progn
          (forward-line 1)
          (setq spoint (point))
          (re-search-forward "^</CsScore>" (point-max) t)
          (forward-line -1)
          (setq epoint (point))
          (delete spoint epoint))
      (re-search-forward "</CsoundSynthesizer>" (point-max) t)
      (insert "<CsScore>

</CsScore>
")
      (forward-line -2))
    (insert-buffer-substring name)
    (goto-char spoint)))

(defun csound-csd-edit-orch ()
  (interactive)
  (let (spoint epoint)
    (goto-char 0)
    (if (re-search-forward "^<CsInstruments>" (point-max) t)
        (progn
          (forward-line 1)
          (setq spoint (point))
          (re-search-forward "^</CsInstruments>" (point-max) t)
          (beginning-of-line)
          (setq epoint (point)))
      (re-search-forward "</CsoundSynthesizer>" (point-max) t)
      (insert "<CsInstruments>

</CsInstruments>
")
      (forward-line -2))
    (save-excursion
      (csound-orc-mode)
      (font-lock-fontify-region spoint epoint)
      (narrow-to-region spoint epoint))))

(defun csound-csd-insert-orch-file (name)
  (interactive "fInsert Orchestra file: ")
  (let (spoint epoint)
    (goto-char 0)
    (if (re-search-forward "^<CsInstruments>" (point-max) t)
        (progn
          (forward-line 1)
          (setq spoint (point))
          (re-search-forward "^</CsInstruments>" (point-max) t)
          (beginning-of-line)
          (setq epoint (point))
          (delete-region spoint epoint))
      (re-search-forward "</CsoundSynthesizer>" (point-max) t)
      (insert "<CsInstruments>

</CsInstruments>
")
      (forward-line -2))
    (insert-file-contents name)
    (goto-char spoint)))

(defun csound-csd-insert-orch-buffer (name)
  (interactive "bInsert Orchestra buffer: ")
  (let (spoint epoint)
    (goto-char 0)
    (if (re-search-forward "^<CsInstruments>" (point-max) t)
        (progn
          (forward-line 1)
          (setq spoint (point))
          (re-search-forward "^</CsInstruments>" (point-max) t)
          (beginning-of-line)
          (setq epoint (point))
          (delete-region spoint epoint))
      (re-search-forward "</CsoundSynthesizer>" (point-max) t)
      (insert "<CsInstruments>

</CsInstruments>
")
      (forward-line -2))
    (insert-buffer-substring name)
    (goto-char spoint)))

(defun csound-csd-edit-options ()
  (interactive)
  (goto-char 0)
  (re-search-forward "^<CsOptions>" (point-max) t)
  (forward-line 1))

(defun csound-csd-insert-midi-file (name)
  (interactive "fMidi file name")
  (let (spoint epoint)
    (goto-char 0)
    (if (re-search-forward "^<CsMidifileB>" (point-max) t)
        (progn
          (forward-line 1)
          (setq spoint (point))
          (re-search-forward "^</CsMidifileB>" (point-max) t)
          (beginning-of-line)
          (setq epoint (point))
          (delete-region spoint epoint))
      (re-search-forward "</CsoundSynthesizer>" (point-max) t)
      (insert "<CsMidifileB>

</CsMidifileB>
")
      (forward-line -2))
    (setq spoint (point))
    (insert-file-contents name)
    (re-search-forward "</CsMidifileB>" (point-max))
    (base64-encode-region spoint (point))
    (goto-char spoint)))

(autoload 'base64-encode-region "base64" "Encloder for base64" t)

(defun csound-csd-delete-midi-file ()
  (interactive)
  (let (spoint epoint)
    (goto-char 0)
    (if (re-search-forward "^<CsMidifileB>" (point-max) t)
        (progn
          (beginning-of-line)
          (setq spoint (point))
          (re-search-forward "^</CsMidifileB>" (point-max) t)
          (forward-line 1)
          (setq epoint (point))
          (delete-region spoint epoint)))))

(defun csound-csd-insert-sample-file (name name1)
  (interactive "fFile name of sample
Mcalled")
  (goto-char 0)
  (re-search-forward "</CsoundSynthesizer>" (point-max) t)
  (beginning-of-line)
  (insert "<CsSampleB filename=")
  (insert name1)
  (insert ">

</CsSampleB>
")
  (forward-line -2)
  (let ((spoint (point)))
    (insert-file-contents name)
    (re-search-forward "</CsSampleB>" (point-max))
    (base64-encode-region spoint (point))
    (goto-char spoint)))

(defun csound-csd-delete-sample-file (name1)
  (interactive "Mcalled")
  (if (re-search-forward (concat "<CsSampleB filename=" name1 ">"))
      (let (spoint)
        (beginning-of-line)
        (setq spoint (point))
        (re-search-forward "</CsSampleB>")
        (forward-line 1)
        (delete-region spoint (point)))))
  
(defun csound-csd-insert-file-file (name name1)
  (interactive "fFile name of sample
MCalled: ")
  (goto-char 0)
  (re-search-forward "</CsoundSynthesizer>" (point-max) t)
  (beginning-of-line)
  (insert "<CsFileB filename=")
  (insert name1)
  (insert ">

</CsFileB>
")
  (forward-line -2)
  (let ((spoint (point)))
    (insert-file-contents name)
    (re-search-forward "</CsFileB>" (point-max))
    (base64-encode-region spoint (point))
    (goto-char spoint)))

(defun csound-csd-delete-file-file (name1)
  (interactive "MCalled: ")
  (if (re-search-forward (concat "<CsFileB filename=" name1 ">") t)
      (let (spoint)
        (beginning-of-line)
        (setq spoint (point))
        (re-search-forward "</CsFileB>")
        (forward-line 1)
        (delete-region spoint (point)))
    (message "Not found")))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MENUS
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun csound-install-csd-menu (name)
  (define-key csound-csd-mode-map [menu-bar csound-csd]
    (cons name (make-sparse-keymap name)))

  (define-key csound-csd-mode-map [menu-bar csound-csd delete-file]
    '("Delete file" .  csound-csd-delete-file-file))
  (define-key csound-csd-mode-map [menu-bar csound-csd insert-file]
    '("Insert file" .  csound-csd-insert-file-file))
  (define-key csound-csd-mode-map [menu-bar csound-csd delete-sample]
    '("Delete Sample file" .  csound-csd-delete-sample-file))
  (define-key csound-csd-mode-map [menu-bar csound-csd insert-sample]
    '("Insert Sample file" .  csound-csd-insert-sample-file))
  (define-key csound-csd-mode-map [menu-bar csound-csd delete-midi]
    '("Delete MIDI file" .  csound-csd-delete-midi-file))
  (define-key csound-csd-mode-map [menu-bar csound-csd insert-midi]
    '("Insert MIDI file" .  csound-csd-insert-midi-file))
  (define-key csound-csd-mode-map [menu-bar csound-csd score-buffer]
    '("Insert Score Buffer" .  csound-csd-insert-score-buffer))
  (define-key csound-csd-mode-map [menu-bar csound-csd score-file]
    '("Insert Score File" . csound-csd-insert-score-file))
  (define-key csound-csd-mode-map [menu-bar csound-csd orc-buffer]
    '("Insert Orchestra Buffer" . csound-csd-insert-orch-buffer))
  (define-key csound-csd-mode-map [menu-bar csound-csd orc-file]
    '("Insert Orchestra File" . csound-csd-insert-orch-file))
  (define-key csound-csd-mode-map [menu-bar csound-csd edit-options]
    '("Edit options section" . csound-csd-edit-options))
  (define-key csound-csd-mode-map [menu-bar csound-csd edit-score]
    '("Edit Score section" . csound-csd-edit-score))
  (define-key csound-csd-mode-map [menu-bar csound-csd edit-orch]
    '("Edit Orchestra section" . csound-csd-edit-orch)))


