;;; -*- auto-recompile: t -*-

;; This file is not part of GNU Emacs.
;; 
;; keykit-interaction.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; keykit-interaction.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;;-------------
;;             interacting with Keykit via TCP/IP

;; last modified March 28, 2006
;; for Emacs 21.1
;;; --------------------------------------------------------------------------------

(require 'keykit-mode nil t)

(defun keykit-connected-somehow-p ()
  (or kk-tcpip-process
      (keykit-connected-via-surman-p)))

(defun keykit-connected-via-surman-p ()
  (and (featurep 'surman)
       (surman-keykit-connected-p)))

(defun keykit-eval (str)
  "Have Keykit evaluate STR as code and return the result"
  (if (keykit-connected-via-surman-p)
    (surman-ask-keykit str)
    (kk-eval-with-return str)))

(defun keykit-do (str)
  "Have Keykit evaluate STR as code"
  (if (keykit-connected-via-surman-p)
    (surman-ask-keykit str)
    (condition-case nil
        (kk-eval str)
      (error (message "No available connection to Keykit")))))


;;; --------------------------- End  -----------------------------

(provide 'keykit-interaction)

;;; keykit-interaction.el ends here







