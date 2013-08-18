;;; -*- auto-recompile: t -*-

;;; csound-pub.el --- publishing csound-based works

;; This file is not part of GNU Emacs.
;; 
;; csound-pub.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; csound-pub.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Author: Stéphane Rollandin <hepta@zogotounga.net>
;;---------

;;; Commentary:
;;-------------

;;    full documentation here:
;;    http://www.zogotounga.net/comp/csoundx_doc_csd.html

;;
;;; Installation:
;;                  
;; ==========================================================
;; this file should be installed through the csound-x package
;; ==========================================================
;;

;; last modified October 1, 2010

;;;=====================================================================
;;; Code:

(defun cscsd-publish-csd-file (csd-file &optional audio-file midi-file)
  "Entry point for publishing a CSD as a new work.
Currently only a stub...
"

  (message "CSD publishing is not yet implemented, sorry...")

  "make appropriate directory, possibly archive previous version"

  "copy csd and midi there, check csd for dependencies, set ksr ?"

  "render audio there, in as many formats as required"

  "record info (date, software versions)"

  "possibly produce a standalone executable ?"

  "upload ? email someone ?"

  "open directory ?"
  )


;;======================================================================
;; this is it

(provide 'csound-pub)

;; csound-pub.el ends here




