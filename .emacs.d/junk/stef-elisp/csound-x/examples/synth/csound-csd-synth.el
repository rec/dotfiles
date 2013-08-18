
;; Template for a generic CSD-based MIDI syntheziser 
;; =================================================

;; this was only tested on Windows.
;; to be used through this kind of shell command:
;;
;;   emacs.exe -batch --load=csound-csd-synth.el %1 %2 %3 %4 %5 %6 %7 %8 %9

;; write down your own settings below
;;================================================================================
;; system settings:

(setq MIDI-dir "c:/csound/midi/"
      SYNTHS-dir "c:/csound/csd/synths/"

      cscsd-SFDIR "c:/csound/rendered/"
;      cscsd-SSDIR "c:/csound/samples/"
;      cscsd-INCDIR "c:/csound/include/"

      kk-lowkey-executable "c:/audio/keykit/bin/lowkey.exe"
      cscsd-temp-csd-file "c:/csound/csd/tmp.csd"

      cscsd-current-processing "csound wav (sync.)"
      scomx-Cmask-support nil

      max-lisp-eval-depth 500  ; ?
)

;;================================================================================

(load "csoundx.emacs")  ;; this file should be in your path, along with all stef-elisp

;; you may also add here a few lines of code in order to load any extra Emacs Lisp 
;; code you might need to run your synthesizer


(defun command-line-arg-with-extension (ext)
  (find ext command-line-args 
	:test (lambda (x str) (string-match (concat "\\." x "$") str))))

(defun write-messages-log ()
  (switch-to-buffer "*Messages*")
  (write-file (expand-file-name "csound-csd-synth-messages.txt" SYNTHS-dir)))


(let ((csd-file (command-line-arg-with-extension "csd"))
      (wav-file (command-line-arg-with-extension "wav"))
      (midi-file (command-line-arg-with-extension "mid")))

  " Evaluate the first <ELISP> area in CSD-FILE
The code there is supposed to read and compile MIDI-FILE
The compiled WAV will be located in the SFDIR directory if its path is not defined"
  
  (unless (and csd-file midi-file
	       (file-exists-p (setq csd-file (expand-file-name csd-file SYNTHS-dir)))
	       (file-exists-p (setq midi-file (expand-file-name midi-file MIDI-dir))))
    (message "midi-file or csd-file missing !")
    (message (prin1-to-string command-line-args))
    (write-messages-log)
    (kill-emacs))

  (message "Using %s" csd-file)

  ;; if no wav filename, play audio to DAC:
  (if wav-file
      (progn
	(setq wav-file (expand-file-name wav-file cscsd-SFDIR))
	(message "Synthesizing %s as %s" midi-file wav-file))
    (setq cscsd-current-processing "csound dac")
    (message "Synthesizing %s (to DAC)" midi-file))

  ;; creating the score:
  (find-file csd-file)
  (goto-char (point-min))
  (condition-case error-spec
      (progn
	(search-forward "<ELISP>")
	(eval-this-embedded-elisp))
    (error (progn 
	     (write-file (expand-file-name "csound-csd-synth-error-there.csd" SYNTHS-dir))
	     (message (prin1-to-string error-spec))
	     (write-messages-log)
	     (unless (y-or-n-p "Error while processing <ELISP> area. Proceed ? ")
	       (kill-emacs)))))

  ;; compiling it:
  (save-buffer)
  (condition-case error-spec
      (progn
	(cscsd-process)
	(when wav-file
	  (rename-file (expand-file-name cscsd-latest-audio-file cscsd-SFDIR) wav-file t)))
    (error (progn 
	     (write-file (expand-file-name "csound-csd-synth-error-there.csd" SYNTHS-dir))
	     (message "Error while invoking Csound !")
	     (message (prin1-to-string error-spec))
	     (write-messages-log)
	     (kill-emacs))))

  (if wav-file
      (message "Wrote %s" wav-file)
    (message "Done"))

  (write-messages-log))