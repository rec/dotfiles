;; Mode for editing Csound Orchestras
;; John Fitch 26 Sep 1996

;; Copyright (C) 1996/97 by Codemist Ltd

;; Author: John Fitch <jpff@maths.bath.ac.uk>
;; Keywords: Csound, orchestra
;; Version: 1.16
(defconst csound-orc-version " 1.18 (cs 4.18)")

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

;; Revisions: Keywords added 9 Oct 96
;;            Font-lock added 25 Oct 96
;;            Font-lonk greatly improved 5 Dec 96
;;            and also completed patterns and generalised
;;            Bug report submitted 9 Dec 96
;;            Version 1.01 released
;;            Protect against non-bold fonts
;;            Version 1.02 released
;;            Corrected typing errors 16 Jan 97
;;            Correct patterns for input/output opcodes
;;            Version 1.03 released
;;            Improved documentation
;;            Added some missing opcodes (ihold, oscil,..)
;;            Corrected ADI opcodes for oscil
;;            Font lock for instr and endin
;;            Removed hilit19 stuff and corrected opcodes to manual
;;            Version 1.05 released
;;            More opcodes added
;;            and more
;;            Opcodes for 3.50 and some minor fixes
;;            Opcode names for 3.53
;;	      Added xemacs support and power-of-2 function
;;            Allow numbers as well as letters for number of channels, 
;;              also add 6 and 8
;;            New opcodes
;;            Template expansion system started
;;            Support for documentation

;; Notes: This code has font-lock support
;; Pattern compilation uses make-regexp code

;;; Things I would like:
;; 1. Flexible (user controlled) layout
;; 2. Insertion of opcodes by templates

;;; I N S T A L L A T I O N
;;; In order to arrange for autoloading add the following to .emacs
;;; (setq auto-mode-alist (cons '("\\.orc$" . csound-orc-mode) auto-mode-alist))
;;; (autoload 'csound-orc-mode "csound-orc" "Csound Orchestra major mode." t)
;;;
;;; The running of the mode can be adjusted by various flags best set in
;;; the mode hook.  A typical example is
;;;
;;; (setq csound-orc-mode-hook
;;;       '(lambda ()
;;;          (setq font-lock-maximum-decoration t)
;;;          (turn-on-font-lock)
;;;          (setq csound-orc-mode-do-write-file-hooks t)
;;;          (setq csound-orc-mode-build-new-buffer t)
;;;          (setq csound-orc-mode-address-string "jpff@maths.bath.ac.uk")
;;; ))
;;

;;;
;;; font-lock fixes and menu-support for XEmacs
;;; added by Steve Kersten 
;;; contact steve-k@gmx.net
;;;

(require 'tempo)
(autoload 'make-regexp "make-regexp"    ; We use this sometimes
   "Return a regexp to match a string item in STRINGS.")

;(autoload 'make-regexps "make-regexp"
;   "Return a regexp to REGEXPS.")

;; I recommend you turn these on.

(defvar csound-orc-mode-do-write-file-hooks nil
  "*If not nil, then csound-orc-mode will do timestamps on file saving.
This is done by modifying the local-write-file-hooks.")

(defvar csound-orc-mode-build-new-buffer nil
  "*If not nil, then new buffers start with csound-orc-mode-new-buffer-strings.")

;; (see also tempo.el)

;; variables to configure

(defvar csound-orc-mode-never-indent nil
  "*If t, the indentation code for csound-orc-mode is turned off.")

;; hooks (see also tempo.el)

(defvar csound-orc-mode-hook nil
  "*Hook run when csound-orc-mode is started.")

(defvar csound-orc-mode-load-hook nil
  "*Hook run when csound-orc-mode is loaded.")

(defvar csound-orc-mode-timestamp-hook 'csound-orc-default-insert-timestamp
  "*Hook called for timestamp insertion.
Override this for your own timestamp styles.")

(defvar csound-orc-comment-col 48
  "*Column at which to align comments.")

(defvar csound-tab-stops '(12 24 36)
  "*Positions for columns layout.")

;; strings you might want to change

(defvar csound-orc-mode-address-string "Orchestra File"
  "*The default author string of each file.")

(defvar csound-orc-mode-new-buffer-template
  '(
    ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n"
    ";;;" csound-orc-mode-address-string "\n"
    csound-orc-timestamp-start
    csound-orc-timestamp-end
    ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n"
    "\n\n"
    "\tsr\t=\t" (int-to-string csound-orc-mode-sr) "\n"
    "\tkr\t=\t" (int-to-string csound-orc-mode-kr) "\n"
    "\tksmps\t=\t" (int-to-string csound-orc-mode-ksmps) "\n"
    "\tnchnls\t=\t" (int-to-string csound-orc-mode-nchnls) "\n\n"
    p
    )
  "*Template for new buffers.
Inserted by csound-orc-insert-new-buffer-strings if 
csound-orc-mode-build-new-buffer is set to t")

(defvar csound-orc-timestamp-start ";****++++\n"
  "*Delimiter for timestamps.
Everything between csound-orc-timestamp-start and csound-orc-timestamp-end will
be deleted and replaced with the output of the function
csound-orc-insert-timestamp if csound-orc-mode-do-write-file-hooks is t")

(defvar csound-orc-timestamp-end ";****----\n"
  "*Delimiter for timestamps.
Everything between csound-orc-timestamp-start and csound-orc-timestamp-end will
be deleted and replaced with the output of the function
csound-orc-insert-timestamp if csound-orc-mode-do-write-file-hooks is t")

(defvar csound-orc-keywords-msg ";;Instrument keywords: "
  "*String to introduce keywords in an instrument.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of user customisable variables.
;;; Use the functional interface to change the opcodes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar cs-running-xemacs (string-match "XEmacs\\|Lucid" emacs-version)
  "Set if running on XEmacs")

;; Csound-orc basic keymap
(defvar csound-orc-mode-map (make-sparse-keymap) "Keymap for csound-orc-mode")

;; special mode keys
(define-key csound-orc-mode-map ":"        'csound-colon)
(define-key csound-orc-mode-map ";"        'csound-comment)
(define-key csound-orc-mode-map "\C-c;"    'comment-region)
(define-key csound-orc-mode-map "\C-c:"    'uncomment-region)
(define-key csound-orc-mode-map "\C-c\C-a" 'csound-ins-add-keyword)
(define-key csound-orc-mode-map "\C-c\C-b" 'csound-set-ksmps)
(define-key csound-orc-mode-map "\C-c\C-c" 'comment-region)
(define-key csound-orc-mode-map "\C-c\C-d" 'csound-ins-kill-keyword)
(define-key csound-orc-mode-map "\C-c\C-g" 'csound-goto-instr)
(define-key csound-orc-mode-map "\C-c\C-i" 'csound-instrument)
(define-key csound-orc-mode-map "\C-c\C-k" 'csound-set-kr)
(define-key csound-orc-mode-map "\C-c\C-n" 'csound-set-nchnls)
(define-key csound-orc-mode-map "\C-c\C-s" 'csound-set-sr)
(define-key csound-orc-mode-map "\C-c\C-w" 'csound-write-instrument)
(define-key csound-orc-mode-map "\C-c\C-r" 'csound-read-instrument)
(define-key csound-orc-mode-map "\C-c\C-d" 'csound-delete-instrument)
(define-key csound-orc-mode-map "\C-c\C-y" 'csound-read-instrument-keyword)
(define-key csound-orc-mode-map "\C-c\C-u" 'csound-csd-quick)
(define-key csound-orc-mode-map "\M-\C-t"
  'csound-orc-insert-timestamp-delimiter-at-point)
(define-key csound-orc-mode-map [(control c) (control ?2)] 'csound-e-power)

;; indentation keys
(define-key csound-orc-mode-map "\t" 'csound-orc-mode-indent-command)
(define-key csound-orc-mode-map "\C-j" 'newline-and-indent)

(mapcar
 (function (lambda (l) (define-key csound-orc-mode-map (car l) (nth 1 l))))
 '(("\M-\C-f" tempo-forward-mark)
   ("\M-\C-b" tempo-backward-mark)
   ("\M-\t"   tempo-complete-tag)))

(defvar csound-orc-mode-syntax-table nil "Syntax table")

(if csound-orc-mode-syntax-table
    ()
  (setq csound-orc-mode-syntax-table
        (make-syntax-table text-mode-syntax-table))
;;
;;  (modify-syntax-entry ?'  "w   " csound-orc-mode-syntax-table)
)


(defvar csound-orc-decls nil
  "*Declaration opcodes")
(setq csound-orc-decls "\\<\\(instr\\|endin\\)\\>")

(defvar csound-orc-equals nil
  "*Assignment operator")
(setq csound-orc-equals "=")



(defvar csound-orc-opcodes nil
  "list of opcodes for indenting")
(defvar csound-orc-opcodes-raw nil "*List of general opcodes")

(setq csound-orc-opcodes-raw
      '(
        "active"
        "add"
        "adsr"
        "adsyn"
        "adsynt"
        "aftouch"
        "alpass"
        "ampdb"
        "ampdbfs"
        "ampmidi"
        "areson"
        "aresonk"
        "atone"
        "atonek"
        "atonex"
        "babo"
        "balance"
        "bbcutm"
        "bbcuts"
        "betarand"
        "bexprnd"
        "biquad"
        "biquada"
        "butbp"
        "butbr"
        "buthp"
        "butlp"
        "butterbp"
        "butterbr"
        "butterhp"
        "butterlp"
        "buzz"
        "cauchy"
        "chanctrl"
        "comb"
        "control"
        "convle"
        "convolve"
        "cps2pch"
        "cpsmidi"
        "cpsmidib"
        "cpstmid"
        "cpstun"
        "cpstuni"
        "cpsxpch"
        "cross2"
        "ctrl14"
        "ctrl21"
        "ctrl7"
        "cuserrnd"
        "dam"
        "dbamp"
        "dbfsamp"
        "dcblock"
        "delay"
        "delay1"
        "delayr"
        "deltap"
        "deltap3"
        "deltapi"
        "deltapn"
        "diff"
        "diskin"
        "distort1"
        "div"
        "divz"
        "downsamp"
        "envlpx"
        "envlpxr"
        "expon"
        "exprand"
        "expseg"
        "expsegr"
        "filelen"
        "filenchnls"
        "filepeak"
        "filesr"
        "filter2"
        "filter2"
        "flanger"
        "fmb3"
        "fmbell"
        "fmmetal"
        "fmpercfl"
        "fmrhode"
        "fmvoice"
        "fmwurlie"
        "fof"
        "fof2"
        "fog"
        "follow"
        "follow2"
        "foscil"
        "foscil3"
        "foscili"
        "ftgen"
        "gain"
        "gauss"
        "gbuzz"
        "gogobel"
        "grain"
        "grain2"
        "grain3"
        "granule"
        "harmon"
        "hrtfer"
        "hsboscil"
        "ictrl14"
        "ictrl21"
        "ictrl7"
        "init"
        "integ"
        "interp"
        "jitter"
        "jitter2"
        "jspline"
        "lfo"
        "limit"
        "line"
        "linen"
        "linenr"
        "lineto"
        "linrand"
        "linseg"
        "linsegr"
        "locsend"
        "locsig"
        "log10"
        "loopseg"
        "lorenz"
        "loscil"
        "loscil3"
        "lowres"
        "lowresx"
        "lpf18"
        "lpfreson"
        "lphasor"
        "lpreson"
        "lpshold"
        "madsr"
        "mandol"
        "marimba"
        "midic14"
        "midic21"
        "midic7"
        "midictrl"
        "midictrl"
        "midiin"
        "mirror"
        "moodvcf"
        "moog"
        "mul"
        "multitap"
        "mxads"
        "nestedap"
        "nlfilt"
        "noise"
        "notnum"
        "nreverb"
        "nsamp"
        "ntrpol"
        "octcps"
        "octmidi"
        "octmidib"
        "oscbnk"
        "oscil"
        "oscil1"
        "oscil1i"
        "oscil3"
        "oscili"
        "osciln"
        "oscils"
        "oscilx"
        "pan"
        "pcauchy"
        "pchbend"
        "pchmidi"
        "pchmidib"
        "peak"
        "phasor"
        "phasorbnk"
        "pitch"
        "pitchamdf"
        "planet"
        "pluck"
        "poisson"
        "port"
        "portk"
        "pow"
        "pvadd"
        "pvcross"
        "pvinterp"
        "pvoc"
        "pvread"
        "pvsadsyn"
        "pvsanal"
        "pvscross"
        "pvsfread"
        "pvsftw"
        "pvsinfo"
        "pvsmaska"
        "pvsynth"
        "rand"
        "randh"
        "randi"
        "readclock"
        "release"
        "repluck"
        "reson"
        "resonk"
        "resonx"
        "reverb"
        "reverb2"
        "rezzy"
        "rms"
        "rnd31"
        "s16b14"
        "s32b14"
        "samphold"
        "scans"
        "setctrl"
        "sfinstr"
        "sfinstrm"
        "sfload"
        "sfplay"
        "sfplaym"
        "sfpreset"
        "shaker"
        "slider16"
        "slider16f"
        "slider32"
        "slider32f"
        "slider64"
        "slider64f"
        "slider8"
        "slider8f"
        "sndwarp"
        "sndwarpst"
        "space"
        "spat3d"
        "spat3di"
        "spdist"
        "specaddm"
        "specdiff"
        "specfilt"
        "spechist"
        "specptrk"
        "specscal"
        "specsum"
        "spectrum"
        "spsend"
        "streson"
        "sub"
        "table"
        "table3"
        "table3kt"
        "tablei"
        "tableikt"
        "tablekt"
        "tableng"
        "tablera"
        "tablewa"
        "tablexkt"
        "taninv2"
        "tbvcf"
        "tempest"
        "tempoval"
        "timeinstk"
        "timeinsts"
        "timek"
        "times"
        "tival"
        "tlineto"
        "tone"
        "tonek"
        "tonex"
        "transeg"
        "trigger"
        "trirand"
        "unirand"
        "upsamp"
        "valpass"
        "vco"
        "vcomb"
        "vdelay"
        "vdelay3"
        "veloc"
        "vibes"
        "vibr"
        "vibrato"
        "vlowres"
        "voice"
        "vpvoc"
        "waveset"
        "weibull"
        "wgbow"
        "wgbrass"
        "wgclar"
        "wgflute"
        "wgpluck"
        "wgpluck2"
        "wguide1"
        "wguide2"
        "wrap"
        "xadsr"
        "xscans"
        "zamod"
        "zar"
        "zarg"
        "zfilter2"
        "zir"
        "zkmod"
        "zkr"
        ))

;;; Ordinary Opcodes
(quote
 (setq max-specpdl-size 1500)
)
(quote
 (concat "\\<\\(" (make-regexp csound-orc-opcodes-raw) "\\)\\>")
)

(setq csound-orc-opcodes "\\<\\(a\\(ctive\\|d\\(d\\|s\\(r\\|ynt?\\)\\)\\|ftouch\\|lpass\\|mp\\(db\\(\\|fs\\)\\|midi\\)\\|resonk?\\|tone\\(\\|[kx]\\)\\)\\|b\\(a\\(bo\\|lance\\)\\|bcut[ms]\\|e\\(tarand\\|xprnd\\)\\|iquada?\\|u\\(t\\(b[pr]\\|hp\\|lp\\|ter\\(b[pr]\\|hp\\|lp\\)\\)\\|zz\\)\\)\\|c\\(auchy\\|hanctrl\\|o\\(mb\\|n\\(trol\\|v\\(le\\|olve\\)\\)\\)\\|ps\\(2pch\\|midib?\\|t\\(mid\\|uni?\\)\\|xpch\\)\\|ross2\\|trl\\(14\\|21\\|7\\)\\|userrnd\\)\\|d\\(am\\|b\\(amp\\|fsamp\\)\\|cblock\\|el\\(ay\\(\\|[1r]\\)\\|tap\\(\\|[3in]\\)\\)\\|i\\(ff\\|s\\(kin\\|tort1\\)\\|vz?\\)\\|ownsamp\\)\\|e\\(nvlpxr?\\|xp\\(on\\|rand\\|segr?\\)\\)\\|f\\(il\\(e\\(len\\|nchnls\\|peak\\|sr\\)\\|ter2\\)\\|langer\\|m\\(b\\(3\\|ell\\)\\|metal\\|percfl\\|rhode\\|voice\\|wurlie\\)\\|o\\([fg]\\|f2\\|llow2?\\|scil\\(\\|[3i]\\)\\)\\|tgen\\)\\|g\\(a\\(in\\|uss\\)\\|buzz\\|ogobel\\|ra\\(in\\(\\|[23]\\)\\|nule\\)\\)\\|h\\(armon\\|rtfer\\|sboscil\\)\\|i\\(ctrl\\(14\\|21\\|7\\)\\|n\\(it\\|te\\(g\\|rp\\)\\)\\)\\|j\\(itter2?\\|spline\\)\\|l\\(fo\\|i\\(mit\\|n\\(e\\(\\|nr?\\|to\\)\\|rand\\|segr?\\)\\)\\|o\\(cs\\(end\\|ig\\)\\|g10\\|opseg\\|renz\\|scil3?\\|wresx?\\)\\|p\\(f\\(18\\|reson\\)\\|hasor\\|reson\\|shold\\)\\)\\|m\\(a\\(dsr\\|ndol\\|rimba\\)\\|i\\(di\\(c\\(14\\|21\\|7\\|trl\\)\\|in\\)\\|rror\\)\\|oo\\(dvcf\\|g\\)\\|ul\\(\\|titap\\)\\|xads\\)\\|n\\(estedap\\|lfilt\\|o\\(ise\\|tnum\\)\\|reverb\\|samp\\|trpol\\)\\|o\\(ct\\(cps\\|midib?\\)\\|sc\\(bnk\\|il\\(\\|[13insx]\\|1i\\)\\)\\)\\|p\\(an\\|c\\(auchy\\|h\\(bend\\|midib?\\)\\)\\|eak\\|hasor\\(\\|bnk\\)\\|itch\\(\\|amdf\\)\\|l\\(anet\\|uck\\)\\|o\\(isson\\|rtk?\\|w\\)\\|v\\(add\\|cross\\|interp\\|oc\\|read\\|s\\(a\\(dsyn\\|nal\\)\\|cross\\|f\\(read\\|tw\\)\\|info\\|maska\\|ynth\\)\\)\\)\\|r\\(and\\(\\|[hi]\\)\\|e\\(adclock\\|lease\\|pluck\\|son\\(\\|[kx]\\)\\|verb2?\\|zzy\\)\\|ms\\|nd31\\)\\|s\\(16b14\\|32b14\\|amphold\\|cans\\|etctrl\\|f\\(instrm?\\|load\\|p\\(laym?\\|reset\\)\\)\\|haker\\|lider\\(16f?\\|32f?\\|64f?\\|8f?\\)\\|ndwarp\\(\\|st\\)\\|p\\(a\\(ce\\|t3di?\\)\\|dist\\|ec\\(addm\\|diff\\|filt\\|hist\\|ptrk\\|s\\(cal\\|um\\)\\|trum\\)\\|send\\)\\|treson\\|ub\\)\\|t\\(a\\(ble\\(\\|[3i]\\|3kt\\|ikt\\|kt\\|ng\\|ra\\|wa\\|xkt\\)\\|ninv2\\)\\|bvcf\\|emp\\(est\\|oval\\)\\|i\\(me\\([ks]\\|inst[ks]\\)\\|val\\)\\|lineto\\|one\\(\\|[kx]\\)\\|r\\(anseg\\|i\\(gger\\|rand\\)\\)\\)\\|u\\(nirand\\|psamp\\)\\|v\\(alpass\\|co\\(\\|mb\\)\\|delay3?\\|eloc\\|ib\\(es\\|r\\(\\|ato\\)\\)\\|lowres\\|oice\\|pvoc\\)\\|w\\(aveset\\|eibull\\|g\\(b\\(ow\\|rass\\)\\|clar\\|flute\\|pluck2?\\|uide[12]\\)\\|rap\\)\\|x\\(adsr\\|scans\\)\\|z\\(a\\(mod\\|rg?\\)\\|filter2\\|ir\\|k\\(mod\\|r\\)\\)\\)\\>"
)


;;; Void opcodes
(defvar csound-orc-opcodes0 nil
  "list of void opcodes for indenting.")
(defvar csound-orc-opcodes0-raw nil
  "*List of opcodes with no result.")
(setq csound-orc-opcodes0-raw
      '(
        "cdisplay"
        "clockoff"
        "clockon"
        "cpuprc"
        "ctrlinit"
        "delayw"
        "event"
        "flashtext"
        "ftmorf"
        "ihold"
        "initc14"
        "initc21"
        "initc7"
        "lpinterp"
        "lpslot"
        "massign"
        "maxalloc"
        "mclock"
        "mdelay"
        "midion"
        "midion2"
        "midiout"
        "moscil"
        "mrtmsg"
        "noteoff"
        "noteon"
        "nrpn"
        "ondur"
        "ondur2"
        "outiat"
        "outkat"
        "prealloc"
        "pset"
        "pvbufread"
        "pvsftr"
        "scanu"
        "schedkwhen"
        "schedule"
        "schedwhen"
        "seed"
        "sfilist"
        "sfpassign"
        "sfplist"
        "spat3dt"
        "specdisp"
        "strset"
        "tablecopy"
        "tablegpw"
        "tableicopy"
        "tableigpw"
        "tableimix"
        "tableiw"
        "tablemix"
        "tableseg"
        "tablew"
        "tablewkt"
        "tablexseg"
        "tempo"
        "turnoff"
        "turnon"
        "xscanu"
        "xtratim"
        "zacl"
        "zakinit"
        "zaw"
        "zawm"
        "ziw"
        "ziwm"
        "zkcl"
        "zkw"
        "zkwm"
))

(quote
 (concat "\\b\\(" (make-regexp csound-orc-opcodes0-raw ) "\\)\\b")
)

(setq csound-orc-opcodes0 "\\b\\(c\\(display\\|locko\\(ff\\|n\\)\\|puprc\\|trlinit\\)\\|delayw\\|event\\|f\\(lashtext\\|tmorf\\)\\|i\\(hold\\|nitc\\(14\\|21\\|7\\)\\)\\|lp\\(interp\\|slot\\)\\|m\\(a\\(ssign\\|xalloc\\)\\|clock\\|delay\\|idio\\(n2?\\|ut\\)\\|oscil\\|rtmsg\\)\\|n\\(oteo\\(ff\\|n\\)\\|rpn\\)\\|o\\(ndur2?\\|ut\\(iat\\|kat\\)\\)\\|p\\(realloc\\|set\\|v\\(bufread\\|sftr\\)\\)\\|s\\(c\\(anu\\|hed\\(kwhen\\|ule\\|when\\)\\)\\|eed\\|f\\(ilist\\|p\\(assign\\|list\\)\\)\\|p\\(at3dt\\|ecdisp\\)\\|trset\\)\\|t\\(able\\(copy\\|gpw\\|i\\(copy\\|gpw\\|mix\\|w\\)\\|mix\\|seg\\|w\\(\\|kt\\)\\|xseg\\)\\|empo\\|urno\\(ff\\|n\\)\\)\\|x\\(scanu\\|tratim\\)\\|z\\(a\\(cl\\|kinit\\|wm?\\)\\|iwm?\\|k\\(cl\\|wm?\\)\\)\\)\\b"
)

;;; Input/Output
(defvar csound-orc-iocodes nil
  "list of I/O opcodes for indenting")
(defvar csound-orc-iocodes-raw nil "list of I/O opcodes.")
(setq csound-orc-iocodes-raw '(
      "in"
      "in32"
      "inc"
      "inh"
      "ino"
      "inq"
      "ins"
      "inx"
      "lpread"
      "readk"
      "readk2"
      "readk3"
      "readk4"
      "sensekey"
      "soundin"
      "xyin"
      ))
(quote
   (concat "\\b\\(" (make-regexp csound-orc-iocodes-raw) "\\)\\b")
)
(setq csound-orc-iocodes
"\\b\\(in\\(\\|[choqsx]\\|32\\)\\|lpread\\|readk\\(\\|[234]\\)\\|s\\(ensekey\\|oundin\\)\\|xyin\\)\\b"
)


;;; Input/Output Void
(defvar csound-orc-iocodes0 nil
  "list of void I/O opcodes for indenting")
(defvar csound-orc-iocodes0-raw nil  "*List of I/O opcodes with no result.")
(setq csound-orc-iocodes0-raw
    '(
        "dispfft"
        "display"
        "dumpk"
        "dumpk2"
        "dumpk3"
        "dumpk4"
        "inz"
        "out"
        "out32"
        "outc"
        "outch"
        "outh"
        "outic"
        "outic14"
        "outipat"
        "outipb"
        "outipc"
        "outkc"
        "outkc14"
        "outkpat"
        "outkpb"
        "outkpc"
        "outo"
        "outq"
        "outq1"
        "outq2"
        "outq3"
        "outq4"
        "outs"
        "outs1"
        "outs2"
        "outx"
        "outz"
        "print"
        "printk"
        "printk2"
        "printks"
        "soundout"
      ))
(quote
   (concat "\\b\\(" (make-regexp csound-orc-iocodes0-raw) "\\)\\b")
)
(setq csound-orc-iocodes0
"\\b\\(d\\(isp\\(fft\\|lay\\)\\|umpk\\(\\|[234]\\)\\)\\|inz\\|out\\(\\|[choqsxz]\\|32\\|ch\\|i\\(c\\(\\|14\\)\\|p\\([bc]\\|at\\)\\)\\|k\\(c\\(\\|14\\)\\|p\\([bc]\\|at\\)\\)\\|q[1234]\\|s[12]\\)\\|print\\(\\|k\\(\\|[2s]\\)\\)\\|soundout\\)\\b"
)

;;; Conditionals (always void)
(defvar csound-orc-opcodesc nil
  "list of conditional opcodes for indenting")
(defvar csound-orc-opcodesc-raw "*List of conditional opcodes")
(setq csound-orc-opcodesc-raw '(
      "gcgoto"          "goto"          "icgoto"        "igoto"
      "kcgoto"          "kgoto"         "reinit"        "rigoto"
      "rireturn"        "tigoto"        "timout"
      ))
(quote
   (concat "\\b\\(" (make-regexp csound-orc-opcodesc-raw) "\\)\\b")
)
(setq csound-orc-opcodesc
      "\\b\\(g\\(cgoto\\|oto\\)\\|i\\(cgoto\\|goto\\)\\|k\\(cgoto\\|goto\\)\\|r\\(einit\\|i\\(goto\\|return\\)\\)\\|ti\\(goto\\|mout\\)\\)\\b")

;;; ADI Opcodes

(defvar csound-orc-opcodes-adi nil
  "list of ADI opcodes for indenting")
(defvar csound-orc-opcodes-adi-raw nil "*List of ADI opcodes.")
(setq csound-orc-opcodes-adi-raw
      '(
        "addin"                         "autopgms"      "chanctrl"
        "chorus1"       "chorus2"       "coscil"        "dexponr"
        "doscilp"       "dpexclus"      "dpkeys"        "dpolaps"
        "dsctrl"        "dsctrlmap"     "dtable"        "envlpxr"
        "filter"        "flange1"       "flange2"       "ftload"
        "ftscale"       "ftsplit"       "ftstep"        "harmon2"
        "harmon2"       "harmon3"       "iftime"        "loscil1"
        "loscil2"       "lrghall"       "maddin"        "mtsplit"
        "octdown"       "octup"         "pluck2"        "polyaft"
        "timegate"      "uctrlmap"      "veloffs"       "woscil"
      ))

(quote
   (concat "\\b\\("
           (make-regexp csound-orc-opcodes-adi-raw)
           "\\)\\b")
)
(setq csound-orc-opcodes-adi "\\b\\(a\\(ddin\\|utopgms\\)\\|c\\(h\\(anctrl\\|orus[12]\\)\\|oscil\\)\\|d\\(exponr\\|oscilp\\|p\\(exclus\\|keys\\|olaps\\)\\|sctrl\\(\\|map\\)\\|table\\)\\|envlpxr\\|f\\(ilter\\|lange[12]\\|t\\(load\\|s\\(cale\\|plit\\|tep\\)\\)\\)\\|harmon[23]\\|iftime\\|l\\(oscil[12]\\|rghall\\)\\|m\\(addin\\|tsplit\\)\\|oct\\(down\\|up\\)\\|p\\(luck2\\|olyaft\\)\\|timegate\\|uctrlmap\\|veloffs\\|woscil\\)\\b"
)

;;; ADI Opcodes Void
(defvar csound-orc-opcodes0-adi nil
  "list of ADI void opcodes for indenting")
(defvar csound-orc-opcodes0-adi-raw nil
  "*List of ADI void opcodes for indenting")
(setq csound-orc-opcodes0-adi-raw
      '(
        "outs12"        "panouts"       "clkon"         "clkoff"
        "send81"        "send81r"
      ))
(quote
   (concat "\\b\\(" (make-regexp csound-orc-opcodes0-adi-raw) "\\)\\b")
   )
(setq csound-orc-opcodes0-adi
      "\\b\\(clko\\(ff\\|n\\)\\|outs12\\|panouts\\|send81r?\\)\\b")

;;; ADI Input/Output
(defvar csound-orc-iocodes-adi nil
  "list of ADI I/O opcodes for indenting")
(defvar csound-orc-iocodes-adi-raw  '(
      )
  "*List of ADI I/O opcodes")
(quote
   (concat "\\b\\(" (make-regexp csound-orc-iocodes-adi-raw) "\\)\\b")
   )
(setq csound-orc-iocodes-adi "\\b\\b")

;;; ADI Input/Output Void
(defvar csound-orc-iocodes0-adi nil
  "list of ADI void I/O opcodes for indenting")
(defvar csound-orc-iocodes0-adi-raw  '(
      "panouts"
      )
  "*List of ADI I/O opcodes with no result")
(quote
   (concat "\\b\\(" (make-regexp csound-orc-iocodes0-adi-raw) "\\)\\b")
   )
(setq  csound-orc-iocodes0-adi  "\\b\\(panouts\\)\\b")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions to maintain opcodes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar csound-orc-opcodes-change nil "Set when list changes")
(defvar csound-orc-opcodes0-change nil "Set when list changes")
(defvar csound-orc-iocodes-change nil "Set when list changes")
(defvar csound-orc-iocodes0-change nil "Set when list changes")
(defvar csound-orc-opcodesc-change nil "Set when list changes")
(defvar csound-orc-opcodes-adi-change nil "Set when list changes")
(defvar csound-orc-opcodes0-adi-change nil "Set when list changes")
(defvar csound-orc-iocodes-adi-change nil "Set when list changes")
(defvar csound-orc-iocodes0-adi-change nil "Set when list changes")

(defun delete-from-list (x l)
  (cond ((null l) nil)
        ((equal x (car l)) (delete-from-list x (cdr l)))
        (t (cons (car l) (delete-from-list x (cdr l))))))

(defun add-opcode (x)
  (if (symbolp x) (add-opcode (symbol-name x))
    (let ((old csound-orc-opcodes-raw))
      (setq csound-orc-opcodes-raw
            (sort (add-to-list x csound-orc-opcodes-raw) 'string<))
      (if (not (equal old csound-orc-opcodes-raw))
          (setq csound-orc-opcodes-change t)))))

(defun delete-opcode (x)
  (if (symbolp x) (delete-opcode (symbol-name x))
    (let ((old csound-orc-opcodes-raw))
      (setq csound-orc-opcodes-raw
            (sort (delete-from-list x csound-orc-opcodes-raw) 'string<))
      (if (not (equal old csound-orc-opcodes-raw))
          (setq csound-orc-opcodes-change t)))))

(defun add-opcode0 (x)
  (if (symbolp x) (add-opcode0 (symbol-name x))
    (let ((old csound-orc-opcodes0-raw))
      (setq csound-orc-opcodes0-raw
            (sort (add-to-list x csound-orc-opcodes0-raw) 'string<))
      (if (not (equal old csound-orc-opcodes0-raw))
          (setq csound-orc-opcodes0-change t)))))

(defun delete-opcode0 (x)
  (if (symbolp x) (delete-opcode0 (symbol-name x))
    (let ((old csound-orc-opcodes0-raw))
      (setq csound-orc-opcodes0-raw
            (sort (delete-from-list x csound-orc-opcodes0-raw) 'string<))
      (if (not (equal old csound-orc-opcodes0-raw))
          (setq csound-orc-opcodes0-change t)))))

(defun add-iocode (x)
  (if (symbolp x) (add-iocode (symbol-name x))
    (let ((old csound-orc-iocodes-raw))
      (setq csound-orc-iocodes-raw
            (sort (add-to-list x csound-orc-iocodes-raw) 'string<))
      (if (not (equal old csound-orc-iocodes-raw))
          (setq csound-orc-iocodes-change t)))))

(defun delete-iocode (x)
  (if (symbolp x) (delete-iocode (symbol-name x))
    (let ((old csound-orc-iocodes-raw))
      (setq csound-orc-iocodes-raw
            (sort (delete-from-list x csound-orc-iocodes-raw) 'string<))
      (if (not (equal old csound-orc-iocodes-raw))
          (setq csound-orc-iocodes-change t)))))

(defun add-iocode0 (x)
  (if (symbolp x) (add-iocode0 (symbol-name x))
    (let ((old csound-orc-iocodes0-raw))
      (setq csound-orc-iocodes0-raw
            (sort (add-to-list x csound-orc-iocodes0-raw) 'string<))
      (if (not (equal old csound-orc-iocodes0-raw))
          (setq csound-orc-iocodes0-change t)))))

(defun delete-iocode0 (x)
  (if (symbolp x) (delete-iocode0 (symbol-name x))
    (let ((old csound-orc-iocodes0-raw))
      (setq csound-orc-iocodes0-raw
            (sort (delete-from-list x csound-orc-iocodes0-raw) 'string<))
      (if (not (equal old csound-orc-iocodes0-raw))
          (setq csound-orc-iocodes0-change t)))))

(defun add-opcodesc (x)
  (if (symbolp x) (add-opcodesc (symbol-name x))
    (let ((old csound-orc-opcodesc-raw))
      (setq csound-orc-opcodesc-raw
            (sort (add-to-list x csound-orc-opcodesc-raw) 'string<))
      (if (not (equal old csound-orc-opcodesc-raw))
          (setq csound-orc-opcodesc-change t)))))

(defun delete-opcodesc (x)
  (if (symbolp x) (delete-opcodesc (symbol-name x))
    (let ((old csound-orc-opcodesc-raw))
      (setq csound-orc-opcodesc-raw
            (sort (delete-from-list x csound-orc-opcodesc-raw) 'string<))
      (if (not (equal old csound-orc-opcodesc-raw))
          (setq csound-orc-opcodesc-change t)))))

(defun add-opcode-adi (x)
  (if (symbolp x) (add-opcode-adi (symbol-name x))
    (let ((old csound-orc-opcodes-adi-raw))
      (setq csound-orc-opcodes-adi-raw
            (sort (add-to-list x csound-orc-opcodes-adi-raw) 'string<))
      (if (not (equal old csound-orc-opcodes-adi-raw))
          (setq csound-orc-opcodes-adi-change t)))))

(defun delete-opcode-adi (x)
  (if (symbolp x) (delete-opcode-adi (symbol-name x))
    (let ((old csound-orc-opcodes-adi-raw))
      (setq csound-orc-opcodes-adi-raw
            (sort (delete-from-list x csound-orc-opcodes-adi-raw) 'string<))
      (if (not (equal old csound-orc-opcodes-adi-raw))
          (setq csound-orc-opcodes-adi-change t)))))

(defun add-opcode0-adi (x)
  (if (symbolp x) (add-opcode0-adi (symbol-name x))
    (let ((old csound-orc-opcodes0-adi-raw))
      (setq csound-orc-opcodes-adi-raw
            (sort (add-to-list x csound-orc-opcodes0-adi-raw) 'string<))
      (if (not (equal old csound-orc-opcodes0-adi-raw))
          (setq csound-orc-opcodes0-adi-change t)))))

(defun delete-opcode0-adi (x)
  (if (symbolp x) (delete-opcode0-adi (symbol-name x))
    (let ((old csound-orc-opcodes0-adi-raw))
      (setq csound-orc-opcodes-adi-raw
            (sort (delete-from-list x csound-orc-opcodes0-adi-raw) 'string<))
      (if (not (equal old csound-orc-opcodes0-adi-raw))
          (setq csound-orc-opcodes0-adi-change t)))))

(defun add-iocode-adi (x)
  (if (symbolp x) (add-iocode-adi (symbol-name x))
    (let ((old csound-orc-iocodes-adi-raw))
      (setq csound-orc-iocodes-adi-raw
            (sort (add-to-list x csound-orc-iocodes-adi-raw) 'string<))
      (if (not (equal old csound-orc-iocodes-adi-raw))
          (setq csound-orc-iocodes-adi-change t)))))

(defun delete-iocode-adi (x)
  (if (symbolp x) (delete-iocode-adi (symbol-name x))
    (let ((old csound-orc-iocodes-adi-raw))
      (setq csound-orc-iocodes-adi-raw
            (sort (delete-from-list x csound-orc-iocodes-adi-raw) 'string<))
      (if (not (equal old csound-orc-iocodes-adi-raw))
          (setq csound-orc-iocodes-adi-change t)))))

(defun add-iocode0-adi (x)
  (if (symbolp x) (add-iocode0-adi (symbol-name x))
    (let ((old csound-orc-iocodes0-adi-raw))
      (setq csound-orc-iocodes0-adi-raw
            (sort (add-to-list x csound-orc-iocodes0-adi-raw) 'string<))
      (if (not (equal old csound-orc-iocodes0-adi-raw))
          (setq csound-orc-iocodes0-adi-change t)))))

(defun delete-iocode0-adi (x)
  (if (symbolp x) (delete-iocode0-adi (symbol-name x))
    (let ((old csound-orc-iocodes0-adi-raw))
      (setq csound-orc-iocodes0-adi-raw
            (sort (delete-from-list x csound-orc-iocodes0-adi-raw) 'string<))
      (if (not (equal old csound-orc-iocodes0-adi-raw))
          (setq csound-orc-iocodes0-adi-change t)))))

(defun make-opcode-patterns ()
  "If the lists of opcodes have changed remake the patterns"
  (if csound-orc-opcodes-change
      (setq csound-orc-opcodes
            (concat "\\<\\(" (make-regexp csound-orc-opcodes-raw) "\\)\\>")))
  (if csound-orc-opcodes0-change
      (setq csound-orc-opcodes0
            (concat "\\<\\(" (make-regexp csound-orc-opcodes0-raw) "\\)\\>")))
  (if csound-orc-iocodes-change
      (setq csound-orc-iocodes
            (concat "\\<\\(" (make-regexp csound-orc-iocodes-raw) "\\)\\>")))
  (if csound-orc-iocodes0-change
      (setq csound-orc-iocodes0
            (concat "\\<\\(" (make-regexp csound-orc-iocodes0-raw) "\\)\\>")))
  (if csound-orc-opcodesc-change
      (setq csound-orc-opcodesc
            (concat "\\<\\(" (make-regexp csound-orc-opcodesc-raw) "\\)\\>")))
  (if csound-orc-opcodes-adi-change
      (setq csound-orc-opcodes-adi
            (concat "\\<\\(" (make-regexp csound-orc-opcodes-adi-raw) "\\)\\>")))
  (if csound-orc-opcodes0-adi-change
      (setq csound-orc-opcodes0-adi
            (concat "\\<\\("
                    (make-regexp csound-orc-opcodes0-adi-raw) "\\)\\>")))
  (if csound-orc-iocodes-adi-change
      (setq csound-orc-iocodes-adi
            (concat "\\<\\(" (make-regexp csound-orc-iocodes-adi-raw) "\\)\\>")))
  (if csound-orc-iocodes0-adi-change
      (setq csound-orc-iocodes0-adi
            (concat "\\<\\("
                    (make-regexp csound-orc-iocodes0-adi-raw) "\\)\\>")))
  (setq csound-orc-opcodes-change nil)
  (setq csound-orc-opcodes0-change nil)
  (setq csound-orc-iocodes-change nil)
  (setq csound-orc-iocodes0-change nil)
  (setq csound-orc-opcodesc-change nil)
  (setq csound-orc-opcodes-adi-change nil)
  (setq csound-orc-opcodes0-adi-change nil)
  (setq csound-orc-iocodes-adi-change nil)
  (setq csound-orc-iocodes0-adi-change nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Indentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar csound-orc-search-limit 2000 "limit on how far back we search")

(defvar csound-orc-mode-print-indent-info nil
  "If t, indent will print out information as a message.")

(defun csound-orc-mode-indent-command ()
  "Command for indenting text.
Just calls csound-orc-mode-indent."
  (interactive)
  (csound-orc-mode-indent))

;; some of the ideas are borrowed from cc-mode.el.

(defun csound-orc-mode-indent ()
  "indentation workhorse function."
  (if csound-orc-mode-never-indent ()
    (let ((m (point-marker))
          (ateol (eolp))
          (pp nil))
      ;; unindent the line
      (end-of-line)
      (setq pp (point))
      (beginning-of-line)
      (save-restriction
        (narrow-to-region (point) pp)
        (if (looking-at "^;") ()        ; Comment at start of line is OK
          (delete-region (point) (progn (back-to-indentation) (point)))
          (if (looking-at ";;;") ()     ; Three comments goes to start
            (if (looking-at ";;")
                (indent-to 8)           ;Two to short state
              (if (looking-at "\\sw+:")
                  (progn
                    (search-forward ":")
                    (if (looking-at "[ \t]*;")
                        (progn
                          (delete-horizontal-space)
                          (indent-to comment-column))
                      (if (looking-at "[ \t]*$")
                          (delete-horizontal-space)
                        (forward-char 1)
                        (delete-horizontal-space)
                        (insert "\n")
                        (csound-orc-mode-indent))))
                (if (looking-at "#") ()
            ;;; General case
                  (if (not (looking-at "^[ \t]*$")) ;If empty line do nothing
                      (progn
                        (delete-horizontal-space) ;Remove initial space
                        (if (looking-at "\\bif\\b") ;A Conditional
                            (progn
                              (goto-char (match-end 0)) ;Skip the "if"
                              (delete-horizontal-space)
                              (insert " ")
                              (insert "%") (zap-to-char -1 ?%)
                                        ;Need to skip boolean!!
                              (re-search-forward "[ \t]+")
                              (insert "%") (zap-to-char -1 ?%)
                              (delete-horizontal-space)
                              (tab-to-tab-stop)))
                        (insert "%") (zap-to-char -1 ?%)
                        (if (looking-at csound-orc-decls)
                            (progn
                              (re-search-forward "\\b\\sw+\\b")
                              (insert "%") (zap-to-char -1 ?%)
                              (delete-horizontal-space) ; and retab
                              (insert " ")) ;Need to deal with list here
                          (if (or                 ;If an opcode with no answer
                               (looking-at csound-orc-opcodes0)
                               (looking-at csound-orc-opcodesc)
                               (looking-at csound-orc-opcodes0-adi)
                               (looking-at csound-orc-iocodes0)
                               (looking-at csound-orc-iocodes0-adi))
                              (tab-to-tab-stop)   ;need another tab
                            (insert "%") (zap-to-char -1 ?%)
                            (insert "  ")  ;Space at start of line
                            (re-search-forward "\\b\\sw+\\b") ;or skip answer(s)
                            (if (looking-at ",")  ; A second answer; minimal checks
                                (skip-chars-forward "a-zA-Z0-9,"))
                            (insert "%") (zap-to-char -1 ?%)
                            (delete-horizontal-space) ;and retab
                            (tab-to-tab-stop)
                            (insert "%") (zap-to-char -1 ?%)
                            (if (not
                                 (or
                                  (looking-at csound-orc-equals)
                                  (looking-at csound-orc-opcodes)
                                  (looking-at csound-orc-iocodes)
                                  (looking-at csound-orc-opcodes-adi)
                                  ;;                            (looking-at csound-orc-iocodes-adi)
                                  ))
                                (message "Unknown opcode")))
                                        ;We now need to deal with argument
                          (if (re-search-forward
                               "\\(\\b[a-zA-Z0-9]+\\b\\|=\\)[ \t]*" (point-max) t)
                              (progn
                                (insert "%") (zap-to-char -1 ?%)
                                (if (looking-at "[a-zA-Z0-9(]") ;If an argument
                                    (delete-horizontal-space))
                                (tab-to-tab-stop)))
                          (insert "%") (zap-to-char -1 ?%)
                          (while (and (not (looking-at "[ \t]*;"))
                                      (< (point) (point-max)))
                            (insert "%") (zap-to-char -1 ?%)
                            (if (looking-at "[ \t]")
                                (delete-char 1)
                              (if (looking-at "\"")
                                  (progn (forward-char 1)
                                         (search-forward "\""))
                                (if (not (looking-at "[,?:]"))
                                    (forward-char 1)
                                  (forward-char 1)
                                  (insert " ")))))
                          (if (looking-at "[ \t]*;")        ; if there is a comment left
                              (progn
                                (delete-horizontal-space)
                                (search-forward ";")
                                (backward-char 1)
                                (indent-to
                                 (max (1+ (current-column)) comment-column))))))))))))
        ;; adjust point to where it was before, or at start of indentation
        (if ateol
            (end-of-line)
          (goto-char (marker-position m)))
        )
      (if (< (current-column) (current-indentation))
          (back-to-indentation)))))

(defun csound-orc-mode-comment ()
  (save-excursion
    (beginning-of-line)
    (if (looking-at ";")
        0
      (skip-chars-backward " \t")
      (max (if (bolp) 0 (1+ (current-column)))
           comment-column))))

(defun csound-orc-indent-line ()
  "Indents current Csound line based on its contents."
  (interactive)
  (csound-orc-mode-indent)
;  (let ((csi (calculate-csound-orc-indent)))
;    (save-excursion
;      (beginning-of-line)
;      (if (not (= csi (csound-current-line-indentation)))
;         (indent-to csi)
;       (beginning-of-line)
;       (if (and (not (looking-at "^;"))
;                (csound-find-comment-start-skip))
;           (csound-orc-indent-comment)))))
  )

(defun csound-orc-indent-comment ()
  "Align or create comment on current line.
Existing comments of all types are recognized and aligned.
If the line has no comment, a side-by-side comment is inserted and aligned
if the value of  comment-start  is not nil.
Otherwise, a separate-line comment is inserted, on this line
or on a new line inserted before this line if this line is not blank."
  (interactive)
  (beginning-of-line)
  ;; Recognize existing comments
  (cond ((looking-at ";")
         (csound-orc-indent-line))
        ; otherwise goto end of line or sth else?
        ;; No existing comment.
        ;; If side-by-side comments are defined, insert one,
        ;; unless line is now blank.
        ((and comment-start (not (looking-at "^[ \t]*$")))
         (end-of-line)
         (delete-horizontal-space)
         (indent-to (csound-orc-mode-comment))
         (insert comment-start))
        ;; Else insert separate-line comment, making a new line if nec.
        (t
         (if (looking-at "^[ \t]*$")
             (delete-horizontal-space)
;;         (beginning-of-line)
;;         (insert "\n")
;;         (forward-char -1)
           )
         (insert ";")
         (insert-char '\; (- (calculate-csound-orc-indent) (current-column))))))

(defun csound-find-comment-start-skip ()
  "Move to past `comment-start-skip' found on current line.
Return t if `comment-start-skip' found, nil if not."
;;; In order to move point only if comment-start-skip is found,
;;; this one uses a lot of save-excursions.  Note that re-search-forward
;;; moves point even if comment-start-skip is inside a string-constant.
;;; Some code expects certain values for match-beginning and end
  (interactive)
  (if (save-excursion
        (re-search-forward comment-start-skip
                           (save-excursion (end-of-line) (point)) t))
      (let ((save-match-beginning (match-beginning 0))
            (save-match-end (match-end 0)))
        (goto-char save-match-beginning)
        (re-search-forward comment-start-skip
                           (save-excursion (end-of-line) (point)) t)
        (goto-char (match-end 0))
        t)
    nil))

(defun csound-current-line-indentation ()
  "Indentation of current line
This is the column position of the first non-whitespace character"
  (save-excursion
    (beginning-of-line)
    (cond ((looking-at "^;")
           (goto-char (match-end 0))
           (skip-chars-forward ";")))
    ;; Move past whitespace.
    (skip-chars-forward " \t")
    (current-column)))

(defun calculate-csound-orc-indent ()
  "Calculates the Csound indent column based on previous lines."
  (let (icol)
    (save-excursion
      (if (= (point) (point-min))
          (setq icol 8)
        (save-excursion
          (beginning-of-line)
          (cond ((looking-at "^;")
                 (setq icol 0))
          ;; Move past whitespace.
                (t (skip-chars-forward " \t")
                   (setq icol (current-column))))))
      (save-excursion
        (beginning-of-line)
        icol))))

;;;; Electric characters

(defun csound-colon ()                  ;; Seems to work OK
  "Insert a colon: if it follows a label, delete the label's indentation."
  (interactive)
  (let (ss)
    (save-excursion
      (beginning-of-line)
      (setq ss (point))
      (if (looking-at "[ \t]*\\(\\sw\\|\\s_\\)+")
          (delete-horizontal-space)))
    (insert ":\n")
    (tab-to-tab-stop)
    ))


;; timestamps

(defun csound-orc-update-timestamp ()
  "Basic function for updating timestamps. It finds the timestamp in
the buffer by looking for csound-orc-timestamp-start, deletes all text
up to csound-orc-timestamp-end, and runs csound-orc-mode-timestamp-hook
which will presumably insert an appropriate timestamp in the buffer."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-max))
      (if (not (search-backward csound-orc-timestamp-start nil t))
          (message "timestamp delimiter start was not found")
        (let ((ts-start (+ (point) (length csound-orc-timestamp-start)))
              (ts-end (if (search-forward csound-orc-timestamp-end nil t)
                          (- (point) (length csound-orc-timestamp-end))
                        nil)))
          (if (not ts-end)
              (message "timestamp delimiter end was not found. Type C-c C-t to insert one.")
            (delete-region ts-start ts-end)
            (goto-char ts-start)
            (run-hooks 'csound-orc-mode-timestamp-hook)))))
    nil))

(defun csound-orc-default-insert-timestamp ()
  "Default timestamp insertion function"
  (insert ";**** Last modified: "
          (current-time-string)
          "\n"))

(defun csound-orc-insert-timestamp-delimiter-at-point ()
  "Simple function that inserts timestamp delimiters at point.
Useful for adding timestamps to existing buffers."
  (interactive)
  (insert csound-orc-timestamp-start)
  (insert csound-orc-timestamp-end))

;; csound-orc-insert-new-buffer-strings

(tempo-define-template "csound-skeleton" csound-orc-mode-new-buffer-template
                       nil
                       "Insert a skeleton for a CSOUND document")

(defun csound-orc-insert-new-buffer-strings ()
  "Insert csound-orc-mode-new-buffer-strings."
  (tempo-template-csound-skeleton)
)

(defvar csound-code-level-empty-comment-pattern nil)
(defvar csound-flush-left-empty-comment-pattern nil)
(defvar csound-inline-empty-comment-pattern nil)

;;; Now an attempt at font-lock support

(defconst csound-font-lock-keywords-1 nil
  "Subdued level highlighting for Csound mode.")

(defconst csound-font-lock-keywords-2 nil
  "Medium level highlighting for Csound mode.")

(defconst csound-font-lock-keywords-3 nil
  "Gaudy level highlighting for Csound mode.")

(defconst csound-font-lock-keywords nil
  "Default expressions to highlight in Csound mode.")

(let (
      ;; (concat "\\b\\(" (make-regexp '("sr" "kr" "ksmps" "nchnls")) "\\)\\b")
      (csound-orc-globals "\\b\\(sr\\|k\\(r\\|smps\\)\\|nchnls\\)\\b")
      (csound-orc-labels "^[ \t]*\\sw+[ \t]*:")
      (tmp (quote
            (concat "\\b\\("
                  (make-regexp
                   '(
                     "abs"
                     "ampdb"
                     "ampdbfs"
                     "birnd"
                     "cent"
                     "cos"
                     "cosh"
                     "cosinv"
                     "cpsoct"
                     "cpspch"
                     "db"
                     "dbamp"
                     "dbampfs"
                     "duserrnd"
                     "exp"
                     "frac"
                     "ftlen"
                     "ftlptim"
                     "ftchnls"
                     "i"
                     "int"
                     "log"
                     "octave"
                     "octcps"
                     "octpch"
                     "p"
                     "pchoct"
                     "rnd"
                     "semitone"
                     "sin"
                     "sinh"
                     "sininv"
                     "sqrt"
                     "tan"
                     "tanh"
                     "taninv"
                     "urd"
                     ))
                  "\\)\\b")
           ))
      (csound-orc-funcs
       "\\b\\([ip]\\|a\\(bs\\|mpdb\\(\\|fs\\)\\)\\|birnd\\|c\\(ent\\|os\\(\\|h\\|inv\\)\\|ps\\(oct\\|pch\\)\\)\\|d\\(b\\(\\|amp\\(\\|fs\\)\\)\\|userrnd\\)\\|exp\\|f\\(rac\\|t\\(chnls\\|l\\(en\\|ptim\\)\\)\\)\\|int\\|log\\|oct\\(ave\\|cps\\|pch\\)\\|pchoct\\|rnd\\|s\\(emitone\\|in\\(\\|h\\|inv\\)\\|qrt\\)\\|tan\\(\\|h\\|inv\\)\\|urd\\)\\b"
       )
      (csound-orc-irate "\\<\\(i\\|p\\|gi\\)[a-zA-Z0-9]+\\>")
      (csound-orc-krate "\\<k[a-zA-Z0-9]+\\|gk[a-zA-Z0-9]+\\>")
      (csound-orc-wrate "\\<w[a-zA-Z0-9]+\\|gw[a-zA-Z0-9]+\\>")
      (csound-orc-frate "\\<f[a-zA-Z0-9]+\\|gk[a-zA-Z0-9]+\\>")
      (csound-orc-arate "\\<a[a-zA-Z0-9]+\\|ga[a-zA-Z0-9]+\\>"))

  (setq csound-font-lock-keywords-1
   (list
    ;; Comments
    ;;
    ;; Fontify syntactically (assuming strings cannot be quoted or span lines).
    '(";.*$" . font-lock-cs-comment)
    '("\"[^\"\n]*\""  . font-lock-cs-string)
    ;;
    ;; Program, subroutine and function declarations, plus calls.
    (cons csound-orc-globals 'font-lock-cs-globs)
    (cons csound-orc-equals 'font-lock-cs-equals)
    (cons csound-orc-decls 'font-lock-cs-decls)
    (cons csound-orc-opcodes 'font-lock-cs-inst)))

  (setq csound-font-lock-keywords-2
        (append
         csound-font-lock-keywords-1
         (list
     ;;
     ;; Fontify all type specifiers (must be first; see below).
          (cons csound-orc-opcodes 'font-lock-cs-opcode)
          (cons csound-orc-opcodes0 'font-lock-cs-opcode)
     ;;
     ;; Fontify all builtin operators.
          (cons csound-orc-iocodes 'font-lock-cs-inout)
          (cons csound-orc-iocodes0 'font-lock-cs-inout)
     ;; Labels
          '("^[ \t]*\\(\\b[a-zA-Z0-9]+\\b\\):" (1 font-lock-cs-label))
     ;; Control
          (cons csound-orc-opcodesc 'font-lock-cs-flow)
     ;; ADI Opcodes
          (cons csound-orc-opcodes-adi 'font-lock-cs-opcode-adi)
          (cons csound-orc-opcodes0-adi 'font-lock-cs-opcode-adi)
;;        (cons csound-orc-iocodes-adi 'font-lock-cs-opcode-adi)
          (cons csound-orc-iocodes0-adi 'font-lock-cs-opcode-adi)
          )))
     ;;

  (setq csound-font-lock-keywords-3
   (append
    ;;
    ;; The list `csound-font-lock-keywords-2'.
    csound-font-lock-keywords-2
    ;;
    (list
    ;; Things extra to `csound-font-lock-keywords-3' (must be done first).
     (cons csound-orc-irate 'font-lock-cs-irate)
     (cons csound-orc-krate 'font-lock-cs-krate)
     (cons csound-orc-wrate 'font-lock-cs-wrate)
     (cons csound-orc-frate 'font-lock-cs-frate)
     (cons csound-orc-arate 'font-lock-cs-arate)
     (cons csound-orc-funcs 'font-lock-cs-funcs)
    ;;
     )))
  )

;;
;; csound-orc-mode

(defun csound-orc-mode ()
  "Mode for editing Csound Orchestras

The main function csound-orc-mode provides is a bunch of keybindings
for the Csound cookies one inserts when writing Csound orchestra. Typing
the key sequence for a command inserts the corresponding cookie and
places point in the right place. If a prefix argument is supplied, the
cookie is instead wrapped around the region. Alternately, one can type
in part of the cookie and complete it.

There is also code for indentation, timestamps, skeletons for new
documents, and lots of other neat features.

\\{csound-orc-mode-map}
Major mode for editing Csound Orchestras.
Written by John Fitch (jpf@maths.bath.ac.uk)"
  (interactive)
  (kill-all-local-variables)

  (use-local-map csound-orc-mode-map)
  (set-syntax-table csound-orc-mode-syntax-table)

  (setq mode-name "Csound Orchestra")
  (setq major-mode 'csound-orc-mode)

  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-column)
  (make-local-variable 'indent-line-function)

  (setq comment-start "; "
        comment-end ""
        comment-column csound-orc-comment-col
        indent-line-function 'csound-orc-indent-line)
  (make-local-variable 'comment-indent-function)
  (setq comment-indent-function 'csound-orc-mode-comment)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip ";+[ \t]*")
  (setq csound-inline-empty-comment-pattern "^.+;+ *$")
  (setq csound-code-level-empty-comment-pattern "^[\t ]+;; *$")
  (setq csound-flush-left-empty-comment-pattern "^;;; *$")
  (make-local-variable 'indent-tabs-mode)
  (setq indent-tabs-mode nil)
  (make-local-variable 'tab-stop-list)
  (setq tab-stop-list (append csound-tab-stops
                              (cons csound-orc-comment-col nil)))

;; Font lock
  (make-local-variable 'font-lock-defaults)
;;  (setq csound-font-lock-keywords csound-font-lock-keywords-2)
  (setq csound-font-lock-keywords csound-font-lock-keywords-3)
  (setq font-lock-defaults '((csound-font-lock-keywords
                              csound-font-lock-keywords-1
                              csound-font-lock-keywords-2
                              csound-font-lock-keywords-3)
                             nil nil nil beginning-of-defun
                             (font-lock-comment-start-regexp . ";")
                             (font-lock-mark-block-function mark-defun)))
  (let ((colour-faces
         (if cs-running-xemacs 
             '(;; Format is:
               ;; face                              "colour"        "background"
               ;; adapted faces to XEmacs conventions 
               ;; change colours to suite your needs
               (font-lock-cs-string                 "green4"            nil)
               (font-lock-cs-comment                "red"               nil)
               (font-lock-cs-globs                  "firebrick"         nil)
               (font-lock-cs-opcode                 "orange"            nil)
               (font-lock-cs-inout                  "orange"            nil)
               (font-lock-cs-opcode-adi             "blue"              nil)
               (font-lock-cs-irate                  "grey30"            nil)
               (font-lock-cs-krate                  "grey30"            nil)
               (font-lock-cs-wrate                  "grey30"            nil)
               (font-lock-cs-frate                  "grey30"            nil)
               (font-lock-cs-arate                  "grey30"            nil)
               (font-lock-cs-funcs                  "orange"            nil)
               (font-lock-cs-inst                   "orange"            nil) 
               (font-lock-cs-equals                 "blue"              nil) 
               (font-lock-cs-label                  "black" "darkseagreen2") 
               (font-lock-cs-decls                  "blue"              nil) 
               (font-lock-cs-flow                   "black" "darkseagreen2") 
               )
           '(;; Format is:
             ;; face                              "colour"        bold
             (font-lock-cs-string                 "magenta"       nil)
             (font-lock-cs-comment                "grey50"        nil)
             (font-lock-cs-globs                  "purple"        nil)
             (font-lock-cs-opcode                 "firebrick"     nil)
             (font-lock-cs-inout                  "Navy"          nil)
             (font-lock-cs-opcode-adi             "MediumAquamarine"      nil)
             (font-lock-cs-irate                  "ForestGreen"   nil)
             (font-lock-cs-krate                  "RoyalBlue"     nil)
             (font-lock-cs-wrate                  "DodgerBlue"    nil)
             (font-lock-cs-frate                  "MediumBlue"    nil)
             (font-lock-cs-arate                  "DarkGoldenrod" nil)
             (font-lock-cs-funcs                  "Sienna"        nil)
             (font-lock-cs-inst                   "red"           t)
             (font-lock-cs-equals                 "red"           t)
             (font-lock-cs-label                  "red"           t)
             (font-lock-cs-decls                  "RoyalBlue"     t)
             (font-lock-cs-flow                   "blue"          t)
           )))
        face colour colour-face colour-bdbg)
    (while colour-faces
      (setq face (nth 0 (car colour-faces))
            colour (nth 1 (car colour-faces))
            colour-face (if cs-running-xemacs face
                          (intern (concat (symbol-name face) "-colour")))
            colour-bdbg (nth 2 (car colour-faces))
            colour-faces (cdr colour-faces))
      (make-face colour-face)
      (if colour-bdbg (if cs-running-xemacs (set-face-background colour-face colour-bdbg)
                        (make-face-bold colour-face nil t)))
      (set-face-foreground colour-face colour)
      (set face colour-face)))
  (font-lock-mode 1)
  ;; added menus <Steve Kersten>
  (csound-install-orc-menu "Csound/Orc")
  (run-hooks 'csound-orc-mode-hook)
  (if csound-orc-mode-do-write-file-hooks
      (add-hook 'local-write-file-hooks 'csound-orc-update-timestamp))

  (add-hook 'local-write-file-hooks 'csound-orc-check-rates)

  (if (and csound-orc-mode-build-new-buffer (zerop (buffer-size)))
        (csound-orc-insert-new-buffer-strings)))



;; Functions specific to Csound Orchestras

(defvar csound-orc-mode-sr  44100 "Default sample rate")
(defvar csound-orc-mode-kr   4410 "Default control rate")
(defvar csound-orc-mode-ksmps  10 "Default ksmps rate")
(defvar csound-orc-mode-nchnls  2 "Default number of channels")

(defun csound-orc-get-rates ()
  (let (opoint ans)
    (save-excursion
      (goto-char 0)
      (setq opoint (re-search-forward "sr[ \t]*=[ \t]*" (point-max) t))
      (if (null opoint) (setq ans nil)
        (forward-line 1)
        (setq csound-orc-mode-sr
              (string-to-int
               (buffer-substring-no-properties opoint (point))))))
    (if ans (save-excursion
      (goto-char 0)
      (setq opoint (re-search-forward "kr[ \t]*=[ \t]*" (point-max) t))
      (if (null opoint) (setq ans nil)
        (forward-line 1)
        (setq csound-orc-mode-kr
              (string-to-int
               (buffer-substring-no-properties opoint (point)))))))
    (if ans (save-excursion
      (goto-char 0)
      (setq opoint (re-search-forward "ksmps[ \t]*=[ \t]*" (point-max) t))
      (if (null opoint) (setq ans nil)
        (forward-line 1)
        (setq csound-orc-mode-ksmps
              (string-to-int
               (buffer-substring-no-properties opoint (point)))))))
    ans))


(defun csound-set-sr (new-rate)
  "Set SR global and adjust KR and KSMPS"
  (interactive "nSample rate: ")
  (csound-orc-get-rates)
  (setq csound-orc-mode-sr new-rate)
  (if (not (equal (* csound-orc-mode-kr csound-orc-mode-ksmps)
                  csound-orc-mode-sr))
      (message "New sr of %d is not compatable with kr=%d and ksmps=%d"
               csound-orc-mode-sr csound-orc-mode-kr csound-orc-mode-ksmps))
  (save-excursion
    (goto-char 0)
    (re-search-forward "sr[ \t]*=[ \t]*")
    (while (looking-at "[0-9]")
      (delete-char 1))
    (insert (int-to-string csound-orc-mode-sr))))

(defun csound-set-kr (new-rate)
  "Set KR global and adjust SR and KSMPS"
  (interactive "nControl rate: ")
  (csound-orc-get-rates)
  (setq csound-orc-mode-kr new-rate)
  (if (not (equal (* csound-orc-mode-kr csound-orc-mode-ksmps)
                  csound-orc-mode-sr))
      (message "New kr of %d is not compatable with sr=%d and ksmps=%d"
               csound-orc-mode-kr csound-orc-mode-sr csound-orc-mode-ksmps))
  (save-excursion
    (goto-char 0)
    (re-search-forward "kr[ \t]*=[ \t]*")
    (while (looking-at "[0-9]")
      (delete-char 1))
    (insert (int-to-string csound-orc-mode-kr))))


(defun csound-set-ksmps (new-rate)
  "Set KSMPS global and adjust KR and SR"
  (interactive "nControl samples: ")
  (csound-orc-get-rates)
  (setq csound-orc-mode-ksmps new-rate)
  (if (not (equal (* csound-orc-mode-kr csound-orc-mode-ksmps)
                  csound-orc-mode-sr))
      (message "New ksmps of %d is not compatable with sr=%d and kr=%d"
               csound-orc-mode-ksmps csound-orc-mode-sr csound-orc-mode-kr))
  (save-excursion
    (goto-char 0)
    (re-search-forward "ksmps[ \t]*=[ \t]*")
    (while (looking-at "[0-9]")
      (delete-char 1))
    (insert (int-to-string csound-orc-mode-ksmps))))

(defun csound-orc-check-rates ()
  (let ((ok (csound-orc-get-rates)))
    (if (and ok (not (equal (* csound-orc-mode-kr csound-orc-mode-ksmps)
                             csound-orc-mode-sr)))
        (message "Sample rate/Control rate incompatability: %d != %d * %d"
                 csound-orc-mode-sr csound-orc-mode-kr
                 csound-orc-mode-ksmps))))

(defun csound-set-nchnls (num-chans)
  "Set Mono, Stereo or Quad"
  (interactive "cChannels [M,S,Q,H,O]: ")
  (setq num-chans (cond ((equal num-chans ?M) 1)
                        ((equal num-chans ?m) 1)
                        ((equal num-chans ?1) 1)
                        ((equal num-chans ?S) 2)
                        ((equal num-chans ?s) 2)
                        ((equal num-chans ?2) 2)
                        ((equal num-chans ?Q) 4)
                        ((equal num-chans ?q) 4)
                        ((equal num-chans ?4) 4)
                        ((equal num-chans ?H) 6)
                        ((equal num-chans ?h) 6)
                        ((equal num-chans ?6) 6)
                        ((equal num-chans ?O) 8)
                        ((equal num-chans ?o) 8)
                        ((equal num-chans ?8) 8)
                        (t 1)))
  (save-excursion
    (goto-char 0)
    (re-search-forward "nchnls[ \t]*=[ \t]*")
    (while (looking-at "[0-9]")
      (delete-char 1))
    (insert (int-to-string num-chans))))

(defvar csound-instrument-template1 "\n\n;;; I N S T R U M E N T    "
  "Text at start of new instument")

(defvar csound-instrument-template2
  "\n;;; Write description here\n;;; Keywords:\n"
  "Text at start of new instument")

(defun csound-instrument (num)
  "Start a new instrument"
  (interactive "nNumber: ")
  (let (ss pp)
    (goto-char (setq ss (point-max)))
    (insert csound-instrument-template1
            (number-to-string num)
            csound-instrument-template2
            "\n")
    (tab-to-tab-stop) (insert "instr") (tab-to-tab-stop)
    (setq pp (point))
    (insert (int-to-string num))
    (insert "\n\t;; Description of Arguments:\n\n\n")
    (tab-to-tab-stop) (insert "endin\n\n")
    (goto-char pp)
    (forward-line 2)))

(defun csound-goto-instr (num)
  "Position cursor within instrument n"
  (interactive "nNumber: ")
  (let ((pp 0)
        (more t))
    (goto-char 0)
    (while
        (and more
             (setq pp (re-search-forward "\\binstr[ \t]*[0-9, \t]*"
                                         (point-max) t)))
      (beginning-of-line)
      (save-restriction
        (narrow-to-region (point) (+ pp 1))
        (if (re-search-forward
             (concat "[ \t,]" (int-to-string num) "[ \t,\n]")
             (point-max) t)
            (progn (widen) (forward-line 1) (setq more nil))
          (widen))
        )
      (goto-char pp))
    (if more (message "Instr %d not found" num))))

(defun csound-write-instrument (num name keywords)
  "Write out the numbered instrument to a file with keywords"
  (interactive "nNumber: \nsname: \nsKeywords: ")
  (let ((pp 0)
        (more t)
        (fname (concat name ".ins")))
    (save-excursion
      (goto-char 0)
      (while
          (and more
               (setq pp (re-search-forward "\\binstr[ \t]*[0-9, \t]*"
                                           (point-max) t)))
        (beginning-of-line)
        (save-restriction
          (narrow-to-region (point) (+ pp 1))
          (if (re-search-forward
               (concat "[ \t,]" (int-to-string num) "[ \t,\n]")
               (point-max) t)
              (progn
                (widen)
                (goto-char pp)
                (if (re-search-forward "\\bendin\\b" (buffer-size) t)
                    (progn
                      (forward-line 1)
                      (write-region ";;Instrument saved\n" nil fname t)
                      (write-region csound-orc-timestamp-start nil fname t)
                      (write-region
                       (concat ";**** Last modified: " (current-time-string) "\n")
                       nil fname t)
                      (write-region csound-orc-timestamp-end nil fname t)
                      (write-region csound-orc-keywords-msg nil fname t)
                      (write-region keywords nil fname t)
                      (write-region "\n" nil fname t)
                      (write-region pp (point) fname t)
                      (setq more nil))
                  (error "Mal-formed orchestra file"))))
          (widen))
        (goto-char pp))
      )
      (if more (message "Instr %d not found" num))))

(defun csound-read-instrument (num name)
  "Insert an instrument from a saved file."
  (interactive "nNumber: \nsname: ")
  (csound-read-instrument1 num (concat name ".ins"))
  (let ((ks (csound-parse-comma-list)))
    (while ks
      (csound-make-keyword (car ks) t)
      (setq ks (cdr ks)))))

(defun csound-read-instrument1 (num name)
  (csound-delete-instrument num)
  (let (ss pp ks)
    (goto-char (setq ss (point-max)))
    (insert-file-contents name)
    (goto-char ss)
    (insert "instr\t" (int-to-string num) "\n") ; Name the instrument
    (kill-line 2)                       ; Kill the header and date stamp header
    (search-forward ";**** ")           ; Change Modified message
    (insert "From file " name ".ins saved on:")
    (zap-to-char 1 ?\:)
    (forward-line 1)                    ; Kill stamp ending
    (kill-line 1)
    (csound-find-keywords)              ; Add keywords to the known list
))

(defun csound-read-instrument-keyword (num keyword)
  "Read an instrument based on its keyword"
  (interactive "nNumber: \nskeyword: ")
  (let
      ((file-list (directory-files default-directory nil ".*\\.ins"))
       buff
       ks
       (obuff (current-buffer)))
    (while file-list
      (setq buff (find-file-noselect (car file-list)))
      (set-buffer buff)
      (goto-char 0)
      (search-forward csound-orc-keywords-msg)
      (setq ks (csound-parse-comma-list))
      (set-buffer obuff)
      (kill-buffer buff)
      (cond ((member keyword ks)                ;A candidate for inclusion
             (csound-read-instrument1 num (car file-list))
             (while ks
               (csound-make-keyword (car ks) t)
               (setq ks (cdr ks)))
             (setq file-list nil))
            (t
             (setq file-list (cdr file-list)))))))



(defun csound-delete-instrument (num)
  "Delete numbered instrument."
  (interactive "nNumber: ")
  (let ((pp 0)
        (more t))
    (goto-char 0)
    (while
        (and more
             (re-search-forward "\\binstr[ \t]*[0-9, \t]*"
                                (point-max) t))
      (beginning-of-line)
      (setq pp (point))
      (cond ((re-search-forward         ;In a list of instruments
              (concat ",[ \t]*\\b" (int-to-string num) "\\b") pp t)
             (zap-to-char -1 ?\,)
             (setq more nil))
            ((re-search-forward         ;Corect instrument
              (concat "\\b" (int-to-string num) "\\b") pp t)
             (if (looking-at "[ \t]*,") ;if in a list kill chars
                   (zap-to-char 1 ?\, )
               (kill-region pp (re-search-forward "\\bendin\\b"))
               (zap-to-char 1 ?\n))
             (setq more nil)))
      (goto-char pp))))

;;; Keyword support for instruments

(defvar csound-last-key nil "Last keyword used")

(defvar csound-keyword-obarray (make-vector 127 0))

(defun csound-ins-add-keyword (string)
  "Add KEYWORD to keywords associated with current Instrument.
Completion is performed over known keywords when reading."
  (interactive (list (csound-read-keyword "Add label")))
  (csound-set-keyword string t))

(defun csound-ins-kill-keyword (string)
  "Remove KEYWORD from keywords associated with current Instrument.
Completion is performed over known keywords when reading."
  (interactive (list (csound-read-keyword "Remove label")))
  (csound-set-keyword string nil))

(defun csound-read-keyword (prompt)
  (let ((result
         (completing-read (concat prompt
                                  (if csound-last-key
                                      (concat " (default "
                                              (symbol-name csound-last-key)
                                              "): ")
                                    ": "))
                          csound-keyword-obarray
                          nil
                          nil)))
    (if (string= result "")
        csound-last-key
      (setq csound-last-key (csound-make-keyword result t)))))

(defun csound-set-keyword (label state)
  (unwind-protect
      (save-excursion
        (let*
            ((pos (csound-find-keywords))
             (ks (csound-parse-comma-list))
             (sym (symbol-name label)))
          (goto-char pos)
          (setq ks (if state (add-to-list ks sym)
                     (delete sym ks)))
          (kill-line)
          (while ks
            (insert (car ks) ",")
            (setq ks (cdr ks)))
          (delete-char -1)))))

(defun csound-parse-comma-list ()
  "Return a list of strngs parsed from current line"
  (let (acc beg)
    (skip-chars-forward " \t")
    (while (not (looking-at "[ \t]*$"))
      (setq beg (point))
      (skip-chars-forward "^, \t\n$")
      (setq acc (cons (buffer-substring beg (point)) acc))
      (skip-chars-forward ", \t"))
    acc))

(defun csound-make-keyword (s &optional forcep)
  (cond ((symbolp s) s)
        (forcep (intern (downcase s) csound-keyword-obarray))
        (t  (intern-soft (downcase s) csound-keyword-obarray))))

(defun csound-force-make-keyword (s)
  (intern (downcase s) csound-keyword-obarray))

(defun csound-find-keywords ()
  "Move to start of keywords"
  (let
      (pos1 pos2)
    (end-of-line)
    (setq pos1 (re-search-backward "\\binstr\\b"))      ; Start of instrument
    (setq pos2 (re-search-forward "\\bendin\\b"))       ; End of instrument
    (goto-char pos1)
    (if (not (search-forward csound-orc-keywords-msg pos2 t))
        (progn
          (goto-char pos1)
          (forward-line 1)
          (while (looking-at "^[ \t]*\\(;\\|$\\)") (forward-line 1))
          (insert csound-orc-keywords-msg "\n")
          (forward-line -1)))
    (point)))

;;; Deal with comments
(defun csound-line-matches (pattern &optional withcomment)
  (save-excursion
    (beginning-of-line)
    (looking-at pattern)))

(defun csound-pop-comment-level ()
  ;; Delete an empty comment ending current line.  Then set up for a new one,
  ;; on the current line if it was all comment, otherwise above it
  (end-of-line)
  (delete-horizontal-space)
  (while (= (preceding-char) ?;
            )
    (delete-backward-char 1))
  (delete-horizontal-space)
  (if (bolp)
      nil
    (beginning-of-line)
    (open-line 1)))

(defun csound-comment ()
  "Convert an empty comment to a `larger' kind, or start a new one.
These are the known comment classes:

   1 -- comment to the right of the code (at the comment-column)
   2 -- comment on its own line, indented like code
   3 -- comment on its own line, beginning at the left-most column.

Suggested usage:  while writing your code, trigger csound-comment
repeatedly until you are satisfied with the kind of comment."
  (interactive)
  (cond

   ;; Blank line?  Then start comment at code indent level.
   ((csound-line-matches "^[ \t]*$")
    (delete-horizontal-space)
    (tab-to-tab-stop)
    (insert comment-start))

   ;; Nonblank line with no comment chars in it?
   ;; Then start a comment at the current comment column
   ((csound-line-matches "^[^;\n]+$")
    (indent-for-comment))

   ;; Flush-left comment present?  Just insert character.
   ((csound-line-matches csound-flush-left-empty-comment-pattern)
    (insert ";"))

   ;; Empty code-level comment already present?
   ;; Then start flush-left comment, on line above if this one is nonempty.
   ((csound-line-matches csound-code-level-empty-comment-pattern)
    (csound-pop-comment-level)
    (insert ";;;"))

   ;; Empty comment ends line?
   ;; Then make code-level comment, on line above if this one is nonempty.
   ((csound-line-matches csound-inline-empty-comment-pattern)
    (csound-pop-comment-level)
    (tab-to-tab-stop)
    (insert ";; "))

   ;; If all else fails, insert character
   (t
    (insert ";")))
  (end-of-line))

(defun uncomment-region ()
  "Call `\\[comment-region]' with an argument"
  (interactive)
  (comment-region (point) (mark) '(t)))

(defun indent-buffer ()
  "*Relayout entire buffer"
  (interactive)
  (indent-region (point-min) (point-max) 0))


;;;
;;; csound-add-ons by Steve Kersten, contact steve-k@gmx.net
;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MENUS
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar csound-orc-mode-menu nil "XEmacs menu")
(setq csound-orc-mode-menu
      '(
        "Csound/Orc"
        ["Indent Region" indent-region t]
        ["Indent Buffer" indent-buffer t]
        ["Comment Region" comment-region t]
        ["Uncomment Region" uncomment-region t]
        ["Power-of-2 Region" e-power t]
        "-----"
        ["Instrument Kill Keyword" csound-ins-kill-keyword t]
        ["Instrument Add Keyword" csound-ins-add-keyword t]
        ["Insert Instrument by Key" csound-read-instrument-keyword t]
        ["Insert Instrument" csound-read-instrument t]
        ["Write Instrument" csound-write-instrument t]
        ["Delete Instrument" csound-delete-instrument t]
        ["Find Instrument" csound-goto-instr t]
        ["New Instrument" csound-instrument t]
        "-----"
        ["Change Channels" csound-set-nchnls t]
        ["Change KSMPS" csound-set-ksmps t]
        ["Change KR" csound-set-kr t]
        ["Change SR" csound-set-sr t]
        "-----"
        ["Return to csd mode" csound-csd-quick]
        ["Document Opcode" csdoc-document-opcode]
        ["Report Bug" csound-orc-submit-bug-report t]))

(autoload 'csdoc-document-opcode "csound-doc" "Csound Documentation browsing." t)

(defun csound-install-orc-menu (menu-name)
  (if (and cs-running-xemacs (featurep 'menubar)
           current-menubar (not (assoc menu-name current-menubar)))
      (progn
        (set-buffer-menubar (copy-sequence current-menubar))
        (add-submenu nil
                     (cons menu-name (cdr csound-orc-mode-menu))
                     "Csound/Orc"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; EDITING FUNCTIONS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; computes power of 2 next to x, returns nil for x=0

(defun e-power-of-2 (x)
  (let (
        (min 2)           ;; Minimum of computation -- at least 2
        (max (expt 2 24)) ;; Maximum of computation -- 2^24 for csound-GEN sizes
        (round-log 0)
        (y 0)
        (z 0))
    
    (if (= x 0) nil
      (progn               
        (if (<= x min) 
            (setq x min)
          (progn 
            (if (>= x max) 
                (setq x max)
              (progn
                (setq round-log (truncate (log x 2)))
                (setq y (expt 2 round-log))
                (setq z (expt 2 (1+ round-log)))
                (if (>= (abs (/ (- x y) (- x z))) 1) 
                    z 
                  y)))))))))

(defun csound-e-power (start end)
  (interactive "r")
  (let ((number 0))
    (when end
      (save-restriction
        (narrow-to-region start end)
        (setq number (string-to-number (buffer-string))
            number (e-power-of-2 number))
        (when number 
          (delete-region start end)
          (insert (format "%d" number)))))))


;; Menus
(if (not cs-running-xemacs)
    (progn
      (define-key csound-orc-mode-map [menu-bar csound-orc]
        (cons "Csound/Orc" (make-sparse-keymap "Csound/Orc")))
      (put 'uncomment-region 'menu-enable 'mark-active)
      (put 'csound-e-power 'menu-enable 'mark-active)
      (define-key csound-orc-mode-map [menu-bar csound-orc csd]
        '("Return to csd mode" . csound-csd-quick))
      (define-key csound-orc-mode-map [menu-bar csound-orc Submit-Report]
        '("Report Bug" . csound-orc-submit-bug-report))
      (define-key csound-orc-mode-map [menu-bar csound-orc Indent-Region]
        '("Indent Region" . indent-region))
      (define-key csound-orc-mode-map [menu-bar csound-orc Indent-buffer]
        '("Indent Buffer" . indent-buffer))
      (define-key csound-orc-mode-map [menu-bar csound-orc UnComment-Region]
        '("UnComment Region" . uncomment-region))
      (define-key csound-orc-mode-map [menu-bar csound-orc Comment-Region]
        '("Comment Region" . comment-region))
      (define-key csound-orc-mode-map [menu-bar csound-orc Power-Region]
        '("Power-of-2 Region" . csound-e-power))
      (define-key csound-orc-mode-map [menu-bar csound-orc Kill-Keyword]
        '("Instrument Kill Keyword" . csound-ins-kill-keyword))
      (define-key csound-orc-mode-map [menu-bar csound-orc Add-Keyword]
        '("Instrument Add Keyword" . csound-ins-add-keyword))
      (define-key csound-orc-mode-map [menu-bar csound-orc Insert-Key]
        '("Insert Instrument by Key" . csound-read-instrument-keyword))
      (define-key csound-orc-mode-map [menu-bar csound-orc Insert-Instrument]
        '("Insert Instrument" . csound-read-instrument))
      (define-key csound-orc-mode-map [menu-bar csound-orc Write-Instrument]
        '("Write Instrument" . csound-write-instrument))
      (define-key csound-orc-mode-map [menu-bar csound-orc Kill-Instrument]
        '("Delete Instrument" . csound-delete-instrument))
      (define-key csound-orc-mode-map [menu-bar csound-orc Goto-Instrument]
        '("Find Instrument" . csound-goto-instr))
      (define-key csound-orc-mode-map [menu-bar csound-orc Start-Instrument]
        '("New Instrument" . csound-instrument))
      (define-key csound-orc-mode-map [menu-bar csound-orc Set-Channels]
        '("Change Channels" . csound-set-nchnls))
      (define-key csound-orc-mode-map [menu-bar csound-orc Set-KSMPS]
        '("Change KSMPS" . csound-set-ksmps))
      (define-key csound-orc-mode-map [menu-bar csound-orc Set-KR]
        '("Change KR" . csound-set-kr))
      (define-key csound-orc-mode-map [menu-bar csound-orc Set-SR]
        '("Change SR" . csound-set-sr))))

;; Debug support

(defun csound-orc-submit-bug-report ()
  "Submit via mail a bug report on csound-orc.el."
  (interactive)
  (require 'reporter)
  (let ((reporter-prompt-for-summary-p t))
    (reporter-submit-bug-report "jpff@maths.bath.ac.uk" (concat "csound-orc" csound-orc-version)
     '(csound-orc-mode-do-write-file-hooks
       csound-orc-mode-build-new-buffer
       csound-orc-mode-never-indent
       csound-orc-mode-hook
       csound-orc-mode-load-hook
       csound-orc-mode-timestamp-hook
       csound-orc-comment-col
       csound-tab-stops
       csound-orc-mode-address-string
       csound-orc-mode-new-buffer-template
       csound-orc-timestamp-start
       csound-orc-timestamp-end
       csound-orc-keywords-msg
       csound-orc-decls
       csound-orc-equals
       csound-orc-opcodes
       csound-orc-opcodes-raw
       csound-orc-opcodes0
       csound-orc-opcodes0-raw
       csound-orc-iocodes
       csound-orc-iocodes-raw
       csound-orc-iocodes0
       csound-orc-iocodes0-raw
       csound-orc-opcodesc
       csound-orc-opcodesc-raw
       csound-orc-opcodes-adi
       csound-orc-opcodes-adi-raw
       csound-orc-opcodes0-adi
       csound-orc-opcodes0-adi-raw
       csound-orc-iocodes-adi
       csound-orc-iocodes-adi-raw
       csound-orc-iocodes0-adi
       csound-orc-iocodes0-adi-raw
       csound-orc-search-limit
       csound-orc-mode-print-indent-info
       csound-code-level-empty-comment-pattern
       csound-flush-left-empty-comment-pattern
       csound-inline-empty-comment-pattern
       csound-orc-mode-sr
       csound-orc-mode-kr
       csound-orc-mode-ksmps
       csound-orc-mode-nchnls
       csound-instrument-template1
       csound-instrument-template2
       csound-last-key
       csound-keyword-obarray
       )                                ; List of variables
     nil nil
     (concat "Hi John.,

I want to report a bug in csound-orc mode

"))))

;;;;;;;; Templates for opcodes
(defvar all-ops nil "List of all templates")
(setq all-ops 
      '(
        ( "active_i"    "i"    "i")
        ( "active_k"    "k"    "k")
        ( "adsr"	"s"	"iiiio")
        ( "adsyn"	"a"	"kkkSo")
        ( "adsynt"     "a"  "kkiiiio")
        ( "aftouch"	"k"	"oh")
        ( "alpass"	"a"	"akioo")
        ( "ampdb_a"	"a"	"a")
        ( "ampdb_i"	"i"	"i")
        ( "ampdb_k"	"k"	"k")
        ( "ampdbfs_a"	"a"	"a")
        ( "ampdbfs_i"	"i"	"i")
        ( "ampdbfs_k"	"k"	"k")
        ( "ampmidi"	"i"	"io")
        ( "areson"	"a"	"akkoo")
        ( "aresonk"     "k"	"kkkpo")
        ( "atone"	"a"	"ako")
        ( "atonek"     "k"	"kko")
        ( "atonex"      "a"    "akoo")
        ( "babo"     "aa"   "akkkiiijj")
        ( "balance"	"a"	"aaqo")
        ( "bamboo"     "a"    "kioooooo")
        ( "bbcutm"	"a""aiiiiipop")
        ( "bbcuts"	"aa""aaiiiiipop")
        ( "betarand_a"	"a"	"kkk")
        ( "betarand_i"	"i"	"kkk")
        ( "betarand_k"	"k"	"kkk")
        ( "bexprnd_a"	"a"	"k")
        ( "bexprnd_i"	"i"	"k")
        ( "bexprnd_k"	"k"	"k")
        ( "biquad"     "a"    "akkkkkko")
        ( "biquada"     "a"    "aaaaaaao")
        ( "birnd_i"      "i"    "i")
        ( "birnd_k"      "k"    "k")
        ( "butbp"      "a"    "akko")
        ( "butbr"      "a"    "akko")
        ( "buthp"      "a"    "ako")
        ( "butlp"      "a"    "ako")
        ( "butterbp"      "a"    "akko")
        ( "butterbr"      "a"    "akko")
        ( "butterhp"      "a"    "ako")
        ( "butterlp"      "a"    "ako")
        ( "button"     "k"    "k")
        ( "buzz"	"a"  "xxkio")
        ( "cabasa"     "a"    "iiooo")
        ( "cauchy_a"	"a"	"k")
        ( "cauchy_i"	"i"	"k")
        ( "cauchy_k"	"k"	"k")
        ( "cggoto"	""	"Bl")
        ( "chanctrl_i"    "i"    "iioh")
        ( "chanctrl_k"    "k"    "iioh")
        ( "checkbox"     "k"    "k")
        ( "clear"     ""	"y")
        ( "clip"	"a"	"aiiv")
        ( "clockoff"     ""     "i")
        ( "clockon"     ""     "i")
        ( "comb"	"a"	"akioo")
        ( "control"     "k"    "k")
        ( "convle"	"mmmm" "aSo")
        ( "convolve"	"mmmm" "aSo")
        ( "cps2pch"      "i"    "ii")
        ( "cpsmidi"	"i"	"")
        ( "cpsmidib_i"	"i"	"o")
        ( "cpsmidib_k"	"k"	"o")
        ( "cpstmid"    "i"    "i")
        ( "cpstun"      "k"    "kkk")
        ( "cpstuni"      "i"    "ii")
        ( "cpsxpch"      "i"    "iiii")
        ( "cpuprc"     ""     "ii")
        ( "cross2"      "a"	"aaiiik")
        ( "crunch"     "a"    "iiooo")
        ( "ctrl14_i"	"i"    "iiiiio")
        ( "ctrl14_k"	"k"    "iiikko")
        ( "ctrl21_i"	"i"    "iiiiiio")
        ( "ctrl21_k"	"k"    "iiiikko")
        ( "ctrl7_i"       "i"    "iiiio")
        ( "ctrl7_k"	"k"    "iikko")
        ( "ctrlinit"      ""     "im")
        ( "cuserrnd_a"  "a"    "kkk")
        ( "cuserrnd_i"  "i"    "iii")
        ( "cuserrnd_k"  "k"    "kkk")
        ( "dam"     "a"    "akiiii")
        ( "dbamp_i"	"i"	"i")
        ( "dbamp_k"	"k"	"k")
        ( "dbfsamp_i"	"i"	"i")
        ( "dbfsamp_k"	"k"	"k")
        ( "dcblock"    "a"    "ao")
        ( "dconv"     "a"    "aii")
        ( "delay"	"a"	"aio")
        ( "delay1"	"a"	"ao")
        ( "delayr"	"a"	"io")
        ( "delayw"	""	"a")
        ( "deltap"	"a"	"k")
        ( "deltap3"	"a"	"x")
        ( "deltapi"	"a"	"x")
        ( "deltapn"      "a"    "x")
        ( "deltapx"     "a"    "ai")
        ( "deltapxw"     ""     "aai")
        ( "diff"	"s"	"xo")  
        ( "diskin"  "mmmm"	"Skooo")
        ( "dispfft"	""	"siiooo")
        ( "display"	""	"sioo")
        ( "distort1"    "a"    "appoo")
        ( "div_aa"	"a"	"aa")
        ( "div_ak"	"a"	"ak")
        ( "div_ii"	"i"	"ii")
        ( "div_ka"	"a"	"ka")
        ( "div_kk"	"k"	"kk")
        ( "divz_aa"	"a"	"aak")
        ( "divz_ak"	"a"	"akk")
        ( "divz_ka"	"a"	"kak")
        ( "divz_kk"	"k"	"kkk")
        ( "downsamp"	"k"	"ao")
        ( "dripwater"   "a"     "kioooooo")
        ( "dumpk"	""	"kSii")
        ( "dumpk2"	""	"kkSii")
        ( "dumpk3"	""	"kkkSii")
        ( "dumpk4"	""	"kkkkSii")
        ( "envlpx"	"s"	"xiiiiiio")
        ( "envlpxr"	"s"	"xiiiiioo")
        ( "event"	""	"Sz")
        ( "expon"	"s"	"iii")
        ( "exprand_a"	"a"	"k")
        ( "exprand_i"	"i"	"k")
        ( "exprand_k"	"k"	"k")
        ( "expseg"	"s"	"iin")
        ( "expsega"     "a"     "iin")
        ( "expsegr"     "s"    "iin")
        ( "filelen"     "i"    "S")
        ( "filenchnls"  "i"    "S")
        ( "filepeak"    "i"   "So")
        ( "filesr"      "i"    "S")
        ( "filter2_a"   "a"    "aiim")
        ( "filter2_k"   "k"    "kiim")
        ( "fin"     ""	"Siiy")
        ( "fini"     ""	"Siim")
        ( "fink"     ""	"Siiz")
        ( "fiopen"     "i"	"Si")
        ( "flanger"     "a"    "aakv")
        ( "flashtxt"     ""     "iS")
        ( "fmb3"     "a"    "kkkkkkiiiii")
        ( "fmbell"     "a"    "kkkkkkiiiii")
        ( "fmmetal"     "a"    "kkkkkkiiiii")
        ( "fmpercfl"    "a"    "kkkkkkiiiii")
        ( "fmrhode"     "a"    "kkkkkkiiiii")
        ( "fmvoice"     "a"    "kkkkkkiiiii")
        ( "fmwurlie"    "a"    "kkkkkkiiiii")
        ( "fof"		"a"	"xxxkkkkkiiiioo")
        ( "fof2"	"a"	"xxxkkkkkiiiikk")
        ( "fog"		"a"	"xxxakkkkkiiiioo")
        ( "fold"    	"a"	"ak")
        ( "follow"	"a"	"ai")
        ( "follow2"	"a"	"akk")
        ( "foscil"      "a"  "xkxxkio")
        ( "foscili"      "a"  "xkxxkio")
        ( "fout"     ""	"Siy")
        ( "fouti"     ""	"iiim")
        ( "foutir"   ""     "iiim")
        ( "foutk"     ""	"Siz")
        ( "ftmorf"      ""      "kii")
        ( "ftsr_i"	"i"	"i")
        ( "gain"	"a"	"akqo")
        ( "gauss_a"	"a"	"k")
        ( "gauss_i"	"i"	"k")
        ( "gauss_k"	"k"	"k")
        ( "gbuzz"	"a"  "xxkkkio")
        ( "gogobel"     "a"    "kkiiikki")
        ( "grain"      "a"    "xxxkkkiiio")
        ( "grain2"     "a"    "kkkikiooo")
        ( "grain3"     "a"    "kkkkkkikikkoo")
        ( "granule"   "a"    "xiiiiiiiiikikiiivppppo")
        ( "guiro"     "a"    "kiooooo")
        ( "harmon"      "a"  "akkkkiii")
        ( "hilbert"    "aa"   "a")
        ( "hrtfer"	"aa"	"akkS")
        ( "hsboscil"	"a" "kkkiiioo")
        ( "ihold"	""	"")
        ( "in"	"a"	"")
        ( "in32"	"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" "")
        ( "inch"	"a"	"k")
        ( "inh"	"aaaaaa" "")
        ( "init_a"	"a"	"i")
        ( "init_i"	"i"	"i")
        ( "init_k"	"k"	"i")
        ( "initc14"     ""     "iiii")
        ( "initc21"     ""     "iiiii")
        ( "initc7"       ""     "iii")
        ( "ino"	"aaaaaaaa" "")
        ( "inq"	"aaaa"	"")
        ( "ins"	"aa"	"")
        ( "integ"	"s"	"xo")
        ( "interp"	"a"	"ko")
        ( "inx"	"aaaaaaaaaaaaaaaa" "")
        ( "inz"	""	"k")
        ( "itablecopy" ""	"ii")
        ( "itablegpw"    ""	"i")
        ( "itablemix"   ""   "iiiiiiiii")
        ( "itablew"     ""	"iiiooo")
        ( "jitter"	"k"   "kkk")
        ( "jitter2"	"k"   "kkkkkkk")
        ( "jspline"	"s"   "xkk")
        ( "kon"  	"" "kkk")
        ( "ktableseg"  ""	"iin")
        ( "lfo"     "s"    "kko")
        ( "limit_a"     "a"	"xkk")
        ( "limit_i"     "i"	"iii")
        ( "limit_k"     "k"	"xkk")
        ( "line"	"s"	"iii")
        ( "linen"	"s"	"xiii")
        ( "linenr"	"s"	"xiii")
        ( "lineto"    "k"   "kk")
        ( "linrand_a"	"a"	"k")
        ( "linrand_i"	"i"	"k")
        ( "linrand_k"	"k"	"k")
        ( "linseg"	"s"	"iin")
        ( "linsegr"      "s"    "iin")
        ( "locsend"  "mmmm"    "")
        ( "locsig"  "mmmm"    "akkk")
        ( "logbtwo_a"     "a"    "a")
        ( "logbtwo_i"     "i"    "i")
        ( "logbtwo_k"     "k"    "k")
        ( "loopseg"	"k"   "kkz")
        ( "lorenz"   "aaa" "kkkkiiii")
        ( "loscil"	"mm""xkiojoojoo")
        ( "loscil3"	"mm""xkiojoojoo")
        ( "lowpass2"    "a"    "akko")
        ( "lowres"    "a"    "akko")
        ( "lowresx"    "a"    "akkoo")
        ( "lpf18"     "a"    "akkk")
        ( "lpfreson"	"a"	"ak"  )
        ( "lphasor"     "a"	"xooooooo")
        ( "lpinterp" ""     "iik")
        ( "lposcil"      "a"    "kkkkio")
        ( "lposcil3"      "a"    "kkkkio")
        ( "lpread"	"kkkk"	"kSoo")
        ( "lpreson"	"a"	"a")
        ( "lpshold"	"k"   "kkz")
        ( "lpslot"   ""     "i")
        ( "mac"     "a"    "Z")
        ( "maca"     "a"    "y")
        ( "madsr"	"s"	"iiiioj")
        ( "mandol"     "a"    "kkkkkkio")
        ( "marimba"     "a"    "kkiiikkiioo")
        ( "massign"      ""     "ii")
        ( "maxalloc"   ""     "ii")
        ( "mclock"	""	"i")
        ( "mdelay"     ""     "kkkkk")
        ( "midic14_i"	"i"    "iiiio")
        ( "midic14_k"  "k"    "iikko")
        ( "midic21_i"   "i"    "iiiiio")
        ( "midic21_k"  "k"    "iiikko")
        ( "midic7_i"     "i"    "iiio")
        ( "midic7_k"    "k"    "ikko")
        ( "midictrl_i"	"i"	"ioh")
        ( "midictrl_k"	"k"	"ioh")
        ( "midiin"     "kkkk" "")
        ( "midion"  	"" "kkk")
        ( "midion2"     ""     "kkkk")
        ( "midiout"     ""     "kkkk")
        ( "mirror"	"s"	"xkk")
        ( "mirror_i"	"i"	"iii")
        ( "mod_aa"	"a"	"aa")
        ( "mod_ak"	"a"	"ak")
        ( "mod_ii"	"i"	"ii")
        ( "mod_ka"	"a"	"ka")
        ( "mod_kk"	"k"	"kk")
        ( "moog"     "a"    "kkkkkkiii")
        ( "moogvcf"     "a"    "axxp")
        ( "moscil"	""	"kkkkk")
        ( "mpulse"     "a"    "kko")
        ( "mrtmsg"	""	"i")
        ( "multitap"	"a"	"am")
        ( "mxadsr"	"s"	"iiiioj")
        ( "nestedap" "a" "aiiiiooooo")
        ( "nlfilt"      "a"	"akkkkk")
        ( "noise"	"a"	"xk")
        ( "noteoff"	"" 	"iii")
        ( "noteon"	"" 	"iii")
        ( "noteondur"	"" "iiii")
        ( "noteondur2"	"" "iiii")
        ( "notnum"	"i"	"")
        ( "nreverb"	"a"	"akkoojoj")
        ( "nrpn"     ""     "kkk")
        ( "ntrpol_a"    "a"    "aakop")
        ( "ntrpol_i"    "i"    "iiiop")
        ( "ntrpol_k"    "k"    "kkkop")
        ( "octmidi"	"i"	"")
        ( "octmidib_i"	"i"	"o")
        ( "octmidib_k"	"k"	"o")
        ( "octpch_i"	"i"	"i")
        ( "oscbnk"     "a" "kkkkiikkkkikkkkkkikooooooo")
        ( "oscil1"	"k"	"ikii")
        ( "oscil1i"	"k"	"ikii")
        ( "oscil3_aa"	"a"	"aaio")
        ( "oscil3_ak"	"a"	"akio")
        ( "oscil3_ka"	"a"	"kaio")
        ( "oscil3_kk"	"s"	"kkio")
        ( "oscil_aa"	"a"	"aaio")
        ( "oscil_ak"	"a"	"akio")
        ( "oscil_ka"	"a"	"kaio")
        ( "oscil_kk"	"s"	"kkio")
        ( "oscili_aa"	"a"	"aaio")
        ( "oscili_ak"	"a"	"akio")
        ( "oscili_ka"	"a"	"kaio")
        ( "oscili_kk"	"s"	"kkio")
        ( "osciln"      "a"    "kiii")
        ( "oscils"      "a"	"iiio")
        ( "oscilx"	"a"	"kiii")
        ( "out"	""	"a")
        ( "out32"	""	"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
        ( "outc"	""	"y")
        ( "outch"	""	"Z")
        ( "outh"	""	"aaaaaa")
        ( "outiat"	""	"iiii")
        ( "outic"	""	"iiiii")
        ( "outic14"	""	"iiiiii")
        ( "outipat"	""    "iiiii")
        ( "outipb"	""	"iiii")
        ( "outipc"	""	"iiii")
        ( "outkat"	""	"kkkk")
        ( "outkc"	""	"kkkkk")
        ( "outkc14"	""	"kkkkkk")
        ( "outkpat"	""    "kkkkk")
        ( "outkpb"	""	"kkkk")
        ( "outkpc"	""	"kkkk")
        ( "outo"	""	"aaaaaaaa")
        ( "outq"	""	"aaaa")
        ( "outq1"	""	"a")
        ( "outq2"	""	"a")
        ( "outq3"	""	"a")
        ( "outq4"	""	"a")
        ( "outs"	""	"aa")
        ( "outs1"	""	"a")
        ( "outs2"	""	"a")
        ( "outx"	""	"aaaaaaaaaaaaaaaa")
        ( "outz"	""	"k")
        ( "pan"		"aaaa"	"akkioo")
        ( "pareq"       "a"     "akkko")
        ( "pcauchy_a"	"a"	"k")
        ( "pcauchy_i"	"i"	"k")
        ( "pcauchy_k"	"k"	"k")
        ( "pchbend_i"   "i"     "jp")
        ( "pchbend_k"   "k"     "jp")
        ( "pchmidi"	"i"	"")
        ( "pchmidib_i"	"i"	"o")
        ( "pchmidib_k"	"k"	"o")  
        ( "peak_a"      "k"	"a")
        ( "peak_k"      "k"	"k")
        ( "phaser1"     "a"     "akkko")
        ( "phaser2"     "a"     "akkkkkk")
        ( "phasor"	"s"	"xo")
        ( "phasorbnk"	"s" 	"xkio")
        ( "pinkish"     "a"     "xoooo")
        ( "pitch"       "kk" 	"aiiiiqooooojo")
        ( "pitchamdf"   "kk"	"aiioppoo")
        ( "planet"      "aaa"   "kkkiiiiiiio")
        ( "pluck"	"a"  	"kkiiioo")
        ( "poisson_a"	"a"	"k")
        ( "poisson_i"	"i"	"k")
        ( "poisson_k"	"k"	"k")
        ( "polyaft_i"	"i"	"ioh")
        ( "polyaft_k"	"k"	"ioh")
        ( "port"	"k"	"kio")
        ( "portk"       "k"	"kko")
        ( "poscil"      "s"    "kkio")
        ( "poscil3"     "s"    "kkio")
        ( "pow_a"       "a"    "akp")
        ( "pow_i"       "i"    "ii")
        ( "pow_k"       "k"    "kkp")
        ( "powoftwo_a"  "a"    "a")
        ( "powoftwo_i"  "i"    "i")
        ( "powoftwo_k"  "k"    "k")
        ( "prealloc"    ""     "ii")
        ( "print"	""	"m")
        ( "printk"      ""	"iko")
        ( "printk2"     ""	"ko")
        ( "printks"     ""	"Sikkkk")
        ( "product"     "a"    "y")
        ( "pvadd"       "a" "kkSiiopooo")
        ( "pvbufread"   ""	"kS")
        ( "pvcross"     "a"	"kkSkko")
        ( "pvinterp"    "a"  "kkSkkkkkk")
        ( "pvoc"	"a"  "kkSoooo")
        ( "pvread"      "kk"	"kSi")
        ( "pvsadsyn"    "a"   "fikopo")
        ( "pvsanal"    "f"   "aiiiioo")
        ( "pvscross"	"f"   "ffkk")
        ( "pvsfread"    "f"   "kSo")
        ( "pvsftr"    ""    "fio")
        ( "pvsftw"    "k"   "fio")
        ( "pvsinfo"    "iiii""f")
        ( "pvsmaska"    "f"   "fik")
        ( "pvsynth"    "a"   "fo")
        ( "rand"	"s"	"xvoo")
        ( "randh"	"s"	"xxvoo")
        ( "randi"	"s"	"xxvoo")
        ( "readclock"     "i"    "i")
        ( "readk"	"k"	"Siio")
        ( "readk2"	"kk"	"Siio")
        ( "readk3"	"kkk"	"Siio")
        ( "readk4"	"kkkk"	"Siio")
        ( "reinit"	""	"l")
        ( "release"	"k"	"")
        ( "repluck"    "a"    "ikikka")
        ( "reson"	"a"	"akkoo")
        ( "resonk"     "k"	"kkkpo")
        ( "resonr"    "a"    "akkoo")
        ( "resonx"      "a"    "akkooo")
        ( "resony"      "a"    "akkikooo")
        ( "resonz"    "a"    "akkoo")
        ( "reverb"	"a"	"ako")
        ( "reverb2"	"a"	"akkoojoj")
        ( "rezzy"     "a"    "axxo")
        ( "rms"	"k"	"aqo")
        ( "rnd31_a"   "a"    "kko")
        ( "rnd31_i"   "i"    "iio")
        ( "rnd31_k"   "k"    "kko")
        ( "rtclock_i"	"i"	"")
        ( "rtclock_k"	"i"	"")
        ( "s16b14_i" "iiiiiiiiiiiiiiii" "iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii")
        ( "s16b14_k" "kkkkkkkkkkkkkkkk" "iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii")
        ( "s32b14_i" "iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii" "iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii")
        ( "s32b14_k" "kkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkk" "iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii")
        ( "samphold"	"s"	"xxoo")
        ( "sandpaper"     "a"    "iiooo ")
        ( "scans"     "a"    "kkiio")
        ( "scanu" "" "iiiiiiikkkkiikkaii")
        ( "schedkwhen"""     "kkkkkz")
        ( "schedule"     ""     "iiim")
        ( "schedwhen"     ""     "kkkkm")
        ( "seed"	""	"i")
        ( "sekere"     "a"    "iiooo")
        ( "sense"     "k"    "")
        ( "sensekey"     "k"    "")
        ( "seqtime"     "k"    "kkkkk")
        ( "setctrl"     ""     "iSi")
        ( "sfilist"    ""	"i")
        ( "sfinstr"    "aa" "iixxiio")
        ( "sfinstr3"    "aa" "iixxiio")
        ( "sfinstr3m" "a" "iixxiio")
        ( "sfinstrm" "a" "iixxiio")
        ( "sfload"    "i"	"S")
        ( "sfpassign"  ""	"ii")
        ( "sfplay"    "aa"   "iixxio")
        ( "sfplay3"    "aa" "iixxio")
        ( "sfplay3m" "a" "iixxio")
        ( "sfplaym"  "a"    "iixxio")
        ( "sfplist"    ""	"i") 
        ( "sfpreset"    "i"	"iiii")
        ( "shaker"     "a"    "kkkkko")
        ( "sleighbells" "a""kioooooo")
        ( "slider16_i" "iiiiiiiiiiiiiiii" "iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii")
        ( "slider16_k" "kkkkkkkkkkkkkkkk" "iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii")
        ( "slider16f" "kkkkkkkkkkkkkkkk" "iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii")
        ( "slider32_i" "iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii" "iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii")
        ( "slider32_k" "kkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkk" "iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii")
        ( "slider32f" "kkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkk"   "iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii")
        ( "slider64_i" "iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii"  "iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii")
        ( "slider64_k" "kkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkk"   "iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii")
        ( "slider64f" "kkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkk"  "iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii")
        ( "slider8_i" "iiiiiiii" "iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii")
        ( "slider8_k" "kkkkkkkk"  "iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii")
        ( "slider8f" "kkkkkkkk""iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii")
        ( "sndwarp"	"mm"	"xxxiiiiiii")
        ( "sndwarpst" "mmmm""xxxiiiiiii")
        ( "soundin" "mmmmmmmmmmmmmmmmmmmmmmmm" "Soo")
        ( "soundout"      ""     "aSo")
        ( "space"  "aaaa"    "aikkkk")
        ( "spat3d"  "aaaa" "akkkiiiiio")
        ( "spat3di"  "aaaa" "aiiiiiio")
        ( "spat3dt"      "" "iiiiiiiio")
        ( "spdist"     "k"    "ikkk")
        ( "specaddm"	"w"	"wwp")
        ( "specdiff"	"w"	"w")
        ( "specdisp"	""	"wio")
        ( "specfilt"	"w"	"wi")
        ( "spechist"	"w"	"w")
        ( "specptrk"	"kk"	"wkiiiiiioqooo")
        ( "specscal"	"w"	"wii")
        ( "specsum"	"k"	"wo")
        ( "spectrum"    "w" "siiiqoooo")
        ( "spsend"  "aaaa"    "")
        ( "stix"     "a"    "iiooo")
        ( "streson"     "a"    "aki")
        ( "strset"      ""     "iS")
        ( "sum"     "a"    "y")
        ( "svfilter"    "aaa"  "akko")
        ( "table"	"s"	"xiooo")
        ( "table3"	"s"	"xiooo")
        ( "table3_i"	"i"	"iiooo")
        ( "table_i"	"i"	"iiooo")
        ( "tablecopy" ""	"kk")
        ( "tablegpw"    ""	"k")
        ( "tablei"	"s"	"xiooo")
        ( "tablei_i"	"i"	"iiooo")
        ( "tableicopy" ""	"ii")
        ( "tableigpw"    ""	"i")
        ( "tableikt"     "s"  "xkooo")
        ( "tableimix"   ""   "iiiiiiiii")
        ( "tableiw"     ""	"iiiooo")
        ( "tablekt"     "s"  "xkooo")
        ( "tablemix"   ""   "kkkkkkkkk")
        ( "tableng_i"   "i"	"i")
        ( "tableng_k"  "k"	"k")
        ( "tablera"     "a"	"kkk")
        ( "tableseg"  ""	"iin")
        ( "tablew"     ""	"xxiooo")
        ( "tablewa"     "k"	"kak")
        ( "tablewkt"     ""     "xxkooo")
        ( "tablexseg" ""	"iin")
        ( "tambourine""a"    "kioooooo")
        ( "taninv2_a"	"a"	"aa")
        ( "taninv2_i"	"i"	"ii")
        ( "taninv2_k"	"k"	"kk")
        ( "tbvcf"     "a"    "axxkk")
        ( "tempest"	"k""kiiiiiiiiiop")
        ( "tempo"    ""     "ki")
        ( "tempoval"     "k"    "")
        ( "timeinstk"    "k"	"")
        ( "timeinsts"    "k"	"")
        ( "timek_i"     "i"	"")
        ( "timek_k"     "k"	"")
        ( "times_i"     "i"	"")
        ( "times_k"     "k"	"")
        ( "timout"	""	"iil")
        ( "tival"	"i"	"")
        ( "tlineto"	"k"   "kkk")
        ( "tone"	"a"	"ako")
        ( "tonek"     "k"	"kko")
        ( "tonex"      "a"    "akoo")
        ( "transeg"	"s"	"iiim")
        ( "trigger"     "k"    "kkk")
        ( "trigseq"     ""     "kkkkkz")
        ( "trirand_a"	"a"	"k")
        ( "trirand_i"	"i"	"k")
        ( "trirand_k"	"k"	"k")
        ( "turnoff"	""	"")
        ( "turnon"      ""     "io")
        ( "unirand_a"     "a"	"k")
        ( "unirand_i"     "i"	"k")
        ( "unirand_k"     "k"	"k")
        ( "upsamp"	"a"	"k")
        ( "valpass"     "a"     "akkioo")
        ( "vbap16" "aaaaaaaaaaaaaaaa""aioo")
        ( "vbap16move" "aaaaaaaaaaaaaaaa""aiiim")
        ( "vbap4" "aaaa""aioo")
        ( "vbap4move" "aaaa""aiiim")
        ( "vbap8" "aaaaaaaa""aioo")
        ( "vbap8move" "aaaaaaaa""aiiim")
        ( "vbaplsinit" """iioooooooooooooooooooooooooooooooo")
        ( "vbapz"     ""    "iiaioo")
        ( "vbapzmove" """iiaiiim")
        ( "vco"     "a"    "xxpppp")
        ( "vcomb"	"a"     "akkioo")
        ( "vdelay"      "a"    "axio")
        ( "vdelay3"      "a"    "axio")
        ( "vdelayx"      "a"    "aaiio")
        ( "vdelayxq"      "aaaa" "aaaaaiio")
        ( "vdelayxs"      "aa"   "aaaiio")
        ( "vdelayxw"      "a"    "aaiio")
        ( "vdelayxwq"      "aaaa" "aaaaaiio")
        ( "vdelayxws"      "aa"   "aaaiio")
        ( "veloc"	"i"	"oh")
        ( "vibes"     "a"    "kkiiikkii")
        ( "vibr"	"k"   "kki")
        ( "vibrato"	"k" "kkkkkkkkio")
        ( "vincr"     ""	"aa")
        ( "vlowres"    "a"    "akkik")
        ( "voice"     "a"    "kkkkkkii")
        ( "vpvoc"     "a"	"kkSoo")
        ( "waveset"	"a"	"ako")
        ( "weibull_a"	"a"	"kk")
        ( "weibull_i"	"i"	"kk")
        ( "weibull_k"	"k"	"kk")
        ( "wgbow"     "a"    "kkkkkkio")
        ( "wgbowedbar" "a"    "kkkkkoooo")
        ( "wgbrass"     "a"    "kkkikkio")
        ( "wgclar"     "a"    "kkkiikkkio")
        ( "wgflute"     "a"    "kkkiikkkiovv")
        ( "wgpluck"     "a"   "iikiiia")
        ( "wgpluck2"    "a"    "ikikk")
        ( "wguide1"     "a"    "axkk")
        ( "wguide2"     "a"    "axxkkkk")
        ( "wrap"	"s"	"xkk")
        ( "wrap_i"	"i"	"iii")
        ( "xadsr"	"s"	"iiiio")
        ( "xscans"      "a"     "kkiio")
        ( "xscanu"	""	"iiiiSiikkkkiikkaii")
        ( "xtratim"	""	"i")
        ( "xxx"	"a"	"kkio")
        ( "xyin"    "kk"   "iiiiioo")
        ( "zacl"     ""	"kk")
        ( "zakinit"    ""	"ii")
        ( "zamod"     "a"	"ak")
        ( "zar"     	"a"	"k")
        ( "zarg"     	"a"	"kk")
        ( "zaw"     	""	"ak")
        ( "zawm"     	""	"akp")
        ( "zfilter2"    "a"     "akkiim")
        ( "zir"     	"i"	"i")
        ( "ziw"     	""	"ii")
        ( "ziwm"     	""	"iip")
        ( "zkcl"     	""	"kk")
        ( "zkmod"     	"k"	"kk")
        ( "zkr"     	"k"	"k")
        ( "zkw"     	""	"kk")
        ( "zkwm"     	""	"kkp")
        { "tablexkt"    "a"	"xkkiooo")
)

(defun csound-abbrev ()
  "Expand Csound opcode"
  (interactive)
  (let (abbrev expansions (oplist all-ops))
    (if (bobp)
        (error "No possible abbreviation preceding point"))
    ;; Return abbrev at point
    (save-excursion
      ;; Record the end of the abbreviation.
      (setq last-abbrev-location (point))
      (save-match-data
        (if (search-backward-regexp "[ \t\n]" 0 t)
            (forward-char 1)
          (error "No possible abbreviation preceding point"))
        (if (looking-at "\\(\\sw\\|\\s_\\)+")
      ;; Now find the beginning of that one.
            (setq abbrev 
                  (concat "^"
                          (buffer-substring-no-properties
                           (point) last-abbrev-location)))
          (error "No possible abbreviation preceding point")))
      (delete-region last-abbrev-location (point)) ;; Remove string
      (while oplist                     ;; Look for all possible expansions
        (if (string-match abbrev (caar oplist))
            (let (new)
              (setq new (list (cadr (car oplist)) (car (car oplist)) (car (cddr (car oplist)))))
              (setq expansions (cons new expansions))
              (setq oplist (cdr oplist)))
          (if expansions (setq oplist nil)
            (setq oplist (cdr oplist)))))
      (if expansions
          (let ((start (point)))
            (setq expansions (reverse expansions))
            (cs-insert-abbrev (car expansions))
            (while (and expansions (not (y-or-n-p "Accept? ")))
              (delete-region start (point))
              (setq expansions (cdr expansions))
              (if expansions (cs-insert-abbrev (car expansions))))
            (insert abbrev)
            (save-excursion
              (search-backward "^")
              (delete-char 1)))
        (insert abbrev)
        (save-excursion
          (search-backward "^")
          (delete-char 1))) )))

(defun cs-insert-abbrev (expand)
  (let ((n 0))
    (setq n (cs-insertvars n (car expand)))
    (insert " ")
    (cs-insertop (cadr expand))
    (insert " ")
    (cs-insertvars n (car (cddr expand)))))

(defun cs-insertop (str)
  (let ((sl (string-to-list str)))
    (while (and sl (not (eq (car sl) 137))) ; 137 == _
      (insert (car sl))
      (setq sl (cdr sl)))))

(defun cs-insertvars (n str)
  (let (cont
        (sl (string-to-list str)))
    (while sl
      (if cont (insert ", "))
      (if (equal (car sl) 123)             ; 123 == S
          (insert "\"abc\"")
        (insert (car sl))
        (insert (number-to-string (setq n (1+ n)))))
        (setq cont t)
      (setq sl (cdr sl)))
    n))
      
(define-key csound-orc-mode-map "\e/"        'csound-abbrev)

(provide 'csound-orc-mode)
(run-hooks 'csound-orc-mode-load-hook)

;;; csound-orc-mode.el ends here
