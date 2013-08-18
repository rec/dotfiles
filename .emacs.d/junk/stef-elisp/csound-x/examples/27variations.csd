; an example taken from the Csound-X documentation
; see node "Embedded elisp" for details

<ELISP> ;;; [load] - [collapse/unfold] - [save] - [EVAL] - [;EVAL]
(let ((cscsd-current-processing "csound wav") 
      (cscsd-call-csound-always-sync t)) 
  ;; the let statement gives the correct options to csound
  ;; and ensure that csound will not be executed asynchronously
  (dotimes (i1 2 3)
    (dotimes (i2 2 3)
      (dotimes (i3 2 3)
	(let* ((m1 (+ 2 i1))
	       (m2 (+ 1 i2 m1))
	       (m3 (+ 1 i3 m2)))
	  (cscsd-set-macro-def "m1" m1)
	  (cscsd-set-macro-def "m2" m2)
	  (cscsd-set-macro-def "m3" m3)
	  (let ((csd-file-name (expand-file-name 
				(format "example%d-%d-%d.csd" m1 m2 m3)
				(file-name-directory (buffer-file-name)))))
	    (write-region (point-min) (point-max) csd-file-name)
	    (message "processing m1=%d m2=%d m3=%d ..." m1 m2 m3)
	    (cscsd-process csd-file-name)))))))
</ELISP>

<CsoundSynthesizer>
<CsInstruments>
#define m3 #4#
#define m2 #3#
#define m1 #2#

; ************************************************************************
; ACCCI:      02_43_1.ORC
; timbre:     tibetan chant
; synthesis:  additive same units(02)
;             basic instrument with minimal differences in frequency(43)
;             arpeggio instrument by Risset
; source:     Phase6, Lorrain(1980); Boulanger(1990): risset1.orc
; coded:      jpg 9/93

sr = 44100
kr  =  441
ksmps= 100
nchnls = 2

instr 1; *****************************************************************
idur  = p3
iamp  = p4/9
ifq   = p5
ioff1 = p6
ioff2 = $m1.*p6
ioff3 = $m2.*p6
ioff4 = $m3.*p6
irise = p7
idec  = p8

   ae  linen   iamp,irise,idur,idec   

   a1  oscili  ae, ifq, 1
   a2  oscili  ae, ifq+ioff1, 1  ; nine oscillators with the same ae
   a3  oscili  ae, ifq+ioff2, 1  ; and waveform, but slightly different
   a4  oscili  ae, ifq+ioff3, 1  ; frequencies create harmonic arpeggio
   a5  oscili  ae, ifq+ioff4, 1
   a6  oscili  ae, ifq-ioff1, 1
   a7  oscili  ae, ifq-ioff2, 1
   a8  oscili  ae, ifq-ioff3, 1
   a9  oscili  ae, ifq-ioff4, 1

   outs1   a1+a2+a3+a4+a5+a6+a7+a8+a9

endin


instr 2; *****************************************************************
idur  = p3
iamp  = p4/9
ifq   = p5
ioff1 = p6
ioff2 = $m1.*p6
ioff3 = $m2.*p6
ioff4 = $m3.*p6
irise = p7
idec  = p8

   ae  linen   iamp,irise,idur,idec   

   a1  oscili  ae, ifq, 1
   a2  oscili  ae, ifq+ioff1, 1  ; nine oscillators with the same ae
   a3  oscili  ae, ifq+ioff2, 1  ; and waveform, but slightly different
   a4  oscili  ae, ifq+ioff3, 1  ; frequencies create harmonic arpeggio
   a5  oscili  ae, ifq+ioff4, 1
   a6  oscili  ae, ifq-ioff1, 1
   a7  oscili  ae, ifq-ioff2, 1
   a8  oscili  ae, ifq-ioff3, 1
   a9  oscili  ae, ifq-ioff4, 1

   outs2   a1+a2+a3+a4+a5+a6+a7+a8+a9

endin
</CsInstruments>

<CsScore>; ************************************************************************
; ACCCI:      02_43_1.SCO
; coded:      jpg 9/93


; GEN functions **********************************************************

; carrier
f1 0 1024 10 .3 0 0 0 .1 .1 .1 .1 .1 .1


; score ******************************************************************

;    start   idur  iamp   ifq     ioff   irise   idec
i1	0	35	8000	110	0.03	0.07	21    
i1	20	20	9600	110	0.04	2	4     
i1	28	30	8000	220	0.04	3	6      
i1	32.1	23	8000	110	0.03	2.3	4.6      
i2	5	20	9600	55	0.02	0.04	12      	
i2	20	15	8000	220	0.05	1.5	3   
i2	32	26	9600	110	0.025	2.6	5.2    
i2	36	22	8000	55	0.01	0.04	13    
e

</CsScore>
</CsoundSynthesizer>