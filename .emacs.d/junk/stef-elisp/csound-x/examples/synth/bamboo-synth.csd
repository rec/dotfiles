
;-----------------------    A synthesizer based on Dan Gutwein's WOOD FLUTE      --------------------
; Instrument:
; by Dan Gutwein,
; based on Hans Mikelson's 1/22/97 physical modeling code, itself based on Perry Cook's Slide Flute
;
; powered by Csound-X for Emacs
;----------------------------------------------------------------------------------------------------

<ELISP> ; MIDI interpretation [collapse/unfold] - [EVAL] 

(require 'k)
(clear-between-marks "BEG" "END")


;=================================== Initialization

;; KEYS
(defmacro with-bamboo (&rest body)
  `(with-pfields-stream    
    '((1 (:instr 2))
      (2 (:start 0))
      (3 (:duration 1))
      (4 (:amp 700))
      (5 (:pitch 5.09))
      (6 (:bthvol 5))
      (7 (:bthatk 15))
      (8 (:p8 0))
      (9 (:p9 0))
      ;;
      (10 (:vibamp0 0)) (11 (:vibamp1 0)) (12 (:vibamp2 0)) (13 (:vibamp3 0)) (14 (:vibamp4 0)) (15 (:vibamp5 0)) (16 (:vibamp6 0)) (17 (:vibamp7 0)) (18 (:vibamp8 0)) (19 (:vibamp9 0))
      (20 (:vibspd0 0)) (21 (:vibspd1 0)) (22 (:vibspd2 0)) (23 (:vibspd3 0)) (24 (:vibspd4 0)) (25 (:vibspd5 0)) (26 (:vibspd6 0)) (27 (:vibspd7 0)) (28 (:vibspd8 0)) (29 (:vibspd9 0))
      (30 (:pan0 0.5)) (31 (:pan1 0.5)) (32 (:pan2 0.5)) (33 (:pan3 0.5)) (34 (:pan4 0.5)) (35 (:pan5 0.5)) (36 (:pan6 0.5)) (37 (:pan7 0.5)) (38 (:pan8 0.5)) (39 (:pan9 0.5))
      (40 (:tidx0 0)) (41 (:tidx1 0)) (42 (:tidx2 0)) (43 (:tidx3 0)) (44 (:tidx4 0)) (45 (:tidx5 0)) (46 (:tidx6 0)) (47 (:tidx7 0)) (48 (:tidx8 0)) (49 (:tidx9 0))
      (50 (:genamp0 0)) (51 (:genamp1 1)) (52 (:genamp2 1)) (53 (:genamp3 1)) (54 (:genamp4 1)) (55 (:genamp5 1)) (56 (:genamp6 1)) (57 (:genamp7 1)) (58 (:genamp8 1)) (59 (:genamp9 0))
      (60 (:pitoff0 0)) (61 (:pitoff1 0)) (62 (:pitoff2 0)) (63 (:pitoff3 0)) (64 (:pitoff4 0)) (65 (:pitoff5 0)) (66 (:pitoff6 0)) (67 (:pitoff7 0)) (68 (:pitoff8 0)) (69 (:pitoff9 0))
      (70 (:p6%0 1)) (71 (:p6%1 1)) (72 (:p6%2 1)) (73 (:p6%3 1)) (74 (:p6%4 1)) (75 (:p6%5 1)) (76 (:p6%6 1)) (77 (:p6%7 1)) (78 (:p6%8 1)) (79 (:p6%9 1))
      ;;
      (80 (:segdur0-1 0.5)) (81 (:segdur1-2 1)) (82 (:segdur2-3 1)) (83 (:segdur3-4 1)) (84 (:segdur4-5 1)) (85 (:segdur5-6 1)) (86 (:segdur6-7 1)) (87 (:segdur7-8 1)) (88 (:segdur8-9 1)))
    ,@body))

;;; CUSTOM KEYS
;; for each :envelope
(dolist (key '(:vibamp :vibspd :pan :tidx :genamp :pitoff :p6%))
  ;; :envelope takes a list of 10 numbers and set all corresponding current values
  (add-custom-key (list key 
			(let (pl)
			  (dotimes (n 10 pl)
			    (setq pl (append pl (list (intern (format "%s%d" (symbol-name key) n))
						      `(lambda (v) (if (listp v) (nth ,n v) v)))))))))
  ;; :envelope+ takes a list of 10 numbers and add them to current values
  (add-custom-key (list (intern (format "%s+" (symbol-name key)))
			(let (pl k)
			  (dotimes (n 10 pl)
			    (setq pl (append pl (list (setq k (intern (format "%s%d" (symbol-name key) n)))
						      `(lambda (v) (+ (current-pfield ,k)
								      (if (listp v)
									  (nth ,n v)
									v))))))))))
  ;; :envelope* takes a list of 10 numbers and multiply them with current values
  (add-custom-key (list (intern (format "%s*" (symbol-name key)))
			(let (pl k)
			  (dotimes (n 10 pl)
			    (setq pl (append pl (list (setq k (intern (format "%s%d" (symbol-name key) n)))
						      `(lambda (v) (* (current-pfield ,k)
								      (if (listp v)
									  (nth ,n v)
									v))))))))))
  ;; :envelope>> n rotates the values by -n
  (add-custom-key (list (intern (format "%s>>" (symbol-name key)))
			(let (pl)
			  (dotimes (n 10 pl)
			    (setq pl (append pl (list (intern (format "%s%d" (symbol-name key) n))
						      `(lambda (v) (current-pfield 
								    (intern (format "%s%d"
										    (symbol-name ,key) 
										    (mod (- ,n v)
			   10))))))))))))
  ;; :envelope<< n rotates the values by n
  (add-custom-key (list (intern (format "%s<<" (symbol-name key)))
			(let (pl)
			  (dotimes (n 10 pl)
			    (setq pl (append pl (list (intern (format "%s%d" (symbol-name key) n))
						      `(lambda (v) (current-pfield 
								    (intern (format "%s%d"
										    (symbol-name ,key) 
										    (mod (+ ,n v) 10)))))))))))))

(defvar Overblow-Ratio 0.1
  "relative duration for overblow in the note: from 0 to 1
this is taken into account when using :durOB instead of :dur")

(add-custom-key '(:durOB
		  (:duration 'clicks-to-seconds
		   :segdur0-1 (lambda (v) (* Overblow-Ratio (clicks-to-seconds v))))))

(add-custom-key `(:segments ,(let (pl)
				  (dotimes (n 8 pl)
				    (setq pl (append pl (list (intern (format ":segdur%d-%d" (1+ n) (+ 2 n)))
							      `(lambda (v) (nth ,n v)))))))))


;=================================== Interpretation

(with-bamboo
 (with-keykit-stream 
  (kstream:vol '(:amp (lambda (v)
			(* (+ 500 (midi-to-range v 0 300))
			   (1+ (/ (random 100) 500.0))))))
  (kstream:pit '(:pitch 'midi-to-pch))
  (set-in-stream :vibamp '(.1 .2 .2 .2 .2 .2 .4 .8 .8 .8)
		 :vibspd '(2 2 2 2 2 2 4 8 8 8)
		 :bthvol 4
		 :bthatk 12
		 :tidx 0)
  (along-MIDIfile (n midi-file)
   (note n)
   (score-line (substitute :durOB :dur n)
	       :vibamp>> 3 :vibspd>> 7))
  (+i :duration 1 :amp 0)))

</ELISP>

<CsoundSynthesizer>
<CsInstruments>
;***************** Dan Gutwein - WOOD FLUTE ****************
; Based on Hans Mikelson's 1/22/97 physical modeling code  *
; which was based on Perry Cook's Slide Flute              *
;***********************************************************

sr = 44100
kr = 4410
ksmps =  10
nchnls = 2


instr 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16
; iamp is overall volume of the note-event
; krelamp is relative volume needed to produce equal volumes for each overblow segment of the overall note-event
; kgenamp is the expressive control over krelamp within the note-event

  aflute1 init 0
  idur = p3
  iamp = p4*64 ;(127*64 = 8128)
  ifreq    = cpspch(p5+2)
  ibthatk = p7
  ipan = .5
  ifeedbk1= .4
  ifeedbk2= .4
  ;ifreq and ibreath are adjusted by linsegs below
  
  ; Copy Overblow values for each pt. in linseg into variables
  ; from tables 10-13
  iOB1 table 	p40, 10  
  iOB2 table	p41, 10
  iOB3 table    p42, 10
  iOB4 table    p43, 10
  iOB5 table    p44, 10
  iOB6 table    p45, 10
  iOB7 table    p46, 10
  iOB8 table    p44, 10
  iOB9 table    p48, 10
  iOB10 table    p49, 10
  
  iVol1 table    p40, 11
  iVol2 table    p41, 11
  iVol3 table    p42, 11
  iVol4 table    p43, 11
  iVol5 table    p44, 11
  iVol6 table    p45, 11
  iVol7 table    p46, 11
  iVol8 table    p47, 11
  iVol9 table    p48, 11
  iVol10 table    p49, 11

  iBth1  table    p40, 12
  iBth2  table    p41, 12
  iBth3  table    p42, 12
  iBth4  table    p43, 12
  iBth5  table    p44, 12
  iBth6  table    p45, 12
  iBth7  table    p46, 12
  iBth8  table    p47, 12
  iBth9  table    p48, 12
  iBth10 table    p49, 12
  
  iAir1  table    p40, 13
  iAir2  table    p41, 13
  iAir3  table    p42, 13
  iAir4  table    p43, 13
  iAir5  table    p44, 13
  iAir6  table    p45, 13
  iAir7  table    p46, 13
  iAir8  table    p47, 13
  iAir9  table    p48, 13
  iAir10 table    p49, 13    	
 
;Convert Additive Notation of durations to % of idur 
;to be used in linseg below
	idur1 = p80 ; LITERAL SECONDS
	isubdiv = (idur-idur1)/(p81+p82+p83+p84+p85+p86+p87+p88) ; The rest are proportional
	idur2 = isubdiv*p81 
	idur3 = isubdiv*p81
	idur4 = isubdiv*p83
	idur5 = isubdiv*p84
	idur6 = isubdiv*p85
	idur7 = isubdiv*p86
	idur8 = isubdiv*p87
	idur9 = isubdiv*p88 
	
; EXPRESSION CONTROL - All linseg time-varying parameters  
  kvibamp    linseg p10 , idur1, p11, idur2, p12, idur3, p13, idur4, p14, idur5, p15, idur6, p16, idur7, p17, idur8, p18, idur9, p19 
  kvibspd    linseg p20 , idur1, p21, idur2, p22, idur3, p23, idur4, p24, idur5, p25, idur6, p26, idur7, p27, idur8, p28, idur9, p29     
  kpan       linseg p30 , idur1, p31, idur2, p32, idur3, p33, idur4, p34, idur5, p35, idur6, p36, idur7, p37, idur8, p38, idur9, p39     
  koverblow  linseg iOB1 , idur1, iOB2, idur2, iOB3, idur3, iOB4, idur4, iOB5, idur5, iOB6, idur6, iOB7, idur7, iOB8, idur8, iOB9, idur9, iOB10 
  kpressure  linseg iAir1, idur1, iAir2, idur2, iAir3, idur3, iAir4, idur4, iAir5, idur5, iAir6, idur6, iAir7, idur7, iAir8, idur8, iAir9, idur9, iAir10 
  krelbth    linseg iBth1*ibthatk , idur1, iBth2, idur2, iBth3, idur3, iBth4, idur4, iBth5, idur5, iBth6, idur6, iBth7, idur7, iBth8, idur8, iBth9, idur9, iBth10 
  krelamp    linseg 0,idur1, iVol2, idur2, iVol3, idur3, iVol4, idur4, iVol5, idur5, iVol6, idur6, iVol7, idur7, iVol8, idur8, iVol9, idur9, 0
  kgenamp    linseg p50,idur1, p51, idur2, p52, idur3, p53, idur4, p54, idur5, p55, idur6, p56, idur7, p57, idur8, p58, idur9, p59
  kpchoffset linseg p60, idur1, p61, idur2, p62, idur3, p63, idur4, p64, idur5, p65, idur6, p66, idur7, p67, idur8, p68, idur9, p69
  kbthpct    linseg p70, idur1, p71, idur2, p72, idur3, p73, idur4, p74, idur5, p75, idur6, p76, idur7, p77, idur8, p78, idur9, p79
  
; Control the i-values
  ibreath = (p6*.01)
  
; The values must be approximately -1 to 1 or the cubic will blow up.
  aflow1 rand kpressure
  kpchvibr oscil kvibamp*.1, kvibspd, 3
  kbthvibr oscil kvibamp*.01, kvibspd, 3
  kbreath = ((ibreath*krelbth)*kbthpct)+kbthvibr; ibreath = overall ref. volume,  krelbth = bthvol. from table adjusted for overblows
                                                   ; kbthpct = the expressive changes to ibreath via linseg
 
 ; Compute the audio sample 
  asum1 = kbreath*aflow1 + kpressure + kpchvibr
  asum2 = asum1 + aflute1*ifeedbk1
  afqc  = 1/(ifreq+kpchoffset) - asum1/20000 -9/sr + (ifreq+kpchoffset)/12000000 ; Get the Pitch
  atemp1 delayr 1/ifreq/2  ; Embouchure delay should be 1/2 the bore delay
  ax     deltapi afqc/koverblow ; - asum1/ifreq/10 + 1/1000
         delayw asum2
  apoly = ax - ax*ax*ax
  asum3 = apoly+aflute1*ifeedbk2
  
  ; Filter audio source sums
  aout01 tone asum3, 2000
  aout02 tone asum3, 2000

; Bore, the bore length determines pitch.  Shorter is higher pitch.
   atemp2   delayr 1/ifreq
   aflute1 deltapi afqc
           delayw aout01
  
  outs (aout01*(iamp*(kgenamp*krelamp)))*kpan, (aout02*(iamp*(kgenamp*krelamp)))*(1-kpan)

endin
</CsInstruments>

<CsScore>
;********************  WOOD FLUTE DEMO - PHYSICAL MODEL ****************
;        Dan Gutwein (2000) via Hans Mikelson via Perry Cook's slide flute



; Tables holding normalizing values for Overblow, Amp, Bth-vol, & Air-Pressure
; These produce "flat" responses from each "overblow" setting.  Expressive values that
; further modify these results are listed in the score p-fields 40 and higher. Air Pressure
; values should produce equal volumes for each overblow setting when genamp p40-49 are set at 1.
;
; OVERBLOW VARIABLE EFFECTS: assuming air-pressure = 1:  	
; 2           = pure-tone (fundamental)
; 2.5  - 2.76 = upper m2nd lip-roll
; 2.77 - 2.89 =       m2nd lip-roll & slight harmonic at m7th above lip-roll (Db -  B nat.)
; 2.9  - 3.9 =   "   M7th above fundamental (B nat.) - full and clear
; 4.0  - 7.00 =   "   oct.
; 4.97 - 5.00 =   "   #11th (with soft flat-9th below #11)	
; 7.01 - 7.5  =   "   12th (.486 is more complex - realistic)
; 7.50 - 8.0   =  "   oct + M7th 
; 9.0          =  "   2 oct.
; 9.5 - 10.00  =  "   2 oct. & M3rd  
;
f3 0 1024 10 1  
f10 0 16 -2     2    2.76  2.76   2.89   3.6       4     4.7    5    7.486  8      9    10;  Overblow values
f11 0 16 -2     1    .7     2      3      1.3     .8     2     2     5.5    4.5    13   15;  Relative Amp Values per overblow value
f12 0 16 -2     1    .4    .4     .15    .5       .6    .20   .2     .075   .1    .03  .03;  Breath vol. per  overblow value
f13 0 16 -2    .89   1     .91    .903   .85      .89   .89    .899  .933   .942   .970   1   ;  Air Pressure per overblow value

t 0 60 


; The following section is generated by the <ELISP> code
;-BEG-;
; (+k :pit 36 :dur 200 :vol 105)
i 1	0.0000	1.0417	798.8976	6.0000	4	12	0	0	0.8000	0.8000	0.8000	0.1000	0.2000	0.2000	0.2000	0.2000	0.2000	0.4000	2	2	2	4	8	8	8	2	2	2	0.5000	0.5000	0.5000	0.5000	0.5000	0.5000	0.5000	0.5000	0.5000	0.5000	0	0	0	0	0	0	0	0	0	0	0	1	1	1	1	1	1	1	1	0	0	0	0	0	0	0	0	0	0	0	1	1	1	1	1	1	1	1	1	1	0.1042	1	1	1	1	1	1	1	1
; (+k :pit 47 :dur 250)
i 1	1.0417	1.3021	798.8976	6.1100	4	12	0	0	0.2000	0.2000	0.4000	0.8000	0.8000	0.8000	0.1000	0.2000	0.2000	0.2000	4	8	8	8	2	2	2	2	2	2	0.5000	0.5000	0.5000	0.5000	0.5000	0.5000	0.5000	0.5000	0.5000	0.5000	0	0	0	0	0	0	0	0	0	0	0	1	1	1	1	1	1	1	1	0	0	0	0	0	0	0	0	0	0	0	1	1	1	1	1	1	1	1	1	1	0.1302	1	1	1	1	1	1	1	1
; (+k :pit 36 :dur 200)
i 1	2.3438	1.0417	798.8976	6.0000	4	12	0	0	0.2000	0.2000	0.2000	0.2000	0.2000	0.4000	0.8000	0.8000	0.8000	0.1000	8	2	2	2	2	2	2	4	8	8	0.5000	0.5000	0.5000	0.5000	0.5000	0.5000	0.5000	0.5000	0.5000	0.5000	0	0	0	0	0	0	0	0	0	0	0	1	1	1	1	1	1	1	1	0	0	0	0	0	0	0	0	0	0	0	1	1	1	1	1	1	1	1	1	1	0.1042	1	1	1	1	1	1	1	1
; (+k :pit 47 :dur 250)
i 1	3.3854	1.3021	798.8976	6.1100	4	12	0	0	0.8000	0.8000	0.1000	0.2000	0.2000	0.2000	0.2000	0.2000	0.4000	0.8000	2	2	2	2	4	8	8	8	2	2	0.5000	0.5000	0.5000	0.5000	0.5000	0.5000	0.5000	0.5000	0.5000	0.5000	0	0	0	0	0	0	0	0	0	0	0	1	1	1	1	1	1	1	1	0	0	0	0	0	0	0	0	0	0	0	1	1	1	1	1	1	1	1	1	1	0.1302	1	1	1	1	1	1	1	1
; (+k :pit 36 :dur 200)
i 1	4.6875	1.0417	798.8976	6.0000	4	12	0	0	0.2000	0.4000	0.8000	0.8000	0.8000	0.1000	0.2000	0.2000	0.2000	0.2000	2	4	8	8	8	2	2	2	2	2	0.5000	0.5000	0.5000	0.5000	0.5000	0.5000	0.5000	0.5000	0.5000	0.5000	0	0	0	0	0	0	0	0	0	0	0	1	1	1	1	1	1	1	1	0	0	0	0	0	0	0	0	0	0	0	1	1	1	1	1	1	1	1	1	1	0.1042	1	1	1	1	1	1	1	1
; (+k :pit 47 :dur 250)
i 1	5.7292	1.3021	798.8976	6.1100	4	12	0	0	0.2000	0.2000	0.2000	0.2000	0.4000	0.8000	0.8000	0.8000	0.1000	0.2000	8	8	2	2	2	2	2	2	4	8	0.5000	0.5000	0.5000	0.5000	0.5000	0.5000	0.5000	0.5000	0.5000	0.5000	0	0	0	0	0	0	0	0	0	0	0	1	1	1	1	1	1	1	1	0	0	0	0	0	0	0	0	0	0	0	1	1	1	1	1	1	1	1	1	1	0.1302	1	1	1	1	1	1	1	1
i 1	7.0313	1	0	6.1100	4	12	0	0	0.2000	0.2000	0.2000	0.2000	0.4000	0.8000	0.8000	0.8000	0.1000	0.2000	8	8	2	2	2	2	2	2	4	8	0.5000	0.5000	0.5000	0.5000	0.5000	0.5000	0.5000	0.5000	0.5000	0.5000	0	0	0	0	0	0	0	0	0	0	0	1	1	1	1	1	1	1	1	0	0	0	0	0	0	0	0	0	0	0	1	1	1	1	1	1	1	1	1	1	0.1302	1	1	1	1	1	1	1	1

;-END-;
e
</CsScore>
</CsoundSynthesizer>
