;; Tobiah's orchestra (what can it play ?)
;;
;; "I have an idea for a brief informal contest.  Given a simple,
;; one-oscillator orchestra, provide a score which produces the
;; most beautiful piece.

;; I offer an orchestra that I crafted for the purpose
;; of this event. One can make use of very limited envelope 
;; and pan controls. Obviously, the craft will
;; be in the score generation program (although one is
;; welcome to hand edit her entry!). " - Tobiah

;;
;; contribution by Stéphane Rollandin
;;  -  april 14, 2009  -


; (eval-buffer)
; (csl-play-composition my-composition)
; (csl-edit-composition my-composition)


(require 'csound-lsp)

(setq my-composition
 (csound-composition
  :sr 44100
  :ksmps 1
  :nchnls 2
  :orchestra (insert "

giSineFunc ftgen 1, 0, 65536, 10, 1

instr 1

       ;***** INIT SECTION *****
       iDur            init            p3
       iVol            init            p4
       iStartPitch     init            p5
       iEndPitch       init            p6

       iAttack         init            p7
       iDecay          init            iDur - iAttack

       iPanStart       init            p8
       iPanEnd         init            p9

       ;***** SYNTH SECTION *****
       kEnv            linseg          0, iAttack, iVol, iDecay, 0
       kPitch          expseg          iStartPitch, iDur, iEndPitch
       aSig            oscili          kEnv, kPitch, giSineFunc

       kPan            linseg          iPanStart, iDur, iPanEnd

       aLeft           =               aSig * kPan
       aRight          =               aSig * (1 - kPan)

                       outs            aLeft, aRight

endin ")
  :i-stream
  (i-be 1 0 0 20000 440 440 .3 0.5 0.05)

  (setq root 110)
  (slide-chord 10 0.4 root (* 3 30000)
	       (// 3 4 root) (// 5 4 root)
	       (// 5 11 root) (// 31 4 root)
	       (// 3 7 root) (// 27 4 root))
  (chord 3 0.001 root (* 3 30000)
	 (// 3 4 root) (// 5 4 root)
	 (// 5 11 root) (// 31 4 root)
	 (// 3 7 root) (// 27 4 root))

  (rest 1.2)

  (setq root 220)
  (chord 4 0.03 root (* 3 30000)
	 (// 3 4 root) (// 5 4 root)
	 (// 5 11 root) (// 7 4 root)
	 (// 3 7 root) (// 11 4 root))
  (slide-chord 10 0.01 root (* 3 30000)
	       (// 3 4 root) (// 5 4 root)
	       (// 5 11 root) (// 7 4 root)
	       (// 3 7 root) (// 11 4 root))
  (chord 7 0.03 root (* 3 30000)
	 (// 3 4 root) (// 5 4 root)
	 (// 5 11 root) (// 7 4 root)
	 (// 3 7 root) (// 11 4 root))

  (rest 0.8)

  (simple-chord 3 1200
		(// 5 4)
		1
		0.9)

  (simple-chord 5 1200
		(// 7 4)
		(// 3 2))

  (simple-chord 4 800
		(// 7 4)
		(// 3 2)
		0.7)

  (rest 0.5)

  (setq root 300)
  (slide-chord 12 0.3 root (* 3 29000)
	       (// 3 4 root) (// 5 4 root)
	       (// 3 11 root) (// 17 4 root)
	       (// 3 7 root) (// 11 4 root))
  (rest 0.2)
  (chord 4 0.01 root (* 3 23000)
	 (// 3 4 root) (// 5 4 root)
	 (// 3 11 root) (// 17 4 root)
	 (// 3 7 root) (// 11 4 root))
  (rest 0.1)
  (chord 5 0.005 root (* 3 31000)
	 (// 3 4 root) (// 5 4 root)
	 (// 3 11 root) (// 17 4 root)
	 (// 3 7 root) (// 11 4 root))
  (rest 0.1)
  (chord 9 0.0001 root (* 3 29000)
	 (// 3 4 root) (// 5 4 root)
	 (// 3 11 root) (// 17 4 root)
	 (// 3 7 root) (// 11 4 root))
  
  (slide-chord 25 0.8 root (* 3 30000)
 	       (// 4 3 root) (// 5 3 root)
	       (// 3 2 root) (// 7 4 root))

  (simple-chord 5 300
		(// 7 4)
		(// 3 2))

  (simple-chord 7 300
		(// 4 3)
		(// 5 3)
		0.85)

  (simple-chord 21 300
		(// 4 3)
		1
		0.65)))

(defun chord (dur af a vol b c &optional d e f g h i j k)
  (+i :p5 a :p6 a 
      :p4 (/ (float vol)
	     (- 11 (count nil '(d e f g h i j k))))
      :p3 dur :p7 (* af dur))
  (|i :p5 b :p6 b :p8 0.6 :p9 0.4)
  (|i :p5 c :p6 c :p8 0.4 :p9 0.6)
  (when d
    (|i :p5 d :p6 d :p8 0.7 :p9 0.4)
    (|i :p5 e :p6 e :p8 0.3 :p9 0.6))
  (when f
    (|i :p5 f :p6 f :p8 0.8 :p9 0.5)
    (|i :p5 g :p6 g :p8 0.2 :p9 0.5))
  (when h
    (|i :p5 h :p6 h :p8 0.7 :p9 0.3)
    (|i :p5 i :p6 i :p8 0.3 :p9 0.7))
  (when j
    (|i :p5 j :p6 j :p8 0.7 :p9 0.3)
    (|i :p5 k :p6 k :p8 0.3 :p9 0.7)))


(defun slide-chord (dur af a vol b c &optional d e f g h i j k)
  (+i :p5 a :p6 b
      :p4 (/ (float vol)
	     (- 11 (count nil '(d e f g h i j k))))
      :p3 dur :p7 (* af dur))
  (|i :p5 b :p6 c :p8 0.8 :p9 0.3)
  (|i :p5 c :p6 a :p8 0.3 :p9 0.8)
  (when d
    (|i :p5 d :p6 e :p8 0.7 :p9 0.5)
    (|i :p5 e :p6 d :p8 0.3 :p9 0.5))
  (when f
    (|i :p5 f :p6 g :p8 0.9 :p9 0.2)
    (|i :p5 g :p6 f :p8 0.1 :p9 0.8))
  (when h
    (|i :p5 h :p6 i :p8 0.7 :p9 0.3)
    (|i :p5 i :p6 h :p8 0.3 :p9 0.7))
  (when j
    (|i :p5 j :p6 k :p8 0.7 :p9 0.3)
    (|i :p5 k :p6 j :p8 0.3 :p9 0.7)))
	  

(defun simple-chord (d root a b &optional v)
  (+i :p5 (* root a) :p6 (* root a)
      :p4 (* 15000 (or v 1))
      :p8 0.7
      :p9 0.2
      :p3 d :p7 (* d .3))
  (|i :p5 (* root b) :p6 (* root b)
      :p4 (* 10000 (or v 1))
      :p8 0.2
      :p9 0.8
      :p3 d :p7 (* d .1)))

