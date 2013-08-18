<CsoundSynthesizer>
<CsOptions>
</CsOptions>

<CsInstruments>

sr = 44100
kr = 4410
ksmps = 10  
nchnls = 2
0dbfs = 32767

; LOAD SOUNDFONT
  giPiano   fluidEngine	  
  ginum	    fluidLoad	"../../timidity/WarrenTrachmanPiano.sf2", giPiano, 1
	    fluidProgramSelect			  giPiano, 1, ginum, 0, 0

    instr 1
  ; INITIALIZATION
	    mididefault	  60, p3 		  ; Default duration of 60 -- overridden by score.
	    midinoteonkey   p4, p5 		  ; Channels MIDI input to pfields.
  ikey	    =  p4
  ivelocity =  p5
	    fluidNote	giPiano, 1, ikey, ivelocity
    endin

    instr 100 ; Fluidsynth output
  kenv	    linseg    0, 0.006*p3, 0.996, 0.05*p3, 0.989, 0.009*p3, 0.595, 0.009*p3, 0.536, 0.009*p3, 0.498, 0.009*p3, 0.469, 0.019*p3, 0.425, 0.019*p3, 0.392, 0.028*p3, 0.353, 0.038*p3, 0.312, 0.047*p3, 0.272, 0.066*p3, 0.228, 0.085*p3, 0.183, 0.113*p3, 0.135, 0.151*p3, 0.084, 0.198*p3, 0.032, 0.142*p3, 0
  ; Normalize so kamplitude for p5 of 80 == ampdb(80).
  kamplitude   =      ampdb(p5) * (10000.0 / 0.1) * kenv
  aleft, aright	      fluidOut			  giPiano
	    outs      aleft * kamplitude, aright * kamplitude
    endin

</CsInstruments>

<CsScore>
i1 0 5 39 60
i1 0.5 5 66 64
i1 2 5 45 64
i1 3 4 80 64

i100 0 7

</CsScore>
</CsoundSynthesizer>
