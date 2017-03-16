; ----- @macros@ -----

zpbyte  .macro
        .proff
        .for ,(*) in [$c5, $c6,$f5,$f6],
        .byte ?
        .next
-       .byte ?
        .pron
        .endm -

zpword  .macro
        .proff
        .for ,any((*, *+1) == [$c5, $c6,$f5,$f6]),
        .byte ?
        .next
-       .word ?
        .pron
        .endm -
      
dfont 	.function f       
		.proff		
i		:= f
		.for n = 0, n < len(f)/8, n = n + 1		
r		:= bits(0)				
		.for m = 0, m < 8, m = m + 1		
r      	..= (i[0] == '#') ? %1 : %0				
i 		:= i[1:]				
		.if i == '' 				
			.break				
		.endif				
		.next				
		.pron				
		.byte r				
		.proff						
		.next		
		.pron		
		.endf		
				       
STAPW .macro 
	lda #>(\1)
	sta (\2)+1 
	lda #<(\1)
	sta \2
.endm

STAB .macro
	lda \1
	sta \2
.endm

ADCBW .macro
	clc
	lda \1
	adc \2
	sta \1
	bcc +
	inc (\1)+1
+
.endm

ADCB .macro
	clc
	lda \1
	adc \2
	sta \1
.endm

ADCW .macro
	clc
	lda \1
	adc \2
	sta \1
	lda (\1)+1
	adc (\2)+1
	sta (\1)+1
.endm

ADCIW .macro
	clc
	lda \1
	adc #<(\2)
	sta \1
	lda (\1)+1
	adc #>(\2)
	sta (\1)+1
.endm

ORI .macro
	lda \1
	ora #\2
	sta \1
.endm

mCallFunctionTable .macro
	lda \1.hi,\2
	pha
	lda \1.lo,\2
	pha
	.INR
	rts
.endm

mMakeFunctionTable .macro	
lo .byte <(\@)-1	
hi .byte >(\@)-1	
.endm	

mMakeTable .macro
lo .byte <(\@)
hi .byte >(\@)
.endm

kBitmapColours .block
	DarkRow = $16
	LightRow = $1E
	EmptyTile = $0B
	CurrRow = $07
.bend

kVectors .block
	Bitmap = $A000
	Screen = $8000
	Sprites = $8400
.bend

kTileMode .block
	empty = 0
	solid = 1
	invalid = 2
.bend

kOffsets .block
	y = 9
	x = 24
.bend

kMapSize .block
	size8 = 0
	size16 = 1
.bend

kMusic .block
	mark = 0
	low_time = 1
	fail = 2
	pass = 3
.bend

; ----- @ZP@ -----

* = $02
PuzzlePointer .zpword
MenuPointer .zpword
CursorX .zpbyte 
CursorY .zpbyte 
Pointer1 .zpword
Pointer2 .zpword
Pointer3 .zpword
Pointer4 .zpword 
ZPTemp1 .zpbyte
ZPTemp2 .zpbyte
ZPTemp3 .zpbyte
ZPTemp4 .zpbyte
ZPTemp5 .zpbyte
ZPTemp6 .zpbyte
ZPTemp7 .zpbyte
ZPTemp8 .zpbyte
ZPTemp9 .zpbyte
ZPTemp10 .zpbyte
MenuItem .zpbyte
MapSize .zpbyte
MaxCusor .zpbyte
DoneLowTime .zpbyte
ForceExitKeyboardLoop .zpbyte
TimerShadow .zpword 
PeneltyMins .zpbyte 
UpFunc .zpword 
DownFunc .zpword 
LeftFunc .zpword 
RightFunc .zpword 
ValidFunc .zpword 
InvalidFunc .zpword 
CurrFuncPointer .zpword 
NextFuncPointer .zpword 
AD_Cycle_con          .zpbyte                   ;> d405 Attack decay cycle
SR_Cycle_con          .zpbyte                   ;> d406 sustain release cycle
Frequency_Hi_Start_A  .zpbyte 
Frequency_Hi_Start_B  .zpbyte 
Frequency_Lo_delta    .zpbyte 
Frequency_Hi_delta    .zpbyte 
Pulse_wave_wide_hi    .zpbyte                   ;>d403 pulse waveform width hi
f0C29                 .zpbyte 
f0C2A                 .zpbyte 
f0C2B                 .zpbyte 
Frequency_Lo_B_delta  .zpbyte 
Frequency_Hi_B_delta  .zpbyte 
Pulse_wave_wide_hi_B  .zpbyte 
Pulse_wave_delta_lo   .zpbyte 
Voice_controll        .zpbyte                   ;>d404 voice control register
Voice_control_B       .zpbyte 
f0C32                 .zpbyte 
f0C33                 .zpbyte                   ;if lower 2 bits < 2 then 0c37 = 0c24 else 0c37 = 0c25
f0C34                 .zpbyte                   ;> 0c3e
f0C35                 .zpbyte 
Frequency_lo          .zpbyte                   ;> d400 frequency control lo
Frequency_hi          .zpbyte                   ;> d401 frequency cotrol hi
Frequency_lo_B        .zpbyte                   ;>d400
Frequency_Hi_B        .zpbyte                   ;>d401
f0C3A                 .zpbyte 
Freq_pulse_set_selec  .zpbyte                   ;if not 0 choose B
Pulse_wave_wide_lo    .zpbyte                   ;>d402 pulse waveform width lo
Pulse_wave_wide_lo_B  .zpbyte                   ;>d402
f0C3E                 .zpbyte 
; ----- @Variables@ -----

variables = $0400
* = $0400
ActiveMap .fill 256 
TargetMap .fill 256 
CharacterBuffer .fill 32
PuzzleBuffer .fill 32
.comment
TimerShadow .word ?
PeneltyMins .byte ?
UpFunc .word ?
DownFunc .word ?
LeftFunc .word ?
RightFunc .word ?
ValidFunc .word ?
InvalidFunc .word ?
CurrFuncPointer .word ?
NextFuncPointer .word ?
AD_Cycle_con          .BYTE ?                  ;> d405 Attack decay cycle
SR_Cycle_con          .BYTE ?                  ;> d406 sustain release cycle
Frequency_Hi_Start_A  .BYTE ?
Frequency_Hi_Start_B  .BYTE ?
Frequency_Lo_delta    .BYTE ?
Frequency_Hi_delta    .BYTE ?
Pulse_wave_wide_hi    .BYTE ?                  ;>d403 pulse waveform width hi
f0C29                 .BYTE ?
f0C2A                 .BYTE ?
f0C2B                 .BYTE ?
Frequency_Lo_B_delta  .BYTE ?
Frequency_Hi_B_delta  .BYTE ?
Pulse_wave_wide_hi_B  .BYTE ?
Pulse_wave_delta_lo   .BYTE ?
Voice_controll        .BYTE ?                  ;>d404 voice control register
Voice_control_B       .BYTE ?
f0C32                 .BYTE ?
f0C33                 .BYTE ?                  ;if lower 2 bits < 2 then 0c37 = 0c24 else 0c37 = 0c25
f0C34                 .BYTE ?                  ;> 0c3e
f0C35                 .BYTE ?
Frequency_lo          .BYTE ?                  ;> d400 frequency control lo
Frequency_hi          .BYTE ?                  ;> d401 frequency cotrol hi
Frequency_lo_B        .BYTE ?                  ;>d400
Frequency_Hi_B        .BYTE ?                  ;>d401
f0C3A                 .BYTE ?
Freq_pulse_set_selec  .BYTE ?                  ;if not 0 choose B
Pulse_wave_wide_lo    .BYTE ?                  ;>d402 pulse waveform width lo
Pulse_wave_wide_lo_B  .BYTE ?                  ;>d402
f0C3E                 .BYTE ?
.endc
.include "strings.lab"

; ----- @START@ -----

.enc none
*= $0801 ; 00 0C 08 0A 00 9E 20 32 30 36 34 00 00
	.word (+), 2005 ;pointer, line number
	.null $9e, ^start;will be sys 4096
+	.word 0	 ;basic line end
		
*= $080D
start
;&&trashes _DONT_CARE_
	sei
	lda #$7f
	sta $dc0d		 ;turn off all types of cia irq/nmi.
	sta $dd0d
	lda $dc0d
	lda $dd0d
	lda #$ff
	sta $D019
	lda #$00
	sta $D01a
	jsr buildSprites
	jsr LOAD
	sei
	lda #$01
	sta $D01a
	; move VIC bank to $4000
	#ORI $dd02,3
	lda $dd00
	and #252
	ora #1
	sta $dd00
	#STAB #0,$D020
	#STAB #%00001000,$D016 ; 40 cols no scroll
TitleScreen
;&&trashes a,x,y,Pointer2+1,Pointer3,Pointer2,Pointer1+1,Pointer1
;&&trashes Pointer3+1,ZPTemp1,MenuPointer,ZPTemp10
;&&modifies CursorX,CursorY,MenuItem,UpFunc,CurrFuncPointer,ForceExitKeyboardLoop
	jsr setTextScreen
	ldx #size(TitleStrList._x)-1
	stx ZPTemp10
-	lda TitleStrList._x,x
	pha
	ldy TitleStrList._y,x
	lda TitleStrList._string,x
	tax
	pla
	;lda #4
	;ldy #10
	;ldx #2
	jsr plotStringAAtIndexX
	dec ZPTemp10
	ldx ZPTemp10
	bpl -
	#STAPW EasyHardMenu,MenuPointer
	jsr plotAndSetupMenuInMenuPointer
	#STAPW KeyboardRoutine,CurrFuncPointer
	lda #$0
	sta $d020
	sta $d021
	sta ForceExitKeyboardLoop
	
	lda #15
	sta $d418

	lda #10
	sta $d012
	#STAPW musicIRQ,$0314
	cli
	
MainLoop
	jmp (CurrFuncPointer)
	
setupPicrossScreen
;&&trashes _DONT_CARE_
	sei
	; make a bitmap in upper half so $6000, screen $4000 
	#STAB #8,$D018	
	#STAB #%00111011,$D011
	jsr clearBitmap
	jsr clearScreenRAMBitmap
	jsr plotPICROSSBitmapLogo
	lda MapSize
	beq +
	jsr setUp16_16
	jsr copy16x16PuzzleToPuzzleBuffer
	jmp ++
+	jsr setUp8_8
	jsr copy8x8PuzzleToPuzzleBuffer
+	jsr clearMaps
	jsr unpackPuzzleToTarget
	lda MapSize
	beq +
	jsr doRowPlots
	jsr setSprites
	jmp ++
+	jsr doRowPlots8
+	jsr setTimerTo30Min
	jsr plotTimer
	jsr setUpNMITOD
	lda #0
	sta CursorX
	sta CursorY
	jsr XorScreenCursor
	ldx #9
	jsr setHorizontalDigitsBarToCurrent
	ldy #24
	jsr setVerticalDigitsBarToCurrent
	#STAB #1,PeneltyMins	
	ldx #11
-	lda PicrossKeyboardFuncTBL,x
	sta UpFunc,x
	dex
	bpl -
	#STAPW KeyboardRoutine,CurrFuncPointer
	jsr setKeyboardToPuzzleMode
	lda #0
	sta DoneLowTime
	cli
EmptyFunction
	jmp MainLoop
	
PicrossKeyboardFuncTBL 		
	.byte <W,>W,<S,>S,<pressA,>pressA,<D,>D,<F5,>F5,<F7,>F7		
KeyboardRoutine
;&&trashes a,x,y,ZPTemp1
;&&modifies ForceExitKeyboardLoop
-	jsr $FF9F ; SCAN KEY
	jsr $FFE4 ; GETIN
	bne +
	lda ForceExitKeyboardLoop
	bne -
	dec ForceExitKeyboardLoop
	jmp MainLoop
+	sta ZPTemp1
_KRXORONE
	jsr XorScreenCursor
	lda #>_exit-1
	pha
	lda #<_exit-1
	pha				; store exit addr for rts
	lda ZPTemp1
	cmp #65
	;beq pressA
	bne +
	jmp (LeftFunc)
+	cmp #68
	;beq D
	bne +
	jmp (RightFunc)
+	cmp #83
	;beq S
	bne +
	jmp (DownFunc)
+	cmp #87 ; W
	;beq W
	bne +
	jmp (UpFunc)
+	cmp #135
	;beq F5
	bne +
	jmp (ValidFunc)
+	cmp #136
	;beq F7	
	bne _exitPLA	
	jmp (InvalidFunc)	
_exitPLA
	pla	; didn't jump so remove rts from stack
	pla	
_exit
;&&trashes a,x,y,Pointer1,Pointer1+1
_KRXORTWO
	jsr XorScreenCursor
	jmp MainLoop
	
setKeyboardToPuzzleMode
;&&trashes a
;&&modifies KeyboardRoutine._KRXORONE,KeyboardRoutine._KRXORTWO
	lda #$20 ;jsr
	sta KeyboardRoutine._KRXORONE
	sta KeyboardRoutine._KRXORTWO
	rts

setKeyboardToMenuMode
;&&trashes a
;&&modifies KeyboardRoutine._KRXORONE,KeyboardRoutine._KRXORTWO
	lda #$2c ; BIT XXXX
	sta KeyboardRoutine._KRXORONE
	sta KeyboardRoutine._KRXORTWO
	rts
	
pressA	
;&&trashes a,x,y,Pointer1,Pointer1+1,Pointer2,Pointer2+1
;&&modifies CursorX
	lda CursorX	
	beq KeyboardRoutine._exitPLA
	clc 
	adc #24
	tay
	jsr restoreVerticalDigitsBar ;&&preserve y
	dey
	jsr setVerticalDigitsBarToCurrent	
	dec CursorX
	rts ;gpl KeyboardRoutine._exit	
D
;&&trashes a,x,y,Pointer1,Pointer1+1,Pointer2,Pointer2+1
;&&modifies CursorX	
	lda CursorX
	cmp MaxCusor
	beq KeyboardRoutine._exitPLA
	clc 
	adc #24
	tay
	jsr restoreVerticalDigitsBar ;&&preserve y
	iny
	jsr setVerticalDigitsBarToCurrent
	inc CursorX
	rts ;gpl KeyboardRoutine._exit

S	
;&&trashes a,x,y,Pointer1,Pointer1+1,Pointer2,Pointer2+1
;&&modifies CursorY
	lda CursorY
	cmp MaxCusor
	beq KeyboardRoutine._exitPLA
	clc
	adc #9
	tax
	jsr restoreHorizontalDigitsBar
	inx
	jsr setHorizontalDigitsBarToCurrent
	inc CursorY
	rts ;gpl KeyboardRoutine._exit
W	
;&&trashes a,x,y,Pointer1,Pointer1+1,Pointer2,Pointer2+1
;&&modifies CursorY
	lda CursorY	
	beq KeyboardRoutine._exitPLA
	clc
	adc #9
	tax
	jsr restoreHorizontalDigitsBar
	dex
	jsr setHorizontalDigitsBarToCurrent
	dec CursorY
	rts ;gpl KeyboardRoutine._exit
F7	; set tile not 
;&&trashes a,x,y,Pointer1,Pointer1+1
;&&modifies ActiveMap
	jsr convertCursorToIndex
	tax
setInvalid
;&&trashes a,x,y,Pointer1,Pointer1+1
;&&modifies ActiveMap
	lda ActiveMap,x
	cmp #kTileMode.solid
	beq _F7Exit
	eor #2
	sta ActiveMap,x
	jsr updateScreen
_F7Exit
	rts ; jmp KeyboardRoutine._exit
	
F5	; set tile
;&&trashes a,x,y,Pointer1,Pointer1+1,Pointer2,Pointer2+1,Pointer3,Pointer3+1,ZPTemp10
;&&modifies ActiveMap,TimerShadow,TimerShadow+1,PeneltyMins,CurrFuncPointer
	jsr convertCursorToIndex
	tax
	lda TargetMap,x
	cmp #kTileMode.solid
	bne _fail
	lda ActiveMap,x
	cmp #kTileMode.empty
	bne _F5Exit
	eor #1
	sta ActiveMap,x
	jsr updateScreen	
	jsr checkComplete
	ldy #kMusic.mark
	jsr SFX_s0C84 ; start sfx

_F5Exit	
	rts ;jmp KeyboardRoutine._exit
_fail
	stx ZPTemp10
	sei
	sed
	lda TimerShadow
	sec
	sbc PeneltyMins
	cmp #$30
	bcc +
	lda #0
	sta TimerShadow+1
+	sta TimerShadow 
	lda PeneltyMins	
	cmp #8
	bcs +
	;clc	
	adc PeneltyMins	
	sta PeneltyMins	
+	cld	
	cli
	jsr plotTimer	
	ldy #kMusic.pass
	jsr SFX_s0C84
	ldx ZPTemp10
	jmp setInvalid
	
checkComplete
;&&trashes a,x,y,Pointer1,Pointer1+1,ZPTemp1,ZPTemp2,ZPTemp3,ZPTemp4
;&&trashes ZPTemp5,ZPTemp6,ZPTemp7,ZPTemp8,ZPTemp9,Pointer3,Pointer3+1
;&&modifies convert2PuzzleRowsTo4_4._8x8OverStep,CurrFuncPointer
	ldx #0
-	lda TargetMap,x
	cmp #kTileMode.solid
	bne _next
	cmp ActiveMap,x
	bne _fail
_next
	inx
	bne -
	; pass
	jsr plotCompletePuzzle
	jsr XorScreenCursor ; turn off curent cursor
	jsr setUp5SecondTimer
	lda MenuItem	
	jsr mulAby7
	tay
	iny
	iny
	lda (MenuPointer),y
	tay
	lda StringTableLUT.lo,y
	sta Pointer1
	lda StringTableLUT.hi,y
	sta Pointer1+1
	ldy #2
	lda #' '
	sta (Pointer1),y
	#STAPW EmptyFunction,CurrFuncPointer
	jsr SAVE
_fail
	rts
	
clearBitmap
;&&trashes a,y,Pointer1,Pointer1+1
	#STAPW kVectors.Bitmap,Pointer1
	ldy #0
_l2	lda #0
	sta (Pointer1),y		
	iny		
	bne _l2		
	inc Pointer1+1		
	lda Pointer1+1		
	cmp # >(kVectors.Bitmap)+8192		
	bne _l2		
	rts		
				
clearScreenRAMBitmap				
;&&trashes a,x,y,Pointer1,Pointer1+1						
;&&modifies kVectors.Screen,kVectors.Screen+$100			
	lda #$10
	pha
	bne clearScreenRAMInternal
clearScreenRAMScreen
;&&trashes a,x,y,Pointer1,Pointer1+1			
;&&modifies kVectors.Screen,kVectors.Screen+$100
	lda #$20
	pha
clearScreenRAMInternal	
;&&trashes a,x,y,Pointer1,Pointer1+1			
;&&modifies kVectors.Screen,kVectors.Screen+$100			
	#STAPW kVectors.Screen,Pointer1		
	pla				
	ldx #3
_loop
	ldy #0				
-	sta (Pointer1),y					
	dey				
	bne -				
	inc Pointer1+1	
	dex	
	bpl _loop	
	rts				

setTextScreen				
;&&trashes a,y			
;&&modifies kVectors.Screen,kVectors.Screen+$100				
	#STAB #%00011011,$D011				
	#STAB #%00000110,$D018					
	jmp clearScreenRAMScreen					
	;rts
					
buildSprites			
;&&trashes a,y			
;&&modifies kVectors.Sprites,kVectors.Sprites+64,kVectors.Sprites+65,kVectors.Sprites+66			
	lda #0			
	ldy #127			
-	sta kVectors.Sprites,y
	dey
	bpl -
	lda #$80
	ldy #61
-	sta kVectors.Sprites,y
	dey
	dey
	dey
	bpl -
	lda #$FF
	sta kVectors.Sprites+64
	sta kVectors.Sprites+65	
	sta kVectors.Sprites+66	
	rts	
		
SpritesXY .byte 15,73+50,15,73+50+42,15,73+50+42+42,216,185,08,185,56,185 ; 12 bytes		
setSprites	
;&&trashes a,x
;&&modifies kVectors.Screen+$3F8,kVectors.Screen+$3F9,kVectors.Screen+$3FD
;&&modifies kVectors.Screen+$3FA,kVectors.Screen+$3FB,kVectors.Screen+$3FC
	ldx #11
-	lda SpritesXY,x 
	sta $D000,x
	dex
	bpl -
	lda #%00110111
	sta $D010
	lda #%00111111
	sta $D015
	lda #0
	sta $D01C
	sta $D01B
	lda #%0000111
	sta $D017
	lda #%0111000
	sta $D01D
	lda #12
	ldx #7
-	sta $D027,x 
	dex	
	bpl -	
	ldx #$10	
	stx kVectors.Screen+$3F8	
	stx kVectors.Screen+$3F9	
	stx kVectors.Screen+$3FA	
	inx	
	stx kVectors.Screen+$3FB	
	stx kVectors.Screen+$3FC	
	stx kVectors.Screen+$3FD	
	rts	 	
					
restoreVerticalDigitsBar					
;&&trashes a,x,Pointer1,Pointer2,Pointer1+1,Pointer2+1				
	#STAPW kVectors.Screen,Pointer1
	tya
	and #1
	tax
	lda ColourDigitsBarsLUT,x
	ldx #8
	jmp pltScreenColoumn					
					
setVerticalDigitsBarToCurrent				
;&&trashes a,x,Pointer1,Pointer2,Pointer1+1,Pointer2+1			
	#STAPW kVectors.Screen,Pointer1
	lda #kBitmapColours.CurrRow
	ldx #8
	jmp pltScreenColoumn				
				
restoreHorizontalDigitsBar			
;&&trashes a,y,Pointer1,Pointer2,Pointer1+1,Pointer2+1			
	jsr screenRowToPointer1
	txa
	and #1
	tay
	lda ColourDigitsBarsLUT,y
	ldy #23
	jmp pltScreenRow ;&&preserve x		

setHorizontalDigitsBarToCurrent			
;&&trashes a,y,Pointer1,Pointer2,Pointer1+1,Pointer2+1			
	jsr screenRowToPointer1
	lda #kBitmapColours.CurrRow
	ldy #23
	jmp pltScreenRow ;&&preserve x						
						
screenRowToPointer1
;&&trashes a
;&&modifies Pointer1,Pointer1+1
	lda screenRowLUTLO,x
	sta Pointer1
	lda screenRowLUTHi,x
	sta Pointer1+1
	rts
	
plotGridColours						
;;&&trashes a,x,y,Pointer1,Pointer1+1,ZPTemp1						
	sta ZPTemp1						
	ldx #9
-	ldy #15
	lda screenRowLUTLO,x
	clc
	adc #24
	sta Pointer1
	lda screenRowLUTHi,x
	adc #0
	sta Pointer1+1
	lda ZPTemp1
	jsr pltScreenRow
	inx
	cpx #25
	bcc -						
	rts						

plotGridColours8_8						
;;&&trashes a,x,y,Pointer1,Pointer1+1,ZPTemp1						
	sta ZPTemp1						
	ldx #9
-	ldy #7
	lda screenRowLUTLO,x
	clc
	adc #24
	sta Pointer1
	lda screenRowLUTHi,x
	adc #0
	sta Pointer1+1
	lda ZPTemp1
	jsr pltScreenRow
	inx
	cpx #9+8
	bcc -						
	rts								
								
ColourDigitsBarsLUT .byte kBitmapColours.DarkRow,kBitmapColours.LightRow			

setupVersionData			
.byte 40,25,15,127,<plotGridColours,>plotGridColours  ;16
.byte 32,17,7,63,<plotGridColours8_8,>plotGridColours8_8 ; 8 			
setUp16_16
;&&trashes a,x,y,Pointer1,Pointer1+1,Pointer2,Pointer2+1,ZPTemp1
	ldy #0
	beq setUp_Internal
;{{{
.comment
; vertical bars
	ldy #24
-	jsr restoreVerticalDigitsBar ;&&preserve y
	iny
	cpy #40
	bcc -
; horizontal
	ldx #9
-	jsr restoreHorizontalDigitsBar ;&&preserve x
	inx
	cpx #25
	bcc -
; set grid colours
	lda #kBitmapColours.EmptyTile
	jsr plotGridColours
	; now write bitmapdata
	lda #>kVectors.Bitmap+(9*320)+(24*8)
	sta Pointer1+1
	sta Pointer2+1 
	lda #<kVectors.Bitmap+(9*320)+(24*8)
	sta Pointer1
	sta Pointer2
	ldx #15
-	ldy #127
-	tya
	and #7
	cmp #7
	bne _single
_line
	lda #$FF
	gne _store
_single
	lda #%00000001;#%11111110
_store	
	sta (Pointer1),y	
	dey	
	bpl -	
	jsr add320Pointer12 ;&&preserve x,y
	dex
	bpl --
	rts	
.endc
;}}}
setUp8_8
;&&trashes a,x,y,Pointer1,Pointer1+1,Pointer2,Pointer2+1,ZPTemp1
	ldy #6
	;drop through
;{{{
.comment
; vertical bars
	ldy #24
-	jsr restoreVerticalDigitsBar ;&&preserve y
	iny
	cpy #24+8
	bcc -
; horizontal
	ldx #9
-	jsr restoreHorizontalDigitsBar ;&&preserve x
	inx
	cpx #9+8
	bcc -
; set grid colours
	lda #kBitmapColours.EmptyTile
	jsr plotGridColours8_8
	; now write bitmapdata
	lda #>kVectors.Bitmap+(9*320)+(24*8)
	sta Pointer1+1
	sta Pointer2+1 
	lda #<kVectors.Bitmap+(9*320)+(24*8)
	sta Pointer1
	sta Pointer2
	ldx #7
-	ldy #63
-	tya
	and #7
	cmp #7
	bne _single
_line
	lda #$FF
	gne _store
_single
	lda #%00000001;#%11111110
_store	
	sta (Pointer1),y	
	dey	
	bpl -	
	jsr add320Pointer12 ;&&preserve x,y
	dex
	bpl --
	rts			
.endc
;}}}
setUp_Internal
;&&trashes a,x,y,Pointer1,Pointer1+1,Pointer2,Pointer2+1,ZPTemp1,ZPTemp2,ZPTemp3,ZPTemp4
;&&modifies _pointerToGrid,_pointerToGrid+1
	ldx #0
-	lda setupVersionData,y
	sta ZPTemp1,x
	iny
	inx
	cpx #4
	bne -
	lda setupVersionData,y
	sta _pointerToGrid
	lda setupVersionData+1,y
	sta _pointerToGrid+1
	
	ldy #24
-	jsr restoreVerticalDigitsBar ;&&preserve y,ZPTemp1,ZPTemp2,ZPTemp3,ZPTemp4
	iny
	cpy ZPTemp1
	bcc -
; horizontal
	ldx #9
-	jsr restoreHorizontalDigitsBar ;&&preserve x,ZPTemp1,ZPTemp2,ZPTemp3,ZPTemp4
	inx
	cpx ZPTemp2
	bcc -
; set grid colours
	lda #kBitmapColours.EmptyTile
_pointerToGrid = *+1
	jsr plotGridColours8_8
	; now write bitmapdata
	lda #>kVectors.Bitmap+(9*320)+(24*8)
	sta Pointer1+1
	sta Pointer2+1 
	lda #<kVectors.Bitmap+(9*320)+(24*8)
	sta Pointer1
	sta Pointer2
	ldx ZPTemp3
-	ldy ZPTemp4
-	tya
	and #7
	cmp #7
	bne _single
_line
	lda #$FF
	gne _store
_single
	lda #%00000001;#%11111110
_store	
	sta (Pointer1),y	
	dey	
	bpl -	
	jsr add320Pointer12 ;&&preserve x,y
	dex
	bpl --
	rts						
				
add320Pointer12	
;&&trashes a	
;&&modifies Pointer2,Pointer2+1,Pointer1,Pointer1+1	
	lda Pointer2
	clc
	adc #<320
	sta Pointer2
	sta Pointer1
	lda Pointer2+1
	adc #>320
	sta Pointer2+1
	sta Pointer1+1	
	rts	
		
pltScreenColoumn	
;&&trashes a,x,Pointer1	
-	sta (Pointer1),y	
	pha
	#ADCBW Pointer1,#40
	pla
	dex
	bpl -
	rts
	
pltScreenRow	
;&&trashes y
-	sta (Pointer1),y	
	dey	
	bpl -	
	rts	
	
XorScreenCursor
;&&trashes a,x,y,Pointer1,Pointer1+1
	lda CursorX
	clc
	adc #24
	tay
	ldx CursorY
	lda screenRowLUTLO+9,x
	sta Pointer1
	lda screenRowLUTHi+9,x
	sta Pointer1+1
	lda (Pointer1),y
	eor #$10
	sta (Pointer1),y
	rts

clearMaps	
;&&trashes a,x	
;&&modifies ActiveMap,TargetMap	
	ldx #0	
	txa	
-	sta ActiveMap,x	
	sta TargetMap,x	
	dex	
	bne -	
	rts	
	
convertCursorToIndex
;&&modifies a
	lda CursorY
	asl a
	asl a
	asl a
	asl a
	ora CursorX
	rts
	
updateScreen
;&&trashes a,x,y,Pointer1,Pointer1+1
	pha
	lda CursorY
	clc
	adc #kOffsets.y
	tax
	lda screenRowLUTLO,x
	sta Pointer1
	lda screenRowLUTHi,x
	sta Pointer1+1
	lda CursorX
	clc
	adc #kOffsets.x
	tay
	pla
	tax
	lda (Pointer1),y
	and #$f0
	ora TileColoursForMode,x
	sta (Pointer1),y
	rts	

copy16x16PuzzleToPuzzleBuffer
;&&trashes a,y
;&&modifies PuzzleBuffer,MaxCusor
	ldy #31
-	lda (PuzzlePointer),y
	sta PuzzleBuffer,y
	dey
	bpl -
	lda #15
	sta MaxCusor
	rts
	
copy8x8PuzzleToPuzzleBuffer
;&&trashes a,x,y
;&&modifies PuzzleBuffer,MaxCusor
	ldx #31
	lda #0
-	sta PuzzleBuffer,x
	dex
	bpl -
	ldy #7
	ldx #14
-	lda (PuzzlePointer),y
	sta PuzzleBuffer,x
	dex
	dex
	dey
	bpl -
	lda #7
	sta MaxCusor
	rts
	
unpackPuzzleToTarget
;&&trashes a,x,y,Pointer1,Pointer1+1,Pointer2,Pointer2+1,ZPTemp1,ZPTemp2,ZPTemp3
;&&modifies TargetMap
	#STAPW PuzzleBuffer,Pointer1
	#STAPW TargetMap,Pointer2
	lda #0
	sta ZPTemp1
	sta ZPTemp2
_byteLoop
	ldx #7
	ldy ZPTemp1
	lda (Pointer1),y
	sta ZPTemp3
_bitLoop	
	lda ZPTemp3
	asl a
	sta ZPTemp3
	txa
	ldx ZPTemp2
	rol TargetMap,x
	inc ZPTemp2
	tax
	dex
	bpl _bitLoop
	inc ZPTemp1
	lda ZPTemp1
	cmp #32
	bne _byteLoop
	rts
	
doRowPlots
;&&trashes a,x,y,Pointer1,Pointer1+1,ZPTemp4,Pointer3,Pointer3+1
;&&trashes ZPTemp1,ZPTemp2,ZPTemp3,ZPTemp5,ZPTemp6
	lda #0
	sta ZPTemp5
	lda #16
	sta ZPTemp6
	bne doRowPlotsInternal
.comment
	lda #>(kVectors.Bitmap+(kOffsets.y*320)+((kOffsets.x-1)*8))
	sta Pointer1+1 
	lda #<(kVectors.Bitmap+(kOffsets.y*320)+((kOffsets.x-1)*8))
	sta Pointer1
	ldx #0
	stx ZPTemp4
_loop
	jsr copyPointer1To3LdxZPTemp4
	jsr countRow ;&&preserve ZPTemp4
	#ADCIW Pointer1,320	
	#ADCB ZPTemp4,#16
	bne _loop	
	
	lda #>(kVectors.Bitmap+((kOffsets.y-1)*320)+((kOffsets.x)*8))
	sta Pointer1+1 
	lda #<(kVectors.Bitmap+((kOffsets.y-1)*320)+((kOffsets.x)*8))
	sta Pointer1
	ldx #0
	stx ZPTemp4
_loop2
	jsr copyPointer1To3LdxZPTemp4
	jsr countColumn ;&&preserve ZPTemp4
	#ADCIW Pointer1,8	
	inc ZPTemp4
	lda ZPTemp4
	cmp #16
	bne _loop2	
	rts
.endc	
	
doRowPlots8
;&&trashes a,x,y,Pointer1,Pointer1+1,ZPTemp4,Pointer3,Pointer3+1
;&&trashes ZPTemp1,ZPTemp2,ZPTemp3,ZPTemp5,ZPTemp6
	lda #128
	sta ZPTemp5
	lda #8
	sta ZPTemp6
	;fall through
.comment
	lda #>(kVectors.Bitmap+(kOffsets.y*320)+((kOffsets.x-1)*8))
	sta Pointer1+1 
	lda #<(kVectors.Bitmap+(kOffsets.y*320)+((kOffsets.x-1)*8))
	sta Pointer1
	ldx #0
	stx ZPTemp4
_loop
	jsr copyPointer1To3LdxZPTemp4
	jsr countRow ;&&preserve ZPTemp4
	#ADCIW Pointer1,320	
	#ADCB ZPTemp4,#16
	cmp #128
	bne _loop	
	
	lda #>(kVectors.Bitmap+((kOffsets.y-1)*320)+((kOffsets.x)*8))
	sta Pointer1+1 
	lda #<(kVectors.Bitmap+((kOffsets.y-1)*320)+((kOffsets.x)*8))
	sta Pointer1
	ldx #0
	stx ZPTemp4
_loop2
	jsr copyPointer1To3LdxZPTemp4
	jsr countColumn ;&&preserve ZPTemp4
	#ADCIW Pointer1,8	
	inc ZPTemp4
	lda ZPTemp4
	cmp #8
	bne _loop2	
	rts
.endc

doRowPlotsInternal
;&&trashes a,x,y,Pointer1,Pointer1+1,ZPTemp4,Pointer3,Pointer3+1
;&&trashes ZPTemp1,ZPTemp2,ZPTemp3
	lda #>(kVectors.Bitmap+(kOffsets.y*320)+((kOffsets.x-1)*8))
	sta Pointer1+1 
	lda #<(kVectors.Bitmap+(kOffsets.y*320)+((kOffsets.x-1)*8))
	sta Pointer1
	ldx #0
	stx ZPTemp4
_loop
	jsr copyPointer1To3LdxZPTemp4 ;&&preserve ZPTemp5,ZPTemp6
	jsr countRow ;&&preserve ZPTemp4,ZPTemp5,ZPTemp6
	#ADCIW Pointer1,320	
	#ADCB ZPTemp4,#16
	cmp ZPTemp5
	bne _loop	
	
	lda #>(kVectors.Bitmap+((kOffsets.y-1)*320)+((kOffsets.x)*8))
	sta Pointer1+1 
	lda #<(kVectors.Bitmap+((kOffsets.y-1)*320)+((kOffsets.x)*8))
	sta Pointer1
	ldx #0
	stx ZPTemp4
_loop2
	jsr copyPointer1To3LdxZPTemp4 ;&&preserve ZPTemp5,ZPTemp6
	jsr countColumn ;&&preserve ZPTemp4,ZPTemp5,ZPTemp6
	#ADCIW Pointer1,8	
	inc ZPTemp4
	lda ZPTemp4
	cmp ZPTemp6
	bne _loop2	
	rts
	
copyPointer1To3LdxZPTemp4	
;&&trashes a	
;&&modifies Pointer1,Pointer1+1,Pointer3,Pointer3+1,x	
	lda Pointer1
	sta Pointer3
	lda Pointer1+1
	sta Pointer3+1
	ldx ZPTemp4
	rts
	
countRow
;&&trashes a,x,y,ZPTemp1,ZPTemp2,Pointer3,Pointer3+1
;&&modifies Pointer3
	ldy #15
	lda #$FF
	pha
_getType
	lda #0	
	sta ZPTemp1	
	sta ZPTemp2
	lda TargetMap,x
	lsr
	rol ZPTemp1
	inx
	dey
	bmi _change
_whileSame	
	lda TargetMap,x	
	cmp ZPTemp1	
	bne _change	
	inc ZPTemp2	
	inx	
	dey	
	bpl _whileSame	
_change	
	lda ZPTemp1
	beq _was0
	lda ZPTemp2
	clc
	adc #1
	pha ; store that we had X 1's on the stack
_was0
	cpy #$FF
	bne _getType ; next 
	; now we have the a list of things on the stack backwards
	; which we now pop off and render
	pla	
	bmi _last	
_printDigits	
	jsr drawFullDigitToPointer3
;	jsr sub8FromP3
	jsr sub8FromP3
	pla
	bpl _printDigits
	rts
_last
	lda #0
	jmp drawFullDigitToPointer3
	;rts
	
countColumn
;&&trashes a,x,y,ZPTemp1,ZPTemp2,ZPTemp3,ZPTemp4,Pointer3,Pointer3+1
	ldy #15
	lda #$FF
	pha
_getType
	lda #0	
	sta ZPTemp1	
	sta ZPTemp2
	lda TargetMap,x
	lsr
	rol ZPTemp1
	txa
	clc
	adc #16
	tax
	dey
	bmi _change
_whileSame	
	lda TargetMap,x	
	cmp ZPTemp1	
	bne _change	
	inc ZPTemp2	
	txa
	clc
	adc #16
	tax
	dey	
	bpl _whileSame	
_change	
	lda ZPTemp1
	beq _was0
	lda ZPTemp2
	clc
	adc #1
	pha ; store that we had X 1's on the stack
_was0
	cpy #$FF
	bne _getType ; next 
	; now we have the a list of things on the stack backwards
	; which we now pop off and render
	pla	
	bmi _last	
_printDigits	
	jsr drawFullDigitToPointer3
	jsr sub320FromP3
	pla
	bpl _printDigits
	rts
_last
	lda #0
	jmp drawFullDigitToPointer3
	;rts
	
plotCompletePuzzle	
;&&trashes a,x,y,ZPTemp1,ZPTemp2,ZPTemp3,ZPTemp4,ZPTemp5,Pointer3,Pointer3+1
;&&trashes Pointer1,Pointer1+1,ZPTemp6,ZPTemp7,ZPTemp8,ZPTemp9
;&&modifies convert2PuzzleRowsTo4_4._8x8OverStep
	; add 4 to pointer 3
	; do lower half
	; add 316 to pointer 3
	; repeat 4 times
	#STAB #7, ZPTemp1		
	lda MapSize		
	asl a		
	asl a		
	asl a
	tax		
	lda Size4_4LUT,x		
	sta Pointer3		
	lda Size4_4LUT+1,x		
	sta Pointer3+1		
	lda Size4_4LUT+2,x		
	sta ZPTemp6		
	lda Size4_4LUT+3,x			
	sta ZPTemp7			
	lda Size4_4LUT+4,x			
	sta ZPTemp8		
	lda Size4_4LUT+5,x			
	sta ZPTemp9			
	lda Size4_4LUT+6,x		
	sta convert2PuzzleRowsTo4_4._8x8OverStep		
;	#STAPW kVectors.Bitmap+(13*320)+(28*8),Pointer3	
	ldx #0
_loop
	jsr convert2PuzzleRowsTo4_4		
	#ADCBW Pointer3,#4
	jsr convert2PuzzleRowsTo4_4
	#ADCIW Pointer3,320-4
	dec ZPTemp1
	bpl _loop
	; clear grid
	lda #0
	sta $D015 ; turn off sprites
	jsr plotGridColours
	lda MapSize
	asl a
	asl a
	tax
	lda Size4_4ColLUT+1,x
	sta ZPTemp2
	lda Size4_4ColLUT+2,x
	sta ZPTemp3
	lda Size4_4ColLUT+3,x
	sta ZPTemp4
	lda Size4_4ColLUT,x
	tax
-	ldy ZPTemp4
	lda screenRowLUTLO,x
	clc
	adc ZPTemp2
	sta Pointer1
	lda screenRowLUTHi,x
	adc #0
	sta Pointer1+1
	lda #$01
	jsr pltScreenRow
	inx
	cpx ZPTemp3
	bcc -	
	rts

Size4_4LUT		
	; start address lo, hi, rows, words		
;8x8			
.word kVectors.Bitmap+(11*320)+(26*8)				
.byte 7,0,27,3,$e8,0			
;16x16			
.word kVectors.Bitmap+(13*320)+(28*8)				
.byte 15,1,59,7,$ea			
Size4_4ColLUT			
;start y, start x,end y,width		
.byte 9+2,24+2,9+6,3		
.byte 9+4,24+4,9+12,7			
			
convert2PuzzleRowsTo4_4
;&&trashes a,x,y,ZPTemp1,ZPTemp2,ZPTemp3,ZPTemp4,ZPTemp5,Pointer3
	#STAB ZPTemp6,ZPTemp4
_4lineLoop
	#STAB ZPTemp7,ZPTemp3
_wordLoop
	ldy #3
	lda PuzzleBuffer,x
	sta ZPTemp5
_byteLoop
	#STAB #0,ZPTemp2
	asl ZPTemp5
	bcc _upperEmpty
	#ORI ZPTemp2,$F0
_upperEmpty
	asl ZPTemp5
	bcc _lowerEmpty
	#ORI ZPTemp2,$0F
_lowerEmpty
	lda ZPTemp2
	pha
	dey
	bpl _byteLoop
	inx	
	dec ZPTemp3	
	bpl _wordLoop	
; plot them	
_8x8OverStep	
	inx
	ldy ZPTemp8	
	#STAB ZPTemp9,ZPTemp2	
_char	
	#STAB #3,ZPTemp3	
	;get value
	pla
	;write 4 times
_byte
	sta (Pointer3),y
	dey
	dec ZPTemp3
	bpl _byte
	;add 4 to offset
	dey
	dey
	dey 
	dey
	; next value x 8
	dec ZPTemp2
	bpl _char

	rts	

drawFailScreen		
;&&trashes a,x,y,ZPTemp1,ZPTemp2,ZPTemp3,Pointer1,Pointer1+1,Pointer2,Pointer2+1
;&&trashes Pointer3,Pointer3+1
	lda #$2
	sta $d020
	sta $d021		
	jsr setTextScreen		
	jsr clearScreenRAMScreen		
	ldx #5 ;string -FAIL-
	stx ZPTemp3
	lda #0
	sta ZPTemp1
	sta ZPTemp2
	sta $D015 ; turn off sprites
-	lda ZPTemp1
	ldy ZPTemp2
	ldx ZPTemp3
	jsr plotStringAAtIndexX ;&&preserve ZPTemp1,ZPTemp2,ZPTemp3
	lda ZPTemp1
	clc
	adc #7
	cmp #40
	bcc +
	sec
	sbc #40
	pha
	inc ZPTemp2
	lda ZPTemp2
	cmp #25
	beq _exit
	pla
+	sta ZPTemp1
	jmp -
_exit
	pla
	rts
	
			
drawFullDigitToPointer3	
;&&trashes a,x,y,ZPTemp1
	asl a
	sta ZPTemp1
	asl a
	clc
	adc ZPTemp1
	tax
	ldy #1
-	lda Font,x 
	ror a
	sta (Pointer3),y
	inx
	iny
	cpy #7
	bne -		
	rts	

sub8FromP3		
;&&trashes a
;&&modifies Pointer3,Pointer3+1
	lda Pointer3		
	sec		
	sbc #8		
	sta Pointer3		
	bcs +		
	dec Pointer3+1		
	;sbc #0		
	;sta Pointer3+1		
+	rts		
	
sub320FromP3		
;&&trashes a
;&&modifies Pointer3,Pointer3+1
	lda Pointer3		
	sec		
	sbc #<320	
	sta Pointer3		
	lda Pointer3+1		
	sbc #>320	
	sta Pointer3+1		
	rts	

Plot2XChar
;&&trashes a,x,y,ZPTemp1,ZPTemp2,CharacterBuffer,CharacterBuffer+8,Pointer2,Pointer2+1,Pointer3,Pointer3+1
;&&modifies 
	;place2XCharIntoBuffer		
	; convert to address		
	asl a	
	asl a	
	asl a	
	sta ZPTemp1
	tax		
	sei		
	;lda $1	
	;and #251	
	lda #$33
	sta $1	
	ldy #0
	sty ZPTemp2
_loop
	ldx ZPTemp1
	lda $D800+(48*8),x
	ldy #3
	ldx ZPTemp2
_firstHalf	
	asl a
	php
	rol CharacterBuffer,x
	plp
	rol CharacterBuffer,x
	dey
	bpl _firstHalf
	ldy #3
_secondHalf	
	asl a
	php
	rol CharacterBuffer+8,x
	plp
	rol CharacterBuffer+8,x
	dey
	bpl _secondHalf
	inc ZPTemp1			
	inc ZPTemp2			
	ldy ZPTemp2			
	cpy #8			
	bne _loop			
	;lda $1	
	;ora #4	
	lda #$37
	sta $1				
	cli				
; plot			
	ldy #0		
	ldx #0			

_nextLine		
	lda CharacterBuffer,x			
	sta (Pointer2),y			
	lda CharacterBuffer+8,x			
	sta (Pointer3),y			
	iny			
	lda CharacterBuffer,x			
	sta (Pointer2),y			
	lda CharacterBuffer+8,x			
	sta (Pointer3),y			
	iny			
	inx			
	cpx #4			
	beq _nextLineBitmap			
	cpx #8	
	bne _nextLine	
	rts	
_nextLineBitmap
	#ADCIW Pointer2,320
	#ADCIW Pointer3,320
	ldy #0
	geq _nextLine			

setTimerTo30Min
;&&trashes a
;&&modifies TimerShadow,TimerShadow+1
	#STAB #$30,TimerShadow
	#STAB #$00,TimerShadow+1
	rts
	
plotTimer				
;&&trashes a,x,y,Pointer2,Pointer2+1,Pointer3,Pointer3+1			
;&&trashes a,x,y,ZPTemp1,ZPTemp2,CharacterBuffer,CharacterBuffer+8,Pointer2,Pointer2+1,Pointer3,Pointer3+1
	#STAPW kVectors.Bitmap+(6*320)+(7*8),Pointer2
	#STAPW kVectors.Bitmap+(6*320)+(8*8),Pointer3
	lda TimerShadow				
	lsr a				
	lsr a				
	lsr a				
	lsr a				
	jsr Plot2XChar				
	#STAPW kVectors.Bitmap+(6*320)+(9*8),Pointer2
	#STAPW kVectors.Bitmap+(6*320)+(10*8),Pointer3
	lda TimerShadow				
	and #$0f			
	jsr Plot2XChar			
	#STAPW kVectors.Bitmap+(6*320)+(11*8),Pointer2
	#STAPW kVectors.Bitmap+(6*320)+(12*8),Pointer3		
	lda #10	; :
	jsr Plot2XChar		
	#STAPW kVectors.Bitmap+(6*320)+(13*8),Pointer2
	#STAPW kVectors.Bitmap+(6*320)+(14*8),Pointer3
	lda TimerShadow+1				
	lsr a				
	lsr a				
	lsr a				
	lsr a				
	jsr Plot2XChar				
	#STAPW kVectors.Bitmap+(6*320)+(15*8),Pointer2
	#STAPW kVectors.Bitmap+(6*320)+(16*8),Pointer3
	lda TimerShadow+1				
	and #$0f			
	jmp Plot2XChar				
	;rts		

plotPICROSSBitmapLogo
	#STAPW kVectors.Bitmap+(8*320),Pointer3
	; for 9 rows
	lda #8
	sta ZPTemp1
	lda #0
	sta ZPtemp4
_rowLoop
	; for 3 bytes
	lda #2
	sta ZPTemp2
	ldy #0
_byteLoop
	lda #7
	sta ZPTemp3
	ldx ZPTemp4
	lda PicrossLogo,x
_bitLoop
	; get bit
	asl a
	pha
	bcc _empty
	; if set, plot char
	ldx #7
_charLoop
	lda PicrossLogoChar,x
	sta (Pointer3),y
	iny
	dex
	bpl _charLoop
	bmi _next
_empty
	 ; advance char
	tya
	clc
	adc #8
	tay
_next
	pla
	dec ZPTemp3
	bpl _bitLoop	
	inc ZPTemp4	
	dec ZPTemp2
	bpl _byteLoop
	; sub 320 to pointer
	jsr sub320FromP3				
	dec ZPTemp1			
	bpl _rowLoop			
	; do the border		
	lda #$35
	sta $01
	#STAPW kVectors.Bitmap+(5*320)+(17*8)+3,Pointer3				
	jsr plotHozLineLogo								
	#STAPW kVectors.Bitmap+(8*320)+(17*8)+3,Pointer3				
	jsr plotHozLineLogo								
;	#STAPW kVectors.Bitmap+(5*320)+(6*8),Pointer3				
;	jsr plotVertLineLogo			
;	#STAPW kVectors.Bitmap+(6*320)+(6*8),Pointer3				
;	jsr plotVertLineLogo								
;	#STAPW kVectors.Bitmap+(7*320)+(6*8),Pointer3				
;	jsr plotVertLineLogo									
;	#STAPW kVectors.Bitmap+(8*320)+(6*8),Pointer3				
;	jsr plotVertLineLogo										
;	#STAPW kVectors.Bitmap+(5*320)+(17*8),Pointer3				
;	jsr plotVertLineLogo								
;	#STAPW kVectors.Bitmap+(6*320)+(17*8),Pointer3				
;	jsr plotVertLineLogo								
;	#STAPW kVectors.Bitmap+(7*320)+(17*8),Pointer3				
;	jsr plotVertLineLogo									
;	#STAPW kVectors.Bitmap+(8*320)+(17*8),Pointer3				
;	jsr plotVertLineLogo									
	ldx #7
-	lda VertBitmapLocationsLo,x
	sta Pointer3
	lda VertBitmapLocationsHi,x
	sta Pointer3+1
	jsr plotVertLineLogo
	dex
	bpl -
	lda #$37								
	sta $01								
	rts							

VertBitmapLocationsLo								
	.byte <kVectors.Bitmap+(5*320)+( 6*8),<kVectors.Bitmap+(6*320)+( 6*8),<kVectors.Bitmap+(7*320)+( 6*8),<kVectors.Bitmap+(8*320)+( 6*8)								
	.byte <kVectors.Bitmap+(5*320)+(17*8),<kVectors.Bitmap+(6*320)+(17*8),<kVectors.Bitmap+(7*320)+(17*8),<kVectors.Bitmap+(8*320)+(17*8)								
VertBitmapLocationsHi								
	.byte >kVectors.Bitmap+(5*320)+( 6*8),>kVectors.Bitmap+(6*320)+( 6*8),>kVectors.Bitmap+(7*320)+( 6*8),>kVectors.Bitmap+(8*320)+( 6*8)								
	.byte >kVectors.Bitmap+(5*320)+(17*8),>kVectors.Bitmap+(6*320)+(17*8),>kVectors.Bitmap+(7*320)+(17*8),>kVectors.Bitmap+(8*320)+(17*8)								

plotHozLineLogo			
;&&trashes a,x,y	
;&&modifies Pointer3,Pointer3+1	
	ldx #11
_topRowLoop
	ldy #0	
	lda #255
	sta (Pointer3),y	
	iny	
	sta (Pointer3),y	
	jsr sub8FromP3	
	dex			
	bpl _topRowLoop			
	rts			
		
plotVertLineLogo		
;&&trashes a,y		
	ldy #7	 				
-	lda (Pointer3),y				
	ora #%00011000				
	sta (Pointer3),y				
	dey				
	bpl -				
	rts				
					
setUpNMITOD			
;&&trashes a			
	#STAPW TODNMI,$318 ; set NMI register
	#STAB #$80,$DD0F			
	#STAB #$01,$DD09
	jmp NMIInternal
setUp5SecondTimer
;&&trashes a			
	#STAPW TODNMIGotToMenu,$318 ; set NMI register
	#STAB #$80,$DD0F			
	#STAB #$05,$DD09	
NMIInternal	
;&&trashes a
	lda #0			
	sta $DD0B 		
	sta $DD0A
	sta $DD08
	sta $DD0F
	; clear current time
	sta $DD0B
	sta $DD0A
	sta $DD09
	sta $DD08
	#STAB #$84,$DD0D ; enable timer alarm
	lda $DD0D
	rts

TODNMI
;&&trashes a,x,y,Pointer2,Pointer2+1,Pointer3,Pointer3+1,ZPTemp1,ZPTemp2,ZPTemp3			
;&&modifies TimerShadow,TimerShadow+1,CurrFuncPointer,ForceExitKeyboardLoop,DoneLowTime
	pha
	txa
	pha
	tya
	pha
	ldx #11
-	lda Pointer1,x
	pha
	dex
	bpl -
	sed
	lda TimerShadow+1
	sec
	sbc #1
	sta TimerShadow+1
	cmp #$99
	bne _noFail
	lda #$59
	sta TimerShadow+1
	lda TimerShadow
	sec
	sbc #1
	sta TimerShadow
	cld
	cmp #$99
	bne _noFail
	inc ForceExitKeyboardLoop
	ldy #kMusic.fail
	jsr SFX_s0C84
	jsr drawFailScreen
	jsr setUp5SecondTimer
	#STAPW EmptyFunction,CurrFuncPointer
	jmp _ExitNMI
_noFail	
	cld
	lda TimerShadow
	cmp #5 ; < 5 mins left
	bcs _notLowTime
	lda DoneLowTime
	bne _notLowTime
	ldy #kMusic.low_time
	jsr SFX_s0C84
	inc DoneLowTime
_notLowTime
	jsr plotTimer
	jsr setUpNMITOD	
_ExitNMI	
	ldx #0
-	pla
	sta Pointer1,x
	inx
	cpx #12
	bne - 	
	pla
	tay
	pla
	tax
	pla
	rti

TODNMIGotToMenu
;&&trashes a
;&&modifies CurrFuncPointer
	pha
	#STAPW TitleScreen,CurrFuncPointer
	#STAB #$04,$DD0D ; enable timer alarm
	lda $DD0D
	pla
	rti
	
; a = x pos	
; y = y pos	
; x = string	
plotStringAAtIndexX			
;&&trashes a,x,y,Pointer1,Pointer1+1,Pointer2,Pointer2+1,Pointer3,Pointer3+1		
	clc		
	adc screenRowLUTLO,y		
	sta Pointer2		
	sta Pointer3		
	lda screenRowLUTHi,y
	adc #0
	sta Pointer2+1
	eor # (>kVectors.Screen) ^ $d8
	sta Pointer3+1
	lda StringTableLUT.lo,x			
	sta Pointer1			
	lda StringTableLUT.hi,x			
	sta Pointer1+1			
	ldy #0			
_l	lda (Pointer1),y			
	beq _done		
	sta (Pointer2),y			
	lda #1			
	sta (Pointer3),y			
_next			
	iny			
	bne _l			
_done			
	rts				

MenuKeyFuncTBL
.byte <moveCursorToPrevItem,>moveCursorToPrevItem,<moveCursorToNextItem,>moveCursorToNextItem
.byte <moveCursorToPrevItem,>moveCursorToPrevItem,<moveCursorToNextItem,>moveCursorToNextItem
.byte <didMenuPress,>didMenuPress,<didMenuPress,>didMenuPress

plotAndSetupMenuInMenuPointer
;&&trashes a,x,y,Pointer1,Pointer1+1,Pointer2,Pointer2+1,Pointer3,Pointer3+1
;&&trashes ZPTemp1
;&&modifies MenuItem,CursorX,CursorY,UpFunc
	ldx #11
-	lda MenuKeyFuncTBL,x
	sta UpFunc,x
	dex
	bpl - 
	ldy #0
	sty MenuItem
_itemItr
	sty ZPTemp1
	; is x negative
	lda (MenuPointer),y
	bmi _exit	; exit	
	; no, get x,y,string and plot
	sta CursorX
	iny
	lda (MenuPointer),y
	sta CursorY
	iny
	lda (MenuPointer),y
	tax
	ldy CursorY
	lda CursorX
	jsr plotStringAAtIndexX
	lda ZPTemp1
	clc
	adc #7
	tay
	gne _itemItr
_exit 
	ldy #0	
	lda (MenuPointer),y	
	sta CursorX	
	iny
	lda (MenuPointer),y	
	sta CursorY	 
	jsr markMenuItem	
	jmp setKeyboardToMenuMode	

mulAby7
;&&trashes ZPTemp8
;&&modifies a
	sta ZPTemp8
	asl a ; x2
	asl a ; x4
	asl a ; x8
	sec
	sbc ZPTemp8 ; - x 
	rts
	
markMenuItem
;&&trashes a,x,y,Pointer1,Pointer1+1,Pointer2,Pointer2+1,Pointer3,Pointer3+1
	lda CursorX
	sec
	sbc #2
	ldy CursorY
	ldx #3
	jmp plotStringAAtIndexX

clearMenuItem
;&&trashes a,x,y,Pointer1,Pointer1+1,Pointer2,Pointer2+1,Pointer3,Pointer3+1
	lda CursorX
	sec
	sbc #2
	ldy CursorY
	ldx #4
	jmp plotStringAAtIndexX
	
moveCursorToNextItem	
;&&trashes a,x,y,ZPTemp8	
;&&modifies moveCursorToItemInernal._mCTIINI,MenuItem,CursorX,CursorY	
	lda #$EA ; NOP
	sta moveCursorToItemInernal._mCTIINI
	jmp moveCursorToItemInernal
	
moveCursorToPrevItem	
;&&trashes a,x,y,ZPTemp8	
;&&modifies moveCursorToItemInernal._mCTIINI,MenuItem,CursorX,CursorY	
	lda #$C8 ; INY
	sta moveCursorToItemInernal._mCTIINI
		
moveCursorToItemInernal
;&&trashes a,x,y,ZPTemp8
;&&modifies MenuItem,CursorX,CursorY
	jsr clearMenuItem	
	lda MenuItem	
	jsr mulAby7	
	tay
	iny
	iny
	iny
_mCTIINI
	nop
	lda (MenuPointer),y	
	sta MenuItem	
	jsr mulAby7	
	tay	
	lda (MenuPointer),y	
	sta CursorX	
	iny	
	lda (MenuPointer),y	
	sta CursorY	
	jmp markMenuItem	

didMenuPress 	
;&&trashes a,y,ZPTemp8
;&&modifies _jumpPtr,_jumpPtr+1
	ldy #kMusic.mark
	jsr SFX_s0C84 ; start sfx
	lda MenuItem	
	jsr mulAby7	
	clc	
	adc #5	
	tay	
	lda (MenuPointer),y	
	sta _jumpPtr	
	iny	
	lda (MenuPointer),y	
	sta _jumpPtr+1	
_jumpPtr = *+1	
	jmp $0000	
	
didSelectEasy 	
;&&trashes a,x,y,ZPTemp1,MenuPointer
	jsr setTextScreen
	#STAPW EasyMenu,MenuPointer
	jmp plotAndSetupMenuInMenuPointer

didSelectEasyGame
;&&trashes a,x,ZPTemp1,ZPTemp2,ZPTemp3,ZPTemp4
;&&modifies PuzzlePointer,PuzzlePointer+1,CurrFuncPointer,MapSize
	ldx #kMapSize.size8
	stx MapSize
	lda #<Puzzle8_1
	sta ZPTemp3
	lda #>Puzzle8_1
	sta ZPTemp4
	ldx #2 ; 3x aka x8
	jmp mulMapItem

didSelectHardGame
;&&trashes a,x,ZPTemp1,ZPTemp2,ZPTemp3,ZPTemp4
;&&modifies PuzzlePointer,PuzzlePointer+1,CurrFuncPointer,MapSize
	ldx #kMapSize.size16
	stx MapSize
	lda #<Puzzle1
	sta ZPTemp3
	lda #>Puzzle1
	sta ZPTemp4
	ldx #4 ; 5x aka x32
	jmp mulMapItem		

mulMapItem	
;&&trashes a,x,ZPTemp1,ZPTemp2
;&&modifies PuzzlePointer,PuzzlePointer+1,CurrFuncPointer
	lda MenuItem
	sta ZPTemp1
	lda #0
	sta ZPTemp2	
-	asl ZPTemp1
	rol ZPTemp2
	dex
	bpl -	
	clc
	lda ZPTemp3
	adc ZPTemp1
	sta PuzzlePointer
	lda ZPTemp4
	adc ZPTemp2
	sta PuzzlePointer+1
	jsr setTextScreen
	#STAPW setupPicrossScreen,CurrFuncPointer
	rts	
		
didSelectHard
;&&trashes a,x,y,ZPTemp1,MenuPointer
	jsr setTextScreen
	#STAPW HardMenu,MenuPointer
	jmp plotAndSetupMenuInMenuPointer		

;----sfx -----	

SFX_s0A70             
;&&trashes _DONT_CARE_      
				LDA f0C3A
                      BEQ _b0A99
                      DEC f0C3E
                      BNE _b0A90
                      LDA f0C34
                      STA f0C3E
                      LDA Freq_pulse_set_selec
                      EOR #$01
                      STA Freq_pulse_set_selec
_b0A90                JSR SFX_s0A9F
_b0A99                RTS 

SFX_s0A9F             LDA f0C35
                      BMI _b0AC9
                      SEC 
                      LDA f0C32
                      SBC #$01
                      STA f0C32
                      BCS _b0AC9
                      DEC f0C35
                      BPL _b0AC9
                      LDA #$00
                      STA f0C3A                      
                      STA $D404
                      STA $D405
                      STA $D406
                      RTS 

_b0AC9                LDY f0C29
                      TYA 
                      AND #$01
                      BEQ _b0AE0
                      SEC 
                      LDA Pulse_wave_wide_lo
                      SBC Pulse_wave_delta_lo
                      STA Pulse_wave_wide_lo
                      BCS _b0AE0
                      DEC Pulse_wave_wide_hi
_b0AE0                TYA 
                      AND #$08
                      BEQ _b0AF9
                      TYA 
                      AND #$10
                      BEQ _b0AF9
                      SEC 
                      LDA Pulse_wave_wide_lo_B
                      SBC Pulse_wave_delta_lo
                      STA Pulse_wave_wide_lo_B
                      BCS _b0AF9
                      DEC Pulse_wave_wide_hi_B
_b0AF9                LDY f0C33
                      TYA 
                      AND #$03
                      BEQ _b0B62
                      CMP #$01
                      BEQ _b0B22
                      SEC 
                      LDA Frequency_lo
                      SBC Frequency_Lo_delta
                      STA Frequency_lo
                      LDA Frequency_hi
                      SBC Frequency_Hi_delta
                      STA Frequency_hi
                      CMP Frequency_Hi_Start_A
                      BCS _b0B62
                      LDA Frequency_Hi_Start_B
                      BCC _b0B3D
_b0B22                CLC 
                      LDA Frequency_lo
                      ADC Frequency_Lo_delta
                      STA Frequency_lo
                      LDA Frequency_hi
                      ADC Frequency_Hi_delta
                      STA Frequency_hi
                      CMP Frequency_Hi_Start_B
                      BCC _b0B62
                      LDA Frequency_Hi_Start_A
_b0B3D                STA _a0B5E
                      TYA 
                      AND #$0C
                      BNE _b0B4B
                      TYA 
                      AND #$FC
                      JMP _j0B52

_b0B4B                CMP #$04
                      BEQ _b0B58
                      TYA 
                      EOR #$03
_j0B52                STA f0C33
                      JMP _b0B62

_b0B58                LDA #$00
                      STA Frequency_lo
_a0B5E   =*+$01
                      LDA #$36
                      STA Frequency_hi
_b0B62                LDY f0C33
                      LDA f0C29
                      AND #$08
                      BEQ _b0BDE
                      TYA 
                      AND #$30
                      BEQ _b0BD2
                      CMP #$10
                      BEQ _b0B92
                      SEC 
                      LDA Frequency_lo_B
                      SBC Frequency_Lo_B_delta
                      STA Frequency_lo_B
                      LDA Frequency_Hi_B
                      SBC Frequency_Hi_B_delta
                      STA Frequency_Hi_B
                      CMP f0C2A
                      BCS _b0BD2
                      LDA f0C2B
                      BCC _b0BAD
_b0B92                CLC 
                      LDA Frequency_lo_B
                      ADC Frequency_Lo_B_delta
                      STA Frequency_lo_B
                      LDA Frequency_Hi_B
                      ADC Frequency_Hi_B_delta
                      STA Frequency_Hi_B
                      CMP f0C2B
                      BCC _b0BD2
                      LDA f0C2A
_b0BAD                STA _selfMod_FreqHiB
                      TYA 
                      AND #$C0
                      BNE _b0BBB
                      TYA 
                      AND #$CF
                      JMP _j0BC2

_b0BBB                CMP #$40
                      BEQ _b0BC8
                      TYA 
                      EOR #$30
_j0BC2                STA f0C33
                      JMP _b0BD2

_b0BC8                LDA #$00
                      STA Frequency_lo_B
_selfMod_FreqHiB   =*+$01
                      LDA #$08
                      STA Frequency_Hi_B
_b0BD2                LDA f0C29
                      AND #$08
                      BEQ _b0BDE
                      LDA Freq_pulse_set_selec
                      BNE _b0C00
_b0BDE                LDA Frequency_lo
                      STA $D400
                      LDA Frequency_hi
                      STA $D401
                      LDA Pulse_wave_wide_lo
                      STA $D402
                      LDA Pulse_wave_wide_hi
                      STA $D403
                      LDA Voice_controll
                      STA $D404
		              RTS 

_b0C00                LDA Frequency_lo_B
                      STA $D400
                      LDA Frequency_Hi_B
                      STA $D401
                      LDA Pulse_wave_wide_lo_B
                      STA $D402
                      LDA Pulse_wave_wide_hi_B
                      STA $D403
                      LDA Voice_control_B
                      STA $D404
                      RTS 


;voiceNumToIndexLUT    .BYTE $00,$07,$0E
;voiceToInstDataLUT    .BYTE $00,$1D,$3A
;f0C7F                 .BYTE $13,$30,$4D,$FF,$FF
; start SFX
SFX_s0C84             
;&&trashes _DONT_CARE_
             		  LDX #$13
                      LDA fC144,Y
                      STA _a0C9D
                      LDA fC145,Y
                      STA _a0C9E
                      LDY #$13
_a0C9D   =*+$01
_a0C9E   =*+$02
_b0C9C                LDA $FFFF,Y
                      STA AD_Cycle_con,X
                      DEX 
                      DEY 
                      BPL _b0C9C
                      INX 
                      LDA #$08
                      STA $D404
                      LDA #$00
                      STA $D404
                      STA $D405
                      STA $D406
                      STA Frequency_lo
                      STA Frequency_lo_B
                      STA Freq_pulse_set_selec
                      STA Pulse_wave_wide_lo
                      STA Pulse_wave_wide_lo_B
                      LDA #$01
                      STA f0C3A
                      LDA AD_Cycle_con,X
                      STA $D405
                      LDA SR_Cycle_con,X
                      STA $D406
                      LDA f0C34
                      STA f0C3E
                      LDA #$01
                      STA f0C3A
                      LDA AD_Cycle_con
                      STA $D405
                      LDA SR_Cycle_con
                      STA $D406
                      LDY Frequency_Hi_Start_A
                      LDA f0C33
                      AND #$03
                      CMP #$02
                      BCC _b0D03
                      LDY Frequency_Hi_Start_B
_b0D03                TYA 
                      STA Frequency_hi
                      LDY f0C2A
                      LDA f0C33
                      AND #$30
                      CMP #$20
                      BCC _b0D16
                      LDY f0C2B
_b0D16                TYA 
                      STA Frequency_Hi_B
                      RTS 

;stop_sound            LDX #$17
;                      LDA #$00
;_b0D1F                STA $D400,X
;                      DEX 
;                      BNE _b0D1F
;                      RTS 

musicIRQ jsr SFX_s0A70	
		 lda #$FF
		 sta $d019	
		 lda #$7f
		 sta $DC0D 
		 jmp $ea81	

a0DA6                 .BYTE $06,$08,$16,$09,$D5,$6C,$07,$19
                      .BYTE $82,$DE,$D6,$2D,$0A,$06,$11,$81
                      .BYTE $0A,$AA,$01,$00
a0EE6                 .BYTE $0C,$CF,$21,$53,$5B,$03,$08,$01
                      .BYTE $0E,$62,$B3,$05,$01,$00,$41,$11
                      .BYTE $3B,$66,$01,$00
a0F36                 .BYTE $0E,$0F,$0A,$6A,$DC,$00,$09,$09
                      .BYTE $07,$93,$1C,$01,$01,$20,$11,$11
                      .BYTE $00,$AA,$03,$03
a0F9A                 .BYTE $39,$0A,$2F,$30,$3F,$01,$08,$19
                      .BYTE $0B,$2F,$3C,$00,$01,$FE,$21,$21
                      .BYTE $17,$22,$0C,$00
   
fC144                 .byte <a0DA6,<a0EE6,<a0F36,<a0F9A
fC145				  .byte >a0DA6,>a0EE6,>a0F36,>a0F9A

SetUpFileName
	LDA #1
    LDX #<fname
    LDY #>fname
SetUpFileName2
    JSR $FFBD     ; call SETNAM
    LDA #$01
    LDX $BA       ; last used device number
    BNE _skip
    LDX #$08      ; default to device 8
_skip   
	LDY #1     ; not $01 means: load to address stored in file
    JSR $FFBA     ; call SETLFS

    
    ;BCS .error    ; if carry set, a load error has happened
    RTS
   
LOAD
	jsr SetUpFileName
	LDA #$00      ; $00 means: load to memory (not verify)
    JMP $FFD5     ; call LOAD
.enc none
fnameReplace .text "@0:"
fname .text "s"    
SAVE    
	LDA #4
    LDX #<fnameReplace
    LDY #>fnameReplace
	jsr SetUpFileName2
	LDA #<$2000
    STA Pointer1
    LDA #>$2000
    STA Pointer1+1

    LDX #<stringEnd
    LDY #>stringEnd
    LDA #Pointer1      ; start address located in $C1/$C2
    JMP $FFD8     ; call SAVE    
    
StringTableLUT .mMakeTable strEasy,strHard,strInstructions,strMark,strClear,strFail,str1,str2,str3,str4,str5,str6,str7,str8,strInstructions2,strInstructions3,strInstructions4,strInstructions5,strTitle,strTitle2,strTitle3,strl1,strl2,strl3,strl4,strl5,strl6,strl7,strl8,str9,str10,strl9,strl10,strl11,strl12	

.enc screen			
strEasy .text "Easy",0		
strHard .text "Hard",0		
strInstructions  .text "WASD Move F5 Set F7 Mark invalid",0		
strMark .text ">",0	
strClear .text " ",0	
strFail .text "-FAIL-",0

;str9 .text " 9",0
;str10 .text "10",0
strInstructions2 .text "The numbers tell you how many squares",0 
strInstructions3 .text "are set in that row. Mark cells that",0	
strInstructions4 .text "can't be set invalid to help expose",0	
strInstructions5 .text "what can be set. Find the image",0	
strTitle .text "Picross 4K Craptastic",0	
strTitle2 .text "code,design oziphantom",0	
strTitle3 .text "SFX 'borrowed' from apex boyz",0	

TitleStrList
_x .byte 4,1,2,2,4,10,9,6
_y .byte 10,11,12,13,14,4,6,7
_string .byte 2,14,15,16,17,18,19,20
	

EasyHardMenu		
; x,y,str,next,prev,action ptr	
.byte 17,16,0,1,1,<didSelectEasy,>didSelectEasy		
.byte 17,18,1,0,0,<didSelectHard,>didSelectHard		
.byte $80		

EasyMenu	
.byte 8,2,6,1,9,<didSelectEasyGame,>didSelectEasyGame	
.byte 8,4,7,2,0,<didSelectEasyGame,>didSelectEasyGame			
.byte 8,6,8,3,1,<didSelectEasyGame,>didSelectEasyGame				
.byte 8,8,9,4,2,<didSelectEasyGame,>didSelectEasyGame					
.byte 8,10,10,5,3,<didSelectEasyGame,>didSelectEasyGame						
.byte 8,12,11,6,4,<didSelectEasyGame,>didSelectEasyGame							
.byte 8,14,12,7,5,<didSelectEasyGame,>didSelectEasyGame								
.byte 8,16,13,8,6,<didSelectEasyGame,>didSelectEasyGame									
.byte 8,18,29,9,7,<didSelectEasyGame,>didSelectEasyGame	
.byte 8,20,30,0,8,<didSelectEasyGame,>didSelectEasyGame	

.byte $80				

HardMenu	
.byte 8,2,21,1,11,<didSelectHardGame,>didSelectHardGame	
.byte 8,3,22,2,0,<didSelectHardGame,>didSelectHardGame			
.byte 8,4,23,3,1,<didSelectHardGame,>didSelectHardGame				
.byte 8,5,24,4,2,<didSelectHardGame,>didSelectHardGame					
.byte 8,6,25,5,3,<didSelectHardGame,>didSelectHardGame						
.byte 8,7,26,6,4,<didSelectHardGame,>didSelectHardGame							
.byte 8,8,27,7,5,<didSelectHardGame,>didSelectHardGame								
.byte 8,9,28,8,6,<didSelectHardGame,>didSelectHardGame									
.byte 8,10,31,9,7,<didSelectHardGame,>didSelectHardGame									
.byte 8,11,32,10,8,<didSelectHardGame,>didSelectHardGame
.byte 8,12,33,11,9,<didSelectHardGame,>didSelectHardGame										
.byte 8,13,34,0,10,<didSelectHardGame,>didSelectHardGame									
.byte $80					
				
TileColoursForMode	
.byte $0B,$01,$02	

MaxCursorLUT	
.byte 07,15	
	
screenRowLUTLO		
.for ue = kVectors.Screen, ue < kVectors.Screen + $400, ue = ue + 40
.byte <ue
.next
screenRowLUTHi	
.for ue = kVectors.Screen, ue < kVectors.Screen + $400, ue = ue + 40
.byte >ue
.next

Font
;0
.dfont '.##.....'
.dfont '#..#....'
.dfont '#..#....'
.dfont '#..#....'
.dfont '#..#....'
.dfont '.##.....'
;1
.dfont '..#.....'
.dfont '.##.....'
.dfont '..#.....'
.dfont '..#.....'
.dfont '..#.....'
.dfont '.###....'
;2
.dfont '.##.....'
.dfont '#..#....'
.dfont '...#....'
.dfont '..#.....'
.dfont '.#......'
.dfont '####....'
;3
.dfont '.##.....'
.dfont '#..#....'
.dfont '..#.....'
.dfont '..#.....'
.dfont '#..#....'
.dfont '.##.....'
;4
.dfont '#.#.....'
.dfont '#.#.....'
.dfont '#.#.....'
.dfont '####....'
.dfont '..#.....'
.dfont '..#.....'
;5
.dfont '####....'
.dfont '#.......'
.dfont '.##.....'
.dfont '...#....'
.dfont '...#....'
.dfont '###.....'
;6
.dfont '.##.....'
.dfont '#.......'
.dfont '###.....'
.dfont '#..#....'
.dfont '#..#....'
.dfont '.##.....'
;7
.dfont '####....'
.dfont '...#....'
.dfont '...#....'
.dfont '..#.....'
.dfont '..#.....'
.dfont '.#......'
;8
.dfont '.##.....'
.dfont '#..#....'
.dfont '.##.....'
.dfont '#..#....'
.dfont '#..#....'
.dfont '.##.....'
;9
.dfont '.###....'
.dfont '#..#....'
.dfont '.###....'
.dfont '...#....'
.dfont '...#....'
.dfont '.##.....'
;10
.dfont '.#..#...'
.dfont '.#.#.#..'
.dfont '.#.#.#..'
.dfont '.#.#.#..'
.dfont '.#.#.#..'
.dfont '.#..#...'
;11
.dfont '.#.#....'
.dfont '.#.#....'
.dfont '.#.#....'
.dfont '.#.#....'
.dfont '.#.#....'
.dfont '.#.#....'
;12
.dfont '.#.##...'
.dfont '.##..#..'
.dfont '.#...#..'
.dfont '.#..#...'
.dfont '.#.#....'
.dfont '.#.###..'
;13
.dfont '.#.##...'
.dfont '.#...#..'
.dfont '.#..#...'
.dfont '.#..#...'
.dfont '.#...#..'
.dfont '.#.##...'
;14
.dfont '.#.#.#..'
.dfont '.#.#.#..'
.dfont '.#.###..'
.dfont '.#...#..'
.dfont '.#...#..'
.dfont '.#...#..'
;15
.dfont '.#.###..'
.dfont '.#.#....'
.dfont '.#.###..'
.dfont '.#...#..'
.dfont '.#...#..'
.dfont '.#.###..'
;16
.dfont '.#..##..'
.dfont '.#.#....'
.dfont '.#.##...'
.dfont '.#.#.#..'
.dfont '.#.#.#..'
.dfont '.#..#...'
PicrossLogo
.dfont '#...#.............##.##.'
.dfont '#...#...............#..#'
.dfont '#...#...............#..#'
.dfont '#...#...............#..#'
.dfont '##..#..##.#.#..#...#..#.'
.dfont '#.#.#.#...#.#.#.#.#..#..'
.dfont '#.#.#.#...##..#.#.#..#..'
.dfont '#.#...#...#.#.#.#.#..#..'
.dfont '###.#..##.##...#...##.##'

PicrossLogoChar
.dfont '###.###.'
.dfont '###.###.'
.dfont '###.###.'
.dfont '........'
.dfont '###.###.'
.dfont '###.###.'
.dfont '###.###.'
.dfont '........'
Puzzle1 ; spikey
.dfont '................'
.dfont '........#.......'
.dfont '........#.......'
.dfont '.......#.#......'
.dfont '..#....#.#....#.'
.dfont '..##..#...#..##.'
.dfont '..#.#.#...#.#.#.'
.dfont '..#..#######..#.'
.dfont '..#.########..#.'
.dfont '.######...###.#.'
.dfont '#..####...####..'
.dfont '.##.###...#####.'
.dfont '#.##.#######...#'
.dfont '#####..###..###.'
.dfont '...####...####..'
.dfont '....#########...'
Puzzle2 ; race car
.dfont '..############..'
.dfont '.#..########..#.'
.dfont '..##.######.##..'
.dfont '..##.######.##..'
.dfont '..##.######.##..'
.dfont '....########....'
.dfont '....########....'
.dfont '...##########...'
.dfont '...####..####...'
.dfont '...####..####...'
.dfont '###.########.###'
.dfont '###.########.###'
.dfont '###.########.###'
.dfont '...#.#.##.#.#...'
.dfont '..#..........#..'
.dfont '..############..'
Puzzle3 ;frogger
.dfont '................'
.dfont '................'
.dfont '......####......'
.dfont '..#####..#####..'
.dfont '..#.##....##.#..'
.dfont '..###...##.###..'
.dfont '....#.##...#....'
.dfont '..############..'
.dfont '..#.########.#..'
.dfont '..##.#....#.##..'
.dfont '..############..'
.dfont '...##########...'
.dfont '...####..####...'
.dfont '................'
.dfont '................'
.dfont '................'
Puzzle4 ;vine monster
.dfont '................'
.dfont '................'
.dfont '................'
.dfont '....####........'
.dfont '....##..........'
.dfont '..##########....'
.dfont '..######..##....'
.dfont '..##..######....'
.dfont '..##..##........'
.dfont '..##..##........'
.dfont '..##..##........'
.dfont '......##........'
.dfont '....##..........'
.dfont '................'
.dfont '................'
.dfont '................'
Puzzle5 ; power up bug
.dfont '...........##...' 
.dfont '.####....##..##.' 
.dfont '.....##..##.....' 
.dfont '.....##..##.....' 
.dfont '...##......##...' 
.dfont '.......##.......' 
.dfont '.....##..##.....' 
.dfont '.##..........##.' 
.dfont '.####..##..####.' 
.dfont '.##############.' 
.dfont '.####..##..####.' 
.dfont '.####......####.' 
.dfont '...####..####...' 
.dfont '...##########...' 
.dfont '.....######.....' 
.dfont '................' 
Puzzle6 ; hexagon
.dfont '########........'
.dfont '########........'
.dfont '########........'
.dfont '########........'
.dfont '.#######.......#'
.dfont '...######....###'
.dfont '....###..#######'
.dfont '.....#....######'
.dfont '.....#....######'
.dfont '.....#....######'
.dfont '....###..#...###'
.dfont '...######......#'
.dfont '########........'
.dfont '########........'
.dfont '########........'
.dfont '########........'
Puzzle8 ; boulderdash
.dfont '................'
.dfont '....##....##....'
.dfont '....########....'
.dfont '..############..'
.dfont '..##..####..##..'
.dfont '....########....'
.dfont '......####......'
.dfont '....########....'
.dfont '..##..####..##..'
.dfont '....########....'
.dfont '......####......'
.dfont '......####......'
.dfont '....########....'
.dfont '....##....##....'
.dfont '..####....##....'
.dfont '..........####..'
Puzzle9 ; qwake
.dfont '....##....##....'
.dfont '...##......##...'
.dfont '..##........##..'
.dfont '..##........##..'
.dfont '.##..........##.'
.dfont '.##....##....##.'
.dfont '.##....##....##.'
.dfont '.###...##...###.'
.dfont '..####.##.####..'
.dfont '....########....'
.dfont '......####......'
.dfont '......####......'
.dfont '......####......'
.dfont '......####......'
.dfont '......####......'
.dfont '.......##.......'
Puzzle10 ; willy
.dfont '......####......'
.dfont '......####......'
.dfont '.....######.....'
.dfont '......##.#......'
.dfont '......#####.....'
.dfont '......####......'
.dfont '.......##.......'
.dfont '......####......'
.dfont '.....######.....'
.dfont '....########....'
.dfont '...##########...'
.dfont '...##.####.##...'
.dfont '......#####.....'
.dfont '.....###.##.#...'
.dfont '....##....###...'
.dfont '....###...##....'
Puzzle11 ; dig dug
.dfont '................'
.dfont '.....###........'
.dfont '...#####........'
.dfont '..########......'
.dfont '.###########....'
.dfont '.#######.#.#....'
.dfont '.#######.#.#....'
.dfont '.##########.....'
.dfont '...#######..#...'
.dfont '..#########.##..'
.dfont '....####...####.'
.dfont '...########.##..'
.dfont '..########..#...'
.dfont '..##...##.......'
.dfont '..####.####.....'
.dfont '................'
Puzzle12 ; galaxian
.dfont '........#.......'
.dfont '........#.......'
.dfont '.......###......'
.dfont '......#####.....'
.dfont '.....#######....'
.dfont '.....#..#..#....'
.dfont '...#...###...#..'
.dfont '...#...###...#..'
.dfont '..###.#####.###.'
.dfont '..#############.'
.dfont '..#############.'
.dfont '..####.###.####.'
.dfont '..###..#.#..###.'
.dfont '..###..#.#..###.'
.dfont '..###.......###.'
.dfont '...#.........#..'
Puzzle13 ; Pitfall Harry
.dfont '................'
.dfont '......###.......'
.dfont '......###.......'
.dfont '......###.......'
.dfont '......#.........'
.dfont '......###..#....'
.dfont '.....#######....'
.dfont '....######......'
.dfont '....#.###.......'
.dfont '....#.###.......'
.dfont '......####......'
.dfont '......#####.....'
.dfont '...##.###.#.....'
.dfont '....###...#.....'
.dfont '.....##...##....'
.dfont '................'

Puzzle8_1 ; l
.dfont '.##.....'
.dfont '.##.....'
.dfont '.##.....'
.dfont '.##.....'
.dfont '.##.....'
.dfont '.##.....'
.dfont '.#######'
.dfont '........'
Puzzle8_2 ; a 
.dfont '...##...'
.dfont '..####..'
.dfont '.##..##.'
.dfont '.######.'
.dfont '.##..##.'
.dfont '.##..##.'
.dfont '.##..##.'
.dfont '........'
Puzzle8_3 ; boat
.dfont '...##...'
.dfont '...#.#..'
.dfont '...#..#.'
.dfont '...#####'
.dfont '...#....'
.dfont '########'
.dfont '.#.#.#.#'
.dfont '..#####.'
Puzzle8_4 ; umbrella
.dfont '...##...'
.dfont '..####..'
.dfont '.######.'
.dfont '########'
.dfont '#..##..#'
.dfont '...##...'
.dfont '...##.##'
.dfont '....###.'
Puzzle8_5 ; C=
.dfont '..###...'
.dfont '.##..#..'
.dfont '##....##'
.dfont '##......'
.dfont '##....##'
.dfont '.##..#..'
.dfont '..###...'
.dfont '........'
Puzzle8_6 ; darlek
.dfont '...##...'
.dfont '..#..###'
.dfont '..####..'
.dfont '..#..##.'
.dfont '..#..#..'
.dfont '.##.#.#.'
.dfont '.#.#.##.'
.dfont '########'
Puzzle8_7 ; ghost
.dfont '#.....##'
.dfont '.......#'
.dfont '.#.#...#'
.dfont '.....#..'
.dfont '.###..#.'
.dfont '.....#..'
.dfont '#......#'
.dfont '##...###'
Puzzle8_8 ; happy sun
.dfont '##....##'
.dfont '#......#'
.dfont '########'
.dfont '.##..##.'
.dfont '........'
.dfont '.#....#.'
.dfont '#.####.#'
.dfont '##....##'
Puzzle8_9 ;heart
.dfont '.##..##.'
.dfont '########'
.dfont '##.#####'
.dfont '.#.####.'
.dfont '.##.###.'
.dfont '..####..'
.dfont '...##...'
.dfont '........'
Puzzle8_10 ; dropzone
.dfont '..####..'
.dfont '########'
.dfont '####..##'
.dfont '####..##'
.dfont '########'
.dfont '..####..'
.dfont '########'
.dfont '##....##'
.comment
str1 .text " 1",0,"L",0 
str2 .text " 2",0,"A",0 
str3 .text " 3",0,"Boat",0 
str4 .text " 4",0,"Umbrella",0 
str5 .text " 5",0,"C=",0 
str6 .text " 6",0,"Darlek",0 	
str7 .text " 7",0,"Ghost",0
str8 .text " 8",0,"Happy Sun",0
strl1 .text " 1",0,"Spikey",0
strl2 .text " 2",0,"Race Car",0
strl3 .text " 3",0,"Frogger",0
strl4 .text " 4",0,"Vine Monster",0
strl5 .text " 5",0,"Powerup Bug",0
strl6 .text " 6",0,"Hexagon",0
strl7 .text " 7",0,"Scroll",0
strl8 .text " 8",0,"Boulderdash",0
.endc
