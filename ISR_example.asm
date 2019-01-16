; ISR_example.asm: a) Increments/decrements a BCD variable every half second using
; an ISR for timer 2; b) Generates a 2kHz square wave at pin P3.7 using
; an ISR for timer 0; and c) in the 'main' loop it displays the variable
; incremented/decremented using the ISR for timer 2 on the LCD.  Also resets it to 
; zero if the 'BOOT' pushbutton connected to P4.5 is pressed.
$NOLIST
$MODLP51
$LIST

; There is a couple of typos in MODLP51 in the definition of the timer 0/1 reload
; special function registers (SFRs), so:

TIMER0_RELOAD_L DATA 0xf2
TIMER1_RELOAD_L DATA 0xf3
TIMER0_RELOAD_H DATA 0xf4
TIMER1_RELOAD_H DATA 0xf5

CLK           EQU 22118400 ; Microcontroller system crystal frequency in Hz
TIMER0_RATE   EQU 4096     ; 2048Hz squarewave (peak amplitude of CEM-1203 speaker)
TIMER0_RELOAD EQU ((65536-(CLK/TIMER0_RATE)))
TIMER2_RATE   EQU 1000     ; 1000Hz, for a timer tick of 1ms
TIMER2_RELOAD EQU ((65536-(CLK/TIMER2_RATE)))

BOOT_BUTTON   equ P4.5
SOUND_OUT     equ P3.7
SECOND_BUTTON equ P0.0
MINUTE_BUTTON equ P0.2
HOUR_BUTTON   equ P0.4
DAY_BUTTON    equ P0.5
AM_BUTTON     equ P0.3
SWITCH_BUTTON  equ P0.1
; Reset vector
org 0x0000
    ljmp main

; External interrupt 0 vector (not used in this code)
org 0x0003
	reti

; Timer/Counter 0 overflow interrupt vector
org 0x000B
	ljmp Timer0_ISR

; External interrupt 1 vector (not used in this code)
org 0x0013
	reti

; Timer/Counter 1 overflow interrupt vector (not used in this code)
org 0x001B
	reti

; Serial port receive/transmit interrupt vector (not used in this code)
org 0x0023 
	reti
	
; Timer/Counter 2 overflow interrupt vector
org 0x002B
	ljmp Timer2_ISR

; In the 8051 we can define direct access variables starting at location 0x30 up to location 0x7F
dseg at 0x30
Count1ms:     ds 2 ; Used to determine when half second has passed
BCD_counter:  ds 1 ; The BCD counter incrememted in the ISR and displayed in the main loop
BCD_minute:   ds 1
BCD_hour:     ds 1
BCD_day:      ds 1
Mode:         ds 1
BCD_counter2:  ds 1 ; The BCD counter incrememted in the ISR and displayed in the main loop
BCD_minute2:   ds 1
BCD_hour2:     ds 1
BCD_counter3:  ds 1 ; The BCD counter incrememted in the ISR and displayed in the main loop
BCD_minute3:   ds 1
BCD_hour3:     ds 1



; In the 8051 we have variables that are 1-bit in size.  We can use the setb, clr, jb, and jnb
; instructions with these variables.  This is how you define a 1-bit variable:
bseg
BCD_ampm:     dbit 1
half_seconds_flag: dbit 1 ; Set to one in the ISR every time 500 ms had passed
trigger_alarm:     dbit 1
cseg
; These 'equ' must match the wiring between the microcontroller and the LCD!
LCD_RS equ P1.1
LCD_RW equ P1.2
LCD_E  equ P1.3
LCD_D4 equ P3.2
LCD_D5 equ P3.3
LCD_D6 equ P3.4
LCD_D7 equ P3.5
$NOLIST
$include(LCD_4bit.inc) ; A library of LCD related functions and utility macros
$LIST

;                     1234567890123456    <- This helps determine the location of the counter
Initial_Message:  db 'xx:xx:xx   M', 0
Mon:           db 'MON', 0
Tue:            db 'TUE', 0
Wed:            db 'WED', 0
Thu:           db 'THU', 0
Fri:           db 'FRI', 0
Sat:           db 'SAT', 0
Sun:           db 'SUN', 0
Wdy:           db 'WDY', 0
Wkn:           db 'WKN', 0
Clock:         db 'CLK', 0
Year:          db '2019', 0
;---------------------------------;
; Routine to initialize the ISR   ;
; for timer 0                     ;
;---------------------------------;
Timer0_Init:
	mov a, TMOD
	anl a, #0xf0 ; Clear the bits for timer 0
	orl a, #0x01 ; Configure timer 0 as 16-timer
	mov TMOD, a
	mov TH0, #high(TIMER0_RELOAD)
	mov TL0, #low(TIMER0_RELOAD)
	; Set autoreload value
	mov TIMER0_RELOAD_H, #high(TIMER0_RELOAD)
	mov TIMER0_RELOAD_L, #low(TIMER0_RELOAD)
	; Enable the timer and interrupts
    
    clr ET0  ; Enable timer 0 interrupt
    setb TR0  ; Start timer 0
	ret

;---------------------------------;
; ISR for timer 0.  Set to execute;
; every 1/4096Hz to generate a    ;
; 2048 Hz square wave at pin P3.7 ;
;---------------------------------;
Timer0_ISR:
	;clr TF0  ; According to the data sheet this is done for us already.
	
	cpl SOUND_OUT ; Connect speaker to P3.7!
 	reti

;---------------------------------;
; Routine to initialize the ISR   ;
; for timer 2                     ;
;---------------------------------;
Timer2_Init:
	mov T2CON, #0 ; Stop timer/counter.  Autoreload mode.
	mov TH2, #high(TIMER2_RELOAD)
	mov TL2, #low(TIMER2_RELOAD)
	; Set the reload value
	mov RCAP2H, #high(TIMER2_RELOAD)
	mov RCAP2L, #low(TIMER2_RELOAD)
	; Init One millisecond interrupt counter.  It is a 16-bit variable made with two 8-bit parts
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	; Enable the timer and interrupts
    setb ET2  ; Enable timer 2 interrupt
    setb TR2  ; Enable timer 2
	ret

;---------------------------------;
; ISR for timer 2                 ;
;---------------------------------;
Timer2_ISR:
	clr TF2  ; Timer 2 doesn't clear TF2 automatically. Do it in ISR
	cpl P3.6 ; To check the interrupt rate with oscilloscope. It must be precisely a 1 ms pulse.
	
	; The two registers used in the ISR must be saved in the stack
	push acc
	push psw
	
	; Increment the 16-bit one mili second counter
	inc Count1ms+0    ; Increment the low 8-bits first
	mov a, Count1ms+0 ; If the low 8-bits overflow, then increment high 8-bits
	jnz Inc_Done
	inc Count1ms+1

Inc_Done:
	; Check if half second has passed
	mov a, Count1ms+0
	cjne a, #low(1000), ISR_done ; Warning: this instruction changes the carry flag!
	mov a, Count1ms+1
	cjne a, #high(1000), ISR_done
	sjmp Not_done
ISR_done:
    ljmp Timer2_ISR_done	
Not_Done:	
	; 1000(aka 1 second) milliseconds have passed.  Set a flag so the main program knows
	setb half_seconds_flag ; Let the main program know half second had passed
	cpl TR0 ; Enable/disable timer/counter 0. This line creates a beep-silence-beep-silence sound.
	; Reset to zero the milli-seconds counter, it is a 16-bit variable
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	; Increment the BCD counter
alarm3: mov a, BCD_day   
        cjne a, #0x06, nextweekend
nextweekend:       
        cjne a, #0x07, alarm2
        mov a, BCD_counter
        cjne a, BCD_counter3, alarm2
        mov a, BCD_minute
        cjne a, BCD_minute3, alarm2
        mov a, BCD_hour
        cjne a, BCD_hour3, alarm2
        setb ET0
        sjmp trigger
alarm2:    
        mov a, BCD_counter
        cjne a, BCD_counter2, normal
        mov a, BCD_minute
        cjne a, BCD_minute2, normal
        mov a, BCD_hour
        cjne a, BCD_hour2, normal
        sjmp trigger        
trigger:
        setb ET0       
        
normal:
	mov a, BCD_counter
	cjne a, #0x59, Second_Increment 

BCD_RESET_WEEK:
    mov a, BCD_hour
    cjne a, #0x11, BCD_RESET_HOUR
    mov a, BCD_minute
    cjne a, #0x59, BCD_Minute_Increment 
    mov a, BCD_day
    cjne a, #0x7, BCD_RESET_HOUR
    mov a, #0x1
    mov BCD_day, a
BCD_RESET_HOUR:
    mov a, BCD_hour
    cjne a, #0x12, BCD_AM_PM
    mov a, BCD_minute
    cjne a, #0x59, BCD_Minute_Increment 
    mov a, #0x1
    mov BCD_hour, a 
    sjmp Reset_Minute
BCD_AM_PM:
    mov a, BCD_hour
    cjne a, #0x11, BCD_Hour_Increment
    mov a, BCD_minute
    cjne a, #0x59, BCD_Minute_Increment
    cpl BCD_ampm
    jnb BCD_ampm, BCD_NEXT_DAY
    sjmp BCD_Hour_Increment
BCD_NEXT_DAY:
    mov a, BCD_day
    add a, #0x1
    da a
    mov BCD_day, a
BCD_Hour_Increment:
    mov a, BCD_minute
    cjne a, #0x59, BCD_Minute_Increment
    mov a, BCD_hour
    add a, #0x01
    da a
    mov BCD_hour, a
Reset_Minute:
    mov a, #0x00
    mov BCD_counter, a
    mov BCD_minute, a
    sjmp Timer2_ISR_done

BCD_Minute_Increment:
    add a, #0x01
    da a
    mov BCD_minute, a
    mov a, #0x00
    mov BCD_counter, a
    sjmp Timer2_ISR_done

Second_Increment:	
    mov a, BCD_counter
	add a, #0x01
	sjmp Timer2_ISR_da
	
Timer2_ISR_decrement:
	add a, #0x99 ; Adding the 10-complement of -1 is like subtracting 1.
Timer2_ISR_da:
	da a ; Decimal adjust instruction.  Check datasheet for more details!
	mov BCD_counter, a
	
Timer2_ISR_done:
	pop psw
	pop acc
	reti

;---------------------------------;
; Main program. Includes hardware ;
; initialization and 'forever'    ;
; loop.                           ;
;---------------------------------;
main:
	; Initialization
    mov SP, #0x7F
    clr trigger_alarm
    lcall Timer0_Init
    lcall Timer2_Init
    ; In case you decide to use the pins of P0, configure the port in bidirectional mode:
    mov P0M0, #0
    mov P0M1, #0
    setb EA   ; Enable Global interrupts
    lcall LCD_4BIT
    Set_Cursor(2,1)
    Send_Constant_String(#Year)
    ; For convenience a few handy macros are included in 'LCD_4bit.inc':
	Set_Cursor(1, 1)
    Send_Constant_String(#Initial_Message)
    clr BCD_ampm
    setb half_seconds_flag
	mov BCD_counter, #0x55
	mov BCD_minute, #0x59
	mov BCD_hour, #0x11
	mov BCD_day, #0x05
	mov Mode, #0x01
	mov BCD_counter2, #0x0
   mov BCD_minute2, #0x0 
   mov BCD_hour2, #0x12
   mov BCD_counter3, #0x0
   mov BCD_minute3, #0x0 
   mov BCD_hour3, #0x12
	; After initialization the program stays in this 'forever' loop
loop:

    
	jb BOOT_BUTTON, Change_second  ; if the 'BOOT' button is not pressed skip
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb BOOT_BUTTON, Change_second  ; if the 'BOOT' button is not pressed skip
	jnb BOOT_BUTTON, $		; Wait for button release.  The '$' means: jump to same instruction.
	; A valid press of the 'BOOT' button has been detected, reset the BCD counter.
	; But first stop timer 2 and reset the milli-seconds counter, to resync everything.
	clr TR2                 ; Stop timer 2
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	; Now clear the BCD counter
	setb TR2
	clr ET0
Change_second:
                        
	jb SECOND_BUTTON, Change_minute
	Wait_Milli_Seconds(#50)
	jb SECOND_BUTTON, Change_minute
	jnb SECOND_BUTTON, $
	mov a, BCD_counter
	cjne a, #0x59, Add_second
	mov a, #0x0
	mov BCD_counter, a
	ljmp Change_minute
Add_second:
    add a, #0x1
    da a
    mov BCD_counter, a	
	ljmp Change_minute
	
Change_minute:
                        
	jb MINUTE_BUTTON, Change_hour
	Wait_Milli_Seconds(#50)
	jb MINUTE_BUTTON, Change_hour
	jnb MINUTE_BUTTON, $
	mov a, BCD_minute
	cjne a, #0x59, Add_minute
	mov a, #0x0
	mov BCD_minute, a
	ljmp Change_hour
Add_minute:
    add a, #0x1
    da a
    mov BCD_minute, a	
	ljmp Change_hour


Change_hour:
                        
	jb HOUR_BUTTON, Change_day
	Wait_Milli_Seconds(#50)
	jb HOUR_BUTTON, Change_day
	jnb HOUR_BUTTON, $
	mov a, BCD_hour
	cjne a, #0x12, Add_hour
	mov a, #0x01
	mov BCD_hour, a
	ljmp Change_day
Add_hour:
    add a, #0x01
    da a
    mov BCD_hour, a	
	ljmp Change_day
	
Change_day:
                        
	jb DAY_BUTTON, Change_AM
	Wait_Milli_Seconds(#50)
	jb DAY_BUTTON, Change_AM
	jnb DAY_BUTTON, $
	mov a, BCD_day
	cjne a, #0x07, Add_day
	mov a, #0x01
	mov BCD_day, a
	ljmp Change_AM
Add_day:
    add a, #0x01
    da a
    mov BCD_day, a	
	ljmp Change_AM

Change_AM:
   	jb AM_BUTTON, Change_mode
	Wait_Milli_Seconds(#50)
	jb AM_BUTTON, Change_mode
	jnb AM_BUTTON, $
	cpl BCD_ampm

Change_mode:
    jb SWITCH_BUTTON, loop_b
	Wait_Milli_Seconds(#50)
	jb SWITCH_BUTTON, loop_b
	jnb SWITCH_BUTTON, $
	mov a, Mode
	cjne a, #0x01, Check_Reset
Increment:	
	add a, #0x1
	da a
	mov Mode, a
	ljmp loop_B
Check_Reset: cjne a, #0x02, Reset_Mode
	ljmp Increment
Reset_Mode:
    mov a, #0x01
    mov Mode, a
loop_b:
    mov a, Mode
    cjne a, #0x01, Weekday_Mode
    Set_Cursor(2,14)
    Send_Constant_String(#Clock)
    clr half_seconds_flag ; We clear this flag in the main loop, but it is set in the ISR for timer 2
	Set_Cursor(1, 7)     ; the place in the LCD where we want the BCD counter value
	Display_BCD(BCD_counter) ; This macro is also in 'LCD_4bit.inc'
	Set_Cursor(1,4)
	Display_BCD(BCD_minute)
	Set_Cursor(1,1)
	Display_BCD(BCD_hour)
	jb BCD_ampm, PM
	Set_Cursor(1,11)
	Display_char(#'A')
	ljmp Set_day
	
Weekday_Mode:
	cjne a, #0x02, Weekend_Mode
	ljmp Weekday
	
Weekend_Mode: 
    ljmp Weekend
    	
PM: 
    Set_Cursor(1,11)
    Display_char(#'P')
   	
Set_day:
	mov a, BCD_day
	Set_Cursor(1,14)
	cjne a, #0x01, Tuesday
	Send_Constant_String(#Mon)
	ljmp loop
Tuesday:
    cjne a, #0x02, Wednesday
	Send_Constant_String(#Tue)
	ljmp loop
Wednesday:
    cjne a, #0x03, Thursday
	Send_Constant_String(#Wed)
	ljmp loop
Thursday:
    cjne a, #0x04, Friday
	Send_Constant_String(#Thu)
	ljmp loop
Friday:      
    cjne a, #0x05, Saturday
	Send_Constant_String(#Fri)
	ljmp loop
Saturday:
    cjne a, #0x06, Sunday
	Send_Constant_String(#Sat)
	ljmp loop
Sunday:
	Send_Constant_String(#Sun)
    ljmp loop
    

Weekday:
   Set_Cursor(2,14)
   Send_Constant_String(#Wdy)
        
   
   Change_second2:
                        
	jb SECOND_BUTTON, Change_minute2
	Wait_Milli_Seconds(#50)
	jb SECOND_BUTTON, Change_minute2
	jnb SECOND_BUTTON, $
	mov a, BCD_counter2
	cjne a, #0x59, Add_second2
	mov a, #0x0
	mov BCD_counter2, a
	ljmp Change_minute2
Add_second2:
    add a, #0x1
    da a
    mov BCD_counter2, a	
	ljmp Change_minute2
	
Change_minute2:
                        
	jb MINUTE_BUTTON, Change_hour2
	Wait_Milli_Seconds(#50)
	jb MINUTE_BUTTON, Change_hour2
	jnb MINUTE_BUTTON, $
	mov a, BCD_minute2
	cjne a, #0x59, Add_minute2
	mov a, #0x0
	mov BCD_minute2, a
	ljmp Change_hour2
Add_minute2:
    add a, #0x1
    da a
    mov BCD_minute2, a	
	ljmp Change_hour2


Change_hour2:
                        
	jb HOUR_BUTTON, loop_c
	Wait_Milli_Seconds(#50)
	jb HOUR_BUTTON, loop_c
	jnb HOUR_BUTTON, $
	mov a, BCD_hour2
	cjne a, #0x12, Add_hour2
	mov a, #0x01
	mov BCD_hour2, a
	ljmp loop_c
Add_hour2:
    add a, #0x01
    da a
    mov BCD_hour2, a	
	ljmp loop_c
	
loop_c:

    jnb SWITCH_BUTTON,BACK
     
    Set_Cursor(2,14)
    Send_Constant_String(#Wdy)
	Set_Cursor(1, 7)     
	Display_BCD(BCD_counter2) 
	Set_Cursor(1,4)
	Display_BCD(BCD_minute2)
	Set_Cursor(1,1)
	Display_BCD(BCD_hour2)
	ljmp Change_second2
BACK:   ljmp Weekend
   
Weekend:
   Set_Cursor(2,14)
   Send_Constant_String(#Wkn)
         
   
   Change_second3:
                        
	jb SECOND_BUTTON, Change_minute3
	Wait_Milli_Seconds(#50)
	jb SECOND_BUTTON, Change_minute3
	jnb SECOND_BUTTON, $
	mov a, BCD_counter3
	cjne a, #0x59, Add_second3
	mov a, #0x0
	mov BCD_counter3, a
	ljmp Change_minute3
Add_second3:
    add a, #0x1
    da a
    mov BCD_counter3, a	
	ljmp Change_minute3
	
Change_minute3:
                        
	jb MINUTE_BUTTON, Change_hour3
	Wait_Milli_Seconds(#50)
	jb MINUTE_BUTTON, Change_hour3
	jnb MINUTE_BUTTON, $
	mov a, BCD_minute3
	cjne a, #0x59, Add_minute3
	mov a, #0x0
	mov BCD_minute3, a
	ljmp Change_hour3
Add_minute3:
    add a, #0x1
    da a
    mov BCD_minute3, a	
	ljmp Change_hour3


Change_hour3:
                        
	jb HOUR_BUTTON, loop_d
	Wait_Milli_Seconds(#50)
	jb HOUR_BUTTON, loop_d
	jnb HOUR_BUTTON, $
	mov a, BCD_hour3
	cjne a, #0x12, Add_hour3
	mov a, #0x01
	mov BCD_hour3, a
	ljmp loop_d
Add_hour3:
    add a, #0x01
    da a
    mov BCD_hour3, a	
	ljmp loop_d
	
loop_d:

    jnb SWITCH_BUTTON,BACK2
     
    Set_Cursor(2,14)
    Send_Constant_String(#Wkn)
	Set_Cursor(1, 7)     
	Display_BCD(BCD_counter3) 
	Set_Cursor(1,4)
	Display_BCD(BCD_minute3)
	Set_Cursor(1,1)
	Display_BCD(BCD_hour3)
	ljmp Change_second3
BACK2:   ljmp loop
END
