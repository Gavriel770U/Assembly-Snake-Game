IDEAL 
MODEL small 
STACK 100h 
DATASEG
	seed dw 2 						; seed for random numbers generator 
	wasd dw 1, 0, 0, 0  	; the previous pressed buttons. 
	current db 'w' 				; current pressed button 
	outOfBorder dw 0 			; 0 - false, 1 - true. 
	snakelength dw 1 			; the length of the snake 
	currentColor dw 3110H ; the current color of the snake's tail
	star dw 4000 dup (0)  ; snake coordinates 
	
CODESEG 

Clock equ 046ch
maxColor equ 6410H
minColor equ 3110H

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; -----------------------GENERAL PURPOSE FUNCTIONS -----------------------------------
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
proc delay
	; function that delays the program using nested loop that does nothing. 
	push cx 
	mov cx, 20h 
	outerDelayloop:
		push cx 
		mov cx, 0ffffh
		innerDelayLoop: loop innerDelayLoop
	pop cx 
	loop outerDelayloop	
	pop cx 
	ret 
endp delay 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
proc clearScreen 
	; the function gets the value of es to change the textual screen 
	; the function that clears the screen 
	push bp 
	mov bp, sp 
	push es 
	push di 
	push ax 
	mov al, ' ' ; settings to ax to be a black space
	mov ah, 0 
	mov es, [bp+4] ; [bp+4] -> es 
	mov di, 0  
	clearScreenLoop: 
		mov [es:di], ax 
		add di, 2
		cmp di, (25*80+1)*2 
		jne clearScreenLoop
	pop ax 
	pop di 
	pop es 
	pop bp 
	ret 2 
endp clearScreen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
proc moveData
	; function that gets the length of the snake, and the coordiantes of the snake parts
	; and changes them in order to move the snake. 
	push bp 
	mov bp, sp 
	push cx ; the length of the snake
	push di ; the address of the snake coordinates 
	push si
	mov cx, [bp+6] 
	mov di, [bp+4] 
	dec cx 
	shl cx,1
	add di, cx 
	shr cx,1
	moveDataLoop:
		mov si, [di-2]
		mov [di], si 
		sub di, 2
	loop moveDataLoop
	pop si 
	pop di 
	pop cx
	pop bp 
	ret 4
endp moveData
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
proc generateRandomNumber
	; function that gets the offset of the snake coordinates and the 'seed' variable address.
	; the function generates random number and puts it in the 'seed' variable. 
	; also this function checks that the apple has been spawned correctly
	push bp 
	mov bp, sp 
	push bx ; the seed 
	push ax 
	push cx 
	push dx
	push si 
	push di 
	mov bx, [bp+4] ; seed
	mov di, [bp+6] ; push cx -> push di -> push bx 
rnd: 
	mov ax, [bx]
	add ax, 364 
	mov cx, 3838 
	and ax, cx 
	mov [bx], ax 
	cmp ax, 364 
	jbe rnd 
	cmp ax, 3840 
	jae rnd 
	mov si, ax 
	xor dx, dx  
	mov cx, 160 
	div cx 
	cmp dx, 0
	je rnd 
	xor dx, dx 
	mov ax, si 
	add ax, 2
	mov cx, 160 
	div cx 
	cmp dx, 0
	je rnd 
	mov cx, [bp+8] ; the length of the snake value 
	dec cx 
	checkRndLoop:
	mov ax, [di]
	cmp si, ax 
	je rnd 
	add di, 2
	loop checkRndLoop
	
	pop di 
	pop si 
	pop dx 
	pop cx 
	pop ax 
	pop bx
	pop bp 
	ret 6 
endp generateRandomNumber
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	proc exitProg
	push ax 
	mov ax, 4c00h 
	int 21h 
	pop ax 
endp exitprog
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ---------------------- GENERAL PURPOSE FUNCTIONS END -------------------------------
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ------------------------- SNAKE RELATED FUNCTIONS ----------------------------------
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
proc initializeCoordinates ; intiailizes the sanke coordiantes	
	push bp
	mov bp, sp 
	push di 
	push ax 
	mov ax, 25*80
	mov di, [bp+4] ; the address of 'star' array, the snake coordinates
	mov [di], ax 	
	pop ax 
	pop di 
	pop bp 
	ret 2
endp initializeCoordinates

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
proc putSnake 
	; this function gets the textual screen coordiantes, the snake coordiantes, the length of the snake and its color
	; the function puts the snake (the player) on the screen
	; also this function can delete the snake if we'll put its color to black (0)
	push bp 
	mov bp, sp 
	push es
	push ax
	push di 
	push cx 
	push ax 
  push dx 
	mov es, [bp+4] 	; textual screen coordiantes
	mov di, [bp+6] 	; the snake coordiantes address 
	mov cx, [bp+8] 	; the length of the snake 
	mov ax, [bp+10] ; the color of the snake 
  mov dx, [bp+12] ; the color of the snake's tail 
	push si
	putSnakeLoop:
		mov si, [di]
		mov [es:si], ax 
		add di, 2 
	loop putSnakeLoop
  sub di, 2
  mov si, [di]
  mov [es:si], dx 

	pop si
  pop dx 
	pop ax 
	pop cx 
	pop di 
	pop ax 
	pop es 
	pop bp 
	ret 10
endp putSnake 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
proc up
	; the function gets the length of the snake and its coordiantes
	; the function updates the coordiantes of the snake in the dataseg
	; such that the snake we'll move up 
	push bp 
	mov bp, sp 
	push cx ; the length of the snake 
	push di ; the pointer to the snake coordiantes 
	mov cx, [bp+6] 
	mov di, [bp+4]
	cmp cx, 1 ; if the length of the snake is 1 skip the change in the coordiantes. 
	je skipMoveData
	push cx
	push di 
	call moveData
	skipMoveData:
	mov cx, [di] 
	sub cx, 160
	mov [di], cx
	pop di 
	pop cx 
	pop bp 
	ret 4
endp up 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
proc down
	; the function gets the length of the snake and its coordiantes
	; the function updates the coordiantes of the snake in the dataseg
	; such that the snake we'll move down 
	push bp 
	mov bp, sp 
	push cx ; the length of the snake 
	push di ; the pointer to the snake coordiantes 
	mov cx, [bp+6]
	mov di, [bp+4]
	cmp cx, 1
	je skipMoveData2
	push cx
	push di 
	call moveData
	skipMoveData2:
	mov cx, [di] 
	add cx, 160
	mov [di], cx
	pop di 
	pop cx 
	pop bp 
	ret 4
endp down 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
proc right
	; the function gets the length of the snake and its coordiantes
	; the function updates the coordiantes of the snake in the dataseg
	; such that the snake we'll move to the right 
	push bp 
	mov bp, sp 
	push cx ; the length of the snake 
	push di ; the pointer to the snake coordiantes 
	mov cx, [bp+6]
	mov di, [bp+4]
	cmp cx, 1
	je skipMoveData3
	push cx
	push di 
	call moveData
	skipMoveData3:
	mov cx, [di] 
	add cx, 2
	mov [di], cx
	pop di 
	pop cx 
	pop bp 
	ret 4
endp right 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
proc left
	; the function gets the length of the snake and its coordiantes
	; the function updates the coordiantes of the snake in the dataseg
	; such that the snake we'll move to the left 
	push bp 
	mov bp, sp 
	push cx ; the length of the snake 
	push di ; the pointer to the snake coordiantes 
	mov cx, [bp+6]
	mov di, [bp+4]
	cmp cx, 1
	je skipMoveData4
	push cx
	push di 
	call moveData
	skipMoveData4:
	mov cx, [di] 
	sub cx, 2
	mov [di], cx
	pop di 
	pop cx 
	pop bp 
	ret 4
endp left 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

proc checkBorders
	; this procedure gets a pointer to the address the the snake and an address to a variable 
	; that contains 1 if the snake is out of borders, else it contains 0. 
	; procedure that checks if the snake isn't out of the borders. 
	push bp 
	mov bp, sp 
	push cx 
	push dx 
	push di 
	push bx 
	
	mov di, [bp+4] ; [bp+4] -> the original pointer to the address to the coordinates of the snake 
	mov bx, [bp+6] ; [bp+6] -> the original address of the var that is 1 if the snake out of borders, else 0
	mov di, [di]  ; gives di the coordinate of the snakes head 
	
	cmp di, 160 ; upper border 
	jbe outOfBordersUpdate 
	cmp di, 24*80*2 ; lower border 
	jae outOfBordersUpdate
	xor dx, dx ; right border 
	mov ax, di 
	inc ax 
	mov cx, 160
	div cx
	cmp dx, 0 
	je outOfBordersUpdate 
	xor dx, dx 	; left border
	mov ax, di ; ax = di  
	mov cx, 160 
	div cx	
	cmp dx, 0 ; ax%160  = di%160 if 0 -> exit
	je outOfBordersUpdate
	
	jmp skipUpdate
	outOfBordersUpdate:
		mov di, 1 
		mov [bx], di 
		
	skipUpdate:
	pop bx
	pop di 
	pop dx 
	pop cx
	pop bp 
	ret 4 
endp checkBorders
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
proc increaseSnake
	; function that gets: a pointer to the head of the snake, a pointer to the current pressed button and a pointer to the snake's length 
	; this function icreases the snake's length by 1 according to its current direction 
	push bp 
	mov bp, sp 
	push di ; the pointer to the head of the snake 
	push cx ; the length of the snake 
	push si ; the pointer to the snake's length 
	push ax 
	push bx ; the pointer to the current pressed button

	mov bx, [bp+8]
	mov si, [bp+6]
	mov di, [bp+4]
	
	mov cx, [si]
	inc cx 
	mov [si], cx 
	mov si, di 
	
	push cx
	push di 
	call moveData
	
	mov ax, [si] 
	mov bx, [bx]
	cmp bx, 'w' 
	je dec160Head
	cmp bx, 's' 
	je inc160Head
	cmp bx, 'd' 
	je inc2Head
	cmp bx, 'a' 
	je dec2Head
	jmp contIncreaseSnake
	
	dec160Head:
	sub ax, 160 
	jmp contIncreaseSnake
	inc160Head:
	add ax, 160 
	jmp contIncreaseSnake
	inc2Head:
	add ax, 2 
	jmp contIncreaseSnake
	dec2Head:
	sub ax, 2
	jmp contIncreaseSnake
	
	contIncreaseSnake:
	mov [si], ax
	
	pop bx 
	pop ax
	pop si 
	pop cx 
	pop di 
	pop bp 
	ret 6
endp increaseSnake
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
proc checkSnakeHead
	; the function gets the offset of the snake coordinates, the length of the snake and an address of a variable that shows if the snake is out of borders 
	; then, the function checks if the snake's head is in touch with others of its 'body parts', if it's true the function will update the value of the out of borders variable such that it will end the program. 
	push bp 
	mov bp, sp 
	push di 
	push cx 
	push ax 
	push bx 
	
	mov di, [bp+8] ; the offset of the snake coordinates 
	mov cx, [bp+6] ; the length of the snake
	mov bx, [bp+4] ; out of borders variable's address
	cmp cx, 1
	je skipDecCx
	dec cx 
	skipDecCx: mov ax, [di]
	
	checkSnakeHeadLoop:
	cmp ax, [di+2]
	je yes
	add di, 2 
	loop checkSnakeHeadLoop
	jmp skipYes
	yes:
	mov ax, 0001h
	mov [bx], ax 
	skipYes:
	pop bx 
	pop ax 
	pop cx 
	pop di 
	pop bp 
	ret 6
endp checkSnakeHead
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; function that deletes the previos snake, calls to a delay and, moves the snake up and updates wasd array .
proc moveWFunc
	
	push bp
	mov bp, sp 
	push cx 
	push di 
	push es ; textual screen coordiantes
	push dx 
	push bx 
	push si
	push ax 
	
	mov ax, 0b800h 
	mov es, ax 
	
	mov ax, [bp+4] ; pressed button 
	mov si, [bp+6] ; offset of current in dataseg 
	mov bx, [bp+8] ; offset of wasd in dataseg
	mov di, [bp+10] ; offset of star in dataseg 
	mov cx, [bp+12] ; the length of the snake 

	; clear previous snake
	mov dx, 0020h ; dx = ' '
	call delay 
  push dx
	push dx 
	push cx 
	push di 
	push es 
	call putSnake
	
	mov dx, 0001h 
	cmp [bx+4],dx ; is 's' has been pressed previously? 
	je skipMoveW  ; if true skip moving up
	push cx
	push di
	call up
	mov [bx], dx   ; set w as last pressed
	xor dx, dx 
	mov [bx+6], dx ; reset d 
	mov [bx+2], dx ; reset a 
	mov [byte ptr si], 'w'
	
skipMoveW:
	
	pop ax 
	pop si 
	pop bx 
	pop dx 
	pop es 
	pop di 
	pop cx 
	pop bp 
	ret 10
endp moveWFunc 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; function that deletes the previos snake, calls to a delay and, moves the snake down and updates wasd array .
proc moveSFunc
	
	push bp
	mov bp, sp 
	push cx 
	push di 
	push es ; textual screen coordiantes
	push dx 
	push bx 
	push si
	push ax 
	
	mov ax, 0b800h 
	mov es, ax 
	
	mov ax, [bp+4] ; pressed button 
	mov si, [bp+6] ; offset of current in dataseg 
	mov bx, [bp+8] ; offset of wasd in dataseg
	mov di, [bp+10] ; offset of star in dataseg 
	mov cx, [bp+12] ; the length of the snake 
	
	mov dx, 0020h
	call delay 
  push dx 
	push dx 
	push cx 
	push di 
	push es 
	call putSnake

	mov dx, 0001h 
	cmp [bx], dx ; is 'w' has been pressed previously? 
	je skipMoveS  ; if true skip moving down
	push cx
	push di
	call down
	mov [bx+4], dx   ; set s as last pressed
	xor dx, dx 
	mov [bx+6], dx ; reset d 
	mov [bx+2], dx ; reset a 
	mov [byte ptr si], 's'
	
skipMoveS:

	pop ax 
	pop si 
	pop bx 
	pop dx 
	pop es 
	pop di 
	pop cx 
	pop bp 
	ret 10
endp moveSFunc 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; function that deletes the previos snake, calls to a delay and, moves the snake to the left and updates wasd array .
proc moveAFunc
	push bp
	mov bp, sp 
	push cx 
	push di 
	push es ; textual screen coordiantes
	push dx 
	push bx 
	push si
	push ax 
	
	mov ax, 0b800h 
	mov es, ax 
	
	mov ax, [bp+4] ; pressed button 
	mov si, [bp+6] ; offset of current in dataseg 
	mov bx, [bp+8] ; offset of wasd in dataseg
	mov di, [bp+10] ; offset of star in dataseg 
	mov cx, [bp+12] ; the length of the snake 
	
	; clear previous snake from screen
	mov dx, 0020h ; dx = ' '
	call delay 
  push dx 
	push dx 
	push cx 
	push di 
	push es 
	call putSnake
	
	mov dx, 0001h 
	cmp [bx+6],dx ; is 'd' has been pressed previously? 
	je skipMoveA  ; if true skip moving left
	push cx
	push di
	call left
	mov [bx+2], dx   ; set a as last pressed
	xor dx, dx 
	mov [bx], dx   ; reset w 
	mov [bx+4], dx ; reset s 
	mov [byte ptr si], 'a'
	
skipMoveA:
	
	pop ax 
	pop si 
	pop bx 
	pop dx 
	pop es 
	pop di 
	pop cx 
	pop bp 
	ret 10
endp moveAFunc 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; function that deletes the previos snake, calls to a delay and, moves the snake to the right and updates wasd array .
proc moveDFunc
	push bp
	mov bp, sp 
	push cx 
	push di 
	push es ; textual screen coordiantes
	push dx 
	push bx 
	push si
	push ax 
	
	mov ax, 0b800h 
	mov es, ax 
	
	mov ax, [bp+4] ; pressed button 
	mov si, [bp+6] ; offset of current in dataseg 
	mov bx, [bp+8] ; offset of wasd in dataseg
	mov di, [bp+10] ; offset of star in dataseg 
	mov cx, [bp+12] ; the length of the snake 
	
	mov dx, 0020h
	call delay 
  push dx
	push dx 
	push cx 
	push di 
	push es 
	call putSnake
	
	mov dx, 0001h 
	cmp [bx+2], dx ; is 'a' has been pressed previously? 
	je skipMoveD   ; if true skip moving right
	push cx
	push di
	call right
	mov [bx+6], dx   ; set d as last pressed
	xor dx, dx 
	mov [bx], dx   ; reset w 
	mov [bx+4], dx ; reset s 
	mov [byte ptr si], 'd'
	
skipMoveD:
	
	pop ax 
	pop si 
	pop bx 
	pop dx 
	pop es 
	pop di 
	pop cx 
	pop bp 
	ret 10
endp moveDFunc 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; function that initializes the game -> clears the screen, initializes the first snake coordinates, first apple, 
;																				seed for random numbers using clock, and puts the first apple on the screen.

proc initGame 
	push bp 
	mov bp, sp 
	push ax 
	push bx 
	push cx 
	push di
	push es

	mov es, [bp+10] ; the value of the graphic screen highset 
 
	push es 
	call clearScreen
	
	; intialize the coordinates of the snake
	mov di, [bp+8] ; the address of 'star' array - the coordinates of the snake
	push di 
	call initializeCoordinates
	
	; put the snake on the screen
	mov cx, [bp+6] ; the length of the snake value 
  push 100
	push 100 ; red / orange color of the snake
	push cx 
	push di 
	push es 
	call putSnake
	
	; generate random number as coordinate for the apple 
	mov bx, [bp+4] 		 ; the address of 'seed' variable, seed for random numbers
	push es       		 ; save the value of es
	xor ax, ax 
	mov es, ax
	mov ax, [es:Clock] ; set pseudo-random seed initial value using clock 
	and ax, 0000000000000110b 
	pop es
	mov [bx], ax 
	push cx 
	push di
	push bx 
	call generateRandomNumber
	
	; put apple on the screen
	mov al, 'O'
	mov ah, 10 ; green
	mov bx, [bx] 
	mov [es:bx], ax 

	pop es 
	pop di  
	pop cx 
	pop bx 
	pop ax 
	pop bp
	ret 8
endp initGame 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; function that gets the current pressed button that can be w, a, s or d and moves the snake according to it.
; the function gets also the length of the snake, the address of the snake coordinates, the address of wasd buffer, and the address of 'current variable'
proc moveSnake 
	push bp
	mov bp, sp 
	push ax 
	push bx 
	push cx 
	push si 
	push di 

	mov cx, [bp+4] 				; the snakeLength value 
	mov di, [bp+6] 				;  address of 'star' array in the dataseg, snake coordinates
	mov bx, [bp+8]				  ; address of 'wasd' array in the dataseg
	mov si, [bp+10] 					; address of 'current' vairable in the dataseg
	mov al, [byte ptr bp+12] ; current pressed button value 

	cmp al, 'a'
	je moveALabel 
	cmp al, 'd'
	je moveDLabel
	cmp al, 'w'
	je moveWLabel 
	cmp al, 's'
	je moveSLabel

moveALabel:
	push cx 
	push di
	push bx  
	push si 
	push ax 
	call moveAFunc
	jmp endMoveSnakeLabel

moveDLabel:
	push cx 
	push di
	push bx  
	push si 
	push ax 
	call moveDFunc
	jmp endMoveSnakeLabel

moveWLabel:
	push cx 
	push di
	push bx  
	push si 
	push ax 
	call moveWFunc
	jmp endMoveSnakeLabel
	
moveSLabel:
	push cx 
	push di
	push bx  
	push si 
	push ax 
	call moveSFunc
	jmp endMoveSnakeLabel

endMoveSnakeLabel:
	pop di 
	pop si 
	pop cx
	pop bx
	pop ax
	pop bp 
	ret 10
endp moveSnake 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ----------------------- SNAKE RELATED FUNCTIONS END --------------------------------
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

start:
	mov ax, @data
	mov ds, ax 
	mov ax, 0b800h
	mov es, ax  
	
	push es 
	push offset star
	push [snakeLength]
	push offset seed 
	call initGame

	xor ax, ax
	
mainLoop: 
	mov ah, 1h ; get keyboard key input and save it in register al 
	int 16h	
	jz noinput
	mov ah, 0h ; buffer 
	int 16h
	jmp skipNoInput
noinput: 
	mov al, [byte ptr current]
skipNoInput:
	
	cmp al, 'w' ; if w is pressed the player will move up
	je moveSnakeLabel
	cmp al, 's' ; if s is pressed the player will move down
	je moveSnakeLabel
	cmp al, 'd' ; if d is pressed the player will move to the right
	je moveSnakeLabel
	cmp al, 'a' ; if a is pressed the player will move to the left
	je moveSnakeLabel
	cmp al, 27  ; compares the register al with (esc)ape
	je exitCall		; if al == 27 (the decimal value of (esc)ape the func. will end)
	jmp skipExitCall
exitCall: call exitProg
skipExitCall:
	jmp skipMoveSnakeLabel

moveSnakeLabel:
	push ax 
	push offset current 
	push offset wasd 
	push offset star 
	push [snakeLength]
	call moveSnake
skipMoveSnakeLabel:

; put snake on screen
	mov cx, 6410H
  push [currentColor]
	push cx 
	push [snakeLength] 
	mov di, offset star 
	push di 
	push es 
	call putSnake
	
; ~~~ BORDERS CHECK ~~~
	mov bx, offset outOfBorder
	push bx  
	push offset star
	call checkBorders
	push offset star
	push [snakeLength]
	push bx 
	call checkSnakeHead
	mov si, 0001h
	cmp [bx], si 
	je exit 

; check if the snake has eaten the apple and increase it if true
	mov di, [di] 
	cmp di, [seed]
	je incSnakeLabel
	jmp mainLoop 
incSnakeLabel:
	mov di, offset star 
	mov bx, offset current 
	mov si, offset snakeLength
	push cx 
	push bx 
	push si 
	push di 
	call increaseSnake

  push ax 
  mov ax, [currentColor]
  add ah, 50
  cmp ax, maxcolor
  jl skipresetColor
  mov ax, minColor
skipresetColor:
  mov [currentColor], ax 
  pop ax 

	mov bx, offset seed 
	push [snakeLength]
	push di
	push bx 
	call generateRandomNumber
	
	mov cl, 'O'
	mov ch, 10
	mov bx, [seed] 
	mov [es:bx], cx 
jmp mainLoop

exit:
	call exitProg
END start