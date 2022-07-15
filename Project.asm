[org 0x0100]
call startpage
call Bar
call Wall
call ball
call Game1

jmp end

partner1: db 'USJID NISAR (L20-0927)',0
partner2: db 'MURTAZA AHMAD (L20-0954)',0
loader:db 'Loading->',0
press: db 'Press Enter to Continue',0
EndingText: db 'GAME OVER',0

startpage:
		  pusha
			  MOV AH, 06h    ; Scroll up function
			  XOR AL, AL     ;
		      XOR CX, CX     ; Upper left corner CH=row, CL=column
			  MOV DX, 184FH  ; lower right corner DH=row, DL=column 
			  MOV BH, 1Ch   ; background color of startpage
			  INT 10H ;video BIOS

			  mov ah, 0x13 ; service 13 - print string
			  mov al, 1 ; subservice 01 – update cursor
			  mov bh, 0 ; output on page 0
			  mov bl, 15 ; normal attrib
			  mov cx, 22 ; length of string
			  mov dx,0x1022
			  push cs
			  pop es ; segment of string
			  mov bp, partner1 ; offset of string
			  int 0x10 ; call BIOS video service


			  mov ah, 0x13 ; service 13 - print string
			  mov al, 1 ; subservice 01 – update cursor
			  mov bh, 0 ; output on page 0
			  mov bl, 15 ; normal attrib
			  mov cx, 24 ; length of string
			  mov dx,0x1122
			  push cs
			  pop es ; segment of string
			  mov bp, partner2 ; offset of string
			  int 0x10 ; call BIOS video service

			  mov ah, 0x13 ; service 13 - print string
			  mov al, 1 ; subservice 01 – update cursor
			  mov bh, 0 ; output on page 0
			  mov bl, 15 ; normal attrib
			  mov cx, 9 ; length of string
			  mov dx,0x1322
			  push cs
			  pop es ; segment of string
			  mov bp, loader ; offset of string
			  int 0x10 ; call BIOS video service

			  push 0xb800
			  pop es
			  mov ah,0x3  
			  mov al,'B'
			  mov word[es:1992],ax
			  mov al,'R'
			  mov word[es:1994],ax
			  mov al,'I'
			  mov word[es:1996],ax
			  mov al,'C'
			  mov word[es:1998],ax
			  mov al,'K'
			  mov word[es:2000],ax
			  mov al,' '
			  mov word[es:2002],ax
			  mov al,'G'
			  mov word[es:2004],ax
			  mov al,'A'
			  mov word[es:2006],ax
			  mov al,'M'
			  mov word[es:2008],ax
			  mov al,'E'
			  mov word[es:2010],ax
			

			
			  mov al,'C'
			  mov word[es:2014],ax
			  mov al,'O'
			  mov word[es:2016],ax
			  mov al,'A'
			  mov word[es:2018],ax
			  mov al,'L'
			  mov word[es:2020],ax
			  mov al,' '
			  mov word[es:2022],ax
			  mov al,'P'
			  mov word[es:2024],ax
			  mov al,'R'
			  mov word[es:2026],ax
			  mov al,'O'
			  mov word[es:2028],ax
			  mov al,'J'
			  mov word[es:2030],ax
			  mov al,'E'
			  mov word[es:2032],ax
			  mov al,'C'
			  mov word[es:2034],ax
			  mov al,'T'
			  mov word[es:2036],ax
			 			  
			  mov ah,75
			  mov al,178
			  mov cx, 30
			  mov di,3126    

loading:
		  mov bx,0xffff
			  call delay
			  mov bx,0xffff
			  call delay
			  mov bx,0xffff
			  call delay
			  mov bx,0xffff
			  call delay
			  mov word[es:di],ax
			  add di,2
			  loop loading


			  mov ah, 0x13 ; service 13 - print string
			  mov al, 1 ; subservice 01 – update cursor
			  mov bh, 0 ; output on page 0
			  mov bl, 75 ; normal attrib
			  mov cx, 24 ; length of string
			  mov dx,0x1516
			  push cs
			  pop es ; segment of string
			  mov bp, press ; offset of string
			  int 0x10 ; call BIOS video service

enter1:
		  mov ah,0
			  int 16h
			  cmp al,13            ;waits for escape key
			  jne enter1

			  mov ah,0h
			  mov al,3h
			  int 10h            ;this will clear the screen

			  popa
			  ret			  


delay:
		  dec bx
			  cmp bx,0
			  jne delay
			  ret

block: db '          ', 0 ; null terminated string
shooter: db '__.-^-.__',0 ;
myball: db 'O', 0 ; null terminated string
Ballx: dw 39
Bally: dw 23

clrscr: 
	push es
	push ax
	push di
	mov ax, 0xb800
	mov es, ax ; point es to video base
	mov di, 0 ; point di to top left column

nextloc: 
	mov word [es:di], 0x0720 ; clear next char on screen
	add di, 2 ; move to next screen location
	cmp di, 4000 ; has the whole screen cleared
	jne nextloc ; if no clear next position
	pop di
	pop ax
	pop es
	ret


; subroutine to calculate the length of a string
; takes the segment and offset of a string as parameters
strlen:
	push bp
	mov bp,sp
	push es
	push cx
	push di
	les di, [bp+4] ; point es:di to string
	mov cx, 0xffff ; load maximum number in cx
	xor al, al ; load a zero in al
	repne scasb ; find zero in the string
	mov ax, 0xffff ; load maximum number in ax
	sub ax, cx ; find change in cx
	dec ax ; exclude null from length
	pop di
	pop cx
	pop es
	pop bp
	ret 4
; subroutine to print a string
; takes the x position, y position, attribute, and address of a null
; terminated string as parameters
printstr:
	push bp
	mov bp, sp
	push es
	push ax
	push cx
	push si
	push di
	push ds ; push segment of string
	mov ax, [bp+4]
	push ax ; push offset of string
	call strlen ; calculate string length	
	cmp ax, 0 ; is the string empty
	jz exit ; no printing if string is empty
	mov cx, ax ; save length in cx
	mov ax, 0xb800
	mov es, ax ; point es to video base
	mov al, 80 ; load al with columns per row
	mul byte [bp+8] ; multiply with y position
	add ax, [bp+10] ; add x position
	shl ax, 1 ; turn into byte offset
	mov di,ax ; point di to required location
	mov si, [bp+4] ; point si to string
	mov ah, [bp+6] ; load attribute in ah
	cld ; auto increment mode
	nextchar: lodsb ; load next char in al
	stosw ; print char/attribute pair
	loop nextchar ; repeat for the whole string
	exit: pop di
	pop si
	pop cx
	pop ax
	pop es
	pop bp
	ret 8


Bar:
	mov ax, 35
	push ax ; push x position
	mov ax, 24
	push ax ; push y position
	mov ax, 1ch ; blue on white attribute
	push ax ; push attribute
	mov ax, shooter 
	push ax ; push address of message
	mov ah,0h
	mov al,3h
	int 10h            ;this will clear the screen
	call printstr ; call the printstr subroutine
	ret

Wall:
	B11:
		mov ax, 1
		push ax ; push x position
		mov ax, 1
		push ax ; push y position
		mov ax, 1ch ; blue on white attribute
		push ax ; push attribute
		mov ax, block 
		push ax ; push address of message	
		call printstr ; call the printstr subroutine
	B12:
		mov ax, 12
		push ax ; push x position
		mov ax, 1
		push ax ; push y position
		mov ax, 1ch ; blue on white attribute
		push ax ; push attribute
		mov ax, block 
		push ax ; push address of message	
		call printstr ; call the printstr subroutine
	B13:
		mov ax, 23
		push ax ; push x position
		mov ax, 1
		push ax ; push y position
		mov ax, 1ch ; blue on white attribute
		push ax ; push attribute
		mov ax, block 
		push ax ; push address of message	
		call printstr ; call the printstr subroutine	
	B14:
		mov ax, 34
		push ax ; push x position
		mov ax, 1
		push ax ; push y position
		mov ax, 1ch ; blue on white attribute
		push ax ; push attribute
		mov ax, block 
		push ax ; push address of message	
		call printstr ; call the printstr subroutine
	B15:
		mov ax, 45
		push ax ; push x position
		mov ax, 1
		push ax ; push y position
		mov ax, 1ch ; blue on white attribute
		push ax ; push attribute
		mov ax, block 
		push ax ; push address of message	
		call printstr ; call the printstr subroutine

	B16:
		mov ax, 56
		push ax ; push x position
		mov ax, 1
		push ax ; push y position
		mov ax, 1ch ; blue on white attribute
		push ax ; push attribute
		mov ax, block 
		push ax ; push address of message	
		call printstr ; call the printstr subroutine
	B17:
		mov ax, 67
		push ax ; push x position
		mov ax, 1
		push ax ; push y position
		mov ax, 1ch ; blue on white attribute
		push ax ; push attribute
		mov ax, block 
		push ax ; push address of message	
		call printstr ; call the printstr subroutine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	B21:
		mov ax, 1
		push ax ; push x position
		mov ax, 3
		push ax ; push y position
		mov ax, 0x70 ; blue on white attribute
		push ax ; push attribute
		mov ax, block 
		push ax ; push address of message	
		call printstr ; call the printstr subroutine
	B22:
		mov ax, 12
		push ax ; push x position
		mov ax, 3
		push ax ; push y position
		mov ax, 0x70 ; blue on white attribute
		push ax ; push attribute
		mov ax, block 
		push ax ; push address of message	
		call printstr ; call the printstr subroutine
	B23:
		mov ax, 23
		push ax ; push x position
		mov ax, 3
		push ax ; push y position
		mov ax, 0x70 ; blue on white attribute
		push ax ; push attribute
		mov ax, block 
		push ax ; push address of message	
		call printstr ; call the printstr subroutine	
	B24:
		mov ax, 34
		push ax ; push x position
		mov ax, 3
		push ax ; push y position
		mov ax, 0x70 ; blue on white attribute
		push ax ; push attribute
		mov ax, block 
		push ax ; push address of message	
		call printstr ; call the printstr subroutine
	B25:
		mov ax, 45
		push ax ; push x position
		mov ax, 3
		push ax ; push y position
		mov ax, 0x70 ; blue on white attribute
		push ax ; push attribute
		mov ax, block 
		push ax ; push address of message	
		call printstr ; call the printstr subroutine

	B26:
		mov ax, 56
		push ax ; push x position
		mov ax, 3
		push ax ; push y position
		mov ax, 0x70 ; blue on white attribute
		push ax ; push attribute
		mov ax, block 
		push ax ; push address of message	
		call printstr ; call the printstr subroutine
	B27:
		mov ax, 67
		push ax ; push x position
		mov ax, 3
		push ax ; push y position
		mov ax, 0x70 ; blue on white attribute
		push ax ; push attribute
		mov ax, block 
		push ax ; push address of message	
		call printstr ; call the printstr subroutine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	B31:
		mov ax, 1
		push ax ; push x position
		mov ax, 5
		push ax ; push y position
		mov ax, 0x20 ; blue on white attribute
		push ax ; push attribute
		mov ax, block 
		push ax ; push address of message	
		call printstr ; call the printstr subroutine
	B32:
		mov ax, 12
		push ax ; push x position
		mov ax, 5
		push ax ; push y position
		mov ax, 0x20 ; blue on white attribute
		push ax ; push attribute
		mov ax, block 
		push ax ; push address of message	
		call printstr ; call the printstr subroutine
	B33:
		mov ax, 23
		push ax ; push x position
		mov ax, 5
		push ax ; push y position
		mov ax, 0x20 ; blue on white attribute
		push ax ; push attribute
		mov ax, block 
		push ax ; push address of message	
		call printstr ; call the printstr subroutine	
	B34:
		mov ax, 34
		push ax ; push x position
		mov ax, 5
		push ax ; push y position
		mov ax, 0x20 ; blue on white attribute
		push ax ; push attribute
		mov ax, block 
		push ax ; push address of message	
		call printstr ; call the printstr subroutine
	B35:
		mov ax, 45
		push ax ; push x position
		mov ax, 5
		push ax ; push y position
		mov ax, 0x20 ; blue on white attribute
		push ax ; push attribute
		mov ax, block 
		push ax ; push address of message	
		call printstr ; call the printstr subroutine

	B36:
		mov ax, 56
		push ax ; push x position
		mov ax, 5
		push ax ; push y position
		mov ax, 0x20 ; blue on white attribute
		push ax ; push attribute
		mov ax, block 
		push ax ; push address of message	
		call printstr ; call the printstr subroutine
	B37:
		mov ax, 67
		push ax ; push x position
		mov ax, 5
		push ax ; push y position
		mov ax, 0x20 ; blue on white attribute
		push ax ; push attribute
		mov ax, block 
		push ax ; push address of message	
		call printstr ; call the printstr subroutine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	B41:
		mov ax, 1
		push ax ; push x position
		mov ax, 7
		push ax ; push y position
		mov ax, 0x40 ; blue on white attribute
		push ax ; push attribute
		mov ax, block 
		push ax ; push address of message	
		call printstr ; call the printstr subroutine
	B42:
		mov ax, 12
		push ax ; push x position
		mov ax, 7
		push ax ; push y position
		mov ax, 0x40 ; blue on white attribute
		push ax ; push attribute
		mov ax, block 
		push ax ; push address of message	
		call printstr ; call the printstr subroutine
	B43:
		mov ax, 23
		push ax ; push x position
		mov ax, 7
		push ax ; push y position
		mov ax, 0x40 ; blue on white attribute
		push ax ; push attribute
		mov ax, block 
		push ax ; push address of message	
		call printstr ; call the printstr subroutine	
	B44:
		mov ax, 34
		push ax ; push x position
		mov ax, 7
		push ax ; push y position
		mov ax, 0x40 ; blue on white attribute
		push ax ; push attribute
		mov ax, block 
		push ax ; push address of message	
		call printstr ; call the printstr subroutine
	B45:
		mov ax, 45
		push ax ; push x position
		mov ax, 7
		push ax ; push y position
		mov ax, 0x40 ; blue on white attribute
		push ax ; push attribute
		mov ax, block 
		push ax ; push address of message	
		call printstr ; call the printstr subroutine

	B46:
		mov ax, 56
		push ax ; push x position
		mov ax, 7
		push ax ; push y position
		mov ax, 0x40 ; blue on white attribute
		push ax ; push attribute
		mov ax, block 
		push ax ; push address of message	
		call printstr ; call the printstr subroutine
	B47:
		mov ax, 67
		push ax ; push x position
		mov ax, 7
		push ax ; push y position
		mov ax, 0x40 ; blue on white attribute
		push ax ; push attribute
		mov ax, block 
		push ax ; push address of message	
		call printstr ; call the printstr subroutine
	
ret

ball:
	mov ax, word [Ballx]
	push ax ; push x position
	mov ax, word [Bally]
	push ax ; push y position
	mov ax, 00000111b ; blue on white attribute
	push ax ; push attribute
	mov ax, myball 
	push ax ; push address of message
	
	call printstr ; call the printstr subroutine
	ret



BR:
	mov ax, 34
	push ax ; push x position
	mov ax, 7
	push ax ; push y position
	mov ax, 0x0 ; blue on white attribute
	push ax ; push attribute
	mov ax, block 
	push ax ; push address of message	
	call printstr ; call the printstr subroutine
	call sleep
	call sleep
	call sleep
	call sleep
	call sleep
	call sleep
	call sleep
	call sleep
	call sleep
	call sleep
	call sleep
	call sleep
	call sleep
	call sleep
	call sleep
	call sleep
	call sleep
	call sleep
	call sleep
	call sleep
	call sleep
	call sleep
	call sleep
	call sleep
	call sleep
	call sleep
	call sleep
	call sleep
	call sleep
	call sleep
	 pusha
			  mov al,3h
			  int 10h               ;this will clear the screen
			  MOV AH, 06h    ; Scroll up function
			  XOR AL, AL     ;
		  XOR CX, CX     ; Upper left corner CH=row, CL=column
			  MOV DX, 184FH  ; lower right corner DH=row, DL=column 
			  MOV BH, 20h   ; background color of startpage
			  INT 10H ;video BIOS

			  xor ax,ax
			  mov ax,0xb800
			  mov es,ax
			  mov di,2150
			  mov byte[es:di],'G'
			  add di,2
			  mov byte[es:di],'a'
			  add di,2
			  mov byte[es:di],'m'
			  add di,2
			  mov byte[es:di],'e'
			  add di,2
			  mov byte[es:di],' '
			  add di,2
			  mov byte[es:di],'O'
			  add di,2
			  mov byte[es:di],'v'
			  add di,2
			  mov byte[es:di],'e'
			  add di,2
			  mov byte[es:di],'r'
			  add di,2

			  popa
			pop ax
			
			mov ax,4c00h
			int 0x21
			 
	;mov ah,0h
	;mov al,3h
	;int 10h            ;this will clear the screen
	jmp end


Collision:
	cmp word[Bally], 8
	je BR


Game1:
	mov cx,10
Game:

	;call clrscr
	call Bar
	call Wall

	mov ax, word [Bally]
	dec ax
	mov word [Bally],ax

	call ball
	call sleep
	call sleep
	call sleep
	call sleep
	call sleep
	call sleep
	call sleep
	call sleep
	call sleep
	call sleep
	call sleep
	call sleep
	call sleep
	call sleep
	call sleep
	call sleep
	call sleep
	call sleep
	call sleep
	call sleep
	call sleep
	call sleep
	call sleep
	call sleep
	call sleep
	call sleep
	call sleep
	call sleep
	call sleep
	call sleep


	

	call Collision

	loop Game
	ret

sleep: 
    push cx
    mov cx, 0xFFFF
delayy: 
    loop delayy
    pop cx
    ret


end:
 	;mov ah,0h
	;mov al,3h
	;int 10h            ;this will clear the screen
	mov ax,0x4c00
	int 0x21