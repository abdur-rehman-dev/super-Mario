[org 0x0100]

jmp start   
 
clear:      ;:::::::::::::::::::: PRINTING PORTION ::::::::::::::::::::;   
		    ; function to clear screen
		pusha	
		push es
		push di
		MOV di,0
		push 0xb800
		pop es
clr:			
		mov ax , WORD[background]
		MOV [es:di],ax
		add di,2
		cmp di,4000
		jne clr
		pop di
		pop es
		popa
		ret
		;subroutine to print string on intro page
	
RANDNUM:
	 push bp
	 mov bp, sp
	 push bx
	 push dx	 
	 push ax
	 
	 mov   ax, 25173
	 mul   word [seed]
	 add   ax, 13849
	 mov   word[seed], ax    ; save the seed for the next call
	 ror   ax,8 
	 
	 mov   bx,[bp+4]  ; maximum value
	 inc bx
	 mov   dx,0
	 div   bx      ; divide by max value
	 mov   [bp+6],dx  ; return the remainder
	 
	 pop ax
	 pop   dx
	 pop   bx
	 pop bp

	 ret 
  		
printstr:
	pusha
 
    MOV [es:3528],word 0x01ba ; line of box
    MOV [es:3544],word 0x01ba ; line of box
	MOV [es:3376],word 0x01cd ; line of box
	MOV [es:3370],word 0x01cd ; line of box
	MOV [es:3368],word 0x01cd ; line of box
	MOV [es:3372],word 0x01cd ; line of box
	MOV [es:3374],word 0x01cd ; line of box
	MOV [es:3378],word 0x01cd ; line of box
	MOV [es:3380],word 0x01cd ; line of box
	MOV [es:3382],word 0x01cd ; line of box
	MOV [es:3384],word 0x01cd ; line of box
	MOV [es:3696],word 0x01cd ; line of box
	MOV [es:3690],word 0x01cd ; line of box
	MOV [es:3688],word 0x01cd ; line of box
	MOV [es:3692],word 0x01cd ; line of box
	MOV [es:3694],word 0x01cd ; line of box
	MOV [es:3698],word 0x01cd ; line of box
	MOV [es:3700],word 0x01cd ; line of box
	MOV [es:3702],word 0x01cd ; line of box
	MOV [es:3704],word 0x01cd ; line of box
	MOV [es:3650],word 0x01ba ; line of box
    MOV [es:3666],word 0x01ba ; line of box
	MOV [es:3490],word 0x01cd ; line of box
	MOV [es:3492],word 0x01cd ; line of box
	MOV [es:3494],word 0x01cd ; line of box
	MOV [es:3496],word 0x01cd ; line of box
	MOV [es:3498],word 0x01cd ; line of box
	MOV [es:3506],word 0x01cd ; line of box
	MOV [es:3502],word 0x01cd ; line of box
	MOV [es:3504],word 0x01cd ; line of box		
	MOV [es:3810],word 0x01cd ; line of box
	MOV [es:3812],word 0x01cd ; line of box
	MOV [es:3814],word 0x01cd ; line of box
	MOV [es:3816],word 0x01cd ; line of box
	MOV [es:3818],word 0x01cd ; line of box
	MOV [es:3820],word 0x01cd ; line of box
	MOV [es:3822],word 0x01cd ; line of box
	MOV [es:3824],word 0x01cd ; line of box
	MOV [es:3826],word 0x01cd ; line of box
	MOV [es:3500],word 0x01cd ; line of box
	
    MOV ah, 0x13 ; service 13 - print string
	MOV al, 1 ; subservice 01 – update cursor
	MOV bh, 0 ; output on page 0
	MOV bl, 129 ; normal attrib
	MOV dx, 0x1605 ; row 10 column 3
	MOV cx, 7 ; length of string
	push ds
	pop es ; segment of string
	MOV bp, str1 ; offset of string
	int 0x10 ; call BIOS video service 
	
	MOV ah, 0x13 ; service 13 - print string
	MOV al, 1 ; subservice 01 – update cursor
	MOV bh, 0 ; output on page 0
	MOV bl, 129 ; normal attrib
	MOV dx, 0x1642 ; row 10 column 3
	MOV cx, 7 ; length of string
	push ds
	pop es ; segment of string
	MOV bp, str2 ; offset of string
	int 0x10 ; call BIOS video service
			
	MOV ah, 0x13 ; service 13 - print string
	MOV al, 1 ; subservice 01 – update cursor
	MOV bh, 0 ; output on page 0
	MOV bl, 11 ; normal attrib
	MOV dx, 0x1839 ; row 10 column 3
	MOV cx, 21 ; length of string
	push ds
	pop es ; segment of string
	MOV bp, str4 ; offset of string
	int 0x10 ; call BIOS video service
	
	
	MOV ah, 0x13 ; service 13 - print string
	MOV al, 1 ; subservice 01 – update cursor
	MOV bh, 0 ; output on page 0
	MOV bl, 11 ; normal attrib
	MOV dx, 0x1800 ; row 10 column 3
	MOV cx, 17 ; length of string
	push ds
	pop es ; segment of string
	MOV bp, str3 ; offset of string
	int 0x10 ; call BIOS video service 
	
	popa
	ret
intro: ; prints intro page 
	
	pusha
	
	MOV ax , 0xb800        ; location for video memory
    MOV es , ax 
	MOV [es:720],word 0x04db ;cap
	MOV [es:718],word 0x04db ;cap
	MOV [es:716],word 0x04db ;cap
	MOV [es:714],word 0x04db ;cap
	MOV [es:712],word 0x04db ;cap
	MOV [es:710],word 0x04db ;cap
	MOV [es:868],word 0x04db ;cap
	MOV [es:870],word 0x04db ;cap
	MOV [es:872],word 0x04db ;cap
	MOV [es:874],word 0x04db ;cap
	MOV [es:876],word 0x04db ;cap
	MOV [es:878],word 0x04db ;cap
	MOV [es:880],word 0x04db ;cap
	MOV [es:882],word 0x04db ;cap
	MOV [es:884],word 0x04db ;cap
	MOV [es:886],word 0x04db ;cap
	MOV [es:1028],word 0x06db ;hair
	MOV [es:1030],word 0x06db ;hair
	MOV [es:1032],word 0x06db ;hair
	MOV [es:1034],word 0x3cdb ;skin
	MOV [es:1036],word 0x3cdb ;skin
	MOV [es:1038],word 0x3cdb ;skin
	MOV [es:1040],word 0x00db ;eye
	MOV [es:1042],word 0x3cdb  ;skin
	MOV [es:1186],word 0x06db ;hair
	MOV [es:1188],word 0x3cdb  ;ear
	MOV [es:1190],word 0x06db ;hair
	MOV [es:1192],word 0x3cdb  ;skin
	MOV [es:1194],word 0x3cdb  ;skin
	MOV [es:1196],word 0x3cdb  ;skin
	MOV [es:1198],word 0x3cdb  ;skin
	MOV [es:1200],word 0x00db ;eye
    MOV [es:1202],word 0x3cdb  ;skin
	MOV [es:1346],word 0x06db ;hair
	MOV [es:1348],word 0x3cdb  ;ear
	MOV [es:1350],word 0x06db ;hair
	MOV [es:1352],word 0x06db ;hair
	MOV [es:1354],word 0x3cdb  ;skin
	MOV [es:1356],word 0x3cdb  ;skin
	MOV [es:1358],word 0x3cdb  ;skin
	MOV [es:1360],word 0x3cdb  ;skin
	MOV [es:1362],word 0x00db ;moustache
    MOV [es:1364],word 0x3cdb  ;skin
	MOV [es:1366],word 0x3cdb  ;skin
	MOV [es:1368],word 0x3cdb  ;skin
	MOV [es:1506],word 0x06db ;hair
	MOV [es:1508],word 0x06db ;hair
    MOV [es:1510],word 0x3cdb  ;skin
    MOV [es:1512],word 0x3cdb  ;skin
    MOV [es:1514],word 0x3cdb  ;skin
	MOV [es:1516],word 0x3cdb  ;skin
	MOV [es:1518],word 0x3cdb  ;skin
	MOV [es:1520],word 0x00db ;moustache
	MOV [es:1522],word 0x00db ;moustache
	MOV [es:1524],word 0x00db ;moustache
	MOV [es:1526],word 0x00db ;moustache
	MOV [es:1670],word 0x3cdb  ;skin
	MOV [es:1672],word 0x3cdb  ;skin
	MOV [es:1674],word 0x3cdb  ;skin
	MOV [es:1676],word 0x3cdb  ;skin
	MOV [es:1678],word 0x3cdb  ;skin
	MOV [es:1680],word 0x3cdb  ;skin
	MOV [es:1682],word 0x3cdb  ;skin
	MOV [es:1684],word 0x3cdb  ;skin
	MOV [es:1828],word 0x04db ; shirt
	MOV [es:1830],word 0x04db ; shirt
	MOV [es:1832],word 0x01db ; shirt
	MOV [es:1834],word 0x04db ; shirt
	MOV [es:1836],word 0x04db ; shirt
	MOV [es:1838],word 0x01db ; shirt
	MOV [es:1840],word 0x04db ; shirt
	MOV [es:1986],word 0x04db ; shirt
	MOV [es:1988],word 0x04db ; shirt
	MOV [es:1990],word 0x04db ; shirt
	MOV [es:1992],word 0x01db ; shirt
	MOV [es:1994],word 0x04db ; shirt
	MOV [es:1996],word 0x04db ; shirt
	MOV [es:1998],word 0x01db ; shirt
	MOV [es:2000],word 0x04db ; shirt
	MOV [es:2002],word 0x04db ; shirt
	MOV [es:2004],word 0x04db ; shirt
	MOV [es:2144],word 0x04db ; shirt
	MOV [es:2146],word 0x04db ; shirt
	MOV [es:2148],word 0x04db ; shirt
	MOV [es:2150],word 0x01db ; shirt
	MOV [es:2152],word 0x01db ; shirt
	MOV [es:2154],word 0x01db ; shirt
	MOV [es:2156],word 0x01db ; shirt
	MOV [es:2158],word 0x01db ; shirt
	MOV [es:2160],word 0x01db ; shirt
	MOV [es:2162],word 0x04db ; shirt
	MOV [es:2164],word 0x04db ; shirt
	MOV [es:2166],word 0x04db ; shirt
	MOV [es:2304],word 0x3cdb ; hand
	MOV [es:2306],word 0x3cdb ; hand
	MOV [es:2308],word 0x04db ; shirt
	MOV [es:2310],word 0x01db ; shirt
	MOV [es:2312],word 0x0edb ; shirt
	MOV [es:2314],word 0x01db ; shirt
	MOV [es:2316],word 0x01db ; shirt
	MOV [es:2318],word 0x0edb ; shirt
	MOV [es:2320],word 0x01db ; shirt
	MOV [es:2322],word 0x04db ; shirt
	MOV [es:2324],word 0x3cdb ; hand
	MOV [es:2326],word 0x3cdb ; hand
	MOV [es:2464],word 0x3cdb ; hand
	MOV [es:2466],word 0x3cdb ; hand
	MOV [es:2468],word 0x3cdb ; hand
	MOV [es:2470],word 0x01db ; shirt
	MOV [es:2472],word 0x01db ; shirt
	MOV [es:2474],word 0x01db ; shirt
	MOV [es:2476],word 0x01db ; shirt
	MOV [es:2478],word 0x01db ; shirt
	MOV [es:2480],word 0x01db ; shirt
	MOV [es:2482],word 0x3cdb ; hand
	MOV [es:2484],word 0x3cdb ; hand
	MOV [es:2486],word 0x3cdb ; hand
	MOV [es:2624],word 0x3cdb ; hand
	MOV [es:2626],word 0x3cdb ; hand
	MOV [es:2628],word 0x01db ; shirt
	MOV [es:2630],word 0x01db ; shirt
	MOV [es:2632],word 0x01db ; shirt
	MOV [es:2634],word 0x01db ; shirt
	MOV [es:2636],word 0x01db ; shirt
	MOV [es:2638],word 0x01db ; shirt
	MOV [es:2640],word 0x01db ; shirt
	MOV [es:2642],word 0x01db ; shirt
	MOV [es:2644],word 0x3cdb ; hand
	MOV [es:2646],word 0x3cdb ; hand
	MOV [es:2788],word 0x01db ; shirt
	MOV [es:2790],word 0x01db ; shirt
	MOV [es:2792],word 0x01db ; shirt
	MOV [es:2798],word 0x01db ; shirt
	MOV [es:2800],word 0x01db ; shirt
	MOV [es:2802],word 0x01db ; shirt
	MOV [es:2946],word 0x00db ; shoe
	MOV [es:2948],word 0x00db ; shoe
	MOV [es:2950],word 0x00db ; shoe
	MOV [es:2960],word 0x00db ; shoe
	MOV [es:2962],word 0x00db ; shoe
	MOV [es:2964],word 0x00db ; shoe
	MOV [es:3104],word 0x00db ; shoe
	MOV [es:3106],word 0x00db ; shoe
	MOV [es:3108],word 0x00db ; shoe
	MOV [es:3110],word 0x00db ; shoe
	MOV [es:3120],word 0x00db ; shoe
	MOV [es:3122],word 0x00db ; shoe
	MOV [es:3124],word 0x00db ; shoe
	MOV [es:3126],word 0x00db ; shoe
	
	MOV di,3139
	MOV si,0
	MOV cx,0
    box1:
	add di,116
	MOV si,0
	box2:
	MOV word[es:di+0],0xdb06 ; box
	add di,2
    add si,1
	cmp si,22
	jne box2
	add cx,1
	cmp cx,5
	jne box1
	MOV di,0
	MOV si,0
	MOV cx,0
	
	MOV [es:228],word 0x0153 ; title
	MOV [es:230],word 0x0020 ; title
	MOV [es:232],word 0x0e55 ; title
	MOV [es:234],word 0x0020 ; title
	MOV [es:236],word 0x0450 ; title
	MOV [es:238],word 0x0020 ; title
	MOV [es:240],word 0x0245 ; title
	MOV [es:242],word 0x0020 ; title
	MOV [es:244],word 0x0e52 ; title
	MOV [es:388],word 0x044d ; title
	MOV [es:390],word 0x0020 ; title
	MOV [es:392],word 0x0241 ; title
	MOV [es:394],word 0x0020 ; title
	MOV [es:396],word 0x0e52 ; title
	MOV [es:398],word 0x0020 ; title
	MOV [es:400],word 0x0149 ; title
	MOV [es:402],word 0x0020 ; title
	MOV [es:404],word 0x024f ; title
	 	
	pusha
    call printstr ; to print play and exit button and strings 
	popa
	
	push 0xb800
	pop es
	
	MOV ah,0
	int 16h
		
	cmp al,0x1b ; escape button
	je quit
		
	popa
	ret
	
quit:
	 popa
	 MOV ax , 0x4c00 ; if user presses esc in the start of the play
	 int 21h
	 
dead: ; if mario dies the dead is displayed on the screen 
	pusha
	push word 0xb800
	pop es
	
	mov si,0
	mov cx,46
	
	de1:
	MOV di,[deadcoord + si]
	MOV  [es: di],word 0x04db 
    add si,2
	loop de1

	popa
	ret
	
win: ; if mario wins then win is displayed on the screen 
	pusha
	
	push word 0xb800
	pop es
    mov si,0
	mov cx,33
	wi1:
	MOV di,[wincoord + si]
	MOV  [es: di],word 0x01db 
    add si,2
	loop wi1	
	
	popa
	ret
		 
              ;:::::::::::::: fucntion to check collison :::::::::::::::::::::::::::::::::::::::::::::::
			  ;:::::: it checks the color and updates the value of hit varible :::::::::::::::::::::::::
collision:
	pusha         ; this instruction pushes all registers flags and si and di .
	
	push 0xb800
	pop es
	
    MOV cx,[bp]   ; size
	MOV bx,[bp+2] ; address
	MOV ax,[color] ; text and text attribute ,  ax -> ah(firstcolor) , al(secondColor)
	MOV si,0
	shl cx,1 ; size * 2
	
check:
    MOV di,[bx+si] ; coordinate
    MOV dx,[es:di] ; 0x03db
	
	cmp dh,ah
    je firstColor
	cmp dh,al
    je secondColor
	add si,2
	cmp si,cx
	jne check
	MOV byte[hit],0
	popa 		  ; this instruction popes all registers flags and si and di .
    ret 
	
firstColor:   
 	
	MOV word[isColor1],1
    MOV byte[hit],1
	popa 		  ; this instruction popes all registers flags and si and di .
    ret
secondColor:   
 	
	MOV byte[isColor2],1
    MOV byte[hit],1
	popa 		  ; this instruction popes all registers flags and si and di .
    ret
	;////////////////// function to check the left boundary of screen for ball ////////////////////
	;//////////////////////////////////////////////////////////////////////////////////////////////
	
killMario:
	mov BYTE[is_death],1
	popa 
	ret
dissolveBall:

	mov si , [ballIndex]
	mov WORD[es:si]	, 0x03db ; restoring background 
	mov ax , [emergIndex]
	mov WORD[ballIndex],ax 
	
	push 0
	push 2
	call RANDNUM
	pop bx 
	pop ax
	
	cmp al , 0
	je diag_L
	cmp al , 1
	je diag_R
	cmp al , 2
	je fall_D
	
diag_L:
	mov WORD[ballMove], 158
	jmp exitBall
diag_R:
	mov WORD[ballMove], 162
	jmp exitBall
fall_D:
	mov WORD[ballMove], 160
	jmp exitBall
; ............................................... Object of the boss  ...................................
;........................................................................................................	
ball: 
	pusha
    cmp BYTE[is_death],1
	je exitBall
	
	push 0xb800
	pop es
	
	mov si , [ballIndex]
	add si , [ballMove]
	mov ax, [es:si]
	
	cmp ah , 0x04 ; for red hurdles 
	je dissolveBall
	cmp ah , 0x64 ; for ground
 	je dissolveBall
   	cmp ah , 0x00 ; for castle color
	je dissolveBall
	cmp ah , 0x31 ; for enemy 1 color
	je dissolveBall
   	cmp ah , 0x30 ; for enemy 2 color
	je dissolveBall
	cmp ah , 0x3c ; for mario death
	je killMario
	
	mov WORD[es:si]	, 0x3040 ; updating value of ball at Si + ballmove
	mov WORD[ballIndex],si
	
	sub si , [ballMove]
	mov WORD[es:si]	, 0x03db ; restoring the track of the ball at si by 03db --> blue background
 	
	
exitBall:
	popa 
	ret
	;........................................ ball functions ends.......................................
	;...................................................................................................

setDirectionBoss:
	
	MOV BYTE[BossDir] , 0
	ror BYTE[alt3],1
	jnc b1
	MOV BYTE[BossDir] , 1
b1:
	ret
bossReset:
	call setDirectionBoss
	MOV word[BossTotal],0
	popa
	ret	
	;.................................................. fuction to print boss ..............................................
	;.......................................................................................................................
bossLoop:
	pusha 
	add word[BossTotal],1
	
	cmp word[BossTotal],34
	je bossReset
	  
	push word[bossIndex] ; value of si  ; using the previous value of index to update updated array
	push BossUpdated
    push Bosscord
	push 85;size of array
	MOV bp,sp
	call updateArray
	
	;::::::::: first check the collision :::::::::::::::
	
	push word 0x03db ; restoring background
	push word BossUpdated
	push 85 ; size you want to read
	MOV bp,sp
	call printArray
	
	
	cmp BYTE[BossDir],0
	jne flag14
	add word[bossIndex],2
	add WORD[emergIndex],2
	jmp flag15
flag14:
	sub word[bossIndex],2
	sub WORD[emergIndex],2
flag15:	
	push word[bossIndex] ; value of si
	push BossUpdated
    push Bosscord
	push 85; size of array
	MOV bp,sp
	call updateArray
	
	call boss
	
	MOV cx , 11 ; 15
l3:
	pop word[temp]
	loop l3
	popa 
	ret
	;/////////////////////////////////// for enemy 1 in loop //////////////////////////////////
setDirectionE1:
	
	MOV BYTE[E1dir] , 0
	ror BYTE[alt1],1
	jnc re1
	MOV BYTE[E1dir] , 1
re1:
	ret
	
E1reset:
	call setDirectionE1
	MOV word[E1total],0
	popa
	ret
E1Loop:
	pusha 
	add word[E1total],1
	
	cmp word[E1total],2
	je E1reset
	MOV ax,[E1index]
	
	cmp BYTE[E1dir],0
	jne flag7
	add ax,2
	jmp flag8
flag7:
	sub ax,2
flag8:
	push 10
	push E1updated
    push Ecord1
	push ax
	MOV bp,sp
	
	MOV word[color] , 0x3c3c
	MOV word[color1] , 0x3c3c
	MOV byte[hit],0 ;  reseting value of hit every time 
	call isCollided
	pop word[temp]
	pop word[temp]
	pop word[temp]
	pop word[temp]
	cmp byte[hit],1 
	  je e1Ret
	  
	push word[E1index] ; value of si  ; using the previous value of index to update updated array
	push E1updated
    push Ecord1
	push 10; size of array
	MOV bp,sp
	call updateArray
	
	;::::::::: first check the collision :::::::::::::::
	
	push word 0x03db ; restoring background
	push word E1updated
	push 10 ; size you want to read
	MOV bp,sp
	call printArray
	
	
	cmp BYTE[E1dir],0
	jne flag3
	add word[E1index],2
	jmp flag4
flag3:
	sub word[E1index],2
flag4:	
	push word[E1index] ; value of si
	push E1updated	
    push Ecord1
	push 10; size of array
	MOV bp,sp
	call updateArray
	
	call Enemy1
	
	MOV cx , 11 ; 15
f1:
	pop word[temp]
	loop f1
	popa 
	ret
e1Ret:
	MOV BYTE[is_death],1
	popa
	ret
	
setDirectionE2:
	
	MOV BYTE[E2dir] , 0
	ror BYTE[alt2],1
	jnc re2
	MOV BYTE[E2dir] , 1
re2:
	ret
		
E2reset:
	call setDirectionE2
	MOV word[E2total],0
	popa
	ret
	;................................................. for Enemy 2 in loop ....................................................
E2Loop:
	pusha
	add word[E2total],1
	
	cmp word[E2total],2
	je E2reset
	MOV ax,[E2index]
	
	cmp BYTE[E2dir],0
	jne flag9
	add ax,2
	jmp flag10
flag9:
	sub ax,2
flag10:
	push 8
	push E2updated
    push Ecord2
	push ax
	MOV bp,sp
	
	MOV word[color] , 0x3c3c
	MOV word[color1] , 0x3c3c
	
	MOV byte[hit],0 ;  reseting value of hit every time
	MOV word[color1],0
	call isCollided
	pop word[temp]
	pop word[temp]
	pop word[temp]
	pop word[temp]
	cmp byte[hit],1 
	  je e2Ret
	  
	push word[E2index] ; value of si  ; using the previous value of index to update updated array
	push E2updated
    push Ecord2
	push 8; size of array
	MOV bp,sp
	call updateArray
	
	;::::::::: first check the collision :::::::::::::::
	
	push word 0x03db ; restoring background
	push word E2updated
	push 8 ; size you want to read
	MOV bp,sp
	call printArray
	
	cmp BYTE[E2dir],0
	jne flag5
	add word[E2index],2
	jmp flag6
flag5:
	sub word[E2index],2
flag6:
	push word[E2index] ; value of si
	push E2updated	
    push Ecord2
	push 8; size of array
	MOV bp,sp
	call updateArray
	
	call Enemy2
	
	MOV cx , 11
f2:
	pop word[temp]
	loop f2
	popa 
	ret
	
e2Ret:	
    MOV BYTE[is_death],1
	popa
	ret
; function to print enemy 2 at one place	
Enemy2:
	pusha
	
	push 0xb800
	pop es
	MOV si,0
	ene2:
	MOV ax,[Eatr2 + si]
	MOV di,[Ecord2 + si]
	add di , [E2index]
	MOV  [es: di],word ax 
    add si,2
	cmp si,16
	jne ene2
	
	popa
	ret
	; function to print enemy 1 at one place
Enemy1:
	pusha
	
	push 0xb800
	pop es
	MOV si,0
	ene1:
	MOV ax,[Eatr1+si]
	MOV di,[Ecord1+si]
	add di,[E1index]
	MOV [es:di],word ax 
    add si,2
	cmp si,20
	jne ene1 
	
	popa
	ret
castle: ; function to print castle on screen 
	pusha
	push 0xb800
	pop es
	
	MOV [es:3838 ],word 0x00db 
	MOV [es:3836 ],word 0x00db 
	MOV [es:3834 ],word 0x00db 
	MOV [es:3832 ],word 0x00db 
	MOV [es:3830 ],word 0x00db 
	MOV [es:3828 ],word 0x00db 
	MOV [es:3826 ],word 0x00db 
	MOV [es:3824 ],word 0x00db 
	MOV [es:3822 ],word 0x00db
    MOV [es:3820 ],word 0x00db 
    MOV [es:3818 ],word 0x00db 	
	MOV [es:3816 ],word 0x00db 
	MOV [es:3814 ],word 0x00db 
	MOV [es:3812 ],word 0x00db  
	MOV [es:3810 ],word 0x00db
	MOV [es:3678 ],word 0x00db 
	MOV [es:3676 ],word 0x08db 
	MOV [es:3674 ],word 0x08db 
	MOV [es:3672 ],word 0x08db 
	MOV [es:3670 ],word 0x08db 
	MOV [es:3668 ],word 0x08db 
	MOV [es:3666 ],word 0x00db 
	MOV [es:3664 ],word 0x00db 
	MOV [es:3662 ],word 0x00db 
	MOV [es:3660 ],word 0x08db
    MOV [es:3658 ],word 0x08db 
    MOV [es:3656 ],word 0x08db 	
	MOV [es:3654 ],word 0x08db 
	MOV [es:3652 ],word 0x08db 
	MOV [es:3650 ],word 0x00db 
	MOV [es:3518 ],word 0x00db 
	MOV [es:3516 ],word 0x08db 
	MOV [es:3514 ],word 0x08db 
	MOV [es:3512 ],word 0x08db 
	MOV [es:3510 ],word 0x08db 
	MOV [es:3508 ],word 0x08db 
	MOV [es:3506 ],word 0x00db 
	MOV [es:3504 ],word 0x00db 
	MOV [es:3502 ],word 0x00db 
	MOV [es:3500 ],word 0x08db
    MOV [es:3498 ],word 0x08db 
    MOV [es:3496 ],word 0x08db 	
	MOV [es:3494 ],word 0x08db 
	MOV [es:3492 ],word 0x08db 
	MOV [es:3490 ],word 0x00db
    MOV [es:3358 ],word 0x00db 
	MOV [es:3356 ],word 0x08db 
	MOV [es:3354 ],word 0x08db 
	MOV [es:3352 ],word 0x08db 
	MOV [es:3350 ],word 0x08db 
	MOV [es:3348 ],word 0x08db 
	MOV [es:3346 ],word 0x08db 
	MOV [es:3344 ],word 0x00db 
	MOV [es:3342 ],word 0x08db 
	MOV [es:3340 ],word 0x08db
    MOV [es:3338 ],word 0x08db 
    MOV [es:3336 ],word 0x08db 	
	MOV [es:3334 ],word 0x08db 
	MOV [es:3332 ],word 0x08db 
	MOV [es:3330 ],word 0x00db
	MOV [es:3198 ],word 0x00db 
	MOV [es:3196 ],word 0x08db 
	MOV [es:3194 ],word 0x08db 
	MOV [es:3192 ],word 0x08db 
	MOV [es:3190 ],word 0x08db 
	MOV [es:3188 ],word 0x08db 
	MOV [es:3186 ],word 0x08db 
	MOV [es:3184 ],word 0x08db 
	MOV [es:3182 ],word 0x08db 
	MOV [es:3180 ],word 0x08db
    MOV [es:3178 ],word 0x08db 
    MOV [es:3176 ],word 0x08db 	
	MOV [es:3174 ],word 0x08db 
	MOV [es:3172 ],word 0x08db 
	MOV [es:3170 ],word 0x00db
    MOV [es:3038 ],word 0x00db 
	MOV [es:3036 ],word 0x08db 
	MOV [es:3034 ],word 0x00db 
	MOV [es:3032 ],word 0x00db 
	MOV [es:3030 ],word 0x00db 
	MOV [es:3028 ],word 0x00db 
	MOV [es:3026 ],word 0x00db 
	MOV [es:3024 ],word 0x08db 
	MOV [es:3022 ],word 0x00db 
	MOV [es:3020 ],word 0x00db
    MOV [es:3018 ],word 0x00db 
    MOV [es:3016 ],word 0x00db 	
	MOV [es:3014 ],word 0x00db 
	MOV [es:3012 ],word 0x08db 
	MOV [es:3010 ],word 0x00db 	
	MOV [es:2878 ],word 0x00db 
	MOV [es:2876 ],word 0x00db 
	MOV [es:2874 ],word 0x00db 
	MOV [es:2872 ],word 0x08db 
	MOV [es:2870 ],word 0x08db 
	MOV [es:2868 ],word 0x00db 
	MOV [es:2866 ],word 0x00db 
	MOV [es:2864 ],word 0x00db 
	MOV [es:2862 ],word 0x00db 
	MOV [es:2860 ],word 0x00db
    MOV [es:2858 ],word 0x08db 
    MOV [es:2856 ],word 0x08db 	
	MOV [es:2854 ],word 0x00db 
	MOV [es:2852 ],word 0x00db 
	MOV [es:2850 ],word 0x00db
	MOV [es:2716 ],word 0x00db 
	MOV [es:2714 ],word 0x08db 
	MOV [es:2712 ],word 0x08db 
	MOV [es:2710 ],word 0x08db 
	MOV [es:2708 ],word 0x00db 
	MOV [es:2706 ],word 0x00db 
	MOV [es:2704 ],word 0x08db 
	MOV [es:2702 ],word 0x00db 
	MOV [es:2700 ],word 0x00db
    MOV [es:2698 ],word 0x08db 
    MOV [es:2696 ],word 0x08db 	
	MOV [es:2694 ],word 0x08db 
	MOV [es:2692 ],word 0x00db  
    MOV [es:2556 ],word 0x00db 
	MOV [es:2554 ],word 0x08db 
	MOV [es:2552 ],word 0x08db 
	MOV [es:2550 ],word 0x08db 
	MOV [es:2548 ],word 0x08db 
	MOV [es:2546 ],word 0x08db 
	MOV [es:2544 ],word 0x08db 
	MOV [es:2542 ],word 0x08db 
	MOV [es:2540 ],word 0x08db
    MOV [es:2538 ],word 0x08db 
    MOV [es:2536 ],word 0x08db 	
	MOV [es:2534 ],word 0x08db 
	MOV [es:2532 ],word 0x00db  
    MOV [es:2396 ],word 0x00db 
	MOV [es:2394 ],word 0x08db 
	MOV [es:2392 ],word 0x00db 
	MOV [es:2390 ],word 0x00db 
	MOV [es:2388 ],word 0x00db 
	MOV [es:2386 ],word 0x08db 
	MOV [es:2384 ],word 0x08db 
	MOV [es:2382 ],word 0x08db 
	MOV [es:2380 ],word 0x00db
    MOV [es:2378 ],word 0x00db 
    MOV [es:2376 ],word 0x00db 	
	MOV [es:2374 ],word 0x08db 
	MOV [es:2372 ],word 0x00db  
    MOV [es:2236 ],word 0x00db 
	MOV [es:2234 ],word 0x08db 
	MOV [es:2232 ],word 0x00db 
	MOV [es:2228 ],word 0x00db 
	MOV [es:2226 ],word 0x08db 
	MOV [es:2224 ],word 0x08db 
	MOV [es:2222 ],word 0x08db 
	MOV [es:2220 ],word 0x00db
    MOV [es:2216 ],word 0x00db 	
	MOV [es:2214 ],word 0x08db 
	MOV [es:2212 ],word 0x00db
    MOV [es:2076 ],word 0x00db 
	MOV [es:2074 ],word 0x00db 
	MOV [es:2072 ],word 0x00db 
	MOV [es:2068 ],word 0x00db 
	MOV [es:2066 ],word 0x00db 
	MOV [es:2064 ],word 0x00db 
	MOV [es:2062 ],word 0x00db 
	MOV [es:2060 ],word 0x00db
    MOV [es:2056 ],word 0x00db 	
	MOV [es:2054 ],word 0x00db 
	MOV [es:2052 ],word 0x00db  	
	popa	
Mario: ; print mario on a sepcific index
	pusha
	push 0xb800
	pop es
	
	MOV word si,[index]
	MOV [es:2362 + si],word 0x3cfe ; head 3c
	MOV [es:2522 + si],word 0x3cdb ; body
	MOV [es:2520 + si],word 0x3c2f ; hand left 
	MOV [es:2524 + si],word 0x3c5c ; hand right
	MOV [es:2682 + si],word 0x3cba ; legs
	
	popa
	ret
boss: ; print boss on a sepcific index
	pusha
	push 0xb800
	pop es

	MOV si , 0
	bs:
	MOV ax,[Bossatr+si ]
	MOV di,[Bosscord+si]
	add di , [bossIndex]
	MOV  [es:di],word ax 
    add si,2
	cmp si,170
	jne bs 
	
	popa
	ret

lives: ; function to represent lives on screens
		 
	pusha
	push 0xb800
	pop es	
    MOV si,0
	MOV di,0
	 p1:
	MOV [es:182+si],word 0x3403
	add si,2
	MOV [es:182+si],word 0x03db
    add si,2
	add di,1
	cmp [live],di
	jne p1
	
	MOV ah, 0x13 ; service 13 - print string
	MOV al, 1 ; subservice 01 – update cursor
	MOV bh, 0 ; output on page 0
	MOV bl, 49 
	MOV dx, 0x0101 
	MOV cx, 9 ; length of string
	push ds
	pop es ; segment of string
	MOV bp, str5 ; offset of string
	int 0x10 ; call BIOS video service
	popa
	
Hurdles:

	push es
	push word 0xb800
	pop es
	MOV [es:3750],word 0x04db ;hurdles
	MOV [es:3590],word 0x04db ;hurdles
	MOV [es:3588],word 0x04db ;hurdles
	MOV [es:3592],word 0x04db ;hurdles
	MOV [es:3788],word 0x04db ;hurdles
	MOV [es:3628],word 0x04db ;hurdles
	MOV [es:3468],word 0x04db ;hurdles
	MOV [es:3466],word 0x04db ;hurdles
	MOV [es:3470],word 0x04db ;hurdles
	MOV [es:3710],word 0x04db ;hurdles
	MOV [es:3550],word 0x04db ;hurdles
	MOV [es:3548],word 0x04db ;hurdles
	MOV [es:3552],word 0x04db ;hurdles
	pop es
	ret
	
sun: ; prints yellow sun on top right corner
	pusha
	push 0xb800
	pop es

	MOV si , 0
	s1:
	MOV ax,[sunattr+si ]
	MOV di,[suncoord+si]
	MOV  [es:di],word ax 
    add si,2
	cmp si,128
	jne s1 
	
	popa
	ret
	
ground: ; creates brown layer of ground
	pusha
	
	MOV si , 0
	push word 0xb800
	pop es
glayer: ; glayer means ground layer
	MOV [es:3840 + si],word 0x64b0 ;b1
	add si,2
	cmp si,160
	jne glayer
	popa
    ret	
	
delay:	; function that creates delay
        push dx   ; function that generates delay in the output
		MOV dx ,[delayCount]
tag:		
		dec dx            ; dx is being decreased ffff times
		cmp dx,0
		jne tag
		pop dx
		ret
		
	;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
	;::::::::::::::::::::::::: MOVEMENT LOGICS :::::::::::::::::::::::::::::
	;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
	
	; prints a specific text at given array through stack .
printArray: ; stack must have address , size and text ; 1) attribute 2) address of array 3) size 
	pusha
	
	push 0xb800
	pop es
	
    MOV cx,[bp] ;size
	MOV bx,[bp+2] ; address
	MOV ax,[bp+4] ; text and text attribute 
	MOV si,0
	shl cx,1
print:
    MOV di,[bx+si]
   	MOV [es:di],ax
	add si,2
	cmp si,cx
	jne print
	popa
	ret

updateArray: ; it updates the array with specific value of si 1) si, 2) address of array, 3)size
; address through stack must always be of mcoord or reference array
	pusha
    MOV cx,[bp] ;size
    MOV bx,[bp+2] ; parent array
    MOV dx,[bp+4] ; resultant array 
    MOV si,[bp+6] ; value to be updated with
    MOV di,0
    shl cx,1

update:
	MOV ax,[bx+di]
    add ax,si
	push bx
	MOV bx , dx ; second array which need to be updated.
	MOV [bx+di],ax
	pop bx
	add di,2
	cmp di,cx
	jne update
	popa
    ret
	
copyArray: ;; fuction to copy one array into other
	pusha
	
	MOV bx , [bp+4] ; source array 
	MOV si , [bp+2] ; desitination array
	MOV cx , [bp] ; size of the array (source)
	MOV dx , 0
copy:
	MOV	ax,[bx+di]
	push bx
	MOV bx,si
	MOV [bx+di],ax
	pop bx
	add di,2
	cmp di,cx
	jne copy 
	popa 
	
	ret
	
fallDown: ; if level of mario is not 0 then it makes it zero , it means it maintain its level.

	pusha
	push word[column]
	
	MOV word[MOVe],160
	MOV cx ,WORD[level]
	MOV BYTE[jumpBool],1
	cmp cx , 0 
	je retu
	
MOVeDown:

	call rightMOVe
	call delay
	loop MOVeDown
	
retu:
	pop word[column]
	MOV BYTE[jumpBool],0
	MOV word[MOVe],2
	popa 
	ret
	
isCollided: ; a utility function that updates hit variable , everytime. 
	pusha
 	cmp byte[hit],0
	jne is_hit
	
	MOV ax,[bp]
	push bp ; restoring the value of bp
	push ax ; value of si  ; using the previous value of index to update updated array
	push word[bp+4] ; updated
    push word[bp+2] ; mcoord
	push word[bp+6]; size of array
	MOV bp,sp
	call updateArray
	pop word[temp]
	pop word[temp]
	pop word[temp]
	pop word[temp]
	pop bp
	;36 for yellow eyes , 04 for red
	
	push word[bp+4]
	push word[bp+6]
	MOV bp,sp
	call collision ; for color 1 and color 2 
	mov BYTE[isColor1],0 ; booleans 
	mov BYTE[isColor2],0 ; booleans
	
	cmp byte[hit],1
	je pop_hit
	
	mov ax ,[color1]
	mov [color],ax
	 
	call collision  ; for color 3 and 4 
	pop word[temp]
	pop word[temp]
	
is_hit:
	popa 
	ret
pop_hit:
    pop word[temp]
    pop word[temp]
	jmp is_hit
;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;:::::::::::::: ARROW MOVEMENT KEYS :::::::::::::::::::::::::::::::::::::::::::::::::::::::::
winGame_r: ; if mario collides with FLAg then it ends the game
	popa
	MOV BYTE[is_win],1
	jmp endg
endGame_r: ; if mario collides with FLAg then it ends the game
	popa
	MOV BYTE[is_death],1
	cmp byte[jumpBool],1
	je retTojmp_r
	jmp endg
retTojmp_r:
	ret
;................................... it acts as tag and for function call ..............................
;.......................................................................................................
;............................................. Right move ........................................
;..................................................................................................... 	
rightMOVe: ; any MOVement that requires rightward MOVement or downward JUMP
	pusha
	cmp BYTE[is_death],1
	je endGame_r
	MOV word[color],0x0404
	MOV word[color1],0x3031
	MOV BYTE[successful_MOVe],0 ; reseting success_MOVe bool for later use in level check
	
	cmp word[column],62 ; checking the left boundary of the screen. 
	jae winGame_r
	
	MOV ax,[index]
	add ax,[MOVe]
	push 5
	push updated ; bp+4
	push mcoord ;bp+2
	push ax ;bp
	MOV bp,sp
	
	MOV byte[hit],0
	call isCollided
	pop word[temp]
	pop word[temp]
	pop word[temp]
	pop word[temp]
	
	cmp BYTE[isColor1],1
	je endGame_r ; it means mario has hit enemy
	cmp BYTE[isColor2],1
	je endGame_r ; it means mario has hit enemy
	cmp byte[hit],1
	je exit
	
	push word[index] ; value of si  ; using the previous value of index to update updated array
	push updated
    push mcoord
	push 5; size of array
	MOV bp,sp
	call updateArray
	;::::::::: first check the collision :::::::::::::::
	
	push word 0x03db ; restoring background
	push word updated
	push 5 ; size you want to read
	MOV bp,sp
	call printArray
	
	MOV byte [hit],0
	MOV si,[index]
	add si,[MOVe]
	add word[column],1
	MOV byte[successful_MOVe],1
	MOV word[index],si
	
	push word[index] ; value of si 
	push updated
    push mcoord
	push 5; size of array
	MOV bp,sp
	call updateArray
	call Mario
	
	MOV cx ,11
	
clearstack:	
	pop word[temp]
	loop clearstack

exit:	
	cmp byte[jumpBool],1
	jne l2
	cmp BYTE[successful_MOVe],1
	jne t3
	sub word[level],1
t3:
	
	popa 
	ret
l2:
	
	popa
	call fallDown
	jmp keyboardRet
;............................................. Right move ENDS........................................
;.....................................................................................................

jumpToReturn:
	jmp return
winGame_l:
	popa
	MOV BYTE[is_win],1
	jmp endg
endGame_l:
	popa
	MOV BYTE[is_death],1
	cmp byte[jumpBool],1
	je retTojmp_l
	jmp endg ; it should be keyboardret
retTojmp_l:
	ret
;............................................. Left move starts ......................................
;.....................................................................................................
leftMOVe:   ; any MOVement that requires leftward MOVement or upward JUMP
	pusha
		
	cmp BYTE[is_death],1
	je endGame_l
	MOV word[color],0x0404 ;for red color
	MOV word[color1],0x3031 ;for enemies color
	MOV BYTE[successful_MOVe],0

	cmp word[column],62 ; checking the left boundary of the castle. 
	jae winGame_l
	MOV ax, [index]; 1000
	cmp byte[jumpBool],1 ; checking the jump bool.
	je condition
	cmp ax, 1000 ; checking the left boundary of the screen. 
	je jumpToReturn
	
condition:
	
	sub ax,[MOVe] ; all the pushed elements are necessary. 1002 ; updated value of index = 1000
	push 5
	push updated ; bp + 4
	push mcoord ; bp + 2 
	push ax
	MOV bp,sp
	
	MOV byte[hit],0 ;  reseting value of hit every time 
	call isCollided
	pop word[temp]
	pop word[temp]
	pop word[temp]
	pop word[temp]
	
	cmp BYTE[isColor1],1
	je endGame_l ; it means mario has hit enemy
	cmp BYTE[isColor2],1
	je endGame_l ; it means mario has hit enemy
	cmp byte[hit],1
	je return
	
	push word[index] ; value of si  ; using the previous value of index to update updated array
	push updated
    push mcoord
	push 5; size of array
	MOV bp,sp
	call updateArray
	
	;::::::::: first check the collision :::::::::::::::
	
	push word 0x03db ; restoring background
	push word updated
	push 5 ; size you want to read
	MOV bp,sp
	call printArray
	
	MOV si,[index]
	sub si,[MOVe]
	sub word[column],1
	MOV BYTE[successful_MOVe],1
	MOV word[index],si
	
	push word[index] ; value of si
	push updated	
    push mcoord
	push 5; size of array
	MOV bp,sp
	call updateArray
	
	call Mario
	
	MOV cx,11
cstack:	
	pop word[temp]
	loop cstack
	
return:		
	cmp byte[jumpBool],1
	jne t5
	cmp BYTE[successful_MOVe],1
	jne t4
	add word[level],1
	add word[column],2
t4:	
	
	popa
	ret
t5:	
	
	popa
	call fallDown
	jmp keyboardRet
	
	;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
	;::::::::::::::::: JUMP subroutine ::::::::::::::::::
	; that can even be called as function as well as for jump call
	; it calls MOVeright and MOVeleft functions for jumping , surprising na ?
nojmp1:

	MOV WORD[delayCount],0xffff
	MOV word[MOVe],2
	ret	
jump:

	MOV ax,[downright]
	MOV word[MOVe],ax

	MOV cx , [jumpSize]
	MOV WORD[delayCount],0xffaa
loop1: ; upar le ka jata ha

	call delay
	call leftMOVe
	cmp BYTE[is_death],1
	je  nojmp1
	call E2Loop
	cmp BYTE[is_death],1
	je nojmp1
	call E1Loop
	cmp BYTE[is_death],1
	je nojmp
	call bossLoop
	call ball
	cmp BYTE[is_death],1
	je nojmp
	loop loop1
	
	cmp BYTE[is_death],1
	je nojmp
	
	cmp word[level],0 ; if there was no incomplete jump 
	je nojmp
	
	add cx , [jumpSize]
	MOV ax,[upright]
	MOV word[MOVe],ax

loop2: ; necha ka le ke atta ha 
	call delay
	call rightMOVe
	cmp BYTE[is_death],1
	je nojmp
	call E1Loop
	cmp BYTE[is_death],1
	je nojmp
	call delay
	call E2Loop
	cmp BYTE[is_death],1
	je nojmp
	call ball
	cmp BYTE[is_death],1
	je nojmp
	call bossLoop
	loop loop2
	
	cmp word[level],0
	je nojmp
	MOV cx , [level]
diagonal:           ; this tag restores level through diagonal downMOVement , often needed after jumps 
	call rightMOVe
	cmp BYTE[is_death],1
	je nojmp
	call delay
	loop diagonal
nojmp:

	MOV WORD[delayCount],0xffff
	MOV word[MOVe],2
	ret
	
	;.......................... tag for jump .............................
jumpMOVe:
	pusha 
	MOV byte[jumpBool],1 ; bool for jump is set now
	call jump ; calling of a function
	MOV byte[jumpBool],0
	cmp BYTE[is_death],1
	je exitJump
	call fallDown
exitJump:
	popa
	jmp keyboardRet
	

initialize: ; as mario dies function is used to re-initialize variables 
	pusha
	MOV WORD[E1index],0
	MOV WORD[E2index],0 
	MOV WORD[bossIndex],24
	MOV WORD[E1total],0 ; 12 MOVements 
	MOV WORD[E2total],0 ; 8 MOVements
	MOV WORD[BossTotal],7 ; 8 MOVements
	MOV WORD[emergIndex],1510
	MOV WORD[ballIndex],1510
	MOV WORD[ballMove] , 160
 

	MOV BYTE[alt1], 85 ; special one byte number that contains alternative bits , it is used to generate alternative bits .
	MOV BYTE[alt2], 85 ; special one byte number that contains alternative bits , it is used to generate alternative bits .
	MOV BYTE[alt3], 85 ; special one byte number that contains alternative bits , it is used to generate alternative bits .
 
	MOV BYTE[E1dir],0 ; direction flags for enemy 1
	MOV BYTE[E2dir], 0 ; direction flags for enemy 2
	MOV BYTE[BossDir], 0 ; direction flags for enemy 2

	MOV BYTE[hit], 0
	MOV WORD[index], 1000 ; this varible always store value of si
	MOV WORD[MOVe],2
	MOV WORD[level],0
	MOV WORD[upright],162 ; this is used in jump MOVement
	MOV WORD[downright],158 ; this is used in jump MOVement
	MOV BYTE[jumpBool],0 ; this is used to determine if one step of the jump was successful
	MOV WORD[column],0 ; always retain the column in which the mario is 
	MOV BYTE[successful_MOVe],0 ; type of bool that determines whether there is a complete jump , means "n" diagonal ups and three downs
	MOV WORD[color], 0x0404 
	MOV WORD[color1], 0x3031
	MOV BYTE[isColor2],0
	MOV BYTE[isColor1],0
	MOV BYTE[is_death],0

	popa
	ret
	
unhook:  ; unhook keyboard 
	pusha 
	
	xor ax ,ax
	mov es ,ax

	mov ax, [oldkb] ; read old offset in ax
	mov bx, [oldkb+2] ; read old segment in bx
	cli	
	mov [es:9*4], ax ; restore old offset from ax
	mov [es:9*4+2], bx ; restore old segment from bx
	sti
	popa
	
	ret	
	
keyboard: ; own written isr

	in ax ,0x60
	cmp al,0x4D
	je rightMOVe
	cmp al,0x4B
	je leftMOVe
	cmp al,0x48
	je jumpMOVe

keyboardRet:	 
	
	jmp far [cs:oldkb]
	
start: 
	
    call clear
	call intro ; calling the intro page
	
	xor ax , ax
	MOV es , ax
	
	
		MOV ax , [es:9*4]                                            ; saving the pevious keyboard isr
		MOV [oldkb] , ax
		MOV ax , [es:9*4+2]											 ; saving the previous keyboard isr cs segment
		MOV [oldkb+2],ax
		
	mov WORD[stackPtr],sp; stores the correct value of stack pointers
	
playAgain:
	
	mov sp,[stackPtr]				
	

	call clear
	call initialize
    call Mario
    call Hurdles
	call ground
	call castle
	call boss
	call sun
	call lives
	
	cli
	xor ax ,ax
	mov es ,ax
	
		MOV word [es:9*4],keyboard									 ; store offset at n*4
		MOV [es:9*4+2], cs 											 ; store segment at n*4+2
	
	sti
	
infiniteLoop:
	
	cmp BYTE[is_death],1
	je endg
	call ball
	cmp BYTE[is_death],1
	je endg
	call E1Loop
	cmp BYTE[is_death],1
	je endg
	call delay
	call E2Loop
	cmp BYTE[is_death],1
	je endg
	call bossLoop
	cmp BYTE[is_win],1
	je endg
	jmp infiniteLoop
 
endg:

	call unhook
	cmp BYTE[is_win],1
	je pageClear
	
	sub WORD[live],1
	cmp WORD[live],0
	jne playAgain

pageClear:	; clear pages 
	mov word[background], 0x0720
	call clear
		
printWinPage:
	cmp BYTE[is_death],1
	je printLossPage
	call win ; if mario wins
	jmp exitDOS
printLossPage:
	call dead ; if mario dies 
exitDOS:		
	MOV ax , 0x4c00
	int 21h
 
data:

 oldkb: dd 0

 seed :dw 50
 mcoord: dw 2362,2522,2520,2524,2682 ; mario ka initial coordinates
 initialCoord: dw 3362,3522,3520,3524,3682 ; mario ka initial coordinates
 updated: dw 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 index : dw 1000 ; this varible always store value of si
 temp : dw 0; this is used to pop data from stack
 hit  : db 0 ; this varible can only have 2 values , 0 if there is no collision , 1 if collision accured.
 
 MOVe : dw 2
 level : dw 0
 upright: dw 162 ; this is used in jump MOVement
 downright: dw 158 ; this is used in jump MOVement
 jumpBool: db 0 ; this is used to determine if one step of the jump was successful
 column: dw 0 ; always retain the column in which the mario is 
 successful_MOVe: db 0 ; type of bool that determines whether there is a complete jump , means "n" diagonal ups and three downs
 color: dw 0x0404 ; any value in this variable will be evaluated in isCollided function , so we have to update this function
 color1 :dw 0x0000
 isColor2: db 0 ; bolean for hit of color 1
 isColor1: dw 0 ; bolean for hit of color 2
 is_win: db 0
 is_death: db 0
 
 
 Ecord1: dw 3446,3284,3288,3444,3448,3606,3604,3608,3764,3768 
 Ecord2: dw 3406,3244,3248,3566,3564,3568,3724,3728
 E1updated: dw 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 ; array of 15 size that always contain updated values of enemies
 E2updated: dw 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 BossUpdated: times 85 dw 0 
 Eatr1: dw 0x31db,0x31aa,0x31a9,0x30e9,0x30e9,0x31db,0x31db,0x31db,0x3121,0x3121
 Eatr2: dw 0x00db,0x30aa,0x30a9,0x30db,0x312a,0x312a,0x305b,0x305d
 Bosscord: dw 200,202,204,206,208,210,212,358,360,362,364,366,368,370,372,374,516,518,520,522,524,526,528,530,532,534,676,678,680,682,684,686,688,690,692,694,840,842,844,846,848,850,852,1008,1006,1152,1154,1156,1158,1164,1166,1168,1170,1176,1178,1180,1182,1312,1314,1316,1318,1320,1322,1324,1326,1328,1330,1332,1334,13336,1338,1340,1342,1476,1484,1486,1488,1490,1498,1634,1636,1638,1656,1658,1660
 Bossatr: dw 0x01db,0x01db,0x01db,0x01db,0x01db,0x02db,0x02db,0x01db,0x01db,0x01db,0x01db,0x01db,0x01db,0x01db,0x02db,0x02db,0x00db,0x00db,0x00db,0x00db,0x00db,0x00db,0x00db,0x01db,0x01db,0x02db,0x04db,0x04db,0x04db,0x04db,0x04db,0x04db,0x00db,0x02db,0x01db,0x01db,0x01db,0x01db,0x01db,0x01db,0x02db,0x02db,0x01db,0x00db,0x00db,0x02db,0x02db,0x02db,0x01db,0x02db,0x02db,0x01db,0x01db,0x02db,0x01db,0x02db,0x01db,0x02db,0x02db,0x01db,0x01db,0x00db,0x00db,0x01db,0x84db,0x84db,0x02db,0x00db,0x00db,0x01db,0x01db,0x02db,0x02db,0x00db,0x01db,0x01db,0x01db,0x02db,0x00db,0x81db,0x81db,0x82db,0x82db,0x81db,0x81db
 wincoord: dw 1656,1816,1976,2136,2296,2298,2306,1982,2140,2144,1668,1828,1988,2148,2308,1676,1836,1996,2156,2316,1684,1844,2004,2164,2324,1846,2008,2170,1692,1852,2012,2172,2332
 deadcoord: dw 1652,1812,1972,2132,2292,1654,1656,1818,1978,2138,2296,2294,1666,1826,1986,2146,2306,1668,1670,1988,2308,2310,1682,1840,1998,2158,2318,1844,2006,2166,2326,2164,2162,2160,1696,1694,1854,2014,2174,2336,2334,1698,1860,2020,2180,2338
 suncoord: dw  140,142,144,146,148,150,152,154,156,158,300,302,304,306,308,310,312,314,316,318,462,464,466,468,470,472,474,476,478,622,624,626,628,630,632,634,636,638,784,786,788,790,792,794,796,798,946,948,950,952,954,956,958,1108,1110,1112,1114,1116,1118,1270,1272,1274,1276,1278
 sunattr: dw 0x06db,0x0edb,0x0edb,0x0edb,0x0edb,0x0edb,0x0edb,0x0edb,0x0edb,0x0edb,0x06db,0x0edb,0x0edb,0x0edb,0x0edb,0x0edb,0x0edb,0x0edb,0x0edb,0x0edb,0x06db,0x0edb,0x0edb,0x0edb,0x0edb,0x0edb,0x0edb,0x0edb,0x0edb,0x06db,0x0edb,0x0edb,0x0edb,0x0edb,0x0edb,0x0edb,0x0edb,0x0edb,0x06db,0x0edb,0x0edb,0x0edb,0x0edb,0x0edb,0x0edb,0x0edb,0x06db,0x0edb,0x0edb,0x0edb,0x0edb,0x0edb,0x0edb,0x06db,0x0edb,0x0edb,0x0edb,0x0edb,0x0edb,0x06db,0x06db,0x06db,0x06db,0x06db
 live: dw 2
 
 E1index: dw 0
 E2index: dw 0 
 bossIndex: dw 0
 emergIndex: dw 1670 ;emergence index for object
 ballIndex: dw 1670
 ballMove: dw 160
 E1total: dw 0 ; 12 MOVements 
 E2total: dw 0 ; 8 MOVements
 BossTotal: dw 0 ; 8 MOVements
 
 jumpSize: dw 6
 alt1: db 85 ; special one byte number that contains alternative bits , it is used to generate alternative 0  1 bits .01010101 . ror 1-> C , jc --> if c==1 jnc c==0 
 alt2: db 85 ; special one byte number that contains alternative bits , it is used to generate alternative 0  1 bits .
 alt3: db 85 ; special one byte number that contains alternative bits , it is used to generate alternative 0  1 bits .
 
 
 E1dir: db 0 ; direction flags for enemy 1
 E2dir: db 0 ; direction flags for enemy 2
 BossDir: db 0 ; direction flags for enemy 2
 stackPtr: dw 0 
 delayCount: dw 0xffff
 background: dw 0x03db
 
 
 str1 : db 'Q U I T'
 str2 : db 'P L A Y' 
 str3 : db 'Press Esc to Quit'
 str4 : db 'Press Any Key to Play'
 str5 : db 'L I V E S'