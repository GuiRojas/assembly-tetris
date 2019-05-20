      .386
      .model flat, stdcall
      option casemap :none

; #########################################################################

      include \masm32\include\windows.inc

      include \masm32\include\user32.inc
      include \masm32\include\kernel32.inc
      include \masm32\include\gdi32.inc
      include \masm32\include\msimg32.inc

      includelib \masm32\lib\user32.lib
      includelib \masm32\lib\kernel32.lib
      includelib \masm32\lib\gdi32.lib
      includelib \masm32\lib\msimg32.lib

; #########################################################################

; MACROS:

      szText MACRO Name, Text:VARARG
        LOCAL lbl
          jmp lbl
            Name db Text,0
          lbl:
        ENDM
;
      m2m MACRO M1, M2
        push M2
        pop  M1
      ENDM
;
      return MACRO arg
        mov eax, arg
        ret
      ENDM

; #########################################################################

; METODOS

        WinMain PROTO :DWORD,:DWORD,:DWORD,:DWORD
        WndProc PROTO :DWORD,:DWORD,:DWORD,:DWORD
        TopXY PROTO   :DWORD,:DWORD

        Paint_Proc    PROTO :DWORD, :DWORD

; #########################################################################

;CONSTANTES

        desenho1 equ  100
        desenho2 equ  101
        mina     equ  102

        CREF_TRANSPARENT  EQU 0FF00FFh
        CREF_TRANSPARENT2 EQU 0FF0000h

        ID_TIMER  EQU 1
        TIMER_MAX EQU 100

    .data
        szDisplayName db "Primeiro Programa",0
        CommandLine   dd 0
        hWnd          dd 0
        hInstance     dd 0

        hBmpDesenho1  dd 0

        posicaoPeca   dd 0
        tipoPeca      db 0

        timerDesce    dd 0
        
        matrix      db  4*4*2   dup(0)  
        matx          db 0
        maty          db 0
        matval        db 1 dup (?)

        posX    dd 128
        posY    dd 32

        posX1    dd 0
        posY1    dd 0

        posX2    dd 0
        posY2    dd 0

        posX3    dd 0
        posY3    dd 0

    .data?
        iTimer  dd ?
        posAux  dd ?


; #########################################################################

    .code

start:
    invoke GetModuleHandle, NULL ; provides the instance handle
    mov hInstance, eax

    invoke LoadBitmap, hInstance, mina
    mov hBmpDesenho1, eax


    invoke GetCommandLine        ; provides the command line address
    mov CommandLine, eax

    invoke WinMain,hInstance,NULL,CommandLine,SW_SHOWDEFAULT
    
    invoke ExitProcess,eax       ; cleanup & return to operating system

; #########################################################################

igualaBlocos proc uses eax
  mov eax, posX
  mov posX1, eax
  mov posX2, eax
  mov posX3, eax
  mov eax, posY
  mov posY1, eax
  mov posY2, eax
  mov posY3, eax

  ret  
igualaBlocos endp

getMult32 proc uses edx eax bx
xor     edx, edx
mov     eax, posAux
mov     bx, 32
div     bx
mov     posAux, eax
ret
getMult32 endp 

;////////// tutorial matriz /////////////////////////////////////////////////
;a matriz tem tamanho 10 no X e 25 no Y
;para usar os procedimentos, basta pensar:
;matriz(x,y)  --->
;mov matx, X    +   mov maty, Y
;valores usam a var matval
;exemplos de uso:
;SETTAR VALOR          GETTAR VALOR         <--------------------------------
;mov matx,2            mov matx,2
;mov maty,4            mov maty,4
;mov matval, 80        call getMatriz
;call setMatriz        mov al, matval
;/////////////////////////////////////////////////////////////////////////////
getMatriz proc uses ax cx
    xor eax, eax            ;limpa registrador
    mov al, maty            ;move valor Y
    mov cx, 4               ;move tamanho da linha
    mul cx                  ;multiplica os valores (anda pela maior dimensão do vetor)
    xor cx,cx               ;limpa cx
    mov cl, matx            ;move valor X
    add ax, cx              ;soma à posição final
    mov edi, OFFSET matrix  ;move pro EDI a posição da memória da matriz
    add edi, eax            ;soma pra posição da matriz a posição desejada
    mov al, byte ptr[edi]   ;coloca o valor da posição no al
    mov matval, al          ;move al pra variavel desejada
    ret                     ;fim
getMatriz endp

setMatriz proc uses ax cx
    xor eax, eax            ;limpa registrador
    mov al, maty            ;move valor Y
    mov cx, 4               ;move tamanho da linha
    mul cx                  ;multiplica os valores (anda pela maior dimensão do vetor)
    xor cx,cx               ;limpa cx
    mov cl, matx            ;move valor X
    add ax, cx              ;soma à posição final
    mov edi, OFFSET matrix  ;move pro EDI a posição da memória da matriz
    add edi, eax            ;soma pra posição da matriz a posição desejada
    ;coloca o valor da matriz no edi
    mov al, matval
    mov byte ptr[edi], al
    ret      
setMatriz endp

WinMain proc hInst     :DWORD,
             hPrevInst :DWORD,
             CmdLine   :DWORD,
             CmdShow   :DWORD

        LOCAL wc   :WNDCLASSEX
        LOCAL msg  :MSG

        LOCAL Wwd  :DWORD
        LOCAL Wht  :DWORD
        LOCAL Wtx  :DWORD
        LOCAL Wty  :DWORD

        szText szClassName,"Primeiro_Class"

        mov wc.cbSize,         sizeof WNDCLASSEX
        mov wc.style,          CS_HREDRAW or CS_VREDRAW \
                               or CS_BYTEALIGNWINDOW
        mov wc.lpfnWndProc,    offset WndProc      ; address of WndProc
        mov wc.cbClsExtra,     NULL
        mov wc.cbWndExtra,     NULL
        m2m wc.hInstance,      hInst               ; instance handle
        mov wc.hbrBackground,  COLOR_BTNFACE+1     ; system color
        mov wc.lpszMenuName,   NULL
        mov wc.lpszClassName,  offset szClassName  ; window class name
          invoke LoadIcon,hInst,500    ; icon ID   ; resource icon
        mov wc.hIcon,          eax
          invoke LoadCursor,NULL,IDC_ARROW         ; system cursor
        mov wc.hCursor,        eax
        mov wc.hIconSm,        0

        invoke RegisterClassEx, ADDR wc     ; register the window class

        ;================================
        ; Centre window at following size
        ;================================

        mov Wwd, 340
        mov Wht, 618 

        invoke GetSystemMetrics,SM_CXSCREEN ; get screen width in pixels
        invoke TopXY,Wwd,eax
        mov Wtx, eax

        invoke GetSystemMetrics,SM_CYSCREEN ; get screen height in pixels
        invoke TopXY,Wht,eax
        mov Wty, eax

        ; ==================================
        ; Create the main application window
        ; ==================================
        invoke CreateWindowEx,WS_EX_OVERLAPPEDWINDOW,
                              ADDR szClassName,
                              ADDR szDisplayName,
                              WS_OVERLAPPEDWINDOW,
                              Wtx,Wty,Wwd,Wht,
                              NULL,NULL,
                              hInst,NULL

        mov   hWnd,eax  ; copy return value into handle DWORD

        invoke LoadMenu,hInst,600                 ; load resource menu
        invoke SetMenu,hWnd,eax                   ; set it to main window

        invoke ShowWindow,hWnd,SW_SHOWNORMAL      ; display the window
        invoke UpdateWindow,hWnd                  ; update the display

      ;===================================
      ; Loop until PostQuitMessage is sent
      ;===================================

    StartLoop:
      invoke GetMessage,ADDR msg,NULL,0,0         ; get each message
      cmp eax, 0                                  ; exit if GetMessage()
      je ExitLoop                                 ; returns zero
      invoke TranslateMessage, ADDR msg           ; translate it
      invoke DispatchMessage,  ADDR msg           ; send it to message proc
      jmp StartLoop
    ExitLoop:

      return msg.wParam

WinMain endp

; #########################################################################

WndProc proc hWin   :DWORD,
             uMsg   :DWORD,
             wParam :DWORD,
             lParam :DWORD

        LOCAL Ps  :PAINTSTRUCT
        LOCAL hDC :DWORD   ;handle do dispositivo

; ########################################################################
    .if uMsg == WM_COMMAND
  
    ;======== menu commands ========

        

    ;====== end menu commands ======

    .elseif uMsg == WM_PAINT

      invoke  BeginPaint, hWin, ADDR Ps
      mov     hDC, eax
      invoke  Paint_Proc, hWin, hDC
      invoke  EndPaint, hWin, ADDR Ps

; ########################################################################

    .elseif uMsg == WM_CREATE

      invoke  SetTimer, hWin, ID_TIMER, TIMER_MAX, NULL
      mov     iTimer, eax

; ########################################################################

    .elseif uMsg == WM_KEYUP

      .if wParam == VK_UP
        dec   posicaoPeca

        .if posicaoPeca == -1
          mov posicaoPeca, 3
        .endif

      .elseif wParam == VK_DOWN
        inc   posicaoPeca

        .if posicaoPeca == 4
          mov posicaoPeca, 0
        .endif
        
      .elseif wParam == VK_RIGHT

        .if posX < 288 && posX1 < 288 && posX2 < 288 && posX3 < 288
          add posX, 32
        .endif       
        
        
      .elseif wParam == VK_LEFT

        .if posX > 0 && posX1 > 0 && posX2 > 0 && posX3 > 0
          sub posX, 32
        .endif  

      .endif

; ########################################################################

    .elseif uMsg == WM_TIMER ;TIMER

      invoke  KillTimer, hWin, iTimer
      inc timerDesce

      .if timerDesce == 3
        mov timerDesce, 0
        add posY, 32
      .endif

      .if posY >= 544 || posY1 >= 544 || posY2 >= 544 || posY3 >= 544
        invoke  InvalidateRect, hWin, NULL, FALSE

        invoke  BeginPaint, hWin, ADDR Ps
        mov     hDC, eax
        invoke  EndPaint, hWin, ADDR Ps
        mov   posY, 32
        mov   posX, 128

      .endif
      
      invoke  InvalidateRect, hWin, NULL, TRUE
      
      invoke  SetTimer, hWin, ID_TIMER, TIMER_MAX, NULL
      mov     iTimer, eax

; ########################################################################

    .elseif uMsg == WM_CLOSE

        invoke  KillTimer, hWin, iTimer

; ########################################################################

    .elseif uMsg == WM_DESTROY

        invoke PostQuitMessage,NULL
        return 0 
    .endif

    invoke DefWindowProc,hWin,uMsg,wParam,lParam

    ret

WndProc endp

; ########################################################################

TopXY proc wDim:DWORD, sDim:DWORD

  ; ----------------------------------------------------
  ; This procedure calculates the top X & Y co-ordinates
  ; for the CreateWindowEx call in the WinMain procedure
  ; ----------------------------------------------------

  shr sDim, 1      ; divide screen dimension by 2
  shr wDim, 1      ; divide window dimension by 2
  mov eax, wDim    ; copy window dimension into eax
  sub sDim, eax    ; sub half win dimension from half screen dimension

  return sDim

TopXY endp

; ########################################################################

Paint_Proc proc hWin:DWORD, hDC:DWORD

  LOCAL hOld:DWORD
  LOCAL memDC:DWORD

  invoke  CreateCompatibleDC, hDC
  mov     memDC, eax

  invoke SelectObject, memDC, hBmpDesenho1
  mov     hOld, eax
dnv:
  .if posicaoPeca == 0
    jmp direita

  .elseif posicaoPeca == 1
    jmp cima

  .elseif posicaoPeca == 2
    jmp esquerda

  .elseif posicaoPeca == 3
    jmp baixo

  .endif
  
esquerda:
  ; 1
  ;2X
  ; 3
  call igualaBlocos
  sub posY1, 32
  sub posX2, 32
  add posY3, 32

  jmp fimA

baixo:
  ;1X2
  ; 3
  call igualaBlocos
  sub posX1, 32
  add posX2, 32
  add posY3, 32

  jmp fimA

cima:
  ; 2
  ;1X3
  call igualaBlocos
  sub posX1, 32
  sub posY2, 32
  add posX3, 32

  jmp fimA

direita:
  ; 1
  ; X2
  ; 3
  call igualaBlocos
  sub posY1, 32
  add posX2, 32
  add posY3, 32
fimA: 
  ;           0-32 = V
  .if posX >= 1989214176 || posX1 >= 1989214176 || posX2 >= 1989214176 || posX3 >= 1989214176 
    add posX, 32
    jmp dnv
  .endif
  .if posX > 288 || posX1 > 288 || posX2 > 288 || posX3 > 288
    sub posX, 32
    jmp dnv
  .endif
  invoke TransparentBlt, hDC, posX,  posY,  32, 32, memDC, 0, 160, 32, 32, TRUE
  invoke TransparentBlt, hDC, posX1, posY1, 32, 32, memDC, 0, 160, 32, 32, TRUE
  invoke TransparentBlt, hDC, posX2, posY2, 32, 32, memDC, 0, 160, 32, 32, TRUE
  invoke TransparentBlt, hDC, posX3, posY3, 32, 32, memDC, 0, 160, 32, 32, TRUE

  invoke SelectObject, hDC, hOld

  invoke DeleteDC, memDC

  return 0

Paint_Proc endp

end start