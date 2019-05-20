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

    .data?
        iTimer  dd ?
        posX    dd ?
        posY    dd ?


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

        mov Wwd, 500
        mov Wht, 350

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

        .if wParam == 1000
            invoke SendMessage,hWin,WM_SYSCOMMAND,SC_CLOSE,NULL
        .elseif wParam == 1900
            szText TheMsg,"Assembler, Puro & Simples"
            invoke MessageBox,hWin,ADDR TheMsg,ADDR szDisplayName,MB_OK
        .endif

    ;====== end menu commands ======

    .elseif uMsg == WM_PAINT

      invoke  BeginPaint, hWin, ADDR Ps
      mov     hDC, eax
      invoke  Paint_Proc, hWin, hDC
      invoke  EndPaint, hWin, ADDR Ps

; ########################################################################

    .elseif uMsg == WM_CREATE

      mov     posX, 10
      mov     posY, 10

      invoke  SetTimer, hWin, ID_TIMER, TIMER_MAX, NULL
      mov     iTimer, eax

; ########################################################################

    .elseif uMsg == WM_KEYUP

      .if wParam == VK_LEFT
        dec   posicaoPeca

        .if posicaoPeca == -1
          mov posicaoPeca, 3
        .endif

      .elseif wParam == VK_RIGHT
        inc   posicaoPeca

        .if posicaoPeca == 4
          mov posicaoPeca, 0
        .endif

      .endif

; ########################################################################

    .elseif uMsg == WM_TIMER ;TIMER

      invoke  KillTimer, hWin, iTimer
      inc     posX
      add     posY, 2

      .if posY == 202 

        invoke  InvalidateRect, hWin, NULL, FALSE

        invoke  BeginPaint, hWin, ADDR Ps
        mov     hDC, eax
        invoke  EndPaint, hWin, ADDR Ps
        mov   posY, 10

      .elseif

      invoke  InvalidateRect, hWin, NULL, TRUE

      .endif

      
      invoke  SetTimer, hWin, ID_TIMER, TIMER_MAX, NULL
      mov     iTimer, eax

; ########################################################################

    .elseif uMsg == WM_CLOSE

        invoke  KillTimer, hWin, iTimer

        szText TheText,"Voce deseja mesmo sair?"
        invoke MessageBox,hWin,ADDR TheText,ADDR szDisplayName,MB_YESNO
          .if eax == IDNO
            return 0
          .endif

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

  .if posicaoPeca == 0
    jmp direita

  .elseif posicaoPeca == 1
    jmp cima

  .elseif posicaoPeca == 2
    jmp esquerda

  .elseif posicaoPeca == 3
    jmp baixo

  .endif
  
direita:
invoke TransparentBlt, hDC, 42, posY, 32, 32, memDC, 0, 160, 32, 32, TRUE
    
  add posY, 32
  invoke TransparentBlt, hDC, 10, posY, 32, 32, memDC, 0, 160, 32, 32, TRUE
  invoke TransparentBlt, hDC, 42, posY, 32, 32, memDC, 0, 160, 32, 32, TRUE
  add posY, 32
  invoke TransparentBlt, hDC, 42, posY, 32, 32, memDC, 0, 160, 32, 32, TRUE
  sub posY, 32
  sub posY, 32

  jmp fimA

baixo:
  invoke TransparentBlt, hDC, 42, posY, 32, 32, memDC, 0, 160, 32, 32, TRUE
    
  add posY, 32
  invoke TransparentBlt, hDC, 10, posY, 32, 32, memDC, 0, 160, 32, 32, TRUE
  invoke TransparentBlt, hDC, 42, posY, 32, 32, memDC, 0, 160, 32, 32, TRUE
  invoke TransparentBlt, hDC, 74, posY, 32, 32, memDC, 0, 160, 32, 32, TRUE
  sub posY, 32

  jmp fimA

cima:
  invoke TransparentBlt, hDC, 10, posY, 32, 32, memDC, 0, 160, 32, 32, TRUE
  invoke TransparentBlt, hDC, 42, posY, 32, 32, memDC, 0, 160, 32, 32, TRUE
  invoke TransparentBlt, hDC, 74, posY, 32, 32, memDC, 0, 160, 32, 32, TRUE
  
  add posY, 32
  invoke TransparentBlt, hDC, 42, posY, 32, 32, memDC, 0, 160, 32, 32, TRUE
  sub posY, 32

  jmp fimA

esquerda:
  invoke TransparentBlt, hDC, 42, posY, 32, 32, memDC, 0, 160, 32, 32, TRUE
    
  add posY, 32
  invoke TransparentBlt, hDC, 74, posY, 32, 32, memDC, 0, 160, 32, 32, TRUE
  invoke TransparentBlt, hDC, 42, posY, 32, 32, memDC, 0, 160, 32, 32, TRUE
  add posY, 32
  invoke TransparentBlt, hDC, 42, posY, 32, 32, memDC, 0, 160, 32, 32, TRUE
  sub posY, 32
  sub posY, 32

fimA:
  invoke SelectObject, hDC, hOld

  invoke DeleteDC, memDC

  return 0

Paint_Proc endp

end start