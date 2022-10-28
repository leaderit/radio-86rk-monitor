;============================================================================
; RADIO-86RK (SOVIET 8-BIT COMPUTER)
; THE MONITOR ROM source recovered from ROM Binaries and well decribed for future using
; by Valerii Grazhdankin, (c) 2022
;
; The Monitor is a system programm with a command line interface, 
; booted after System On or Reset.
; Monitor is placed in system's Read-Only Memory (ROM)
;
; Monitor Commands
;
;     Memory Operations
;         D<Start_Address>,<End_Address> - Display memory content in hexadecimal
;         L<Start_Address>,<End_Address> - Display memory content in ASCII
;         F<Start_Address>,<End_Address>,<Value> - Fill memory with the specified value
;         M<Address> - Modify memory content
;         T<Start_Address>,<End_Address>,<Destination_Start_Address> - Copy memory block to destination
;         C<Start_Address>,<End_Address>,<Destination_Start_Address> - Compare memory block with destination
;         S<Start_Address>,<End_Address>,<Value> - Search memory for a value
;         R<ROM_Start_Address>,<ROM_End_Address>,<Destination_Start_Address> - Read from the ROM connected to the Parallel interface to memory
;
;     Run Control
;         G<Start_Address>[,End_Address] - Run code, optionally stop at the specified address
;         X - Display and modify registers
;
;     Cassette Input/Output
;         O<Start_Address>,<End_Address>[,Speed] - Write memory to cassette. Default speed is 1Dh / 1200 bps
;         I[Offset][,Speed] - Read data from a tape to memory at the specified offset
;
;         Monitor's format of a tape record:
;           0xE6  - Syncronization byte
;           XX XX - Start address of memory allocation, High byte First
;           XX XX - End address of memory allocation, High byte First
;           ..... - Data of the file. Size of the file is END_ADDR - START_ADDR
;           00 00 - Junk bytes, may be absant
;           0xE6  - Syncronization byte
;           XX XX - Check summ, High byte First
;============================================================================

; ---------------------------------------------------------------------------
.IFNDEF MEM_SIZE
.DEF MEM_SIZE   4000h
.ENDIF

.IFNDEF GPIO_PORT
.DEF GPIO_MEM
;
.MACRO IN_P ARGS PORT
    lda PORT
.ENDM
;
.MACRO OUT_P ARGS PORT
    sta PORT
.ENDM

.ENDIF

; ---------------------------------------------------------------------------

; System Configuration Constants
.DEF SCREEN_MEM_GAP         12      ; 16K: TOP - (2352  = 930h)
.DEF SCREEN_SIZE            2340    ; 924h
.DEF SCREEN_BUF_ADR         MEM_SIZE - SCREEN_MEM_GAP - SCREEN_SIZE ; 16K = 36D0h
.DEF SCREEN_BUF_LEN         4923h
.DEF SCREEN_BUF_ADDR_L      lobyte(SCREEN_BUF_ADR) 
.DEF SCREEN_BUF_ADDR_H      hibyte(SCREEN_BUF_ADR) 
.DEF SCREEN_BUF_LEN_L       lobyte(SCREEN_BUF_LEN) 
.DEF SCREEN_BUF_LEN_H       hibyte(SCREEN_BUF_LEN) 

; Stack Address
.DEF STACK_SIZE             6Fh
.DEF STACK_ADDR             SCREEN_BUF_ADR-1    ; 16K=36CFh
; Memory mapped Ports addresses
.DEF PPI_KBD_ADDR           08000h
.DEF PPI_EXT_ADDR           0A000h
.DEF CRT_ADDR               0C000h
.DEF DMA_ADDR               0E000h
; ROMS
.DEF ROM_EXT_ADDR           0F000h
.DEF ROM_ADDR               0F800h
; ROM Varoables
.DEF VARS_SIZE              60h
.DEF VARS_ADDR              STACK_ADDR-STACK_SIZE-VARS_SIZE ; 16K=3600h
; BUFFERS
.DEF LINE_BUF_SIZE        32

; I/O Modes
.DEF PPI_KBD_MODE           8Ah
.DEF PPI_EXP_ROMDISK_MODE   90h     ; A - Input = Data, B, C - Output = Address 16 bit

; CRT Commands and Flags
.DEF CRT_CMD_SETCURSOR      80h
.DEF CRT_READY_FLAG         20h

; DMA Modes
.DEF DMA_MODE_STOP          80h
.DEF DMA_MODE_START         0A4h

; PPI_KBD.PORT_C Controll Keys Masks
.DEF KEY_CC_MASK            20h
.DEF KEY_UC_MASK            40h
.DEF KEY_RUSLAT_MASK        80h

; Scan Key Constants
.DEF KEY_DEBOUNCE_DELAY     20h


; Key codes
.DEF KEY_LEFT               8
.DEF KEY_BACKSPACE          7Fh
.DEF KEY_ENTER              0Dh
.DEF KEY_SPACE              20h     ; ' '
.DEF KEY_COMA               2Ch     ; ','


; Default Variable Values
.DEF TAPE_RD_DEFAULT        2Ah
.DEF TAPE_WR_DEFAULT        1Dh     ; Default speed is 1Dh / 1200 bps
.DEF TAPE_WR_CORRECTION     0Eh     ; Substract from a Tape Write Constant

.DEF TAPE_SYNC_BYTE         0E6h    ; Writed before Header and After Traler 

; Debug codes
.DEF JMP_OPCODE             0C3h
.DEF RTS_6_OPCODE           0F7h
.DEF RTS_6_ADDR             30h 

; Definitions of Commands constants
.DEF CMD_EDIT_REGISTERS     'X'
.DEF CMD_RUN_ROM_EXT        'U'

; ---------------------------------------------------------------------------

.MEMORYMAP
    DEFAULTSLOT 6
    ; RAM Area
    SLOT 0 START $0000 SIZE MEM_SIZE    NAME "RAM"
    ; RAM Mapped I/O Ports Area
    SLOT 1 START PPI_KBD_ADDR   SIZE $4000       NAME "PPI_KBD"
    SLOT 2 START PPI_EXT_ADDR   SIZE $4000       NAME "PPI_EXT"
    SLOT 3 START CRT_ADDR       SIZE $4000       NAME "CRT"
    SLOT 4 START DMA_ADDR       SIZE $2000       NAME "DMA"
    ; ROM area
    SLOT 5 START $F000          SIZE $800        NAME "ROM_EXT"
    SLOT 6 START $F800          SIZE $800        NAME "ROM_MONITOR"
.ENDME

; ROM Banks
.ROMBANKMAP
    BANKSTOTAL 1
    BANKSIZE $800
    BANKS 1
.ENDRO

.EMPTYFILL 0FFh

; ---------------------------------------------------------------------------

; PPI 8255/580VV55
.STRUCT  PPI_8255                               
    PORT_A:         db
    PORT_B:         db
    PORT_C:         db
    CFG:            db
.ENDST

; ---------------------------------------------------------------------------

.STRUCT DMA_8257 
    CH0_ADDR:       db 
    CH0_LEN:        db 
    CH1_ADDR:       db 
    CH1_LEN:        db 
    CH2_ADDR:       db                   
    CH2_LEN:        db 
    CH3_ADDR:       db 
    CH3_LEN:        db 
    CFG:            db                   
.ENDST

; ---------------------------------------------------------------------------

.struct CRT_8275 
    DATA:           db 
    CFG:            db 
.endst

; ---------------------------------------------------------------------------

.STRUCT MON_VARS
    unk_3600:           dw
    CURSOR_POS:         dw
    PRINT_CHAR_MODE:    db
.ENDST

; ---------------------------------------------------------------------------

.STRUCT SCREEN_BUF
    BUF:   ds SCREEN_SIZE
.ENDST

; ---------------------------------------------------------------------------
; RAM Variables
; ---------------------------------------------------------------------------

.SLOT 0
.ORG 0

; RK86-16K = 3600h
.ENUM VARS_ADDR
    VARS:                   dw                      ; 
    CURSOR_POS:             dw                      ; 
    PRINT_CHAR_MODE:        db                      ; 
    KEY_READED:             db                      ; 
    RUSLAT_FLAG:            ds 3                    ; 
    KEY_CODE:               dw                      ; 
    KEY_FIRST_TIME_FLAG:    dw                      ; 1 = First pressed, 0 = Auto Repeated
    TAPE_READ_STACK_SAVED:  ds 7                    ; 
    DEBUG_REG_PC:           dw                      ; Break Address Saved from Stack after RST 6
    DEBUG_REG_HL:           dw                      ; The Debug Value of HL Register 
    DEBUG_REG_BC:           dw                      ; The Debug Value of BC Register 
    DEBUG_REG_DE:           dw                      ; The Debug Value of DE Register 
    DEBUG_REG_SP:           dw                      ; The Debug Value of SP Register 
    DEBUG_REG_PSW:          dw                      ; The Debug Value of PSW Register 
    DEBUG_REG_Unknown:      ds 3                    ; DATA XREF: ROM:FFA7↓w
    DEBUG_STOP_ADDR:        dw                      ; In Command 'Go' Stop Address Saved Here
    DEBUG_STOP_ADDR_VALUE:  db                      ; 'Go' Stop Address Value Saved
    JMP_TO:                 db                      ; Default value = C3h, JMP command code
    ARG_1:                  dw                      ; Command Argument 1
    ARG_2:                  dw                      ; Command Argument 2
    ARG_3:                  dw                      ; Command Argument 3
    IS_ARG_3:               db                      ; 0 - no data in ARG_3, FFh - ARG_3 has a data
    TAPE_READ_ERR:          db                      ; 0 - read OK, 0FFh - read error
    TAPE_RD_CONST:          db                      ; Tape Read delay constant
    TAPE_WR_CONST:          db                      ; Tape Write delay constant
    MEM_AVAILABLE:          dw                      ; Address of the Top of Available Memory
    INPUT_LINE_BUF:         ds LINE_BUF_SIZE        ; Input Line Buffer               
.ENDE

; ---------------------------------------------------------------------------

.ENUM STACK_ADDR-STACK_SIZE ; 16K = 3660h, SIZE = 70h
STACK:      ds STACK_SIZE
.ENDE

; ---------------------------------------------------------------------------

.ENUM SCREEN_BUF_ADR ; 16K = 36D0h
    SCREEN INSTANCEOF SCREEN_BUF 
    SCREEN_END:       db     
.ENDE

; ---------------------------------------------------------------------------

; ---------------------------------------------------------------------------
; end of 'RAM'
; ---------------------------------------------------------------------------

; ===========================================================================

.RAMSECTION "PPI_KBD" SLOT "PPI_KBD" ;ORGA PPI_KBD_ADDR
PPI_KBD INSTANCEOF PPI_8255            
.ENDS

; ===========================================================================

.RAMSECTION "PPI_EXT" SLOT "PPI_EXT" ;ORGA PPI_EXT_ADDR
PPI_EXP INSTANCEOF PPI_8255
.ENDS

; ===========================================================================

.RAMSECTION "CRT" SLOT "CRT" ;ORGA CRT_ADDR
CRT INSTANCEOF CRT_8275     
.ENDS

; ===========================================================================

.RAMSECTION "DMA" SLOT "DMA" ;ORGA DMA_ADDR
DMA INSTANCEOF DMA_8257   
.ENDS

; ---------------------------------------------------------------------------
; ===========================================================================

.BANK 0 SLOT 6 
.ORGA ROM_ADDR  ; $F800
.ORG 0

COLD_START:
                ; jmp     SCREEN.BUF
; ---------------------------------------------------------------------------
; OF800h - Entry point after reset                
                jmp     COLD_INIT              
; ---------------------------------------------------------------------------
; OF803h - Keyboard input 
;   Input : None
;   Output: A - character code read from keyboard
                jmp     INPUT_KEY               ;
; ---------------------------------------------------------------------------
; 0F806Н - Read a byte from a tape 
;   Input : A=0FFh - with sync; A=08h - no sync
;   Output: A - data read from cassette
                jmp     TAPE_READ_BYTE          ;
; ---------------------------------------------------------------------------
; 0F809h - Print character to screen 
;   Input : C - character to print
;   Output: None
                jmp     PRINT_CHAR              ;
; ---------------------------------------------------------------------------
; 0F80Ch - Write a byte onto a tape 
;   Input : C - data to write
;   Output: None
                jmp     TAPE_WRITE_BYTE         ;
; ---------------------------------------------------------------------------
; Print character to screen (Duplicated entry)
;   Input : C - character to print
;   Output: None
                jmp     PRINT_CHAR              ;
; ---------------------------------------------------------------------------
; 0F812h - Query keyboard 
;   Input : None
;   Output: A=00h - key not pressed; A=0FFh - key pressed
                jmp     KEY_STATUS              ;
; ---------------------------------------------------------------------------
; 0F15h - Print to screen in hex 
;   Input: A - data to print
;   Output: None
                jmp     PRINT_HEX               ;
; ---------------------------------------------------------------------------
; 0F18h - Print string 
;   Input: HL - string address
;   Output: None
                jmp     PRINT_STRING            ;
; ---------------------------------------------------------------------------
; 0F1Bh - Get key
;   Input : None
;   Output: A=0FFh - key not pressed; A=0FEh - Rus/Lat; otherwise A - key code
                jmp     GET_KEY                 ;
; ---------------------------------------------------------------------------
; 0F81Eh - Get cursor position
;   Input : None
;   Output: H - row, L - column
                jmp     GET_CURSOR_POSITION     ;
; ---------------------------------------------------------------------------
; 0F821h - Read character from screen at current cursor position
;   Input : None
;   Output: A - char read from screen
                jmp     READ_VIDEO_RAM          ;
; ---------------------------------------------------------------------------
; 0F824h - Read from a tape
;   Input: HL - offset of memory buffer to store data to
;   Output: HL - start, DE - end, BC - checksum

                jmp     TAPE_READ_BLOCK         ;
; ---------------------------------------------------------------------------
; 0F27h - Write block to a tape
;   Input: HL - start, DE - end of memory block
;   Output:  BC - checksum
                jmp     TAPE_WRITE_BLOCK        ;
; ---------------------------------------------------------------------------
; 0F82Ah - Calculate Check Summ
;   Input: HL - start; DE - end;
;   Output: BC - checksum
                jmp     CALC_CHECK_SUMM         ;
; ---------------------------------------------------------------------------
; 0F82Dh - Init Display
; Input: None
; Output: None
                jmp     INIT_DISPLAY_REFRESH    ;
; ---------------------------------------------------------------------------
; 0F830h - Get memory heap
;   Input: None
;   Output: HL - Address of highest address of memory, avalable for a user's programm

                jmp     GET_FREE_MEM_ADDR       ;
; ---------------------------------------------------------------------------
; 0F833h - Set memory heap
;   Input: HL - Address of highest address of memory, avalable for a user's programm
;   Output:  None

                jmp     SET_FREE_MEM_ADDR       ;
; ---------------------------------------------------------------------------

COLD_INIT:                      
                mvi     a, PPI_KBD_MODE
                sta     PPI_KBD.CFG
                lxi     sp, STACK_ADDR
                call    INIT_DISPLAY_REFRESH
                lxi     h, VARS_ADDR
                lxi     d, VARS_ADDR + VARS_SIZE - 1
                mvi     c, 0
                call    CMD_F                   ; Clear Vars
                lxi     h, STACK_ADDR
                shld    DEBUG_REG_SP
                lxi     h, HELLO_STR
                call    PRINT_STRING
                call    INIT_DISPLAY_REFRESH
                lxi     h, VARS_ADDR - 1
                shld    MEM_AVAILABLE
                lxi     h, ( TAPE_WR_DEFAULT << 8 | TAPE_RD_DEFAULT )
                shld    TAPE_RD_CONST 
                mvi     a, JMP_OPCODE            
                sta     JMP_TO
PRINT_PROMPT:                              
                lxi     sp, STACK_ADDR
                lxi     h, PROMPT_STR   
                call    PRINT_STRING
                ; A=0 here after return
                sta     PPI_KBD.PORT_C
                dcr     a
                ; 0FFh -> PPI_EXP.PORT_C
                sta     PPI_EXP.PORT_C
                call    INPUT_LINE
                lxi     h, PRINT_PROMPT
                push    h
                lxi     h, INPUT_LINE_BUF
                mov     a, m
                cpi     'X'             ; Command 'X'
                jz      EDIT_REGISTERS
                cpi     'U'             ; Command 'U'
                jz      ROM_EXT_ADDR
                push    psw             ; Save A=CMD and Flags
                call    PARSE_ARGS
                lhld    ARG_3
                mov     c, l
                mov     b, h
                lhld    ARG_2
                xchg
                lhld    ARG_1
                pop     psw             ; Restore A=CMD and Flags
                ; Here A=CMD, HL=ARG_1, DE=ARG_2, BC=ARG_3
                cpi     'D'
                jz      CMD_D
                cpi     'C'
                jz      CMD_C
                cpi     'F'
                jz      CMD_F
                cpi     'S'
                jz      CMD_S
                cpi     'T'
                jz      CMD_T
                cpi     'M'
                jz      CMD_M
                cpi     'G'
                jz      CMD_G
                cpi     'I'
                jz      CMD_I
                cpi     'O'
                jz      CMD_O
                cpi     'L'
                jz      CMD_L
                cpi     'R'
                jz      CMD_R
                jmp     ROM_EXT_ADDR
; ---------------------------------------------------------------------------

DEL_LINE_CHAR:                              
                mvi     a, lobyte(INPUT_LINE_BUF)
                cmp     l
                jz      EMPTY_LINE
                push    h
                lxi     h, BACKSPACE_STR 
                call    PRINT_STRING

; ---------------------------------------------------------------------------
                pop     h
                dcx     h
                jmp     INPUT_CHAR

; =============== S U B R O U T I N E =======================================


INPUT_LINE:                               
                lxi     h, INPUT_LINE_BUF

EMPTY_LINE:                              
                mvi     b, 0

INPUT_CHAR:                               
                call    INPUT_KEY
                cpi     KEY_LEFT          
                jz      DEL_LINE_CHAR
                cpi     KEY_BACKSPACE
                jz      DEL_LINE_CHAR
                cnz     PRINT_CHAR_FROM_A
                mov     m, a
                cpi     KEY_ENTER
                jz      @exit
                cpi     '.'         
                jz      PRINT_PROMPT
                mvi     b, 0FFh
                mvi     a, lobyte(INPUT_LINE_BUF + LINE_BUF_SIZE -1)
                cmp     l
                jz      PRINT_QUESTION_PROMPT
                inx     h
                jmp     INPUT_CHAR

@exit:                        
                mov     a, b
                ral
                lxi     d, INPUT_LINE_BUF
                mvi     b, 0
                ret

; =============== S U B R O U T I N E =======================================

PRINT_STRING:                           
                mov     a, m
                ana     a
                rz
                call    PRINT_CHAR_FROM_A
                inx     h
                jmp     PRINT_STRING

; =============== S U B R O U T I N E =======================================

PARSE_ARGS:                               
                lxi     h, ARG_1
                lxi     d, ARG_3 + 2
                mvi     c, 0
                call    CMD_F                   ; Clear A Command Arguments 1, 2, 3

; ---------------------------------------------------------------------------
                lxi     d, INPUT_LINE_BUF + 1   ; First byte is a Command
                call    PARSE_ARG
                shld    ARG_1
                shld    ARG_2
                rc
                mvi     a, 0FFh
                sta     IS_ARG_3
                call    PARSE_ARG
                shld    ARG_2
                rc
                call    PARSE_ARG
                shld    ARG_3
                rc
                jmp     PRINT_QUESTION_PROMPT

; =============== S U B R O U T I N E =======================================

PARSE_ARG:                              
                lxi     h, 0

@next_char:                              
                ldax    d
                inx     d
                cpi     KEY_ENTER
                jz      @exit
                cpi     KEY_COMA                ; ','
                rz                              ; ARG is parset but it isn't last one
                cpi     KEY_SPACE               ; ' '
                jz      @next_char
                sui     '0'                     ; Convert '0'-'?' key code to Hex number 0h-Fh
                jm      PRINT_QUESTION_PROMPT
                cpi     0Ah
                jm      @digit_0_to_9
                cpi     ( 'A' - '0' )           ; Key Code is Less than 'A'?
                jm      PRINT_QUESTION_PROMPT
                cpi     ( 'F' - '0' + 1 )       ; Key Code is Greater than 'F'?
                jp      PRINT_QUESTION_PROMPT
                sui     ( 'A' - '0' - 10 )      ; Convert A to F Key codes to Hex

@digit_0_to_9:                              
                mov     c, a
                dad     h                       ; HL * 16
                dad     h
                dad     h
                dad     h
                jc      PRINT_QUESTION_PROMPT
                dad     b                       ; HL + BC -> HL
                jmp     @next_char
; ---------------------------------------------------------------------------

@exit:                               
                stc                             ; Set Carry Flag = End of Line
                ret

; ---------------------------------------------------------------------------

COMPARE_HL_DE:                         
                mov     a, h
                cmp     d
                rnz
                mov     a, l
                cmp     e
                ret

; ---------------------------------------------------------------------------

CHECK_BREAK_HL_DE_NEXT:                               
                call    IS_BREAK

CHECK_HL_DE:                              
                call    COMPARE_HL_DE
                jnz     NEXT_BYTE

STACK_INC2_RET:                              
                inx     sp
                inx     sp
                ret

NEXT_BYTE:                             
                inx     h
                ret

; ---------------------------------------------------------------------------

IS_BREAK:                              
                call    GET_KEY
                cpi     3
                rnz
                call    INIT_DISPLAY_REFRESH
                jmp     PRINT_QUESTION_PROMPT

; ---------------------------------------------------------------------------

PRINT_RIGHT_4:                             
                push    h
                lxi     h, RIGHT_4_STR
                call    PRINT_STRING
                pop     h
                ret

; ---------------------------------------------------------------------------

PRINT_M_HEX_SPACE:                                     
                mov     a, m
PRINT_A_HEX_SPACE:                              
                                      
                push    b
                call    PRINT_HEX
                mvi     a, ' '
                call    PRINT_CHAR_FROM_A
                pop     b
                ret

; ---------------------------------------------------------------------------
; Here A=CMD, HL=ARG_1, DE=ARG_2, BC=ARG_3
;
CMD_D:
                call    PRINT_HL_HEX

@print_byte:                               
                call    PRINT_M_HEX_SPACE
                call    CHECK_BREAK_HL_DE_NEXT
                mov     a, l
                ani     0Fh             ; if L==0h, Print Address
                jz      CMD_D
                jmp     @print_byte

; ---------------------------------------------------------------------------
; Here A=CMD, HL=ARG_1, DE=ARG_2, BC=ARG_3
;
CMD_C:                                  
                ldax    b
                cmp     m
                jz      @next
                call    PRINT_HL_HEX
                call    PRINT_M_HEX_SPACE
                ldax    b
                call    PRINT_A_HEX_SPACE

@next:                              
                inx     b
                call    CHECK_BREAK_HL_DE_NEXT
                jmp     CMD_C

; ---------------------------------------------------------------------------
; Here A=CMD, HL=ARG_1, DE=ARG_2, BC=ARG_3
;
CMD_F:                                 
                mov     m, c
                call    CHECK_HL_DE
                jmp     CMD_F

; ---------------------------------------------------------------------------
; Search Memory For A Value
; Here A=CMD, HL=ARG_1, DE=ARG_2, BC=ARG_3
;
CMD_S:                                 
                mov     a, c
                cmp     m
                cz      PRINT_HL_HEX
                call    CHECK_BREAK_HL_DE_NEXT
                jmp     CMD_S

; ---------------------------------------------------------------------------
; Copy Memory Block To Destination
; Here A=CMD, HL=ARG_1, DE=ARG_2, BC=ARG_3
;
CMD_T:                              
                mov     a, m
                stax    b
                inx     b
                call    CHECK_HL_DE
                jmp     CMD_T

; ---------------------------------------------------------------------------
; Display Memory Bytes As Characters In ASCII
; Here A=CMD, HL=ARG_1, DE=ARG_2, BC=ARG_3
;
CMD_L:                                
                call    PRINT_HL_HEX

@next:                               
                mov     a, m
                ora     a
                jm      @not_printable
                cpi     ' '             ; 20h = ' '
                jnc     @printable

@not_printable:                              
                mvi     a, '.'          ; 2Eh = '.'

@printable:                               
                call    PRINT_CHAR_FROM_A
                call    CHECK_BREAK_HL_DE_NEXT
                mov     a, l
                ani     0Fh             ; if L=0 Print Address
                jz      CMD_L
                jmp     @next

; ---------------------------------------------------------------------------
; Modify Memory
; Here A=CMD, HL=ARG_1, DE=ARG_2, BC=ARG_3
;
CMD_M:                                  
                call    PRINT_HL_HEX
                call    PRINT_M_HEX_SPACE
                push    h
                call    INPUT_LINE
                pop     h
                jnc     @next
                push    h
                call    PARSE_ARG
                mov     a, l
                pop     h
                mov     m, a

@next:                               
                inx     h
                jmp     CMD_M

; ---------------------------------------------------------------------------
; Run A Programm
; Here A=CMD, HL=ARG_1, DE=ARG_2, BC=ARG_3
;
CMD_G:                                  
                call    COMPARE_HL_DE
                jz      @nodebug
                xchg                            ; HL<->DE
                shld    DEBUG_STOP_ADDR
                mov     a, m
                sta     DEBUG_STOP_ADDR_VALUE
                mvi     m, RTS_6_OPCODE
                mvi     a, JMP_OPCODE
                sta     RTS_6_ADDR              ; RST 6   <- JMP Opcode
                lxi     h, RST_6_DEBUG_ENTRY
                shld    RTS_6_ADDR + 1          ; RST 6+1 <- ADDR of debuger entry

@nodebug:                               
                lxi     sp, DEBUG_REG_BC
                pop     b
                pop     d
                pop     h
                pop     psw
                sphl
                lhld    DEBUG_REG_HL
                jmp     JMP_TO

; ---------------------------------------------------------------------------
; Read from the ROM connected to the PPI_EXP interface to memory
; Here A=CMD, HL=ARG_1, DE=ARG_2, BC=ARG_3
;
CMD_R:                                 
                mvi     a, PPI_EXP_ROMDISK_MODE
                ; sta     PPI_EXP.CFG
                OUT_P   PPI_EXP.CFG

@next:                               
                shld    PPI_EXP.PORT_B
                ; lda     PPI_EXP.PORT_A
                IN_P    PPI_EXP.PORT_A
                stax    b
                inx     b
                call    CHECK_HL_DE
                jmp     @next

; ---------------------------------------------------------------------------
; Get cursor position
;   Input : None
;   Output: H - row, L - column
;
GET_CURSOR_POSITION:                    
                lhld    CURSOR_POS
                ret
; ---------------------------------------------------------------------------
; Read character from screen at current cursor position
;   Input : None
;   Output: A - char read from screen

READ_VIDEO_RAM:                         
                push    h
                lhld    VARS_ADDR
                mov     a, m
                pop     h
                ret
; ---------------------------------------------------------------------------
; Read data from a tape to memory at the specified offset
; Here A=CMD, HL=ARG_1, DE=ARG_2, BC=ARG_3
;
CMD_I:                                  
                lda     IS_ARG_3
                ora     a
                jz      @no_new_const
                mov     a, e
                sta     TAPE_RD_CONST

@no_new_const:                               
                call    TAPE_READ_BLOCK
                call    PRINT_HL_HEX
                xchg
                call    PRINT_HL_HEX
                xchg
                push    b
                call    CALC_CHECK_SUMM
                mov     h, b
                mov     l, c
                call    PRINT_HL_HEX
                pop     d
                call    COMPARE_HL_DE
                rz
                xchg
                call    PRINT_HL_HEX

; ---------------------------------------------------------------------------

PRINT_QUESTION_PROMPT:                              
                mvi     a, '?'
                call    PRINT_CHAR_FROM_A
                jmp     PRINT_PROMPT

; ---------------------------------------------------------------------------

TAPE_READ_BLOCK:                        
                mvi     a, 0FFh
                call    TAPE_READ_TO_BC_SYNC
                push    h
                dad     b
                xchg
                call    TAPE_READ_TO_BC
                pop     h
                dad     b
                xchg
                push    h
                call    TAPE_READ_MEM_TO_HL_DE
                mvi     a, 0FFh
                call    TAPE_READ_TO_BC_SYNC
                pop     h

; ---------------------------------------------------------------------------

INIT_DISPLAY_REFRESH:                  
                push    h
                lxi     h,  CRT.CFG
                mvi     m, 0
                dcx     h                       ; HL = CRT.DATA
                mvi     m, 4Dh 
                mvi     m, 1Dh
                mvi     m, 99h
                mvi     m, 93h
                inx     h                       ; HL = CRT.CFG
                mvi     m, 27h 
                mov     a, m

@not_ready:                              
                mov     a, m
                ani     CRT_READY_FLAG
                jz      @not_ready
                lxi     h, DMA.CFG
                mvi     m, 80h                  ; Stop DMA
                mvi     l, 4                    ; HL = DMA.CH2_ADDR
                mvi     m, SCREEN_BUF_ADDR_L
                mvi     m, SCREEN_BUF_ADDR_H
                inr     l                       ; HL = DMA.CH2_LEN
                mvi     m, 23h 
                mvi     m, 49h 
                mvi     l, 8                    ; HL = DMA.CFG
                mvi     m, DMA_MODE_START       ; Start DMA
                pop     h
                ret
; ---------------------------------------------------------------------------

TAPE_READ_TO_BC:                              
                mvi     a, 8

; ---------------------------------------------------------------------------

TAPE_READ_TO_BC_SYNC:                               
                call    TAPE_READ_BYTE
                mov     b, a
                mvi     a, 8
                call    TAPE_READ_BYTE
                mov     c, a
                ret

; ---------------------------------------------------------------------------
; TAPE_WRITE_MEM_FROM_HL_TO_DE
TAPE_READ_MEM_TO_HL_DE:                               
                mvi     a, 8
                call    TAPE_READ_BYTE
                mov     m, a
                call    CHECK_HL_DE
                jmp     TAPE_READ_MEM_TO_HL_DE

; ---------------------------------------------------------------------------
; Calculate Check Summ
;   Input: HL - start; DE - end;
;   Output: BC - checksum

CALC_CHECK_SUMM:                       
                lxi     b, 0

@loop:                               
                mov     a, m
                add     c
                mov     c, a
                push    psw
                call    COMPARE_HL_DE
                jz      STACK_INC2_RET
                pop     psw
                mov     a, b
                adc     m
                mov     b, a
                call    CHECK_HL_DE
                jmp     @loop

; ---------------------------------------------------------------------------
; Write memory to cassette. Default speed is 1Dh / 1200 bps
; Here A=CMD, HL=ARG_1, DE=ARG_2, BC=ARG_3
;
CMD_O:                                  
                mov     a, c
                ora     a
                jz      @no_new_speed
                sta     TAPE_WR_CONST

@no_new_speed:
                ; Print Start Address, End Address and Check Summ                               
                push    h
                call    CALC_CHECK_SUMM
                pop     h
                call    PRINT_HL_HEX
                xchg
                call    PRINT_HL_HEX
                xchg
                push    h
                mov     h, b
                mov     l, c
                call    PRINT_HL_HEX
                pop     h

TAPE_WRITE_BLOCK:                       
                push    b                               ; Save Check Summ
                lxi     b, 0        

@next:                                      
                call    TAPE_WRITE_BYTE                 ; Write byte from C
                dcr     b                               ; Refresh Memory Cycle?
                xthl                                    ; ?
                xthl                                    ; ?
                jnz     @next       
                mvi     c, TAPE_SYNC_BYTE     
                call    TAPE_WRITE_BYTE                 ; Write TAPE_SYNC_BYTE
                call    TAPE_WRITE_HL                   ; Write Start Address
                xchg        
                call    TAPE_WRITE_HL                   ; Write End Address
                xchg
                call    TAPE_WRITE_MEM_FROM_HL_TO_DE    ; Write Memory Block
                lxi     h, 0
                call    TAPE_WRITE_HL                   ; Write 00h, 00h
                mvi     c, TAPE_SYNC_BYTE
                call    TAPE_WRITE_BYTE                 ; Write TAPE_SYNC_BYTE
                pop     h                               ; Restore Check Summ
                call    TAPE_WRITE_HL                   ; Write Check Summ
                jmp     INIT_DISPLAY_REFRESH

; ---------------------------------------------------------------------------

PRINT_HL_HEX:                         
                push    b
                call    PRINT_RIGHT_4
                mov     a, h
                call    PRINT_HEX
                mov     a, l
                call    PRINT_A_HEX_SPACE
                pop     b
                ret

; ---------------------------------------------------------------------------

TAPE_WRITE_MEM_FROM_HL_TO_DE:                              
                mov     c, m
                call    TAPE_WRITE_BYTE
                call    CHECK_HL_DE
                jmp     TAPE_WRITE_MEM_FROM_HL_TO_DE

; ---------------------------------------------------------------------------

TAPE_WRITE_HL:                               
                mov     c, h
                call    TAPE_WRITE_BYTE
                mov     c, l
                jmp     TAPE_WRITE_BYTE

; ---------------------------------------------------------------------------
; Cassette input 
;   Input : A=0FFh - with sync; A=08h - no sync
;   Output: A - data read from cassette
;
TAPE_READ_BYTE:                         
                push    h
                push    b
                push    d
                mov     d, a
@dma_off:                            
                mvi     a, DMA_MODE_STOP                
                sta     DMA.CFG
                lxi     h, 0
                dad     sp
                lxi     sp, 0
                shld    TAPE_READ_STACK_SAVED
                mvi     c, 0
                ; Get a Data Bit
                lda     PPI_KBD.PORT_C
                rrc
                rrc
                rrc
                rrc
                ani     1
                mov     e, a                    ; E = readed bit
@get_next_bit:                               
                pop     psw
                mov     a, c
                ani     7Fh
                rlc
                mov     c, a
                mvi     h, 0
@same_bit:                               
                dcr     h
                jz      @check_user_break                ; no next bit, read error
                pop     psw
                ; Get a Data Bit
                lda     PPI_KBD.PORT_C
                rrc
                rrc
                rrc
                rrc
                ani     1
                cmp     e
                jz      @same_bit
                ora     c
                mov     c, a
                dcr     d
                lda     TAPE_RD_CONST
                jnz     @no_new_speed
                sui     12h             ; The procedure time correction for start of the bit
@no_new_speed:                               
                mov     b, a

@delay_rd_const:                              
                pop     psw
                dcr     b
                jnz     @delay_rd_const
                inr     d
                lda     PPI_KBD.PORT_C
                rrc
                rrc
                rrc
                rrc
                ani     1
                mov     e, a
                mov     a, d
                ora     a
                jp      @next_bit
                mov     a, c
                cpi     TAPE_SYNC_BYTE
                jnz     @check_inversion
                xra     a                   ; No Error
                sta     TAPE_READ_ERR
                jmp     @get_byte
@check_inversion:                             
                cpi     ( TAPE_SYNC_BYTE ~ 0FFh )   ; 19h
                jnz     @get_next_bit
                mvi     a, 0FFh             ; Error
                sta     TAPE_READ_ERR
@get_byte:                              
                mvi     d, 9
@next_bit:                              
                dcr     d
                jnz     @get_next_bit
                ; Restore Screen
                lxi     h, DMA.CH2_ADDR
                mvi     m, SCREEN_BUF_ADDR_L
                mvi     m, SCREEN_BUF_ADDR_H
                ; DMA.CH2_LEN
                inx     h
                mvi     m, 23h ; '#'
                mvi     m, 49h ; 'I'
                ; CRT Configure
                mvi     a, 27h ; '''
                sta     CRT.CFG
                mvi     a, 0E0h
                sta     CRT.CFG
                ; DMA.CFG - Start DMA
                mvi     l, DMA_8257.CFG
                mvi     m, DMA_MODE_START

                lhld    TAPE_READ_STACK_SAVED
                sphl
                lda     TAPE_READ_ERR
                xra     c
                jmp     TAPE_IO_EXIT
@check_user_break:                               
                lhld    TAPE_READ_STACK_SAVED
                sphl
                call    INIT_DISPLAY_REFRESH
                mov     a, d
                ora     a
                jp      PRINT_QUESTION_PROMPT
                call    IS_BREAK                ; If Break, it WIL NOT return here
                jmp     @dma_off

; ---------------------------------------------------------------------------

TAPE_WRITE_BYTE:                      
                push    h
                push    b
                push    d
                push    psw
                mvi     a, 80h
                sta     DMA.CFG
                lxi     h, 0
                dad     sp
                lxi     sp, 0
                mvi     d, 8

@next_bit:                              
                pop     psw
                mov     a, c
                rlc
                mov     c, a
                mvi     a, 1
                xra     c
                sta     PPI_KBD.PORT_C
                lda     TAPE_WR_CONST
                mov     b, a

@delay1_wr_const:                               
                pop     psw
                dcr     b
                jnz     @delay1_wr_const
                mvi     a, 0
                xra     c
                sta     PPI_KBD.PORT_C
                dcr     d
                lda     TAPE_WR_CONST
                jnz     @no_wr_cont_arg
                sui     0Eh

@no_wr_cont_arg:                            
                mov     b, a

@delay2_wr_const:                               
                pop     psw
                dcr     b
                jnz     @delay2_wr_const
                inr     d
                dcr     d
                jnz     @next_bit
                sphl
                lxi     h, DMA.CH2_ADDR
                mvi     m, SCREEN_BUF_ADDR_L
                mvi     m, SCREEN_BUF_ADDR_H
                inx     h                       ; DMA.CH2_LEN 
                mvi     m, 23h ; '#'
                mvi     m, 49h ; 'I'
                ; CRT Configure
                mvi     a, 27h ; '''
                sta     CRT.CFG
                mvi     a, 0E0h
                sta     CRT.CFG
                mvi     l, DMA_8257.CFG
                mvi     m, DMA_MODE_START
                pop     psw

TAPE_IO_EXIT:                             
                pop     d
                pop     b
                pop     h
                ret

; ---------------------------------------------------------------------------

PRINT_HEX:                             
                push    psw
                rrc
                rrc
                rrc
                rrc
                call    @hex_to_char
                pop     psw

; ---------------------------------------------------------------------------

@hex_to_char:                               
                ani     0Fh
                cpi     0Ah
                jm      @under_10d
                adi     7

@under_10d:                               
                adi     '0'     ; 30h 

PRINT_CHAR_FROM_A: 
                mov     c, a
PRINT_CHAR:                            
                push    psw
                push    b
                push    d
                push    h
                call    KEY_STATUS
                lxi     h, MOVE_CURSOR_TO_CURSOR_POS
                push    h
                lhld    CURSOR_POS
                xchg
                lhld    VARS_ADDR
                lda     PRINT_CHAR_MODE
                dcr     a
                jm      PRINT_CHAR_MODE_0
                jz      PRINT_CHAR_MODE_1
                jpo     PRINT_CHAR_MODE_2
                mov     a, c
                sui     20h ; ' '
                mov     c, a

@loop:                               
                dcr     c
                jm      NETX_CHAR
                push    b
                call    PRINT_CHAR_RIGHT
                pop     b
                jmp     @loop
NETX_CHAR:                               
                xra     a
PRINT_CHAR_EXIT:                              
                sta     PRINT_CHAR_MODE
                ret

PRINT_CHAR_MODE_0:                             
                mov     a, c
                ani     7Fh                 ; Clear High Bit
                mov     c, a
                cpi     1Fh
                jz      PRINT_CHAR_CLS
                cpi     0Ch
                jz      PRINT_CHAR_HOME
                cpi     0Dh
                jz      PRINT_CHAR_CR
                cpi     0Ah
                jz      PRINT_CHAR_LF
                cpi     8
                jz      PRINT_CHAR_LEFT
                cpi     18h
                jz      PRINT_CHAR_RIGHT
                cpi     19h
                jz      PRINT_CHAR_UP
                cpi     1Ah
                jz      PRINT_CHAR_DOWN
                cpi     1Bh
                jz      PRINT_CHAR_AR2
                cpi     7
                jnz     PRINT_CHAR_OTHERS
                lxi     b, 5F0h

BEEP:                                                                         
                mov     a, b
@loop1:                                 
                ei
                dcr     a
                jnz     @loop1
                mov     a, b
@loop2:                                 
                di
                dcr     a
                jnz     @loop2
                dcr     c
                jnz     BEEP
                ret

; ---------------------------------------------------------------------------

PRINT_CHAR_OTHERS:                              
                mov     m, c
                call    PRINT_CHAR_RIGHT
                mov     a, d
                cpi     3
                rnz
                mov     a, e
                cpi     8
                rnz
                call    PRINT_CHAR_UP

PRINT_CHAR_LF:                               
                mov     a, d
                cpi     1Bh
                jnz     PRINT_CHAR_DOWN
                push    h
                push    d
                lxi     h, SCREEN.BUF + 242 ; 37C2h
                lxi     d, SCREEN.BUF + 320 ; 3810h    
                lxi     b, 79Eh

@next_char:                               
                ldax    d
                mov     m, a
                inx     h
                inx     d
                dcx     b
                mov     a, c
                ora     b
                jnz     @next_char
                pop     d
                pop     h
                ret

; ---------------------------------------------------------------------------

PRINT_CHAR_MODE_1:                               
                mov     a, c
                cpi     59h ; 'Y'
                jnz     NETX_CHAR
                call    PRINT_CHAR_HOME
                mvi     a, 2
                jmp     PRINT_CHAR_EXIT

; ---------------------------------------------------------------------------

PRINT_CHAR_MODE_2:                               
                mov     a, c
                sui     20h ; ' '
                mov     c, a

@next_char:                              
                dcr     c
                mvi     a, 4
                jm      PRINT_CHAR_EXIT
                push    b
                call    PRINT_CHAR_DOWN
                pop     b
                jmp     @next_char

; ---------------------------------------------------------------------------

MOVE_CURSOR_TO_CURSOR_POS:                               
                shld    VARS_ADDR
                xchg
                shld    CURSOR_POS
                mvi     a, CRT_CMD_SETCURSOR
                sta     CRT.CFG
                mov     a, l
                sta     CRT.DATA
                mov     a, h
                sta     CRT.DATA
                pop     h
                pop     d
                pop     b
                pop     psw
                ret

; ---------------------------------------------------------------------------

PRINT_CHAR_AR2:                              
                mvi     a, 1
                jmp     PRINT_CHAR_EXIT

; ---------------------------------------------------------------------------

PRINT_CHAR_CLS:                              
                lxi     h, SCREEN_END
                lxi     d, SCREEN_SIZE + 1
@loop:                               
                xra     a
                mov     m, a
                dcx     h
                dcx     d
                mov     a, e
                ora     d
                jnz     @loop

; ---------------------------------------------------------------------------

PRINT_CHAR_HOME:                              
                                       
                lxi     d, 308h
                lxi     h, SCREEN + 242
                ret

; ---------------------------------------------------------------------------

PRINT_CHAR_RIGHT:                             
                mov     a, e
                inx     h
                inr     e
                cpi     47h ; 'G'
                rnz
                mvi     e, 8
                lxi     b, 0FFC0h       ; ??
                dad     b

; ---------------------------------------------------------------------------

PRINT_CHAR_DOWN:                               
                mov     a, d
                cpi     1Bh
                lxi     b, 4Eh ; 'N'
                jnz     @exit
                mvi     d, 2
                lxi     b, 0F8B0h       ; ??

@exit:                             
                inr     d
                dad     b
                ret

; ---------------------------------------------------------------------------

PRINT_CHAR_LEFT:                              
                mov     a, e
                dcx     h
                dcr     e
                cpi     8
                rnz
                mvi     e, 47h ; 'G'
                lxi     b, 40h ; '@'
                dad     b

PRINT_CHAR_UP:                               
                mov     a, d
                cpi     3
                lxi     b, DEBUG_EXEPTION
                jnz     @exit
                mvi     d, 1Ch
                lxi     b, 750h

@exit:                               
                dcr     d
                dad     b
                ret
                
; ---------------------------------------------------------------------------

PRINT_CHAR_CR:                              
                mov     a, l
                sub     e
                jnc     @exit
                dcr     h

@exit:                              
                mov     l, a
                mvi     e, 8
                lxi     b, 8
                dad     b
                ret

; ---------------------------------------------------------------------------

KEY_STATUS:                             
                lda     PPI_KBD.PORT_C
                ani     KEY_RUSLAT_MASK
                jz      @ruslat
                lda     KEY_READED
                ora     a
                rnz

@ruslat:                             
                push    h
                lhld    KEY_CODE
                call    GET_KEY
                cmp     l
                mov     l, a
                jz      SAME_KEY

SCAN_KEY:                              
                mvi     a, 1
                sta     KEY_FIRST_TIME_FLAG
                mvi     h, 15h

KEY_WAIT_FOR_AUTOREPEAT:                             
                xra     a

KEY_READ_EXIT:                               
                shld    KEY_CODE
                pop     h
                sta     KEY_READED
                ret

; ---------------------------------------------------------------------------

SAME_KEY:                               
                dcr     h
                jnz     KEY_WAIT_FOR_AUTOREPEAT
                inr     a
                jz      KEY_READ_EXIT
                inr     a
                jz      RUSLAT_KEY_PRESSED
                push    b
                lxi     b, 5003h
                call    BEEP
                pop     b
                lda     KEY_FIRST_TIME_FLAG
                mvi     h, 0E0h
                dcr     a
                sta     KEY_FIRST_TIME_FLAG
                jz      @auto_repeat
                mvi     h, 40h ; '@'

@auto_repeat:                              
                mvi     a, 0FFh
                jmp     KEY_READ_EXIT

; ---------------------------------------------------------------------------

RUSLAT_KEY_PRESSED:                               
                lda     PPI_KBD.PORT_C
                ani     KEY_RUSLAT_MASK
                jz      RUSLAT_KEY_PRESSED
                ; Inverse RUS/LAT Flag
                lda     RUSLAT_FLAG
                cma
                sta     RUSLAT_FLAG
                jmp     SCAN_KEY

; ---------------------------------------------------------------------------

INPUT_KEY:                              
                call    KEY_STATUS
                ora     a
                jz      INPUT_KEY
                xra     a
                sta     KEY_READED
                lda     KEY_CODE
                ret

; ---------------------------------------------------------------------------

GET_KEY:                               
                lda     PPI_KBD.PORT_C
                ani     KEY_RUSLAT_MASK
                jnz     @no_ruslat
                mvi     a, 0FEh         ; RUS/LAT Key is holding
                ret

; ---------------------------------------------------------------------------

@no_ruslat:                               
                xra     a
                sta     PPI_KBD.PORT_A
                sta     PPI_KBD.PORT_C
                lda     RUSLAT_FLAG
                ani     1
                ori     6
                sta     PPI_KBD.CFG
                lda     PPI_KBD.PORT_B
                inr     a
                jnz     @key_pressed
                dcr     a
                ret
; ---------------------------------------------------------------------------

@key_pressed:                             
                push    h
                mvi     l, 1
                mvi     h, 7

@next_column:                             
                mov     a, l
                rrc
                mov     l, a
                cma
                sta     PPI_KBD.PORT_A
                lda     PPI_KBD.PORT_B
                cma
                ora     a
                jnz     @col_key_pressed
                dcr     h
                jp      @next_column

@no_keys:                              
                mvi     a, 0FFh
                pop     h
                ret

; ---------------------------------------------------------------------------

@col_key_pressed:                              
                mvi     l, KEY_DEBOUNCE_DELAY

@debounce:                               
                lda     PPI_KBD.PORT_B
                cma
                ora     a
                jz      @no_keys
                dcr     l
                jnz     @debounce
                mvi     l, 8

@count_rows:                             
                dcr     l
                rlc
                jnc     @count_rows
                ; L = Key Row <=> H = Key Column
                mov     a, h
                mov     h, l
                mov     l, a
                ; Here is L = Key Column, H = Key Row
                cpi     1
                jz      LOAD_TABLE_1        ; Column 1
                jc      LOAD_TABLE_0        ; Column 0
                ; Columns 2-7
                rlc
                rlc
                rlc
                adi     20h ; ' '
                ora     h   ; Column * 8 + 20h
                cpi     5Fh ; Is Row=7 and Col=7 ?
                jnz     CHECK_UC_KEY
                mvi     a, ' '
                pop     h
                ret

; ---------------------------------------------------------------------------

TABLE_1:        .db   9, 0Ah, 0Dh, 7Fh,   8, 19h, 18h, 1Ah
TABLE_0:        .db 0Ch, 1Fh, 1Bh,   0,   1,   2,   3,   4,  5

LOAD_TABLE_0:                              
                mov     a, h
                lxi     h, TABLE_0
                jmp     TRANSLATE_CODE

; ---------------------------------------------------------------------------

LOAD_TABLE_1:                              
                mov     a, h
                lxi     h, TABLE_1

TRANSLATE_CODE:                               
                add     l
                mov     l, a
                mov     a, m
                cpi     40h ; '@'
                pop     h
                rc
                push    h

CHECK_UC_KEY:                             
                mov     l, a
                lda     PPI_KBD.PORT_C
                mov     h, a
                ani     KEY_UC_MASK
                jnz     @no_uc_key
                mov     a, l
                cpi     40h ; '@'
                jm      KEY_IS_DIGIT
                ani     1Fh ; English Letter
                pop     h
                ret

; ---------------------------------------------------------------------------

@no_uc_key:                             
                lda     RUSLAT_FLAG
                ora     a
                jz      RUS_LAT_IS_OFF
                mov     a, l
                cpi     40h ; '@'
                jm      RUS_LAT_IS_OFF
                ori     20h ; Change to Russian
                mov     l, a

RUS_LAT_IS_OFF:                              
                mov     a, h
                ani     20h
                jnz     KEY_IS_DIGIT
                mov     a, l
                cpi     40h ; '@'
                jm      KEY_IS_SPECIAL
                mov     a, l
                xri     20h
                pop     h
                ret

; ---------------------------------------------------------------------------

KEY_IS_SPECIAL:                               
                mov     a, l
                ani     2Fh     ; Masc Special Code
                mov     l, a

KEY_IS_DIGIT:                               
                mov     a, l
                cpi     40h ; '@'
                pop     h
                rp
                push    h
                mov     l, a
                ani     0Fh
                cpi     0Ch
                mov     a, l
                jm      @exit
                xri     10h

@exit:                                  
                pop     h
                ret

; ---------------------------------------------------------------------------

GET_FREE_MEM_ADDR:                      
                lhld    MEM_AVAILABLE
                ret

; ---------------------------------------------------------------------------

SET_FREE_MEM_ADDR:                     
                shld    MEM_AVAILABLE
                ret

; ---------------------------------------------------------------------------

HELLO_STR:      .db 1Fh, "radio-86rk", 0
PROMPT_STR:     .db 0Dh, 0Ah, "-->", 0
RIGHT_4_STR:    .db 0Dh, 0Ah, 18h, 18h, 18h, 18h, 0 ; 18H = RIGHT
REGISTERS_STR:  .db 0Dh, 0Ah
                .db " PC-", 0Dh, 0Ah
                .db " HL-", 0Dh, 0Ah
                .db " BC-", 0Dh, 0Ah
                .db " DE-", 0Dh, 0Ah
                .db " SP-", 0Dh, 0Ah
                .db " AF-"
                .db 19h, 19h, 19h, 19h, 19h, 19h, 0 ; 19h = UP
BACKSPACE_STR:  .db 8, " ", 8, 0                    ; 8h = LEFT

; ---------------------------------------------------------------------------

RST_6_DEBUG_ENTRY:
                shld    DEBUG_REG_HL
                push    psw
                pop     h
                shld    DEBUG_REG_PSW
                pop     h
                dcx     h
                shld    DEBUG_REG_PC
                lxi     h, 0

DEBUG_EXEPTION:                              
                dad     sp
                lxi     sp, DEBUG_REG_PSW
                push    h
                push    d
                push    b
                lhld    DEBUG_REG_PC
                lxi     sp, STACK_ADDR
                call    PRINT_HL_HEX
                xchg
                lhld    DEBUG_STOP_ADDR
                call    COMPARE_HL_DE
                jnz     PRINT_PROMPT                    ; Others Break Address
                lda     DEBUG_STOP_ADDR_VALUE           ; Our Break Sddress
                mov     m, a
                jmp     PRINT_PROMPT

; ---------------------------------------------------------------------------

EDIT_REGISTERS:                               
                lxi     h, REGISTERS_STR
                call    PRINT_STRING
                lxi     h, DEBUG_REG_PC
                mvi     b, 6

@loop1:                               
                mov     e, m
                inx     h
                mov     d, m
                push    b
                push    h
                xchg
                call    PRINT_HL_HEX
                call    INPUT_LINE
                jnc     @exit
                call    PARSE_ARG
                pop     d
                push    d
                xchg
                mov     m, d
                dcx     h
                mov     m, e

@exit:                               
                pop     h
                pop     b
                dcr     b
                inx     h
                jnz     @loop1
                ret

; ---------------------------------------------------------------------------
; end of 'ROM'
