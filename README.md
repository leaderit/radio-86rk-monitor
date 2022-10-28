# Radio-86rk-monitor
Monitor for Radio-86RK (Retro Soviet hobbyist computer), Rewrited from Scratch, Well Described, Binary Identical to Original One.

I did the project because i would like to update hardware of "good-old" 8-bit PC, but it needs to update firmware. I did not found
binary variants for my goals, i did not found sources for my modifications. Eventualy, i restored it from scratch by reverce enginering
of the original binary file.

I described and commented it well enouth for future modifications and your education goals.

# Tools for compile sources are

Tools for compile sources are:
https://github.com/vhelin/wla-dx

You may build it from sources for platforms:

    Build Status - Linux
    Build Status - macOS
    Build Status - Windows x86
    Build Status - Windows x64
    Build Status - Windows MSVC


For Mac OS use:
brew install wla-dx

Docs are here:
https://wla-dx.readthedocs.io/en/latest/

# The Monitor

RADIO-86RK (SOVIET 8-BIT COMPUTER)
THE MONITOR ROM source recovered from ROM Binaries and well decribed for future using
by Valerii Grazhdankin, (c) 2022

The Monitor is a system programm with a command line interface, 
booted after System On or Reset.
Monitor is placed in system's Read-Only Memory (ROM)

## Monitor Commands

- Memory Operations
```
    D<Start_Address>,<End_Address> - Display memory content in hexadecimal
    L<Start_Address>,<End_Address> - Display memory content in ASCII
    F<Start_Address>,<End_Address>,<Value> - Fill memory with the specified value
    M<Address> - Modify memory content
    T<Start_Address>,<End_Address>,<Destination_Start_Address> - Copy memory block to destination
    C<Start_Address>,<End_Address>,<Destination_Start_Address> - Compare memory block with destination
    S<Start_Address>,<End_Address>,<Value> - Search memory for a value
    R<ROM_Start_Address>,<ROM_End_Address>,<Destination_Start_Address> - Read from the ROM connected to the Parallel interface to memory
```
- Run Control
```
    G<Start_Address>[,End_Address] - Run code, optionally stop at the specified address
    X - Display and modify registers
    U - Run Expanded ROM from 0F000h Address
```
- Cassette Input/Output
```
    O<Start_Address>,<End_Address>[,Speed] - Write memory to cassette. Default speed is 1Dh / 1200 bps
    I[Offset][,Speed] - Read data from a tape to memory at the specified offset
```

# The Monitor Tape Records

Monitor's format of a tape record:

- 0xE6  - Syncronization byte
- XX XX - Start address of memory allocation, High byte First
- XX XX - End address of memory allocation, High byte First
- ..... - Data of the file. Size of the file is END_ADDR - START_ADDR
- 00 00 - Junk bytes, may be absant
- 0xE6  - Syncronization byte
- XX XX - Check summ, High byte First

# The Monitor Memory Map

- 16K Version:
    - 0000h - 3FFFh - SRAM
        - 3600h - 3FFFh - Display buffer and monitor variables

- 32K Version:
    - 0000h - 7FFFh - SRAM
        - 7600h - 7FFFh - Display buffer and monitor variables

- Both versions:
    - 8000h - 8FFFh - 8255A PPI (Keyboard, Cassette recorder, Sound control)
        - 8000h - Channel A data
        - 8001h - Channel B data
        - 8002h - Channel C data
        - 8003h - Control word

    - 0A000h - 0AFFFh - 8255A PPI (ROM Disk and Parallel Interface)
        - 0A000h - Channel A data
        - 0A001h - Channel B data
        - 0A002h - Channel C data
        - 0A003h - Control word

    - 0C000h - 0CFFFh - 8275 CRT Controller
        - 0C000h - Data Register
        - 0C001h - Configuration / Status register

    - 0E000h - 0EFFFh - 8257 DMA Controller (write only, because the computer architecture )
        - 0E000h - Channel 0 Address register 
        - 0E000h - Channel 0 Lenght register
        - 0E000h - Channel 1 Address register
        - 0E000h - Channel 1 Lenght register
        - 0E000h - Channel 2 Address register
        - 0E000h - Channel 2 Lenght register
        - 0E000h - Channel 3 Address register
        - 0E000h - Channel 3 Lenght register
        - 0E000h - Configuration register      

    - 0F000h - 0F7FFh - Monitor Extention EPROM (read only)

    - 0F800h - 0FFFFh - The Monitor EPROM (read only)

# Monitor API

- 0F800h - Entry point after reset                

- 0F803h - Keyboard input 
  - Input : None
  - Output: A - character code read from keyboard

- 0F806h - Tape Read 
  - Input : A=0FFh - with sync; A=08h - no sync
  - Output: A - data read from a tape

- 0F809h - Print character to screen 
  - Input : C - character to print
  - Output: None

- 0F80Ch - Tape write 
  - Input : C - data to write
  - Output: None

- 0F80Fh - Print character to screen (Duplicated entry)
  - Input : C - character to print
  - Output: None

- 0F812h - Query keyboard 
  - Input : None
  - Output: A=00h - key not pressed; A=0FFh - key pressed

- 0F815h - Print to screen in hex 
  - Input: A - data to print
  - Output: None

- 0F818h - Print string 
  - Input: HL - string address
  - Output: None

- 0F81Bh - Get key
  - Input : None
  - Output: A=0FFh - key not pressed; A=0FEh - Rus/Lat; otherwise A - key code

- 0F81Eh - Get cursor position
  - Input : None
  - Output: H - row, L - column

- 0F821h - Read character from screen at current cursor position
  - Input : None
  - Output: A - char read from screen

- 0F824h - Read from a tape
  - Input: HL - offset of memory buffer to store data to
  - Output: HL - start; DE - end; BC - checksum

- 0F27h - Write block to a tape
   - Input: HL - start, DE - end of memory block
   - Output:  BC - checksum

- 0F82Ah - Calculate Check Summ
   - Input: HL - start; DE - end;
   - Output: BC - checksum

- 0F82Dh - Init Display
   - Input: None
   - Output: None

- 0F830h - Get memory heap
   - Input: None
   - Output: HL - Address of highest address of memory, avalable for a user's programm

- 0F833h - Set memory heap
   - Input: HL - Address of highest address of memory, avalable for a user's programm
   - Output:  None

**Happy Hobying!**

*Valerii Grazhdankin*
