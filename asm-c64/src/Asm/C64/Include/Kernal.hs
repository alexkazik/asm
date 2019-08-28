{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}

module Asm.C64.Include.Kernal
  ( kernal
  ) where

import           Asm.Cpu6502

kernal :: Asm
kernal =
  [asm|
    namespace kernal
      pointer byte[0x2000] rom = 0xe000

      --                               Function                           Function Input/Output                    Register Usage
      --                               Description                        Parameters                             entry  return  used
      -- ---------------------------------------------------------------------------------------------------------------------------
      pointer code init    = 0xff81 -- init vic & screen editor                                                  - - -  - - -  a x y
      pointer code ioinit  = 0xff84 -- initialize cia & irq                                                      - - -  - - -  a x y
      pointer code ramtas  = 0xff87 -- ram test & search ram end                                                 - - -  - - -  a x y
      pointer code restor  = 0xff8a -- restore default i/o vectors                                               - - -  - - -  a - y
      pointer code vectors = 0xff8d -- read/set i/o vectors               in: c=0 moves from y/x to vectors      - x y  - x -  a - y
                                    --                                        c=1 moves vectors to y/x           - x y  - x -  a - y
      pointer code setmsg  = 0xff90 -- enable/disable kernal messages     in: a bit7=1 error msgs on             a - -  - - -  a - -
                                    --                                          bit6=1 control msgs on
      pointer code second  = 0xff93 -- send secondary addr after listen   in: a=secondary address                a - -  - - -  a - -
      pointer code tksa    = 0xff96 -- send secondary addr after talk     in: a=secondary address                a - -  - - -  a - -
      pointer code memtop  = 0xff99 -- read/set top of memory             in: c=0; y/x address                   - x y  - x y  - - -
                                    --                                    out:c=1; y/x address                   - - -  - x y  - x y
      pointer code membot  = 0xff9c -- read/set bottom of memory          in: c=0; y/x address                   - x y  - x y  - - -
                                    --                                    out:c=1; y/x address                   - - -  - x y  - x y
      pointer code scnkey  = 0xff9f -- scan keyboard                                                             - - -  - - -  a x y
      pointer code settmo  = 0xffa2 -- set ieee timeout                   in: a bit7=1 disable, bit7=0 enable    a - -  a - -  - - -
      pointer code acptr   = 0xffa5 -- input byte from serial             out:a=byte, c=1 and st=2 if timeout    - - -  a - -  a - -
      pointer code ciout   = 0xffa8 -- output byte to serial              in: a=byte, c=1 and st=3 if timeout    a - -  a - -  - - -
      pointer code untlk   = 0xffab -- untalk all serial devices                                                 - - -  - - -  a - -
      pointer code unlsn   = 0xffae -- unlisten all serial devices                                               - - -  - - -  a - -
      pointer code listen  = 0xffb1 -- make serial device listen          in: a=device number                    a - -  - - -  a - -
      pointer code talk    = 0xffb4 -- make serial device talk            in: a=device number                    a - -  - - -  a - -
      pointer code readst  = 0xffb7 -- read i/o status byte               out:a=status byte                      - - -  a - -  a - -
      pointer code setlfs  = 0xffba -- set file parameters                in: a=logical file number              a x y  a x y  - - -
                                    --                                        x=device number
                                    --                                        y=secondary addr
      pointer code setnam  = 0xffbd -- set file name                      in: a=length of filename               a x y  a x y  - - -
                                    --                                        y/x=pointer code to name addr
      pointer code open    = 0xffc0 -- open log.file after setlfs,setnam  out:a=error# if c=1                    - - -  - - -  a x y
      pointer code close   = 0xffc3 -- close a logical file               in: a=logical file number              a - -  - - -  a x y
      pointer code chkin   = 0xffc6 -- open channel for input             in: x=logical file number              - x -  - - -  a x -
      pointer code chkout  = 0xffc9 -- open channel for output            in: x=logical file number              - x -  - - -  a x -
      pointer code clrchn  = 0xffcc -- restore default devices                                                   - - -  - - -  a x -
      pointer code chrin   = 0xffcf -- input character                    out:a=character, c=1 and st=error      - - -  a - -  a - -
      pointer code chrout  = 0xffd2 -- output character                   in: a=character, c=1 and st=error      a - -  a - -  - - -
      pointer code load    = 0xffd5 -- load after call setlfs,setnam      in: a=0 load, a=1 verify               a x y  a x y  a x y
                                    --                                        y/x = dest.addr if sec.addr=0
      pointer code save    = 0xffd8 -- save after call setlfs,setnam      in: a=zero page pointer code to start.addr  a x y  - - -  a x y
                                    --                                        y/x=ending address
      pointer code settim  = 0xffdb -- set jiffy clock                    in: a=msb, x=middle, y=lsb             a x y  - - -  - - -
      pointer code rdtim   = 0xffde -- read jiffy clock                   out:a=msb, x=middle, y=lsb             - - -  a x y  a x y
      pointer code stop    = 0xffe1 -- check stop key                     out:z=0 if stop not used; x unchanged  - - -  a - -  a - -
                                    --                                        z=1 if stop used; x changed        - - -  a - -  a x -
                                    --                                        a=last line of keyboard matrix
      pointer code getin   = 0xffe4 -- get a byte from channel            out:keyboard:a=0 if puffer empty       - - -  a - -  a x y
                                    --                                        rs232:status byte                  - - -  a - -  a - -
                                    --                                        serial:status byte                 - - -  a - -  a - -
                                    --                                        tape:status byte                   - - -  a - -  a - y
      pointer code clall   = 0xffe7 -- close or abort all files                                                  - - -  - - -  a x -
      pointer code udtim   = 0xffea -- update jiffy clock                                                        - - -  - - -  a x -
      pointer code screen  = 0xffed -- return screen size                 out:x=columns, y=rows                  - - -  - x y  - x y
      pointer code plot    = 0xfff0 -- read/set cursor position           in: c=0, x=row, y=column               - x y  - x y  - - -
                                    --                                    out:c=1, x=row, y=column               - - -  - x y  - x y
      pointer code iobase  = 0xfff3 -- returns the addr of i/o devices    out:y/x=addr($dc00)                    - - -  - x y  - x y
      -- based on http://codebase64.org/doku.php?id=base:kernalreference


      -- RAM used by the kernal

      namespace vector
        pointer _lohi irq = 0x0314
        pointer _lohi brk = 0x0316
        pointer _lohi nmi = 0x0318
        pointer _lohi open = 0x031a
        pointer _lohi close = 0x031c
        pointer _lohi chkin = 0x031e
        pointer _lohi chkout = 0x0320
        pointer _lohi clrchn = 0x0322
        pointer _lohi chrin = 0x0324
        pointer _lohi chrout = 0x0326
        pointer _lohi stop = 0x0328
        pointer _lohi getin = 0x032a
        pointer _lohi clall = 0x032c
        pointer _lohi user = 0x032e
        pointer _lohi load = 0x0330
        pointer _lohi save = 0x0332
      end

      pointer byte keyboard_buffer_len = 0xc6
      pointer _lohi end_of_program = 0xae
      pointer byte[9] keyboard_buffer = 0x0277

      --   $0A/10                  Load/Verify-Flag
      --   $17-$18/23-24           Last temporary String Address
      --   $90/144                 Kernal I/O Status Word ST
      --   $91/145                 Flag: $7F = STOP key
      --   $92/146                 Timing Constant for Tape
      --   $93/147                 Flag: 0 = Load, 1 = Verify
      --   $94/148                 Flag: Serial Bus - Output Character buffered
      --   $95/149                 Buffered Character for Serial Bus
      --   $96/150                 Cassette Sync number
      --   $97/151                 Storage of X Register during CHRIN
      --   $97/151                 Storage of Y Register during RS232 fetch
      --   $98/152                 Number of Open Files/Index to File Table
      --   $99/153                 Default Input Device (0)
      --   $9A/154                 Default Output Device (3)
      --   $9B/155                 Parity of Byte Output to Tape
      --   $9C/156                 Flag: Byte received from Tape
      --   $9D/157                 Error-Mode-Flag
      --   $9E/158                 Index to Tape File name/Header ID for Tape write
      --   $9E/158                 Tape Error log pass 1
      --   $9F/159                 Tape Error log pass 2
      --   $A0-$A2/160-162         Real-time jiffy Clock
      --   $A3/163                 Bit Counter Tape Read or Write
      --   $A3/163                 Serial Bus EOI (End Of Input) Flag
      --   $A4/164                 Pulse Counter Tape Read or Write
      --   $A4/164                 Serial Bus shift Counter
      --   $A5/165                 Tape Synchronising count down
      --   $A6/166                 Pointer: Tape I/O buffer
      --   $A7/167                 RS232 temporary for received Bit
      --   $A7/167                 Tape temporary
      --   $A8/168                 RS232 Input Bit count/Tape temporary
      --   $A9/169                 RS232 Flag: Start Bit check/Tape temporary
      --   $AA/170                 RS232 Input Byte Buffer/Tape temporary
      --   $AB/171                 RS232 Input parity/Tape temporary
      --   $AC-$AD/172-173         Pointer: Tape Buffer/Screen scrolling
      --   $B0-$B1/176-177         Tape timing Constants
      --   $B2-$B3/178-179         Pointer: Start Address of Tape Buffer
      --   $B4/180                 RS232 Write bit count/Tape Read timing Flag
      --   $B5/181                 RS232 Next Bit to send/Tape Read - End of Tape
      --   $B6/182                 RS232 Output Byte Buffer/Tape Read Error Flag
      --   $B7/183                 Number of Characters in Filename
      --   $B8/184                 Current File - Logical File number
      --   $B9/185                 Current File - Secondary Address
      --   $BA/186                 Current File - First Address (Device number)
      --   $BB-$BC/187-188         Pointer: Current File name Address
      --   $BD/189                 RS232 Output Parity/Tape Byte temporary
      --   $BE/190                 Tape Input/Output Block count
      --   $BF/191                 Serial Word Buffer
      --   $C0/192                 Tape Motor Switch
      --   $C1-$C2/193-194         Start Address for LOAD and Cassette Write
      --   $C3-$C4/195-196         Pointer: Type 3 Tape LOAD and general use
      --   $C5/197                 Matrix value of last Key pressed
      --   $C7/199                 Flag: Reverse On/Off
      --   $C8/200                 Pointer: End of Line for Input
      --   $C9-$CA/201-202         Cursor X/Y position at start of Input
      --   $CB/203                 Matrix value of last Key pressed
      --   $CC/204                 Flag: Cursor blink
      --   $CD/205                 Timer: Count down for Cursor blink toggle
      --   $CE/206                 Character under Cursor while Cursor Inverted
      --   $CF/207                 Flag: Cursor Status
      --   $D0/208                 Flag: Input from ...
      --   $D1-$D2/209-210         Pointer: Current Screen Line Address
      --   $D3/211                 Cursor Column on current Line
      --   $D4/212                 Flag: Editor in Quote Mode
      --   $D5/213                 Current logical Line length: 39 or 79
      --   $D6/214                 Current Screen Line number of Cursor
      --   $D7/215                 Current Input Character/Last Character Output
      --   $D8/216                 Count of number of inserts outstanding
      --   $D9-$F2/217-242         Screen Line Link Table
      --   $F3-$F4/243-244         Pointer: Current Color RAM Location
      --   $F5-$F6/245-246         Vector: Current Keyboard decoding Table
      --   $F7-$F8/247-248         RS232 Input Buffer Pointer
      --   $F9-$FA/249-250         RS232 Output Buffer Pointer
      --   $0259-$0262/601-610     Active logical File numbers
      --   $0263-$026C/611-620     Active File First Addresses (Device numbers)
      --   $026D-$0276/621-630     Active File Secondary Addresses
      --   $0281-$0282/641-642     Pointer: Bottom of Memory for Operating System
      --   $0283-$0284/643-644     Pointer: Top of Memory for Operating System
      --   $0285/645               Serial IEEE Bus timeout defeat Flag
      --   $0286/646               Current Character Color code
      --   $0287/647               Background Color under Cursor
      --   $0288/648               High Byte of Screen Memory Address
      --   $0289/649               Maximum number of Bytes in Keyboard Buffer
      --   $028A/650               Flag: Repeat keys
      --   $028B/651               Repeat Key: Speed Counter
      --   $028C/652               Repeat Key: First repeat delay Counter
      --   $028D/653               Flag: Shift Keys
      --   $028E/654               Last Shift Key used for debouncing
      --   $028F-$0290/655-656     Vector: Routine to determine Keyboard table
      --   $0291/657               Flag: Upper/Lower Case change
      --   $0292/658               Flag: Auto scroll down
      --   $0293/659               RS232 Pseudo 6551 control Register Image
      --   $0294/660               RS232 Pseudo 6551 command Register Image
      --   $0295-$0296/661-662     RS232 Non-standard Bits/Second
      --   $0297/663               RS232 Pseudo 6551 Status Register Image
      --   $0298/664               RS232 Number of Bits left to send
      --   $0299-$029A/665-666     RS232 Baud Rate; Full Bit time microseconds
      --   $029B/667               RS232 Index to End of Input Buffer
      --   $029C/668               RS232 Pointer: High Byte of Input Buffer
      --   $029D/669               RS232 Pointer: High Byte of Output Buffer
      --   $029E/670               RS232 Index to End of Output Buffer
      --   $029F-$02A0/671-672     Temp. store for IRQ Vector during Tape I/O
      --   $02A1/673               RS232 Enables
      --   $02A2/674               TOD sense during Tape I/O
      --   $02A3/675               Temporary storage during Tape READ
      --   $02A4/676               Temporary D1IRQ Indicator during Tape READ
      --   $02A5/677               Temporary for Line Index
      --   $02A6/678               Flag: TV Standard
      --   $033C-$03FB/828-1019    Tape I/O Buffer

      -- based on http://unusedino.de/ec64/technical/aay/c64/zpmain.htm
    }
  |]
