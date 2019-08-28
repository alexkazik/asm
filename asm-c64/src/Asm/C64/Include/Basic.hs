{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}

module Asm.C64.Include.Basic
  ( basic
  ) where

import           Asm.Cpu6502

basic :: Asm
basic =
  [asm|
    namespace basic
      pointer byte[0x400] screen = 0x0400
      pointer byte[0x7800] area = 0x0800
      pointer byte[0x2000] rom = 0xa000

      -- RAM used by BASIC

      pointer _lohi start_variables = 0x2d
      pointer _lohi start_arrays = 0x2f
      pointer _lohi end_arrays = 0x31

      --   $03-$04/3-4             Jump Vector: Convert FAC to Integer
      --   $05-$06/5-6             Jump Vector: Convert Integer to Floating point
      --   $07/7                   Search Character/Temporary Integer during INT
      --   $08/8                   Flag: Scan for Quote at end of String
      --   $07-$08/7-8             Temporary Integer during OR/AND
      --   $09/9                   Screen Column for last TAB
      --   $0B/11                  Input Buffer Pointer/Number of Subscripts
      --   $0C/12                  Flag: Default Array dimension
      --   $0D/13                  Data type Flag
      --   $0E/14                  Data type Flag
      --   $0F/15                  Flag: DATA scan/List Quote/Garbage collection
      --   $10/16                  Flag: Subscript reference/User Function call
      --   $11/17                  Input Flag
      --   $12/18                  Flag: TAN sign/Comparative result
      --   $13/19                  File number of current Input Device
      --   $14-$15/20-21           Temporary: Integer value
      --   $16/22                  Pointer: Temporary String Stack
      --   $17-$18/23-24           Last temporary String Address
      --   $19-$21/25-33           Stack for temporary Strings (NEITHER BASIC NOR KERNAL)
      --   $22-$25/34-37           Utility Pointer Area
      --   $22-$23/34-35           First Utility Pointer
      --   $24-$25/36-37           Second Utility Pointer
      --   $26-$2A/38-42           Floating point product of Multiply and Divide
      --   $2B-$2C/43-44           Pointer: Start of BASIC Text Area
      --   $33-$34/51-52           Pointer: Bottom of String space
      --   $35-$36/53-54           Utility String Pointer
      --   $37-$38/55-56           Pointer: Highest Address available to BASIC
      --   $39-$3A/57-58           Current BASIC Line number
      --   $3B-$3C/59-60           Previous BASIC Line number
      --   $3D-$3E/61-62           Pointer: BASIC Statement for CONT
      --   $3F-$40/63-64           Current DATA Line number
      --   $41-$42/65-66           Pointer: Used by READ - current DATA Item
      --   $43-$44/67-68           Pointer: Temporary storage during INPUT
      --   $45-$46/69-70           Name of Variable being sought in Variable Table
      --   $47-$48/71-72           Value/Descriptor-Pointer
      --   $49-$4A/73-74           Pointer: Index Variable for FOR/NEXT loop
      --   $4B-$4C/75-76           Temporary storage for TXTPTR
      --   $4D/77                  Mask used during FRMEVL
      --   $4E-$52/78-82           Temporary storage for FLPT value
      --   $53/83                  Length of String during Garbage collection
      --   $54-$56/84-86           Jump Vector used in Function Evaluation
      --   $57-$5B/87-91           Temporary storage for FLPT value
      --   $5C-$60/92-96           Temporary storage for FLPT value
      --   $61-$66/97-102          Main Floating point Accumulator
      --   $61/97                  FAC Exponent
      --   $62-$65/98-101          FAC Mantissa
      --   $66/102                 FAC Sign
      --   $67/103                 Pointer: Series Evaluation Constant
      --   $68/104                 Bit Overflow Area during normalisation Routine
      --   $69-$6E/105-110         Auxiliary Floating point Accumulator
      --   $69/105                 AFAC Exponent
      --   $6A-$6D/106-109         AFAC Mantissa
      --   $6E/110                 AFAC Sign
      --   $6F/111                 Sign of result of Arithmetic Evaluation
      --   $70/112                 FAC low-order rounding
      --   $71-$72/113-114         Pointer: Used during CRUNCH/ASCII conversion
      --   $73-$78/115-120         CHRGET: Get next Byte of BASIC Text
      --   $79-$8A/121-138         CHRGOT: Get same Byte again
      --   $7A-$7B/122-123         Pointer: Current Byte of BASIC Text
      --   $8B-$8F/139-143         Floating RND Function Seed Value (NEITHER BASIC NOR KERNAL)
      --   $FF/255                 BASIC temporary Data Area
      --   $FF-$010A/255-266       Assembly Area for Floating point to ASCII
      --   $0200-$0258/512-600     BASIC Input Buffer (Input Line from Screen)
      --   $0300-$0301/768-769     Vector: BASIC Error Message
      --   $0302-$0303/770-771     Vector: BASIC Input Line and Decode
      --   $0304-$0305/772-773     Vector: BASIC Tokenise Routine
      --   $0306-$0307/774-775     Vector: BASIC LIST Routine
      --   $0308-$0309/776-777     Vector: BASIC Character dispatch Routine
      --   $030A-$030B/778-779     Vector: BASIC Token evaluation
      --   $030C/780               Storage for 6510 Accumulator during SYS
      --   $030D/781               Storage for 6510 X-Register during SYS
      --   $030E/782               Storage for 6510 Y-Register during SYS
      --   $030F/783               Storage for 6510 Status Register during SYS
      --   $0310/784               USR Function JMP Instruction
      --   $0311-$0312/785-786     USR Address

      -- based on http://unusedino.de/ec64/technical/aay/c64/zpmain.htm
    }
  |]
