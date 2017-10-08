set meta.cpu = #6502
pool out[basic_startup,prog,data] = 0x0801
pool reloc = 0x0002

const byte[] image = $[imageRle] @data

$(c64include)

block basicStart @basic_startup
    data addr(next_line) & 0xff
    data addr(next_line) >> 8
    data 0x38
    data 0x80
    data 0x9e
    data 0x30 + addr(start.run) / 1000
    data 0x30 + (addr(start.run) / 100) % 10
    data 0x30 + (addr(start.run) / 10) % 10
    data 0x30 + addr(start.run) % 10
    data 0x00
  next_line:
    data 0x00
    data 0x00
end

block start @prog
  const pool relocated = reloc @prog -- store the whole pool "reloc" in the constant "relocated", which is placed in the pool "prog"
  run:
    sei

    ldx size(reloc)-1
  loop:
    lda relocated, x
    sta reloc, x
    dex
    bpl loop

    lda $black4
    sta vic.border_color

    jsr unrle.run

    lda 0x16 // use $4000..$7fff for graphics
    sta cia2.porta

    lda 0xbb
    sta vic.control1

    lda 0x08
    sta vic.control2

    lda 0b0111.1??? // (0b0111.xxxx) use $5c00..$5fff for color and (0bxxxx.1xxx) use $6000..$7fff for bitmap
    sta vic.memory

  infinite:
    jmp infinite
end

$block (unrle6502 "image" 0x5c00) unrle @reloc
