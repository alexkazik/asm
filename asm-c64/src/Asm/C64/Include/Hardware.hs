{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}

module Asm.C64.Include.Hardware
  ( hardware
  ) where

import           Asm.Cpu6502

hardware :: Asm
hardware =
  [asm|
    -- vic
    type struct {
      _xy[8] sprite;
      byte sprite_x_msb;
      byte control1;
      byte raster;
      _xy latch;
      byte sprite_enable;
      byte control2;
      byte sprite_ydouble;
      byte memory;
      byte irr;
      byte imr;
      byte sprite_prio;
      byte sprite_mc;
      byte sprite_xdouble;
      byte sprite_sprite_coll;
      byte sprite_back_coll;
      byte border_color;
      byte background_color;
      byte[2] multicolor;
      byte background_col3;
      byte[2] sprite_multicolor;
      byte[8] sprite_color;
    } vicType
    pointer vicType vic = 0xd000
    -- cia
    type struct {
      byte porta;
      byte portb;
      byte ddra;
      byte ddrb;
      _lohi timera;
      _lohi timerb;
      struct {
        byte tenth;
        byte seconds;
        byte minutes;
        byte hours;
      } tod;
      byte byffer;
      byte icr;
      byte controla;
      byte controlb;
    } ciaType
    pointer ciaType cia1 = 0xdc00
    pointer ciaType cia2 = 0xdd00
    -- sid
    type struct {
      _lohi freq;
      _lohi pulse;
      byte control;
      byte attack;
      byte sustain;
    } __sid_voice
    type struct {
      __sid_voice voice1;
      __sid_voice voice2;
      __sid_voice voice3;
      _lohi cutoff;
      byte frc_vic;
      byte sfmv;
      byte potx;
      byte poty;
      byte osc3out;
      byte eg3out;
    } sidType
    pointer sidType sid = 0xd400

    pointer byte[0x400] color_memory = 0xd800
  |]
