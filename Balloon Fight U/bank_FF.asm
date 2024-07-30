; Disassembly created and organized using BZK 6502 Disassembler and LuaScripts by Cyneprepou4uk
; Comments and labels taken from original Balloon Fight (USA) NES Disassembly by LuigiBlood, built using ARM9's bass fork
; Additional contributions made by Cyneprepou4uk
; v1.0 release: Tuesday, May 21, 2024 by Quick Curly
; Nothing provided is guaranteed to be 100% error-free. If something needs to be fixed, please contact the appropriate person/people to
; bring it to their attention, and/or feel free to contribute to make this disassembly more accurate and better. Thank you.

.segment "BANK_FF"
.include "bank_ram.inc"
.org $C000  ; for listing file
; 0x000010-0x00400F

vec_C000_RESET:
C - - - - - 0x000010 00:C000: A9 00     LDA #$00                                                   ; Initialize PPU registers
C - - - - - 0x000012 00:C002: 8D 00 20  STA $2000
C - - - - - 0x000015 00:C005: 8D 01 20  STA $2001
bra_C008:
C - - - - - 0x000018 00:C008: AD 02 20  LDA $2002                                                  ; Get to next V-Blank
C - - - - - 0x00001B 00:C00B: 10 FB     BPL bra_C008
bra_C00D:
C - - - - - 0x00001D 00:C00D: AD 02 20  LDA $2002
C - - - - - 0x000020 00:C010: 30 FB     BMI bra_C00D
bra_C012:
C - - - - - 0x000022 00:C012: AD 02 20  LDA $2002
C - - - - - 0x000025 00:C015: 10 FB     BPL bra_C012
C - - - - - 0x000027 00:C017: 78        SEI                                                        ; Disable Interrupts
C - - - - - 0x000028 00:C018: D8        CLD                                                        ; Clear Decimal Mode
C - - - - - 0x000029 00:C019: A2 FF     LDX #$FF                                                   ; Initialize stack pointer
C - - - - - 0x00002B 00:C01B: 9A        TXS                                                        ; to $01FF
C - - - - - 0x00002C 00:C01C: A2 12     LDX #$12                                                   ; Clear RAM $0012-$00FF
C - - - - - 0x00002E 00:C01E: A9 00     LDA #$00
bra_C020:
C - - - - - 0x000030 00:C020: 95 00     STA ram_0000_ppuctrl_shadow,X
C - - - - - 0x000032 00:C022: E8        INX
C - - - - - 0x000033 00:C023: D0 FB     BNE bra_C020
C - - - - - 0x000035 00:C025: A2 02     LDX #$02
bra_C027:
C - - - - - 0x000037 00:C027: BD FA 07  LDA ram_07FA_hal_reset_check,X                             ; Check if system was reset
C - - - - - 0x00003A 00:C02A: DD 82 C0  CMP tbl_C082_HAL,X                                         ; by checking if $07FA has "HAL" string
C - - - - - 0x00003D 00:C02D: D0 05     BNE bra_C034
C - - - - - 0x00003F 00:C02F: CA        DEX                                                        ; If found, then skip to the end
C - - - - - 0x000040 00:C030: 10 F5     BPL bra_C027                                               ; Else, proceed with initialization code
- - - - - - 0x000042 00:C032: 30 43     BMI bra_C077
bra_C034:
C - - - - - 0x000044 00:C034: A2 00     LDX #$00
C - - - - - 0x000046 00:C036: 8A        TXA                                                        ; Initialize parts of RAM
bra_C037:
C - - - - - 0x000047 00:C037: 95 00     STA ram_0000_ppuctrl_shadow,X                              ; $0000-$00FF: Main RAM
C - - - - - 0x000049 00:C039: 9D 00 07  STA ram_0700_trip_rank_score,X                             ; $0700-$07FF: Balloon Trip RAM
C - - - - - 0x00004C 00:C03C: E8        INX
C - - - - - 0x00004D 00:C03D: D0 F8     BNE bra_C037
C - - - - - 0x00004F 00:C03F: A9 32     LDA #$32                                                   ; Initialize Balloon Trip ranking scores
C - - - - - 0x000051 00:C041: 85 15     STA ram_0015_temp_player_x_trip_rank_score
bra_C043:
C - - - - - 0x000053 00:C043: A9 32     LDA #$32                                                   ; Add +50 to score
C - - - - - 0x000055 00:C045: 20 DE D6  JSR sub_D6DE_Score_Add                                     ; and update
C - - - - - 0x000058 00:C048: A9 00     LDA #$00
C - - - - - 0x00005A 00:C04A: 85 46     STA ram_0046_status_bar_update_flag                        ; Clear status bar update flag
C - - - - - 0x00005C 00:C04C: 20 79 C5  JSR sub_C579_Rank_Score_Update                             ; Update Balloon Trip Rank 01 to 50 scores
C - - - - - 0x00005F 00:C04F: C6 15     DEC ram_0015_temp_player_x_trip_rank_score                 ; with multiples of 50
C - - - - - 0x000061 00:C051: D0 F0     BNE bra_C043
C - - - - - 0x000063 00:C053: A2 0E     LDX #$0E
bra_C055:
C - - - - - 0x000065 00:C055: BD 85 C0  LDA tbl_C085,X                                             ; Write default high scores
C - - - - - 0x000068 00:C058: 9D 29 06  STA ram_0629_highest_score_1p_game,X                       ; for each game mode
C - - - - - 0x00006B 00:C05B: CA        DEX
C - - - - - 0x00006C 00:C05C: 10 F7     BPL bra_C055
C - - - - - 0x00006E 00:C05E: A2 04     LDX #$04
bra_C060:
C - - - - - 0x000070 00:C060: A9 00     LDA #$00                                                   ; Initialize Player 1 score
C - - - - - 0x000072 00:C062: 95 03     STA ram_0003_p1_score_0000x,X
C - - - - - 0x000074 00:C064: CA        DEX
C - - - - - 0x000075 00:C065: 10 F9     BPL bra_C060
C - - - - - 0x000077 00:C067: A9 00     LDA #$00
C - - - - - 0x000079 00:C069: 20 DE D6  JSR sub_D6DE_Score_Add                                     ; Update score
C - - - - - 0x00007C 00:C06C: A2 02     LDX #$02
bra_C06E:
C - - - - - 0x00007E 00:C06E: BD 82 C0  LDA tbl_C082_HAL,X                                         ; Write "HAL" to $07FA
C - - - - - 0x000081 00:C071: 9D FA 07  STA ram_07FA_hal_reset_check,X                             ; for future reset checking
C - - - - - 0x000084 00:C074: CA        DEX
C - - - - - 0x000085 00:C075: 10 F7     BPL bra_C06E
bra_C077:
C - - - - - 0x000087 00:C077: A9 1E     LDA #$1E                                                   ; PPUMASK Shadow
C - - - - - 0x000089 00:C079: 85 01     STA ram_0001_ppumask_shadow                                ; Enable background and sprites
C - - - - - 0x00008B 00:C07B: A9 90     LDA #$90                                                   ; PPUCTRL Shadow
C - - - - - 0x00008D 00:C07D: 85 00     STA ram_0000_ppuctrl_shadow                                ; Enable NMI at V-Blank, background pattern table at $1000
C - - - - - 0x00008F 00:C07F: 4C D4 F1  JMP loc_F1D4                                               ; Start
tbl_C082_HAL:
- D 2 - - - 0x000092 00:C082: 48        .byte $48, $41, $4C
tbl_C085:
- D 2 - - - 0x000095 00:C085: 00        .byte $00, $00, $00, $01, $00                              ; 1 Player Game: 010000 Points
- D 2 - - - 0x00009A 00:C08A: 00        .byte $00, $00, $00, $01, $00                              ; 2 Player Game: 010000 Points
- D 2 - - - 0x00009F 00:C08F: 00        .byte $00, $00, $05, $02, $00                              ;  Balloon Trip: 025000 Points
vec_C094_NMI:
C - - - - - 0x0000A4 00:C094: 48        PHA                                                        ; Push A
C - - - - - 0x0000A5 00:C095: 8A        TXA
C - - - - - 0x0000A6 00:C096: 48        PHA                                                        ; Push X
C - - - - - 0x0000A7 00:C097: 98        TYA
C - - - - - 0x0000A8 00:C098: 48        PHA                                                        ; Push Y
C - - - - - 0x0000A9 00:C099: A9 00     LDA #$00
C - - - - - 0x0000AB 00:C09B: 8D 03 20  STA $2003                                                  ; Upload OAM Buffer
C - - - - - 0x0000AE 00:C09E: A9 02     LDA #$02                                                   ; $0200-$02FF to OAM (via DMA)
C - - - - - 0x0000B0 00:C0A0: 8D 14 40  STA $4014
C - - - - - 0x0000B3 00:C0A3: A5 52     LDA ram_0052_ppu_upload_buffer_pos                         ; Check for PPU Buffer Upload
C - - - - - 0x0000B5 00:C0A5: C5 53     CMP ram_0053_ppu_upload_buffer_size                        ; If position in Buffer
C - - - - - 0x0000B7 00:C0A7: F0 03     BEQ bra_C0AC                                               ; does not equal Buffer Size
C - - - - - 0x0000B9 00:C0A9: 20 7C C1  JSR sub_C17C_Upload_PPU_Buffer                             ; Then upload PPU Buffer
bra_C0AC:
C - - - - - 0x0000BC 00:C0AC: 20 0D D6  JSR sub_D60D_Update_Star_BG_Animation                      ; Update star animation
C - - - - - 0x0000BF 00:C0AF: 20 98 D7  JSR sub_D798_Update_Status_Bar                             ; Update status bar
C - - - - - 0x0000C2 00:C0B2: E6 19     INC ram_0019_f_counter                                     ; Increment frame counter
C - - - - - 0x0000C4 00:C0B4: A9 20     LDA #$20                                                   ; PPU Address $2000
C - - - - - 0x0000C6 00:C0B6: 8D 06 20  STA $2006                                                  ; (Nametable 0)
C - - - - - 0x0000C9 00:C0B9: A9 00     LDA #$00
C - - - - - 0x0000CB 00:C0BB: 8D 06 20  STA $2006
C - - - - - 0x0000CE 00:C0BE: A9 00     LDA #$00                                                   ; PPU scroll = X: 0, Y: 0
C - - - - - 0x0000D0 00:C0C0: 8D 05 20  STA $2005
C - - - - - 0x0000D3 00:C0C3: 8D 05 20  STA $2005
C - - - - - 0x0000D6 00:C0C6: 20 F7 FF  JSR sub_FFF7                                               ; Manage audio
C - - - - - 0x0000D9 00:C0C9: A9 01     LDA #$01                                                   ; Set video frame done flag
C - - - - - 0x0000DB 00:C0CB: 85 02     STA ram_0002_video_processed_flag
C - - - - - 0x0000DD 00:C0CD: A5 16     LDA ram_0016_game_mode                                     ; If game mode is Balloon Fight mode
C - - - - - 0x0000DF 00:C0CF: F0 20     BEQ bra_C0F1                                               ; then end NMI
bra_C0D1:
C - - - - - 0x0000E1 00:C0D1: AD 02 20  LDA $2002                                                  ; Wait for V-Blank end
C - - - - - 0x0000E4 00:C0D4: 30 FB     BMI bra_C0D1
C - - - - - 0x0000E6 00:C0D6: A2 04     LDX #$04
C - - - - - 0x0000E8 00:C0D8: A0 C6     LDY #$C6
bra_C0DA:
C - - - - - 0x0000EA 00:C0DA: 88        DEY                                                        ; Wait 6125 cycles
C - - - - - 0x0000EB 00:C0DB: D0 FD     BNE bra_C0DA                                               ; for updating the scrolling
C - - - - - 0x0000ED 00:C0DD: CA        DEX                                                        ; mid-frame (under scoreboard)
C - - - - - 0x0000EE 00:C0DE: D0 FA     BNE bra_C0DA
C - - - - - 0x0000F0 00:C0E0: A5 18     LDA ram_0018_ppuctrl_shadow_trip
C - - - - - 0x0000F2 00:C0E2: 05 00     ORA ram_0000_ppuctrl_shadow                                ; PPUCTRL = [$0018] | [$0000]
C - - - - - 0x0000F4 00:C0E4: 8D 00 20  STA $2000
C - - - - - 0x0000F7 00:C0E7: A5 17     LDA ram_0017_ppuscroll_shadow_trip
C - - - - - 0x0000F9 00:C0E9: 8D 05 20  STA $2005                                                  ; Input X scroll value
C - - - - - 0x0000FC 00:C0EC: A9 00     LDA #$00                                                   ; PPUSCROLL = X: [$0017], Y: 0
C - - - - - 0x0000FE 00:C0EE: 8D 05 20  STA $2005
bra_C0F1:
C - - - - - 0x000101 00:C0F1: 68        PLA                                                        ; Pull Y
C - - - - - 0x000102 00:C0F2: A8        TAY
C - - - - - 0x000103 00:C0F3: 68        PLA                                                        ; Pull X
C - - - - - 0x000104 00:C0F4: AA        TAX
C - - - - - 0x000105 00:C0F5: 68        PLA                                                        ; Pull A
C - - - - - 0x000106 00:C0F6: 40        RTI
vec_C0F7_IRQ:
loc_C0F7:
- - - - - - 0x000107 00:C0F7: 4C F7 C0  JMP loc_C0F7                                               ; Loop
sub_C0FA_Disable_NMI:
C - - - - - 0x00010A 00:C0FA: A5 00     LDA ram_0000_ppuctrl_shadow
C - - - - - 0x00010C 00:C0FC: 29 7F     AND #$7F                                                   ; Disable NMI
bra_C0FE:
C - - - - - 0x00010E 00:C0FE: 8D 00 20  STA $2000
C - - - - - 0x000111 00:C101: 85 00     STA ram_0000_ppuctrl_shadow                                ; Update PPUCTRL
C - - - - - 0x000113 00:C103: 60        RTS
sub_C104_Enable_NMI:
C - - - - - 0x000114 00:C104: A5 00     LDA ram_0000_ppuctrl_shadow
C - - - - - 0x000116 00:C106: 09 80     ORA #$80                                                   ; Enable NMI
C - - - - - 0x000118 00:C108: D0 F4     BNE bra_C0FE
sub_C10A_Clear_PPU_Mask:
C - - - - - 0x00011A 00:C10A: A9 00     LDA #$00                                                   ; Clear PPUMASK
bra_C10C:
C - - - - - 0x00011C 00:C10C: 48        PHA
C - - - - - 0x00011D 00:C10D: 20 65 F4  JSR sub_F465_Clear_F_Flag                                  ; Clear Frame Flag
C - - - - - 0x000120 00:C110: 68        PLA
C - - - - - 0x000121 00:C111: 8D 01 20  STA $2001                                                  ; Update PPUMASK
C - - - - - 0x000124 00:C114: 60        RTS
sub_C115:
loc_C115:
C D 2 - - - 0x000125 00:C115: A5 01     LDA ram_0001_ppumask_shadow                                ; Write PPUMASK Shadow to PPUMASK
C - - - - - 0x000127 00:C117: D0 F3     BNE bra_C10C
sub_C119:
loc_C119:
C D 2 - - - 0x000129 00:C119: 20 54 C1  JSR sub_C154
C - - - - - 0x00012C 00:C11C: A0 00     LDY #$00
bra_C11E:
C - - - - - 0x00012E 00:C11E: B9 57 00  LDA ram_0057_ppu_buffer_upload_data,Y                      ; Put PPU data
C - - - - - 0x000131 00:C121: 9D 00 03  STA ram_0300_ppu_upload_buffer_blocks,X                    ; to upload to Nametable 1
C - - - - - 0x000134 00:C124: E8        INX
C - - - - - 0x000135 00:C125: C8        INY
C - - - - - 0x000136 00:C126: C4 56     CPY ram_0056_size_of_upload_to_ppu_buffer
C - - - - - 0x000138 00:C128: D0 F4     BNE bra_C11E
C - - - - - 0x00013A 00:C12A: 86 53     STX ram_0053_ppu_upload_buffer_size                        ; Update PPU Buffer Size
C - - - - - 0x00013C 00:C12C: 60        RTS
sub_C12D_Copy_PPU_Temp_Block:
loc_C12D_Copy_PPU_Temp_Block:
C D 2 - - - 0x00013D 00:C12D: A9 57     LDA #< ram_0057_ppu_buffer_upload_data
C - - - - - 0x00013F 00:C12F: A0 00     LDY #> ram_0057_ppu_buffer_upload_data                     ; [$0021] = $0057
sub_C131_Copy_PPU_Block:
C D 2 - - - 0x000141 00:C131: 85 21     STA ram_0021_hi_score_pointer_21                           ; Update pointer
C - - - - - 0x000143 00:C133: 84 22     STY ram_0022_hi_score_pointer_21                           ; [$0021] = $YYAA
C - - - - - 0x000145 00:C135: 8A        TXA
C - - - - - 0x000146 00:C136: 48        PHA
C - - - - - 0x000147 00:C137: A0 02     LDY #$02
C - - - - - 0x000149 00:C139: B1 21     LDA (ram_0021_hi_score_pointer_21),Y                       ; Get data size + 3
C - - - - - 0x00014B 00:C13B: 18        CLC                                                        ; to include address and size info
C - - - - - 0x00014C 00:C13C: 69 03     ADC #$03
C - - - - - 0x00014E 00:C13E: 85 12     STA ram_0012_temp
C - - - - - 0x000150 00:C140: A6 53     LDX ram_0053_ppu_upload_buffer_size                        ; Get PPU Buffer Size
C - - - - - 0x000152 00:C142: A0 00     LDY #$00
bra_C144:
C - - - - - 0x000154 00:C144: B1 21     LDA (ram_0021_hi_score_pointer_21),Y                       ; Copy PPU Buffer Block
C - - - - - 0x000156 00:C146: 9D 00 03  STA ram_0300_ppu_upload_buffer_blocks,X
C - - - - - 0x000159 00:C149: E8        INX
C - - - - - 0x00015A 00:C14A: C8        INY
C - - - - - 0x00015B 00:C14B: C4 12     CPY ram_0012_temp
C - - - - - 0x00015D 00:C14D: D0 F5     BNE bra_C144
C - - - - - 0x00015F 00:C14F: 86 53     STX ram_0053_ppu_upload_buffer_size                        ; Update PPU Buffer Size
C - - - - - 0x000161 00:C151: 68        PLA
C - - - - - 0x000162 00:C152: AA        TAX
C - - - - - 0x000163 00:C153: 60        RTS
sub_C154:
C - - - - - 0x000164 00:C154: A6 53     LDX ram_0053_ppu_upload_buffer_size                        ; X = PPU Buffer Size
C - - - - - 0x000166 00:C156: A9 00     LDA #$00
C - - - - - 0x000168 00:C158: 85 12     STA ram_0012_temp
C - - - - - 0x00016A 00:C15A: A5 55     LDA ram_0055_temp_cloud_flip_y
C - - - - - 0x00016C 00:C15C: 0A        ASL                                                        ; Prepare PPUADDR
C - - - - - 0x00016D 00:C15D: 0A        ASL                                                        ; [$0055] = 000XXDDD
C - - - - - 0x00016E 00:C15E: 0A        ASL
C - - - - - 0x00016F 00:C15F: 0A        ASL                                                        ; PPUADDR_H = 001000XX
C - - - - - 0x000170 00:C160: 26 12     ROL ram_0012_temp                                          ; PPUADDR_L = DDD00000 | [$0054]
C - - - - - 0x000172 00:C162: 0A        ASL
C - - - - - 0x000173 00:C163: 26 12     ROL ram_0012_temp
C - - - - - 0x000175 00:C165: 05 54     ORA ram_0054_temp_cloud_flip_x
C - - - - - 0x000177 00:C167: 48        PHA
C - - - - - 0x000178 00:C168: A5 12     LDA ram_0012_temp
C - - - - - 0x00017A 00:C16A: 09 20     ORA #$20                                                   ; Put PPUADDR high byte
C - - - - - 0x00017C 00:C16C: 9D 00 03  STA ram_0300_ppu_upload_buffer_blocks,X                    ; {From Nametable 1)
C - - - - - 0x00017F 00:C16F: E8        INX
C - - - - - 0x000180 00:C170: 68        PLA                                                        ; Put PPUADDR low byte
C - - - - - 0x000181 00:C171: 9D 00 03  STA ram_0300_ppu_upload_buffer_blocks,X
C - - - - - 0x000184 00:C174: E8        INX
C - - - - - 0x000185 00:C175: A5 56     LDA ram_0056_size_of_upload_to_ppu_buffer                  ; Put Upload Size
C - - - - - 0x000187 00:C177: 9D 00 03  STA ram_0300_ppu_upload_buffer_blocks,X
C - - - - - 0x00018A 00:C17A: E8        INX                                                        ; Return:
C - - - - - 0x00018B 00:C17B: 60        RTS                                                        ; X = Current PPU Buffer Address
sub_C17C_Upload_PPU_Buffer:
loc_C17C_Upload_PPU_Buffer:
C D 2 - - - 0x00018C 00:C17C: 98        TYA
C - - - - - 0x00018D 00:C17D: 48        PHA                                                        ; Push Y
C - - - - - 0x00018E 00:C17E: 8A        TXA
C - - - - - 0x00018F 00:C17F: 48        PHA                                                        ; Push X
C - - - - - 0x000190 00:C180: 20 88 C1  JSR sub_C188
C - - - - - 0x000193 00:C183: 68        PLA                                                        ; Pull X
C - - - - - 0x000194 00:C184: AA        TAX
C - - - - - 0x000195 00:C185: 68        PLA                                                        ; Pull Y
C - - - - - 0x000196 00:C186: A8        TAY
C - - - - - 0x000197 00:C187: 60        RTS
bra_C188:
sub_C188:
C - - - - - 0x000198 00:C188: A6 52     LDX ram_0052_ppu_upload_buffer_pos                         ; Get current position in PPU Buffer Upload
C - - - - - 0x00019A 00:C18A: BD 00 03  LDA ram_0300_ppu_upload_buffer_blocks,X
C - - - - - 0x00019D 00:C18D: E8        INX
C - - - - - 0x00019E 00:C18E: 85 50     STA ram_0050_star_anim_ppu_lo
C - - - - - 0x0001A0 00:C190: 8D 06 20  STA $2006                                                  ; Get PPU Address
C - - - - - 0x0001A3 00:C193: BD 00 03  LDA ram_0300_ppu_upload_buffer_blocks,X                    ; And set PPUADDR
C - - - - - 0x0001A6 00:C196: E8        INX
C - - - - - 0x0001A7 00:C197: 8D 06 20  STA $2006
C - - - - - 0x0001AA 00:C19A: BC 00 03  LDY ram_0300_ppu_upload_buffer_blocks,X                    ; Get Upload Size to Y
C - - - - - 0x0001AD 00:C19D: E8        INX
bra_C19E:
C - - - - - 0x0001AE 00:C19E: BD 00 03  LDA ram_0300_ppu_upload_buffer_blocks,X
C - - - - - 0x0001B1 00:C1A1: E8        INX
C - - - - - 0x0001B2 00:C1A2: 8D 07 20  STA $2007                                                  ; Upload Y bytes to PPU
C - - - - - 0x0001B5 00:C1A5: 88        DEY
C - - - - - 0x0001B6 00:C1A6: D0 F6     BNE bra_C19E
C - - - - - 0x0001B8 00:C1A8: A5 50     LDA ram_0050_star_anim_ppu_lo
C - - - - - 0x0001BA 00:C1AA: C9 3F     CMP #$3F                                                   ; If Upload Address != $3FXX (Palette Data)
C - - - - - 0x0001BC 00:C1AC: D0 10     BNE bra_C1BE                                               ; Then skip ahead
C - - - - - 0x0001BE 00:C1AE: A9 3F     LDA #$3F
C - - - - - 0x0001C0 00:C1B0: 8D 06 20  STA $2006
C - - - - - 0x0001C3 00:C1B3: A9 00     LDA #$00                                                   ; PPUADDR = $3F00
C - - - - - 0x0001C5 00:C1B5: 8D 06 20  STA $2006                                                  ; PPUADDR = $0000
C - - - - - 0x0001C8 00:C1B8: 8D 06 20  STA $2006
C - - - - - 0x0001CB 00:C1BB: 8D 06 20  STA $2006
bra_C1BE:
C - - - - - 0x0001CE 00:C1BE: 86 52     STX ram_0052_ppu_upload_buffer_pos
C - - - - - 0x0001D0 00:C1C0: E4 53     CPX ram_0053_ppu_upload_buffer_size                        ; If PPU Buffer Position != PPU Buffer Size
C - - - - - 0x0001D2 00:C1C2: D0 C4     BNE bra_C188                                               ; Then upload more data
C - - - - - 0x0001D4 00:C1C4: 60        RTS
loc_C1C5_Balloon_Trip:
C D 2 - - - 0x0001D5 00:C1C5: A9 20     LDA #$20                                                   ; Play Balloon Trip music
C - - - - - 0x0001D7 00:C1C7: 85 F2     STA ram_00F2_music_jingle
C - - - - - 0x0001D9 00:C1C9: 20 27 C5  JSR sub_C527_Set_Bonus_Pts_10
C - - - - - 0x0001DC 00:C1CC: 20 39 C5  JSR sub_C539_Rank_Update
C - - - - - 0x0001DF 00:C1CF: A9 FF     LDA #$FF                                                   ; No platforms?
C - - - - - 0x0001E1 00:C1D1: 85 CD     STA ram_00CD_amount_of_platforms
C - - - - - 0x0001E3 00:C1D3: A9 AD     LDA #< tbl_C4AD_Trip_Premade_Layout
C - - - - - 0x0001E5 00:C1D5: 85 23     STA ram_0023_plat_coll_pointer_left                        ; Set pointer
C - - - - - 0x0001E7 00:C1D7: A9 C4     LDA #> tbl_C4AD_Trip_Premade_Layout                        ; [$23] = $C4AD
C - - - - - 0x0001E9 00:C1D9: 85 24     STA ram_0024_plat_coll_pointer_left
C - - - - - 0x0001EB 00:C1DB: A9 80     LDA #$80                                                   ; Set Player 1 X Position
C - - - - - 0x0001ED 00:C1DD: 85 91     STA ram_0091_object_x_pos_int_p1                           ; to #$80
C - - - - - 0x0001EF 00:C1DF: 8D 88 04  STA ram_0488_trip_plat_start_x_pos                         ; Set Balloon Trip Starting Platform X Position to #$80
C - - - - - 0x0001F2 00:C1E2: A9 70     LDA #$70                                                   ; Set Player 1 Y Position
C - - - - - 0x0001F4 00:C1E4: 85 9A     STA ram_009A_object_y_pos_int_p1                           ; to #$70
C - - - - - 0x0001F6 00:C1E6: 20 4A CD  JSR sub_CD4A_Init_Balloons
C - - - - - 0x0001F9 00:C1E9: A9 00     LDA #$00
C - - - - - 0x0001FB 00:C1EB: 85 41     STA ram_0041_p1_lives                                      ; 0 lives to Player 1
C - - - - - 0x0001FD 00:C1ED: 85 C9     STA ram_00C9_tile_scroll_counter                           ; Init Tile Scroll Counter
C - - - - - 0x0001FF 00:C1EF: 85 CA     STA ram_00CA_screen_scroll_counter                         ; Init Screen Scroll Counter
C - - - - - 0x000201 00:C1F1: 85 BA     STA ram_00BA_bolt_intensity_speed                          ; Init 10 Screens Counter?
C - - - - - 0x000203 00:C1F3: 85 C5     STA ram_00C5_lock_scroll_time                              ; Init Scrolling Lock Time
C - - - - - 0x000205 00:C1F5: 85 C8     STA ram_00C8_phase_type                                    ; Phase Type = 0
C - - - - - 0x000207 00:C1F7: 20 A5 F4  JSR sub_F4A5_Init_Fish
C - - - - - 0x00020A 00:C1FA: A2 13     LDX #$13
bra_C1FC:
C - - - - - 0x00020C 00:C1FC: A9 FF     LDA #$FF                                                   ; Reset all lightning bolts
C - - - - - 0x00020E 00:C1FE: 9D 30 05  STA ram_0530_bolt_animation_f_01,X                         ; Animation Frame = -1
C - - - - - 0x000211 00:C201: A9 F0     LDA #$F0
C - - - - - 0x000213 00:C203: 9D A4 04  STA ram_04A4_bolt_y_pos_int_01,X                           ; No vertical direction?
C - - - - - 0x000216 00:C206: CA        DEX
C - - - - - 0x000217 00:C207: 10 F3     BPL bra_C1FC
loc_C209:
C D 2 - - - 0x000219 00:C209: 20 70 F4  JSR sub_F470_Pause
C - - - - - 0x00021C 00:C20C: 20 91 E6  JSR sub_E691_Object_Manage
C - - - - - 0x00021F 00:C20F: A5 C5     LDA ram_00C5_lock_scroll_time                              ; If screen is locked
C - - - - - 0x000221 00:C211: D0 03     BNE bra_C216                                               ; then don't manage fish
C - - - - - 0x000223 00:C213: 20 F9 C6  JSR sub_C6F9_Fish_Manage
bra_C216:
C - - - - - 0x000226 00:C216: A5 19     LDA ram_0019_f_counter                                     ; Manage screen scrolling and stuff
C - - - - - 0x000228 00:C218: 4A        LSR                                                        ; every 2 frames...
C - - - - - 0x000229 00:C219: B0 03     BCS bra_C21E
C - - - - - 0x00022B 00:C21B: 4C D0 C2  JMP loc_C2D0
bra_C21E:
C - - - - - 0x00022E 00:C21E: A5 C5     LDA ram_00C5_lock_scroll_time                              ; ...unless the scrolling
C - - - - - 0x000230 00:C220: F0 05     BEQ bra_C227                                               ; is locked
C - - - - - 0x000232 00:C222: C6 C5     DEC ram_00C5_lock_scroll_time
C - - - - - 0x000234 00:C224: 4C D0 C2  JMP loc_C2D0
bra_C227:
C - - - - - 0x000237 00:C227: A5 17     LDA ram_0017_ppuscroll_shadow_trip                         ; If the scrolling X position
C - - - - - 0x000239 00:C229: D0 06     BNE bra_C231                                               ; is 0 then
C - - - - - 0x00023B 00:C22B: A5 18     LDA ram_0018_ppuctrl_shadow_trip                           ; Toggle between
C - - - - - 0x00023D 00:C22D: 49 01     EOR #$01                                                   ; Nametable 0 and 1
C - - - - - 0x00023F 00:C22F: 85 18     STA ram_0018_ppuctrl_shadow_trip
bra_C231:
C - - - - - 0x000241 00:C231: C6 17     DEC ram_0017_ppuscroll_shadow_trip                         ; Scroll 1 pixel from the left
C - - - - - 0x000243 00:C233: AD 88 04  LDA ram_0488_trip_plat_start_x_pos                         ; Skip if starting platform
C - - - - - 0x000246 00:C236: F0 15     BEQ bra_C24D                                               ; does not exist
C - - - - - 0x000248 00:C238: EE 88 04  INC ram_0488_trip_plat_start_x_pos                         ; Scroll starting platform 1 pixel to the right
C - - - - - 0x00024B 00:C23B: AD 88 04  LDA ram_0488_trip_plat_start_x_pos
C - - - - - 0x00024E 00:C23E: C9 F0     CMP #$F0                                                   ; If starting platform reaches
C - - - - - 0x000250 00:C240: 90 05     BCC bra_C247                                               ; X position #$F0
C - - - - - 0x000252 00:C242: A9 00     LDA #$00                                                   ; then it disappears
C - - - - - 0x000254 00:C244: 8D 88 04  STA ram_0488_trip_plat_start_x_pos
bra_C247:
C - - - - - 0x000257 00:C247: A5 BD     LDA ram_00BD_p1_invincibility_flag                         ; If Player is invincible
C - - - - - 0x000259 00:C249: F0 02     BEQ bra_C24D                                               ; (has not yet moved)
C - - - - - 0x00025B 00:C24B: E6 91     INC ram_0091_object_x_pos_int_p1                           ; then scroll 1 pixel to the right
bra_C24D:
C - - - - - 0x00025D 00:C24D: A2 07     LDX #$07
bra_C24F:
C - - - - - 0x00025F 00:C24F: BD 5D 05  LDA ram_055D_balloon_gfx,X                                 ; If balloon doesn't exist
C - - - - - 0x000262 00:C252: 30 19     BMI bra_C26D                                               ; skip to the next one
C - - - - - 0x000264 00:C254: FE 67 05  INC ram_0567_balloon_x_pos,X                               ; Scroll balloon 1 pixel to the right
C - - - - - 0x000267 00:C257: BD 67 05  LDA ram_0567_balloon_x_pos,X
C - - - - - 0x00026A 00:C25A: C9 F8     CMP #$F8                                                   ; If balloon's X position
C - - - - - 0x00026C 00:C25C: D0 0F     BNE bra_C26D                                               ; reaches #$F8
C - - - - - 0x00026E 00:C25E: A9 FF     LDA #$FF                                                   ; then make it disappear
C - - - - - 0x000270 00:C260: 9D 5D 05  STA ram_055D_balloon_gfx,X
C - - - - - 0x000273 00:C263: A9 F0     LDA #$F0
C - - - - - 0x000275 00:C265: 9D 7B 05  STA ram_057B_balloon_y_pos,X                               ; And reset the balloon counter
C - - - - - 0x000278 00:C268: A9 00     LDA #$00
C - - - - - 0x00027A 00:C26A: 8D CE 05  STA ram_05CE_trip_balloons_counter
bra_C26D:
C - - - - - 0x00027D 00:C26D: CA        DEX                                                        ; Check next balloon
C - - - - - 0x00027E 00:C26E: 10 DF     BPL bra_C24F
C - - - - - 0x000280 00:C270: A2 13     LDX #$13
bra_C272:
C - - - - - 0x000282 00:C272: BD 30 05  LDA ram_0530_bolt_animation_f_01,X                         ; If lightning bolt doesn't exist
C - - - - - 0x000285 00:C275: 30 12     BMI bra_C289                                               ; then skip to next one
C - - - - - 0x000287 00:C277: FE 90 04  INC ram_0490_bolt_x_pos_int_01,X                           ; Scroll bolt 1 pixel to the right
C - - - - - 0x00028A 00:C27A: BD 90 04  LDA ram_0490_bolt_x_pos_int_01,X
C - - - - - 0x00028D 00:C27D: C9 F8     CMP #$F8                                                   ; If bolt's X position
C - - - - - 0x00028F 00:C27F: 90 08     BCC bra_C289                                               ; reaches #$F8
C - - - - - 0x000291 00:C281: A9 F0     LDA #$F0                                                   ; then make it disappear
C - - - - - 0x000293 00:C283: 9D A4 04  STA ram_04A4_bolt_y_pos_int_01,X
C - - - - - 0x000296 00:C286: 9D 30 05  STA ram_0530_bolt_animation_f_01,X
bra_C289:
C - - - - - 0x000299 00:C289: CA        DEX                                                        ; Check next bolt
C - - - - - 0x00029A 00:C28A: 10 E6     BPL bra_C272
C - - - - - 0x00029C 00:C28C: A5 17     LDA ram_0017_ppuscroll_shadow_trip                         ; Every 8 pixels scrolled
C - - - - - 0x00029E 00:C28E: 29 07     AND #$07
C - - - - - 0x0002A0 00:C290: D0 3E     BNE bra_C2D0
C - - - - - 0x0002A2 00:C292: A6 88     LDX ram_0088_object_balloons_p1                            ; If Player still has balloons
C - - - - - 0x0002A4 00:C294: CA        DEX
C - - - - - 0x0002A5 00:C295: 30 39     BMI bra_C2D0
C - - - - - 0x0002A7 00:C297: A9 00     LDA #$00
C - - - - - 0x0002A9 00:C299: 85 3E     STA ram_003E_score_id_update                               ; Add 10 to Player 1's score
C - - - - - 0x0002AB 00:C29B: A9 01     LDA #$01
C - - - - - 0x0002AD 00:C29D: 20 DE D6  JSR sub_D6DE_Score_Add
C - - - - - 0x0002B0 00:C2A0: E6 C9     INC ram_00C9_tile_scroll_counter                           ; Increment Tile Scroll Counter
C - - - - - 0x0002B2 00:C2A2: A5 C9     LDA ram_00C9_tile_scroll_counter
C - - - - - 0x0002B4 00:C2A4: 29 1F     AND #$1F                                                   ; If 32 tiles have been scrolled
C - - - - - 0x0002B6 00:C2A6: D0 14     BNE bra_C2BC
C - - - - - 0x0002B8 00:C2A8: E6 CA     INC ram_00CA_screen_scroll_counter                         ; Increment Screen Scroll Counter
C - - - - - 0x0002BA 00:C2AA: A5 CA     LDA ram_00CA_screen_scroll_counter
C - - - - - 0x0002BC 00:C2AC: C9 0A     CMP #$0A                                                   ; If 10 screens have been scrolled
C - - - - - 0x0002BE 00:C2AE: D0 0C     BNE bra_C2BC
C - - - - - 0x0002C0 00:C2B0: A9 02     LDA #$02                                                   ; Then reset to screen #$02
C - - - - - 0x0002C2 00:C2B2: 85 CA     STA ram_00CA_screen_scroll_counter
C - - - - - 0x0002C4 00:C2B4: A4 BA     LDY ram_00BA_bolt_intensity_speed
C - - - - - 0x0002C6 00:C2B6: C8        INY                                                        ; Increment
C - - - - - 0x0002C7 00:C2B7: 98        TYA                                                        ; Lightning Bolt Intensity Level
C - - - - - 0x0002C8 00:C2B8: 29 03     AND #$03
C - - - - - 0x0002CA 00:C2BA: 85 BA     STA ram_00BA_bolt_intensity_speed
bra_C2BC:
C - - - - - 0x0002CC 00:C2BC: A6 CA     LDX ram_00CA_screen_scroll_counter
C - - - - - 0x0002CE 00:C2BE: BD BF C3  LDA tbl_C3BF_Screen_Layout_Order,X
C - - - - - 0x0002D1 00:C2C1: 0A        ASL
C - - - - - 0x0002D2 00:C2C2: A8        TAY                                                        ; Manage screen layout
C - - - - - 0x0002D3 00:C2C3: B9 B5 C3  LDA tbl_C3B5_Screen_Layout_Subs,Y                          ; Jump to subroutines
C - - - - - 0x0002D6 00:C2C6: 85 25     STA ram_0025_plat_coll_pointer_right                       ; dedicated to each screen layout
C - - - - - 0x0002D8 00:C2C8: B9 B6 C3  LDA tbl_C3B5_Screen_Layout_Subs + 1,Y
C - - - - - 0x0002DB 00:C2CB: 85 26     STA ram_0026_plat_coll_pointer_right
C - - - - - 0x0002DD 00:C2CD: 20 B2 C3  JSR sub_C3B2
bra_C2D0:
loc_C2D0:
C D 2 - - - 0x0002E0 00:C2D0: A2 07     LDX #$07
bra_C2D2:
C - - - - - 0x0002E2 00:C2D2: BD 5D 05  LDA ram_055D_balloon_gfx,X                                 ; If balloon doesn't exist
C - - - - - 0x0002E5 00:C2D5: 30 18     BMI bra_C2EF                                               ; then skip collision check
C - - - - - 0x0002E7 00:C2D7: 20 CE CE  JSR sub_CECE_Balloon_Collision
C - - - - - 0x0002EA 00:C2DA: AD CD 05  LDA ram_05CD_touched_balloons_counter
C - - - - - 0x0002ED 00:C2DD: F0 10     BEQ bra_C2EF                                               ; Every balloon touched
C - - - - - 0x0002EF 00:C2DF: CE CD 05  DEC ram_05CD_touched_balloons_counter                      ; counts towards the
C - - - - - 0x0002F2 00:C2E2: EE CE 05  INC ram_05CE_trip_balloons_counter                         ; main counter
C - - - - - 0x0002F5 00:C2E5: 8A        TXA
C - - - - - 0x0002F6 00:C2E6: 48        PHA                                                        ; Push X
C - - - - - 0x0002F7 00:C2E7: AD 59 05  LDA ram_0559_bonus_trip_ball_pts                           ; Add score
C - - - - - 0x0002FA 00:C2EA: 20 DE D6  JSR sub_D6DE_Score_Add
C - - - - - 0x0002FD 00:C2ED: 68        PLA                                                        ; Pull X
C - - - - - 0x0002FE 00:C2EE: AA        TAX
bra_C2EF:
C - - - - - 0x0002FF 00:C2EF: 20 2F CE  JSR sub_CE2F_Balloon_X_Sprite_Manage
C - - - - - 0x000302 00:C2F2: CA        DEX                                                        ; Check next balloon
C - - - - - 0x000303 00:C2F3: 10 DD     BPL bra_C2D2
C - - - - - 0x000305 00:C2F5: A2 13     LDX #$13
bra_C2F7:
C - - - - - 0x000307 00:C2F7: BD 30 05  LDA ram_0530_bolt_animation_f_01,X                         ; If lightning bolt exists?
C - - - - - 0x00030A 00:C2FA: 30 1B     BMI bra_C317
C - - - - - 0x00030C 00:C2FC: A5 C5     LDA ram_00C5_lock_scroll_time                              ; If scrolling is locked
C - - - - - 0x00030E 00:C2FE: D0 14     BNE bra_C314
C - - - - - 0x000310 00:C300: 20 B6 C9  JSR sub_C9B6_Bolt_Update                                   ; Update lightning bolt position
C - - - - - 0x000313 00:C303: BD A4 04  LDA ram_04A4_bolt_y_pos_int_01,X
C - - - - - 0x000316 00:C306: C9 02     CMP #$02                                                   ; If Y position < #$02
C - - - - - 0x000318 00:C308: B0 03     BCS bra_C30D                                               ; then
C - - - - - 0x00031A 00:C30A: 20 4F CA  JSR sub_CA4F                                               ; bounce lightning bolt vertically
bra_C30D:
C - - - - - 0x00031D 00:C30D: C9 D8     CMP #$D8                                                   ; If Y position >= #$D8
C - - - - - 0x00031F 00:C30F: 90 03     BCC bra_C314                                               ; then
C - - - - - 0x000321 00:C311: 20 4F CA  JSR sub_CA4F                                               ; bounce lightning bolt vertically
bra_C314:
C - - - - - 0x000324 00:C314: 20 1C CB  JSR sub_CB1C_Bolt_Player_Collision
bra_C317:
C - - - - - 0x000327 00:C317: A5 19     LDA ram_0019_f_counter
C - - - - - 0x000329 00:C319: 29 07     AND #$07                                                   ; Get lightning bolt
C - - - - - 0x00032B 00:C31B: 4A        LSR                                                        ; Animation frame tile
C - - - - - 0x00032C 00:C31C: A8        TAY
C - - - - - 0x00032D 00:C31D: B9 DD C9  LDA tbl_C9DD,Y                                             ; Unused note: Animation is 8 frames
C - - - - - 0x000330 00:C320: 48        PHA                                                        ; but only half of them are used
C - - - - - 0x000331 00:C321: A5 19     LDA ram_0019_f_counter
C - - - - - 0x000333 00:C323: 4A        LSR                                                        ; Every 2 frames...
C - - - - - 0x000334 00:C324: 8A        TXA
C - - - - - 0x000335 00:C325: 90 06     BCC bra_C32D
C - - - - - 0x000337 00:C327: 85 12     STA ram_0012_temp
C - - - - - 0x000339 00:C329: A9 13     LDA #$13                                                   ; ...count from the end
C - - - - - 0x00033B 00:C32B: E5 12     SBC ram_0012_temp
bra_C32D:
C - - - - - 0x00033D 00:C32D: 0A        ASL
C - - - - - 0x00033E 00:C32E: 0A        ASL                                                        ; Get OAM sprite address
C - - - - - 0x00033F 00:C32F: A8        TAY
C - - - - - 0x000340 00:C330: 68        PLA                                                        ; Update lightning bolt sprite
C - - - - - 0x000341 00:C331: 99 B1 02  STA ram_02B1_sprite_2C_tile,Y                              ; Tile ID
C - - - - - 0x000344 00:C334: BD A4 04  LDA ram_04A4_bolt_y_pos_int_01,X
C - - - - - 0x000347 00:C337: 99 B0 02  STA ram_02B0_sprite_2C_y,Y                                 ; Y position
C - - - - - 0x00034A 00:C33A: BD 90 04  LDA ram_0490_bolt_x_pos_int_01,X
C - - - - - 0x00034D 00:C33D: 99 B3 02  STA ram_02B3_sprite_2C_x,Y                                 ; X position
C - - - - - 0x000350 00:C340: A9 00     LDA #$00
C - - - - - 0x000352 00:C342: 99 B2 02  STA ram_02B2_sprite_2C_attributes,Y                        ; Use palette 4
C - - - - - 0x000355 00:C345: CA        DEX                                                        ; Loop to next bolt
C - - - - - 0x000356 00:C346: 10 AF     BPL bra_C2F7
C - - - - - 0x000358 00:C348: AD CE 05  LDA ram_05CE_trip_balloons_counter                         ; If you touched
C - - - - - 0x00035B 00:C34B: C9 14     CMP #$14                                                   ; 20 balloons in a row...
C - - - - - 0x00035D 00:C34D: 90 20     BCC bra_C36F
C - - - - - 0x00035F 00:C34F: E6 47     INC ram_0047_4th_digit_score_add                           ; Add 10000
C - - - - - 0x000361 00:C351: A9 00     LDA #$00                                                   ; to score
C - - - - - 0x000363 00:C353: 20 DE D6  JSR sub_D6DE_Score_Add
C - - - - - 0x000366 00:C356: C6 47     DEC ram_0047_4th_digit_score_add                           ; Reset score to add
C - - - - - 0x000368 00:C358: A9 10     LDA #$10                                                   ; Play Bonus Phase Perfect Jingle
C - - - - - 0x00036A 00:C35A: 85 F2     STA ram_00F2_music_jingle
C - - - - - 0x00036C 00:C35C: E6 C8     INC ram_00C8_phase_type                                    ; Set Bonus Phase Type
C - - - - - 0x00036E 00:C35E: 20 ED D3  JSR sub_D3ED_Set_Palette                                   ; Update balloon palette
C - - - - - 0x000371 00:C361: 20 27 C5  JSR sub_C527_Set_Bonus_Pts_10
C - - - - - 0x000374 00:C364: C6 C8     DEC ram_00C8_phase_type                                    ; Reset to normal Phase
C - - - - - 0x000376 00:C366: A2 64     LDX #$64                                                   ; Wait 100 frames
C - - - - - 0x000378 00:C368: 20 5E F4  JSR sub_F45E_Wait_Y_Ticks
C - - - - - 0x00037B 00:C36B: A9 20     LDA #$20                                                   ; Play Balloon Trip music
C - - - - - 0x00037D 00:C36D: 85 F2     STA ram_00F2_music_jingle
bra_C36F:
C - - - - - 0x00037F 00:C36F: A2 F0     LDX #$F0                                                   ; If Balloon Trip Starting Platform
C - - - - - 0x000381 00:C371: AD 88 04  LDA ram_0488_trip_plat_start_x_pos                         ; X position is 0
C - - - - - 0x000384 00:C374: F0 02     BEQ bra_C378                                               ; then don't make it appear on screen
C - - - - - 0x000386 00:C376: A2 88     LDX #$88                                                   ; At Y position #$88:
bra_C378:
C - - - - - 0x000388 00:C378: 8E 00 02  STX ram_0200_sprite_00_y                                   ; Display left and right
C - - - - - 0x00038B 00:C37B: 8E 04 02  STX ram_0204_sprite_01_y                                   ; sides of platform
C - - - - - 0x00038E 00:C37E: 8D 03 02  STA ram_0203_sprite_00_x
C - - - - - 0x000391 00:C381: 18        CLC                                                        ; Display left and right
C - - - - - 0x000392 00:C382: 69 08     ADC #$08                                                   ; sides at current X position
C - - - - - 0x000394 00:C384: 8D 07 02  STA ram_0207_sprite_01_x
C - - - - - 0x000397 00:C387: A5 19     LDA ram_0019_f_counter
C - - - - - 0x000399 00:C389: 29 03     AND #$03                                                   ; Switch between palettes
C - - - - - 0x00039B 00:C38B: 8D 02 02  STA ram_0202_sprite_00_attributes                          ; on platform
C - - - - - 0x00039E 00:C38E: 8D 06 02  STA ram_0206_sprite_01_attributes
C - - - - - 0x0003A1 00:C391: A2 E3     LDX #$E3
C - - - - - 0x0003A3 00:C393: 8E 01 02  STX ram_0201_sprite_00_tile                                ; Display Tile #$E3 and #$E4
C - - - - - 0x0003A6 00:C396: E8        INX
C - - - - - 0x0003A7 00:C397: 8E 05 02  STX ram_0205_sprite_01_tile
C - - - - - 0x0003AA 00:C39A: A5 88     LDA ram_0088_object_balloons_p1                            ; If Player is dead (no balloons)
C - - - - - 0x0003AC 00:C39C: 30 03     BMI bra_C3A1                                               ; then game over
C - - - - - 0x0003AE 00:C39E: 4C 09 C2  JMP loc_C209                                               ; else game loop
bra_C3A1:
C - - - - - 0x0003B1 00:C3A1: 20 79 C5  JSR sub_C579_Rank_Score_Update
C - - - - - 0x0003B4 00:C3A4: A9 01     LDA #$01                                                   ; \ Stop All Sounds
C - - - - - 0x0003B6 00:C3A6: 85 F0     STA ram_00F0_sfx_1                                         ; /
C - - - - - 0x0003B8 00:C3A8: 20 65 F4  JSR sub_F465_Clear_F_Flag                                  ; Clear Frame Flag
C - - - - - 0x0003BB 00:C3AB: A9 02     LDA #$02                                                   ; Play Stage Clear Jingle
C - - - - - 0x0003BD 00:C3AD: 85 F2     STA ram_00F2_music_jingle
C - - - - - 0x0003BF 00:C3AF: 4C 6A F3  JMP loc_F36A                                               ; Put Game Over on screen
sub_C3B2:
C - - - - - 0x0003C2 00:C3B2: 6C 25 00  JMP (ram_0025_plat_coll_pointer_right)
tbl_C3B5_Screen_Layout_Subs:
- D 2 - - - 0x0003C5 00:C3B5: C9 C3     .word ofs_C3C9
- D 2 - - - 0x0003C7 00:C3B7: F7 C3     .word ofs_C3F7
- D 2 - - - 0x0003C9 00:C3B9: 3E C4     .word ofs_C43E
- D 2 - - - 0x0003CB 00:C3BB: 5F C4     .word ofs_C45F
- D 2 - - - 0x0003CD 00:C3BD: 5E C4     .word ofs_C45E_RTS
tbl_C3BF_Screen_Layout_Order:
- D 2 - - - 0x0003CF 00:C3BF: 00        .byte $00, $00, $02, $02, $02, $02, $02, $04, $03, $01
loc_C3C9:
ofs_C3C9:
C D 2 J - - 0x0003D9 00:C3C9: A0 00     LDY #$00
C - - - - - 0x0003DB 00:C3CB: B1 23     LDA (ram_0023_plat_coll_pointer_left),Y                    ; Read layout data byte
C - - - - - 0x0003DD 00:C3CD: E6 23     INC ram_0023_plat_coll_pointer_left                        ; [$23] and increment
C - - - - - 0x0003DF 00:C3CF: D0 02     BNE bra_C3D3                                               ; Bit format: BL0YYYYY
C - - - - - 0x0003E1 00:C3D1: E6 24     INC ram_0024_plat_coll_pointer_left                        ; B = Balloon, L = Lightning bolt, Y = Tile Y position
bra_C3D3:
C - - - - - 0x0003E3 00:C3D3: AA        TAX
C - - - - - 0x0003E4 00:C3D4: F0 20     BEQ bra_C3F6_RTS                                           ; If layout byte = 0 then return
C - - - - - 0x0003E6 00:C3D6: 0A        ASL
C - - - - - 0x0003E7 00:C3D7: 0A        ASL                                                        ; Set Y position
C - - - - - 0x0003E8 00:C3D8: 0A        ASL
C - - - - - 0x0003E9 00:C3D9: 85 15     STA ram_0015_temp_player_x_trip_rank_score
C - - - - - 0x0003EB 00:C3DB: A9 00     LDA #$00
C - - - - - 0x0003ED 00:C3DD: 85 14     STA ram_0014_temp
C - - - - - 0x0003EF 00:C3DF: 8A        TXA
C - - - - - 0x0003F0 00:C3E0: 29 C0     AND #$C0
C - - - - - 0x0003F2 00:C3E2: C9 80     CMP #$80                                                   ; If bit B is set
C - - - - - 0x0003F4 00:C3E4: D0 06     BNE bra_C3EC                                               ; then
C - - - - - 0x0003F6 00:C3E6: 20 6B C4  JSR sub_C46B                                               ; spawn balloon
C - - - - - 0x0003F9 00:C3E9: 4C C9 C3  JMP loc_C3C9                                               ; Repeat
bra_C3EC:
C - - - - - 0x0003FC 00:C3EC: C9 00     CMP #$00                                                   ; If bit L is set
C - - - - - 0x0003FE 00:C3EE: D0 06     BNE bra_C3F6_RTS                                           ; then
C - - - - - 0x000400 00:C3F0: 20 86 C4  JSR sub_C486                                               ; spawn lightning bolt
C - - - - - 0x000403 00:C3F3: 4C C9 C3  JMP loc_C3C9                                               ; Repeat
bra_C3F6_RTS:
C - - - - - 0x000406 00:C3F6: 60        RTS
bra_C3F7:
ofs_C3F7:
C - - - - - 0x000407 00:C3F7: 20 B3 F1  JSR sub_F1B3_RNG
C - - - - - 0x00040A 00:C3FA: 29 7F     AND #$7F
C - - - - - 0x00040C 00:C3FC: C9 04     CMP #$04
C - - - - - 0x00040E 00:C3FE: 90 0C     BCC bra_C40C                                               ; If RNG value is between
C - - - - - 0x000410 00:C400: C9 18     CMP #$18                                                   ; 4 and 23
C - - - - - 0x000412 00:C402: B0 08     BCS bra_C40C
C - - - - - 0x000414 00:C404: 0A        ASL                                                        ; then spawn balloon
C - - - - - 0x000415 00:C405: 0A        ASL                                                        ; at Tile Y position value
C - - - - - 0x000416 00:C406: 0A        ASL
C - - - - - 0x000417 00:C407: 85 15     STA ram_0015_temp_player_x_trip_rank_score
C - - - - - 0x000419 00:C409: 20 6B C4  JSR sub_C46B
bra_C40C:
loc_C40C:
sub_C40C:
C D 2 - - - 0x00041C 00:C40C: 20 B3 F1  JSR sub_F1B3_RNG
C - - - - - 0x00041F 00:C40F: 29 3F     AND #$3F
C - - - - - 0x000421 00:C411: C9 02     CMP #$02
C - - - - - 0x000423 00:C413: 90 24     BCC bra_C439_RTS                                           ; If RNG value is between
C - - - - - 0x000425 00:C415: C9 18     CMP #$18                                                   ; 2 and 23
C - - - - - 0x000427 00:C417: B0 20     BCS bra_C439_RTS
C - - - - - 0x000429 00:C419: 0A        ASL                                                        ; then spawn lightning bolt
C - - - - - 0x00042A 00:C41A: 0A        ASL                                                        ; at Tile Y position value
C - - - - - 0x00042B 00:C41B: 0A        ASL                                                        ; and set Y velocity value
C - - - - - 0x00042C 00:C41C: 85 15     STA ram_0015_temp_player_x_trip_rank_score                 ; using tbl_C43A value
C - - - - - 0x00042E 00:C41E: 20 B3 F1  JSR sub_F1B3_RNG                                           ; (depending on full loop)
C - - - - - 0x000431 00:C421: 29 3F     AND #$3F                                                   ; + RNG value up to 63
C - - - - - 0x000433 00:C423: A6 BA     LDX ram_00BA_bolt_intensity_speed
C - - - - - 0x000435 00:C425: 7D 3A C4  ADC tbl_C43A,X
C - - - - - 0x000438 00:C428: 85 14     STA ram_0014_temp
C - - - - - 0x00043A 00:C42A: 20 86 C4  JSR sub_C486
C - - - - - 0x00043D 00:C42D: 20 B3 F1  JSR sub_F1B3_RNG
C - - - - - 0x000440 00:C430: 4A        LSR                                                        ; Make Y velocity value negative
C - - - - - 0x000441 00:C431: 90 D9     BCC bra_C40C                                               ; 50% of the time
C - - - - - 0x000443 00:C433: 20 4F CA  JSR sub_CA4F
C - - - - - 0x000446 00:C436: 4C 0C C4  JMP loc_C40C
bra_C439_RTS:
C - - - - - 0x000449 00:C439: 60        RTS
tbl_C43A:
- D 2 - - - 0x00044A 00:C43A: 20        .byte $20, $30, $40, $60   ; 
ofs_C43E:
C - - J - - 0x00044E 00:C43E: 20 B3 F1  JSR sub_F1B3_RNG                                           ; Only bits 00XX0000 have to be set
C - - - - - 0x000451 00:C441: 29 CF     AND #$CF                                                   ; else spawn something at random
C - - - - - 0x000453 00:C443: D0 B2     BNE bra_C3F7
C - - - - - 0x000455 00:C445: A4 89     LDY ram_0089_object_balloons_p2                            ; If Player 2 exists (???)
C - - - - - 0x000457 00:C447: C8        INY
C - - - - - 0x000458 00:C448: D0 AD     BNE bra_C3F7
C - - - - - 0x00045A 00:C44A: A9 E6     LDA #$E6                                                   ; Player 2 Y Position = #$E6
C - - - - - 0x00045C 00:C44C: 85 9B     STA ram_009B_object_y_pos_int_p2
C - - - - - 0x00045E 00:C44E: A5 1B     LDA ram_001B_rng_output_seed
C - - - - - 0x000460 00:C450: 29 7F     AND #$7F                                                   ; Player 2 X Position = #$40 + RNG (up to 127)
C - - - - - 0x000462 00:C452: 69 40     ADC #$40
C - - - - - 0x000464 00:C454: 85 92     STA ram_0092_object_x_pos_int_p2
C - - - - - 0x000466 00:C456: A9 80     LDA #$80                                                   ; Player 2 Balloons = -128
C - - - - - 0x000468 00:C458: 85 89     STA ram_0089_object_balloons_p2
C - - - - - 0x00046A 00:C45A: A9 00     LDA #$00                                                   ; Player 2 Status = 00
C - - - - - 0x00046C 00:C45C: 85 80     STA ram_0080_object_status_p2
ofs_C45E_RTS:
C - - - - - 0x00046E 00:C45E: 60        RTS
ofs_C45F:
C - - J - - 0x00046F 00:C45F: 20 0C C4  JSR sub_C40C                                               ; Randomly spawn lightning bolt
C - - - - - 0x000472 00:C462: 20 B3 F1  JSR sub_F1B3_RNG
C - - - - - 0x000475 00:C465: 29 7F     AND #$7F                                                   ; Set X velocity (Frac)
C - - - - - 0x000477 00:C467: 9D 08 05  STA ram_0508_bolt_x_vel_frac_01,X                          ; RNG up to 127
C - - - - - 0x00047A 00:C46A: 60        RTS
sub_C46B:
C - - - - - 0x00047B 00:C46B: A2 07     LDX #$07
bra_C46D:
C - - - - - 0x00047D 00:C46D: BD 5D 05  LDA ram_055D_balloon_gfx,X                                 ; Find balloon that
C - - - - - 0x000480 00:C470: 30 04     BMI bra_C476                                               ; hasn't spawned yet
C - - - - - 0x000482 00:C472: CA        DEX
C - - - - - 0x000483 00:C473: 10 F8     BPL bra_C46D
C - - - - - 0x000485 00:C475: 60        RTS
bra_C476:
C - - - - - 0x000486 00:C476: A9 01     LDA #$01                                                   ; Set balloon type/GFX? to 01
C - - - - - 0x000488 00:C478: 9D 5D 05  STA ram_055D_balloon_gfx,X
C - - - - - 0x00048B 00:C47B: A9 00     LDA #$00                                                   ; Set balloon X position to 00
C - - - - - 0x00048D 00:C47D: 9D 67 05  STA ram_0567_balloon_x_pos,X
C - - - - - 0x000490 00:C480: A5 15     LDA ram_0015_temp_player_x_trip_rank_score                 ; Set balloon Y position to [$15]
C - - - - - 0x000492 00:C482: 9D 7B 05  STA ram_057B_balloon_y_pos,X
C - - - - - 0x000495 00:C485: 60        RTS
sub_C486:
C - - - - - 0x000496 00:C486: A2 13     LDX #$13
bra_C488:
C - - - - - 0x000498 00:C488: BD 30 05  LDA ram_0530_bolt_animation_f_01,X                         ; Find lightning bolt
C - - - - - 0x00049B 00:C48B: 30 04     BMI bra_C491                                               ; that hasn't spawned yet
C - - - - - 0x00049D 00:C48D: CA        DEX
C - - - - - 0x00049E 00:C48E: 10 F8     BPL bra_C488
C - - - - - 0x0004A0 00:C490: 60        RTS
bra_C491:
C - - - - - 0x0004A1 00:C491: A9 00     LDA #$00
C - - - - - 0x0004A3 00:C493: 9D 30 05  STA ram_0530_bolt_animation_f_01,X                         ; Set animation frame to 00
C - - - - - 0x0004A6 00:C496: 9D 90 04  STA ram_0490_bolt_x_pos_int_01,X                           ; Set X position to 00
C - - - - - 0x0004A9 00:C499: 9D F4 04  STA ram_04F4_bolt_y_vel_int_01,X                           ; Set Y velocity to 00
C - - - - - 0x0004AC 00:C49C: 9D 08 05  STA ram_0508_bolt_x_vel_frac_01,X                          ; Set X velocity to 00 (Int and Frac)
C - - - - - 0x0004AF 00:C49F: 9D E0 04  STA ram_04E0_bolt_x_vel_int_01,X
C - - - - - 0x0004B2 00:C4A2: A5 14     LDA ram_0014_temp                                          ; Set Y velocity (Frac) to [$14]
C - - - - - 0x0004B4 00:C4A4: 9D 1C 05  STA ram_051C_bolt_y_vel_frac_01,X
C - - - - - 0x0004B7 00:C4A7: A5 15     LDA ram_0015_temp_player_x_trip_rank_score                 ; Set Y position to [$15]
C - - - - - 0x0004B9 00:C4A9: 9D A4 04  STA ram_04A4_bolt_y_pos_int_01,X
C - - - - - 0x0004BC 00:C4AC: 60        RTS
tbl_C4AD_Trip_Premade_Layout:
- D 2 - I - 0x0004BD 00:C4AD: 00        .byte $00   ; 
- D 2 - I - 0x0004BE 00:C4AE: 00        .byte $00   ; 
- D 2 - I - 0x0004BF 00:C4AF: 09        .byte $09   ; 
- D 2 - I - 0x0004C0 00:C4B0: 00        .byte $00   ; 
- D 2 - I - 0x0004C1 00:C4B1: 08        .byte $08   ; 
- D 2 - I - 0x0004C2 00:C4B2: 8C        .byte $8C   ; 
- D 2 - I - 0x0004C3 00:C4B3: 00        .byte $00   ; 
- D 2 - I - 0x0004C4 00:C4B4: 07        .byte $07   ; 
- D 2 - I - 0x0004C5 00:C4B5: 18        .byte $18   ; 
- D 2 - I - 0x0004C6 00:C4B6: 00        .byte $00   ; 
- D 2 - I - 0x0004C7 00:C4B7: 18        .byte $18   ; 
- D 2 - I - 0x0004C8 00:C4B8: 00        .byte $00   ; 
- D 2 - I - 0x0004C9 00:C4B9: 19        .byte $19   ; 
- D 2 - I - 0x0004CA 00:C4BA: 00        .byte $00   ; 
- D 2 - I - 0x0004CB 00:C4BB: 1A        .byte $1A   ; 
- D 2 - I - 0x0004CC 00:C4BC: 00        .byte $00   ; 
- D 2 - I - 0x0004CD 00:C4BD: 84        .byte $84   ; 
- D 2 - I - 0x0004CE 00:C4BE: 94        .byte $94   ; 
- D 2 - I - 0x0004CF 00:C4BF: 1A        .byte $1A   ; 
- D 2 - I - 0x0004D0 00:C4C0: 00        .byte $00   ; 
- D 2 - I - 0x0004D1 00:C4C1: 1A        .byte $1A   ; 
- D 2 - I - 0x0004D2 00:C4C2: 00        .byte $00   ; 
- D 2 - I - 0x0004D3 00:C4C3: 1A        .byte $1A   ; 
- D 2 - I - 0x0004D4 00:C4C4: 00        .byte $00   ; 
- D 2 - I - 0x0004D5 00:C4C5: 0B        .byte $0B   ; 
- D 2 - I - 0x0004D6 00:C4C6: 12        .byte $12   ; 
- D 2 - I - 0x0004D7 00:C4C7: 00        .byte $00   ; 
- D 2 - I - 0x0004D8 00:C4C8: 0C        .byte $0C   ; 
- D 2 - I - 0x0004D9 00:C4C9: 13        .byte $13   ; 
- D 2 - I - 0x0004DA 00:C4CA: 00        .byte $00   ; 
- D 2 - I - 0x0004DB 00:C4CB: 0D        .byte $0D   ; 
- D 2 - I - 0x0004DC 00:C4CC: 14        .byte $14   ; 
- D 2 - I - 0x0004DD 00:C4CD: 00        .byte $00   ; 
- D 2 - I - 0x0004DE 00:C4CE: 14        .byte $14   ; 
- D 2 - I - 0x0004DF 00:C4CF: 00        .byte $00   ; 
- D 2 - I - 0x0004E0 00:C4D0: 00        .byte $00   ; 
- D 2 - I - 0x0004E1 00:C4D1: 90        .byte $90   ; 
- D 2 - I - 0x0004E2 00:C4D2: 00        .byte $00   ; 
- D 2 - I - 0x0004E3 00:C4D3: 07        .byte $07   ; 
- D 2 - I - 0x0004E4 00:C4D4: 00        .byte $00   ; 
- D 2 - I - 0x0004E5 00:C4D5: 07        .byte $07   ; 
- D 2 - I - 0x0004E6 00:C4D6: 8C        .byte $8C   ; 
- D 2 - I - 0x0004E7 00:C4D7: 96        .byte $96   ; 
- D 2 - I - 0x0004E8 00:C4D8: 00        .byte $00   ; 
- D 2 - I - 0x0004E9 00:C4D9: 08        .byte $08   ; 
- D 2 - I - 0x0004EA 00:C4DA: 00        .byte $00   ; 
- D 2 - I - 0x0004EB 00:C4DB: 09        .byte $09   ; 
- D 2 - I - 0x0004EC 00:C4DC: 00        .byte $00   ; 
- D 2 - I - 0x0004ED 00:C4DD: 00        .byte $00   ; 
- D 2 - I - 0x0004EE 00:C4DE: 18        .byte $18   ; 
- D 2 - I - 0x0004EF 00:C4DF: 00        .byte $00   ; 
- D 2 - I - 0x0004F0 00:C4E0: 17        .byte $17   ; 
- D 2 - I - 0x0004F1 00:C4E1: 00        .byte $00   ; 
- D 2 - I - 0x0004F2 00:C4E2: 16        .byte $16   ; 
- D 2 - I - 0x0004F3 00:C4E3: 00        .byte $00   ; 
- D 2 - I - 0x0004F4 00:C4E4: 00        .byte $00   ; 
- D 2 - I - 0x0004F5 00:C4E5: 00        .byte $00   ; 
- D 2 - I - 0x0004F6 00:C4E6: 00        .byte $00   ; 
- D 2 - I - 0x0004F7 00:C4E7: 00        .byte $00   ; 
- D 2 - I - 0x0004F8 00:C4E8: 00        .byte $00   ; 
- D 2 - I - 0x0004F9 00:C4E9: 8A        .byte $8A   ; 
- D 2 - I - 0x0004FA 00:C4EA: 90        .byte $90   ; 
- D 2 - I - 0x0004FB 00:C4EB: 00        .byte $00   ; 
- D 2 - I - 0x0004FC 00:C4EC: 00        .byte $00   ; 
- D 2 - I - 0x0004FD 00:C4ED: 00        .byte $00   ; 
- D 2 - I - 0x0004FE 00:C4EE: 08        .byte $08   ; 
- D 2 - I - 0x0004FF 00:C4EF: 00        .byte $00   ; 
- D 2 - I - 0x000500 00:C4F0: 09        .byte $09   ; 
- D 2 - I - 0x000501 00:C4F1: 98        .byte $98   ; 
- D 2 - I - 0x000502 00:C4F2: 00        .byte $00   ; 
- D 2 - I - 0x000503 00:C4F3: 0A        .byte $0A   ; 
- D 2 - I - 0x000504 00:C4F4: 00        .byte $00   ; 
- D 2 - I - 0x000505 00:C4F5: 00        .byte $00   ; 
- D 2 - I - 0x000506 00:C4F6: 00        .byte $00   ; 
- D 2 - I - 0x000507 00:C4F7: 86        .byte $86   ; 
- D 2 - I - 0x000508 00:C4F8: 8A        .byte $8A   ; 
- D 2 - I - 0x000509 00:C4F9: 15        .byte $15   ; 
- D 2 - I - 0x00050A 00:C4FA: 00        .byte $00   ; 
- D 2 - I - 0x00050B 00:C4FB: 14        .byte $14   ; 
- D 2 - I - 0x00050C 00:C4FC: 00        .byte $00   ; 
- D 2 - I - 0x00050D 00:C4FD: 8E        .byte $8E   ; 
- D 2 - I - 0x00050E 00:C4FE: 13        .byte $13   ; 
- D 2 - I - 0x00050F 00:C4FF: 00        .byte $00   ; 
- D 2 - I - 0x000510 00:C500: 00        .byte $00   ; 
- D 2 - I - 0x000511 00:C501: 03        .byte $03   ; 
- D 2 - I - 0x000512 00:C502: 0D        .byte $0D   ; 
- D 2 - I - 0x000513 00:C503: 00        .byte $00   ; 
- D 2 - I - 0x000514 00:C504: 0D        .byte $0D   ; 
- D 2 - I - 0x000515 00:C505: 0E        .byte $0E   ; 
- D 2 - I - 0x000516 00:C506: 00        .byte $00   ; 
- D 2 - I - 0x000517 00:C507: 0C        .byte $0C   ; 
- D 2 - I - 0x000518 00:C508: 0D        .byte $0D   ; 
- D 2 - I - 0x000519 00:C509: 00        .byte $00   ; 
- D 2 - I - 0x00051A 00:C50A: 0D        .byte $0D   ; 
- D 2 - I - 0x00051B 00:C50B: 19        .byte $19   ; 
- D 2 - I - 0x00051C 00:C50C: 00        .byte $00   ; 
- D 2 - I - 0x00051D 00:C50D: 86        .byte $86   ; 
- D 2 - I - 0x00051E 00:C50E: 92        .byte $92   ; 
- D 2 - I - 0x00051F 00:C50F: 00        .byte $00   ; 
- D 2 - I - 0x000520 00:C510: 00        .byte $00   ; 
- D 2 - I - 0x000521 00:C511: 98        .byte $98   ; 
- D 2 - I - 0x000522 00:C512: 00        .byte $00   ; 
- D 2 - I - 0x000523 00:C513: 00        .byte $00   ; 
- D 2 - I - 0x000524 00:C514: 0A        .byte $0A   ; 
- D 2 - I - 0x000525 00:C515: 12        .byte $12   ; 
- D 2 - I - 0x000526 00:C516: 00        .byte $00   ; 
- D 2 - I - 0x000527 00:C517: 09        .byte $09   ; 
- D 2 - I - 0x000528 00:C518: 13        .byte $13   ; 
- D 2 - I - 0x000529 00:C519: 00        .byte $00   ; 
- D 2 - I - 0x00052A 00:C51A: 08        .byte $08   ; 
- D 2 - I - 0x00052B 00:C51B: 14        .byte $14   ; 
- D 2 - I - 0x00052C 00:C51C: 00        .byte $00   ; 
- D 2 - I - 0x00052D 00:C51D: 07        .byte $07   ; 
- D 2 - I - 0x00052E 00:C51E: 15        .byte $15   ; 
- D 2 - I - 0x00052F 00:C51F: 00        .byte $00   ; 
- D 2 - I - 0x000530 00:C520: 07        .byte $07   ; 
- D 2 - I - 0x000531 00:C521: 16        .byte $16   ; 
- D 2 - I - 0x000532 00:C522: 00        .byte $00   ; 
- D 2 - I - 0x000533 00:C523: 07        .byte $07   ; 
- D 2 - I - 0x000534 00:C524: 00        .byte $00   ; 
- D 2 - I - 0x000535 00:C525: 00        .byte $00   ; 
- D 2 - I - 0x000536 00:C526: 00        .byte $00   ; 
sub_C527_Set_Bonus_Pts_10:
C D 2 - I - 0x000537 00:C527: 20 E2 D0  JSR sub_D0E2_Set_Bonus_Phase                               ; Set up balloon points
C D 2 - I - 0x00053A 00:C52A: 0E 59 05  ASL ram_0559_bonus_trip_ball_pts
C D 2 - I - 0x00053D 00:C52D: AD 59 05  LDA ram_0559_bonus_trip_ball_pts                           ; ([$0559] * 2) * 5
C D 2 - I - 0x000540 00:C530: 0A        ASL                                                        ; Multiply balloon points
C D 2 - I - 0x000541 00:C531: 0A        ASL                                                        ; by 10
C D 2 - I - 0x000542 00:C532: 6D 59 05  ADC ram_0559_bonus_trip_ball_pts
C - - - - - 0x000545 00:C535: 8D 59 05  STA ram_0559_bonus_trip_ball_pts
C - - - - - 0x000548 00:C538: 60        RTS
sub_C539_Rank_Update:
C - - - - - 0x000549 00:C539: A9 00     LDA #$00                                                   ; Set Balloon Trip Rank 01 (00 + 1)
C - - - - - 0x00054B 00:C53B: 85 12     STA ram_0012_temp
bra_C53D:
C - - - - - 0x00054D 00:C53D: A5 12     LDA ram_0012_temp
C - - - - - 0x00054F 00:C53F: 0A        ASL
C - - - - - 0x000550 00:C540: 0A        ASL                                                        ; Set up pointer to
C - - - - - 0x000551 00:C541: 65 12     ADC ram_0012_temp                                          ; 0700 + (Rank) * 5
C - - - - - 0x000553 00:C543: 85 1D     STA ram_001D_loading_pointer_1d
C - - - - - 0x000555 00:C545: A9 07     LDA #$07
C - - - - - 0x000557 00:C547: 85 1E     STA ram_001E_loading_pointer_1d
C - - - - - 0x000559 00:C549: A0 04     LDY #$04
bra_C54B:
C - - - - - 0x00055B 00:C54B: B1 1D     LDA (ram_001D_loading_pointer_1d),Y                        ; Check each digit
C - - - - - 0x00055D 00:C54D: D9 03 00  CMP ram_0003_p1_score_0000x,Y                              ; If Pl Score Digit < Rank Score
C - - - - - 0x000560 00:C550: 90 11     BCC bra_C563                                               ; then stop
C - - - - - 0x000562 00:C552: D0 05     BNE bra_C559                                               ; If >= then check next Rank Score
C - - - - - 0x000564 00:C554: 88        DEY
C - - - - - 0x000565 00:C555: 10 F4     BPL bra_C54B                                               ; Else check next digit
C - - - - - 0x000567 00:C557: 30 0A     BMI bra_C563                                               ; When done, update current Rank
bra_C559:
C - - - - - 0x000569 00:C559: E6 12     INC ram_0012_temp
C - - - - - 0x00056B 00:C55B: A5 12     LDA ram_0012_temp                                          ; If (Rank+1) != 50 (!)
C - - - - - 0x00056D 00:C55D: C9 32     CMP #$32                                                   ; then check the next rank
C - - - - - 0x00056F 00:C55F: D0 DC     BNE bra_C53D                                               ; else update current rank
- - - - - - 0x000571 00:C561: C6 12     DEC ram_0012_temp
bra_C563:
C - - - - - 0x000573 00:C563: E6 12     INC ram_0012_temp
C - - - - - 0x000575 00:C565: A5 12     LDA ram_0012_temp
C - - - - - 0x000577 00:C567: 48        PHA                                                        ; Update Current Rank variable
C - - - - - 0x000578 00:C568: 85 43     STA ram_0043_div_mod_result
C - - - - - 0x00057A 00:C56A: A0 0A     LDY #$0A
C - - - - - 0x00057C 00:C56C: 20 7C D7  JSR sub_D77C_Divide                                        ; (Rank+1) / 10
C - - - - - 0x00057F 00:C56F: 85 4A     STA ram_004A_balloon_trip_rank_x0                          ; Write second digit
C - - - - - 0x000581 00:C571: A5 43     LDA ram_0043_div_mod_result
C - - - - - 0x000583 00:C573: 85 49     STA ram_0049_balloon_trip_rank_0x                          ; Write first digit (modulo)
C - - - - - 0x000585 00:C575: 68        PLA
C - - - - - 0x000586 00:C576: 85 12     STA ram_0012_temp
C - - - - - 0x000588 00:C578: 60        RTS
sub_C579_Rank_Score_Update:
C - - - - - 0x000589 00:C579: 20 39 C5  JSR sub_C539_Rank_Update                                   ; Update Balloon Trip Rank
C - - - - - 0x00058C 00:C57C: C6 12     DEC ram_0012_temp
C - - - - - 0x00058E 00:C57E: A9 31     LDA #$31                                                   ; A = (Rank - 49)
C - - - - - 0x000590 00:C580: 38        SEC
C - - - - - 0x000591 00:C581: E5 12     SBC ram_0012_temp
C - - - - - 0x000593 00:C583: 85 13     STA ram_0013_temp
C - - - - - 0x000595 00:C585: 0A        ASL
C - - - - - 0x000596 00:C586: 0A        ASL                                                        ; Y = A * 5
C - - - - - 0x000597 00:C587: 65 13     ADC ram_0013_temp
C - - - - - 0x000599 00:C589: A8        TAY
C - - - - - 0x00059A 00:C58A: A5 12     LDA ram_0012_temp
C - - - - - 0x00059C 00:C58C: 0A        ASL
C - - - - - 0x00059D 00:C58D: 0A        ASL                                                        ; [$1D] = Pointer to Score Rank
C - - - - - 0x00059E 00:C58E: 65 12     ADC ram_0012_temp
C - - - - - 0x0005A0 00:C590: 85 1D     STA ram_001D_loading_pointer_1d
C - - - - - 0x0005A2 00:C592: 18        CLC
C - - - - - 0x0005A3 00:C593: 69 05     ADC #$05
C - - - - - 0x0005A5 00:C595: 85 1F     STA ram_001F_gfx_enemy_data_pointer_1f                     ; [$1F] = Pointer to Score Rank+1
C - - - - - 0x0005A7 00:C597: A9 07     LDA #$07
C - - - - - 0x0005A9 00:C599: 85 1E     STA ram_001E_loading_pointer_1d
C - - - - - 0x0005AB 00:C59B: 85 20     STA ram_0020_gfx_enemy_data_pointer_1f
C - - - - - 0x0005AD 00:C59D: 98        TYA                                                        ; If Rank == 49 then
C - - - - - 0x0005AE 00:C59E: F0 0C     BEQ bra_C5AC                                               ; only update one rank score
C - - - - - 0x0005B0 00:C5A0: 88        DEY
bra_C5A1:
C - - - - - 0x0005B1 00:C5A1: B1 1D     LDA (ram_001D_loading_pointer_1d),Y                        ; Shift Balloon Trip
C - - - - - 0x0005B3 00:C5A3: 91 1F     STA (ram_001F_gfx_enemy_data_pointer_1f),Y                 ; Score Ranking
C - - - - - 0x0005B5 00:C5A5: 88        DEY                                                        ; by one rank above
C - - - - - 0x0005B6 00:C5A6: D0 F9     BNE bra_C5A1
C - - - - - 0x0005B8 00:C5A8: B1 1D     LDA (ram_001D_loading_pointer_1d),Y
C - - - - - 0x0005BA 00:C5AA: 91 1F     STA (ram_001F_gfx_enemy_data_pointer_1f),Y
bra_C5AC:
C - - - - - 0x0005BC 00:C5AC: A0 04     LDY #$04
bra_C5AE:
C - - - - - 0x0005BE 00:C5AE: B9 03 00  LDA ram_0003_p1_score_0000x,Y                              ; Copy current score
C - - - - - 0x0005C1 00:C5B1: 91 1D     STA (ram_001D_loading_pointer_1d),Y                        ; to current Score Rank
C - - - - - 0x0005C3 00:C5B3: 88        DEY
C - - - - - 0x0005C4 00:C5B4: 10 F8     BPL bra_C5AE
C - - - - - 0x0005C6 00:C5B6: 60        RTS
tbl_C5B7_Fish_Animation_0:
- D 2 - - - 0x0005C7 00:C5B7: 01        .byte $01   ; 
- D 2 - - - 0x0005C8 00:C5B8: 02        .byte $02   ; 
- D 2 - - - 0x0005C9 00:C5B9: 03        .byte $03   ; 
- D 2 - - - 0x0005CA 00:C5BA: 03        .byte $03   ; 
tbl_C5BB_Fish_Animation_1:
- D 2 - - - 0x0005CB 00:C5BB: 02        .byte $02   ; 
- D 2 - - - 0x0005CC 00:C5BC: 01        .byte $01   ; 
- D 2 - - - 0x0005CD 00:C5BD: FF        .byte $FF   ; 
- D 2 - - - 0x0005CE 00:C5BE: 03        .byte $03   ; 
- D 2 - - - 0x0005CF 00:C5BF: 04        .byte $04   ; 
- D 2 - - - 0x0005D0 00:C5C0: 05        .byte $05   ; 
- D 2 - - - 0x0005D1 00:C5C1: 06        .byte $06   ; 
- D 2 - - - 0x0005D2 00:C5C2: FF        .byte $FF   ; 
loc_C5C3:
C D 2 - - - 0x0005D3 00:C5C3: AD 8D 04  LDA ram_048D_fish_f_time
C - - - - - 0x0005D6 00:C5C6: 4A        LSR
C - - - - - 0x0005D7 00:C5C7: 4A        LSR
C - - - - - 0x0005D8 00:C5C8: 4A        LSR
C - - - - - 0x0005D9 00:C5C9: AA        TAX
C - - - - - 0x0005DA 00:C5CA: AD 8A 04  LDA ram_048A_fish_animation
C - - - - - 0x0005DD 00:C5CD: D0 06     BNE bra_C5D5                                               ; If Fish Animation? == 0
C - - - - - 0x0005DF 00:C5CF: BD B7 C5  LDA tbl_C5B7_Fish_Animation_0,X                            ; Set Fish Status
C - - - - - 0x0005E2 00:C5D2: 4C D8 C5  JMP loc_C5D8
bra_C5D5:
C - - - - - 0x0005E5 00:C5D5: BD BB C5  LDA tbl_C5BB_Fish_Animation_1,X                            ; If Fish Animation? != 0
loc_C5D8:
C D 2 - - - 0x0005E8 00:C5D8: 85 87     STA ram_0087_object_status_fish                            ; Update Fish Status
C - - - - - 0x0005EA 00:C5DA: A2 08     LDX #$08
C - - - - - 0x0005EC 00:C5DC: 20 A4 E3  JSR sub_E3A4
C - - - - - 0x0005EF 00:C5DF: AD 8C 04  LDA ram_048C_fish_target_eaten_flag                        ; If Fish Target Eaten Flag
C - - - - - 0x0005F2 00:C5E2: F0 2F     BEQ bra_C613_RTS                                           ; is set
C - - - - - 0x0005F4 00:C5E4: AE 8B 04  LDX ram_048B_fish_target_id                                ; X = Fish Target
C - - - - - 0x0005F7 00:C5E7: AD 8D 04  LDA ram_048D_fish_f_time
C - - - - - 0x0005FA 00:C5EA: C9 20     CMP #$20
C - - - - - 0x0005FC 00:C5EC: D0 06     BNE bra_C5F4                                               ; If Fish Frame Time == #$20
C - - - - - 0x0005FE 00:C5EE: A9 FF     LDA #$FF                                                   ; then target is eaten
C - - - - - 0x000600 00:C5F0: 95 88     STA ram_0088_object_balloons_p1,X                          ; (Balloons = -1)
C - - - - - 0x000602 00:C5F2: 30 1C     BMI bra_C610
bra_C5F4:
C - - - - - 0x000604 00:C5F4: B0 1D     BCS bra_C613_RTS                                           ; If Fish Frame Time < #$20
C - - - - - 0x000606 00:C5F6: AD 50 04  LDA ram_0450_direction_fish                                ; Depending on Fish Direction
C - - - - - 0x000609 00:C5F9: D0 07     BNE bra_C602
C - - - - - 0x00060B 00:C5FB: A5 99     LDA ram_0099_object_x_pos_int_fish
C - - - - - 0x00060D 00:C5FD: 18        CLC                                                        ; Move Fish 4 pixels to the right
C - - - - - 0x00060E 00:C5FE: 69 04     ADC #$04
C - - - - - 0x000610 00:C600: D0 05     BNE bra_C607
bra_C602:
C - - - - - 0x000612 00:C602: A5 99     LDA ram_0099_object_x_pos_int_fish                         ; Or
C - - - - - 0x000614 00:C604: 38        SEC                                                        ; Move Fish 4 pixels to the left
C - - - - - 0x000615 00:C605: E9 04     SBC #$04
bra_C607:
C - - - - - 0x000617 00:C607: 95 91     STA ram_0091_object_x_pos_int_p1,X
C - - - - - 0x000619 00:C609: A5 A2     LDA ram_00A2_object_y_pos_int_fish
C - - - - - 0x00061B 00:C60B: 38        SEC                                                        ; Fish Target's Y position =
C - - - - - 0x00061C 00:C60C: E9 0A     SBC #$0A                                                   ; (Fish Y - #$0A)
C - - - - - 0x00061E 00:C60E: 95 9A     STA ram_009A_object_y_pos_int_p1,X
bra_C610:
C - - - - - 0x000620 00:C610: 20 A4 E3  JSR sub_E3A4
bra_C613_RTS:
C - - - - - 0x000623 00:C613: 60        RTS
sub_C614_Fish_Search_Target:
C - - - - - 0x000624 00:C614: A9 FF     LDA #$FF                                                   ; Reset Target
C - - - - - 0x000626 00:C616: 8D 8B 04  STA ram_048B_fish_target_id                                ; to none
C - - - - - 0x000629 00:C619: A2 07     LDX #$07
bra_C61B:
C - - - - - 0x00062B 00:C61B: B5 88     LDA ram_0088_object_balloons_p1,X                          ; Check each object
C - - - - - 0x00062D 00:C61D: 30 0C     BMI bra_C62B                                               ; if it exists,
C - - - - - 0x00062F 00:C61F: B5 9A     LDA ram_009A_object_y_pos_int_p1,X                         ; if Y position >= #$9A
C - - - - - 0x000631 00:C621: C9 B4     CMP #$B4                                                   ; if X position == Fish X position
C - - - - - 0x000633 00:C623: 90 06     BCC bra_C62B                                               ; then the first one
C - - - - - 0x000635 00:C625: B5 91     LDA ram_0091_object_x_pos_int_p1,X                         ; that meets these conditions
C - - - - - 0x000637 00:C627: C5 99     CMP ram_0099_object_x_pos_int_fish                         ; is the target
C - - - - - 0x000639 00:C629: F0 04     BEQ bra_C62F
bra_C62B:
C - - - - - 0x00063B 00:C62B: CA        DEX                                                        ; else check next object
C - - - - - 0x00063C 00:C62C: 10 ED     BPL bra_C61B
C - - - - - 0x00063E 00:C62E: 60        RTS
bra_C62F:
C - - - - - 0x00063F 00:C62F: 8E 8B 04  STX ram_048B_fish_target_id                                ; Update Target
C - - - - - 0x000642 00:C632: BD 48 04  LDA ram_0448_direction_p1,X                                ; Update Fish Direction
C - - - - - 0x000645 00:C635: 8D 50 04  STA ram_0450_direction_fish                                ; with Target Object's Direction
C - - - - - 0x000648 00:C638: A9 00     LDA #$00
C - - - - - 0x00064A 00:C63A: 8D 8A 04  STA ram_048A_fish_animation                                ; Reset Fish Animation?
C - - - - - 0x00064D 00:C63D: 8D 8D 04  STA ram_048D_fish_f_time                                   ; Reset Fish Frame Time
C - - - - - 0x000650 00:C640: 8D 8C 04  STA ram_048C_fish_target_eaten_flag                        ; Reset Fish Target Eaten Flag
C - - - - - 0x000653 00:C643: 8D 89 04  STA ram_0489_fish_y_direction                              ; Reset Fish Y Direction to Up
C - - - - - 0x000656 00:C646: A9 DC     LDA #$DC                                                   ; Set Fish Y position to #$DC
C - - - - - 0x000658 00:C648: 85 A2     STA ram_00A2_object_y_pos_int_fish
C - - - - - 0x00065A 00:C64A: 60        RTS
sub_C64B_Fish_Move:
C - - - - - 0x00065B 00:C64B: E6 99     INC ram_0099_object_x_pos_int_fish                         ; Move Fish +1 pixel to the right
C - - - - - 0x00065D 00:C64D: A5 99     LDA ram_0099_object_x_pos_int_fish
C - - - - - 0x00065F 00:C64F: C9 B1     CMP #$B1                                                   ; If Fish X position >= #$B1
C - - - - - 0x000661 00:C651: 90 04     BCC bra_C657_RTS                                           ; then go back to X position = #$40
C - - - - - 0x000663 00:C653: A9 40     LDA #$40
C - - - - - 0x000665 00:C655: 85 99     STA ram_0099_object_x_pos_int_fish
bra_C657_RTS:
C - - - - - 0x000667 00:C657: 60        RTS
sub_C658:
C - - - - - 0x000668 00:C658: AD 89 04  LDA ram_0489_fish_y_direction                              ; If Fish Y Direction == Up
C - - - - - 0x00066B 00:C65B: D0 12     BNE bra_C66F
C - - - - - 0x00066D 00:C65D: C6 A2     DEC ram_00A2_object_y_pos_int_fish                         ; Fish Y goes up by 1 pixel
C - - - - - 0x00066F 00:C65F: A5 A2     LDA ram_00A2_object_y_pos_int_fish
C - - - - - 0x000671 00:C661: C9 C4     CMP #$C4                                                   ; If Fish Y position is about
C - - - - - 0x000673 00:C663: B0 0C     BCS bra_C671                                               ; to go above #$C4
C - - - - - 0x000675 00:C665: E6 A2     INC ram_00A2_object_y_pos_int_fish                         ; then
C - - - - - 0x000677 00:C667: EE 8A 04  INC ram_048A_fish_animation                                ; Set Fish Animation? to 1
C - - - - - 0x00067A 00:C66A: EE 89 04  INC ram_0489_fish_y_direction                              ; and Fish Y Direction to Down
C - - - - - 0x00067D 00:C66D: D0 02     BNE bra_C671
bra_C66F:
C - - - - - 0x00067F 00:C66F: E6 A2     INC ram_00A2_object_y_pos_int_fish                         ; Fish Y goes down by 1 pixel
bra_C671:
C - - - - - 0x000681 00:C671: EE 8D 04  INC ram_048D_fish_f_time                                   ; Increase Fish Frame Time
C - - - - - 0x000684 00:C674: AD 8D 04  LDA ram_048D_fish_f_time
C - - - - - 0x000687 00:C677: C9 18     CMP #$18                                                   ; If Fish Frame Time == #$18
C - - - - - 0x000689 00:C679: D0 28     BNE bra_C6A3
C - - - - - 0x00068B 00:C67B: AE 8B 04  LDX ram_048B_fish_target_id
C - - - - - 0x00068E 00:C67E: B5 88     LDA ram_0088_object_balloons_p1,X                          ; If Target exists...
C - - - - - 0x000690 00:C680: 30 21     BMI bra_C6A3                                               ; (has balloons)
C - - - - - 0x000692 00:C682: B5 9A     LDA ram_009A_object_y_pos_int_p1,X
C - - - - - 0x000694 00:C684: 18        CLC                                                        ; If the target is above
C - - - - - 0x000695 00:C685: 69 10     ADC #$10                                                   ; the fish by 10 pixels
C - - - - - 0x000697 00:C687: C5 A2     CMP ram_00A2_object_y_pos_int_fish
C - - - - - 0x000699 00:C689: 90 18     BCC bra_C6A3
C - - - - - 0x00069B 00:C68B: BC 51 04  LDY ram_0451_object_type_p1,X
C - - - - - 0x00069E 00:C68E: B9 B8 C6  LDA tbl_C6B8_Target_Object_Type,Y                          ; Change Target Object Type
C - - - - - 0x0006A1 00:C691: 9D 51 04  STA ram_0451_object_type_p1,X
C - - - - - 0x0006A4 00:C694: A9 00     LDA #$00                                                   ; Insta Kill
C - - - - - 0x0006A6 00:C696: 95 7F     STA ram_007F_object_status_p1,X                            ; Target Object Status = 00
C - - - - - 0x0006A8 00:C698: 95 88     STA ram_0088_object_balloons_p1,X                          ; Target Object Balloons == 0
C - - - - - 0x0006AA 00:C69A: A5 F2     LDA ram_00F2_music_jingle
C - - - - - 0x0006AC 00:C69C: 09 40     ORA #$40                                                   ; Play Fish Jingle
C - - - - - 0x0006AE 00:C69E: 85 F2     STA ram_00F2_music_jingle
C - - - - - 0x0006B0 00:C6A0: EE 8C 04  INC ram_048C_fish_target_eaten_flag                        ; Set Fish Target Eaten Flag
bra_C6A3:
C - - - - - 0x0006B3 00:C6A3: AD 8A 04  LDA ram_048A_fish_animation                                ; Fish Animation? != 0
C - - - - - 0x0006B6 00:C6A6: F0 0F     BEQ bra_C6B7_RTS
C - - - - - 0x0006B8 00:C6A8: AD 8D 04  LDA ram_048D_fish_f_time
C - - - - - 0x0006BB 00:C6AB: C9 28     CMP #$28                                                   ; If Fish Frame Time? == #$28
C - - - - - 0x0006BD 00:C6AD: F0 04     BEQ bra_C6B3                                               ; OR
C - - - - - 0x0006BF 00:C6AF: C9 30     CMP #$30                                                   ; If Fish Frame Time? == #$30
C - - - - - 0x0006C1 00:C6B1: D0 04     BNE bra_C6B7_RTS
bra_C6B3:
C - - - - - 0x0006C3 00:C6B3: A9 CC     LDA #$CC                                                   ; then
C - - - - - 0x0006C5 00:C6B5: 85 A2     STA ram_00A2_object_y_pos_int_fish                         ; Fish Y position = #$CC
bra_C6B7_RTS:
C - - - - - 0x0006C7 00:C6B7: 60        RTS
tbl_C6B8_Target_Object_Type:
- - - - - - 0x0006C8 00:C6B8: 08        .byte $08   ; 
- - - - - - 0x0006C9 00:C6B9: 09        .byte $09   ; 
- - - - - - 0x0006CA 00:C6BA: 0A        .byte $0A   ; 
- D 2 - - - 0x0006CB 00:C6BB: 0B        .byte $0B   ; 
- D 2 - - - 0x0006CC 00:C6BC: 08        .byte $08   ; 
- D 2 - - - 0x0006CD 00:C6BD: 09        .byte $09   ; 
- D 2 - - - 0x0006CE 00:C6BE: 0A        .byte $0A   ; 
- D 2 - - - 0x0006CF 00:C6BF: 0B        .byte $0B   ; 
- - - - - - 0x0006D0 00:C6C0: 08        .byte $08   ; 
- - - - - - 0x0006D1 00:C6C1: 09        .byte $09   ; 
- - - - - - 0x0006D2 00:C6C2: 0A        .byte $0A   ; 
- - - - - - 0x0006D3 00:C6C3: 0B        .byte $0B   ; 
sub_C6C4:
C - - - - - 0x0006D4 00:C6C4: AD 89 04  LDA ram_0489_fish_y_direction                              ; If Fish Direction is Up
C - - - - - 0x0006D7 00:C6C7: D0 2F     BNE bra_C6F8_RTS
C - - - - - 0x0006D9 00:C6C9: AE 8B 04  LDX ram_048B_fish_target_id
C - - - - - 0x0006DC 00:C6CC: B5 88     LDA ram_0088_object_balloons_p1,X                          ; Does Object X exist?
C - - - - - 0x0006DE 00:C6CE: 30 10     BMI bra_C6E0
C - - - - - 0x0006E0 00:C6D0: B5 9A     LDA ram_009A_object_y_pos_int_p1,X
C - - - - - 0x0006E2 00:C6D2: C9 B4     CMP #$B4                                                   ; Is Object X >= Y position #$B4?
C - - - - - 0x0006E4 00:C6D4: 90 0A     BCC bra_C6E0
C - - - - - 0x0006E6 00:C6D6: B5 91     LDA ram_0091_object_x_pos_int_p1,X
C - - - - - 0x0006E8 00:C6D8: C9 40     CMP #$40                                                   ; Is Object X between
C - - - - - 0x0006EA 00:C6DA: 90 04     BCC bra_C6E0                                               ; X positions #$40 and #$B1?
C - - - - - 0x0006EC 00:C6DC: C9 B1     CMP #$B1                                                   ; If so, teleport fish
C - - - - - 0x0006EE 00:C6DE: 90 0E     BCC bra_C6EE
bra_C6E0:
C - - - - - 0x0006F0 00:C6E0: A9 30     LDA #$30                                                   ; Else
C - - - - - 0x0006F2 00:C6E2: 38        SEC                                                        ; Fish Frame Time? = #$30 - itself
C - - - - - 0x0006F3 00:C6E3: ED 8D 04  SBC ram_048D_fish_f_time
C - - - - - 0x0006F6 00:C6E6: 8D 8D 04  STA ram_048D_fish_f_time
C - - - - - 0x0006F9 00:C6E9: EE 89 04  INC ram_0489_fish_y_direction                              ; Set Fish Direction to Down
C - - - - - 0x0006FC 00:C6EC: D0 0A     BNE bra_C6F8_RTS
bra_C6EE:
C - - - - - 0x0006FE 00:C6EE: B5 91     LDA ram_0091_object_x_pos_int_p1,X                         ; Teleport Fish
C - - - - - 0x000700 00:C6F0: 85 99     STA ram_0099_object_x_pos_int_fish                         ; to Object's X position
C - - - - - 0x000702 00:C6F2: BD 48 04  LDA ram_0448_direction_p1,X                                ; Change Fish Direction
C - - - - - 0x000705 00:C6F5: 8D 50 04  STA ram_0450_direction_fish                                ; to Object's Direction
bra_C6F8_RTS:
C - - - - - 0x000708 00:C6F8: 60        RTS
sub_C6F9_Fish_Manage:
C - - - - - 0x000709 00:C6F9: A5 87     LDA ram_0087_object_status_fish                            ; If Fish Status >= 0
C - - - - - 0x00070B 00:C6FB: 10 10     BPL bra_C70D                                               ; then handle eating
C - - - - - 0x00070D 00:C6FD: 20 4B C6  JSR sub_C64B_Fish_Move
C - - - - - 0x000710 00:C700: 20 14 C6  JSR sub_C614_Fish_Search_Target
C - - - - - 0x000713 00:C703: AD 8B 04  LDA ram_048B_fish_target_id                                ; If target found
C - - - - - 0x000716 00:C706: 10 01     BPL bra_C709                                               ; then handle fish attack
C - - - - - 0x000718 00:C708: 60        RTS
bra_C709:
C - - - - - 0x000719 00:C709: A9 40     LDA #$40                                                   ; Play Fish Eating SFX
C - - - - - 0x00071B 00:C70B: 85 F3     STA ram_00F3_sfx_3
bra_C70D:
C - - - - - 0x00071D 00:C70D: 20 C4 C6  JSR sub_C6C4                                               ; Handle Fish Teleport to Target
C - - - - - 0x000720 00:C710: 20 58 C6  JSR sub_C658                                               ; Handle Fish Target Eating
C - - - - - 0x000723 00:C713: 4C C3 C5  JMP loc_C5C3                                               ; Handle Fish Target Eating Movement
sub_C716_Init_Cloud_Bolt:
C - - - - - 0x000726 00:C716: A2 01     LDX #$01
bra_C718:
C - - - - - 0x000728 00:C718: A9 FF     LDA #$FF                                                   ; Reset 2 Lightning Bolts
C - - - - - 0x00072A 00:C71A: 9D 30 05  STA ram_0530_bolt_animation_f_01,X
C - - - - - 0x00072D 00:C71D: 9D 44 05  STA ram_0544_lightning_bolt_01,X
C - - - - - 0x000730 00:C720: CA        DEX
C - - - - - 0x000731 00:C721: 10 F5     BPL bra_C718
C - - - - - 0x000733 00:C723: 20 7A C7  JSR sub_C77A_Cloud_Bolt_Select                             ; Select Cloud that sends the bolt?
sub_C726:
C - - - - - 0x000736 00:C726: A6 3C     LDX ram_003C_current_phase
C - - - - - 0x000738 00:C728: E0 18     CPX #$18                                                   ; There are only 25 (#$18) phases
C - - - - - 0x00073A 00:C72A: 90 02     BCC bra_C72E                                               ; X = Current Phase OR X = 24
- - - - - - 0x00073C 00:C72C: A2 18     LDX #$18
bra_C72E:
C - - - - - 0x00073E 00:C72E: BD 48 C7  LDA tbl_C748_Bolt_Intensity,X                              ; Change Lightning Bolt Intensity
C - - - - - 0x000741 00:C731: 85 BA     STA ram_00BA_bolt_intensity_speed
C - - - - - 0x000743 00:C733: BD 61 C7  LDA tbl_C761_Bolt_Countdown,X                              ; Change Lightning Bolt Countdown
C - - - - - 0x000746 00:C736: 85 B8     STA ram_00B8_bolt_countdown                                ; Depending on Current Phase
C - - - - - 0x000748 00:C738: A9 F0     LDA #$F0
C - - - - - 0x00074A 00:C73A: 8D E0 02  STA ram_02E0_sprite_38_y                                   ; Hide last 3 sprites
C - - - - - 0x00074D 00:C73D: 8D E4 02  STA ram_02E4_sprite_39_y
C - - - - - 0x000750 00:C740: 8D E8 02  STA ram_02E8_sprite_3A_y
C - - - - - 0x000753 00:C743: A9 03     LDA #$03
C - - - - - 0x000755 00:C745: 4C 56 C8  JMP loc_C856                                               ; Blink selected cloud
tbl_C748_Bolt_Intensity:
- D 2 - - - 0x000758 00:C748: 00        .byte $00   ; Phase 01
- D 2 - - - 0x000759 00:C749: 00        .byte $00   ; Phase 02
- D 2 - - - 0x00075A 00:C74A: 00        .byte $00   ; Phase 03
- D 2 - - - 0x00075B 00:C74B: 00        .byte $00   ; Phase 04
- D 2 - - - 0x00075C 00:C74C: 00        .byte $00   ; Phase 05
- D 2 - - - 0x00075D 00:C74D: 00        .byte $00   ; Phase 06
- D 2 - - - 0x00075E 00:C74E: 00        .byte $00   ; Phase 07
- D 2 - - - 0x00075F 00:C74F: 00        .byte $00   ; Phase 08
- D 2 - - - 0x000760 00:C750: 00        .byte $00   ; Phase 09
- D 2 - - - 0x000761 00:C751: 00        .byte $00   ; Phase 10
- D 2 - - - 0x000762 00:C752: 01        .byte $01   ; Phase 11
- D 2 - - - 0x000763 00:C753: 01        .byte $01   ; Phase 12
- D 2 - - - 0x000764 00:C754: 01        .byte $01   ; Phase 13
- D 2 - - - 0x000765 00:C755: 01        .byte $01   ; Phase 14
- D 2 - - - 0x000766 00:C756: 01        .byte $01   ; Phase 15
- D 2 - - - 0x000767 00:C757: 01        .byte $01   ; Phase 16
- D 2 - - - 0x000768 00:C758: 02        .byte $02   ; Phase 17
- D 2 - - - 0x000769 00:C759: 01        .byte $01   ; Phase 18
- D 2 - - - 0x00076A 00:C75A: 01        .byte $01   ; Phase 19
- D 2 - - - 0x00076B 00:C75B: 01        .byte $01   ; Phase 20
- D 2 - - - 0x00076C 00:C75C: 01        .byte $01   ; Phase 21
- D 2 - - - 0x00076D 00:C75D: 01        .byte $01   ; Phase 22
- D 2 - - - 0x00076E 00:C75E: 01        .byte $01   ; Phase 23
- D 2 - - - 0x00076F 00:C75F: 01        .byte $01   ; Phase 24
- - - - - - 0x000770 00:C760: 01        .byte $01   ; Phase 25
tbl_C761_Bolt_Countdown:
- D 2 - - - 0x000771 00:C761: 0F        .byte $0F   ; Phase 01
- D 2 - - - 0x000772 00:C762: 0F        .byte $0F   ; Phase 02
- D 2 - - - 0x000773 00:C763: 0C        .byte $0C   ; Phase 03
- D 2 - - - 0x000774 00:C764: 0C        .byte $0C   ; Phase 04
- D 2 - - - 0x000775 00:C765: 0C        .byte $0C   ; Phase 05
- D 2 - - - 0x000776 00:C766: 0C        .byte $0C   ; Phase 06
- D 2 - - - 0x000777 00:C767: 0A        .byte $0A   ; Phase 07
- D 2 - - - 0x000778 00:C768: 0A        .byte $0A   ; Phase 08
- D 2 - - - 0x000779 00:C769: 0A        .byte $0A   ; Phase 09
- D 2 - - - 0x00077A 00:C76A: 0A        .byte $0A   ; Phase 10
- D 2 - - - 0x00077B 00:C76B: 0C        .byte $0C   ; Phase 11
- D 2 - - - 0x00077C 00:C76C: 0C        .byte $0C   ; Phase 12
- D 2 - - - 0x00077D 00:C76D: 0A        .byte $0A   ; Phase 13
- D 2 - - - 0x00077E 00:C76E: 0A        .byte $0A   ; Phase 14
- D 2 - - - 0x00077F 00:C76F: 0A        .byte $0A   ; Phase 15
- D 2 - - - 0x000780 00:C770: 08        .byte $08   ; Phase 16
- D 2 - - - 0x000781 00:C771: 0A        .byte $0A   ; Phase 17
- D 2 - - - 0x000782 00:C772: 0A        .byte $0A   ; Phase 18
- D 2 - - - 0x000783 00:C773: 08        .byte $08   ; Phase 19
- D 2 - - - 0x000784 00:C774: 08        .byte $08   ; Phase 20
- D 2 - - - 0x000785 00:C775: 08        .byte $08   ; Phase 21
- D 2 - - - 0x000786 00:C776: 08        .byte $08   ; Phase 22
- D 2 - - - 0x000787 00:C777: 08        .byte $08   ; Phase 23
- D 2 - - - 0x000788 00:C778: 08        .byte $08   ; Phase 24
- - - - - - 0x000789 00:C779: 05        .byte $05   ; Phase 25
sub_C77A_Cloud_Bolt_Select:
loc_C77A_Cloud_Bolt_Select:
; Randomly select a cloud to send bolts?
C D 2 - - - 0x00078A 00:C77A: A5 A3     LDA ram_00A3_amount_of_clouds                              ; If there are clouds, then select one
C - - - - - 0x00078C 00:C77C: 10 03     BPL bra_C781                                               ; else don't do anything
bra_C77E:
C - - - - - 0x00078E 00:C77E: 85 A4     STA ram_00A4_cloud_id_blink                                ; Select cloud
C - - - - - 0x000790 00:C780: 60        RTS
bra_C781:
C - - - - - 0x000791 00:C781: 20 B3 F1  JSR sub_F1B3_RNG
loc_C784:
C D 2 - - - 0x000794 00:C784: C5 A3     CMP ram_00A3_amount_of_clouds                              ; If RNG value <= amount of Clouds
C - - - - - 0x000796 00:C786: 90 F6     BCC bra_C77E                                               ; then select cloud based on value
C - - - - - 0x000798 00:C788: F0 F4     BEQ bra_C77E
C - - - - - 0x00079A 00:C78A: 18        CLC                                                        ; Subtract to the RNG
C - - - - - 0x00079B 00:C78B: E5 A3     SBC ram_00A3_amount_of_clouds                              ; the amount of Clouds
C - - - - - 0x00079D 00:C78D: 4C 84 C7  JMP loc_C784                                               ; until the condition is right
sub_C790_Cloud_Bolt:
C - - - - - 0x0007A0 00:C790: A5 19     LDA ram_0019_f_counter
C - - - - - 0x0007A2 00:C792: 29 7F     AND #$7F                                                   ; Every 128 frames...
C - - - - - 0x0007A4 00:C794: F0 01     BEQ bra_C797
bra_C796_RTS:
C - - - - - 0x0007A6 00:C796: 60        RTS
bra_C797:
C - - - - - 0x0007A7 00:C797: C6 B8     DEC ram_00B8_bolt_countdown                                ; Do Lightning Bolt Countdown
C - - - - - 0x0007A9 00:C799: D0 FB     BNE bra_C796_RTS                                           ; ...once it reaches zero...
C - - - - - 0x0007AB 00:C79B: A2 00     LDX #$00
C - - - - - 0x0007AD 00:C79D: BD 30 05  LDA ram_0530_bolt_animation_f_01,X
C - - - - - 0x0007B0 00:C7A0: 30 0B     BMI bra_C7AD
C - - - - - 0x0007B2 00:C7A2: E8        INX
C - - - - - 0x0007B3 00:C7A3: BD 30 05  LDA ram_0530_bolt_animation_f_01,X
C - - - - - 0x0007B6 00:C7A6: 30 05     BMI bra_C7AD
- - - - - - 0x0007B8 00:C7A8: A9 01     LDA #$01
- - - - - - 0x0007BA 00:C7AA: 85 B8     STA ram_00B8_bolt_countdown
- - - - - - 0x0007BC 00:C7AC: 60        RTS
bra_C7AD:
C - - - - - 0x0007BD 00:C7AD: A4 A4     LDY ram_00A4_cloud_id_blink
C - - - - - 0x0007BF 00:C7AF: 84 A5     STY ram_00A5_cloud_id_lightning
C - - - - - 0x0007C1 00:C7B1: 10 01     BPL bra_C7B4
- - - - - - 0x0007C3 00:C7B3: 60        RTS
bra_C7B4:
C - - - - - 0x0007C4 00:C7B4: A9 80     LDA #$80
C - - - - - 0x0007C6 00:C7B6: 9D B8 04  STA ram_04B8_bolt_x_pos_frac_01,X
C - - - - - 0x0007C9 00:C7B9: 9D CC 04  STA ram_04CC_bolt_y_pos_frac_01,X
C - - - - - 0x0007CC 00:C7BC: A9 00     LDA #$00
C - - - - - 0x0007CE 00:C7BE: 9D 30 05  STA ram_0530_bolt_animation_f_01,X
C - - - - - 0x0007D1 00:C7C1: B9 B2 00  LDA ram_00B2_cloud_related,Y
C - - - - - 0x0007D4 00:C7C4: 9D 90 04  STA ram_0490_bolt_x_pos_int_01,X
C - - - - - 0x0007D7 00:C7C7: B9 B5 00  LDA ram_00B5_cloud_related,Y
C - - - - - 0x0007DA 00:C7CA: 9D A4 04  STA ram_04A4_bolt_y_pos_int_01,X
C - - - - - 0x0007DD 00:C7CD: A4 BA     LDY ram_00BA_bolt_intensity_speed
C - - - - - 0x0007DF 00:C7CF: 20 B3 F1  JSR sub_F1B3_RNG
C - - - - - 0x0007E2 00:C7D2: 29 1F     AND #$1F
C - - - - - 0x0007E4 00:C7D4: 79 9F C8  ADC tbl_C89F,Y
C - - - - - 0x0007E7 00:C7D7: 9D 08 05  STA ram_0508_bolt_x_vel_frac_01,X
C - - - - - 0x0007EA 00:C7DA: B9 AB C8  LDA tbl_C8AB,Y
C - - - - - 0x0007ED 00:C7DD: 9D 1C 05  STA ram_051C_bolt_y_vel_frac_01,X
C - - - - - 0x0007F0 00:C7E0: B9 A5 C8  LDA tbl_C8A5,Y
C - - - - - 0x0007F3 00:C7E3: 9D E0 04  STA ram_04E0_bolt_x_vel_int_01,X
C - - - - - 0x0007F6 00:C7E6: B9 B1 C8  LDA tbl_C8B1,Y
C - - - - - 0x0007F9 00:C7E9: 9D F4 04  STA ram_04F4_bolt_y_vel_int_01,X
C - - - - - 0x0007FC 00:C7EC: 20 B3 F1  JSR sub_F1B3_RNG
C - - - - - 0x0007FF 00:C7EF: 29 03     AND #$03
C - - - - - 0x000801 00:C7F1: 9D 44 05  STA ram_0544_lightning_bolt_01,X
C - - - - - 0x000804 00:C7F4: A8        TAY
C - - - - - 0x000805 00:C7F5: B9 97 C8  LDA tbl_C897,Y
C - - - - - 0x000808 00:C7F8: 18        CLC
C - - - - - 0x000809 00:C7F9: 7D 90 04  ADC ram_0490_bolt_x_pos_int_01,X
C - - - - - 0x00080C 00:C7FC: 9D 90 04  STA ram_0490_bolt_x_pos_int_01,X
C - - - - - 0x00080F 00:C7FF: B9 9B C8  LDA tbl_C89B,Y
C - - - - - 0x000812 00:C802: 18        CLC
C - - - - - 0x000813 00:C803: 7D A4 04  ADC ram_04A4_bolt_y_pos_int_01,X
C - - - - - 0x000816 00:C806: 9D A4 04  STA ram_04A4_bolt_y_pos_int_01,X
C - - - - - 0x000819 00:C809: B9 8F C8  LDA tbl_C88F,Y
C - - - - - 0x00081C 00:C80C: F0 03     BEQ bra_C811
C - - - - - 0x00081E 00:C80E: 20 3D CA  JSR sub_CA3D
bra_C811:
C - - - - - 0x000821 00:C811: B9 93 C8  LDA tbl_C893,Y
C - - - - - 0x000824 00:C814: F0 03     BEQ bra_C819
C - - - - - 0x000826 00:C816: 20 55 CA  JSR sub_CA55
bra_C819:
C - - - - - 0x000829 00:C819: A5 BA     LDA ram_00BA_bolt_intensity_speed
C - - - - - 0x00082B 00:C81B: C9 05     CMP #$05
C - - - - - 0x00082D 00:C81D: B0 02     BCS bra_C821
C - - - - - 0x00082F 00:C81F: E6 BA     INC ram_00BA_bolt_intensity_speed
bra_C821:
C - - - - - 0x000831 00:C821: A9 06     LDA #$06
C - - - - - 0x000833 00:C823: 38        SEC
C - - - - - 0x000834 00:C824: E5 BA     SBC ram_00BA_bolt_intensity_speed
C - - - - - 0x000836 00:C826: 85 B8     STA ram_00B8_bolt_countdown
C - - - - - 0x000838 00:C828: A5 F0     LDA ram_00F0_sfx_1
C - - - - - 0x00083A 00:C82A: 09 04     ORA #$04                                                   ; Play Lightning Bolt Sound
C - - - - - 0x00083C 00:C82C: 85 F0     STA ram_00F0_sfx_1
C - - - - - 0x00083E 00:C82E: 4C 7A C7  JMP loc_C77A_Cloud_Bolt_Select
sub_C831_Cloud_Blink:
C - - - - - 0x000841 00:C831: A5 B8     LDA ram_00B8_bolt_countdown                                ; If Lightning Bolt Countdown != 1
C - - - - - 0x000843 00:C833: C9 01     CMP #$01                                                   ; then return
C - - - - - 0x000845 00:C835: D0 53     BNE bra_C88A_RTS
C - - - - - 0x000847 00:C837: AD 30 05  LDA ram_0530_bolt_animation_f_01                           ; If Lightning Bolt 0 doesn't exist
C - - - - - 0x00084A 00:C83A: 30 0A     BMI bra_C846                                               ; then prepare for one
C - - - - - 0x00084C 00:C83C: AD 31 05  LDA ram_0531_bolt_animation_f_02                           ; If Lightning Bolt 1 doesn't exist
C - - - - - 0x00084F 00:C83F: 30 05     BMI bra_C846                                               ; then prepare for one
C - - - - - 0x000851 00:C841: A9 02     LDA #$02                                                   ; Else up the countdown to 2
C - - - - - 0x000853 00:C843: 85 B8     STA ram_00B8_bolt_countdown
C - - - - - 0x000855 00:C845: 60        RTS
bra_C846:
C - - - - - 0x000856 00:C846: A5 19     LDA ram_0019_f_counter
C - - - - - 0x000858 00:C848: 29 7F     AND #$7F                                                   ; If Frame Counter < 64
C - - - - - 0x00085A 00:C84A: C9 40     CMP #$40                                                   ; then don't do anything
C - - - - - 0x00085C 00:C84C: 90 3C     BCC bra_C88A_RTS                                           ; If not equal to 64
C - - - - - 0x00085E 00:C84E: D0 06     BNE bra_C856                                               ; then don't play SFX
C - - - - - 0x000860 00:C850: A5 F1     LDA ram_00F1_sfx_2
C - - - - - 0x000862 00:C852: 09 08     ORA #$08                                                   ; Play Sound Effect
C - - - - - 0x000864 00:C854: 85 F1     STA ram_00F1_sfx_2
bra_C856:
loc_C856:
sub_C856:
C D 2 - - - 0x000866 00:C856: 29 03     AND #$03
C - - - - - 0x000868 00:C858: AA        TAX
C - - - - - 0x000869 00:C859: BD 8B C8  LDA tbl_C88B,X
C - - - - - 0x00086C 00:C85C: 85 5A     STA ram_005A
C - - - - - 0x00086E 00:C85E: A6 A4     LDX ram_00A4_cloud_id_blink                                ; Blink the selected cloud
C - - - - - 0x000870 00:C860: 30 28     BMI bra_C88A_RTS
C - - - - - 0x000872 00:C862: A9 23     LDA #$23
C - - - - - 0x000874 00:C864: 85 57     STA ram_0057_ppu_buffer_upload_data                        ; Set Tile Attribute Palette
C - - - - - 0x000876 00:C866: B5 A6     LDA ram_00A6,X                                             ; at PPUADDR[$23xx], Size = 1
C - - - - - 0x000878 00:C868: 85 58     STA ram_0058
C - - - - - 0x00087A 00:C86A: A9 01     LDA #$01
C - - - - - 0x00087C 00:C86C: 85 59     STA ram_0059
C - - - - - 0x00087E 00:C86E: 20 83 C8  JSR sub_C883                                               ; Set 16x16 Tile Attribute 1
C - - - - - 0x000881 00:C871: B5 A9     LDA ram_00A9,X
C - - - - - 0x000883 00:C873: 85 58     STA ram_0058
C - - - - - 0x000885 00:C875: 20 83 C8  JSR sub_C883                                               ; Set 16x16 Tile Attribute 2
C - - - - - 0x000888 00:C878: B5 AC     LDA ram_00AC,X
C - - - - - 0x00088A 00:C87A: 85 58     STA ram_0058
C - - - - - 0x00088C 00:C87C: 20 83 C8  JSR sub_C883                                               ; Set 16x16 Tile Attribute 3
C - - - - - 0x00088F 00:C87F: B5 AF     LDA ram_00AF,X
C - - - - - 0x000891 00:C881: 85 58     STA ram_0058
sub_C883:
C - - - - - 0x000893 00:C883: A9 57     LDA #< ram_0057_ppu_buffer_upload_data                     ; Set 16x16 Tile Attribute 4
C - - - - - 0x000895 00:C885: A0 00     LDY #> ram_0057_ppu_buffer_upload_data                     ; Copy Temp PPU Block
C - - - - - 0x000897 00:C887: 4C 31 C1  JMP sub_C131_Copy_PPU_Block                                ; [$0057]
bra_C88A_RTS:
C - - - - - 0x00089A 00:C88A: 60        RTS
tbl_C88B:
- D 2 - - - 0x00089B 00:C88B: 55        .byte $55   ; 
- D 2 - - - 0x00089C 00:C88C: FF        .byte $FF   ; 
- D 2 - - - 0x00089D 00:C88D: 00        .byte $00   ; 
- D 2 - - - 0x00089E 00:C88E: FF        .byte $FF   ; 
tbl_C88F:
- D 2 - - - 0x00089F 00:C88F: 00        .byte $00   ; 
- D 2 - - - 0x0008A0 00:C890: 00        .byte $00   ; 
- D 2 - - - 0x0008A1 00:C891: FF        .byte $FF   ; 
- D 2 - - - 0x0008A2 00:C892: FF        .byte $FF   ; 
tbl_C893:
- D 2 - - - 0x0008A3 00:C893: FF        .byte $FF   ; 
- D 2 - - - 0x0008A4 00:C894: 00        .byte $00   ; 
- D 2 - - - 0x0008A5 00:C895: 00        .byte $00   ; 
- D 2 - - - 0x0008A6 00:C896: FF        .byte $FF   ; 
tbl_C897:
- D 2 - - - 0x0008A7 00:C897: 10        .byte $10   ; 
- D 2 - - - 0x0008A8 00:C898: 10        .byte $10   ; 
- D 2 - - - 0x0008A9 00:C899: F0        .byte $F0   ; 
- D 2 - - - 0x0008AA 00:C89A: F0        .byte $F0   ; 
tbl_C89B:
- D 2 - - - 0x0008AB 00:C89B: DE        .byte $DE   ; 
- D 2 - - - 0x0008AC 00:C89C: 22        .byte $22   ; 
- D 2 - - - 0x0008AD 00:C89D: 22        .byte $22   ; 
- D 2 - - - 0x0008AE 00:C89E: DE        .byte $DE   ; 
tbl_C89F:
- D 2 - - - 0x0008AF 00:C89F: 60        .byte $60   ; 
- D 2 - - - 0x0008B0 00:C8A0: 70        .byte $70   ; 
- D 2 - - - 0x0008B1 00:C8A1: 80        .byte $80   ; 
- D 2 - - - 0x0008B2 00:C8A2: 90        .byte $90   ; 
- D 2 - - - 0x0008B3 00:C8A3: A0        .byte $A0   ; 
- D 2 - - - 0x0008B4 00:C8A4: B0        .byte $B0   ; 
tbl_C8A5:
- D 2 - - - 0x0008B5 00:C8A5: 00        .byte $00   ; 
- D 2 - - - 0x0008B6 00:C8A6: 00        .byte $00   ; 
- D 2 - - - 0x0008B7 00:C8A7: 00        .byte $00   ; 
- D 2 - - - 0x0008B8 00:C8A8: 00        .byte $00   ; 
- D 2 - - - 0x0008B9 00:C8A9: 00        .byte $00   ; 
- D 2 - - - 0x0008BA 00:C8AA: 00        .byte $00   ; 
tbl_C8AB:
- D 2 - - - 0x0008BB 00:C8AB: C0        .byte $C0   ; 
- D 2 - - - 0x0008BC 00:C8AC: F0        .byte $F0   ; 
- D 2 - - - 0x0008BD 00:C8AD: 20        .byte $20   ; 
- D 2 - - - 0x0008BE 00:C8AE: 50        .byte $50   ; 
- D 2 - - - 0x0008BF 00:C8AF: 80        .byte $80   ; 
- D 2 - - - 0x0008C0 00:C8B0: B0        .byte $B0   ; 
tbl_C8B1:
- D 2 - - - 0x0008C1 00:C8B1: 00        .byte $00   ; 
- D 2 - - - 0x0008C2 00:C8B2: 00        .byte $00   ; 
- D 2 - - - 0x0008C3 00:C8B3: 01        .byte $01   ; 
- D 2 - - - 0x0008C4 00:C8B4: 01        .byte $01   ; 
- D 2 - - - 0x0008C5 00:C8B5: 01        .byte $01   ; 
- D 2 - - - 0x0008C6 00:C8B6: 01        .byte $01   ; 
sub_C8B7:
C - - - - - 0x0008C7 00:C8B7: A2 01     LDX #$01
loc_C8B9:
C D 2 - - - 0x0008C9 00:C8B9: BD 30 05  LDA ram_0530_bolt_animation_f_01,X
C - - - - - 0x0008CC 00:C8BC: 10 03     BPL bra_C8C1
C - - - - - 0x0008CE 00:C8BE: 4C AF C9  JMP loc_C9AF
bra_C8C1:
C - - - - - 0x0008D1 00:C8C1: BD 44 05  LDA ram_0544_lightning_bolt_01,X
C - - - - - 0x0008D4 00:C8C4: 30 7B     BMI bra_C941
C - - - - - 0x0008D6 00:C8C6: A8        TAY
C - - - - - 0x0008D7 00:C8C7: 8A        TXA
C - - - - - 0x0008D8 00:C8C8: 48        PHA
C - - - - - 0x0008D9 00:C8C9: A6 A5     LDX ram_00A5_cloud_id_lightning
C - - - - - 0x0008DB 00:C8CB: B5 B2     LDA ram_00B2_cloud_related,X
C - - - - - 0x0008DD 00:C8CD: 79 E5 C9  ADC tbl_C9E5,Y
C - - - - - 0x0008E0 00:C8D0: 8D E3 02  STA ram_02E3_sprite_38_x
C - - - - - 0x0008E3 00:C8D3: 8D E7 02  STA ram_02E7_sprite_39_x
C - - - - - 0x0008E6 00:C8D6: 8D EB 02  STA ram_02EB_sprite_3A_x
C - - - - - 0x0008E9 00:C8D9: B5 B5     LDA ram_00B5_cloud_related,X
C - - - - - 0x0008EB 00:C8DB: 79 F5 C9  ADC tbl_C9F5,Y
C - - - - - 0x0008EE 00:C8DE: 8D E0 02  STA ram_02E0_sprite_38_y
C - - - - - 0x0008F1 00:C8E1: 79 05 CA  ADC tbl_CA05,Y
C - - - - - 0x0008F4 00:C8E4: 8D E4 02  STA ram_02E4_sprite_39_y
C - - - - - 0x0008F7 00:C8E7: 79 05 CA  ADC tbl_CA05,Y
C - - - - - 0x0008FA 00:C8EA: 8D E8 02  STA ram_02E8_sprite_3A_y
C - - - - - 0x0008FD 00:C8ED: 98        TYA
C - - - - - 0x0008FE 00:C8EE: 29 03     AND #$03
C - - - - - 0x000900 00:C8F0: AA        TAX
C - - - - - 0x000901 00:C8F1: 98        TYA
C - - - - - 0x000902 00:C8F2: 4A        LSR
C - - - - - 0x000903 00:C8F3: 4A        LSR
C - - - - - 0x000904 00:C8F4: A8        TAY
C - - - - - 0x000905 00:C8F5: A5 19     LDA ram_0019_f_counter
C - - - - - 0x000907 00:C8F7: 4A        LSR
C - - - - - 0x000908 00:C8F8: 4A        LSR
C - - - - - 0x000909 00:C8F9: B0 04     BCS bra_C8FF
C - - - - - 0x00090B 00:C8FB: 98        TYA
C - - - - - 0x00090C 00:C8FC: 69 05     ADC #$05
C - - - - - 0x00090E 00:C8FE: A8        TAY
bra_C8FF:
C - - - - - 0x00090F 00:C8FF: B9 15 CA  LDA tbl_CA15,Y
C - - - - - 0x000912 00:C902: 8D E1 02  STA ram_02E1_sprite_38_tile
C - - - - - 0x000915 00:C905: B9 1F CA  LDA tbl_CA1F,Y
C - - - - - 0x000918 00:C908: 8D E5 02  STA ram_02E5_sprite_39_tile
C - - - - - 0x00091B 00:C90B: B9 29 CA  LDA tbl_CA29,Y
C - - - - - 0x00091E 00:C90E: 8D E9 02  STA ram_02E9_sprite_3A_tile
C - - - - - 0x000921 00:C911: BD 33 CA  LDA tbl_CA33,X
C - - - - - 0x000924 00:C914: 8D E2 02  STA ram_02E2_sprite_38_attributes
C - - - - - 0x000927 00:C917: 8D E6 02  STA ram_02E6_sprite_39_attributes
C - - - - - 0x00092A 00:C91A: 8D EA 02  STA ram_02EA_sprite_3A_attributes
C - - - - - 0x00092D 00:C91D: 68        PLA
C - - - - - 0x00092E 00:C91E: AA        TAX
C - - - - - 0x00092F 00:C91F: A5 19     LDA ram_0019_f_counter
C - - - - - 0x000931 00:C921: 29 07     AND #$07
C - - - - - 0x000933 00:C923: D0 12     BNE bra_C937
C - - - - - 0x000935 00:C925: BD 44 05  LDA ram_0544_lightning_bolt_01,X
C - - - - - 0x000938 00:C928: 18        CLC
C - - - - - 0x000939 00:C929: 69 04     ADC #$04
C - - - - - 0x00093B 00:C92B: 9D 44 05  STA ram_0544_lightning_bolt_01,X
C - - - - - 0x00093E 00:C92E: C9 14     CMP #$14
C - - - - - 0x000940 00:C930: 90 05     BCC bra_C937
C - - - - - 0x000942 00:C932: A9 FF     LDA #$FF
C - - - - - 0x000944 00:C934: 9D 44 05  STA ram_0544_lightning_bolt_01,X
bra_C937:
C - - - - - 0x000947 00:C937: BD 44 05  LDA ram_0544_lightning_bolt_01,X
C - - - - - 0x00094A 00:C93A: C9 10     CMP #$10
C - - - - - 0x00094C 00:C93C: B0 03     BCS bra_C941
C - - - - - 0x00094E 00:C93E: 4C AF C9  JMP loc_C9AF
bra_C941:
C - - - - - 0x000951 00:C941: 20 B6 C9  JSR sub_C9B6_Bolt_Update
C - - - - - 0x000954 00:C944: BD 90 04  LDA ram_0490_bolt_x_pos_int_01,X
C - - - - - 0x000957 00:C947: C9 02     CMP #$02
C - - - - - 0x000959 00:C949: B0 03     BCS bra_C94E
C - - - - - 0x00095B 00:C94B: 20 37 CA  JSR sub_CA37
bra_C94E:
C - - - - - 0x00095E 00:C94E: BD 90 04  LDA ram_0490_bolt_x_pos_int_01,X
C - - - - - 0x000961 00:C951: C9 F7     CMP #$F7
C - - - - - 0x000963 00:C953: 90 03     BCC bra_C958
C - - - - - 0x000965 00:C955: 20 37 CA  JSR sub_CA37
bra_C958:
C - - - - - 0x000968 00:C958: BD A4 04  LDA ram_04A4_bolt_y_pos_int_01,X
C - - - - - 0x00096B 00:C95B: C9 02     CMP #$02
C - - - - - 0x00096D 00:C95D: B0 03     BCS bra_C962
C - - - - - 0x00096F 00:C95F: 20 4F CA  JSR sub_CA4F
bra_C962:
C - - - - - 0x000972 00:C962: BD A4 04  LDA ram_04A4_bolt_y_pos_int_01,X
C - - - - - 0x000975 00:C965: C9 E0     CMP #$E0
C - - - - - 0x000977 00:C967: 90 0D     BCC bra_C976
C - - - - - 0x000979 00:C969: A9 FF     LDA #$FF
C - - - - - 0x00097B 00:C96B: 9D 30 05  STA ram_0530_bolt_animation_f_01,X
C - - - - - 0x00097E 00:C96E: A9 F0     LDA #$F0
C - - - - - 0x000980 00:C970: 9D A4 04  STA ram_04A4_bolt_y_pos_int_01,X
C - - - - - 0x000983 00:C973: 4C AF C9  JMP loc_C9AF
bra_C976:
C - - - - - 0x000986 00:C976: 20 67 CA  JSR sub_CA67
C - - - - - 0x000989 00:C979: 20 1C CB  JSR sub_CB1C_Bolt_Player_Collision
C - - - - - 0x00098C 00:C97C: BC 30 05  LDY ram_0530_bolt_animation_f_01,X
C - - - - - 0x00098F 00:C97F: C8        INY
C - - - - - 0x000990 00:C980: 98        TYA
C - - - - - 0x000991 00:C981: 29 07     AND #$07
C - - - - - 0x000993 00:C983: 9D 30 05  STA ram_0530_bolt_animation_f_01,X
C - - - - - 0x000996 00:C986: BC 30 05  LDY ram_0530_bolt_animation_f_01,X
C - - - - - 0x000999 00:C989: B9 DD C9  LDA tbl_C9DD,Y
C - - - - - 0x00099C 00:C98C: 85 12     STA ram_0012_temp
C - - - - - 0x00099E 00:C98E: 8A        TXA
C - - - - - 0x00099F 00:C98F: 0A        ASL
C - - - - - 0x0009A0 00:C990: 0A        ASL
C - - - - - 0x0009A1 00:C991: 18        CLC
C - - - - - 0x0009A2 00:C992: A8        TAY
C - - - - - 0x0009A3 00:C993: BD A4 04  LDA ram_04A4_bolt_y_pos_int_01,X
C - - - - - 0x0009A6 00:C996: C9 D0     CMP #$D0
C - - - - - 0x0009A8 00:C998: 99 00 02  STA ram_0200_sprite_00_y,Y
C - - - - - 0x0009AB 00:C99B: BD 90 04  LDA ram_0490_bolt_x_pos_int_01,X
C - - - - - 0x0009AE 00:C99E: 99 03 02  STA ram_0203_sprite_00_x,Y
C - - - - - 0x0009B1 00:C9A1: A5 12     LDA ram_0012_temp
C - - - - - 0x0009B3 00:C9A3: 99 01 02  STA ram_0201_sprite_00_tile,Y
C - - - - - 0x0009B6 00:C9A6: A9 00     LDA #$00
C - - - - - 0x0009B8 00:C9A8: 90 02     BCC bra_C9AC
C - - - - - 0x0009BA 00:C9AA: A9 20     LDA #$20
bra_C9AC:
C - - - - - 0x0009BC 00:C9AC: 99 02 02  STA ram_0202_sprite_00_attributes,Y
loc_C9AF:
C D 2 - - - 0x0009BF 00:C9AF: CA        DEX
C - - - - - 0x0009C0 00:C9B0: 30 03     BMI bra_C9B5_RTS
C - - - - - 0x0009C2 00:C9B2: 4C B9 C8  JMP loc_C8B9
bra_C9B5_RTS:
C - - - - - 0x0009C5 00:C9B5: 60        RTS
sub_C9B6_Bolt_Update:
C - - - - - 0x0009C6 00:C9B6: BD 08 05  LDA ram_0508_bolt_x_vel_frac_01,X
C - - - - - 0x0009C9 00:C9B9: 18        CLC                                                        ; Update X Position (Frac)
C - - - - - 0x0009CA 00:C9BA: 7D B8 04  ADC ram_04B8_bolt_x_pos_frac_01,X
C - - - - - 0x0009CD 00:C9BD: 9D B8 04  STA ram_04B8_bolt_x_pos_frac_01,X
C - - - - - 0x0009D0 00:C9C0: BD E0 04  LDA ram_04E0_bolt_x_vel_int_01,X
C - - - - - 0x0009D3 00:C9C3: 7D 90 04  ADC ram_0490_bolt_x_pos_int_01,X                           ; Update X Position (Int)
C - - - - - 0x0009D6 00:C9C6: 9D 90 04  STA ram_0490_bolt_x_pos_int_01,X
C - - - - - 0x0009D9 00:C9C9: BD 1C 05  LDA ram_051C_bolt_y_vel_frac_01,X
C - - - - - 0x0009DC 00:C9CC: 18        CLC                                                        ; Update Y Position (Frac)
C - - - - - 0x0009DD 00:C9CD: 7D CC 04  ADC ram_04CC_bolt_y_pos_frac_01,X
C - - - - - 0x0009E0 00:C9D0: 9D CC 04  STA ram_04CC_bolt_y_pos_frac_01,X
C - - - - - 0x0009E3 00:C9D3: BD F4 04  LDA ram_04F4_bolt_y_vel_int_01,X
C - - - - - 0x0009E6 00:C9D6: 7D A4 04  ADC ram_04A4_bolt_y_pos_int_01,X                           ; Update Y Position (Int)
C - - - - - 0x0009E9 00:C9D9: 9D A4 04  STA ram_04A4_bolt_y_pos_int_01,X
C - - - - - 0x0009EC 00:C9DC: 60        RTS
tbl_C9DD:
- D 2 - - - 0x0009ED 00:C9DD: 9D        .byte $9D   ; 
- D 2 - - - 0x0009EE 00:C9DE: 9E        .byte $9E   ; 
- D 2 - - - 0x0009EF 00:C9DF: 9F        .byte $9F   ; 
- D 2 - - - 0x0009F0 00:C9E0: 9E        .byte $9E   ; 
- D 2 - - - 0x0009F1 00:C9E1: 9D        .byte $9D   ; 
- D 2 - - - 0x0009F2 00:C9E2: A0        .byte $A0   ; 
- D 2 - - - 0x0009F3 00:C9E3: A1        .byte $A1   ; 
- D 2 - - - 0x0009F4 00:C9E4: A0        .byte $A0   ; 
tbl_C9E5:
- D 2 - - - 0x0009F5 00:C9E5: 08        .byte $08   ; 
- D 2 - - - 0x0009F6 00:C9E6: 08        .byte $08   ; 
- D 2 - - - 0x0009F7 00:C9E7: F0        .byte $F0   ; 
- D 2 - - - 0x0009F8 00:C9E8: F0        .byte $F0   ; 
- D 2 - - - 0x0009F9 00:C9E9: 08        .byte $08   ; 
- D 2 - - - 0x0009FA 00:C9EA: 08        .byte $08   ; 
- D 2 - - - 0x0009FB 00:C9EB: F0        .byte $F0   ; 
- D 2 - - - 0x0009FC 00:C9EC: F0        .byte $F0   ; 
- D 2 - - - 0x0009FD 00:C9ED: 08        .byte $08   ; 
- D 2 - - - 0x0009FE 00:C9EE: 08        .byte $08   ; 
- D 2 - - - 0x0009FF 00:C9EF: F0        .byte $F0   ; 
- D 2 - - - 0x000A00 00:C9F0: F0        .byte $F0   ; 
- D 2 - - - 0x000A01 00:C9F1: 08        .byte $08   ; 
- D 2 - - - 0x000A02 00:C9F2: 08        .byte $08   ; 
- D 2 - - - 0x000A03 00:C9F3: F0        .byte $F0   ; 
- D 2 - - - 0x000A04 00:C9F4: F0        .byte $F0   ; 
tbl_C9F5:
- D 2 - - - 0x000A05 00:C9F5: EE        .byte $EE   ; 
- D 2 - - - 0x000A06 00:C9F6: 0A        .byte $0A   ; 
- D 2 - - - 0x000A07 00:C9F7: 0A        .byte $0A   ; 
- D 2 - - - 0x000A08 00:C9F8: EE        .byte $EE   ; 
- D 2 - - - 0x000A09 00:C9F9: EE        .byte $EE   ; 
- D 2 - - - 0x000A0A 00:C9FA: 0A        .byte $0A   ; 
- D 2 - - - 0x000A0B 00:C9FB: 0A        .byte $0A   ; 
- D 2 - - - 0x000A0C 00:C9FC: EE        .byte $EE   ; 
- D 2 - - - 0x000A0D 00:C9FD: EE        .byte $EE   ; 
- D 2 - - - 0x000A0E 00:C9FE: 0A        .byte $0A   ; 
- D 2 - - - 0x000A0F 00:C9FF: 0A        .byte $0A   ; 
- D 2 - - - 0x000A10 00:CA00: EE        .byte $EE   ; 
- D 2 - - - 0x000A11 00:CA01: EE        .byte $EE   ; 
- D 2 - - - 0x000A12 00:CA02: 0A        .byte $0A   ; 
- D 2 - - - 0x000A13 00:CA03: 0A        .byte $0A   ; 
- D 2 - - - 0x000A14 00:CA04: EE        .byte $EE   ; 
tbl_CA05:
- D 2 - - - 0x000A15 00:CA05: F8        .byte $F8   ; 
- D 2 - - - 0x000A16 00:CA06: 08        .byte $08   ; 
- D 2 - - - 0x000A17 00:CA07: 08        .byte $08   ; 
- D 2 - - - 0x000A18 00:CA08: F8        .byte $F8   ; 
- D 2 - - - 0x000A19 00:CA09: F8        .byte $F8   ; 
- D 2 - - - 0x000A1A 00:CA0A: 08        .byte $08   ; 
- D 2 - - - 0x000A1B 00:CA0B: 08        .byte $08   ; 
- D 2 - - - 0x000A1C 00:CA0C: F8        .byte $F8   ; 
- D 2 - - - 0x000A1D 00:CA0D: F8        .byte $F8   ; 
- D 2 - - - 0x000A1E 00:CA0E: 08        .byte $08   ; 
- D 2 - - - 0x000A1F 00:CA0F: 08        .byte $08   ; 
- D 2 - - - 0x000A20 00:CA10: F8        .byte $F8   ; 
- D 2 - - - 0x000A21 00:CA11: F8        .byte $F8   ; 
- D 2 - - - 0x000A22 00:CA12: 08        .byte $08   ; 
- D 2 - - - 0x000A23 00:CA13: 08        .byte $08   ; 
- D 2 - - - 0x000A24 00:CA14: F8        .byte $F8   ; 
tbl_CA15:
- D 2 - - - 0x000A25 00:CA15: 91        .byte $91   ; 
- D 2 - - - 0x000A26 00:CA16: 93        .byte $93   ; 
- D 2 - - - 0x000A27 00:CA17: 97        .byte $97   ; 
- D 2 - - - 0x000A28 00:CA18: 97        .byte $97   ; 
- D 2 - - - 0x000A29 00:CA19: FC        .byte $FC   ; 
- D 2 - - - 0x000A2A 00:CA1A: 92        .byte $92   ; 
- D 2 - - - 0x000A2B 00:CA1B: 95        .byte $95   ; 
- D 2 - - - 0x000A2C 00:CA1C: 9A        .byte $9A   ; 
- D 2 - - - 0x000A2D 00:CA1D: 9A        .byte $9A   ; 
- D 2 - - - 0x000A2E 00:CA1E: FC        .byte $FC   ; 
tbl_CA1F:
- - - - - - 0x000A2F 00:CA1F: FC        .byte $FC   ; 
- D 2 - - - 0x000A30 00:CA20: 94        .byte $94   ; 
- D 2 - - - 0x000A31 00:CA21: 98        .byte $98   ; 
- D 2 - - - 0x000A32 00:CA22: 98        .byte $98   ; 
- D 2 - - - 0x000A33 00:CA23: FC        .byte $FC   ; 
- D 2 - - - 0x000A34 00:CA24: FC        .byte $FC   ; 
- D 2 - - - 0x000A35 00:CA25: 96        .byte $96   ; 
- D 2 - - - 0x000A36 00:CA26: 9B        .byte $9B   ; 
- D 2 - - - 0x000A37 00:CA27: 9B        .byte $9B   ; 
- D 2 - - - 0x000A38 00:CA28: FC        .byte $FC   ; 
tbl_CA29:
- - - - - - 0x000A39 00:CA29: FC        .byte $FC   ; 
- D 2 - - - 0x000A3A 00:CA2A: FC        .byte $FC   ; 
- D 2 - - - 0x000A3B 00:CA2B: 99        .byte $99   ; 
- D 2 - - - 0x000A3C 00:CA2C: 99        .byte $99   ; 
- D 2 - - - 0x000A3D 00:CA2D: FC        .byte $FC   ; 
- D 2 - - - 0x000A3E 00:CA2E: FC        .byte $FC   ; 
- D 2 - - - 0x000A3F 00:CA2F: FC        .byte $FC   ; 
- D 2 - - - 0x000A40 00:CA30: 9C        .byte $9C   ; 
- D 2 - - - 0x000A41 00:CA31: 9C        .byte $9C   ; 
- D 2 - - - 0x000A42 00:CA32: FC        .byte $FC   ; 
tbl_CA33:
- D 2 - - - 0x000A43 00:CA33: C0        .byte $C0   ; 
- D 2 - - - 0x000A44 00:CA34: 40        .byte $40   ; 
- D 2 - - - 0x000A45 00:CA35: 00        .byte $00   ; 
- D 2 - - - 0x000A46 00:CA36: 80        .byte $80   ; 
sub_CA37:
C - - - - - 0x000A47 00:CA37: A5 F3     LDA ram_00F3_sfx_3
C - - - - - 0x000A49 00:CA39: 09 80     ORA #$80                                                   ; Play Bolt Bounce SFX
C - - - - - 0x000A4B 00:CA3B: 85 F3     STA ram_00F3_sfx_3
sub_CA3D:
C - - - - - 0x000A4D 00:CA3D: A9 00     LDA #$00
C - - - - - 0x000A4F 00:CA3F: 38        SEC
C - - - - - 0x000A50 00:CA40: FD 08 05  SBC ram_0508_bolt_x_vel_frac_01,X                          ; Lightning Bolt
C - - - - - 0x000A53 00:CA43: 9D 08 05  STA ram_0508_bolt_x_vel_frac_01,X                          ; Reverse X Velocity
C - - - - - 0x000A56 00:CA46: A9 00     LDA #$00
C - - - - - 0x000A58 00:CA48: FD E0 04  SBC ram_04E0_bolt_x_vel_int_01,X
C - - - - - 0x000A5B 00:CA4B: 9D E0 04  STA ram_04E0_bolt_x_vel_int_01,X
C - - - - - 0x000A5E 00:CA4E: 60        RTS
sub_CA4F:
C - - - - - 0x000A5F 00:CA4F: A5 F3     LDA ram_00F3_sfx_3
C - - - - - 0x000A61 00:CA51: 09 80     ORA #$80                                                   ; Play Bolt Bounce SFX
C - - - - - 0x000A63 00:CA53: 85 F3     STA ram_00F3_sfx_3
sub_CA55:
C - - - - - 0x000A65 00:CA55: A9 00     LDA #$00
C - - - - - 0x000A67 00:CA57: 38        SEC
C - - - - - 0x000A68 00:CA58: FD 1C 05  SBC ram_051C_bolt_y_vel_frac_01,X                          ; Lightning Bolt
C - - - - - 0x000A6B 00:CA5B: 9D 1C 05  STA ram_051C_bolt_y_vel_frac_01,X                          ; Reverse Y Velocity
C - - - - - 0x000A6E 00:CA5E: A9 00     LDA #$00
C - - - - - 0x000A70 00:CA60: FD F4 04  SBC ram_04F4_bolt_y_vel_int_01,X
C - - - - - 0x000A73 00:CA63: 9D F4 04  STA ram_04F4_bolt_y_vel_int_01,X
C - - - - - 0x000A76 00:CA66: 60        RTS
sub_CA67:
; Lightning Bolt Platform Collision?
C - - - - - 0x000A77 00:CA67: A4 CD     LDY ram_00CD_amount_of_platforms
loc_CA69:
C D 2 - - - 0x000A79 00:CA69: A9 00     LDA #$00
C - - - - - 0x000A7B 00:CA6B: 85 CC     STA ram_00CC_collision_related
C - - - - - 0x000A7D 00:CA6D: B1 27     LDA (ram_0027_plat_coll_pointer_top),Y
C - - - - - 0x000A7F 00:CA6F: 38        SEC
C - - - - - 0x000A80 00:CA70: E9 08     SBC #$08
C - - - - - 0x000A82 00:CA72: DD A4 04  CMP ram_04A4_bolt_y_pos_int_01,X
C - - - - - 0x000A85 00:CA75: B0 66     BCS bra_CADD
C - - - - - 0x000A87 00:CA77: 69 03     ADC #$03
C - - - - - 0x000A89 00:CA79: DD A4 04  CMP ram_04A4_bolt_y_pos_int_01,X
C - - - - - 0x000A8C 00:CA7C: 90 04     BCC bra_CA82
C - - - - - 0x000A8E 00:CA7E: A9 01     LDA #$01
C - - - - - 0x000A90 00:CA80: D0 10     BNE bra_CA92
bra_CA82:
C - - - - - 0x000A92 00:CA82: B1 29     LDA (ram_0029_plat_coll_pointer_bottom),Y
C - - - - - 0x000A94 00:CA84: DD A4 04  CMP ram_04A4_bolt_y_pos_int_01,X
C - - - - - 0x000A97 00:CA87: 90 54     BCC bra_CADD
C - - - - - 0x000A99 00:CA89: E9 03     SBC #$03
C - - - - - 0x000A9B 00:CA8B: DD A4 04  CMP ram_04A4_bolt_y_pos_int_01,X
C - - - - - 0x000A9E 00:CA8E: B0 1D     BCS bra_CAAD
C - - - - - 0x000AA0 00:CA90: A9 02     LDA #$02
bra_CA92:
C - - - - - 0x000AA2 00:CA92: 85 CC     STA ram_00CC_collision_related
C - - - - - 0x000AA4 00:CA94: B1 23     LDA (ram_0023_plat_coll_pointer_left),Y
C - - - - - 0x000AA6 00:CA96: C9 10     CMP #$10
C - - - - - 0x000AA8 00:CA98: F0 08     BEQ bra_CAA2
C - - - - - 0x000AAA 00:CA9A: 38        SEC
C - - - - - 0x000AAB 00:CA9B: E9 04     SBC #$04
C - - - - - 0x000AAD 00:CA9D: DD 90 04  CMP ram_0490_bolt_x_pos_int_01,X
C - - - - - 0x000AB0 00:CAA0: B0 07     BCS bra_CAA9
bra_CAA2:
C - - - - - 0x000AB2 00:CAA2: B1 25     LDA (ram_0025_plat_coll_pointer_right),Y
C - - - - - 0x000AB4 00:CAA4: DD 90 04  CMP ram_0490_bolt_x_pos_int_01,X
C - - - - - 0x000AB7 00:CAA7: B0 04     BCS bra_CAAD
bra_CAA9:
C - - - - - 0x000AB9 00:CAA9: A9 00     LDA #$00
C - - - - - 0x000ABB 00:CAAB: 85 CC     STA ram_00CC_collision_related
bra_CAAD:
C - - - - - 0x000ABD 00:CAAD: B1 23     LDA (ram_0023_plat_coll_pointer_left),Y
C - - - - - 0x000ABF 00:CAAF: C9 10     CMP #$10
C - - - - - 0x000AC1 00:CAB1: F0 15     BEQ bra_CAC8
C - - - - - 0x000AC3 00:CAB3: 38        SEC
C - - - - - 0x000AC4 00:CAB4: E9 08     SBC #$08
C - - - - - 0x000AC6 00:CAB6: DD 90 04  CMP ram_0490_bolt_x_pos_int_01,X
C - - - - - 0x000AC9 00:CAB9: B0 22     BCS bra_CADD
C - - - - - 0x000ACB 00:CABB: 69 03     ADC #$03
C - - - - - 0x000ACD 00:CABD: DD 90 04  CMP ram_0490_bolt_x_pos_int_01,X
C - - - - - 0x000AD0 00:CAC0: 90 06     BCC bra_CAC8
C - - - - - 0x000AD2 00:CAC2: A5 CC     LDA ram_00CC_collision_related
C - - - - - 0x000AD4 00:CAC4: 09 04     ORA #$04
C - - - - - 0x000AD6 00:CAC6: D0 13     BNE bra_CADB
bra_CAC8:
C - - - - - 0x000AD8 00:CAC8: B1 25     LDA (ram_0025_plat_coll_pointer_right),Y
C - - - - - 0x000ADA 00:CACA: C9 FF     CMP #$FF
C - - - - - 0x000ADC 00:CACC: F0 0F     BEQ bra_CADD
C - - - - - 0x000ADE 00:CACE: DD 90 04  CMP ram_0490_bolt_x_pos_int_01,X
C - - - - - 0x000AE1 00:CAD1: 90 0A     BCC bra_CADD
C - - - - - 0x000AE3 00:CAD3: E9 03     SBC #$03
C - - - - - 0x000AE5 00:CAD5: B0 06     BCS bra_CADD
- - - - - - 0x000AE7 00:CAD7: A5 CC     LDA ram_00CC_collision_related
- - - - - - 0x000AE9 00:CAD9: 09 08     ORA #$08
bra_CADB:
C - - - - - 0x000AEB 00:CADB: 85 CC     STA ram_00CC_collision_related
bra_CADD:
C - - - - - 0x000AED 00:CADD: A5 CC     LDA ram_00CC_collision_related
C - - - - - 0x000AEF 00:CADF: D0 07     BNE bra_CAE8
loc_CAE1:
C D 2 - - - 0x000AF1 00:CAE1: 88        DEY
C - - - - - 0x000AF2 00:CAE2: 30 03     BMI bra_CAE7_RTS
C - - - - - 0x000AF4 00:CAE4: 4C 69 CA  JMP loc_CA69
bra_CAE7_RTS:
C - - - - - 0x000AF7 00:CAE7: 60        RTS
bra_CAE8:
C - - - - - 0x000AF8 00:CAE8: 46 CC     LSR ram_00CC_collision_related
C - - - - - 0x000AFA 00:CAEA: 90 08     BCC bra_CAF4
C - - - - - 0x000AFC 00:CAEC: BD F4 04  LDA ram_04F4_bolt_y_vel_int_01,X
C - - - - - 0x000AFF 00:CAEF: 30 03     BMI bra_CAF4
C - - - - - 0x000B01 00:CAF1: 20 4F CA  JSR sub_CA4F
bra_CAF4:
C - - - - - 0x000B04 00:CAF4: 46 CC     LSR ram_00CC_collision_related
C - - - - - 0x000B06 00:CAF6: 90 08     BCC bra_CB00
C - - - - - 0x000B08 00:CAF8: BD F4 04  LDA ram_04F4_bolt_y_vel_int_01,X
C - - - - - 0x000B0B 00:CAFB: 10 03     BPL bra_CB00
C - - - - - 0x000B0D 00:CAFD: 20 4F CA  JSR sub_CA4F
bra_CB00:
C - - - - - 0x000B10 00:CB00: 46 CC     LSR ram_00CC_collision_related
C - - - - - 0x000B12 00:CB02: 90 08     BCC bra_CB0C
C - - - - - 0x000B14 00:CB04: BD E0 04  LDA ram_04E0_bolt_x_vel_int_01,X
C - - - - - 0x000B17 00:CB07: 30 03     BMI bra_CB0C
C - - - - - 0x000B19 00:CB09: 20 37 CA  JSR sub_CA37
bra_CB0C:
C - - - - - 0x000B1C 00:CB0C: 46 CC     LSR ram_00CC_collision_related
C - - - - - 0x000B1E 00:CB0E: 90 08     BCC bra_CB18
- - - - - - 0x000B20 00:CB10: BD E0 04  LDA ram_04E0_bolt_x_vel_int_01,X
- - - - - - 0x000B23 00:CB13: 10 03     BPL bra_CB18
- - - - - - 0x000B25 00:CB15: 20 37 CA  JSR sub_CA37
bra_CB18:
C - - - - - 0x000B28 00:CB18: 4C E1 CA  JMP loc_CAE1


; bzk garbage
- - - - - - 0x000B2B 00:CB1B: 60        RTS



sub_CB1C_Bolt_Player_Collision:
C - - - - - 0x000B2C 00:CB1C: A0 01     LDY #$01
bra_CB1E:
C - - - - - 0x000B2E 00:CB1E: B9 88 00  LDA ram_0088_object_balloons_p1,Y
C - - - - - 0x000B31 00:CB21: 30 4D     BMI bra_CB70                                               ; If Player Y has balloons...
C - - - - - 0x000B33 00:CB23: F0 4B     BEQ bra_CB70
C - - - - - 0x000B35 00:CB25: B9 BD 00  LDA ram_00BD_p1_invincibility_flag,Y                       ; and if Player Y is not invincible...
C - - - - - 0x000B38 00:CB28: D0 46     BNE bra_CB70
C - - - - - 0x000B3A 00:CB2A: BD 90 04  LDA ram_0490_bolt_x_pos_int_01,X
C - - - - - 0x000B3D 00:CB2D: 38        SEC                                                        ; If Player Y's X position
C - - - - - 0x000B3E 00:CB2E: F9 91 00  SBC ram_0091_object_x_pos_int_p1,Y                         ; is within the X position
C - - - - - 0x000B41 00:CB31: 20 8E F0  JSR sub_F08E_Absolute                                      ; of Lightning Bolt X
C - - - - - 0x000B44 00:CB34: C9 08     CMP #$08                                                   ; (size 8 pixels)
C - - - - - 0x000B46 00:CB36: B0 38     BCS bra_CB70
C - - - - - 0x000B48 00:CB38: BD A4 04  LDA ram_04A4_bolt_y_pos_int_01,X
C - - - - - 0x000B4B 00:CB3B: 38        SEC
C - - - - - 0x000B4C 00:CB3C: F9 9A 00  SBC ram_009A_object_y_pos_int_p1,Y                         ; If Player Y's Y position
C - - - - - 0x000B4F 00:CB3F: 38        SEC                                                        ; is within the Y position
C - - - - - 0x000B50 00:CB40: E9 08     SBC #$08                                                   ; of Lightning Bolt X
C - - - - - 0x000B52 00:CB42: 20 8E F0  JSR sub_F08E_Absolute                                      ; (size 12 pixels high
C - - - - - 0x000B55 00:CB45: C9 0C     CMP #$0C                                                   ; to take balloons into account)
C - - - - - 0x000B57 00:CB47: B0 27     BCS bra_CB70
C - - - - - 0x000B59 00:CB49: A9 00     LDA #$00
C - - - - - 0x000B5B 00:CB4B: 99 88 00  STA ram_0088_object_balloons_p1,Y                          ; Player Y's balloons = #$00
C - - - - - 0x000B5E 00:CB4E: A9 01     LDA #$01
C - - - - - 0x000B60 00:CB50: 99 7F 00  STA ram_007F_object_status_p1,Y                            ; Player Y's status = #$01
C - - - - - 0x000B63 00:CB53: 99 C1 00  STA ram_00C1_p1_freeze_flag,Y                              ; Player Y's freeze flag = #$01
C - - - - - 0x000B66 00:CB56: A9 0B     LDA #$0B
C - - - - - 0x000B68 00:CB58: 99 51 04  STA ram_0451_object_type_p1,Y                              ; Player Y's type = #$0B
C - - - - - 0x000B6B 00:CB5B: A9 20     LDA #$20
C - - - - - 0x000B6D 00:CB5D: 99 5A 04  STA ram_045A_shock_timer_p1,Y                              ; Player Y's shock time = #$20
C - - - - - 0x000B70 00:CB60: A5 F0     LDA ram_00F0_sfx_1
C - - - - - 0x000B72 00:CB62: 09 80     ORA #$80                                                   ; Play Shock SFX
C - - - - - 0x000B74 00:CB64: 85 F0     STA ram_00F0_sfx_1
C - - - - - 0x000B76 00:CB66: A9 F0     LDA #$F0
C - - - - - 0x000B78 00:CB68: 9D A4 04  STA ram_04A4_bolt_y_pos_int_01,X                           ; Lightning Bolt X
C - - - - - 0x000B7B 00:CB6B: A9 FF     LDA #$FF                                                   ; disappears
C - - - - - 0x000B7D 00:CB6D: 9D 30 05  STA ram_0530_bolt_animation_f_01,X
bra_CB70:
C - - - - - 0x000B80 00:CB70: 88        DEY                                                        ; Check next player
C - - - - - 0x000B81 00:CB71: 10 AB     BPL bra_CB1E
C - - - - - 0x000B83 00:CB73: 60        RTS

; Flipper code.

sub_CB74_Flipper_Manage:
C - - - - - 0x000B84 00:CB74: AE D1 05  LDX ram_05D1_amount_of_flippers
C - - - - - 0x000B87 00:CB77: 30 2E     BMI bra_CBA7_RTS
bra_CB79:
C - - - - - 0x000B89 00:CB79: 20 A8 CB  JSR sub_CBA8
C - - - - - 0x000B8C 00:CB7C: BD 04 06  LDA ram_0604,X
C - - - - - 0x000B8F 00:CB7F: F0 23     BEQ bra_CBA4
C - - - - - 0x000B91 00:CB81: 8A        TXA
C - - - - - 0x000B92 00:CB82: 45 19     EOR ram_0019_f_counter
C - - - - - 0x000B94 00:CB84: 29 01     AND #$01
C - - - - - 0x000B96 00:CB86: D0 1C     BNE bra_CBA4
C - - - - - 0x000B98 00:CB88: BC FA 05  LDY ram_05FA_flippers_type,X
C - - - - - 0x000B9B 00:CB8B: C8        INY
C - - - - - 0x000B9C 00:CB8C: 98        TYA
C - - - - - 0x000B9D 00:CB8D: 29 03     AND #$03
C - - - - - 0x000B9F 00:CB8F: 9D FA 05  STA ram_05FA_flippers_type,X
C - - - - - 0x000BA2 00:CB92: 20 CB CC  JSR sub_CCCB
C - - - - - 0x000BA5 00:CB95: BD FA 05  LDA ram_05FA_flippers_type,X
C - - - - - 0x000BA8 00:CB98: C9 01     CMP #$01
C - - - - - 0x000BAA 00:CB9A: D0 08     BNE bra_CBA4
C - - - - - 0x000BAC 00:CB9C: DE 0E 06  DEC ram_060E,X
C - - - - - 0x000BAF 00:CB9F: D0 03     BNE bra_CBA4
C - - - - - 0x000BB1 00:CBA1: DE 04 06  DEC ram_0604,X
bra_CBA4:
C - - - - - 0x000BB4 00:CBA4: CA        DEX
C - - - - - 0x000BB5 00:CBA5: 10 D2     BPL bra_CB79
bra_CBA7_RTS:
C - - - - - 0x000BB7 00:CBA7: 60        RTS
sub_CBA8:
C - - - - - 0x000BB8 00:CBA8: A0 07     LDY #$07
C - - - - - 0x000BBA 00:CBAA: BD 04 06  LDA ram_0604,X
C - - - - - 0x000BBD 00:CBAD: D0 03     BNE bra_CBB2
C - - - - - 0x000BBF 00:CBAF: 4C 3A CC  JMP loc_CC3A
bra_CBB2:
C - - - - - 0x000BC2 00:CBB2: B9 88 00  LDA ram_0088_object_balloons_p1,Y
C - - - - - 0x000BC5 00:CBB5: 30 78     BMI bra_CC2F
C - - - - - 0x000BC7 00:CBB7: F0 76     BEQ bra_CC2F
C - - - - - 0x000BC9 00:CBB9: C0 02     CPY #$02
C - - - - - 0x000BCB 00:CBBB: 90 04     BCC bra_CBC1
C - - - - - 0x000BCD 00:CBBD: C9 01     CMP #$01
C - - - - - 0x000BCF 00:CBBF: F0 6E     BEQ bra_CC2F
bra_CBC1:
C - - - - - 0x000BD1 00:CBC1: B9 91 00  LDA ram_0091_object_x_pos_int_p1,Y
C - - - - - 0x000BD4 00:CBC4: 18        CLC
C - - - - - 0x000BD5 00:CBC5: 69 08     ADC #$08
C - - - - - 0x000BD7 00:CBC7: 38        SEC
C - - - - - 0x000BD8 00:CBC8: FD D2 05  SBC ram_05D2_flipper_x_pos,X
C - - - - - 0x000BDB 00:CBCB: 85 12     STA ram_0012_temp
C - - - - - 0x000BDD 00:CBCD: 20 8E F0  JSR sub_F08E_Absolute
C - - - - - 0x000BE0 00:CBD0: C9 12     CMP #$12
C - - - - - 0x000BE2 00:CBD2: B0 5B     BCS bra_CC2F
C - - - - - 0x000BE4 00:CBD4: B9 9A 00  LDA ram_009A_object_y_pos_int_p1,Y
C - - - - - 0x000BE7 00:CBD7: 18        CLC
C - - - - - 0x000BE8 00:CBD8: 69 0C     ADC #$0C
C - - - - - 0x000BEA 00:CBDA: 38        SEC
C - - - - - 0x000BEB 00:CBDB: FD DC 05  SBC ram_05DC_flipper_y_pos,X
C - - - - - 0x000BEE 00:CBDE: 85 13     STA ram_0013_temp
C - - - - - 0x000BF0 00:CBE0: 20 8E F0  JSR sub_F08E_Absolute
C - - - - - 0x000BF3 00:CBE3: C9 12     CMP #$12
C - - - - - 0x000BF5 00:CBE5: B0 48     BCS bra_CC2F
C - - - - - 0x000BF7 00:CBE7: A5 12     LDA ram_0012_temp
C - - - - - 0x000BF9 00:CBE9: 30 11     BMI bra_CBFC
C - - - - - 0x000BFB 00:CBEB: C9 03     CMP #$03
C - - - - - 0x000BFD 00:CBED: 90 1C     BCC bra_CC0B
C - - - - - 0x000BFF 00:CBEF: A9 02     LDA #$02
C - - - - - 0x000C01 00:CBF1: 99 1B 04  STA ram_041B_y_vel_int_p1,Y
C - - - - - 0x000C04 00:CBF4: 20 33 CC  JSR sub_CC33
C - - - - - 0x000C07 00:CBF7: 20 BB EB  JSR sub_EBBB
C - - - - - 0x000C0A 00:CBFA: D0 0F     BNE bra_CC0B
bra_CBFC:
C - - - - - 0x000C0C 00:CBFC: C9 FD     CMP #$FD
C - - - - - 0x000C0E 00:CBFE: B0 0B     BCS bra_CC0B
C - - - - - 0x000C10 00:CC00: A9 FE     LDA #$FE
C - - - - - 0x000C12 00:CC02: 99 1B 04  STA ram_041B_y_vel_int_p1,Y
C - - - - - 0x000C15 00:CC05: 20 BB EB  JSR sub_EBBB
C - - - - - 0x000C18 00:CC08: 20 33 CC  JSR sub_CC33
bra_CC0B:
C - - - - - 0x000C1B 00:CC0B: A5 13     LDA ram_0013_temp
C - - - - - 0x000C1D 00:CC0D: 30 11     BMI bra_CC20
C - - - - - 0x000C1F 00:CC0F: C9 03     CMP #$03
C - - - - - 0x000C21 00:CC11: 90 1C     BCC bra_CC2F
C - - - - - 0x000C23 00:CC13: A9 02     LDA #$02
C - - - - - 0x000C25 00:CC15: 99 2D 04  STA ram_042D_x_vel_int_p1,Y
C - - - - - 0x000C28 00:CC18: 20 B2 EB  JSR sub_EBB2
C - - - - - 0x000C2B 00:CC1B: 20 33 CC  JSR sub_CC33
C - - - - - 0x000C2E 00:CC1E: D0 0F     BNE bra_CC2F
bra_CC20:
C - - - - - 0x000C30 00:CC20: C9 FD     CMP #$FD
C - - - - - 0x000C32 00:CC22: B0 0B     BCS bra_CC2F
C - - - - - 0x000C34 00:CC24: A9 FE     LDA #$FE
C - - - - - 0x000C36 00:CC26: 99 2D 04  STA ram_042D_x_vel_int_p1,Y
C - - - - - 0x000C39 00:CC29: 20 B2 EB  JSR sub_EBB2
C - - - - - 0x000C3C 00:CC2C: 20 33 CC  JSR sub_CC33
bra_CC2F:
C - - - - - 0x000C3F 00:CC2F: 88        DEY
C - - - - - 0x000C40 00:CC30: 10 80     BPL bra_CBB2
C - - - - - 0x000C42 00:CC32: 60        RTS
sub_CC33:
C - - - - - 0x000C43 00:CC33: A5 F1     LDA ram_00F1_sfx_2
C - - - - - 0x000C45 00:CC35: 09 02     ORA #$02
C - - - - - 0x000C47 00:CC37: 85 F1     STA ram_00F1_sfx_2
C - - - - - 0x000C49 00:CC39: 60        RTS
loc_CC3A:
C D 2 - - - 0x000C4A 00:CC3A: B9 88 00  LDA ram_0088_object_balloons_p1,Y
C - - - - - 0x000C4D 00:CC3D: 30 79     BMI bra_CCB8
C - - - - - 0x000C4F 00:CC3F: F0 77     BEQ bra_CCB8
C - - - - - 0x000C51 00:CC41: C0 02     CPY #$02
C - - - - - 0x000C53 00:CC43: 90 2E     BCC bra_CC73
C - - - - - 0x000C55 00:CC45: BD FA 05  LDA ram_05FA_flippers_type,X
C - - - - - 0x000C58 00:CC48: C9 03     CMP #$03
C - - - - - 0x000C5A 00:CC4A: D0 27     BNE bra_CC73
C - - - - - 0x000C5C 00:CC4C: BD D2 05  LDA ram_05D2_flipper_x_pos,X
C - - - - - 0x000C5F 00:CC4F: 38        SEC
C - - - - - 0x000C60 00:CC50: E9 0A     SBC #$0A
C - - - - - 0x000C62 00:CC52: D9 91 00  CMP ram_0091_object_x_pos_int_p1,Y
C - - - - - 0x000C65 00:CC55: B0 1C     BCS bra_CC73
C - - - - - 0x000C67 00:CC57: 69 04     ADC #$04
C - - - - - 0x000C69 00:CC59: D9 91 00  CMP ram_0091_object_x_pos_int_p1,Y
C - - - - - 0x000C6C 00:CC5C: 90 15     BCC bra_CC73
C - - - - - 0x000C6E 00:CC5E: BD DC 05  LDA ram_05DC_flipper_y_pos,X
C - - - - - 0x000C71 00:CC61: 38        SEC
C - - - - - 0x000C72 00:CC62: E9 1C     SBC #$1C
C - - - - - 0x000C74 00:CC64: D9 9A 00  CMP ram_009A_object_y_pos_int_p1,Y
C - - - - - 0x000C77 00:CC67: B0 0A     BCS bra_CC73
C - - - - - 0x000C79 00:CC69: 69 04     ADC #$04
C - - - - - 0x000C7B 00:CC6B: D9 9A 00  CMP ram_009A_object_y_pos_int_p1,Y
C - - - - - 0x000C7E 00:CC6E: 90 03     BCC bra_CC73
C - - - - - 0x000C80 00:CC70: 20 BF CC  JSR sub_CCBF
bra_CC73:
C - - - - - 0x000C83 00:CC73: B9 91 00  LDA ram_0091_object_x_pos_int_p1,Y
C - - - - - 0x000C86 00:CC76: 18        CLC
C - - - - - 0x000C87 00:CC77: 69 08     ADC #$08
C - - - - - 0x000C89 00:CC79: 38        SEC
C - - - - - 0x000C8A 00:CC7A: FD D2 05  SBC ram_05D2_flipper_x_pos,X
C - - - - - 0x000C8D 00:CC7D: 20 8E F0  JSR sub_F08E_Absolute
C - - - - - 0x000C90 00:CC80: 85 12     STA ram_0012_temp
C - - - - - 0x000C92 00:CC82: B9 9A 00  LDA ram_009A_object_y_pos_int_p1,Y
C - - - - - 0x000C95 00:CC85: 18        CLC
C - - - - - 0x000C96 00:CC86: 69 0C     ADC #$0C
C - - - - - 0x000C98 00:CC88: 38        SEC
C - - - - - 0x000C99 00:CC89: FD DC 05  SBC ram_05DC_flipper_y_pos,X
C - - - - - 0x000C9C 00:CC8C: 20 8E F0  JSR sub_F08E_Absolute
C - - - - - 0x000C9F 00:CC8F: 85 13     STA ram_0013_temp
C - - - - - 0x000CA1 00:CC91: BD FA 05  LDA ram_05FA_flippers_type,X
C - - - - - 0x000CA4 00:CC94: C9 03     CMP #$03
C - - - - - 0x000CA6 00:CC96: F0 0A     BEQ bra_CCA2
C - - - - - 0x000CA8 00:CC98: A5 12     LDA ram_0012_temp
C - - - - - 0x000CAA 00:CC9A: 48        PHA
C - - - - - 0x000CAB 00:CC9B: A5 13     LDA ram_0013_temp
C - - - - - 0x000CAD 00:CC9D: 85 12     STA ram_0012_temp
C - - - - - 0x000CAF 00:CC9F: 68        PLA
C - - - - - 0x000CB0 00:CCA0: 85 13     STA ram_0013_temp
bra_CCA2:
C - - - - - 0x000CB2 00:CCA2: A5 12     LDA ram_0012_temp
C - - - - - 0x000CB4 00:CCA4: C9 14     CMP #$14
C - - - - - 0x000CB6 00:CCA6: B0 10     BCS bra_CCB8
C - - - - - 0x000CB8 00:CCA8: A5 13     LDA ram_0013_temp
C - - - - - 0x000CBA 00:CCAA: C9 0B     CMP #$0B
C - - - - - 0x000CBC 00:CCAC: B0 0A     BCS bra_CCB8
C - - - - - 0x000CBE 00:CCAE: A9 01     LDA #$01
C - - - - - 0x000CC0 00:CCB0: 9D 04 06  STA ram_0604,X
C - - - - - 0x000CC3 00:CCB3: A9 32     LDA #$32
C - - - - - 0x000CC5 00:CCB5: 9D 0E 06  STA ram_060E,X
bra_CCB8:
C - - - - - 0x000CC8 00:CCB8: 88        DEY
C - - - - - 0x000CC9 00:CCB9: 30 03     BMI bra_CCBE_RTS
C - - - - - 0x000CCB 00:CCBB: 4C 3A CC  JMP loc_CC3A
bra_CCBE_RTS:
C - - - - - 0x000CCE 00:CCBE: 60        RTS
sub_CCBF:
C - - - - - 0x000CCF 00:CCBF: 8A        TXA
C - - - - - 0x000CD0 00:CCC0: 48        PHA
C - - - - - 0x000CD1 00:CCC1: 98        TYA
C - - - - - 0x000CD2 00:CCC2: AA        TAX
C - - - - - 0x000CD3 00:CCC3: E6 CB     INC ram_00CB
C - - - - - 0x000CD5 00:CCC5: 20 83 E9  JSR sub_E983
C - - - - - 0x000CD8 00:CCC8: 68        PLA
C - - - - - 0x000CD9 00:CCC9: AA        TAX
C - - - - - 0x000CDA 00:CCCA: 60        RTS
sub_CCCB:
C - - - - - 0x000CDB 00:CCCB: BD F0 05  LDA ram_05F0,X
C - - - - - 0x000CDE 00:CCCE: 85 57     STA ram_0057_ppu_buffer_upload_data
C - - - - - 0x000CE0 00:CCD0: BD E6 05  LDA ram_05E6,X
C - - - - - 0x000CE3 00:CCD3: 85 58     STA ram_0058
C - - - - - 0x000CE5 00:CCD5: A9 03     LDA #$03
C - - - - - 0x000CE7 00:CCD7: 85 59     STA ram_0059
C - - - - - 0x000CE9 00:CCD9: BC FA 05  LDY ram_05FA_flippers_type,X
C - - - - - 0x000CEC 00:CCDC: B9 26 CD  LDA tbl_CD26,Y
C - - - - - 0x000CEF 00:CCDF: 85 5A     STA ram_005A
C - - - - - 0x000CF1 00:CCE1: B9 2A CD  LDA tbl_CD2A,Y
C - - - - - 0x000CF4 00:CCE4: 85 5B     STA ram_005B
C - - - - - 0x000CF6 00:CCE6: B9 2E CD  LDA tbl_CD2E,Y
C - - - - - 0x000CF9 00:CCE9: 85 5C     STA ram_005C
C - - - - - 0x000CFB 00:CCEB: 20 0F CD  JSR sub_CD0F
C - - - - - 0x000CFE 00:CCEE: B9 32 CD  LDA tbl_CD32,Y
C - - - - - 0x000D01 00:CCF1: 85 5A     STA ram_005A
C - - - - - 0x000D03 00:CCF3: B9 36 CD  LDA tbl_CD36,Y
C - - - - - 0x000D06 00:CCF6: 85 5B     STA ram_005B
C - - - - - 0x000D08 00:CCF8: B9 3A CD  LDA tbl_CD3A,Y
C - - - - - 0x000D0B 00:CCFB: 85 5C     STA ram_005C
C - - - - - 0x000D0D 00:CCFD: 20 0F CD  JSR sub_CD0F
C - - - - - 0x000D10 00:CD00: B9 3E CD  LDA tbl_CD3E,Y
C - - - - - 0x000D13 00:CD03: 85 5A     STA ram_005A
C - - - - - 0x000D15 00:CD05: B9 42 CD  LDA tbl_CD42,Y
C - - - - - 0x000D18 00:CD08: 85 5B     STA ram_005B
C - - - - - 0x000D1A 00:CD0A: B9 46 CD  LDA tbl_CD46,Y
C - - - - - 0x000D1D 00:CD0D: 85 5C     STA ram_005C
sub_CD0F:
C - - - - - 0x000D1F 00:CD0F: 98        TYA
C - - - - - 0x000D20 00:CD10: 48        PHA
C - - - - - 0x000D21 00:CD11: A9 57     LDA #< ram_0057_ppu_buffer_upload_data
C - - - - - 0x000D23 00:CD13: A0 00     LDY #> ram_0057_ppu_buffer_upload_data
C - - - - - 0x000D25 00:CD15: 20 31 C1  JSR sub_C131_Copy_PPU_Block
C - - - - - 0x000D28 00:CD18: 68        PLA
C - - - - - 0x000D29 00:CD19: A8        TAY
C - - - - - 0x000D2A 00:CD1A: A5 58     LDA ram_0058
C - - - - - 0x000D2C 00:CD1C: 18        CLC
C - - - - - 0x000D2D 00:CD1D: 69 20     ADC #$20
C - - - - - 0x000D2F 00:CD1F: 85 58     STA ram_0058
C - - - - - 0x000D31 00:CD21: 90 02     BCC bra_CD25_RTS
C - - - - - 0x000D33 00:CD23: E6 57     INC ram_0057_ppu_buffer_upload_data
bra_CD25_RTS:
C - - - - - 0x000D35 00:CD25: 60        RTS
tbl_CD26:
- D 2 - - - 0x000D36 00:CD26: A1        .byte $A1   ; 
- D 2 - - - 0x000D37 00:CD27: 24        .byte $24   ; 
- D 2 - - - 0x000D38 00:CD28: 24        .byte $24   ; 
- D 2 - - - 0x000D39 00:CD29: 24        .byte $24   ; 
tbl_CD2A:
- D 2 - - - 0x000D3A 00:CD2A: A2        .byte $A2   ; 
- D 2 - - - 0x000D3B 00:CD2B: 9E        .byte $9E   ; 
- D 2 - - - 0x000D3C 00:CD2C: AB        .byte $AB   ; 
- D 2 - - - 0x000D3D 00:CD2D: 24        .byte $24   ; 
tbl_CD2E:
- D 2 - - - 0x000D3E 00:CD2E: 24        .byte $24   ; 
- D 2 - - - 0x000D3F 00:CD2F: 24        .byte $24   ; 
- D 2 - - - 0x000D40 00:CD30: AC        .byte $AC   ; 
- D 2 - - - 0x000D41 00:CD31: 24        .byte $24   ; 
tbl_CD32:
- D 2 - - - 0x000D42 00:CD32: A3        .byte $A3   ; 
- D 2 - - - 0x000D43 00:CD33: 24        .byte $24   ; 
- D 2 - - - 0x000D44 00:CD34: AD        .byte $AD   ; 
- D 2 - - - 0x000D45 00:CD35: A8        .byte $A8   ; 
tbl_CD36:
- D 2 - - - 0x000D46 00:CD36: A4        .byte $A4   ; 
- D 2 - - - 0x000D47 00:CD37: 9F        .byte $9F   ; 
- D 2 - - - 0x000D48 00:CD38: AE        .byte $AE   ; 
- D 2 - - - 0x000D49 00:CD39: A9        .byte $A9   ; 
tbl_CD3A:
- D 2 - - - 0x000D4A 00:CD3A: A5        .byte $A5   ; 
- D 2 - - - 0x000D4B 00:CD3B: 24        .byte $24   ; 
- D 2 - - - 0x000D4C 00:CD3C: AF        .byte $AF   ; 
- D 2 - - - 0x000D4D 00:CD3D: AA        .byte $AA   ; 
tbl_CD3E:
- D 2 - - - 0x000D4E 00:CD3E: 24        .byte $24   ; 
- D 2 - - - 0x000D4F 00:CD3F: 24        .byte $24   ; 
- D 2 - - - 0x000D50 00:CD40: B0        .byte $B0   ; 
- D 2 - - - 0x000D51 00:CD41: 24        .byte $24   ; 
tbl_CD42:
- D 2 - - - 0x000D52 00:CD42: A6        .byte $A6   ; 
- D 2 - - - 0x000D53 00:CD43: A0        .byte $A0   ; 
- D 2 - - - 0x000D54 00:CD44: B1        .byte $B1   ; 
- D 2 - - - 0x000D55 00:CD45: 24        .byte $24   ; 
tbl_CD46:
- D 2 - - - 0x000D56 00:CD46: A7        .byte $A7   ; 
- D 2 - - - 0x000D57 00:CD47: 24        .byte $24   ; 
- D 2 - - - 0x000D58 00:CD48: 24        .byte $24   ; 
- D 2 - - - 0x000D59 00:CD49: 24        .byte $24   ; 

; Balloon code.

sub_CD4A_Init_Balloons:
C - - - - - 0x000D5A 00:CD4A: A2 09     LDX #$09                                                   ; Reset all 10 balloons
bra_CD4C:
C - - - - - 0x000D5C 00:CD4C: A9 FF     LDA #$FF                                                   ; GFX = #$FF
C - - - - - 0x000D5E 00:CD4E: 9D 5D 05  STA ram_055D_balloon_gfx,X
C - - - - - 0x000D61 00:CD51: A9 F0     LDA #$F0                                                   ; Y positions = #$F0
C - - - - - 0x000D63 00:CD53: 9D 7B 05  STA ram_057B_balloon_y_pos,X
C - - - - - 0x000D66 00:CD56: CA        DEX
C - - - - - 0x000D67 00:CD57: 10 F3     BPL bra_CD4C
C - - - - - 0x000D69 00:CD59: 60        RTS
sub_CD5A:
C - - - - - 0x000D6A 00:CD5A: CE CC 05  DEC ram_05CC
C - - - - - 0x000D6D 00:CD5D: F0 01     BEQ bra_CD60
C - - - - - 0x000D6F 00:CD5F: 60        RTS
bra_CD60:
C - - - - - 0x000D70 00:CD60: A5 1B     LDA ram_001B_rng_output_seed
C - - - - - 0x000D72 00:CD62: 29 3F     AND #$3F
C - - - - - 0x000D74 00:CD64: 69 28     ADC #$28
C - - - - - 0x000D76 00:CD66: 8D CC 05  STA ram_05CC
C - - - - - 0x000D79 00:CD69: A2 09     LDX #$09
bra_CD6B:
C - - - - - 0x000D7B 00:CD6B: BD 5D 05  LDA ram_055D_balloon_gfx,X
C - - - - - 0x000D7E 00:CD6E: 30 04     BMI bra_CD74
C - - - - - 0x000D80 00:CD70: CA        DEX
C - - - - - 0x000D81 00:CD71: 10 F8     BPL bra_CD6B
- - - - - - 0x000D83 00:CD73: 60        RTS
bra_CD74:
C - - - - - 0x000D84 00:CD74: A9 00     LDA #$00
C - - - - - 0x000D86 00:CD76: 9D 5D 05  STA ram_055D_balloon_gfx,X
C - - - - - 0x000D89 00:CD79: 9D 99 05  STA ram_0599,X
C - - - - - 0x000D8C 00:CD7C: 9D 8F 05  STA ram_058F,X
C - - - - - 0x000D8F 00:CD7F: A9 80     LDA #$80
C - - - - - 0x000D91 00:CD81: 9D 71 05  STA ram_0571_balloon,X
C - - - - - 0x000D94 00:CD84: 9D 85 05  STA ram_0585,X
C - - - - - 0x000D97 00:CD87: A9 D0     LDA #$D0
C - - - - - 0x000D99 00:CD89: 9D 7B 05  STA ram_057B_balloon_y_pos,X
C - - - - - 0x000D9C 00:CD8C: 20 B3 F1  JSR sub_F1B3_RNG
C - - - - - 0x000D9F 00:CD8F: 29 03     AND #$03
C - - - - - 0x000DA1 00:CD91: A8        TAY
C - - - - - 0x000DA2 00:CD92: B9 AE CE  LDA tbl_CEAE_Balloon_X_Sprouts,Y                           ; X-coordinates for rising balloons in Bonus Phase
C - - - - - 0x000DA5 00:CD95: 9D 67 05  STA ram_0567_balloon_x_pos,X
C - - - - - 0x000DA8 00:CD98: A0 00     LDY #$00
C - - - - - 0x000DAA 00:CD9A: A5 1B     LDA ram_001B_rng_output_seed
C - - - - - 0x000DAC 00:CD9C: 9D B7 05  STA ram_05B7,X
C - - - - - 0x000DAF 00:CD9F: 10 01     BPL bra_CDA2
C - - - - - 0x000DB1 00:CDA1: 88        DEY
bra_CDA2:
C - - - - - 0x000DB2 00:CDA2: 98        TYA
C - - - - - 0x000DB3 00:CDA3: 9D C1 05  STA ram_05C1,X
C - - - - - 0x000DB6 00:CDA6: CE CB 05  DEC ram_05CB
C - - - - - 0x000DB9 00:CDA9: 60        RTS
sub_CDAA:
C - - - - - 0x000DBA 00:CDAA: A2 09     LDX #$09
loc_CDAC:
C D 2 - - - 0x000DBC 00:CDAC: BD 5D 05  LDA ram_055D_balloon_gfx,X
C - - - - - 0x000DBF 00:CDAF: 30 71     BMI bra_CE22
C - - - - - 0x000DC1 00:CDB1: F0 49     BEQ bra_CDFC
C - - - - - 0x000DC3 00:CDB3: BD 99 05  LDA ram_0599,X
C - - - - - 0x000DC6 00:CDB6: 85 12     STA ram_0012_temp
C - - - - - 0x000DC8 00:CDB8: BD 8F 05  LDA ram_058F,X
C - - - - - 0x000DCB 00:CDBB: 85 13     STA ram_0013_temp
C - - - - - 0x000DCD 00:CDBD: 20 A6 F1  JSR sub_F1A6
C - - - - - 0x000DD0 00:CDC0: BD B7 05  LDA ram_05B7,X
C - - - - - 0x000DD3 00:CDC3: 18        CLC
C - - - - - 0x000DD4 00:CDC4: 65 12     ADC ram_0012_temp
C - - - - - 0x000DD6 00:CDC6: 9D B7 05  STA ram_05B7,X
C - - - - - 0x000DD9 00:CDC9: 85 12     STA ram_0012_temp
C - - - - - 0x000DDB 00:CDCB: BD C1 05  LDA ram_05C1,X
C - - - - - 0x000DDE 00:CDCE: 65 13     ADC ram_0013_temp
C - - - - - 0x000DE0 00:CDD0: 9D C1 05  STA ram_05C1,X
C - - - - - 0x000DE3 00:CDD3: 85 13     STA ram_0013_temp
C - - - - - 0x000DE5 00:CDD5: 20 A6 F1  JSR sub_F1A6
C - - - - - 0x000DE8 00:CDD8: BD 99 05  LDA ram_0599,X
C - - - - - 0x000DEB 00:CDDB: 38        SEC
C - - - - - 0x000DEC 00:CDDC: E5 12     SBC ram_0012_temp
C - - - - - 0x000DEE 00:CDDE: 9D 99 05  STA ram_0599,X
C - - - - - 0x000DF1 00:CDE1: BD 8F 05  LDA ram_058F,X
C - - - - - 0x000DF4 00:CDE4: E5 13     SBC ram_0013_temp
C - - - - - 0x000DF6 00:CDE6: 9D 8F 05  STA ram_058F,X
C - - - - - 0x000DF9 00:CDE9: BD 71 05  LDA ram_0571_balloon,X
C - - - - - 0x000DFC 00:CDEC: 18        CLC
C - - - - - 0x000DFD 00:CDED: 7D 99 05  ADC ram_0599,X
C - - - - - 0x000E00 00:CDF0: 9D 71 05  STA ram_0571_balloon,X
C - - - - - 0x000E03 00:CDF3: BD 67 05  LDA ram_0567_balloon_x_pos,X
C - - - - - 0x000E06 00:CDF6: 7D 8F 05  ADC ram_058F,X
C - - - - - 0x000E09 00:CDF9: 9D 67 05  STA ram_0567_balloon_x_pos,X
bra_CDFC:
C - - - - - 0x000E0C 00:CDFC: BD 85 05  LDA ram_0585,X
C - - - - - 0x000E0F 00:CDFF: 38        SEC
C - - - - - 0x000E10 00:CE00: ED 5A 05  SBC ram_055A_balloon_rising_speed
C - - - - - 0x000E13 00:CE03: 9D 85 05  STA ram_0585,X
C - - - - - 0x000E16 00:CE06: B0 03     BCS bra_CE0B
C - - - - - 0x000E18 00:CE08: DE 7B 05  DEC ram_057B_balloon_y_pos,X
bra_CE0B:
C - - - - - 0x000E1B 00:CE0B: BD 7B 05  LDA ram_057B_balloon_y_pos,X
C - - - - - 0x000E1E 00:CE0E: C9 F0     CMP #$F0
C - - - - - 0x000E20 00:CE10: F0 0B     BEQ bra_CE1D
C - - - - - 0x000E22 00:CE12: C9 A8     CMP #$A8
C - - - - - 0x000E24 00:CE14: B0 0C     BCS bra_CE22
C - - - - - 0x000E26 00:CE16: A9 01     LDA #$01
C - - - - - 0x000E28 00:CE18: 9D 5D 05  STA ram_055D_balloon_gfx,X
C - - - - - 0x000E2B 00:CE1B: D0 05     BNE bra_CE22
bra_CE1D:
C - - - - - 0x000E2D 00:CE1D: A9 FF     LDA #$FF
C - - - - - 0x000E2F 00:CE1F: 9D 5D 05  STA ram_055D_balloon_gfx,X
bra_CE22:
C - - - - - 0x000E32 00:CE22: 20 2F CE  JSR sub_CE2F_Balloon_X_Sprite_Manage
C - - - - - 0x000E35 00:CE25: 20 CE CE  JSR sub_CECE_Balloon_Collision
C - - - - - 0x000E38 00:CE28: CA        DEX
C - - - - - 0x000E39 00:CE29: 30 03     BMI bra_CE2E_RTS
C - - - - - 0x000E3B 00:CE2B: 4C AC CD  JMP loc_CDAC
bra_CE2E_RTS:
C - - - - - 0x000E3E 00:CE2E: 60        RTS
sub_CE2F_Balloon_X_Sprite_Manage:
loc_CE2F_Balloon_X_Sprite_Manage:
C D 2 - - - 0x000E3F 00:CE2F: BC 5D 05  LDY ram_055D_balloon_gfx,X
C - - - - - 0x000E42 00:CE32: C8        INY
C - - - - - 0x000E43 00:CE33: B9 B2 CE  LDA tbl_CEB2,Y
C - - - - - 0x000E46 00:CE36: 85 13     STA ram_0013_temp
C - - - - - 0x000E48 00:CE38: 8A        TXA
C - - - - - 0x000E49 00:CE39: 85 12     STA ram_0012_temp
C - - - - - 0x000E4B 00:CE3B: 0A        ASL
C - - - - - 0x000E4C 00:CE3C: 65 12     ADC ram_0012_temp
C - - - - - 0x000E4E 00:CE3E: 0A        ASL
C - - - - - 0x000E4F 00:CE3F: 0A        ASL
C - - - - - 0x000E50 00:CE40: A8        TAY
C - - - - - 0x000E51 00:CE41: BD 7B 05  LDA ram_057B_balloon_y_pos,X
C - - - - - 0x000E54 00:CE44: 99 50 02  STA ram_0250_sprite_14_y,Y
C - - - - - 0x000E57 00:CE47: 99 54 02  STA ram_0254_sprite_15_y,Y
C - - - - - 0x000E5A 00:CE4A: 18        CLC
C - - - - - 0x000E5B 00:CE4B: 69 08     ADC #$08
C - - - - - 0x000E5D 00:CE4D: 99 58 02  STA ram_0258_sprite_16_y,Y
C - - - - - 0x000E60 00:CE50: BD 67 05  LDA ram_0567_balloon_x_pos,X
C - - - - - 0x000E63 00:CE53: 99 53 02  STA ram_0253_sprite_14_x,Y
C - - - - - 0x000E66 00:CE56: 18        CLC
C - - - - - 0x000E67 00:CE57: 69 04     ADC #$04
C - - - - - 0x000E69 00:CE59: 99 5B 02  STA ram_025B_sprite_16_x,Y
C - - - - - 0x000E6C 00:CE5C: 18        CLC
C - - - - - 0x000E6D 00:CE5D: 69 04     ADC #$04
C - - - - - 0x000E6F 00:CE5F: 99 57 02  STA ram_0257_sprite_15_x,Y
C - - - - - 0x000E72 00:CE62: A5 13     LDA ram_0013_temp
C - - - - - 0x000E74 00:CE64: 99 52 02  STA ram_0252_sprite_14_attributes,Y
C - - - - - 0x000E77 00:CE67: 99 56 02  STA ram_0256_sprite_15_attributes,Y
C - - - - - 0x000E7A 00:CE6A: 99 5A 02  STA ram_025A_sprite_16_attributes,Y
C - - - - - 0x000E7D 00:CE6D: BD 5D 05  LDA ram_055D_balloon_gfx,X
C - - - - - 0x000E80 00:CE70: 30 27     BMI bra_CE99
C - - - - - 0x000E82 00:CE72: A9 A8     LDA #$A8
C - - - - - 0x000E84 00:CE74: 99 51 02  STA ram_0251_sprite_14_tile,Y
C - - - - - 0x000E87 00:CE77: A9 A9     LDA #$A9
C - - - - - 0x000E89 00:CE79: 99 55 02  STA ram_0255_sprite_15_tile,Y
C - - - - - 0x000E8C 00:CE7C: A5 19     LDA ram_0019_f_counter
C - - - - - 0x000E8E 00:CE7E: 4A        LSR
C - - - - - 0x000E8F 00:CE7F: 4A        LSR
C - - - - - 0x000E90 00:CE80: 4A        LSR
C - - - - - 0x000E91 00:CE81: 4A        LSR
C - - - - - 0x000E92 00:CE82: 29 07     AND #$07
C - - - - - 0x000E94 00:CE84: 86 13     STX ram_0013_temp
C - - - - - 0x000E96 00:CE86: AA        TAX
C - - - - - 0x000E97 00:CE87: BD B5 CE  LDA tbl_CEB5,X
C - - - - - 0x000E9A 00:CE8A: 99 59 02  STA ram_0259_sprite_16_tile,Y
C - - - - - 0x000E9D 00:CE8D: B9 5A 02  LDA ram_025A_sprite_16_attributes,Y
C - - - - - 0x000EA0 00:CE90: 5D BD CE  EOR tbl_CEBD,X
C - - - - - 0x000EA3 00:CE93: 99 5A 02  STA ram_025A_sprite_16_attributes,Y
C - - - - - 0x000EA6 00:CE96: A6 13     LDX ram_0013_temp
C - - - - - 0x000EA8 00:CE98: 60        RTS
bra_CE99:
C - - - - - 0x000EA9 00:CE99: A9 F0     LDA #$F0
C - - - - - 0x000EAB 00:CE9B: 9D 7B 05  STA ram_057B_balloon_y_pos,X
C - - - - - 0x000EAE 00:CE9E: A9 AC     LDA #$AC
C - - - - - 0x000EB0 00:CEA0: 99 51 02  STA ram_0251_sprite_14_tile,Y
C - - - - - 0x000EB3 00:CEA3: A9 AD     LDA #$AD
C - - - - - 0x000EB5 00:CEA5: 99 55 02  STA ram_0255_sprite_15_tile,Y
C - - - - - 0x000EB8 00:CEA8: A9 FC     LDA #$FC
C - - - - - 0x000EBA 00:CEAA: 99 59 02  STA ram_0259_sprite_16_tile,Y
C - - - - - 0x000EBD 00:CEAD: 60        RTS
tbl_CEAE_Balloon_X_Sprouts:
- D 2 - - - 0x000EBE 00:CEAE: 20        .byte $20   ; 
- D 2 - - - 0x000EBF 00:CEAF: 50        .byte $50   ; 
- D 2 - - - 0x000EC0 00:CEB0: A0        .byte $A0   ; 
- D 2 - - - 0x000EC1 00:CEB1: D0        .byte $D0   ; 
tbl_CEB2:
- D 2 - - - 0x000EC2 00:CEB2: 02        .byte $02   ; 
- D 2 - - - 0x000EC3 00:CEB3: 22        .byte $22   ; 
- D 2 - - - 0x000EC4 00:CEB4: 02        .byte $02   ; 
tbl_CEB5:
- D 2 - - - 0x000EC5 00:CEB5: AA        .byte $AA   ; 
- D 2 - - - 0x000EC6 00:CEB6: AB        .byte $AB   ; 
- D 2 - - - 0x000EC7 00:CEB7: AB        .byte $AB   ; 
- D 2 - - - 0x000EC8 00:CEB8: AA        .byte $AA   ; 
- D 2 - - - 0x000EC9 00:CEB9: AA        .byte $AA   ; 
- D 2 - - - 0x000ECA 00:CEBA: AB        .byte $AB   ; 
- D 2 - - - 0x000ECB 00:CEBB: AB        .byte $AB   ; 
- D 2 - - - 0x000ECC 00:CEBC: AA        .byte $AA   ; 
tbl_CEBD:
- D 2 - - - 0x000ECD 00:CEBD: 00        .byte $00   ; 
- D 2 - - - 0x000ECE 00:CEBE: 00        .byte $00   ; 
- D 2 - - - 0x000ECF 00:CEBF: 40        .byte $40   ; 
- D 2 - - - 0x000ED0 00:CEC0: 40        .byte $40   ; 
- D 2 - - - 0x000ED1 00:CEC1: 40        .byte $40   ; 
- D 2 - - - 0x000ED2 00:CEC2: 40        .byte $40   ; 
- D 2 - - - 0x000ED3 00:CEC3: 00        .byte $00   ; 
- D 2 - - - 0x000ED4 00:CEC4: 00        .byte $00   ; 


; bzk garbage, see 0x000E92
- - - - - - 0x000ED5 00:CEC5: FC        .byte $FC   ; 
- - - - - - 0x000ED6 00:CEC6: FC        .byte $FC   ; 
- - - - - - 0x000ED7 00:CEC7: DF        .byte $DF   ; 
- - - - - - 0x000ED8 00:CEC8: FC        .byte $FC   ; 
- - - - - - 0x000ED9 00:CEC9: FC        .byte $FC   ; 
- - - - - - 0x000EDA 00:CECA: E0        .byte $E0   ; 
- - - - - - 0x000EDB 00:CECB: E2        .byte $E2   ; 
- - - - - - 0x000EDC 00:CECC: E1        .byte $E1   ; 
- - - - - - 0x000EDD 00:CECD: FC        .byte $FC   ; 



sub_CECE_Balloon_Collision:
C - - - - - 0x000EDE 00:CECE: A0 01     LDY #$01
bra_CED0:
C - - - - - 0x000EE0 00:CED0: B9 88 00  LDA ram_0088_object_balloons_p1,Y
C - - - - - 0x000EE3 00:CED3: 30 3A     BMI bra_CF0F
C - - - - - 0x000EE5 00:CED5: F0 38     BEQ bra_CF0F
C - - - - - 0x000EE7 00:CED7: BD 5D 05  LDA ram_055D_balloon_gfx,X
C - - - - - 0x000EEA 00:CEDA: 30 36     BMI bra_CF12_RTS
C - - - - - 0x000EEC 00:CEDC: B9 9A 00  LDA ram_009A_object_y_pos_int_p1,Y
C - - - - - 0x000EEF 00:CEDF: C9 C0     CMP #$C0
C - - - - - 0x000EF1 00:CEE1: B0 2C     BCS bra_CF0F
C - - - - - 0x000EF3 00:CEE3: 38        SEC
C - - - - - 0x000EF4 00:CEE4: FD 7B 05  SBC ram_057B_balloon_y_pos,X
C - - - - - 0x000EF7 00:CEE7: 20 8E F0  JSR sub_F08E_Absolute
C - - - - - 0x000EFA 00:CEEA: C9 18     CMP #$18
C - - - - - 0x000EFC 00:CEEC: B0 21     BCS bra_CF0F
C - - - - - 0x000EFE 00:CEEE: B9 91 00  LDA ram_0091_object_x_pos_int_p1,Y
C - - - - - 0x000F01 00:CEF1: 38        SEC
C - - - - - 0x000F02 00:CEF2: FD 67 05  SBC ram_0567_balloon_x_pos,X
C - - - - - 0x000F05 00:CEF5: 20 8E F0  JSR sub_F08E_Absolute
C - - - - - 0x000F08 00:CEF8: C9 10     CMP #$10
C - - - - - 0x000F0A 00:CEFA: B0 13     BCS bra_CF0F
C - - - - - 0x000F0C 00:CEFC: A9 FF     LDA #$FF
C - - - - - 0x000F0E 00:CEFE: 9D 5D 05  STA ram_055D_balloon_gfx,X
C - - - - - 0x000F11 00:CF01: B9 CD 05  LDA ram_05CD_touched_balloons_counter,Y
C - - - - - 0x000F14 00:CF04: 18        CLC
C - - - - - 0x000F15 00:CF05: 69 01     ADC #$01
C - - - - - 0x000F17 00:CF07: 99 CD 05  STA ram_05CD_touched_balloons_counter,Y
C - - - - - 0x000F1A 00:CF0A: A9 02     LDA #$02                                                   ; \ Play Balloon Pop SFX
C - - - - - 0x000F1C 00:CF0C: 85 F0     STA ram_00F0_sfx_1                                         ; /
C - - - - - 0x000F1E 00:CF0E: 60        RTS
bra_CF0F:
C - - - - - 0x000F1F 00:CF0F: 88        DEY
C - - - - - 0x000F20 00:CF10: 10 BE     BPL bra_CED0
bra_CF12_RTS:
C - - - - - 0x000F22 00:CF12: 60        RTS

; Bonus Phase code.

loc_CF13:
C D 2 - - - 0x000F23 00:CF13: A9 20     LDA #$20
C - - - - - 0x000F25 00:CF15: 85 F2     STA ram_00F2_music_jingle                                  ; Play Bonus Phase music
C - - - - - 0x000F27 00:CF17: 20 E2 D0  JSR sub_D0E2_Set_Bonus_Phase
C - - - - - 0x000F2A 00:CF1A: 20 4A CD  JSR sub_CD4A_Init_Balloons
C - - - - - 0x000F2D 00:CF1D: A6 40     LDX ram_0040_2p_flag
bra_CF1F:
C - - - - - 0x000F2F 00:CF1F: B5 41     LDA ram_0041_p1_lives,X
C - - - - - 0x000F31 00:CF21: 30 03     BMI bra_CF26
C - - - - - 0x000F33 00:CF23: 20 B0 F3  JSR sub_F3B0_Init_Player_Type
bra_CF26:
C - - - - - 0x000F36 00:CF26: CA        DEX
C - - - - - 0x000F37 00:CF27: 10 F6     BPL bra_CF1F
C - - - - - 0x000F39 00:CF29: A2 00     LDX #$00
C - - - - - 0x000F3B 00:CF2B: 86 BD     STX ram_00BD_p1_invincibility_flag                         ; No invincibility
C - - - - - 0x000F3D 00:CF2D: 86 BE     STX ram_00BE_p2_invincibility_flag                         ; for either player
C - - - - - 0x000F3F 00:CF2F: A9 14     LDA #$14
C - - - - - 0x000F41 00:CF31: 8D CB 05  STA ram_05CB                                               ; 20 balloons for Bonus Phase
bra_CF34:
C - - - - - 0x000F44 00:CF34: 20 70 F4  JSR sub_F470_Pause
C - - - - - 0x000F47 00:CF37: E6 4C     INC ram_004C_star_update
C - - - - - 0x000F49 00:CF39: 20 DD D8  JSR sub_D8DD
C - - - - - 0x000F4C 00:CF3C: 20 91 E6  JSR sub_E691_Object_Manage
C - - - - - 0x000F4F 00:CF3F: AD CB 05  LDA ram_05CB
C - - - - - 0x000F52 00:CF42: F0 03     BEQ bra_CF47
C - - - - - 0x000F54 00:CF44: 20 5A CD  JSR sub_CD5A
bra_CF47:
C - - - - - 0x000F57 00:CF47: 20 AA CD  JSR sub_CDAA
C - - - - - 0x000F5A 00:CF4A: AD CB 05  LDA ram_05CB
C - - - - - 0x000F5D 00:CF4D: D0 E5     BNE bra_CF34
C - - - - - 0x000F5F 00:CF4F: A2 09     LDX #$09
bra_CF51:
C - - - - - 0x000F61 00:CF51: BD 5D 05  LDA ram_055D_balloon_gfx,X
C - - - - - 0x000F64 00:CF54: 10 DE     BPL bra_CF34
C - - - - - 0x000F66 00:CF56: CA        DEX
C - - - - - 0x000F67 00:CF57: 10 F8     BPL bra_CF51
C - - - - - 0x000F69 00:CF59: A5 19     LDA ram_0019_f_counter
C - - - - - 0x000F6B 00:CF5B: D0 D7     BNE bra_CF34
C - - - - - 0x000F6D 00:CF5D: 20 46 D2  JSR sub_D246_Clear_PPU
C - - - - - 0x000F70 00:CF60: A2 02     LDX #$02
C - - - - - 0x000F72 00:CF62: 86 46     STX ram_0046_status_bar_update_flag
C - - - - - 0x000F74 00:CF64: 20 5E F4  JSR sub_F45E_Wait_Y_Ticks
C - - - - - 0x000F77 00:CF67: A9 2B     LDA #< tbl_D12B
C - - - - - 0x000F79 00:CF69: A0 D1     LDY #> tbl_D12B
C - - - - - 0x000F7B 00:CF6B: 20 31 C1  JSR sub_C131_Copy_PPU_Block
C - - - - - 0x000F7E 00:CF6E: A9 5A     LDA #< tbl_D15A
C - - - - - 0x000F80 00:CF70: A0 D1     LDY #> tbl_D15A
C - - - - - 0x000F82 00:CF72: 20 31 C1  JSR sub_C131_Copy_PPU_Block
C - - - - - 0x000F85 00:CF75: A9 65     LDA #< tbl_D165
C - - - - - 0x000F87 00:CF77: A0 D1     LDY #> tbl_D165
C - - - - - 0x000F89 00:CF79: 20 31 C1  JSR sub_C131_Copy_PPU_Block
C - - - - - 0x000F8C 00:CF7C: A6 40     LDX ram_0040_2p_flag
bra_CF7E:
C - - - - - 0x000F8E 00:CF7E: A9 20     LDA #$20
C - - - - - 0x000F90 00:CF80: 95 91     STA ram_0091_object_x_pos_int_p1,X
C - - - - - 0x000F92 00:CF82: BD 9E D1  LDA tbl_D19E,X
C - - - - - 0x000F95 00:CF85: 95 9A     STA ram_009A_object_y_pos_int_p1,X
C - - - - - 0x000F97 00:CF87: A9 03     LDA #$03
C - - - - - 0x000F99 00:CF89: 95 7F     STA ram_007F_object_status_p1,X
C - - - - - 0x000F9B 00:CF8B: A9 01     LDA #$01
C - - - - - 0x000F9D 00:CF8D: 9D 48 04  STA ram_0448_direction_p1,X
C - - - - - 0x000FA0 00:CF90: 20 B0 F3  JSR sub_F3B0_Init_Player_Type
C - - - - - 0x000FA3 00:CF93: 20 A4 E3  JSR sub_E3A4
C - - - - - 0x000FA6 00:CF96: CA        DEX
C - - - - - 0x000FA7 00:CF97: 10 E5     BPL bra_CF7E
C - - - - - 0x000FA9 00:CF99: A9 44     LDA #$44
C - - - - - 0x000FAB 00:CF9B: 8D 67 05  STA ram_0567_balloon_x_pos
C - - - - - 0x000FAE 00:CF9E: 8D 68 05  STA ram_0568
C - - - - - 0x000FB1 00:CFA1: A9 54     LDA #$54
C - - - - - 0x000FB3 00:CFA3: 8D 7B 05  STA ram_057B_balloon_y_pos
C - - - - - 0x000FB6 00:CFA6: A9 74     LDA #$74
C - - - - - 0x000FB8 00:CFA8: 8D 7C 05  STA ram_057C
C - - - - - 0x000FBB 00:CFAB: A9 01     LDA #$01
C - - - - - 0x000FBD 00:CFAD: 8D 5D 05  STA ram_055D_balloon_gfx
C - - - - - 0x000FC0 00:CFB0: 8D 5E 05  STA ram_055E
C - - - - - 0x000FC3 00:CFB3: A6 40     LDX ram_0040_2p_flag
bra_CFB5:
C - - - - - 0x000FC5 00:CFB5: 20 2F CE  JSR sub_CE2F_Balloon_X_Sprite_Manage
C - - - - - 0x000FC8 00:CFB8: CA        DEX
C - - - - - 0x000FC9 00:CFB9: 10 FA     BPL bra_CFB5
C - - - - - 0x000FCB 00:CFBB: 20 5C F4  JSR sub_F45C_Wait_20_Ticks
C - - - - - 0x000FCE 00:CFBE: A9 2B     LDA #$2B
C - - - - - 0x000FD0 00:CFC0: 85 57     STA ram_0057_ppu_buffer_upload_data
C - - - - - 0x000FD2 00:CFC2: A9 24     LDA #$24
C - - - - - 0x000FD4 00:CFC4: 85 58     STA ram_0058
C - - - - - 0x000FD6 00:CFC6: 85 59     STA ram_0059
C - - - - - 0x000FD8 00:CFC8: A9 0C     LDA #$0C
C - - - - - 0x000FDA 00:CFCA: 85 54     STA ram_0054_temp_cloud_flip_x
C - - - - - 0x000FDC 00:CFCC: A9 0B     LDA #$0B
C - - - - - 0x000FDE 00:CFCE: 85 55     STA ram_0055_temp_cloud_flip_y
C - - - - - 0x000FE0 00:CFD0: A9 05     LDA #$05
C - - - - - 0x000FE2 00:CFD2: 85 56     STA ram_0056_size_of_upload_to_ppu_buffer
C - - - - - 0x000FE4 00:CFD4: AD CD 05  LDA ram_05CD_touched_balloons_counter
C - - - - - 0x000FE7 00:CFD7: 20 C9 D1  JSR sub_D1C9
C - - - - - 0x000FEA 00:CFDA: A5 40     LDA ram_0040_2p_flag
C - - - - - 0x000FEC 00:CFDC: F0 0A     BEQ bra_CFE8
C - - - - - 0x000FEE 00:CFDE: A9 0F     LDA #$0F
C - - - - - 0x000FF0 00:CFE0: 85 55     STA ram_0055_temp_cloud_flip_y
C - - - - - 0x000FF2 00:CFE2: AD CE 05  LDA ram_05CE_trip_balloons_counter
C - - - - - 0x000FF5 00:CFE5: 20 C9 D1  JSR sub_D1C9
bra_CFE8:
C - - - - - 0x000FF8 00:CFE8: 20 5C F4  JSR sub_F45C_Wait_20_Ticks
C - - - - - 0x000FFB 00:CFEB: AD 59 05  LDA ram_0559_bonus_trip_ball_pts
C - - - - - 0x000FFE 00:CFEE: 85 57     STA ram_0057_ppu_buffer_upload_data
C - - - - - 0x001000 00:CFF0: A9 00     LDA #$00
C - - - - - 0x001002 00:CFF2: 85 58     STA ram_0058
C - - - - - 0x001004 00:CFF4: 85 59     STA ram_0059
C - - - - - 0x001006 00:CFF6: A9 08     LDA #$08
C - - - - - 0x001008 00:CFF8: 85 54     STA ram_0054_temp_cloud_flip_x
C - - - - - 0x00100A 00:CFFA: A9 0B     LDA #$0B
C - - - - - 0x00100C 00:CFFC: 85 55     STA ram_0055_temp_cloud_flip_y
C - - - - - 0x00100E 00:CFFE: A9 03     LDA #$03
C - - - - - 0x001010 00:D000: 85 56     STA ram_0056_size_of_upload_to_ppu_buffer
C - - - - - 0x001012 00:D002: AD 59 05  LDA ram_0559_bonus_trip_ball_pts
C - - - - - 0x001015 00:D005: 20 19 C1  JSR sub_C119
C - - - - - 0x001018 00:D008: A5 40     LDA ram_0040_2p_flag
C - - - - - 0x00101A 00:D00A: F0 07     BEQ bra_D013
C - - - - - 0x00101C 00:D00C: A9 0F     LDA #$0F
C - - - - - 0x00101E 00:D00E: 85 55     STA ram_0055_temp_cloud_flip_y
C - - - - - 0x001020 00:D010: 20 19 C1  JSR sub_C119
bra_D013:
C - - - - - 0x001023 00:D013: A9 FF     LDA #$FF
C - - - - - 0x001025 00:D015: 8D 5D 05  STA ram_055D_balloon_gfx
C - - - - - 0x001028 00:D018: 8D 5E 05  STA ram_055E
C - - - - - 0x00102B 00:D01B: A6 40     LDX ram_0040_2p_flag
bra_D01D:
C - - - - - 0x00102D 00:D01D: 20 2F CE  JSR sub_CE2F_Balloon_X_Sprite_Manage
C - - - - - 0x001030 00:D020: CA        DEX
C - - - - - 0x001031 00:D021: 10 FA     BPL bra_D01D
C - - - - - 0x001033 00:D023: A9 02     LDA #$02                                                   ; \ Play Balloon Pop SFX
C - - - - - 0x001035 00:D025: 85 F0     STA ram_00F0_sfx_1                                         ; /
C - - - - - 0x001037 00:D027: A2 02     LDX #$02
C - - - - - 0x001039 00:D029: 20 5E F4  JSR sub_F45E_Wait_Y_Ticks
C - - - - - 0x00103C 00:D02C: A6 40     LDX ram_0040_2p_flag
bra_D02E:
C - - - - - 0x00103E 00:D02E: 20 2F CE  JSR sub_CE2F_Balloon_X_Sprite_Manage
C - - - - - 0x001041 00:D031: CA        DEX
C - - - - - 0x001042 00:D032: 10 FA     BPL bra_D02E
C - - - - - 0x001044 00:D034: 20 A0 D1  JSR sub_D1A0
C - - - - - 0x001047 00:D037: 20 5C F4  JSR sub_F45C_Wait_20_Ticks
C - - - - - 0x00104A 00:D03A: A9 01     LDA #$01                                                   ; \ Stop All Sounds
C - - - - - 0x00104C 00:D03C: 85 F0     STA ram_00F0_sfx_1                                         ; /
C - - - - - 0x00104E 00:D03E: 20 21 D1  JSR sub_D121_Balloons_Collected_Check
C - - - - - 0x001051 00:D041: D0 25     BNE bra_D068
C - - - - - 0x001053 00:D043: A9 70     LDA #< tbl_D170
C - - - - - 0x001055 00:D045: A0 D1     LDY #> tbl_D170
C - - - - - 0x001057 00:D047: 20 31 C1  JSR sub_C131_Copy_PPU_Block
C - - - - - 0x00105A 00:D04A: 20 65 F4  JSR sub_F465_Clear_F_Flag                                  ; Clear Frame Flag
C - - - - - 0x00105D 00:D04D: A2 1A     LDX #$1A
bra_D04F:
C - - - - - 0x00105F 00:D04F: BD 84 D1  LDA tbl_D184,X
C - - - - - 0x001062 00:D052: 95 57     STA ram_0057_ppu_buffer_upload_data,X
C - - - - - 0x001064 00:D054: CA        DEX
C - - - - - 0x001065 00:D055: 10 F8     BPL bra_D04F
C - - - - - 0x001067 00:D057: AD 5B 05  LDA ram_055B_super_bonus_x0000
C - - - - - 0x00106A 00:D05A: 85 68     STA ram_0068
C - - - - - 0x00106C 00:D05C: AD 5C 05  LDA ram_055C_super_bonus_0x000
C - - - - - 0x00106F 00:D05F: 85 69     STA ram_0069
C - - - - - 0x001071 00:D061: 20 2D C1  JSR sub_C12D_Copy_PPU_Temp_Block
C - - - - - 0x001074 00:D064: A9 10     LDA #$10                                                   ; Play Bonus Phase Perfect Jingle
C - - - - - 0x001076 00:D066: 85 F2     STA ram_00F2_music_jingle
bra_D068:
C - - - - - 0x001078 00:D068: A2 78     LDX #$78
C - - - - - 0x00107A 00:D06A: 20 5E F4  JSR sub_F45E_Wait_Y_Ticks
C - - - - - 0x00107D 00:D06D: 20 A0 D1  JSR sub_D1A0
bra_D070:
C - - - - - 0x001080 00:D070: A9 00     LDA #$00
C - - - - - 0x001082 00:D072: 85 3E     STA ram_003E_score_id_update
C - - - - - 0x001084 00:D074: A2 04     LDX #$04
C - - - - - 0x001086 00:D076: 20 13 D2  JSR sub_D213
C - - - - - 0x001089 00:D079: 20 2D C1  JSR sub_C12D_Copy_PPU_Temp_Block
C - - - - - 0x00108C 00:D07C: A5 40     LDA ram_0040_2p_flag
C - - - - - 0x00108E 00:D07E: F0 0E     BEQ bra_D08E
C - - - - - 0x001090 00:D080: E6 3E     INC ram_003E_score_id_update
C - - - - - 0x001092 00:D082: A2 12     LDX #$12
C - - - - - 0x001094 00:D084: 20 13 D2  JSR sub_D213
C - - - - - 0x001097 00:D087: A9 65     LDA #< ram_0065
C - - - - - 0x001099 00:D089: A0 00     LDY #> ram_0065
C - - - - - 0x00109B 00:D08B: 20 31 C1  JSR sub_C131_Copy_PPU_Block
bra_D08E:
C - - - - - 0x00109E 00:D08E: A9 01     LDA #$01
C - - - - - 0x0010A0 00:D090: 85 F1     STA ram_00F1_sfx_2
C - - - - - 0x0010A2 00:D092: A2 02     LDX #$02
C - - - - - 0x0010A4 00:D094: 20 5E F4  JSR sub_F45E_Wait_Y_Ticks
C - - - - - 0x0010A7 00:D097: A5 5D     LDA ram_005D
C - - - - - 0x0010A9 00:D099: C9 24     CMP #$24
C - - - - - 0x0010AB 00:D09B: D0 D3     BNE bra_D070
C - - - - - 0x0010AD 00:D09D: A5 40     LDA ram_0040_2p_flag
C - - - - - 0x0010AF 00:D09F: F0 07     BEQ bra_D0A8
C - - - - - 0x0010B1 00:D0A1: AD 6B 00  LDA a: ram_006B
C - - - - - 0x0010B4 00:D0A4: C9 24     CMP #$24
C - - - - - 0x0010B6 00:D0A6: D0 C8     BNE bra_D070
bra_D0A8:
C - - - - - 0x0010B8 00:D0A8: A2 0A     LDX #$0A
C - - - - - 0x0010BA 00:D0AA: 20 5E F4  JSR sub_F45E_Wait_Y_Ticks
C - - - - - 0x0010BD 00:D0AD: 20 21 D1  JSR sub_D121_Balloons_Collected_Check
C - - - - - 0x0010C0 00:D0B0: D0 1C     BNE bra_D0CE
C - - - - - 0x0010C2 00:D0B2: AD 5B 05  LDA ram_055B_super_bonus_x0000
C - - - - - 0x0010C5 00:D0B5: 85 47     STA ram_0047_4th_digit_score_add
C - - - - - 0x0010C7 00:D0B7: AD 5C 05  LDA ram_055C_super_bonus_0x000
C - - - - - 0x0010CA 00:D0BA: 85 48     STA ram_0048_5th_digit_score_add
C - - - - - 0x0010CC 00:D0BC: A5 40     LDA ram_0040_2p_flag
C - - - - - 0x0010CE 00:D0BE: 85 3E     STA ram_003E_score_id_update
bra_D0C0:
C - - - - - 0x0010D0 00:D0C0: 20 DC D6  JSR sub_D6DC_Score_Update
C - - - - - 0x0010D3 00:D0C3: C6 3E     DEC ram_003E_score_id_update
C - - - - - 0x0010D5 00:D0C5: 10 F9     BPL bra_D0C0
C - - - - - 0x0010D7 00:D0C7: A9 01     LDA #$01
C - - - - - 0x0010D9 00:D0C9: 85 F1     STA ram_00F1_sfx_2
C - - - - - 0x0010DB 00:D0CB: 20 5C F4  JSR sub_F45C_Wait_20_Ticks
bra_D0CE:
C - - - - - 0x0010DE 00:D0CE: A9 00     LDA #$00
C - - - - - 0x0010E0 00:D0D0: 85 47     STA ram_0047_4th_digit_score_add
C - - - - - 0x0010E2 00:D0D2: 85 48     STA ram_0048_5th_digit_score_add
C - - - - - 0x0010E4 00:D0D4: A2 01     LDX #$01
bra_D0D6:
C - - - - - 0x0010E6 00:D0D6: B5 41     LDA ram_0041_p1_lives,X
C - - - - - 0x0010E8 00:D0D8: 10 02     BPL bra_D0DC
C - - - - - 0x0010EA 00:D0DA: 95 88     STA ram_0088_object_balloons_p1,X
bra_D0DC:
C - - - - - 0x0010EC 00:D0DC: CA        DEX
C - - - - - 0x0010ED 00:D0DD: 10 F7     BPL bra_D0D6
C - - - - - 0x0010EF 00:D0DF: 4C 53 F3  JMP loc_F353
sub_D0E2_Set_Bonus_Phase:
C - - - - - 0x0010F2 00:D0E2: AE 58 05  LDX ram_0558_bonus_phase_intensity_level                   ; Set up Bonus Phase according to Intensity (max 4)
C - - - - - 0x0010F5 00:D0E5: BD 0D D1  LDA tbl_D10D_Points_Per_Balloon,X
C - - - - - 0x0010F8 00:D0E8: 8D 59 05  STA ram_0559_bonus_trip_ball_pts                           ; Set points per balloon
C - - - - - 0x0010FB 00:D0EB: BD 12 D1  LDA tbl_D112_Balloon_Rising_Speed,X
C - - - - - 0x0010FE 00:D0EE: 8D 5A 05  STA ram_055A_balloon_rising_speed                          ; Set rising speed
C - - - - - 0x001101 00:D0F1: BD 17 D1  LDA tbl_D117_Super_Bonus_X0000_Pts,X                       ; Set Super Bonus Points
C - - - - - 0x001104 00:D0F4: 8D 5B 05  STA ram_055B_super_bonus_x0000
C - - - - - 0x001107 00:D0F7: BD 1C D1  LDA tbl_D11C_Super_Bonus_0X000_Pts,X
C - - - - - 0x00110A 00:D0FA: 8D 5C 05  STA ram_055C_super_bonus_0x000
C - - - - - 0x00110D 00:D0FD: E0 04     CPX #$04                                                   ; Increment Bonus Phase Intensity
C - - - - - 0x00110F 00:D0FF: B0 03     BCS bra_D104                                               ; until maximum (4)
C - - - - - 0x001111 00:D101: EE 58 05  INC ram_0558_bonus_phase_intensity_level
bra_D104:
C - - - - - 0x001114 00:D104: A9 00     LDA #$00
C - - - - - 0x001116 00:D106: 8D CD 05  STA ram_05CD_touched_balloons_counter                      ; Initialize Balloon Counters
C - - - - - 0x001119 00:D109: 8D CE 05  STA ram_05CE_trip_balloons_counter
C - - - - - 0x00111C 00:D10C: 60        RTS
tbl_D10D_Points_Per_Balloon:
- D 2 - - - 0x00111D 00:D10D: 03        .byte $03                                                  ; Intensity = 0
- D 2 - - - 0x00111E 00:D10E: 05        .byte $05                                                  ; Intensity = 1
- D 2 - - - 0x00111F 00:D10F: 07        .byte $07                                                  ; Intensity = 2
- D 2 - - - 0x001120 00:D110: 07        .byte $07                                                  ; Intensity = 3
- D 2 - - - 0x001121 00:D111: 07        .byte $07                                                  ; Intensity = 4
tbl_D112_Balloon_Rising_Speed:
- D 2 - - - 0x001122 00:D112: 80        .byte $80                                                  ; Intensity = 0
- D 2 - - - 0x001123 00:D113: 90        .byte $90                                                  ; Intensity = 1
- D 2 - - - 0x001124 00:D114: 98        .byte $98                                                  ; Intensity = 2
- D 2 - - - 0x001125 00:D115: A0        .byte $A0                                                  ; Intensity = 3
- D 2 - - - 0x001126 00:D116: A8        .byte $A8                                                  ; Intensity = 4
tbl_D117_Super_Bonus_X0000_Pts:
- D 2 - - - 0x001127 00:D117: 01        .byte $01                                                  ; Intensity = 0; 10000 Super Bonus Points
- D 2 - - - 0x001128 00:D118: 01        .byte $01                                                  ; Intensity = 1; 15000 Super Bonus Points
- D 2 - - - 0x001129 00:D119: 02        .byte $02                                                  ; Intensity = 2; 20000 Super Bonus Points
- D 2 - - - 0x00112A 00:D11A: 02        .byte $02                                                  ; Intensity = 3; 25000 Super Bonus Points
- D 2 - - - 0x00112B 00:D11B: 03        .byte $03                                                  ; Intensity = 4; 30000 Super Bonus Points
tbl_D11C_Super_Bonus_0X000_Pts:
- D 2 - - - 0x00112C 00:D11C: 00        .byte $00                                                  ; Intensity = 0; 10000 Super Bonus Points
- D 2 - - - 0x00112D 00:D11D: 05        .byte $05                                                  ; Intensity = 1; 15000 Super Bonus Points
- D 2 - - - 0x00112E 00:D11E: 00        .byte $00                                                  ; Intensity = 2; 20000 Super Bonus Points
- D 2 - - - 0x00112F 00:D11F: 05        .byte $05                                                  ; Intensity = 3; 25000 Super Bonus Points
- D 2 - - - 0x001130 00:D120: 00        .byte $00                                                  ; Intensity = 4; 30000 Super Bonus Points
sub_D121_Balloons_Collected_Check:
C - - - - - 0x001131 00:D121: AD CD 05  LDA ram_05CD_touched_balloons_counter
C - - - - - 0x001134 00:D124: 18        CLC
C - - - - - 0x001135 00:D125: 6D CE 05  ADC ram_05CE_trip_balloons_counter
C - - - - - 0x001138 00:D128: C9 14     CMP #$14
C - - - - - 0x00113A 00:D12A: 60        RTS
tbl_D12B:
- D 2 - I - 0x00113B 00:D12B: 3F 00     .dbyt $3F00                                                ; Palette at PPU Address $3F00
- D 2 - I - 0x00113D 00:D12D: 10        .byte $10                                                  ; 16 bytes
- D 2 - I - 0x00113E 00:D12E: 0F        .byte $0F, $30, $30, $30, $0F, $30, $27, $15, $0F, $30, $02, $21, $0F, $16, $16, $16
tbl_D13E:
- D 2 - - - 0x00114E 00:D13E: 21 73     .dbyt $2173                                                ; PPU Address $2173
- D 2 - - - 0x001150 00:D140: 0B        .byte $0B                                                  ; 11 bytes
- D 2 - - - 0x001151 00:D141: 29        .byte $29, $00, $00, $00, $00, $00, $24, $19, $1D, $1C, $26
- D 2 - - - 0x00115C 00:D14C: 21 F3     .dbyt $21F3                                                ; PPU Address $21F3
- D 2 - - - 0x00115E 00:D14E: 0B        .byte $0B                                                  ; 11 bytes
- D 2 - - - 0x00115F 00:D14F: 29        .byte $29, $00, $00, $00, $00, $00, $24, $19, $1D, $1C, $26
tbl_D15A:
- D 2 - I - 0x00116A 00:D15A: 23 E8     .dbyt $23E8                                                ; PPU Address $23E8
- D 2 - I - 0x00116C 00:D15C: 08        .byte $08                                                  ; 8 bytes
- D 2 - I - 0x00116D 00:D15D: FF        .byte $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF
tbl_D165:
- D 2 - I - 0x001175 00:D165: 23 C0     .dbyt $23C0                                                ; PPU Address $23C0
- D 2 - I - 0x001177 00:D167: 08        .byte $08                                                  ; 8 bytes
- D 2 - I - 0x001178 00:D168: 40        .byte $40, $50, $50, $50, $50, $90, $A0, $A0
tbl_D170:
- D 2 - I - 0x001180 00:D170: 22 88     .dbyt $2288                                                ; PPU Address $2288
- D 2 - I - 0x001182 00:D172: 11        .byte $11                                                  ; 17 bytes
- D 2 - I - 0x001183 00:D173: 19        .byte $19, $24, $0E, $24, $1B, $24, $0F, $24, $0E, $24, $0C, $24, $1D, $24, $2C, $2C, $2C
tbl_D184:
- D 2 - - - 0x001194 00:D184: 22 C6     .dbyt $22C6                                                ; PPU Address $22C6
- D 2 - - - 0x001196 00:D186: 17        .byte $17                                                  ; 23 bytes
- D 2 - - - 0x001197 00:D187: 1C        .byte $1C, $1E, $19, $0E, $1B, $24, $0B, $18, $17
- D 2 - - - 0x0011A0 00:D190: 1E        .byte $1E, $1C, $24, $24, $24, $01, $00, $00, $00, $00, $19, $1D, $1C, $2C
tbl_D19E:
- D 2 - - - 0x0011AE 00:D19E: 50        .byte $50   ; 
- D 2 - - - 0x0011AF 00:D19F: 70        .byte $70   ; 
sub_D1A0:
C - - - - - 0x0011B0 00:D1A0: A2 1C     LDX #$1C
bra_D1A2:
C - - - - - 0x0011B2 00:D1A2: BD 3E D1  LDA tbl_D13E,X
C - - - - - 0x0011B5 00:D1A5: 95 57     STA ram_0057_ppu_buffer_upload_data,X
C - - - - - 0x0011B7 00:D1A7: CA        DEX
C - - - - - 0x0011B8 00:D1A8: 10 F8     BPL bra_D1A2
C - - - - - 0x0011BA 00:D1AA: A2 04     LDX #$04
C - - - - - 0x0011BC 00:D1AC: AC CD 05  LDY ram_05CD_touched_balloons_counter
C - - - - - 0x0011BF 00:D1AF: 20 DC D1  JSR sub_D1DC
C - - - - - 0x0011C2 00:D1B2: A2 12     LDX #$12
C - - - - - 0x0011C4 00:D1B4: AC CE 05  LDY ram_05CE_trip_balloons_counter
C - - - - - 0x0011C7 00:D1B7: 20 DC D1  JSR sub_D1DC
C - - - - - 0x0011CA 00:D1BA: 20 2D C1  JSR sub_C12D_Copy_PPU_Temp_Block
C - - - - - 0x0011CD 00:D1BD: A5 40     LDA ram_0040_2p_flag
C - - - - - 0x0011CF 00:D1BF: D0 01     BNE bra_D1C2
C - - - - - 0x0011D1 00:D1C1: 60        RTS
bra_D1C2:
C - - - - - 0x0011D2 00:D1C2: A9 65     LDA #< ram_0065
C - - - - - 0x0011D4 00:D1C4: A0 00     LDY #> ram_0065
C - - - - - 0x0011D6 00:D1C6: 4C 31 C1  JMP sub_C131_Copy_PPU_Block
sub_D1C9:
C - - - - - 0x0011D9 00:D1C9: A0 00     LDY #$00
loc_D1CB:
C D 2 - - - 0x0011DB 00:D1CB: C9 0A     CMP #$0A
C - - - - - 0x0011DD 00:D1CD: 90 06     BCC bra_D1D5
C - - - - - 0x0011DF 00:D1CF: C8        INY
C - - - - - 0x0011E0 00:D1D0: E9 0A     SBC #$0A
C - - - - - 0x0011E2 00:D1D2: 4C CB D1  JMP loc_D1CB
bra_D1D5:
C - - - - - 0x0011E5 00:D1D5: 84 5A     STY ram_005A
C - - - - - 0x0011E7 00:D1D7: 85 5B     STA ram_005B
C - - - - - 0x0011E9 00:D1D9: 4C 19 C1  JMP loc_C119
sub_D1DC:
loc_D1DC:
C D 2 - - - 0x0011EC 00:D1DC: 88        DEY
C - - - - - 0x0011ED 00:D1DD: 30 1F     BMI bra_D1FE
C - - - - - 0x0011EF 00:D1DF: AD 59 05  LDA ram_0559_bonus_trip_ball_pts
C - - - - - 0x0011F2 00:D1E2: 18        CLC
C - - - - - 0x0011F3 00:D1E3: 75 59     ADC ram_0059,X
C - - - - - 0x0011F5 00:D1E5: C9 0A     CMP #$0A
C - - - - - 0x0011F7 00:D1E7: 90 04     BCC bra_D1ED
C - - - - - 0x0011F9 00:D1E9: E9 0A     SBC #$0A
C - - - - - 0x0011FB 00:D1EB: F6 58     INC ram_0058,X
bra_D1ED:
C - - - - - 0x0011FD 00:D1ED: 95 59     STA ram_0059,X
C - - - - - 0x0011FF 00:D1EF: B5 58     LDA ram_0058,X
C - - - - - 0x001201 00:D1F1: C9 0A     CMP #$0A
C - - - - - 0x001203 00:D1F3: 90 06     BCC bra_D1FB
C - - - - - 0x001205 00:D1F5: E9 0A     SBC #$0A
C - - - - - 0x001207 00:D1F7: F6 57     INC ram_0057_ppu_buffer_upload_data,X
C - - - - - 0x001209 00:D1F9: 95 58     STA ram_0058,X
bra_D1FB:
C - - - - - 0x00120B 00:D1FB: 4C DC D1  JMP loc_D1DC
bra_D1FE:
loc_D1FE:
C D 2 - - - 0x00120E 00:D1FE: A0 00     LDY #$00
bra_D200:
C - - - - - 0x001210 00:D200: B5 57     LDA ram_0057_ppu_buffer_upload_data,X
C - - - - - 0x001212 00:D202: F0 04     BEQ bra_D208
C - - - - - 0x001214 00:D204: C9 24     CMP #$24
C - - - - - 0x001216 00:D206: D0 0A     BNE bra_D212_RTS
bra_D208:
C - - - - - 0x001218 00:D208: A9 24     LDA #$24
C - - - - - 0x00121A 00:D20A: 95 57     STA ram_0057_ppu_buffer_upload_data,X
C - - - - - 0x00121C 00:D20C: E8        INX
C - - - - - 0x00121D 00:D20D: C8        INY
C - - - - - 0x00121E 00:D20E: C0 04     CPY #$04
C - - - - - 0x001220 00:D210: D0 EE     BNE bra_D200
bra_D212_RTS:
C - - - - - 0x001222 00:D212: 60        RTS
sub_D213:
C - - - - - 0x001223 00:D213: B5 59     LDA ram_0059,X
C - - - - - 0x001225 00:D215: C9 24     CMP #$24
C - - - - - 0x001227 00:D217: F0 2A     BEQ bra_D243
C - - - - - 0x001229 00:D219: A8        TAY
C - - - - - 0x00122A 00:D21A: D0 1C     BNE bra_D238
C - - - - - 0x00122C 00:D21C: B5 58     LDA ram_0058,X
C - - - - - 0x00122E 00:D21E: C9 24     CMP #$24
C - - - - - 0x001230 00:D220: F0 21     BEQ bra_D243
C - - - - - 0x001232 00:D222: B5 58     LDA ram_0058,X
C - - - - - 0x001234 00:D224: D0 0C     BNE bra_D232
C - - - - - 0x001236 00:D226: B5 57     LDA ram_0057_ppu_buffer_upload_data,X
C - - - - - 0x001238 00:D228: C9 24     CMP #$24
C - - - - - 0x00123A 00:D22A: F0 17     BEQ bra_D243
C - - - - - 0x00123C 00:D22C: A9 0A     LDA #$0A
C - - - - - 0x00123E 00:D22E: 95 58     STA ram_0058,X
C - - - - - 0x001240 00:D230: D6 57     DEC ram_0057_ppu_buffer_upload_data,X
bra_D232:
C - - - - - 0x001242 00:D232: A9 0A     LDA #$0A
C - - - - - 0x001244 00:D234: 95 59     STA ram_0059,X
C - - - - - 0x001246 00:D236: D6 58     DEC ram_0058,X
bra_D238:
C - - - - - 0x001248 00:D238: D6 59     DEC ram_0059,X
C - - - - - 0x00124A 00:D23A: 8A        TXA
C - - - - - 0x00124B 00:D23B: 48        PHA
C - - - - - 0x00124C 00:D23C: A9 0A     LDA #$0A
C - - - - - 0x00124E 00:D23E: 20 DE D6  JSR sub_D6DE_Score_Add
C - - - - - 0x001251 00:D241: 68        PLA
C - - - - - 0x001252 00:D242: AA        TAX
bra_D243:
C - - - - - 0x001253 00:D243: 4C FE D1  JMP loc_D1FE
sub_D246_Clear_PPU:
C - - - - - 0x001256 00:D246: 20 0A C1  JSR sub_C10A_Clear_PPU_Mask
C - - - - - 0x001259 00:D249: 20 FA C0  JSR sub_C0FA_Disable_NMI
C - - - - - 0x00125C 00:D24C: A9 20     LDA #$20
C - - - - - 0x00125E 00:D24E: 8D 06 20  STA $2006                                                  ; PPUADDR = $2000
C - - - - - 0x001261 00:D251: A9 00     LDA #$00                                                   ; Nametable 0 Address
C - - - - - 0x001263 00:D253: 8D 06 20  STA $2006
C - - - - - 0x001266 00:D256: 20 75 D2  JSR sub_D275_Clear_Nametable                               ; Clear Nametable 0
C - - - - - 0x001269 00:D259: 20 75 D2  JSR sub_D275_Clear_Nametable                               ; Clear Nametable 1
C - - - - - 0x00126C 00:D25C: 20 04 C1  JSR sub_C104_Enable_NMI
C - - - - - 0x00126F 00:D25F: 20 15 C1  JSR sub_C115
C - - - - - 0x001272 00:D262: A2 3F     LDX #$3F
C - - - - - 0x001274 00:D264: A0 00     LDY #$00
C - - - - - 0x001276 00:D266: 84 4C     STY ram_004C_star_update
bra_D268:
; Hide all sprites.
C - - - - - 0x001278 00:D268: A9 F0     LDA #$F0
C - - - - - 0x00127A 00:D26A: 99 00 02  STA ram_0200_sprite_00_y,Y
C - - - - - 0x00127D 00:D26D: C8        INY
C - - - - - 0x00127E 00:D26E: C8        INY
C - - - - - 0x00127F 00:D26F: C8        INY
C - - - - - 0x001280 00:D270: C8        INY
C - - - - - 0x001281 00:D271: CA        DEX
C - - - - - 0x001282 00:D272: 10 F4     BPL bra_D268
C - - - - - 0x001284 00:D274: 60        RTS
sub_D275_Clear_Nametable:
C - - - - - 0x001285 00:D275: A2 F0     LDX #$F0
C - - - - - 0x001287 00:D277: A9 24     LDA #$24
bra_D279:
C - - - - - 0x001289 00:D279: 8D 07 20  STA $2007                                                  ; Fill PPU Nametable with empty tiles (ID: #$24)
C - - - - - 0x00128C 00:D27C: 8D 07 20  STA $2007                                                  ; #$3C0 bytes
C - - - - - 0x00128F 00:D27F: 8D 07 20  STA $2007
C - - - - - 0x001292 00:D282: 8D 07 20  STA $2007
C - - - - - 0x001295 00:D285: CA        DEX
C - - - - - 0x001296 00:D286: D0 F1     BNE bra_D279
C - - - - - 0x001298 00:D288: A2 40     LDX #$40
C - - - - - 0x00129A 00:D28A: A9 00     LDA #$00
bra_D28C:
C - - - - - 0x00129C 00:D28C: 8D 07 20  STA $2007                                                  ; Fill rest of Nametable with #$00 for palette attributes
C - - - - - 0x00129F 00:D28F: CA        DEX                                                        ; #$40 bytes
C - - - - - 0x0012A0 00:D290: D0 FA     BNE bra_D28C                                               ; Total: #$400 bytes
C - - - - - 0x0012A2 00:D292: 60        RTS
sub_D293_Init_Game_Mode:
C - - - - - 0x0012A3 00:D293: 20 0A C1  JSR sub_C10A_Clear_PPU_Mask
C - - - - - 0x0012A6 00:D296: 20 FA C0  JSR sub_C0FA_Disable_NMI
C - - - - - 0x0012A9 00:D299: A5 16     LDA ram_0016_game_mode                                     ; Check game mode for initialization
C - - - - - 0x0012AB 00:D29B: F0 03     BEQ bra_D2A0
C - - - - - 0x0012AD 00:D29D: 4C 72 D5  JMP loc_D572
bra_D2A0:
; Initialize Balloon Fight Game Mode.
C - - - - - 0x0012B0 00:D2A0: A4 3B     LDY ram_003B_current_level_header
C - - - - - 0x0012B2 00:D2A2: B9 2A DB  LDA tbl_DB2A_Phase_Low_Bytes,Y                             ; Load Phase graphics
C - - - - - 0x0012B5 00:D2A5: 85 1D     STA ram_001D_loading_pointer_1d
C - - - - - 0x0012B7 00:D2A7: B9 3A DB  LDA tbl_DB3A_Phase_High_Bytes,Y
C - - - - - 0x0012BA 00:D2AA: 85 1E     STA ram_001E_loading_pointer_1d
C - - - - - 0x0012BC 00:D2AC: 20 97 D4  JSR sub_D497_Upload_Background
C - - - - - 0x0012BF 00:D2AF: A2 00     LDX #$00
loc_D2B1:
C D 2 - - - 0x0012C1 00:D2B1: 20 E5 D4  JSR sub_D4E5_Get_Byte_Load_Pointer
C - - - - - 0x0012C4 00:D2B4: C9 FF     CMP #$FF                                                   ; Load Clouds (XX YY)
C - - - - - 0x0012C6 00:D2B6: F0 6A     BEQ bra_D322                                               ; until one has #$FF as
C - - - - - 0x0012C8 00:D2B8: 85 54     STA ram_0054_temp_cloud_flip_x                             ; X coordinate
C - - - - - 0x0012CA 00:D2BA: 20 E5 D4  JSR sub_D4E5_Get_Byte_Load_Pointer
C - - - - - 0x0012CD 00:D2BD: 85 55     STA ram_0055_temp_cloud_flip_y
C - - - - - 0x0012CF 00:D2BF: A0 03     LDY #$03
bra_D2C1:
C - - - - - 0x0012D1 00:D2C1: 20 FB D4  JSR sub_D4FB_Set_PPU_Address_Render
C - - - - - 0x0012D4 00:D2C4: A9 04     LDA #$04
C - - - - - 0x0012D6 00:D2C6: 85 12     STA ram_0012_temp                                          ; Render Cloud
C - - - - - 0x0012D8 00:D2C8: B9 93 D4  LDA tbl_D493,Y                                             ; to the screen
bra_D2CB:
C - - - - - 0x0012DB 00:D2CB: 8D 07 20  STA $2007
C - - - - - 0x0012DE 00:D2CE: 18        CLC
C - - - - - 0x0012DF 00:D2CF: 69 04     ADC #$04
C - - - - - 0x0012E1 00:D2D1: C6 12     DEC ram_0012_temp
C - - - - - 0x0012E3 00:D2D3: D0 F6     BNE bra_D2CB
C - - - - - 0x0012E5 00:D2D5: E6 55     INC ram_0055_temp_cloud_flip_y
C - - - - - 0x0012E7 00:D2D7: 88        DEY
C - - - - - 0x0012E8 00:D2D8: 10 E7     BPL bra_D2C1
C - - - - - 0x0012EA 00:D2DA: A5 55     LDA ram_0055_temp_cloud_flip_y
C - - - - - 0x0012EC 00:D2DC: 38        SEC
C - - - - - 0x0012ED 00:D2DD: E9 04     SBC #$04
C - - - - - 0x0012EF 00:D2DF: 85 55     STA ram_0055_temp_cloud_flip_y
C - - - - - 0x0012F1 00:D2E1: 20 1C D5  JSR sub_D51C
C - - - - - 0x0012F4 00:D2E4: 95 A6     STA ram_00A6,X
C - - - - - 0x0012F6 00:D2E6: E6 54     INC ram_0054_temp_cloud_flip_x
C - - - - - 0x0012F8 00:D2E8: E6 54     INC ram_0054_temp_cloud_flip_x
C - - - - - 0x0012FA 00:D2EA: 20 1C D5  JSR sub_D51C
C - - - - - 0x0012FD 00:D2ED: 95 A9     STA ram_00A9,X
C - - - - - 0x0012FF 00:D2EF: E6 55     INC ram_0055_temp_cloud_flip_y
C - - - - - 0x001301 00:D2F1: E6 55     INC ram_0055_temp_cloud_flip_y
C - - - - - 0x001303 00:D2F3: 20 1C D5  JSR sub_D51C
C - - - - - 0x001306 00:D2F6: 95 AF     STA ram_00AF,X
C - - - - - 0x001308 00:D2F8: C6 54     DEC ram_0054_temp_cloud_flip_x
C - - - - - 0x00130A 00:D2FA: C6 54     DEC ram_0054_temp_cloud_flip_x
C - - - - - 0x00130C 00:D2FC: 20 1C D5  JSR sub_D51C
C - - - - - 0x00130F 00:D2FF: 95 AC     STA ram_00AC,X
C - - - - - 0x001311 00:D301: 86 A4     STX ram_00A4_cloud_id_blink
C - - - - - 0x001313 00:D303: A9 03     LDA #$03
C - - - - - 0x001315 00:D305: 20 56 C8  JSR sub_C856
C - - - - - 0x001318 00:D308: 20 7C C1  JSR sub_C17C_Upload_PPU_Buffer
C - - - - - 0x00131B 00:D30B: A6 A4     LDX ram_00A4_cloud_id_blink
C - - - - - 0x00131D 00:D30D: A5 54     LDA ram_0054_temp_cloud_flip_x
C - - - - - 0x00131F 00:D30F: 0A        ASL
C - - - - - 0x001320 00:D310: 0A        ASL
C - - - - - 0x001321 00:D311: 0A        ASL
C - - - - - 0x001322 00:D312: 18        CLC
C - - - - - 0x001323 00:D313: 69 10     ADC #$10
C - - - - - 0x001325 00:D315: 95 B2     STA ram_00B2_cloud_related,X
C - - - - - 0x001327 00:D317: A5 55     LDA ram_0055_temp_cloud_flip_y
C - - - - - 0x001329 00:D319: 0A        ASL
C - - - - - 0x00132A 00:D31A: 0A        ASL
C - - - - - 0x00132B 00:D31B: 0A        ASL
C - - - - - 0x00132C 00:D31C: 95 B5     STA ram_00B5_cloud_related,X
C - - - - - 0x00132E 00:D31E: E8        INX
C - - - - - 0x00132F 00:D31F: 4C B1 D2  JMP loc_D2B1                                               ; Load another Cloud's data
bra_D322:
C - - - - - 0x001332 00:D322: CA        DEX                                                        ; Write amount of Clouds to RAM
C - - - - - 0x001333 00:D323: 86 A3     STX ram_00A3_amount_of_clouds
C - - - - - 0x001335 00:D325: A2 00     LDX #$00
loc_D327:
C D 2 - - - 0x001337 00:D327: 20 E5 D4  JSR sub_D4E5_Get_Byte_Load_Pointer
C - - - - - 0x00133A 00:D32A: C9 FF     CMP #$FF                                                   ; Load Flippers (XX YY TT)
C - - - - - 0x00133C 00:D32C: F0 50     BEQ bra_D37E                                               ; until one has #$FF as
C - - - - - 0x00133E 00:D32E: 85 54     STA ram_0054_temp_cloud_flip_x                             ; X coordinate
C - - - - - 0x001340 00:D330: 20 E5 D4  JSR sub_D4E5_Get_Byte_Load_Pointer
C - - - - - 0x001343 00:D333: 85 55     STA ram_0055_temp_cloud_flip_y
C - - - - - 0x001345 00:D335: 20 E5 D4  JSR sub_D4E5_Get_Byte_Load_Pointer
C - - - - - 0x001348 00:D338: 9D FA 05  STA ram_05FA_flippers_type,X
C - - - - - 0x00134B 00:D33B: A5 54     LDA ram_0054_temp_cloud_flip_x
C - - - - - 0x00134D 00:D33D: 0A        ASL
C - - - - - 0x00134E 00:D33E: 0A        ASL
C - - - - - 0x00134F 00:D33F: 0A        ASL
C - - - - - 0x001350 00:D340: 69 0C     ADC #$0C
C - - - - - 0x001352 00:D342: 9D D2 05  STA ram_05D2_flipper_x_pos,X
C - - - - - 0x001355 00:D345: A5 55     LDA ram_0055_temp_cloud_flip_y
C - - - - - 0x001357 00:D347: 0A        ASL
C - - - - - 0x001358 00:D348: 0A        ASL
C - - - - - 0x001359 00:D349: 0A        ASL
C - - - - - 0x00135A 00:D34A: 69 0C     ADC #$0C
C - - - - - 0x00135C 00:D34C: 9D DC 05  STA ram_05DC_flipper_y_pos,X
C - - - - - 0x00135F 00:D34F: A9 00     LDA #$00
C - - - - - 0x001361 00:D351: 9D 04 06  STA ram_0604,X
C - - - - - 0x001364 00:D354: 20 FB D4  JSR sub_D4FB_Set_PPU_Address_Render
C - - - - - 0x001367 00:D357: 9D E6 05  STA ram_05E6,X
C - - - - - 0x00136A 00:D35A: A5 13     LDA ram_0013_temp
C - - - - - 0x00136C 00:D35C: 9D F0 05  STA ram_05F0,X
C - - - - - 0x00136F 00:D35F: 20 6C D5  JSR sub_D56C
C - - - - - 0x001372 00:D362: 20 3C D5  JSR sub_D53C
C - - - - - 0x001375 00:D365: E6 54     INC ram_0054_temp_cloud_flip_x
C - - - - - 0x001377 00:D367: E6 54     INC ram_0054_temp_cloud_flip_x
C - - - - - 0x001379 00:D369: 20 3C D5  JSR sub_D53C
C - - - - - 0x00137C 00:D36C: E6 55     INC ram_0055_temp_cloud_flip_y
C - - - - - 0x00137E 00:D36E: E6 55     INC ram_0055_temp_cloud_flip_y
C - - - - - 0x001380 00:D370: 20 3C D5  JSR sub_D53C
C - - - - - 0x001383 00:D373: C6 54     DEC ram_0054_temp_cloud_flip_x
C - - - - - 0x001385 00:D375: C6 54     DEC ram_0054_temp_cloud_flip_x
C - - - - - 0x001387 00:D377: 20 3C D5  JSR sub_D53C
C - - - - - 0x00138A 00:D37A: E8        INX
C - - - - - 0x00138B 00:D37B: 4C 27 D3  JMP loc_D327                                               ; Load another Flipper's data
bra_D37E:
C - - - - - 0x00138E 00:D37E: CA        DEX                                                        ; Write amount of Flippers to RAM
C - - - - - 0x00138F 00:D37F: 8E D1 05  STX ram_05D1_amount_of_flippers
C - - - - - 0x001392 00:D382: 20 E5 D4  JSR sub_D4E5_Get_Byte_Load_Pointer                         ; Load Enemy Data Pointer
C - - - - - 0x001395 00:D385: 85 1F     STA ram_001F_gfx_enemy_data_pointer_1f
C - - - - - 0x001397 00:D387: 20 E5 D4  JSR sub_D4E5_Get_Byte_Load_Pointer
C - - - - - 0x00139A 00:D38A: 85 20     STA ram_0020_gfx_enemy_data_pointer_1f
C - - - - - 0x00139C 00:D38C: A0 00     LDY #$00
C - - - - - 0x00139E 00:D38E: B1 1F     LDA (ram_001F_gfx_enemy_data_pointer_1f),Y                 ; Load Enemy Amount
C - - - - - 0x0013A0 00:D390: AA        TAX
C - - - - - 0x0013A1 00:D391: CA        DEX
C - - - - - 0x0013A2 00:D392: 10 05     BPL bra_D399
C - - - - - 0x0013A4 00:D394: E6 C8     INC ram_00C8_phase_type                                    ; If no enemies then it's a Bonus Phase
C - - - - - 0x0013A6 00:D396: 4C BA D3  JMP loc_D3BA                                               ; Skip enemy loading
bra_D399:
C - - - - - 0x0013A9 00:D399: C8        INY
bra_D39A:
C - - - - - 0x0013AA 00:D39A: B1 1F     LDA (ram_001F_gfx_enemy_data_pointer_1f),Y                 ; Load Enemy X Position
C - - - - - 0x0013AC 00:D39C: C8        INY
C - - - - - 0x0013AD 00:D39D: 95 93     STA ram_0093_object_x_pos_int_enemy1,X
C - - - - - 0x0013AF 00:D39F: B1 1F     LDA (ram_001F_gfx_enemy_data_pointer_1f),Y                 ; Load Enemy Y Position
C - - - - - 0x0013B1 00:D3A1: C8        INY
C - - - - - 0x0013B2 00:D3A2: 95 9C     STA ram_009C_object_y_pos_int_enemy1,X
C - - - - - 0x0013B4 00:D3A4: B1 1F     LDA (ram_001F_gfx_enemy_data_pointer_1f),Y                 ; Load Enemy Type
C - - - - - 0x0013B6 00:D3A6: C8        INY
C - - - - - 0x0013B7 00:D3A7: 9D 53 04  STA ram_0453_object_type_enemy1,X
C - - - - - 0x0013BA 00:D3AA: A9 02     LDA #$02                                                   ; Initialize Enemy Status
C - - - - - 0x0013BC 00:D3AC: 95 81     STA ram_0081_object_status_enemy1,X                        ; (#$02 = Sitting)
C - - - - - 0x0013BE 00:D3AE: A9 01     LDA #$01                                                   ; Initialize Enemy Balloons
C - - - - - 0x0013C0 00:D3B0: 95 8A     STA ram_008A_object_balloons_enemy1,X                      ; (#$01 = Sitting/Umbrella)
C - - - - - 0x0013C2 00:D3B2: A5 C6     LDA ram_00C6                                               ; Initialize Enemy Animation Frame Timer
C - - - - - 0x0013C4 00:D3B4: 9D 41 04  STA ram_0441_animation_f_timer_enemy1,X
C - - - - - 0x0013C7 00:D3B7: CA        DEX
C - - - - - 0x0013C8 00:D3B8: 10 E0     BPL bra_D39A                                               ; Load another Enemy's data
loc_D3BA:
C D 2 - - - 0x0013CA 00:D3BA: 20 E5 D4  JSR sub_D4E5_Get_Byte_Load_Pointer                         ; Load amount of platforms
C - - - - - 0x0013CD 00:D3BD: 85 CD     STA ram_00CD_amount_of_platforms
C - - - - - 0x0013CF 00:D3BF: 20 E5 D4  JSR sub_D4E5_Get_Byte_Load_Pointer                         ; Load Platform Collision Pointer
C - - - - - 0x0013D2 00:D3C2: 85 23     STA ram_0023_plat_coll_pointer_left
C - - - - - 0x0013D4 00:D3C4: 20 E5 D4  JSR sub_D4E5_Get_Byte_Load_Pointer                         ; Left Side
C - - - - - 0x0013D7 00:D3C7: A8        TAY
C - - - - - 0x0013D8 00:D3C8: 85 24     STA ram_0024_plat_coll_pointer_left
C - - - - - 0x0013DA 00:D3CA: A5 23     LDA ram_0023_plat_coll_pointer_left
C - - - - - 0x0013DC 00:D3CC: 20 8C D4  JSR sub_D48C_Next_Platform_Pointer                         ; Load Right Side Platform Collision Pointer
C - - - - - 0x0013DF 00:D3CF: 85 25     STA ram_0025_plat_coll_pointer_right
C - - - - - 0x0013E1 00:D3D1: 84 26     STY ram_0026_plat_coll_pointer_right
C - - - - - 0x0013E3 00:D3D3: 20 8C D4  JSR sub_D48C_Next_Platform_Pointer                         ; Load Top Side Platform Collision Pointer
C - - - - - 0x0013E6 00:D3D6: 85 27     STA ram_0027_plat_coll_pointer_top
C - - - - - 0x0013E8 00:D3D8: 84 28     STY ram_0028_plat_coll_pointer_top
C - - - - - 0x0013EA 00:D3DA: 20 8C D4  JSR sub_D48C_Next_Platform_Pointer                         ; Load Bottom Side Platform Collision Pointer
C - - - - - 0x0013ED 00:D3DD: 85 29     STA ram_0029_plat_coll_pointer_bottom
C - - - - - 0x0013EF 00:D3DF: 84 2A     STY ram_002A_plat_coll_pointer_bottom
loc_D3E1:
C D 2 - - - 0x0013F1 00:D3E1: 20 D9 D5  JSR sub_D5D9
C - - - - - 0x0013F4 00:D3E4: 20 ED D3  JSR sub_D3ED_Set_Palette
C - - - - - 0x0013F7 00:D3E7: 20 04 C1  JSR sub_C104_Enable_NMI
C - - - - - 0x0013FA 00:D3EA: 4C 15 C1  JMP loc_C115
sub_D3ED_Set_Palette:
C - - - - - 0x0013FD 00:D3ED: A2 22     LDX #$22
bra_D3EF:
C - - - - - 0x0013FF 00:D3EF: BD 37 D4  LDA tbl_D437_Main_Palette,X                                ; Copy palette
C - - - - - 0x001402 00:D3F2: 95 57     STA ram_0057_ppu_buffer_upload_data,X                      ; to PPU Temp Block
C - - - - - 0x001404 00:D3F4: CA        DEX
C - - - - - 0x001405 00:D3F5: 10 F8     BPL bra_D3EF
C - - - - - 0x001407 00:D3F7: A5 C8     LDA ram_00C8_phase_type                                    ; Check Phase type...
C - - - - - 0x001409 00:D3F9: D0 15     BNE bra_D410
C - - - - - 0x00140B 00:D3FB: A5 3B     LDA ram_003B_current_level_header                          ; ...If Normal Phase
C - - - - - 0x00140D 00:D3FD: 29 0C     AND #$0C                                                   ; Select palette based
C - - - - - 0x00140F 00:D3FF: 09 03     ORA #$03                                                   ; on current level header
C - - - - - 0x001411 00:D401: A8        TAY
C - - - - - 0x001412 00:D402: A2 03     LDX #$03
bra_D404:
C - - - - - 0x001414 00:D404: B9 5A D4  LDA tbl_D45A_Normal_Phase_Palettes,Y                       ; Copy single palette data
C - - - - - 0x001417 00:D407: 95 5A     STA ram_005A,X                                             ; to Background Palette 1
C - - - - - 0x001419 00:D409: 88        DEY                                                        ; to PPU Temp Block
C - - - - - 0x00141A 00:D40A: CA        DEX
C - - - - - 0x00141B 00:D40B: 10 F7     BPL bra_D404
bra_D40D:
C - - - - - 0x00141D 00:D40D: 4C 2D C1  JMP loc_C12D_Copy_PPU_Temp_Block
bra_D410:
C - - - - - 0x001420 00:D410: AE 58 05  LDX ram_0558_bonus_phase_intensity_level                   ; ...If Bonus Phase
C - - - - - 0x001423 00:D413: BD 6A D4  LDA tbl_D46A_Bonus_Phase_Balloon_Palettes_Low_Bytes,X
C - - - - - 0x001426 00:D416: 85 1D     STA ram_001D_loading_pointer_1d                            ; Select Balloon Palette
C - - - - - 0x001428 00:D418: BD 6F D4  LDA tbl_D46F_Bonus_Phase_Balloon_Palettes_High_Bytes,X     ; based on Intensity Level
C - - - - - 0x00142B 00:D41B: 85 1E     STA ram_001E_loading_pointer_1d
C - - - - - 0x00142D 00:D41D: A2 03     LDX #$03
C - - - - - 0x00142F 00:D41F: A0 07     LDY #$07
bra_D421:
C - - - - - 0x001431 00:D421: B1 1D     LDA (ram_001D_loading_pointer_1d),Y                        ; Copy second palette data
C - - - - - 0x001433 00:D423: 95 72     STA ram_0072,X                                             ; to Sprite Palette 2
C - - - - - 0x001435 00:D425: 88        DEY                                                        ; to PPU Temp Block
C - - - - - 0x001436 00:D426: CA        DEX
C - - - - - 0x001437 00:D427: 10 F8     BPL bra_D421
C - - - - - 0x001439 00:D429: A5 16     LDA ram_0016_game_mode                                     ; If Balloon Trip Mode
C - - - - - 0x00143B 00:D42B: D0 E0     BNE bra_D40D                                               ; then stop and copy PPU Temp Block as is
bra_D42D:
C - - - - - 0x00143D 00:D42D: B1 1D     LDA (ram_001D_loading_pointer_1d),Y                        ; Copy first palette data
C - - - - - 0x00143F 00:D42F: 99 5A 00  STA ram_005A,Y                                             ; to Background Palette 1
C - - - - - 0x001442 00:D432: 88        DEY                                                        ; to PPU Temp Block
C - - - - - 0x001443 00:D433: 10 F8     BPL bra_D42D
C - - - - - 0x001445 00:D435: 30 D6     BMI bra_D40D
tbl_D437_Main_Palette:
- D 2 - - - 0x001447 00:D437: 3F 00     .dbyt $3F00                                                ; Palette at PPU Address $3F00
- D 2 - - - 0x001449 00:D439: 20        .byte $20                                                  ; 32 bytes
- D 2 - - - 0x00144A 00:D43A: 0F        .byte $0F, $2A, $09, $07, $0F, $30, $27, $15, $0F, $30, $02, $21, $0F, $30, $00, $10
- D 2 - - - 0x00145A 00:D44A: 0F        .byte $0F, $16, $12, $37, $0F, $12, $16, $37, $0F, $17, $11, $35, $0F, $17, $11, $2B
tbl_D45A_Normal_Phase_Palettes:
- D 2 - - - 0x00146A 00:D45A: 0F        .byte $0F, $2A, $09, $07                                   ; Phases 01, 02, 03
- D 2 - - - 0x00146E 00:D45E: 0F        .byte $0F, $26, $06, $07                                   ; Phases 04, 05, 06
- D 2 - - - 0x001472 00:D462: 0F        .byte $0F, $1B, $0C, $07                                   ; Phases 07, 08, 09
- D 2 - - - 0x001476 00:D466: 0F        .byte $0F, $2C, $01, $06                                   ; Phases 10, 11, 12
tbl_D46A_Bonus_Phase_Balloon_Palettes_Low_Bytes:
- D 2 - - - 0x00147A 00:D46A: 74        .byte < off_D474                                           ; Intensity = 0
- D 2 - - - 0x00147B 00:D46B: 7C        .byte < off_D47C                                           ; Intensity = 1
- D 2 - - - 0x00147C 00:D46C: 84        .byte < off_D484                                           ; Intensity = 2
- D 2 - - - 0x00147D 00:D46D: 84        .byte < off_D484                                           ; Intensity = 3
- D 2 - - - 0x00147E 00:D46E: 84        .byte < off_D484                                           ; Intensity = 4
tbl_D46F_Bonus_Phase_Balloon_Palettes_High_Bytes:
- D 2 - - - 0x00147F 00:D46F: D4        .byte > off_D474                                           ; Intensity = 0; $D474
- D 2 - - - 0x001480 00:D470: D4        .byte > off_D47C                                           ; Intensity = 1; $D47C
- D 2 - - - 0x001481 00:D471: D4        .byte > off_D484                                           ; Intensity = 2; $D484
- D 2 - - - 0x001482 00:D472: D4        .byte > off_D484                                           ; Intensity = 3; $D484
- D 2 - - - 0x001483 00:D473: D4        .byte > off_D484                                           ; Intensity = 4; $D484

off_D474:
- D 2 - I - 0x001484 00:D474: 0F        .byte $0F, $02, $08, $06                                   ; Background Palette 1 for Bonus Phase 1
- D 2 - I - 0x001488 00:D478: 0F        .byte $0F, $2B, $30, $12                                   ; Sprite Palette 2 for Intensity 0
off_D47C:
- D 2 - I - 0x00148C 00:D47C: 0F        .byte $0F, $07, $0A, $19                                   ; Background Palette 1 for Bonus Phase 2
- D 2 - I - 0x001490 00:D480: 0F        .byte $0F, $26, $30, $2B                                   ; Sprite Palette 2 for Intensity 1
off_D484:
- D 2 - I - 0x001494 00:D484: 0F        .byte $0F, $07, $0C, $1C                                   ; Background Palette 1 for Bonus Phase 3+
- D 2 - I - 0x001498 00:D488: 0F        .byte $0F, $15, $30, $26                                   ; Sprite Palette 2 for Intensity 2, 3, 4
sub_D48C_Next_Platform_Pointer:
C - - - - - 0x00149C 00:D48C: 38        SEC
C - - - - - 0x00149D 00:D48D: 65 CD     ADC ram_00CD_amount_of_platforms
C - - - - - 0x00149F 00:D48F: 90 01     BCC bra_D492_RTS
- - - - - - 0x0014A1 00:D491: C8        INY
bra_D492_RTS:
C - - - - - 0x0014A2 00:D492: 60        RTS
tbl_D493:
- D 2 - - - 0x0014A3 00:D493: 7F        .byte $7F   ; 
- D 2 - - - 0x0014A4 00:D494: 7E        .byte $7E   ; 
- D 2 - - - 0x0014A5 00:D495: 7D        .byte $7D   ; 
- D 2 - - - 0x0014A6 00:D496: 7C        .byte $7C   ; 
bra_D497_Upload_Background:
sub_D497_Upload_Background:
; Argument: $001D = Pointer to pointers to screen data.
C - - - - - 0x0014A7 00:D497: 20 E5 D4  JSR sub_D4E5_Get_Byte_Load_Pointer
C - - - - - 0x0014AA 00:D49A: 85 1F     STA ram_001F_gfx_enemy_data_pointer_1f
C - - - - - 0x0014AC 00:D49C: 20 E5 D4  JSR sub_D4E5_Get_Byte_Load_Pointer
C - - - - - 0x0014AF 00:D49F: 85 20     STA ram_0020_gfx_enemy_data_pointer_1f
C - - - - - 0x0014B1 00:D4A1: AA        TAX
C - - - - - 0x0014B2 00:D4A2: F0 40     BEQ bra_D4E4_RTS
bra_D4A4:
C - - - - - 0x0014B4 00:D4A4: 20 F0 D4  JSR sub_D4F0_Get_Byte_From_GFX_Pointer
C - - - - - 0x0014B7 00:D4A7: AA        TAX
C - - - - - 0x0014B8 00:D4A8: F0 ED     BEQ bra_D497_Upload_Background
C - - - - - 0x0014BA 00:D4AA: 29 7F     AND #$7F
C - - - - - 0x0014BC 00:D4AC: 8D 06 20  STA $2006
C - - - - - 0x0014BF 00:D4AF: 20 F0 D4  JSR sub_D4F0_Get_Byte_From_GFX_Pointer
C - - - - - 0x0014C2 00:D4B2: 8D 06 20  STA $2006
C - - - - - 0x0014C5 00:D4B5: 20 F0 D4  JSR sub_D4F0_Get_Byte_From_GFX_Pointer
C - - - - - 0x0014C8 00:D4B8: 85 12     STA ram_0012_temp
C - - - - - 0x0014CA 00:D4BA: 8A        TXA
C - - - - - 0x0014CB 00:D4BB: 29 80     AND #$80
C - - - - - 0x0014CD 00:D4BD: 4A        LSR
C - - - - - 0x0014CE 00:D4BE: 4A        LSR
C - - - - - 0x0014CF 00:D4BF: 4A        LSR
C - - - - - 0x0014D0 00:D4C0: 4A        LSR
C - - - - - 0x0014D1 00:D4C1: 4A        LSR
C - - - - - 0x0014D2 00:D4C2: 05 00     ORA ram_0000_ppuctrl_shadow
C - - - - - 0x0014D4 00:D4C4: 8D 00 20  STA $2000
C - - - - - 0x0014D7 00:D4C7: 8A        TXA
C - - - - - 0x0014D8 00:D4C8: 29 40     AND #$40
C - - - - - 0x0014DA 00:D4CA: D0 0C     BNE bra_D4D8
bra_D4CC:
C - - - - - 0x0014DC 00:D4CC: 20 F0 D4  JSR sub_D4F0_Get_Byte_From_GFX_Pointer
C - - - - - 0x0014DF 00:D4CF: 8D 07 20  STA $2007
C - - - - - 0x0014E2 00:D4D2: C6 12     DEC ram_0012_temp
C - - - - - 0x0014E4 00:D4D4: D0 F6     BNE bra_D4CC
C - - - - - 0x0014E6 00:D4D6: F0 CC     BEQ bra_D4A4
bra_D4D8:
C - - - - - 0x0014E8 00:D4D8: 20 F0 D4  JSR sub_D4F0_Get_Byte_From_GFX_Pointer
bra_D4DB:
C - - - - - 0x0014EB 00:D4DB: 8D 07 20  STA $2007
C - - - - - 0x0014EE 00:D4DE: C6 12     DEC ram_0012_temp
C - - - - - 0x0014F0 00:D4E0: D0 F9     BNE bra_D4DB
C - - - - - 0x0014F2 00:D4E2: F0 C0     BEQ bra_D4A4
bra_D4E4_RTS:
C - - - - - 0x0014F4 00:D4E4: 60        RTS
sub_D4E5_Get_Byte_Load_Pointer:
C - - - - - 0x0014F5 00:D4E5: A0 00     LDY #$00
C - - - - - 0x0014F7 00:D4E7: B1 1D     LDA (ram_001D_loading_pointer_1d),Y
C - - - - - 0x0014F9 00:D4E9: E6 1D     INC ram_001D_loading_pointer_1d
C - - - - - 0x0014FB 00:D4EB: D0 02     BNE bra_D4EF_RTS
C - - - - - 0x0014FD 00:D4ED: E6 1E     INC ram_001E_loading_pointer_1d
bra_D4EF_RTS:
C - - - - - 0x0014FF 00:D4EF: 60        RTS
sub_D4F0_Get_Byte_From_GFX_Pointer:
C - - - - - 0x001500 00:D4F0: A0 00     LDY #$00
C - - - - - 0x001502 00:D4F2: B1 1F     LDA (ram_001F_gfx_enemy_data_pointer_1f),Y
C - - - - - 0x001504 00:D4F4: E6 1F     INC ram_001F_gfx_enemy_data_pointer_1f
C - - - - - 0x001506 00:D4F6: D0 02     BNE bra_D4FA_RTS
C - - - - - 0x001508 00:D4F8: E6 20     INC ram_0020_gfx_enemy_data_pointer_1f
bra_D4FA_RTS:
C - - - - - 0x00150A 00:D4FA: 60        RTS
sub_D4FB_Set_PPU_Address_Render:
C - - - - - 0x00150B 00:D4FB: A5 55     LDA ram_0055_temp_cloud_flip_y
C - - - - - 0x00150D 00:D4FD: 85 12     STA ram_0012_temp
C - - - - - 0x00150F 00:D4FF: A9 00     LDA #$00
C - - - - - 0x001511 00:D501: 06 12     ASL ram_0012_temp
C - - - - - 0x001513 00:D503: 06 12     ASL ram_0012_temp
C - - - - - 0x001515 00:D505: 06 12     ASL ram_0012_temp
C - - - - - 0x001517 00:D507: 06 12     ASL ram_0012_temp
C - - - - - 0x001519 00:D509: 2A        ROL
C - - - - - 0x00151A 00:D50A: 06 12     ASL ram_0012_temp
C - - - - - 0x00151C 00:D50C: 2A        ROL
C - - - - - 0x00151D 00:D50D: 09 20     ORA #$20
C - - - - - 0x00151F 00:D50F: 8D 06 20  STA $2006
C - - - - - 0x001522 00:D512: 85 13     STA ram_0013_temp
C - - - - - 0x001524 00:D514: A5 12     LDA ram_0012_temp
C - - - - - 0x001526 00:D516: 05 54     ORA ram_0054_temp_cloud_flip_x
C - - - - - 0x001528 00:D518: 8D 06 20  STA $2006
C - - - - - 0x00152B 00:D51B: 60        RTS
sub_D51C:
C - - - - - 0x00152C 00:D51C: A5 55     LDA ram_0055_temp_cloud_flip_y
C - - - - - 0x00152E 00:D51E: 29 FC     AND #$FC
C - - - - - 0x001530 00:D520: 0A        ASL
C - - - - - 0x001531 00:D521: 85 12     STA ram_0012_temp
C - - - - - 0x001533 00:D523: A5 54     LDA ram_0054_temp_cloud_flip_x
C - - - - - 0x001535 00:D525: 4A        LSR
C - - - - - 0x001536 00:D526: 4A        LSR
C - - - - - 0x001537 00:D527: 05 12     ORA ram_0012_temp
C - - - - - 0x001539 00:D529: 09 C0     ORA #$C0
C - - - - - 0x00153B 00:D52B: 48        PHA
C - - - - - 0x00153C 00:D52C: A5 55     LDA ram_0055_temp_cloud_flip_y
C - - - - - 0x00153E 00:D52E: 29 02     AND #$02
C - - - - - 0x001540 00:D530: 85 12     STA ram_0012_temp
C - - - - - 0x001542 00:D532: A5 54     LDA ram_0054_temp_cloud_flip_x
C - - - - - 0x001544 00:D534: 29 02     AND #$02
C - - - - - 0x001546 00:D536: 4A        LSR
C - - - - - 0x001547 00:D537: 05 12     ORA ram_0012_temp
C - - - - - 0x001549 00:D539: A8        TAY
C - - - - - 0x00154A 00:D53A: 68        PLA
C - - - - - 0x00154B 00:D53B: 60        RTS
sub_D53C:
C - - - - - 0x00154C 00:D53C: A9 23     LDA #$23
C - - - - - 0x00154E 00:D53E: 8D 06 20  STA $2006
C - - - - - 0x001551 00:D541: 20 1C D5  JSR sub_D51C
C - - - - - 0x001554 00:D544: 8D 06 20  STA $2006
C - - - - - 0x001557 00:D547: AD 07 20  LDA $2007
C - - - - - 0x00155A 00:D54A: AD 07 20  LDA $2007
C - - - - - 0x00155D 00:D54D: 39 64 D5  AND tbl_D564,Y
C - - - - - 0x001560 00:D550: 19 68 D5  ORA tbl_D568,Y
C - - - - - 0x001563 00:D553: 48        PHA
C - - - - - 0x001564 00:D554: A9 23     LDA #$23
C - - - - - 0x001566 00:D556: 8D 06 20  STA $2006
C - - - - - 0x001569 00:D559: 20 1C D5  JSR sub_D51C
C - - - - - 0x00156C 00:D55C: 8D 06 20  STA $2006
C - - - - - 0x00156F 00:D55F: 68        PLA
C - - - - - 0x001570 00:D560: 8D 07 20  STA $2007
C - - - - - 0x001573 00:D563: 60        RTS
tbl_D564:
- D 2 - - - 0x001574 00:D564: FC        .byte $FC   ; 
- D 2 - - - 0x001575 00:D565: F3        .byte $F3   ; 
- D 2 - - - 0x001576 00:D566: CF        .byte $CF   ; 
- D 2 - - - 0x001577 00:D567: 3F        .byte $3F   ; 
tbl_D568:
- D 2 - - - 0x001578 00:D568: 01        .byte $01   ; 
- D 2 - - - 0x001579 00:D569: 04        .byte $04   ; 
- D 2 - - - 0x00157A 00:D56A: 10        .byte $10   ; 
- D 2 - - - 0x00157B 00:D56B: 40        .byte $40   ; 
sub_D56C:
C - - - - - 0x00157C 00:D56C: 20 CB CC  JSR sub_CCCB
C - - - - - 0x00157F 00:D56F: 4C 7C C1  JMP loc_C17C_Upload_PPU_Buffer
loc_D572:
; Initialize Balloon Trip Game Mode.
C D 2 - - - 0x001582 00:D572: A9 C0     LDA #$C0
C - - - - - 0x001584 00:D574: A0 23     LDY #$23
C - - - - - 0x001586 00:D576: 20 93 D5  JSR sub_D593
C - - - - - 0x001589 00:D579: A9 C0     LDA #$C0
C - - - - - 0x00158B 00:D57B: A0 27     LDY #$27
C - - - - - 0x00158D 00:D57D: 20 93 D5  JSR sub_D593
C - - - - - 0x001590 00:D580: A0 23     LDY #$23
C - - - - - 0x001592 00:D582: A9 60     LDA #$60
C - - - - - 0x001594 00:D584: 20 B8 D5  JSR sub_D5B8
C - - - - - 0x001597 00:D587: A0 27     LDY #$27
C - - - - - 0x001599 00:D589: A9 60     LDA #$60
C - - - - - 0x00159B 00:D58B: 20 B8 D5  JSR sub_D5B8
C - - - - - 0x00159E 00:D58E: E6 C8     INC ram_00C8_phase_type
C - - - - - 0x0015A0 00:D590: 4C E1 D3  JMP loc_D3E1
sub_D593:
C - - - - - 0x0015A3 00:D593: 8C 06 20  STY $2006
C - - - - - 0x0015A6 00:D596: 8D 06 20  STA $2006
C - - - - - 0x0015A9 00:D599: A2 00     LDX #$00
bra_D59B:
C - - - - - 0x0015AB 00:D59B: BD AE DC  LDA tbl_DCAE,X
C - - - - - 0x0015AE 00:D59E: 8D 07 20  STA $2007
C - - - - - 0x0015B1 00:D5A1: E8        INX
C - - - - - 0x0015B2 00:D5A2: E0 08     CPX #$08
C - - - - - 0x0015B4 00:D5A4: D0 F5     BNE bra_D59B
C - - - - - 0x0015B6 00:D5A6: A9 00     LDA #$00
C - - - - - 0x0015B8 00:D5A8: A2 28     LDX #$28
C - - - - - 0x0015BA 00:D5AA: 20 B1 D5  JSR sub_D5B1
C - - - - - 0x0015BD 00:D5AD: A9 AA     LDA #$AA
C - - - - - 0x0015BF 00:D5AF: A2 10     LDX #$10
bra_D5B1:
sub_D5B1:
C - - - - - 0x0015C1 00:D5B1: 8D 07 20  STA $2007
C - - - - - 0x0015C4 00:D5B4: CA        DEX
C - - - - - 0x0015C5 00:D5B5: D0 FA     BNE bra_D5B1
C - - - - - 0x0015C7 00:D5B7: 60        RTS
sub_D5B8:
C - - - - - 0x0015C8 00:D5B8: 8C 06 20  STY $2006
C - - - - - 0x0015CB 00:D5BB: 8D 06 20  STA $2006
C - - - - - 0x0015CE 00:D5BE: A2 20     LDX #$20
C - - - - - 0x0015D0 00:D5C0: A9 58     LDA #$58
C - - - - - 0x0015D2 00:D5C2: 20 C9 D5  JSR sub_D5C9
C - - - - - 0x0015D5 00:D5C5: A2 40     LDX #$40
C - - - - - 0x0015D7 00:D5C7: A9 5C     LDA #$5C
sub_D5C9:
C - - - - - 0x0015D9 00:D5C9: 85 12     STA ram_0012_temp
bra_D5CB:
C - - - - - 0x0015DB 00:D5CB: 8A        TXA
C - - - - - 0x0015DC 00:D5CC: 29 03     AND #$03
C - - - - - 0x0015DE 00:D5CE: 49 03     EOR #$03
C - - - - - 0x0015E0 00:D5D0: 05 12     ORA ram_0012_temp
C - - - - - 0x0015E2 00:D5D2: 8D 07 20  STA $2007
C - - - - - 0x0015E5 00:D5D5: CA        DEX
C - - - - - 0x0015E6 00:D5D6: D0 F3     BNE bra_D5CB
C - - - - - 0x0015E8 00:D5D8: 60        RTS
sub_D5D9:
C - - - - - 0x0015E9 00:D5D9: A2 00     LDX #$00
bra_D5DB:
C - - - - - 0x0015EB 00:D5DB: 20 51 D6  JSR sub_D651
C - - - - - 0x0015EE 00:D5DE: 20 F1 D5  JSR sub_D5F1
C - - - - - 0x0015F1 00:D5E1: A5 51     LDA ram_0051_star_anim_ppu_hi
C - - - - - 0x0015F3 00:D5E3: 09 04     ORA #$04
C - - - - - 0x0015F5 00:D5E5: 85 51     STA ram_0051_star_anim_ppu_hi
C - - - - - 0x0015F7 00:D5E7: 20 F1 D5  JSR sub_D5F1
C - - - - - 0x0015FA 00:D5EA: E8        INX
C - - - - - 0x0015FB 00:D5EB: E8        INX
C - - - - - 0x0015FC 00:D5EC: E0 80     CPX #$80
C - - - - - 0x0015FE 00:D5EE: D0 EB     BNE bra_D5DB
C - - - - - 0x001600 00:D5F0: 60        RTS
sub_D5F1:
C - - - - - 0x001601 00:D5F1: A5 51     LDA ram_0051_star_anim_ppu_hi
C - - - - - 0x001603 00:D5F3: 8D 06 20  STA $2006
C - - - - - 0x001606 00:D5F6: A5 50     LDA ram_0050_star_anim_ppu_lo
C - - - - - 0x001608 00:D5F8: 8D 06 20  STA $2006
C - - - - - 0x00160B 00:D5FB: AD 07 20  LDA $2007
C - - - - - 0x00160E 00:D5FE: AD 07 20  LDA $2007
C - - - - - 0x001611 00:D601: C9 24     CMP #$24
C - - - - - 0x001613 00:D603: D0 07     BNE bra_D60C_RTS
C - - - - - 0x001615 00:D605: 8A        TXA
C - - - - - 0x001616 00:D606: 29 03     AND #$03
C - - - - - 0x001618 00:D608: A8        TAY
C - - - - - 0x001619 00:D609: 4C 3B D6  JMP loc_D63B
bra_D60C_RTS:
C - - - - - 0x00161C 00:D60C: 60        RTS
sub_D60D_Update_Star_BG_Animation:
C - - - - - 0x00161D 00:D60D: A5 4C     LDA ram_004C_star_update                                   ; If [$4C] == 0
C - - - - - 0x00161F 00:D60F: F0 29     BEQ bra_D63A_RTS                                           ; Then do nothing
C - - - - - 0x001621 00:D611: C6 4C     DEC ram_004C_star_update
C - - - - - 0x001623 00:D613: A5 4F     LDA ram_004F_star_anim_id
C - - - - - 0x001625 00:D615: 18        CLC
C - - - - - 0x001626 00:D616: 69 02     ADC #$02                                                   ; Update and get current
C - - - - - 0x001628 00:D618: 29 3F     AND #$3F                                                   ; Star ID
C - - - - - 0x00162A 00:D61A: 85 4F     STA ram_004F_star_anim_id
C - - - - - 0x00162C 00:D61C: AA        TAX
C - - - - - 0x00162D 00:D61D: 20 51 D6  JSR sub_D651
C - - - - - 0x001630 00:D620: A5 51     LDA ram_0051_star_anim_ppu_hi
C - - - - - 0x001632 00:D622: 8D 06 20  STA $2006                                                  ; Set PPU Address for Star Tile
C - - - - - 0x001635 00:D625: A5 50     LDA ram_0050_star_anim_ppu_lo
C - - - - - 0x001637 00:D627: 8D 06 20  STA $2006
C - - - - - 0x00163A 00:D62A: AD 07 20  LDA $2007
C - - - - - 0x00163D 00:D62D: AD 07 20  LDA $2007
C - - - - - 0x001640 00:D630: A0 03     LDY #$03                                                   ; Check if Tile is part of
bra_D632:
C - - - - - 0x001642 00:D632: D9 4C D6  CMP tbl_D64C_Star_Tile_Animation,Y                         ; Star Animation Tiles
C - - - - - 0x001645 00:D635: F0 04     BEQ bra_D63B                                               ; If not, stop
C - - - - - 0x001647 00:D637: 88        DEY
C - - - - - 0x001648 00:D638: 10 F8     BPL bra_D632
bra_D63A_RTS:
C - - - - - 0x00164A 00:D63A: 60        RTS
bra_D63B:
loc_D63B:
C D 2 - - - 0x00164B 00:D63B: A5 51     LDA ram_0051_star_anim_ppu_hi                              ; Write next Star Tile
C - - - - - 0x00164D 00:D63D: 8D 06 20  STA $2006
C - - - - - 0x001650 00:D640: A5 50     LDA ram_0050_star_anim_ppu_lo
C - - - - - 0x001652 00:D642: 8D 06 20  STA $2006
C - - - - - 0x001655 00:D645: B9 4D D6  LDA tbl_D64C_Star_Tile_Animation + 1,Y
C - - - - - 0x001658 00:D648: 8D 07 20  STA $2007
C - - - - - 0x00165B 00:D64B: 60        RTS
tbl_D64C_Star_Tile_Animation:
; Star Tile Animation Frames.
- D 2 - - - 0x00165C 00:D64C: 24        .byte $24, $ED, $EE, $EF, $24
sub_D651:
C - - - - - 0x001661 00:D651: BD 5C D6  LDA tbl_D65C_Animated_Locations,X
C - - - - - 0x001664 00:D654: 85 50     STA ram_0050_star_anim_ppu_lo
C - - - - - 0x001666 00:D656: BD 5D D6  LDA tbl_D65C_Animated_Locations + 1,X
C - - - - - 0x001669 00:D659: 85 51     STA ram_0051_star_anim_ppu_hi
C - - - - - 0x00166B 00:D65B: 60        RTS
tbl_D65C_Animated_Locations:
; PPU Addresses of animated tiles.
- D 2 - - - 0x00166C 00:D65C: 63 21     .word $2163   ; 00: PPUADDR = $2163
- D 2 - - - 0x00166E 00:D65E: A5 21     .word $21A5   ; 01: PPUADDR = $21A5
- D 2 - - - 0x001670 00:D660: CB 20     .word $20CB   ; 02: PPUADDR = $20CB
- D 2 - - - 0x001672 00:D662: B7 20     .word $20B7   ; 03: PPUADDR = $20B7
- D 2 - - - 0x001674 00:D664: 7D 21     .word $217D   ; 04: PPUADDR = $217D
- D 2 - - - 0x001676 00:D666: 9B 22     .word $229B   ; 05: PPUADDR = $229B
- D 2 - - - 0x001678 00:D668: F2 20     .word $20F2   ; 06: PPUADDR = $20F2
- D 2 - - - 0x00167A 00:D66A: 49 22     .word $2249   ; 07: PPUADDR = $2249
- D 2 - - - 0x00167C 00:D66C: 6D 21     .word $216D   ; 08: PPUADDR = $216D
- D 2 - - - 0x00167E 00:D66E: 0B 22     .word $220B   ; 09: PPUADDR = $220B
- D 2 - - - 0x001680 00:D670: 92 22     .word $2292   ; 0A: PPUADDR = $2292
- D 2 - - - 0x001682 00:D672: 95 21     .word $2195   ; 0B: PPUADDR = $2195
- D 2 - - - 0x001684 00:D674: 1C 21     .word $211C   ; 0C: PPUADDR = $211C
- D 2 - - - 0x001686 00:D676: 48 21     .word $2148   ; 0D: PPUADDR = $2148
- D 2 - - - 0x001688 00:D678: E0 20     .word $20E0   ; 0E: PPUADDR = $20E0
- D 2 - - - 0x00168A 00:D67A: 0B 23     .word $230B   ; 0F: PPUADDR = $230B
- D 2 - - - 0x00168C 00:D67C: CE 20     .word $20CE   ; 10: PPUADDR = $20CE
- D 2 - - - 0x00168E 00:D67E: D0 21     .word $21D0   ; 11: PPUADDR = $21D0
- D 2 - - - 0x001690 00:D680: 06 21     .word $2106   ; 12: PPUADDR = $2106
- D 2 - - - 0x001692 00:D682: 19 21     .word $2119   ; 13: PPUADDR = $2119
- D 2 - - - 0x001694 00:D684: 30 22     .word $2230   ; 14: PPUADDR = $2230
- D 2 - - - 0x001696 00:D686: 8A 22     .word $228A   ; 15: PPUADDR = $228A
- D 2 - - - 0x001698 00:D688: 88 22     .word $2288   ; 16: PPUADDR = $2288
- D 2 - - - 0x00169A 00:D68A: A4 20     .word $20A4   ; 17: PPUADDR = $20A4
- D 2 - - - 0x00169C 00:D68C: 42 22     .word $2242   ; 18: PPUADDR = $2242
- D 2 - - - 0x00169E 00:D68E: 68 21     .word $2168   ; 19: PPUADDR = $2168
- D 2 - - - 0x0016A0 00:D690: 3C 22     .word $223C   ; 1A: PPUADDR = $223C
- D 2 - - - 0x0016A2 00:D692: 36 21     .word $2136   ; 1B: PPUADDR = $2136
- D 2 - - - 0x0016A4 00:D694: CA 21     .word $21CA   ; 1C: PPUADDR = $21CA
- D 2 - - - 0x0016A6 00:D696: BC 20     .word $20BC   ; 1D: PPUADDR = $20BC
- D 2 - - - 0x0016A8 00:D698: 96 21     .word $2196   ; 1E: PPUADDR = $2196
- D 2 - - - 0x0016AA 00:D69A: 4C 21     .word $214C   ; 1F: PPUADDR = $214C
- D 2 - - - 0x0016AC 00:D69C: 35 22     .word $2235   ; 20: PPUADDR = $2235
- D 2 - - - 0x0016AE 00:D69E: EF 20     .word $20EF   ; 21: PPUADDR = $20EF
- D 2 - - - 0x0016B0 00:D6A0: 68 22     .word $2268   ; 22: PPUADDR = $2268
- D 2 - - - 0x0016B2 00:D6A2: A6 20     .word $20A6   ; 23: PPUADDR = $20A6
- D 2 - - - 0x0016B4 00:D6A4: BB 21     .word $21BB   ; 24: PPUADDR = $21BB
- D 2 - - - 0x0016B6 00:D6A6: 7A 21     .word $217A   ; 25: PPUADDR = $217A
- D 2 - - - 0x0016B8 00:D6A8: EA 20     .word $20EA   ; 26: PPUADDR = $20EA
- D 2 - - - 0x0016BA 00:D6AA: F1 21     .word $21F1   ; 27: PPUADDR = $21F1
- D 2 - - - 0x0016BC 00:D6AC: C2 20     .word $20C2   ; 28: PPUADDR = $20C2
- D 2 - - - 0x0016BE 00:D6AE: 77 21     .word $2177   ; 29: PPUADDR = $2177
- D 2 - - - 0x0016C0 00:D6B0: 54 21     .word $2154   ; 2A: PPUADDR = $2154
- D 2 - - - 0x0016C2 00:D6B2: BA 20     .word $20BA   ; 2B: PPUADDR = $20BA
- D 2 - - - 0x0016C4 00:D6B4: C5 22     .word $22C5   ; 2C: PPUADDR = $22C5
- D 2 - - - 0x0016C6 00:D6B6: BE 20     .word $20BE   ; 2D: PPUADDR = $20BE
- D 2 - - - 0x0016C8 00:D6B8: FA 20     .word $20FA   ; 2E: PPUADDR = $20FA
- D 2 - - - 0x0016CA 00:D6BA: AE 21     .word $21AE   ; 2F: PPUADDR = $21AE
- D 2 - - - 0x0016CC 00:D6BC: 46 21     .word $2146   ; 30: PPUADDR = $2146
- D 2 - - - 0x0016CE 00:D6BE: 9A 21     .word $219A   ; 31: PPUADDR = $219A
- D 2 - - - 0x0016D0 00:D6C0: D2 20     .word $20D2   ; 32: PPUADDR = $20D2
- D 2 - - - 0x0016D2 00:D6C2: 3D 21     .word $213D   ; 33: PPUADDR = $213D
- D 2 - - - 0x0016D4 00:D6C4: 2B 22     .word $222B   ; 34: PPUADDR = $222B
- D 2 - - - 0x0016D6 00:D6C6: B0 20     .word $20B0   ; 35: PPUADDR = $20B0
- D 2 - - - 0x0016D8 00:D6C8: B6 21     .word $21B6   ; 36: PPUADDR = $21B6
- D 2 - - - 0x0016DA 00:D6CA: AC 20     .word $20AC   ; 37: PPUADDR = $20AC
- D 2 - - - 0x0016DC 00:D6CC: B3 20     .word $20B3   ; 38: PPUADDR = $20B3
- D 2 - - - 0x0016DE 00:D6CE: DB 20     .word $20DB   ; 39: PPUADDR = $20DB
- D 2 - - - 0x0016E0 00:D6D0: F6 20     .word $20F6   ; 3A: PPUADDR = $20F6
- D 2 - - - 0x0016E2 00:D6D2: 2C 21     .word $212C   ; 3B: PPUADDR = $212C
- D 2 - - - 0x0016E4 00:D6D4: E7 20     .word $20E7   ; 3C: PPUADDR = $20E7
- D 2 - - - 0x0016E6 00:D6D6: 62 21     .word $2162   ; 3D: PPUADDR = $2162
- D 2 - - - 0x0016E8 00:D6D8: E4 21     .word $21E4   ; 3E: PPUADDR = $21E4
- D 2 - - - 0x0016EA 00:D6DA: 4E 21     .word $214E   ; 3F: PPUADDR = $214E
sub_D6DC_Score_Update:
C - - - - - 0x0016EC 00:D6DC: A9 00     LDA #$00                                                   ; Only update score
sub_D6DE_Score_Add:
C - - - - - 0x0016EE 00:D6DE: 85 43     STA ram_0043_div_mod_result                                ; Score to add
C - - - - - 0x0016F0 00:D6E0: A5 3A     LDA ram_003A_demo_flag                                     ; If not Demo Play
C - - - - - 0x0016F2 00:D6E2: F0 01     BEQ bra_D6E5                                               ; then skip to bra_D6E5
bra_D6E4_RTS:
C - - - - - 0x0016F4 00:D6E4: 60        RTS                                                        ; Else return
bra_D6E5:
C - - - - - 0x0016F5 00:D6E5: A6 3E     LDX ram_003E_score_id_update                               ; If [$3E] >= 2
C - - - - - 0x0016F7 00:D6E7: E0 02     CPX #$02                                                   ; then return
C - - - - - 0x0016F9 00:D6E9: B0 F9     BCS bra_D6E4_RTS
C - - - - - 0x0016FB 00:D6EB: B5 41     LDA ram_0041_p1_lives,X                                    ; If Player X has no lives
C - - - - - 0x0016FD 00:D6ED: 30 F5     BMI bra_D6E4_RTS                                           ; then return
C - - - - - 0x0016FF 00:D6EF: A0 64     LDY #$64                                                   ; Process score to add up
C - - - - - 0x001701 00:D6F1: 20 7C D7  JSR sub_D77C_Divide                                        ; Score to add / 100
C - - - - - 0x001704 00:D6F4: 18        CLC
C - - - - - 0x001705 00:D6F5: 65 48     ADC ram_0048_5th_digit_score_add
C - - - - - 0x001707 00:D6F7: 85 45     STA ram_0045_3rd_digit_score_add
C - - - - - 0x001709 00:D6F9: A0 0A     LDY #$0A
C - - - - - 0x00170B 00:D6FB: 20 7C D7  JSR sub_D77C_Divide                                        ; Modulo result / 10
C - - - - - 0x00170E 00:D6FE: 85 44     STA ram_0044_2nd_digit_score_add
C - - - - - 0x001710 00:D700: A6 3F     LDX ram_003F_main_menu_cursor                              ; Selected game mode?
C - - - - - 0x001712 00:D702: BD 79 D7  LDA tbl_D779,X
C - - - - - 0x001715 00:D705: 85 21     STA ram_0021_hi_score_pointer_21                           ; Set up pointer to default top score
C - - - - - 0x001717 00:D707: A9 06     LDA #$06                                                   ; [$21] = 06XX
C - - - - - 0x001719 00:D709: 85 22     STA ram_0022_hi_score_pointer_21
C - - - - - 0x00171B 00:D70B: A5 3E     LDA ram_003E_score_id_update
C - - - - - 0x00171D 00:D70D: 0A        ASL                                                        ; Select score to update
C - - - - - 0x00171E 00:D70E: 0A        ASL                                                        ; X = [$3E] * 5
C - - - - - 0x00171F 00:D70F: 05 3E     ORA ram_003E_score_id_update
C - - - - - 0x001721 00:D711: AA        TAX
C - - - - - 0x001722 00:D712: 18        CLC
C - - - - - 0x001723 00:D713: B5 03     LDA ram_0003_p1_score_0000x,X                              ; Add score 0000X
C - - - - - 0x001725 00:D715: 65 43     ADC ram_0043_div_mod_result                                ; Lock score between 0 and 9
C - - - - - 0x001727 00:D717: 20 8F D7  JSR sub_D78F                                               ; First digit
C - - - - - 0x00172A 00:D71A: 95 03     STA ram_0003_p1_score_0000x,X
C - - - - - 0x00172C 00:D71C: B5 04     LDA ram_0004_p1_score_000x0,X                              ; Add score 000X0
C - - - - - 0x00172E 00:D71E: 65 44     ADC ram_0044_2nd_digit_score_add                           ; Lock score between 0 and 9
C - - - - - 0x001730 00:D720: 20 8F D7  JSR sub_D78F                                               ; Second digit
C - - - - - 0x001733 00:D723: 95 04     STA ram_0004_p1_score_000x0,X
C - - - - - 0x001735 00:D725: B5 05     LDA ram_0005_p1_score_00x00,X                              ; Add score 00X00
C - - - - - 0x001737 00:D727: 65 45     ADC ram_0045_3rd_digit_score_add                           ; Lock score between 0 and 9
C - - - - - 0x001739 00:D729: 20 8F D7  JSR sub_D78F                                               ; Third digit
C - - - - - 0x00173C 00:D72C: 95 05     STA ram_0005_p1_score_00x00,X
C - - - - - 0x00173E 00:D72E: B5 06     LDA ram_0006_p1_score_0x000,X                              ; Add score 0X000
C - - - - - 0x001740 00:D730: 65 47     ADC ram_0047_4th_digit_score_add                           ; Lock score between 0 and 9
C - - - - - 0x001742 00:D732: 20 8F D7  JSR sub_D78F                                               ; Fourth digit
C - - - - - 0x001745 00:D735: 95 06     STA ram_0006_p1_score_0x000,X
C - - - - - 0x001747 00:D737: B5 07     LDA ram_0007_p1_score_x0000,X                              ; Add score X0000
C - - - - - 0x001749 00:D739: 69 00     ADC #$00                                                   ; Lock score between 0 and 9
C - - - - - 0x00174B 00:D73B: 20 8F D7  JSR sub_D78F                                               ; Fifth digit
C - - - - - 0x00174E 00:D73E: 95 07     STA ram_0007_p1_score_x0000,X
C - - - - - 0x001750 00:D740: E8        INX
C - - - - - 0x001751 00:D741: E8        INX                                                        ; Goes to last digit
C - - - - - 0x001752 00:D742: E8        INX
C - - - - - 0x001753 00:D743: E8        INX
C - - - - - 0x001754 00:D744: A0 04     LDY #$04                                                   ; From highest digit
bra_D746:
C - - - - - 0x001756 00:D746: B5 03     LDA ram_0003_p1_score_0000x,X                              ; If this score digit is
C - - - - - 0x001758 00:D748: D1 21     CMP (ram_0021_hi_score_pointer_21),Y                       ; under Highest Top Score Digit
C - - - - - 0x00175A 00:D74A: 90 19     BCC bra_D765                                               ; then Top Score was not beaten
C - - - - - 0x00175C 00:D74C: D0 04     BNE bra_D752                                               ; so go to bra_D752 (stop checking)
C - - - - - 0x00175E 00:D74E: CA        DEX                                                        ; If not equal then Top Score is beaten
C - - - - - 0x00175F 00:D74F: 88        DEY                                                        ; If equal then check the lower digit
C - - - - - 0x001760 00:D750: 10 F4     BPL bra_D746                                               ; until the last
bra_D752:
C - - - - - 0x001762 00:D752: A0 00     LDY #$00
C - - - - - 0x001764 00:D754: A5 3E     LDA ram_003E_score_id_update
C - - - - - 0x001766 00:D756: 0A        ASL                                                        ; Select score???
C - - - - - 0x001767 00:D757: 0A        ASL                                                        ; X = [$3E] * 5
C - - - - - 0x001768 00:D758: 05 3E     ORA ram_003E_score_id_update
C - - - - - 0x00176A 00:D75A: AA        TAX
bra_D75B:
C - - - - - 0x00176B 00:D75B: B5 03     LDA ram_0003_p1_score_0000x,X
C - - - - - 0x00176D 00:D75D: 91 21     STA (ram_0021_hi_score_pointer_21),Y                       ; Copy Current Score
C - - - - - 0x00176F 00:D75F: E8        INX                                                        ; to Highest Top Score
C - - - - - 0x001770 00:D760: C8        INY
C - - - - - 0x001771 00:D761: C0 05     CPY #$05
C - - - - - 0x001773 00:D763: D0 F6     BNE bra_D75B
bra_D765:
C - - - - - 0x001775 00:D765: A0 04     LDY #$04
bra_D767:
C - - - - - 0x001777 00:D767: B1 21     LDA (ram_0021_hi_score_pointer_21),Y                       ; Copy Highest Top Score
C - - - - - 0x001779 00:D769: 99 0D 00  STA ram_000D_hi_score_0000x,Y                              ; back to Current Top Score
C - - - - - 0x00177C 00:D76C: 88        DEY
C - - - - - 0x00177D 00:D76D: 10 F8     BPL bra_D767
C - - - - - 0x00177F 00:D76F: E6 46     INC ram_0046_status_bar_update_flag                        ; Status Bar Update Flag
C - - - - - 0x001781 00:D771: A5 16     LDA ram_0016_game_mode
C - - - - - 0x001783 00:D773: F0 03     BEQ bra_D778_RTS                                           ; If Balloon Trip Game Mode
C - - - - - 0x001785 00:D775: 20 39 C5  JSR sub_C539_Rank_Update                                   ; then update the rank
bra_D778_RTS:
C - - - - - 0x001788 00:D778: 60        RTS
tbl_D779:
- D 2 - - - 0x001789 00:D779: 29        .byte < ram_0629_highest_score_1p_game   ; $0629
- D 2 - - - 0x00178A 00:D77A: 2E        .byte < ram_062E_highest_score_2p_game   ; $062E
- D 2 - - - 0x00178B 00:D77B: 33        .byte < ram_0633_highest_score_trip      ; $0633
sub_D77C_Divide:
; Divide [$43] by Y
C - - - - - 0x00178C 00:D77C: 84 12     STY ram_0012_temp
C - - - - - 0x00178E 00:D77E: A2 FF     LDX #$FF
C - - - - - 0x001790 00:D780: A5 43     LDA ram_0043_div_mod_result
bra_D782:
C - - - - - 0x001792 00:D782: 38        SEC
C - - - - - 0x001793 00:D783: E5 12     SBC ram_0012_temp                                          ; Subtract Y
C - - - - - 0x001795 00:D785: E8        INX                                                        ; X + 1
C - - - - - 0x001796 00:D786: B0 FA     BCS bra_D782                                               ; If it doesn't overflow, then continue
C - - - - - 0x001798 00:D788: 18        CLC
C - - - - - 0x001799 00:D789: 65 12     ADC ram_0012_temp                                          ; Add Y value again to cancel overflow
C - - - - - 0x00179B 00:D78B: 85 43     STA ram_0043_div_mod_result                                ; [$43] = Remainder
C - - - - - 0x00179D 00:D78D: 8A        TXA                                                        ; A and X = Result
C - - - - - 0x00179E 00:D78E: 60        RTS
sub_D78F:
C - - - - - 0x00179F 00:D78F: C9 0A     CMP #$0A                                                   ; Check if Score Digit >= 0x0A
C - - - - - 0x0017A1 00:D791: B0 01     BCS bra_D794                                               ; Then ...
C - - - - - 0x0017A3 00:D793: 60        RTS                                                        ; Else return
bra_D794:
C - - - - - 0x0017A4 00:D794: 38        SEC                                                        ; ... subtract 0x0A
C - - - - - 0x0017A5 00:D795: E9 0A     SBC #$0A                                                   ; from digit
C - - - - - 0x0017A7 00:D797: 60        RTS
sub_D798_Update_Status_Bar:
C - - - - - 0x0017A8 00:D798: A4 46     LDY ram_0046_status_bar_update_flag
C - - - - - 0x0017AA 00:D79A: 88        DEY
C - - - - - 0x0017AB 00:D79B: F0 03     BEQ bra_D7A0
C - - - - - 0x0017AD 00:D79D: 10 66     BPL bra_D805
C - - - - - 0x0017AF 00:D79F: 60        RTS
bra_D7A0:
C - - - - - 0x0017B0 00:D7A0: A9 20     LDA #$20
C - - - - - 0x0017B2 00:D7A2: 8D 06 20  STA $2006                                                  ; PPUADDR = $2043
C - - - - - 0x0017B5 00:D7A5: A9 43     LDA #$43
C - - - - - 0x0017B7 00:D7A7: 8D 06 20  STA $2006
C - - - - - 0x0017BA 00:D7AA: A9 8E     LDA #$8E                                                   ; Upload I- to PPU
C - - - - - 0x0017BC 00:D7AC: 8D 07 20  STA $2007
C - - - - - 0x0017BF 00:D7AF: A2 04     LDX #$04
bra_D7B1:
C - - - - - 0x0017C1 00:D7B1: B5 03     LDA ram_0003_p1_score_0000x,X                              ; Upload Player 1 Score to PPU
C - - - - - 0x0017C3 00:D7B3: 8D 07 20  STA $2007
C - - - - - 0x0017C6 00:D7B6: CA        DEX
C - - - - - 0x0017C7 00:D7B7: 10 F8     BPL bra_D7B1
C - - - - - 0x0017C9 00:D7B9: A9 00     LDA #$00
C - - - - - 0x0017CB 00:D7BB: 8D 07 20  STA $2007
C - - - - - 0x0017CE 00:D7BE: A9 24     LDA #$24
C - - - - - 0x0017D0 00:D7C0: 8D 07 20  STA $2007                                                  ; Upload 2 empty spaces to PPU
C - - - - - 0x0017D3 00:D7C3: 8D 07 20  STA $2007
C - - - - - 0x0017D6 00:D7C6: A2 8C     LDX #$8C
C - - - - - 0x0017D8 00:D7C8: 8E 07 20  STX $2007                                                  ; Upload TOP- to PPU
C - - - - - 0x0017DB 00:D7CB: E8        INX
C - - - - - 0x0017DC 00:D7CC: 8E 07 20  STX $2007
C - - - - - 0x0017DF 00:D7CF: A2 04     LDX #$04
bra_D7D1:
C - - - - - 0x0017E1 00:D7D1: B5 0D     LDA ram_000D_hi_score_0000x,X                              ; Upload Top Score to PPU
C - - - - - 0x0017E3 00:D7D3: 8D 07 20  STA $2007
C - - - - - 0x0017E6 00:D7D6: CA        DEX
C - - - - - 0x0017E7 00:D7D7: 10 F8     BPL bra_D7D1
C - - - - - 0x0017E9 00:D7D9: A9 00     LDA #$00
C - - - - - 0x0017EB 00:D7DB: 8D 07 20  STA $2007
C - - - - - 0x0017EE 00:D7DE: A9 24     LDA #$24
C - - - - - 0x0017F0 00:D7E0: 8D 07 20  STA $2007                                                  ; Upload 2 empty spaces to PPU
C - - - - - 0x0017F3 00:D7E3: 8D 07 20  STA $2007
C - - - - - 0x0017F6 00:D7E6: A5 16     LDA ram_0016_game_mode                                     ; If Game Mode is Balloon Trip Mode
C - - - - - 0x0017F8 00:D7E8: D0 6A     BNE bra_D854                                               ; then render RANK
C - - - - - 0x0017FA 00:D7EA: A5 40     LDA ram_0040_2p_flag                                       ; If Single Player
C - - - - - 0x0017FC 00:D7EC: F0 14     BEQ bra_D802                                               ; then don't render Player 2 Score
C - - - - - 0x0017FE 00:D7EE: A9 8F     LDA #$8F                                                   ; Upload II- to PPU
C - - - - - 0x001800 00:D7F0: 8D 07 20  STA $2007
C - - - - - 0x001803 00:D7F3: A2 04     LDX #$04
bra_D7F5:
C - - - - - 0x001805 00:D7F5: B5 08     LDA ram_0008_p2_score_0000x,X                              ; Upload Player 2 Score to PPU
C - - - - - 0x001807 00:D7F7: 8D 07 20  STA $2007
C - - - - - 0x00180A 00:D7FA: CA        DEX
C - - - - - 0x00180B 00:D7FB: 10 F8     BPL bra_D7F5
C - - - - - 0x00180D 00:D7FD: A9 00     LDA #$00
C - - - - - 0x00180F 00:D7FF: 8D 07 20  STA $2007
bra_D802:
C - - - - - 0x001812 00:D802: C6 46     DEC ram_0046_status_bar_update_flag
C - - - - - 0x001814 00:D804: 60        RTS
bra_D805:
C - - - - - 0x001815 00:D805: C6 46     DEC ram_0046_status_bar_update_flag
C - - - - - 0x001817 00:D807: A9 20     LDA #$20
C - - - - - 0x001819 00:D809: 8D 06 20  STA $2006                                                  ; PPUADDR = $2062
C - - - - - 0x00181C 00:D80C: A9 62     LDA #$62                                                   ; GAME OVER Player 1 Status Bar
C - - - - - 0x00181E 00:D80E: 8D 06 20  STA $2006
C - - - - - 0x001821 00:D811: A5 41     LDA ram_0041_p1_lives                                      ; If Player 1 Lives is negative
C - - - - - 0x001823 00:D813: 20 26 D8  JSR sub_D826                                               ; Then upload GAME OVER
C - - - - - 0x001826 00:D816: A5 40     LDA ram_0040_2p_flag                                       ; If Single Player
C - - - - - 0x001828 00:D818: F0 20     BEQ bra_D83A_RTS                                           ; then return
C - - - - - 0x00182A 00:D81A: A9 20     LDA #$20
C - - - - - 0x00182C 00:D81C: 8D 06 20  STA $2006                                                  ; PPUADDR = $2075
C - - - - - 0x00182F 00:D81F: A9 75     LDA #$75                                                   ; GAME OVER Player 2 Status Bar
C - - - - - 0x001831 00:D821: 8D 06 20  STA $2006
C - - - - - 0x001834 00:D824: A5 42     LDA ram_0042_p2_lives                                      ; If Player 2 Lives is negative
sub_D826:
C - - - - - 0x001836 00:D826: 30 13     BMI bra_D83B                                               ; Then upload GAME OVER
bra_D828:
C - - - - - 0x001838 00:D828: 85 50     STA ram_0050_star_anim_ppu_lo
C - - - - - 0x00183A 00:D82A: A2 06     LDX #$06
bra_D82C:
C - - - - - 0x00183C 00:D82C: A9 24     LDA #$24                                                   ; Upload amount of lives to PPU
C - - - - - 0x00183E 00:D82E: E4 50     CPX ram_0050_star_anim_ppu_lo
C - - - - - 0x001840 00:D830: B0 02     BCS bra_D834
C - - - - - 0x001842 00:D832: A9 2A     LDA #$2A
bra_D834:
C - - - - - 0x001844 00:D834: 8D 07 20  STA $2007
C - - - - - 0x001847 00:D837: CA        DEX
C - - - - - 0x001848 00:D838: 10 F2     BPL bra_D82C
bra_D83A_RTS:
C - - - - - 0x00184A 00:D83A: 60        RTS
bra_D83B:
C - - - - - 0x00184B 00:D83B: A5 40     LDA ram_0040_2p_flag                                       ; If Single Player
C - - - - - 0x00184D 00:D83D: F0 E9     BEQ bra_D828                                               ; then go back
C - - - - - 0x00184F 00:D83F: A2 08     LDX #$08                                                   ; 9 characters for text string
bra_D841:
C - - - - - 0x001851 00:D841: BD 4B D8  LDA tbl_D84B_GAME_OVER_TEXT,X                              ; Upload GAME OVER to PPU
C - - - - - 0x001854 00:D844: 8D 07 20  STA $2007
C - - - - - 0x001857 00:D847: CA        DEX
C - - - - - 0x001858 00:D848: 10 F7     BPL bra_D841
C - - - - - 0x00185A 00:D84A: 60        RTS
tbl_D84B_GAME_OVER_TEXT:
- D 2 - - - 0x00185B 00:D84B: 1B        .byte $1B   ; R
- D 2 - - - 0x00185C 00:D84C: 0E        .byte $0E   ; E
- D 2 - - - 0x00185D 00:D84D: 1F        .byte $1F   ; V
- D 2 - - - 0x00185E 00:D84E: 18        .byte $18   ; O
- D 2 - - - 0x00185F 00:D84F: 24        .byte $24   ; 
- D 2 - - - 0x001860 00:D850: 0E        .byte $0E   ; E
- D 2 - - - 0x001861 00:D851: 16        .byte $16   ; M
- D 2 - - - 0x001862 00:D852: 0A        .byte $0A   ; A
- D 2 - - - 0x001863 00:D853: 10        .byte $10   ; G
bra_D854:
C - - - - - 0x001864 00:D854: A0 04     LDY #$04                                                   ; 5 characters for text string
bra_D856:
C - - - - - 0x001866 00:D856: B9 6C D8  LDA tbl_D86C_RANK_TEXT,Y                                   ; Upload RANK- to PPU
C - - - - - 0x001869 00:D859: 8D 07 20  STA $2007
C - - - - - 0x00186C 00:D85C: 88        DEY
C - - - - - 0x00186D 00:D85D: 10 F7     BPL bra_D856
C - - - - - 0x00186F 00:D85F: A5 4A     LDA ram_004A_balloon_trip_rank_x0                          ; Upload Rank Number to PPU
C - - - - - 0x001871 00:D861: 8D 07 20  STA $2007
C - - - - - 0x001874 00:D864: A5 49     LDA ram_0049_balloon_trip_rank_0x
C - - - - - 0x001876 00:D866: 8D 07 20  STA $2007
C - - - - - 0x001879 00:D869: C6 46     DEC ram_0046_status_bar_update_flag
C - - - - - 0x00187B 00:D86B: 60        RTS
tbl_D86C_RANK_TEXT:
- D 2 - - - 0x00187C 00:D86C: FB        .byte $FB   ; -
- D 2 - - - 0x00187D 00:D86D: FA        .byte $FA   ; K
- D 2 - - - 0x00187E 00:D86E: F9        .byte $F9   ; N
- D 2 - - - 0x00187F 00:D86F: F8        .byte $F8   ; A
- D 2 - - - 0x001880 00:D870: F7        .byte $F7   ; R
sub_D871:
C - - - - - 0x001881 00:D871: 85 12     STA ram_0012_temp
C - - - - - 0x001883 00:D873: 86 13     STX ram_0013_temp
C - - - - - 0x001885 00:D875: 84 14     STY ram_0014_temp
C - - - - - 0x001887 00:D877: A2 01     LDX #$01
bra_D879:
C - - - - - 0x001889 00:D879: BD 1A 06  LDA ram_061A,X
C - - - - - 0x00188C 00:D87C: 30 0E     BMI bra_D88C
C - - - - - 0x00188E 00:D87E: CA        DEX
C - - - - - 0x00188F 00:D87F: 10 F8     BPL bra_D879
C - - - - - 0x001891 00:D881: A2 01     LDX #$01
C - - - - - 0x001893 00:D883: AD 19 06  LDA ram_0619
C - - - - - 0x001896 00:D886: CD 18 06  CMP ram_0618
C - - - - - 0x001899 00:D889: 90 01     BCC bra_D88C
C - - - - - 0x00189B 00:D88B: CA        DEX
bra_D88C:
C - - - - - 0x00189C 00:D88C: A9 64     LDA #$64
C - - - - - 0x00189E 00:D88E: 9D 18 06  STA ram_0618,X
C - - - - - 0x0018A1 00:D891: A5 12     LDA ram_0012_temp
C - - - - - 0x0018A3 00:D893: 9D 1A 06  STA ram_061A,X
C - - - - - 0x0018A6 00:D896: A8        TAY
C - - - - - 0x0018A7 00:D897: 8A        TXA
C - - - - - 0x0018A8 00:D898: 0A        ASL
C - - - - - 0x0018A9 00:D899: 0A        ASL
C - - - - - 0x0018AA 00:D89A: 0A        ASL
C - - - - - 0x0018AB 00:D89B: AA        TAX
C - - - - - 0x0018AC 00:D89C: B9 D1 D8  LDA tbl_D8D1,Y
C - - - - - 0x0018AF 00:D89F: 9D F1 02  STA ram_02F1_sprite_3C_tile,X
C - - - - - 0x0018B2 00:D8A2: B9 D7 D8  LDA tbl_D8D7,Y
C - - - - - 0x0018B5 00:D8A5: 9D F5 02  STA ram_02F5_sprite_3D_tile,X
C - - - - - 0x0018B8 00:D8A8: A4 13     LDY ram_0013_temp
C - - - - - 0x0018BA 00:D8AA: B9 9A 00  LDA ram_009A_object_y_pos_int_p1,Y
C - - - - - 0x0018BD 00:D8AD: 38        SEC
C - - - - - 0x0018BE 00:D8AE: E9 08     SBC #$08
C - - - - - 0x0018C0 00:D8B0: 9D F0 02  STA ram_02F0_sprite_3C_y,X
C - - - - - 0x0018C3 00:D8B3: 9D F4 02  STA ram_02F4_sprite_3D_y,X
C - - - - - 0x0018C6 00:D8B6: B9 91 00  LDA ram_0091_object_x_pos_int_p1,Y
C - - - - - 0x0018C9 00:D8B9: 9D F3 02  STA ram_02F3_sprite_3C_x,X
C - - - - - 0x0018CC 00:D8BC: 18        CLC
C - - - - - 0x0018CD 00:D8BD: 69 08     ADC #$08
C - - - - - 0x0018CF 00:D8BF: 9D F7 02  STA ram_02F7_sprite_3D_x,X
C - - - - - 0x0018D2 00:D8C2: A5 3E     LDA ram_003E_score_id_update
C - - - - - 0x0018D4 00:D8C4: 9D F2 02  STA ram_02F2_sprite_3C_attributes,X
C - - - - - 0x0018D7 00:D8C7: 9D F6 02  STA ram_02F6_sprite_3D_attributes,X
C - - - - - 0x0018DA 00:D8CA: A4 14     LDY ram_0014_temp
C - - - - - 0x0018DC 00:D8CC: A6 13     LDX ram_0013_temp
C - - - - - 0x0018DE 00:D8CE: A5 12     LDA ram_0012_temp
C - - - - - 0x0018E0 00:D8D0: 60        RTS
tbl_D8D1:
- - - - - - 0x0018E1 00:D8D1: F4        .byte $F4   ; 
- D 2 - - - 0x0018E2 00:D8D2: F5        .byte $F5   ; 
- D 2 - - - 0x0018E3 00:D8D3: F6        .byte $F6   ; 
- D 2 - - - 0x0018E4 00:D8D4: F7        .byte $F7   ; 
- D 2 - - - 0x0018E5 00:D8D5: F8        .byte $F8   ; 
- D 2 - - - 0x0018E6 00:D8D6: F9        .byte $F9   ; 
tbl_D8D7:
- - - - - - 0x0018E7 00:D8D7: FB        .byte $FB   ; 
- D 2 - - - 0x0018E8 00:D8D8: FB        .byte $FB   ; 
- D 2 - - - 0x0018E9 00:D8D9: FA        .byte $FA   ; 
- D 2 - - - 0x0018EA 00:D8DA: FB        .byte $FB   ; 
- D 2 - - - 0x0018EB 00:D8DB: FB        .byte $FB   ; 
- D 2 - - - 0x0018EC 00:D8DC: FB        .byte $FB   ; 
sub_D8DD:
C - - - - - 0x0018ED 00:D8DD: A2 01     LDX #$01
bra_D8DF:
C - - - - - 0x0018EF 00:D8DF: BD 1A 06  LDA ram_061A,X
C - - - - - 0x0018F2 00:D8E2: 30 17     BMI bra_D8FB
C - - - - - 0x0018F4 00:D8E4: DE 18 06  DEC ram_0618,X
C - - - - - 0x0018F7 00:D8E7: D0 12     BNE bra_D8FB
C - - - - - 0x0018F9 00:D8E9: A9 FF     LDA #$FF
C - - - - - 0x0018FB 00:D8EB: 9D 1A 06  STA ram_061A,X
C - - - - - 0x0018FE 00:D8EE: 8A        TXA
C - - - - - 0x0018FF 00:D8EF: 0A        ASL
C - - - - - 0x001900 00:D8F0: 0A        ASL
C - - - - - 0x001901 00:D8F1: 0A        ASL
C - - - - - 0x001902 00:D8F2: A8        TAY
C - - - - - 0x001903 00:D8F3: A9 F0     LDA #$F0
C - - - - - 0x001905 00:D8F5: 99 F0 02  STA ram_02F0_sprite_3C_y,Y
C - - - - - 0x001908 00:D8F8: 99 F4 02  STA ram_02F4_sprite_3D_y,Y
bra_D8FB:
C - - - - - 0x00190B 00:D8FB: CA        DEX
C - - - - - 0x00190C 00:D8FC: 10 E1     BPL bra_D8DF
C - - - - - 0x00190E 00:D8FE: 60        RTS
sub_D8FF:
C - - - - - 0x00190F 00:D8FF: A2 01     LDX #$01
bra_D901:
C - - - - - 0x001911 00:D901: A9 00     LDA #$00
C - - - - - 0x001913 00:D903: 9D 18 06  STA ram_0618,X
C - - - - - 0x001916 00:D906: A9 FF     LDA #$FF
C - - - - - 0x001918 00:D908: 9D 1A 06  STA ram_061A,X
C - - - - - 0x00191B 00:D90B: CA        DEX
C - - - - - 0x00191C 00:D90C: 10 F3     BPL bra_D901
C - - - - - 0x00191E 00:D90E: 60        RTS
sub_D90F_Upload_Title_Screen:
C - - - - - 0x00191F 00:D90F: 20 46 D2  JSR sub_D246_Clear_PPU
C - - - - - 0x001922 00:D912: 20 0A C1  JSR sub_C10A_Clear_PPU_Mask
C - - - - - 0x001925 00:D915: 20 65 F4  JSR sub_F465_Clear_F_Flag                                  ; Clear Frame Flag
C - - - - - 0x001928 00:D918: 20 FA C0  JSR sub_C0FA_Disable_NMI
C - - - - - 0x00192B 00:D91B: A9 2C     LDA #< tbl_D92C_Title_Screen_Pointers
C - - - - - 0x00192D 00:D91D: 85 1D     STA ram_001D_loading_pointer_1d
C - - - - - 0x00192F 00:D91F: A9 D9     LDA #> tbl_D92C_Title_Screen_Pointers
C - - - - - 0x001931 00:D921: 85 1E     STA ram_001E_loading_pointer_1d
C - - - - - 0x001933 00:D923: 20 97 D4  JSR sub_D497_Upload_Background
C - - - - - 0x001936 00:D926: 20 04 C1  JSR sub_C104_Enable_NMI
C - - - - - 0x001939 00:D929: 4C 15 C1  JMP loc_C115
tbl_D92C_Title_Screen_Pointers:
- D 2 - I - 0x00193C 00:D92C: 30        .byte < tbl_D930_Title_Screen_Data   ; 
- D 2 - I - 0x00193D 00:D92D: D9        .byte > tbl_D930_Title_Screen_Data   ; 
- D 2 - I - 0x00193E 00:D92E: 00        .byte $00   ; End
- D 2 - I - 0x00193F 00:D92F: 00        .byte $00   ; End
tbl_D930_Title_Screen_Data:
- D 2 - I - 0x001940 00:D930: 3F        .dbyt $3F00      ; PPUADDR = $3F00
- D 2 - I - 0x001942 00:D932: 04        .byte $04   ; 4 bytes
- D 2 - I - 0x001943 00:D933: 0F        .byte $0F, $30, $27, $2A   ; Palette
- D 2 - I - 0x001947 00:D937: 3F        .dbyt $3F18      ; PPUADDR = $3F18
- D 2 - I - 0x001949 00:D939: 04        .byte $04   ; 4 bytes
- D 2 - I - 0x00194A 00:D93A: 0F        .byte $0F, $16, $30, $21   ; Palette
- D 2 - I - 0x00194E 00:D93E: 20        .dbyt $207C      ; PPUADDR = $207C
- D 2 - I - 0x001950 00:D940: 21        .byte $21   ; 33 bytes
- D 2 - I - 0x001951 00:D941: F0        .byte $F0, $F1, $24, $24, $24, $24, $E0, $E1, $E1, $E2, $E0, $E1, $E1, $E2, $E0   ; 
- D 2 - I - 0x001960 00:D950: E2        .byte $E2, $24, $E0, $E2, $24, $E0, $E1, $E1, $E2, $E0, $E1, $E1, $E2, $E0, $EC, $24   ; 
- D 2 - I - 0x001970 00:D960: E0        .byte $E0, $E2   ; 
- D 2 - I - 0x001972 00:D962: 20        .dbyt $20A2      ; PPUADDR = $20A2
- D 2 - I - 0x001974 00:D964: 1B        .byte $1B   ; 27 bytes
- D 2 - I - 0x001975 00:D965: E3        .byte $E3, $E3, $E3, $E5, $E3, $E3, $E3, $E5, $E3, $E5, $24   ; 
- D 2 - I - 0x001980 00:D970: E3        .byte $E3, $E5, $24, $E3, $E3, $E3, $E5, $E3, $E3, $E3, $E5, $E3, $E3, $F3, $E3, $E5   ; 
- D 2 - I - 0x001990 00:D980: 20        .dbyt $20C2      ; PPUADDR = $20C2
- D 2 - I - 0x001992 00:D982: 1B        .byte $1B   ; 27 bytes
- D 2 - I - 0x001993 00:D983: E3        .byte $E3, $E4, $E3, $E7, $E3, $E4, $E3, $E5, $E3, $E5, $24, $E3, $E5   ; 
- D 2 - I - 0x0019A0 00:D990: 24        .byte $24, $E3, $E4, $E3, $E5, $E3, $E4, $E3, $E5, $E3, $E3, $E3, $E3, $E5   ; 
- D 2 - I - 0x0019AE 00:D99E: 20        .dbyt $20E2      ; PPUADDR = $20E2
- D 2 - I - 0x0019B0 00:D9A0: 1B        .byte $1B   ; 27 bytes
- D 2 - I - 0x0019B1 00:D9A1: E3        .byte $E3, $E3, $E3, $E2, $E3, $E3, $E3, $E5, $E3, $E5, $24, $E3, $E5, $24, $E3   ; 
- D 2 - I - 0x0019C0 00:D9B0: E3        .byte $E3, $E3, $E5, $E3, $E3, $E3, $E5, $E3, $E3, $E3, $E3, $E5   ; 
- D 2 - I - 0x0019CC 00:D9BC: 21        .dbyt $2102      ; PPUADDR = $2102
- D 2 - I - 0x0019CE 00:D9BE: 1B        .byte $1B   ; 27 bytes
- D 2 - I - 0x0019CF 00:D9BF: E3        .byte $E3   ; 
- D 2 - I - 0x0019D0 00:D9C0: E4        .byte $E4, $E3, $E5, $E3, $F2, $E3, $E5, $E3, $E3, $E2, $E3, $E3, $E2, $E3, $E3, $E3   ; 
- D 2 - I - 0x0019E0 00:D9D0: E5        .byte $E5, $E3, $E3, $E3, $E5, $E3, $F2, $E3, $E3, $E5   ; 
- D 2 - I - 0x0019EA 00:D9DA: 21        .dbyt $2122      ; PPUADDR = $2122
- D 2 - I - 0x0019EC 00:D9DC: 1B        .byte $1B   ; 27 bytes
- D 2 - I - 0x0019ED 00:D9DD: E6        .byte $E6, $E3, $E3   ; 
- D 2 - I - 0x0019F0 00:D9E0: E7        .byte $E7, $EB, $24, $E6, $E7, $E6, $E3, $E7, $E6, $E3, $E7, $E6, $E3, $E3, $E7, $E6   ; 
- D 2 - I - 0x001A00 00:D9F0: E3        .byte $E3, $E3, $E7, $EB, $24, $E6, $E3, $E7   ; 
- D 2 - I - 0x001A08 00:D9F8: 21        .dbyt $214C      ; PPUADDR = $214C
- D 2 - I - 0x001A0A 00:D9FA: 12        .byte $12   ; 18 bytes
- D 2 - I - 0x001A0B 00:D9FB: E0        .byte $E0, $E1, $E1, $E2, $E0   ; 
- D 2 - I - 0x001A10 00:DA00: E2        .byte $E2, $E0, $E1, $E1, $E2, $E8, $24, $E0, $E2, $E0, $E1, $E1, $E2   ; 
- D 2 - I - 0x001A1D 00:DA0D: 21        .dbyt $216C      ; PPUADDR = $216C
- D 2 - I - 0x001A1F 00:DA0F: 12        .byte $12   ; 18 bytes
- D 2 - I - 0x001A20 00:DA10: E3        .byte $E3, $E3, $E3, $E7, $E3, $E5, $E3, $F5, $F6, $E7, $E3, $F3, $E3, $E5, $E6, $E3   ; 
- D 2 - I - 0x001A30 00:DA20: E3        .byte $E3, $E7   ; 
- D 2 - I - 0x001A32 00:DA22: 21        .dbyt $218C      ; PPUADDR = $218C
- D 2 - I - 0x001A34 00:DA24: 12        .byte $12   ; 18 bytes
- D 2 - I - 0x001A35 00:DA25: E3        .byte $E3, $E3, $EF, $24, $E3, $E5, $E3, $24, $24, $24, $E3   ; 
- D 2 - I - 0x001A40 00:DA30: E3        .byte $E3, $E3, $E5, $24, $E3, $E5, $24   ; 
- D 2 - I - 0x001A47 00:DA37: 21        .dbyt $21AC      ; PPUADDR = $21AC
- D 2 - I - 0x001A49 00:DA39: 12        .byte $12   ; 18 bytes
- D 2 - I - 0x001A4A 00:DA3A: E3        .byte $E3, $E3, $E1, $EA, $E3, $E5   ; 
- D 2 - I - 0x001A50 00:DA40: E3        .byte $E3, $E9, $E3, $E2, $E3, $E3, $E3, $E5, $24, $E3, $E5, $24   ; 
- D 2 - I - 0x001A5C 00:DA4C: 21        .dbyt $21CC      ; PPUADDR = $21CC
- D 2 - I - 0x001A5E 00:DA4E: 12        .byte $12   ; 18 bytes
- D 2 - I - 0x001A5F 00:DA4F: E3        .byte $E3   ; 
- D 2 - I - 0x001A60 00:DA50: E3        .byte $E3, $EF, $24, $E3, $E5, $E3, $F3, $E3, $E5, $E3, $F2, $E3, $E5, $24, $E3, $E5   ; 
- D 2 - I - 0x001A70 00:DA60: 24        .byte $24   ; 
- D 2 - I - 0x001A71 00:DA61: 21        .dbyt $21EC      ; PPUADDR = $21EC
- D 2 - I - 0x001A73 00:DA63: 12        .byte $12   ; 18 bytes
- D 2 - I - 0x001A74 00:DA64: E6        .byte $E6, $E7, $24, $24, $E6, $E7, $E6, $E7, $E6, $E7, $EB, $24   ; 
- D 2 - I - 0x001A80 00:DA70: E6        .byte $E6, $E7, $24, $E6, $E7, $24   ; 
- D 2 - I - 0x001A86 00:DA76: 22        .dbyt $2248      ; PPUADDR = $2248
- D 2 - I - 0x001A88 00:DA78: 10        .byte $10   ; 16 bytes
- D 2 - I - 0x001A89 00:DA79: 0A        .byte $0A, $24, $24, $01, $25, $19, $15   ; 
- D 2 - I - 0x001A90 00:DA80: 0A        .byte $0A, $22, $0E, $1B, $24, $10, $0A, $16, $0E   ; 
- D 2 - I - 0x001A99 00:DA89: 22        .dbyt $2288      ; PPUADDR = $2288
- D 2 - I - 0x001A9B 00:DA8B: 10        .byte $10   ; 16 bytes
- D 2 - I - 0x001A9C 00:DA8C: 0B        .byte $0B, $24, $24, $02   ; 
- D 2 - I - 0x001AA0 00:DA90: 25        .byte $25, $19, $15, $0A, $22, $0E, $1B, $24, $10, $0A, $16, $0E   ; 
- D 2 - I - 0x001AAC 00:DA9C: 22        .dbyt $22C8      ; PPUADDR = $22C8
- D 2 - I - 0x001AAE 00:DA9E: 10        .byte $10   ; 16 bytes
- D 2 - I - 0x001AAF 00:DA9F: 0C        .byte $0C   ; 
- D 2 - I - 0x001AB0 00:DAA0: 24        .byte $24, $24, $0B, $0A, $15, $15, $18, $18, $17, $24, $24, $1D, $1B, $12, $19   ; 
- D 2 - I - 0x001ABF 00:DAAF: 23        .dbyt $2349      ; PPUADDR = $2349
- D 2 - I - 0x001AC1 00:DAB1: 0E        .byte $0E   ; 14 bytes
- D 2 - I - 0x001AC2 00:DAB2: F4        .byte $F4, $01, $09, $08, $04, $24, $17, $12, $17, $1D, $0E, $17, $0D, $18   ; 
- D 2 - I - 0x001AD0 00:DAC0: 00        .byte $00   ; End
bra_DAC1_Title_Screen_Loop:
sub_DAC1_Title_Screen_Loop:
C - - - - - 0x001AD1 00:DAC1: 20 04 C1  JSR sub_C104_Enable_NMI
C - - - - - 0x001AD4 00:DAC4: 20 0F D9  JSR sub_D90F_Upload_Title_Screen
C - - - - - 0x001AD7 00:DAC7: A9 00     LDA #$00                                                   ; Reset Frame Counter
C - - - - - 0x001AD9 00:DAC9: 85 19     STA ram_0019_f_counter
loc_DACB:
C D 2 - - - 0x001ADB 00:DACB: 20 65 F4  JSR sub_F465_Clear_F_Flag                                  ; Clear Frame Flag
C - - - - - 0x001ADE 00:DACE: A5 19     LDA ram_0019_f_counter                                     ; If Frame Counter is 0
C - - - - - 0x001AE0 00:DAD0: F0 1F     BEQ bra_DAF1                                               ; Then do demo
C - - - - - 0x001AE2 00:DAD2: 20 08 DB  JSR sub_DB08                                               ; Set Modes & Cursor
C - - - - - 0x001AE5 00:DAD5: 20 68 E7  JSR sub_E768_Poll_Joypad_0
C - - - - - 0x001AE8 00:DAD8: AA        TAX
C - - - - - 0x001AE9 00:DAD9: 29 10     AND #$10                                                   ; If Start button is pressed
C - - - - - 0x001AEB 00:DADB: D0 13     BNE bra_DAF0_RTS                                           ; Then exit Title Screen Loop
C - - - - - 0x001AED 00:DADD: 8A        TXA
C - - - - - 0x001AEE 00:DADE: 29 20     AND #$20                                                   ; If Select button is pressed
C - - - - - 0x001AF0 00:DAE0: F0 0B     BEQ bra_DAED                                               ; Then loop again
C - - - - - 0x001AF2 00:DAE2: A9 00     LDA #$00                                                   ; Reset Frame Counter
C - - - - - 0x001AF4 00:DAE4: 85 19     STA ram_0019_f_counter
C - - - - - 0x001AF6 00:DAE6: A6 3F     LDX ram_003F_main_menu_cursor
C - - - - - 0x001AF8 00:DAE8: BD 05 DB  LDA tbl_DB05_Title_Screen_Choices,X                        ; Select Next Mode
C - - - - - 0x001AFB 00:DAEB: 85 3F     STA ram_003F_main_menu_cursor
bra_DAED:
C - - - - - 0x001AFD 00:DAED: 4C CB DA  JMP loc_DACB                                               ; Loop
bra_DAF0_RTS:
C - - - - - 0x001B00 00:DAF0: 60        RTS
bra_DAF1:
C - - - - - 0x001B01 00:DAF1: E6 3A     INC ram_003A_demo_flag                                     ; Set Demo Flag
C - - - - - 0x001B03 00:DAF3: E6 40     INC ram_0040_2p_flag                                       ; Set to 2 Players
C - - - - - 0x001B05 00:DAF5: A9 00     LDA #$00                                                   ; Disable all Sound Channels
C - - - - - 0x001B07 00:DAF7: 8D 15 40  STA $4015
C - - - - - 0x001B0A 00:DAFA: 85 16     STA ram_0016_game_mode                                     ; Set Game Mode to 00 (Balloon Fight)
C - - - - - 0x001B0C 00:DAFC: 20 F2 F1  JSR sub_F1F2
C - - - - - 0x001B0F 00:DAFF: A9 00     LDA #$00
C - - - - - 0x001B11 00:DB01: 85 3A     STA ram_003A_demo_flag
C - - - - - 0x001B13 00:DB03: F0 BC     BEQ bra_DAC1_Title_Screen_Loop
tbl_DB05_Title_Screen_Choices:
; The current selection is replaced with the next menu option.
- D 2 - - - 0x001B15 00:DB05: 01        .byte $01   ; 2 Player Game
- D 2 - - - 0x001B16 00:DB06: 02        .byte $02   ; Balloon Trip
- D 2 - - - 0x001B17 00:DB07: 00        .byte $00   ; 1 Player Game
sub_DB08:
C - - - - - 0x001B18 00:DB08: A5 3F     LDA ram_003F_main_menu_cursor
C - - - - - 0x001B1A 00:DB0A: 4A        LSR                                                        ; Set Game Mode
C - - - - - 0x001B1B 00:DB0B: 85 16     STA ram_0016_game_mode                                     ; depending on Selected Mode
C - - - - - 0x001B1D 00:DB0D: A5 3F     LDA ram_003F_main_menu_cursor
C - - - - - 0x001B1F 00:DB0F: AA        TAX                                                        ; Set amount of Players
C - - - - - 0x001B20 00:DB10: 29 01     AND #$01                                                   ; depending on Selected Mode
C - - - - - 0x001B22 00:DB12: 85 40     STA ram_0040_2p_flag
C - - - - - 0x001B24 00:DB14: BD 27 DB  LDA tbl_DB27_Title_Screen_Y_Cursor,X                       ; Set Y position of title screen menu cursor balloon
C - - - - - 0x001B27 00:DB17: 8D 7B 05  STA ram_057B_balloon_y_pos
C - - - - - 0x001B2A 00:DB1A: A9 2C     LDA #$2C                                                   ; Set X position of title screen menu cursor balloon
C - - - - - 0x001B2C 00:DB1C: 8D 67 05  STA ram_0567_balloon_x_pos
C - - - - - 0x001B2F 00:DB1F: A2 00     LDX #$00                                                   ; Set graphics of title screen menu cursor balloon
C - - - - - 0x001B31 00:DB21: 8E 5D 05  STX ram_055D_balloon_gfx
C - - - - - 0x001B34 00:DB24: 4C 2F CE  JMP loc_CE2F_Balloon_X_Sprite_Manage
tbl_DB27_Title_Screen_Y_Cursor:
- D 2 - - - 0x001B37 00:DB27: 8C        .byte $8C   ; 1 Player Game
- D 2 - - - 0x001B38 00:DB28: 9C        .byte $9C   ; 2 Player Game
- D 2 - - - 0x001B39 00:DB29: AC        .byte $AC   ; Balloon Trip
tbl_DB2A_Phase_Low_Bytes:
- D 2 - - - 0x001B3A 00:DB2A: 4A        .byte < _off_DB4A_00  ; Phase 01
- D 2 - - - 0x001B3B 00:DB2B: 59        .byte < _off_DB59_01  ; Phase 02
- D 2 - - - 0x001B3C 00:DB2C: F2        .byte < _off_DBF2_02  ; Phase 03
- D 2 - - - 0x001B3D 00:DB2D: 6C        .byte < _off_DB6C_03  ; Bonus Phase 01
- D 2 - - - 0x001B3E 00:DB2E: 77        .byte < _off_DB77_04  ; Phase 04
- D 2 - - - 0x001B3F 00:DB2F: 88        .byte < _off_DB88_05  ; Phase 05
- D 2 - - - 0x001B40 00:DB30: E1        .byte < _off_DBE1_06  ; Phase 06
- D 2 - - - 0x001B41 00:DB31: 6C        .byte < _off_DB6C_07  ; Bonus Phase 02
- D 2 - - - 0x001B42 00:DB32: 99        .byte < _off_DB99_08  ; Phase 07
- D 2 - - - 0x001B43 00:DB33: 03        .byte < _off_DC03_09  ; Phase 08
- D 2 - - - 0x001B44 00:DB34: CA        .byte < _off_DBCA_0A  ; Phase 09
- D 2 - - - 0x001B45 00:DB35: 6C        .byte < _off_DB6C_0B  ; Bonus Phase 03
- D 2 - - - 0x001B46 00:DB36: AD        .byte < _off_DBAD_0C  ; Phase 10
- D 2 - - - 0x001B47 00:DB37: 17        .byte < _off_DC17_0D  ; Phase 11
- D 2 - - - 0x001B48 00:DB38: E1        .byte < _off_DBE1_0E  ; Phase 12
- D 2 - - - 0x001B49 00:DB39: 6C        .byte < _off_DB6C_0F  ; Bonus Phase 04
tbl_DB3A_Phase_High_Bytes:
- D 2 - - - 0x001B4A 00:DB3A: DB        .byte > _off_DB4A_00   ; Phase 01
- D 2 - - - 0x001B4B 00:DB3B: DB        .byte > _off_DB59_01   ; Phase 02
- D 2 - - - 0x001B4C 00:DB3C: DB        .byte > _off_DBF2_02   ; Phase 03
- D 2 - - - 0x001B4D 00:DB3D: DB        .byte > _off_DB6C_03   ; Bonus Phase 01
- D 2 - - - 0x001B4E 00:DB3E: DB        .byte > _off_DB77_04   ; Phase 04
- D 2 - - - 0x001B4F 00:DB3F: DB        .byte > _off_DB88_05   ; Phase 05
- D 2 - - - 0x001B50 00:DB40: DB        .byte > _off_DBE1_06   ; Phase 06
- D 2 - - - 0x001B51 00:DB41: DB        .byte > _off_DB6C_07   ; Bonus Phase 02
- D 2 - - - 0x001B52 00:DB42: DB        .byte > _off_DB99_08   ; Phase 07
- D 2 - - - 0x001B53 00:DB43: DC        .byte > _off_DC03_09   ; Phase 08
- D 2 - - - 0x001B54 00:DB44: DB        .byte > _off_DBCA_0A   ; Phase 09
- D 2 - - - 0x001B55 00:DB45: DB        .byte > _off_DB6C_0B   ; Bonus Phase 03
- D 2 - - - 0x001B56 00:DB46: DB        .byte > _off_DBAD_0C   ; Phase 10
- D 2 - - - 0x001B57 00:DB47: DC        .byte > _off_DC17_0D   ; Phase 11
- D 2 - - - 0x001B58 00:DB48: DB        .byte > _off_DBE1_0E   ; Phase 12
- D 2 - - - 0x001B59 00:DB49: DB        .byte > _off_DB6C_0F   ; Bonus Phase 04
_off_DB4A_00:
- D 2 - I - 0x001B5A 00:DB4A: 2B DC     .word off_DC2B
- D 2 - I - 0x001B5C 00:DB4C: 39 DD     .word off_DD39
- D 2 - I - 0x001B5E 00:DB4E: 00        .byte $00   ; 
- D 2 - I - 0x001B5F 00:DB4F: 00        .byte $00   ; 
- D 2 - I - 0x001B60 00:DB50: 10        .byte $10   ; 
- D 2 - I - 0x001B61 00:DB51: 06        .byte $06   ; 
- D 2 - I - 0x001B62 00:DB52: FF        .byte $FF   ; 
- D 2 - I - 0x001B63 00:DB53: FF        .byte $FF   ; 
- D 2 - I - 0x001B64 00:DB54: 96 DF     .word off_DF96
- D 2 - I - 0x001B66 00:DB56: 02        .byte $02   ; 
- D 2 - I - 0x001B67 00:DB57: 7A DE     .word off_DE7A
_off_DB59_01:
- D 2 - I - 0x001B69 00:DB59: 2B DC     .word off_DC2B
- D 2 - I - 0x001B6B 00:DB5B: 39 DD     .word off_DD39
- D 2 - I - 0x001B6D 00:DB5D: 4B DD     .word off_DD4B
- D 2 - I - 0x001B6F 00:DB5F: 00        .byte $00   ; 
- D 2 - I - 0x001B70 00:DB60: 00        .byte $00   ; 
- D 2 - I - 0x001B71 00:DB61: 18        .byte $18   ; 
- D 2 - I - 0x001B72 00:DB62: 0C        .byte $0C   ; 
- D 2 - I - 0x001B73 00:DB63: 04        .byte $04   ; 
- D 2 - I - 0x001B74 00:DB64: 0E        .byte $0E   ; 
- D 2 - I - 0x001B75 00:DB65: FF        .byte $FF   ; 
- D 2 - I - 0x001B76 00:DB66: FF        .byte $FF   ; 
- D 2 - I - 0x001B77 00:DB67: A0 DF     .word off_DFA0
- D 2 - I - 0x001B79 00:DB69: 04        .byte $04   ; 
- D 2 - I - 0x001B7A 00:DB6A: 86 DE     .word off_DE86
_off_DB6C_03:
_off_DB6C_07:
_off_DB6C_0B:
_off_DB6C_0F:
- D 2 - I - 0x001B7C 00:DB6C: CA DC     .word off_DCCA
- D 2 - I - 0x001B7E 00:DB6E: 00        .byte $00   ; 
- D 2 - I - 0x001B7F 00:DB6F: 00        .byte $00   ; 
- D 2 - I - 0x001B80 00:DB70: FF        .byte $FF   ; 
- D 2 - I - 0x001B81 00:DB71: FF        .byte $FF   ; 
- D 2 - I - 0x001B82 00:DB72: B0 DF     .word off_DFB0
- D 2 - I - 0x001B84 00:DB74: 00        .byte $00   ; 
- D 2 - I - 0x001B85 00:DB75: 9A DE     .word off_DE9A
_off_DB77_04:
- D 2 - I - 0x001B87 00:DB77: 2B DC     .word off_DC2B
- D 2 - I - 0x001B89 00:DB79: 5E DD     .word off_DD5E
- D 2 - I - 0x001B8B 00:DB7B: 00        .byte $00   ; 
- D 2 - I - 0x001B8C 00:DB7C: 00        .byte $00   ; 
- D 2 - I - 0x001B8D 00:DB7D: 08        .byte $08   ; 
- D 2 - I - 0x001B8E 00:DB7E: 06        .byte $06   ; 
- D 2 - I - 0x001B8F 00:DB7F: 18        .byte $18   ; 
- D 2 - I - 0x001B90 00:DB80: 0A        .byte $0A   ; 
- D 2 - I - 0x001B91 00:DB81: FF        .byte $FF   ; 
- D 2 - I - 0x001B92 00:DB82: FF        .byte $FF   ; 
- D 2 - I - 0x001B93 00:DB83: B1 DF     .word off_DFB1
- D 2 - I - 0x001B95 00:DB85: 06        .byte $06   ; 
- D 2 - I - 0x001B96 00:DB86: 9E DE     .word off_DE9E
_off_DB88_05:
- D 2 - I - 0x001B98 00:DB88: 2B DC     .word off_DC2B
- D 2 - I - 0x001B9A 00:DB8A: 7D DD     .word off_DD7D
- D 2 - I - 0x001B9C 00:DB8C: 00        .byte $00   ; 
- D 2 - I - 0x001B9D 00:DB8D: 00        .byte $00   ; 
- D 2 - I - 0x001B9E 00:DB8E: 04        .byte $04   ; 
- D 2 - I - 0x001B9F 00:DB8F: 06        .byte $06   ; 
- D 2 - I - 0x001BA0 00:DB90: 12        .byte $12   ; 
- D 2 - I - 0x001BA1 00:DB91: 08        .byte $08   ; 
- D 2 - I - 0x001BA2 00:DB92: FF        .byte $FF   ; 
- D 2 - I - 0x001BA3 00:DB93: FF        .byte $FF   ; 
- D 2 - I - 0x001BA4 00:DB94: C1 DF     .word off_DFC1
- D 2 - I - 0x001BA6 00:DB96: 07        .byte $07   ; 
- D 2 - I - 0x001BA7 00:DB97: BA DE     .word off_DEBA
_off_DB99_08:
- D 2 - I - 0x001BA9 00:DB99: 2B DC     .word off_DC2B
- D 2 - I - 0x001BAB 00:DB9B: A3 DD     .word off_DDA3
- D 2 - I - 0x001BAD 00:DB9D: 00        .byte $00   ; 
- D 2 - I - 0x001BAE 00:DB9E: 00        .byte $00   ; 
- D 2 - I - 0x001BAF 00:DB9F: 06        .byte $06   ; 
- D 2 - I - 0x001BB0 00:DBA0: 06        .byte $06   ; 
- D 2 - I - 0x001BB1 00:DBA1: 14        .byte $14   ; 
- D 2 - I - 0x001BB2 00:DBA2: 10        .byte $10   ; 
- D 2 - I - 0x001BB3 00:DBA3: FF        .byte $FF   ; 
- D 2 - I - 0x001BB4 00:DBA4: 0F        .byte $0F   ; 
- D 2 - I - 0x001BB5 00:DBA5: 0D        .byte $0D   ; 
- D 2 - I - 0x001BB6 00:DBA6: 01        .byte $01   ; 
- D 2 - I - 0x001BB7 00:DBA7: FF        .byte $FF   ; 
- D 2 - I - 0x001BB8 00:DBA8: D4 DF     .word off_DFD4
- D 2 - I - 0x001BBA 00:DBAA: 09        .byte $09   ; 
- D 2 - I - 0x001BBB 00:DBAB: DA DE     .word off_DEDA
_off_DBAD_0C:
- D 2 - I - 0x001BBD 00:DBAD: 2B DC     .word off_DC2B
- D 2 - I - 0x001BBF 00:DBAF: CE DD     .word off_DDCE
- D 2 - I - 0x001BC1 00:DBB1: 00        .byte $00   ; 
- D 2 - I - 0x001BC2 00:DBB2: 00        .byte $00   ; 
- D 2 - I - 0x001BC3 00:DBB3: 04        .byte $04   ; 
- D 2 - I - 0x001BC4 00:DBB4: 06        .byte $06   ; 
- D 2 - I - 0x001BC5 00:DBB5: 10        .byte $10   ; 
- D 2 - I - 0x001BC6 00:DBB6: 0E        .byte $0E   ; 
- D 2 - I - 0x001BC7 00:DBB7: FF        .byte $FF   ; 
- D 2 - I - 0x001BC8 00:DBB8: 08        .byte $08   ; 
- D 2 - I - 0x001BC9 00:DBB9: 0E        .byte $0E   ; 
- D 2 - I - 0x001BCA 00:DBBA: 03        .byte $03   ; 
- D 2 - I - 0x001BCB 00:DBBB: 0D        .byte $0D   ; 
- D 2 - I - 0x001BCC 00:DBBC: 09        .byte $09   ; 
- D 2 - I - 0x001BCD 00:DBBD: 03        .byte $03   ; 
- D 2 - I - 0x001BCE 00:DBBE: 12        .byte $12   ; 
- D 2 - I - 0x001BCF 00:DBBF: 08        .byte $08   ; 
- D 2 - I - 0x001BD0 00:DBC0: 03        .byte $03   ; 
- D 2 - I - 0x001BD1 00:DBC1: 17        .byte $17   ; 
- D 2 - I - 0x001BD2 00:DBC2: 0D        .byte $0D   ; 
- D 2 - I - 0x001BD3 00:DBC3: 03        .byte $03   ; 
- D 2 - I - 0x001BD4 00:DBC4: FF        .byte $FF   ; 
- D 2 - I - 0x001BD5 00:DBC5: E7 DF     .word off_DFE7
- D 2 - I - 0x001BD7 00:DBC7: 03        .byte $03   ; 
- D 2 - I - 0x001BD8 00:DBC8: 02 DF     .word off_DF02
_off_DBCA_0A:
- D 2 - I - 0x001BDA 00:DBCA: 2B DC     .word off_DC2B
- D 2 - I - 0x001BDC 00:DBCC: 5E DD     .word off_DD5E
- D 2 - I - 0x001BDE 00:DBCE: 00        .byte $00   ; 
- D 2 - I - 0x001BDF 00:DBCF: 00        .byte $00   ; 
- D 2 - I - 0x001BE0 00:DBD0: 10        .byte $10   ; 
- D 2 - I - 0x001BE1 00:DBD1: 06        .byte $06   ; 
- D 2 - I - 0x001BE2 00:DBD2: 1A        .byte $1A   ; 
- D 2 - I - 0x001BE3 00:DBD3: 0C        .byte $0C   ; 
- D 2 - I - 0x001BE4 00:DBD4: FF        .byte $FF   ; 
- D 2 - I - 0x001BE5 00:DBD5: 08        .byte $08   ; 
- D 2 - I - 0x001BE6 00:DBD6: 08        .byte $08   ; 
- D 2 - I - 0x001BE7 00:DBD7: 01        .byte $01   ; 
- D 2 - I - 0x001BE8 00:DBD8: 18        .byte $18   ; 
- D 2 - I - 0x001BE9 00:DBD9: 04        .byte $04   ; 
- D 2 - I - 0x001BEA 00:DBDA: 01        .byte $01   ; 
- D 2 - I - 0x001BEB 00:DBDB: FF        .byte $FF   ; 
- D 2 - I - 0x001BEC 00:DBDC: B1 DF     .word off_DFB1
- D 2 - I - 0x001BEE 00:DBDE: 06        .byte $06   ; 
- D 2 - I - 0x001BEF 00:DBDF: 9E DE     .word off_DE9E
_off_DBE1_06:
_off_DBE1_0E:
- D 2 - I - 0x001BF1 00:DBE1: 2B DC     .word off_DC2B
- D 2 - I - 0x001BF3 00:DBE3: DB DD     .word off_DDDB
- D 2 - I - 0x001BF5 00:DBE5: 00        .byte $00   ; 
- D 2 - I - 0x001BF6 00:DBE6: 00        .byte $00   ; 
- D 2 - I - 0x001BF7 00:DBE7: 0E        .byte $0E   ; 
- D 2 - I - 0x001BF8 00:DBE8: 06        .byte $06   ; 
- D 2 - I - 0x001BF9 00:DBE9: 0C        .byte $0C   ; 
- D 2 - I - 0x001BFA 00:DBEA: 14        .byte $14   ; 
- D 2 - I - 0x001BFB 00:DBEB: FF        .byte $FF   ; 
- D 2 - I - 0x001BFC 00:DBEC: FF        .byte $FF   ; 
- D 2 - I - 0x001BFD 00:DBED: F7 DF     .word off_DFF7
- D 2 - I - 0x001BFF 00:DBEF: 06        .byte $06   ; 
- D 2 - I - 0x001C00 00:DBF0: 12 DF     .word off_DF12
_off_DBF2_02:
- D 2 - I - 0x001C02 00:DBF2: 2B DC     .word off_DC2B
- D 2 - I - 0x001C04 00:DBF4: 01 DE     .word off_DE01
- D 2 - I - 0x001C06 00:DBF6: 00        .byte $00   ; 
- D 2 - I - 0x001C07 00:DBF7: 00        .byte $00   ; 
- D 2 - I - 0x001C08 00:DBF8: 04        .byte $04   ; 
- D 2 - I - 0x001C09 00:DBF9: 08        .byte $08   ; 
- D 2 - I - 0x001C0A 00:DBFA: 16        .byte $16   ; 
- D 2 - I - 0x001C0B 00:DBFB: 10        .byte $10   ; 
- D 2 - I - 0x001C0C 00:DBFC: FF        .byte $FF   ; 
- D 2 - I - 0x001C0D 00:DBFD: FF        .byte $FF   ; 
- D 2 - I - 0x001C0E 00:DBFE: 07 E0     .word off_E007
- D 2 - I - 0x001C10 00:DC00: 09        .byte $09   ; 
- D 2 - I - 0x001C11 00:DC01: 2E DF     .word off_DF2E
_off_DC03_09:
- D 2 - I - 0x001C13 00:DC03: 2B DC     .word off_DC2B
- D 2 - I - 0x001C15 00:DC05: 32 DE     .word off_DE32
- D 2 - I - 0x001C17 00:DC07: 00        .byte $00   ; 
- D 2 - I - 0x001C18 00:DC08: 00        .byte $00   ; 
- D 2 - I - 0x001C19 00:DC09: 04        .byte $04   ; 
- D 2 - I - 0x001C1A 00:DC0A: 10        .byte $10   ; 
- D 2 - I - 0x001C1B 00:DC0B: 18        .byte $18   ; 
- D 2 - I - 0x001C1C 00:DC0C: 10        .byte $10   ; 
- D 2 - I - 0x001C1D 00:DC0D: FF        .byte $FF   ; 
- D 2 - I - 0x001C1E 00:DC0E: 0E        .byte $0E   ; 
- D 2 - I - 0x001C1F 00:DC0F: 06        .byte $06   ; 
- D 2 - I - 0x001C20 00:DC10: 01        .byte $01   ; 
- D 2 - I - 0x001C21 00:DC11: FF        .byte $FF   ; 
- D 2 - I - 0x001C22 00:DC12: 17 E0     .word off_E017
- D 2 - I - 0x001C24 00:DC14: 07        .byte $07   ; 
- D 2 - I - 0x001C25 00:DC15: 56 DF     .word off_DF56
_off_DC17_0D:
- D 2 - I - 0x001C27 00:DC17: 2B DC     .word off_DC2B
- D 2 - I - 0x001C29 00:DC19: 5B DE     .word off_DE5B
- D 2 - I - 0x001C2B 00:DC1B: 00        .byte $00   ; 
- D 2 - I - 0x001C2C 00:DC1C: 00        .byte $00   ; 
- D 2 - I - 0x001C2D 00:DC1D: 04        .byte $04   ; 
- D 2 - I - 0x001C2E 00:DC1E: 08        .byte $08   ; 
- D 2 - I - 0x001C2F 00:DC1F: 0E        .byte $0E   ; 
- D 2 - I - 0x001C30 00:DC20: 10        .byte $10   ; 
- D 2 - I - 0x001C31 00:DC21: FF        .byte $FF   ; 
- D 2 - I - 0x001C32 00:DC22: 10        .byte $10   ; 
- D 2 - I - 0x001C33 00:DC23: 07        .byte $07   ; 
- D 2 - I - 0x001C34 00:DC24: 01        .byte $01   ; 
- D 2 - I - 0x001C35 00:DC25: FF        .byte $FF   ; 
- D 2 - I - 0x001C36 00:DC26: 2A E0     .word off_E02A
- D 2 - I - 0x001C38 00:DC28: 07        .byte $07   ; 
- D 2 - I - 0x001C39 00:DC29: 76 DF     .word off_DF76
off_DC2B:
- D 2 - I - 0x001C3B 00:DC2B: 23        .byte $23   ; 
- D 2 - I - 0x001C3C 00:DC2C: 40        .byte $40   ; 
- D 2 - I - 0x001C3D 00:DC2D: 88        .byte $88   ; 
- D 2 - I - 0x001C3E 00:DC2E: 39        .byte $39   ; 
- D 2 - I - 0x001C3F 00:DC2F: 38        .byte $38   ; 
- D 2 - I - 0x001C40 00:DC30: 39        .byte $39   ; 
- D 2 - I - 0x001C41 00:DC31: 38        .byte $38   ; 
- D 2 - I - 0x001C42 00:DC32: 39        .byte $39   ; 
- D 2 - I - 0x001C43 00:DC33: 38        .byte $38   ; 
- D 2 - I - 0x001C44 00:DC34: 39        .byte $39   ; 
- D 2 - I - 0x001C45 00:DC35: 33        .byte $33   ; 
- D 2 - I - 0x001C46 00:DC36: 24        .byte $24   ; 
- D 2 - I - 0x001C47 00:DC37: 24        .byte $24   ; 
- D 2 - I - 0x001C48 00:DC38: 24        .byte $24   ; 
- D 2 - I - 0x001C49 00:DC39: 24        .byte $24   ; 
- D 2 - I - 0x001C4A 00:DC3A: 24        .byte $24   ; 
- D 2 - I - 0x001C4B 00:DC3B: 24        .byte $24   ; 
- D 2 - I - 0x001C4C 00:DC3C: 24        .byte $24   ; 
- D 2 - I - 0x001C4D 00:DC3D: 24        .byte $24   ; 
- D 2 - I - 0x001C4E 00:DC3E: 24        .byte $24   ; 
- D 2 - I - 0x001C4F 00:DC3F: 24        .byte $24   ; 
- D 2 - I - 0x001C50 00:DC40: 24        .byte $24   ; 
- D 2 - I - 0x001C51 00:DC41: 24        .byte $24   ; 
- D 2 - I - 0x001C52 00:DC42: 24        .byte $24   ; 
- D 2 - I - 0x001C53 00:DC43: 24        .byte $24   ; 
- D 2 - I - 0x001C54 00:DC44: 24        .byte $24   ; 
- D 2 - I - 0x001C55 00:DC45: 24        .byte $24   ; 
- D 2 - I - 0x001C56 00:DC46: 30        .byte $30   ; 
- D 2 - I - 0x001C57 00:DC47: 38        .byte $38   ; 
- D 2 - I - 0x001C58 00:DC48: 39        .byte $39   ; 
- D 2 - I - 0x001C59 00:DC49: 38        .byte $38   ; 
- D 2 - I - 0x001C5A 00:DC4A: 39        .byte $39   ; 
- D 2 - I - 0x001C5B 00:DC4B: 38        .byte $38   ; 
- D 2 - I - 0x001C5C 00:DC4C: 39        .byte $39   ; 
- D 2 - I - 0x001C5D 00:DC4D: 38        .byte $38   ; 
- D 2 - I - 0x001C5E 00:DC4E: 3C        .byte $3C   ; 
- D 2 - I - 0x001C5F 00:DC4F: 3B        .byte $3B   ; 
- D 2 - I - 0x001C60 00:DC50: 3C        .byte $3C   ; 
- D 2 - I - 0x001C61 00:DC51: 3B        .byte $3B   ; 
- D 2 - I - 0x001C62 00:DC52: 3C        .byte $3C   ; 
- D 2 - I - 0x001C63 00:DC53: 3B        .byte $3B   ; 
- D 2 - I - 0x001C64 00:DC54: 3C        .byte $3C   ; 
- D 2 - I - 0x001C65 00:DC55: 3D        .byte $3D   ; 
- D 2 - I - 0x001C66 00:DC56: 58        .byte $58   ; 
- D 2 - I - 0x001C67 00:DC57: 59        .byte $59   ; 
- D 2 - I - 0x001C68 00:DC58: 5A        .byte $5A   ; 
- D 2 - I - 0x001C69 00:DC59: 5B        .byte $5B   ; 
- D 2 - I - 0x001C6A 00:DC5A: 58        .byte $58   ; 
- D 2 - I - 0x001C6B 00:DC5B: 59        .byte $59   ; 
- D 2 - I - 0x001C6C 00:DC5C: 5A        .byte $5A   ; 
- D 2 - I - 0x001C6D 00:DC5D: 5B        .byte $5B   ; 
- D 2 - I - 0x001C6E 00:DC5E: 58        .byte $58   ; 
- D 2 - I - 0x001C6F 00:DC5F: 59        .byte $59   ; 
- D 2 - I - 0x001C70 00:DC60: 5A        .byte $5A   ; 
- D 2 - I - 0x001C71 00:DC61: 5B        .byte $5B   ; 
- D 2 - I - 0x001C72 00:DC62: 58        .byte $58   ; 
- D 2 - I - 0x001C73 00:DC63: 59        .byte $59   ; 
- D 2 - I - 0x001C74 00:DC64: 5A        .byte $5A   ; 
- D 2 - I - 0x001C75 00:DC65: 5B        .byte $5B   ; 
- D 2 - I - 0x001C76 00:DC66: 3A        .byte $3A   ; 
- D 2 - I - 0x001C77 00:DC67: 3B        .byte $3B   ; 
- D 2 - I - 0x001C78 00:DC68: 3C        .byte $3C   ; 
- D 2 - I - 0x001C79 00:DC69: 3B        .byte $3B   ; 
- D 2 - I - 0x001C7A 00:DC6A: 3C        .byte $3C   ; 
- D 2 - I - 0x001C7B 00:DC6B: 3B        .byte $3B   ; 
- D 2 - I - 0x001C7C 00:DC6C: 3C        .byte $3C   ; 
- D 2 - I - 0x001C7D 00:DC6D: 3B        .byte $3B   ; 
- D 2 - I - 0x001C7E 00:DC6E: 60        .byte $60   ; 
- D 2 - I - 0x001C7F 00:DC6F: 61        .byte $61   ; 
- D 2 - I - 0x001C80 00:DC70: 62        .byte $62   ; 
- D 2 - I - 0x001C81 00:DC71: 63        .byte $63   ; 
- D 2 - I - 0x001C82 00:DC72: 60        .byte $60   ; 
- D 2 - I - 0x001C83 00:DC73: 61        .byte $61   ; 
- D 2 - I - 0x001C84 00:DC74: 62        .byte $62   ; 
- D 2 - I - 0x001C85 00:DC75: 63        .byte $63   ; 
- D 2 - I - 0x001C86 00:DC76: 5C        .byte $5C   ; 
- D 2 - I - 0x001C87 00:DC77: 5D        .byte $5D   ; 
- D 2 - I - 0x001C88 00:DC78: 5E        .byte $5E   ; 
- D 2 - I - 0x001C89 00:DC79: 5F        .byte $5F   ; 
- D 2 - I - 0x001C8A 00:DC7A: 5C        .byte $5C   ; 
- D 2 - I - 0x001C8B 00:DC7B: 5D        .byte $5D   ; 
- D 2 - I - 0x001C8C 00:DC7C: 5E        .byte $5E   ; 
- D 2 - I - 0x001C8D 00:DC7D: 5F        .byte $5F   ; 
- D 2 - I - 0x001C8E 00:DC7E: 5C        .byte $5C   ; 
- D 2 - I - 0x001C8F 00:DC7F: 5D        .byte $5D   ; 
- D 2 - I - 0x001C90 00:DC80: 5E        .byte $5E   ; 
- D 2 - I - 0x001C91 00:DC81: 5F        .byte $5F   ; 
- D 2 - I - 0x001C92 00:DC82: 5C        .byte $5C   ; 
- D 2 - I - 0x001C93 00:DC83: 5D        .byte $5D   ; 
- D 2 - I - 0x001C94 00:DC84: 5E        .byte $5E   ; 
- D 2 - I - 0x001C95 00:DC85: 5F        .byte $5F   ; 
- D 2 - I - 0x001C96 00:DC86: 60        .byte $60   ; 
- D 2 - I - 0x001C97 00:DC87: 61        .byte $61   ; 
- D 2 - I - 0x001C98 00:DC88: 62        .byte $62   ; 
- D 2 - I - 0x001C99 00:DC89: 63        .byte $63   ; 
- D 2 - I - 0x001C9A 00:DC8A: 60        .byte $60   ; 
- D 2 - I - 0x001C9B 00:DC8B: 61        .byte $61   ; 
- D 2 - I - 0x001C9C 00:DC8C: 62        .byte $62   ; 
- D 2 - I - 0x001C9D 00:DC8D: 63        .byte $63   ; 
- D 2 - I - 0x001C9E 00:DC8E: 5C        .byte $5C   ; 
- D 2 - I - 0x001C9F 00:DC8F: 5D        .byte $5D   ; 
- D 2 - I - 0x001CA0 00:DC90: 5E        .byte $5E   ; 
- D 2 - I - 0x001CA1 00:DC91: 5F        .byte $5F   ; 
- D 2 - I - 0x001CA2 00:DC92: 5C        .byte $5C   ; 
- D 2 - I - 0x001CA3 00:DC93: 5D        .byte $5D   ; 
- D 2 - I - 0x001CA4 00:DC94: 5E        .byte $5E   ; 
- D 2 - I - 0x001CA5 00:DC95: 5F        .byte $5F   ; 
- D 2 - I - 0x001CA6 00:DC96: 5C        .byte $5C   ; 
- D 2 - I - 0x001CA7 00:DC97: 5D        .byte $5D   ; 
- D 2 - I - 0x001CA8 00:DC98: 5E        .byte $5E   ; 
- D 2 - I - 0x001CA9 00:DC99: 5F        .byte $5F   ; 
- D 2 - I - 0x001CAA 00:DC9A: 5C        .byte $5C   ; 
- D 2 - I - 0x001CAB 00:DC9B: 5D        .byte $5D   ; 
- D 2 - I - 0x001CAC 00:DC9C: 5E        .byte $5E   ; 
- D 2 - I - 0x001CAD 00:DC9D: 5F        .byte $5F   ; 
- D 2 - I - 0x001CAE 00:DC9E: 5C        .byte $5C   ; 
- D 2 - I - 0x001CAF 00:DC9F: 5D        .byte $5D   ; 
- D 2 - I - 0x001CB0 00:DCA0: 5E        .byte $5E   ; 
- D 2 - I - 0x001CB1 00:DCA1: 5F        .byte $5F   ; 
- D 2 - I - 0x001CB2 00:DCA2: 5C        .byte $5C   ; 
- D 2 - I - 0x001CB3 00:DCA3: 5D        .byte $5D   ; 
- D 2 - I - 0x001CB4 00:DCA4: 5E        .byte $5E   ; 
- D 2 - I - 0x001CB5 00:DCA5: 5F        .byte $5F   ; 
- D 2 - I - 0x001CB6 00:DCA6: 5C        .byte $5C   ; 
- D 2 - I - 0x001CB7 00:DCA7: 5D        .byte $5D   ; 
- D 2 - I - 0x001CB8 00:DCA8: 5E        .byte $5E   ; 
- D 2 - I - 0x001CB9 00:DCA9: 5F        .byte $5F   ; 
- D 2 - I - 0x001CBA 00:DCAA: 5C        .byte $5C   ; 
- D 2 - I - 0x001CBB 00:DCAB: 5D        .byte $5D   ; 
- D 2 - I - 0x001CBC 00:DCAC: 5E        .byte $5E   ; 
- D 2 - I - 0x001CBD 00:DCAD: 5F        .byte $5F   ; 
tbl_DCAE:
- D 2 - I - 0x001CBE 00:DCAE: 40        .byte $40   ; 
- D 2 - I - 0x001CBF 00:DCAF: 50        .byte $50   ; 
- D 2 - I - 0x001CC0 00:DCB0: 50        .byte $50   ; 
- D 2 - I - 0x001CC1 00:DCB1: 50        .byte $50   ; 
- D 2 - I - 0x001CC2 00:DCB2: 50        .byte $50   ; 
- D 2 - I - 0x001CC3 00:DCB3: 90        .byte $90   ; 
- D 2 - I - 0x001CC4 00:DCB4: A0        .byte $A0   ; 
- D 2 - I - 0x001CC5 00:DCB5: A0        .byte $A0   ; 
- D 2 - I - 0x001CC6 00:DCB6: 23        .byte $23   ; 
- D 2 - I - 0x001CC7 00:DCB7: F0        .byte $F0   ; 
- D 2 - I - 0x001CC8 00:DCB8: 10        .byte $10   ; 
- D 2 - I - 0x001CC9 00:DCB9: 00        .byte $00   ; 
- D 2 - I - 0x001CCA 00:DCBA: 00        .byte $00   ; 
- D 2 - I - 0x001CCB 00:DCBB: A0        .byte $A0   ; 
- D 2 - I - 0x001CCC 00:DCBC: A0        .byte $A0   ; 
- D 2 - I - 0x001CCD 00:DCBD: A0        .byte $A0   ; 
- D 2 - I - 0x001CCE 00:DCBE: A0        .byte $A0   ; 
- D 2 - I - 0x001CCF 00:DCBF: 00        .byte $00   ; 
- D 2 - I - 0x001CD0 00:DCC0: 00        .byte $00   ; 
- D 2 - I - 0x001CD1 00:DCC1: 0A        .byte $0A   ; 
- D 2 - I - 0x001CD2 00:DCC2: 0A        .byte $0A   ; 
- D 2 - I - 0x001CD3 00:DCC3: 0A        .byte $0A   ; 
- D 2 - I - 0x001CD4 00:DCC4: 0A        .byte $0A   ; 
- D 2 - I - 0x001CD5 00:DCC5: 0A        .byte $0A   ; 
- D 2 - I - 0x001CD6 00:DCC6: 0A        .byte $0A   ; 
- D 2 - I - 0x001CD7 00:DCC7: 0A        .byte $0A   ; 
- D 2 - I - 0x001CD8 00:DCC8: 0A        .byte $0A   ; 
- D 2 - I - 0x001CD9 00:DCC9: 00        .byte $00   ; 
off_DCCA:
- D 2 - I - 0x001CDA 00:DCCA: A3        .byte $A3   ; 
- D 2 - I - 0x001CDB 00:DCCB: 04        .byte $04   ; 
- D 2 - I - 0x001CDC 00:DCCC: 04        .byte $04   ; 
- D 2 - I - 0x001CDD 00:DCCD: 93        .byte $93   ; 
- D 2 - I - 0x001CDE 00:DCCE: 94        .byte $94   ; 
- D 2 - I - 0x001CDF 00:DCCF: 94        .byte $94   ; 
- D 2 - I - 0x001CE0 00:DCD0: 94        .byte $94   ; 
- D 2 - I - 0x001CE1 00:DCD1: A3        .byte $A3   ; 
- D 2 - I - 0x001CE2 00:DCD2: 05        .byte $05   ; 
- D 2 - I - 0x001CE3 00:DCD3: 04        .byte $04   ; 
- D 2 - I - 0x001CE4 00:DCD4: 95        .byte $95   ; 
- D 2 - I - 0x001CE5 00:DCD5: 96        .byte $96   ; 
- D 2 - I - 0x001CE6 00:DCD6: 96        .byte $96   ; 
- D 2 - I - 0x001CE7 00:DCD7: 96        .byte $96   ; 
- D 2 - I - 0x001CE8 00:DCD8: A2        .byte $A2   ; 
- D 2 - I - 0x001CE9 00:DCD9: EA        .byte $EA   ; 
- D 2 - I - 0x001CEA 00:DCDA: 05        .byte $05   ; 
- D 2 - I - 0x001CEB 00:DCDB: 93        .byte $93   ; 
- D 2 - I - 0x001CEC 00:DCDC: 94        .byte $94   ; 
- D 2 - I - 0x001CED 00:DCDD: 94        .byte $94   ; 
- D 2 - I - 0x001CEE 00:DCDE: 94        .byte $94   ; 
- D 2 - I - 0x001CEF 00:DCDF: 94        .byte $94   ; 
- D 2 - I - 0x001CF0 00:DCE0: A2        .byte $A2   ; 
- D 2 - I - 0x001CF1 00:DCE1: EB        .byte $EB   ; 
- D 2 - I - 0x001CF2 00:DCE2: 05        .byte $05   ; 
- D 2 - I - 0x001CF3 00:DCE3: 95        .byte $95   ; 
- D 2 - I - 0x001CF4 00:DCE4: 96        .byte $96   ; 
- D 2 - I - 0x001CF5 00:DCE5: 96        .byte $96   ; 
- D 2 - I - 0x001CF6 00:DCE6: 96        .byte $96   ; 
- D 2 - I - 0x001CF7 00:DCE7: 96        .byte $96   ; 
- D 2 - I - 0x001CF8 00:DCE8: A3        .byte $A3   ; 
- D 2 - I - 0x001CF9 00:DCE9: 34        .byte $34   ; 
- D 2 - I - 0x001CFA 00:DCEA: 03        .byte $03   ; 
- D 2 - I - 0x001CFB 00:DCEB: 93        .byte $93   ; 
- D 2 - I - 0x001CFC 00:DCEC: 94        .byte $94   ; 
- D 2 - I - 0x001CFD 00:DCED: 94        .byte $94   ; 
- D 2 - I - 0x001CFE 00:DCEE: A3        .byte $A3   ; 
- D 2 - I - 0x001CFF 00:DCEF: 35        .byte $35   ; 
- D 2 - I - 0x001D00 00:DCF0: 03        .byte $03   ; 
- D 2 - I - 0x001D01 00:DCF1: 95        .byte $95   ; 
- D 2 - I - 0x001D02 00:DCF2: 96        .byte $96   ; 
- D 2 - I - 0x001D03 00:DCF3: 96        .byte $96   ; 
- D 2 - I - 0x001D04 00:DCF4: A3        .byte $A3   ; 
- D 2 - I - 0x001D05 00:DCF5: 1A        .byte $1A   ; 
- D 2 - I - 0x001D06 00:DCF6: 04        .byte $04   ; 
- D 2 - I - 0x001D07 00:DCF7: 93        .byte $93   ; 
- D 2 - I - 0x001D08 00:DCF8: 94        .byte $94   ; 
- D 2 - I - 0x001D09 00:DCF9: 94        .byte $94   ; 
- D 2 - I - 0x001D0A 00:DCFA: 94        .byte $94   ; 
- D 2 - I - 0x001D0B 00:DCFB: A3        .byte $A3   ; 
- D 2 - I - 0x001D0C 00:DCFC: 1B        .byte $1B   ; 
- D 2 - I - 0x001D0D 00:DCFD: 04        .byte $04   ; 
- D 2 - I - 0x001D0E 00:DCFE: 95        .byte $95   ; 
- D 2 - I - 0x001D0F 00:DCFF: 96        .byte $96   ; 
- D 2 - I - 0x001D10 00:DD00: 96        .byte $96   ; 
- D 2 - I - 0x001D11 00:DD01: 96        .byte $96   ; 
- D 2 - I - 0x001D12 00:DD02: 63        .byte $63   ; 
- D 2 - I - 0x001D13 00:DD03: 80        .byte $80   ; 
- D 2 - I - 0x001D14 00:DD04: 20        .byte $20   ; 
- D 2 - I - 0x001D15 00:DD05: 97        .byte $97   ; 
- D 2 - I - 0x001D16 00:DD06: 23        .byte $23   ; 
- D 2 - I - 0x001D17 00:DD07: A0        .byte $A0   ; 
- D 2 - I - 0x001D18 00:DD08: 20        .byte $20   ; 
- D 2 - I - 0x001D19 00:DD09: 98        .byte $98   ; 
- D 2 - I - 0x001D1A 00:DD0A: 99        .byte $99   ; 
- D 2 - I - 0x001D1B 00:DD0B: 98        .byte $98   ; 
- D 2 - I - 0x001D1C 00:DD0C: 99        .byte $99   ; 
- D 2 - I - 0x001D1D 00:DD0D: 98        .byte $98   ; 
- D 2 - I - 0x001D1E 00:DD0E: 99        .byte $99   ; 
- D 2 - I - 0x001D1F 00:DD0F: 98        .byte $98   ; 
- D 2 - I - 0x001D20 00:DD10: 99        .byte $99   ; 
- D 2 - I - 0x001D21 00:DD11: 98        .byte $98   ; 
- D 2 - I - 0x001D22 00:DD12: 99        .byte $99   ; 
- D 2 - I - 0x001D23 00:DD13: 98        .byte $98   ; 
- D 2 - I - 0x001D24 00:DD14: 99        .byte $99   ; 
- D 2 - I - 0x001D25 00:DD15: 98        .byte $98   ; 
- D 2 - I - 0x001D26 00:DD16: 99        .byte $99   ; 
- D 2 - I - 0x001D27 00:DD17: 98        .byte $98   ; 
- D 2 - I - 0x001D28 00:DD18: 99        .byte $99   ; 
- D 2 - I - 0x001D29 00:DD19: 98        .byte $98   ; 
- D 2 - I - 0x001D2A 00:DD1A: 99        .byte $99   ; 
- D 2 - I - 0x001D2B 00:DD1B: 98        .byte $98   ; 
- D 2 - I - 0x001D2C 00:DD1C: 99        .byte $99   ; 
- D 2 - I - 0x001D2D 00:DD1D: 98        .byte $98   ; 
- D 2 - I - 0x001D2E 00:DD1E: 99        .byte $99   ; 
- D 2 - I - 0x001D2F 00:DD1F: 98        .byte $98   ; 
- D 2 - I - 0x001D30 00:DD20: 99        .byte $99   ; 
- D 2 - I - 0x001D31 00:DD21: 98        .byte $98   ; 
- D 2 - I - 0x001D32 00:DD22: 99        .byte $99   ; 
- D 2 - I - 0x001D33 00:DD23: 98        .byte $98   ; 
- D 2 - I - 0x001D34 00:DD24: 99        .byte $99   ; 
- D 2 - I - 0x001D35 00:DD25: 98        .byte $98   ; 
- D 2 - I - 0x001D36 00:DD26: 99        .byte $99   ; 
- D 2 - I - 0x001D37 00:DD27: 98        .byte $98   ; 
- D 2 - I - 0x001D38 00:DD28: 99        .byte $99   ; 
- D 2 - I - 0x001D39 00:DD29: 23        .byte $23   ; 
- D 2 - I - 0x001D3A 00:DD2A: C0        .byte $C0   ; 
- D 2 - I - 0x001D3B 00:DD2B: 08        .byte $08   ; 
- D 2 - I - 0x001D3C 00:DD2C: 40        .byte $40   ; 
- D 2 - I - 0x001D3D 00:DD2D: 50        .byte $50   ; 
- D 2 - I - 0x001D3E 00:DD2E: 50        .byte $50   ; 
- D 2 - I - 0x001D3F 00:DD2F: 50        .byte $50   ; 
- D 2 - I - 0x001D40 00:DD30: 50        .byte $50   ; 
- D 2 - I - 0x001D41 00:DD31: 90        .byte $90   ; 
- D 2 - I - 0x001D42 00:DD32: A0        .byte $A0   ; 
- D 2 - I - 0x001D43 00:DD33: A0        .byte $A0   ; 
- D 2 - I - 0x001D44 00:DD34: 63        .byte $63   ; 
- D 2 - I - 0x001D45 00:DD35: E8        .byte $E8   ; 
- D 2 - I - 0x001D46 00:DD36: 10        .byte $10   ; 
- D 2 - I - 0x001D47 00:DD37: FF        .byte $FF   ; 
- D 2 - I - 0x001D48 00:DD38: 00        .byte $00   ; 
off_DD39:
- D 2 - I - 0x001D49 00:DD39: 22        .byte $22   ; 
- D 2 - I - 0x001D4A 00:DD3A: 49        .byte $49   ; 
- D 2 - I - 0x001D4B 00:DD3B: 0E        .byte $0E   ; 
- D 2 - I - 0x001D4C 00:DD3C: 30        .byte $30   ; 
- D 2 - I - 0x001D4D 00:DD3D: 31        .byte $31   ; 
- D 2 - I - 0x001D4E 00:DD3E: 32        .byte $32   ; 
- D 2 - I - 0x001D4F 00:DD3F: 31        .byte $31   ; 
- D 2 - I - 0x001D50 00:DD40: 32        .byte $32   ; 
- D 2 - I - 0x001D51 00:DD41: 31        .byte $31   ; 
- D 2 - I - 0x001D52 00:DD42: 32        .byte $32   ; 
- D 2 - I - 0x001D53 00:DD43: 31        .byte $31   ; 
- D 2 - I - 0x001D54 00:DD44: 32        .byte $32   ; 
- D 2 - I - 0x001D55 00:DD45: 31        .byte $31   ; 
- D 2 - I - 0x001D56 00:DD46: 32        .byte $32   ; 
- D 2 - I - 0x001D57 00:DD47: 31        .byte $31   ; 
- D 2 - I - 0x001D58 00:DD48: 32        .byte $32   ; 
- D 2 - I - 0x001D59 00:DD49: 33        .byte $33   ; 
- D 2 - I - 0x001D5A 00:DD4A: 00        .byte $00   ; 
off_DD4B:
- D 2 - I - 0x001D5B 00:DD4B: 21        .byte $21   ; 
- D 2 - I - 0x001D5C 00:DD4C: 57        .byte $57   ; 
- D 2 - I - 0x001D5D 00:DD4D: 06        .byte $06   ; 
- D 2 - I - 0x001D5E 00:DD4E: 30        .byte $30   ; 
- D 2 - I - 0x001D5F 00:DD4F: 31        .byte $31   ; 
- D 2 - I - 0x001D60 00:DD50: 32        .byte $32   ; 
- D 2 - I - 0x001D61 00:DD51: 31        .byte $31   ; 
- D 2 - I - 0x001D62 00:DD52: 32        .byte $32   ; 
- D 2 - I - 0x001D63 00:DD53: 33        .byte $33   ; 
- D 2 - I - 0x001D64 00:DD54: 21        .byte $21   ; 
- D 2 - I - 0x001D65 00:DD55: 65        .byte $65   ; 
- D 2 - I - 0x001D66 00:DD56: 06        .byte $06   ; 
- D 2 - I - 0x001D67 00:DD57: 30        .byte $30   ; 
- D 2 - I - 0x001D68 00:DD58: 31        .byte $31   ; 
- D 2 - I - 0x001D69 00:DD59: 32        .byte $32   ; 
- D 2 - I - 0x001D6A 00:DD5A: 31        .byte $31   ; 
- D 2 - I - 0x001D6B 00:DD5B: 32        .byte $32   ; 
- D 2 - I - 0x001D6C 00:DD5C: 33        .byte $33   ; 
- D 2 - I - 0x001D6D 00:DD5D: 00        .byte $00   ; 
off_DD5E:
- D 2 - I - 0x001D6E 00:DD5E: 21        .byte $21   ; 
- D 2 - I - 0x001D6F 00:DD5F: 90        .byte $90   ; 
- D 2 - I - 0x001D70 00:DD60: 03        .byte $03   ; 
- D 2 - I - 0x001D71 00:DD61: 30        .byte $30   ; 
- D 2 - I - 0x001D72 00:DD62: 31        .byte $31   ; 
- D 2 - I - 0x001D73 00:DD63: 33        .byte $33   ; 
- D 2 - I - 0x001D74 00:DD64: 22        .byte $22   ; 
- D 2 - I - 0x001D75 00:DD65: 26        .byte $26   ; 
- D 2 - I - 0x001D76 00:DD66: 03        .byte $03   ; 
- D 2 - I - 0x001D77 00:DD67: 30        .byte $30   ; 
- D 2 - I - 0x001D78 00:DD68: 31        .byte $31   ; 
- D 2 - I - 0x001D79 00:DD69: 33        .byte $33   ; 
- D 2 - I - 0x001D7A 00:DD6A: 22        .byte $22   ; 
- D 2 - I - 0x001D7B 00:DD6B: 57        .byte $57   ; 
- D 2 - I - 0x001D7C 00:DD6C: 03        .byte $03   ; 
- D 2 - I - 0x001D7D 00:DD6D: 30        .byte $30   ; 
- D 2 - I - 0x001D7E 00:DD6E: 31        .byte $31   ; 
- D 2 - I - 0x001D7F 00:DD6F: 33        .byte $33   ; 
- D 2 - I - 0x001D80 00:DD70: 22        .byte $22   ; 
- D 2 - I - 0x001D81 00:DD71: 6C        .byte $6C   ; 
- D 2 - I - 0x001D82 00:DD72: 03        .byte $03   ; 
- D 2 - I - 0x001D83 00:DD73: 30        .byte $30   ; 
- D 2 - I - 0x001D84 00:DD74: 31        .byte $31   ; 
- D 2 - I - 0x001D85 00:DD75: 33        .byte $33   ; 
- D 2 - I - 0x001D86 00:DD76: 22        .byte $22   ; 
- D 2 - I - 0x001D87 00:DD77: F2        .byte $F2   ; 
- D 2 - I - 0x001D88 00:DD78: 03        .byte $03   ; 
- D 2 - I - 0x001D89 00:DD79: 30        .byte $30   ; 
- D 2 - I - 0x001D8A 00:DD7A: 31        .byte $31   ; 
- D 2 - I - 0x001D8B 00:DD7B: 33        .byte $33   ; 
- D 2 - I - 0x001D8C 00:DD7C: 00        .byte $00   ; 
off_DD7D:
- D 2 - I - 0x001D8D 00:DD7D: 20        .byte $20   ; 
- D 2 - I - 0x001D8E 00:DD7E: CB        .byte $CB   ; 
- D 2 - I - 0x001D8F 00:DD7F: 03        .byte $03   ; 
- D 2 - I - 0x001D90 00:DD80: 30        .byte $30   ; 
- D 2 - I - 0x001D91 00:DD81: 31        .byte $31   ; 
- D 2 - I - 0x001D92 00:DD82: 33        .byte $33   ; 
- D 2 - I - 0x001D93 00:DD83: A1        .byte $A1   ; 
- D 2 - I - 0x001D94 00:DD84: 6D        .byte $6D   ; 
- D 2 - I - 0x001D95 00:DD85: 03        .byte $03   ; 
- D 2 - I - 0x001D96 00:DD86: 3E        .byte $3E   ; 
- D 2 - I - 0x001D97 00:DD87: 3F        .byte $3F   ; 
- D 2 - I - 0x001D98 00:DD88: 40        .byte $40   ; 
- D 2 - I - 0x001D99 00:DD89: A1        .byte $A1   ; 
- D 2 - I - 0x001D9A 00:DD8A: 59        .byte $59   ; 
- D 2 - I - 0x001D9B 00:DD8B: 04        .byte $04   ; 
- D 2 - I - 0x001D9C 00:DD8C: 3E        .byte $3E   ; 
- D 2 - I - 0x001D9D 00:DD8D: 3F        .byte $3F   ; 
- D 2 - I - 0x001D9E 00:DD8E: 3F        .byte $3F   ; 
- D 2 - I - 0x001D9F 00:DD8F: 40        .byte $40   ; 
- D 2 - I - 0x001DA0 00:DD90: A1        .byte $A1   ; 
- D 2 - I - 0x001DA1 00:DD91: A5        .byte $A5   ; 
- D 2 - I - 0x001DA2 00:DD92: 03        .byte $03   ; 
- D 2 - I - 0x001DA3 00:DD93: 3E        .byte $3E   ; 
- D 2 - I - 0x001DA4 00:DD94: 3F        .byte $3F   ; 
- D 2 - I - 0x001DA5 00:DD95: 40        .byte $40   ; 
- D 2 - I - 0x001DA6 00:DD96: 22        .byte $22   ; 
- D 2 - I - 0x001DA7 00:DD97: AA        .byte $AA   ; 
- D 2 - I - 0x001DA8 00:DD98: 03        .byte $03   ; 
- D 2 - I - 0x001DA9 00:DD99: 30        .byte $30   ; 
- D 2 - I - 0x001DAA 00:DD9A: 31        .byte $31   ; 
- D 2 - I - 0x001DAB 00:DD9B: 33        .byte $33   ; 
- D 2 - I - 0x001DAC 00:DD9C: 22        .byte $22   ; 
- D 2 - I - 0x001DAD 00:DD9D: B3        .byte $B3   ; 
- D 2 - I - 0x001DAE 00:DD9E: 03        .byte $03   ; 
- D 2 - I - 0x001DAF 00:DD9F: 30        .byte $30   ; 
- D 2 - I - 0x001DB0 00:DDA0: 31        .byte $31   ; 
- D 2 - I - 0x001DB1 00:DDA1: 33        .byte $33   ; 
- D 2 - I - 0x001DB2 00:DDA2: 00        .byte $00   ; 
off_DDA3:
- D 2 - I - 0x001DB3 00:DDA3: 20        .byte $20   ; 
- D 2 - I - 0x001DB4 00:DDA4: E2        .byte $E2   ; 
- D 2 - I - 0x001DB5 00:DDA5: 02        .byte $02   ; 
- D 2 - I - 0x001DB6 00:DDA6: 30        .byte $30   ; 
- D 2 - I - 0x001DB7 00:DDA7: 33        .byte $33   ; 
- D 2 - I - 0x001DB8 00:DDA8: 20        .byte $20   ; 
- D 2 - I - 0x001DB9 00:DDA9: FB        .byte $FB   ; 
- D 2 - I - 0x001DBA 00:DDAA: 02        .byte $02   ; 
- D 2 - I - 0x001DBB 00:DDAB: 30        .byte $30   ; 
- D 2 - I - 0x001DBC 00:DDAC: 33        .byte $33   ; 
- D 2 - I - 0x001DBD 00:DDAD: 21        .byte $21   ; 
- D 2 - I - 0x001DBE 00:DDAE: 57        .byte $57   ; 
- D 2 - I - 0x001DBF 00:DDAF: 02        .byte $02   ; 
- D 2 - I - 0x001DC0 00:DDB0: 30        .byte $30   ; 
- D 2 - I - 0x001DC1 00:DDB1: 33        .byte $33   ; 
- D 2 - I - 0x001DC2 00:DDB2: 21        .byte $21   ; 
- D 2 - I - 0x001DC3 00:DDB3: 93        .byte $93   ; 
- D 2 - I - 0x001DC4 00:DDB4: 02        .byte $02   ; 
- D 2 - I - 0x001DC5 00:DDB5: 30        .byte $30   ; 
- D 2 - I - 0x001DC6 00:DDB6: 33        .byte $33   ; 
- D 2 - I - 0x001DC7 00:DDB7: 22        .byte $22   ; 
- D 2 - I - 0x001DC8 00:DDB8: 0B        .byte $0B   ; 
- D 2 - I - 0x001DC9 00:DDB9: 02        .byte $02   ; 
- D 2 - I - 0x001DCA 00:DDBA: 30        .byte $30   ; 
- D 2 - I - 0x001DCB 00:DDBB: 33        .byte $33   ; 
- D 2 - I - 0x001DCC 00:DDBC: 22        .byte $22   ; 
- D 2 - I - 0x001DCD 00:DDBD: 47        .byte $47   ; 
- D 2 - I - 0x001DCE 00:DDBE: 02        .byte $02   ; 
- D 2 - I - 0x001DCF 00:DDBF: 30        .byte $30   ; 
- D 2 - I - 0x001DD0 00:DDC0: 33        .byte $33   ; 
- D 2 - I - 0x001DD1 00:DDC1: 22        .byte $22   ; 
- D 2 - I - 0x001DD2 00:DDC2: 83        .byte $83   ; 
- D 2 - I - 0x001DD3 00:DDC3: 02        .byte $02   ; 
- D 2 - I - 0x001DD4 00:DDC4: 30        .byte $30   ; 
- D 2 - I - 0x001DD5 00:DDC5: 33        .byte $33   ; 
- D 2 - I - 0x001DD6 00:DDC6: 22        .byte $22   ; 
- D 2 - I - 0x001DD7 00:DDC7: CF        .byte $CF   ; 
- D 2 - I - 0x001DD8 00:DDC8: 04        .byte $04   ; 
- D 2 - I - 0x001DD9 00:DDC9: 30        .byte $30   ; 
- D 2 - I - 0x001DDA 00:DDCA: 31        .byte $31   ; 
- D 2 - I - 0x001DDB 00:DDCB: 32        .byte $32   ; 
- D 2 - I - 0x001DDC 00:DDCC: 33        .byte $33   ; 
- D 2 - I - 0x001DDD 00:DDCD: 00        .byte $00   ; 
off_DDCE:
- D 2 - I - 0x001DDE 00:DDCE: 22        .byte $22   ; 
- D 2 - I - 0x001DDF 00:DDCF: CA        .byte $CA   ; 
- D 2 - I - 0x001DE0 00:DDD0: 03        .byte $03   ; 
- D 2 - I - 0x001DE1 00:DDD1: 30        .byte $30   ; 
- D 2 - I - 0x001DE2 00:DDD2: 31        .byte $31   ; 
- D 2 - I - 0x001DE3 00:DDD3: 33        .byte $33   ; 
- D 2 - I - 0x001DE4 00:DDD4: 22        .byte $22   ; 
- D 2 - I - 0x001DE5 00:DDD5: D2        .byte $D2   ; 
- D 2 - I - 0x001DE6 00:DDD6: 03        .byte $03   ; 
- D 2 - I - 0x001DE7 00:DDD7: 30        .byte $30   ; 
- D 2 - I - 0x001DE8 00:DDD8: 31        .byte $31   ; 
- D 2 - I - 0x001DE9 00:DDD9: 33        .byte $33   ; 
- D 2 - I - 0x001DEA 00:DDDA: 00        .byte $00   ; 
off_DDDB:
- D 2 - I - 0x001DEB 00:DDDB: 21        .byte $21   ; 
- D 2 - I - 0x001DEC 00:DDDC: 08        .byte $08   ; 
- D 2 - I - 0x001DED 00:DDDD: 04        .byte $04   ; 
- D 2 - I - 0x001DEE 00:DDDE: 30        .byte $30   ; 
- D 2 - I - 0x001DEF 00:DDDF: 31        .byte $31   ; 
- D 2 - I - 0x001DF0 00:DDE0: 32        .byte $32   ; 
- D 2 - I - 0x001DF1 00:DDE1: 33        .byte $33   ; 
- D 2 - I - 0x001DF2 00:DDE2: 21        .byte $21   ; 
- D 2 - I - 0x001DF3 00:DDE3: 14        .byte $14   ; 
- D 2 - I - 0x001DF4 00:DDE4: 04        .byte $04   ; 
- D 2 - I - 0x001DF5 00:DDE5: 30        .byte $30   ; 
- D 2 - I - 0x001DF6 00:DDE6: 31        .byte $31   ; 
- D 2 - I - 0x001DF7 00:DDE7: 32        .byte $32   ; 
- D 2 - I - 0x001DF8 00:DDE8: 33        .byte $33   ; 
- D 2 - I - 0x001DF9 00:DDE9: A1        .byte $A1   ; 
- D 2 - I - 0x001DFA 00:DDEA: A5        .byte $A5   ; 
- D 2 - I - 0x001DFB 00:DDEB: 04        .byte $04   ; 
- D 2 - I - 0x001DFC 00:DDEC: 3E        .byte $3E   ; 
- D 2 - I - 0x001DFD 00:DDED: 3F        .byte $3F   ; 
- D 2 - I - 0x001DFE 00:DDEE: 3F        .byte $3F   ; 
- D 2 - I - 0x001DFF 00:DDEF: 40        .byte $40   ; 
- D 2 - I - 0x001E00 00:DDF0: A1        .byte $A1   ; 
- D 2 - I - 0x001E01 00:DDF1: BA        .byte $BA   ; 
- D 2 - I - 0x001E02 00:DDF2: 04        .byte $04   ; 
- D 2 - I - 0x001E03 00:DDF3: 3E        .byte $3E   ; 
- D 2 - I - 0x001E04 00:DDF4: 3F        .byte $3F   ; 
- D 2 - I - 0x001E05 00:DDF5: 3F        .byte $3F   ; 
- D 2 - I - 0x001E06 00:DDF6: 40        .byte $40   ; 
- D 2 - I - 0x001E07 00:DDF7: 22        .byte $22   ; 
- D 2 - I - 0x001E08 00:DDF8: 6C        .byte $6C   ; 
- D 2 - I - 0x001E09 00:DDF9: 06        .byte $06   ; 
- D 2 - I - 0x001E0A 00:DDFA: 30        .byte $30   ; 
- D 2 - I - 0x001E0B 00:DDFB: 31        .byte $31   ; 
- D 2 - I - 0x001E0C 00:DDFC: 32        .byte $32   ; 
- D 2 - I - 0x001E0D 00:DDFD: 31        .byte $31   ; 
- D 2 - I - 0x001E0E 00:DDFE: 32        .byte $32   ; 
- D 2 - I - 0x001E0F 00:DDFF: 33        .byte $33   ; 
- D 2 - I - 0x001E10 00:DE00: 00        .byte $00   ; 
off_DE01:
- D 2 - I - 0x001E11 00:DE01: 22        .byte $22   ; 
- D 2 - I - 0x001E12 00:DE02: EE        .byte $EE   ; 
- D 2 - I - 0x001E13 00:DE03: 04        .byte $04   ; 
- D 2 - I - 0x001E14 00:DE04: 30        .byte $30   ; 
- D 2 - I - 0x001E15 00:DE05: 31        .byte $31   ; 
- D 2 - I - 0x001E16 00:DE06: 32        .byte $32   ; 
- D 2 - I - 0x001E17 00:DE07: 33        .byte $33   ; 
- D 2 - I - 0x001E18 00:DE08: 20        .byte $20   ; 
- D 2 - I - 0x001E19 00:DE09: F9        .byte $F9   ; 
- D 2 - I - 0x001E1A 00:DE0A: 03        .byte $03   ; 
- D 2 - I - 0x001E1B 00:DE0B: 30        .byte $30   ; 
- D 2 - I - 0x001E1C 00:DE0C: 31        .byte $31   ; 
- D 2 - I - 0x001E1D 00:DE0D: 33        .byte $33   ; 
- D 2 - I - 0x001E1E 00:DE0E: A1        .byte $A1   ; 
- D 2 - I - 0x001E1F 00:DE0F: 1A        .byte $1A   ; 
- D 2 - I - 0x001E20 00:DE10: 03        .byte $03   ; 
- D 2 - I - 0x001E21 00:DE11: 3F        .byte $3F   ; 
- D 2 - I - 0x001E22 00:DE12: 3F        .byte $3F   ; 
- D 2 - I - 0x001E23 00:DE13: 40        .byte $40   ; 
- D 2 - I - 0x001E24 00:DE14: 21        .byte $21   ; 
- D 2 - I - 0x001E25 00:DE15: 90        .byte $90   ; 
- D 2 - I - 0x001E26 00:DE16: 03        .byte $03   ; 
- D 2 - I - 0x001E27 00:DE17: 30        .byte $30   ; 
- D 2 - I - 0x001E28 00:DE18: 31        .byte $31   ; 
- D 2 - I - 0x001E29 00:DE19: 33        .byte $33   ; 
- D 2 - I - 0x001E2A 00:DE1A: A1        .byte $A1   ; 
- D 2 - I - 0x001E2B 00:DE1B: B1        .byte $B1   ; 
- D 2 - I - 0x001E2C 00:DE1C: 03        .byte $03   ; 
- D 2 - I - 0x001E2D 00:DE1D: 3F        .byte $3F   ; 
- D 2 - I - 0x001E2E 00:DE1E: 3F        .byte $3F   ; 
- D 2 - I - 0x001E2F 00:DE1F: 40        .byte $40   ; 
- D 2 - I - 0x001E30 00:DE20: 22        .byte $22   ; 
- D 2 - I - 0x001E31 00:DE21: 28        .byte $28   ; 
- D 2 - I - 0x001E32 00:DE22: 03        .byte $03   ; 
- D 2 - I - 0x001E33 00:DE23: 30        .byte $30   ; 
- D 2 - I - 0x001E34 00:DE24: 31        .byte $31   ; 
- D 2 - I - 0x001E35 00:DE25: 33        .byte $33   ; 
- D 2 - I - 0x001E36 00:DE26: A2        .byte $A2   ; 
- D 2 - I - 0x001E37 00:DE27: 49        .byte $49   ; 
- D 2 - I - 0x001E38 00:DE28: 03        .byte $03   ; 
- D 2 - I - 0x001E39 00:DE29: 3F        .byte $3F   ; 
- D 2 - I - 0x001E3A 00:DE2A: 3F        .byte $3F   ; 
- D 2 - I - 0x001E3B 00:DE2B: 40        .byte $40   ; 
- D 2 - I - 0x001E3C 00:DE2C: 20        .byte $20   ; 
- D 2 - I - 0x001E3D 00:DE2D: EA        .byte $EA   ; 
- D 2 - I - 0x001E3E 00:DE2E: 02        .byte $02   ; 
- D 2 - I - 0x001E3F 00:DE2F: 30        .byte $30   ; 
- D 2 - I - 0x001E40 00:DE30: 33        .byte $33   ; 
- D 2 - I - 0x001E41 00:DE31: 00        .byte $00   ; 
off_DE32:
- D 2 - I - 0x001E42 00:DE32: A2        .byte $A2   ; 
- D 2 - I - 0x001E43 00:DE33: 6C        .byte $6C   ; 
- D 2 - I - 0x001E44 00:DE34: 03        .byte $03   ; 
- D 2 - I - 0x001E45 00:DE35: 3E        .byte $3E   ; 
- D 2 - I - 0x001E46 00:DE36: 3F        .byte $3F   ; 
- D 2 - I - 0x001E47 00:DE37: 40        .byte $40   ; 
- D 2 - I - 0x001E48 00:DE38: A2        .byte $A2   ; 
- D 2 - I - 0x001E49 00:DE39: 73        .byte $73   ; 
- D 2 - I - 0x001E4A 00:DE3A: 03        .byte $03   ; 
- D 2 - I - 0x001E4B 00:DE3B: 3E        .byte $3E   ; 
- D 2 - I - 0x001E4C 00:DE3C: 3F        .byte $3F   ; 
- D 2 - I - 0x001E4D 00:DE3D: 40        .byte $40   ; 
- D 2 - I - 0x001E4E 00:DE3E: 20        .byte $20   ; 
- D 2 - I - 0x001E4F 00:DE3F: E4        .byte $E4   ; 
- D 2 - I - 0x001E50 00:DE40: 04        .byte $04   ; 
- D 2 - I - 0x001E51 00:DE41: 30        .byte $30   ; 
- D 2 - I - 0x001E52 00:DE42: 31        .byte $31   ; 
- D 2 - I - 0x001E53 00:DE43: 32        .byte $32   ; 
- D 2 - I - 0x001E54 00:DE44: 33        .byte $33   ; 
- D 2 - I - 0x001E55 00:DE45: 20        .byte $20   ; 
- D 2 - I - 0x001E56 00:DE46: F8        .byte $F8   ; 
- D 2 - I - 0x001E57 00:DE47: 04        .byte $04   ; 
- D 2 - I - 0x001E58 00:DE48: 30        .byte $30   ; 
- D 2 - I - 0x001E59 00:DE49: 31        .byte $31   ; 
- D 2 - I - 0x001E5A 00:DE4A: 32        .byte $32   ; 
- D 2 - I - 0x001E5B 00:DE4B: 33        .byte $33   ; 
- D 2 - I - 0x001E5C 00:DE4C: 21        .byte $21   ; 
- D 2 - I - 0x001E5D 00:DE4D: A8        .byte $A8   ; 
- D 2 - I - 0x001E5E 00:DE4E: 04        .byte $04   ; 
- D 2 - I - 0x001E5F 00:DE4F: 30        .byte $30   ; 
- D 2 - I - 0x001E60 00:DE50: 31        .byte $31   ; 
- D 2 - I - 0x001E61 00:DE51: 32        .byte $32   ; 
- D 2 - I - 0x001E62 00:DE52: 33        .byte $33   ; 
- D 2 - I - 0x001E63 00:DE53: 21        .byte $21   ; 
- D 2 - I - 0x001E64 00:DE54: B5        .byte $B5   ; 
- D 2 - I - 0x001E65 00:DE55: 04        .byte $04   ; 
- D 2 - I - 0x001E66 00:DE56: 30        .byte $30   ; 
- D 2 - I - 0x001E67 00:DE57: 31        .byte $31   ; 
- D 2 - I - 0x001E68 00:DE58: 32        .byte $32   ; 
- D 2 - I - 0x001E69 00:DE59: 33        .byte $33   ; 
- D 2 - I - 0x001E6A 00:DE5A: 00        .byte $00   ; 
off_DE5B:
- D 2 - I - 0x001E6B 00:DE5B: 22        .byte $22   ; 
- D 2 - I - 0x001E6C 00:DE5C: 64        .byte $64   ; 
- D 2 - I - 0x001E6D 00:DE5D: 02        .byte $02   ; 
- D 2 - I - 0x001E6E 00:DE5E: 30        .byte $30   ; 
- D 2 - I - 0x001E6F 00:DE5F: 33        .byte $33   ; 
- D 2 - I - 0x001E70 00:DE60: 22        .byte $22   ; 
- D 2 - I - 0x001E71 00:DE61: 08        .byte $08   ; 
- D 2 - I - 0x001E72 00:DE62: 02        .byte $02   ; 
- D 2 - I - 0x001E73 00:DE63: 30        .byte $30   ; 
- D 2 - I - 0x001E74 00:DE64: 33        .byte $33   ; 
- D 2 - I - 0x001E75 00:DE65: 21        .byte $21   ; 
- D 2 - I - 0x001E76 00:DE66: AC        .byte $AC   ; 
- D 2 - I - 0x001E77 00:DE67: 02        .byte $02   ; 
- D 2 - I - 0x001E78 00:DE68: 30        .byte $30   ; 
- D 2 - I - 0x001E79 00:DE69: 33        .byte $33   ; 
- D 2 - I - 0x001E7A 00:DE6A: 21        .byte $21   ; 
- D 2 - I - 0x001E7B 00:DE6B: B4        .byte $B4   ; 
- D 2 - I - 0x001E7C 00:DE6C: 02        .byte $02   ; 
- D 2 - I - 0x001E7D 00:DE6D: 30        .byte $30   ; 
- D 2 - I - 0x001E7E 00:DE6E: 33        .byte $33   ; 
- D 2 - I - 0x001E7F 00:DE6F: 22        .byte $22   ; 
- D 2 - I - 0x001E80 00:DE70: 18        .byte $18   ; 
- D 2 - I - 0x001E81 00:DE71: 02        .byte $02   ; 
- D 2 - I - 0x001E82 00:DE72: 30        .byte $30   ; 
- D 2 - I - 0x001E83 00:DE73: 33        .byte $33   ; 
- D 2 - I - 0x001E84 00:DE74: 22        .byte $22   ; 
- D 2 - I - 0x001E85 00:DE75: 7C        .byte $7C   ; 
- D 2 - I - 0x001E86 00:DE76: 02        .byte $02   ; 
- D 2 - I - 0x001E87 00:DE77: 30        .byte $30   ; 
- D 2 - I - 0x001E88 00:DE78: 33        .byte $33   ; 
- D 2 - I - 0x001E89 00:DE79: 00        .byte $00   ; 
off_DE7A:
- D 2 - I - 0x001E8A 00:DE7A: 10        .byte $10   ; 
- D 2 - I - 0x001E8B 00:DE7B: C8        .byte $C8   ; 
- D 2 - I - 0x001E8C 00:DE7C: 48        .byte $48   ; 
- D 2 - I - 0x001E8D 00:DE7D: 38        .byte $38   ; 
- D 2 - I - 0x001E8E 00:DE7E: FF        .byte $FF   ; 
- D 2 - I - 0x001E8F 00:DE7F: B8        .byte $B8   ; 
- D 2 - I - 0x001E90 00:DE80: CF        .byte $CF   ; 
- D 2 - I - 0x001E91 00:DE81: CF        .byte $CF   ; 
- D 2 - I - 0x001E92 00:DE82: 8F        .byte $8F   ; 
- D 2 - I - 0x001E93 00:DE83: E0        .byte $E0   ; 
- D 2 - I - 0x001E94 00:DE84: E0        .byte $E0   ; 
- D 2 - I - 0x001E95 00:DE85: 98        .byte $98   ; 
off_DE86:
- D 2 - I - 0x001E96 00:DE86: 10        .byte $10   ; 
- D 2 - I - 0x001E97 00:DE87: C4        .byte $C4   ; 
- D 2 - I - 0x001E98 00:DE88: 48        .byte $48   ; 
- D 2 - I - 0x001E99 00:DE89: B8        .byte $B8   ; 
- D 2 - I - 0x001E9A 00:DE8A: 28        .byte $28   ; 
- D 2 - I - 0x001E9B 00:DE8B: 3C        .byte $3C   ; 
- D 2 - I - 0x001E9C 00:DE8C: FF        .byte $FF   ; 
- D 2 - I - 0x001E9D 00:DE8D: B8        .byte $B8   ; 
- D 2 - I - 0x001E9E 00:DE8E: E8        .byte $E8   ; 
- D 2 - I - 0x001E9F 00:DE8F: 58        .byte $58   ; 
- D 2 - I - 0x001EA0 00:DE90: CF        .byte $CF   ; 
- D 2 - I - 0x001EA1 00:DE91: CF        .byte $CF   ; 
- D 2 - I - 0x001EA2 00:DE92: 8F        .byte $8F   ; 
- D 2 - I - 0x001EA3 00:DE93: 4F        .byte $4F   ; 
- D 2 - I - 0x001EA4 00:DE94: 57        .byte $57   ; 
- D 2 - I - 0x001EA5 00:DE95: E0        .byte $E0   ; 
- D 2 - I - 0x001EA6 00:DE96: E0        .byte $E0   ; 
- D 2 - I - 0x001EA7 00:DE97: 98        .byte $98   ; 
- D 2 - I - 0x001EA8 00:DE98: 58        .byte $58   ; 
- D 2 - I - 0x001EA9 00:DE99: 60        .byte $60   ; 
off_DE9A:
- D 2 - I - 0x001EAA 00:DE9A: 10        .byte $10   ; 
- D 2 - I - 0x001EAB 00:DE9B: FF        .byte $FF   ; 
- D 2 - I - 0x001EAC 00:DE9C: DF        .byte $DF   ; 
- - - - - - 0x001EAD 00:DE9D: EC        .byte $EC   ; 
off_DE9E:
- D 2 - I - 0x001EAE 00:DE9E: 10        .byte $10   ; 
- D 2 - I - 0x001EAF 00:DE9F: C8        .byte $C8   ; 
- D 2 - I - 0x001EB0 00:DEA0: 80        .byte $80   ; 
- D 2 - I - 0x001EB1 00:DEA1: 30        .byte $30   ; 
- D 2 - I - 0x001EB2 00:DEA2: B8        .byte $B8   ; 
- D 2 - I - 0x001EB3 00:DEA3: 60        .byte $60   ; 
- D 2 - I - 0x001EB4 00:DEA4: 90        .byte $90   ; 
- D 2 - I - 0x001EB5 00:DEA5: 38        .byte $38   ; 
- D 2 - I - 0x001EB6 00:DEA6: FF        .byte $FF   ; 
- D 2 - I - 0x001EB7 00:DEA7: 98        .byte $98   ; 
- D 2 - I - 0x001EB8 00:DEA8: 48        .byte $48   ; 
- D 2 - I - 0x001EB9 00:DEA9: D0        .byte $D0   ; 
- D 2 - I - 0x001EBA 00:DEAA: 78        .byte $78   ; 
- D 2 - I - 0x001EBB 00:DEAB: A8        .byte $A8   ; 
- D 2 - I - 0x001EBC 00:DEAC: CF        .byte $CF   ; 
- D 2 - I - 0x001EBD 00:DEAD: CF        .byte $CF   ; 
- D 2 - I - 0x001EBE 00:DEAE: 5F        .byte $5F   ; 
- D 2 - I - 0x001EBF 00:DEAF: 87        .byte $87   ; 
- D 2 - I - 0x001EC0 00:DEB0: 8F        .byte $8F   ; 
- D 2 - I - 0x001EC1 00:DEB1: 97        .byte $97   ; 
- D 2 - I - 0x001EC2 00:DEB2: B7        .byte $B7   ; 
- D 2 - I - 0x001EC3 00:DEB3: E0        .byte $E0   ; 
- D 2 - I - 0x001EC4 00:DEB4: E0        .byte $E0   ; 
- D 2 - I - 0x001EC5 00:DEB5: 68        .byte $68   ; 
- D 2 - I - 0x001EC6 00:DEB6: 90        .byte $90   ; 
- D 2 - I - 0x001EC7 00:DEB7: 98        .byte $98   ; 
- D 2 - I - 0x001EC8 00:DEB8: A0        .byte $A0   ; 
- D 2 - I - 0x001EC9 00:DEB9: C0        .byte $C0   ; 
off_DEBA:
- D 2 - I - 0x001ECA 00:DEBA: 10        .byte $10   ; 
- D 2 - I - 0x001ECB 00:DEBB: C8        .byte $C8   ; 
- D 2 - I - 0x001ECC 00:DEBC: 58        .byte $58   ; 
- D 2 - I - 0x001ECD 00:DEBD: 68        .byte $68   ; 
- D 2 - I - 0x001ECE 00:DEBE: C8        .byte $C8   ; 
- D 2 - I - 0x001ECF 00:DEBF: 28        .byte $28   ; 
- D 2 - I - 0x001ED0 00:DEC0: 50        .byte $50   ; 
- D 2 - I - 0x001ED1 00:DEC1: 98        .byte $98   ; 
- D 2 - I - 0x001ED2 00:DEC2: 38        .byte $38   ; 
- D 2 - I - 0x001ED3 00:DEC3: FF        .byte $FF   ; 
- D 2 - I - 0x001ED4 00:DEC4: 70        .byte $70   ; 
- D 2 - I - 0x001ED5 00:DEC5: 78        .byte $78   ; 
- D 2 - I - 0x001ED6 00:DEC6: D0        .byte $D0   ; 
- D 2 - I - 0x001ED7 00:DEC7: 30        .byte $30   ; 
- D 2 - I - 0x001ED8 00:DEC8: 68        .byte $68   ; 
- D 2 - I - 0x001ED9 00:DEC9: B0        .byte $B0   ; 
- D 2 - I - 0x001EDA 00:DECA: CF        .byte $CF   ; 
- D 2 - I - 0x001EDB 00:DECB: CF        .byte $CF   ; 
- D 2 - I - 0x001EDC 00:DECC: 2F        .byte $2F   ; 
- D 2 - I - 0x001EDD 00:DECD: 57        .byte $57   ; 
- D 2 - I - 0x001EDE 00:DECE: 4F        .byte $4F   ; 
- D 2 - I - 0x001EDF 00:DECF: 67        .byte $67   ; 
- D 2 - I - 0x001EE0 00:DED0: A7        .byte $A7   ; 
- D 2 - I - 0x001EE1 00:DED1: A7        .byte $A7   ; 
- D 2 - I - 0x001EE2 00:DED2: E0        .byte $E0   ; 
- D 2 - I - 0x001EE3 00:DED3: E0        .byte $E0   ; 
- D 2 - I - 0x001EE4 00:DED4: 38        .byte $38   ; 
- D 2 - I - 0x001EE5 00:DED5: 6C        .byte $6C   ; 
- D 2 - I - 0x001EE6 00:DED6: 6C        .byte $6C   ; 
- D 2 - I - 0x001EE7 00:DED7: 7C        .byte $7C   ; 
- D 2 - I - 0x001EE8 00:DED8: B0        .byte $B0   ; 
- D 2 - I - 0x001EE9 00:DED9: B0        .byte $B0   ; 
off_DEDA:
- D 2 - I - 0x001EEA 00:DEDA: 10        .byte $10   ; 
- D 2 - I - 0x001EEB 00:DEDB: C8        .byte $C8   ; 
- D 2 - I - 0x001EEC 00:DEDC: 12        .byte $12   ; 
- D 2 - I - 0x001EED 00:DEDD: DA        .byte $DA   ; 
- D 2 - I - 0x001EEE 00:DEDE: BA        .byte $BA   ; 
- D 2 - I - 0x001EEF 00:DEDF: 9A        .byte $9A   ; 
- D 2 - I - 0x001EF0 00:DEE0: 5A        .byte $5A   ; 
- D 2 - I - 0x001EF1 00:DEE1: 3A        .byte $3A   ; 
- D 2 - I - 0x001EF2 00:DEE2: 1A        .byte $1A   ; 
- D 2 - I - 0x001EF3 00:DEE3: 7A        .byte $7A   ; 
- D 2 - I - 0x001EF4 00:DEE4: 38        .byte $38   ; 
- D 2 - I - 0x001EF5 00:DEE5: FF        .byte $FF   ; 
- D 2 - I - 0x001EF6 00:DEE6: 1E        .byte $1E   ; 
- D 2 - I - 0x001EF7 00:DEE7: E6        .byte $E6   ; 
- D 2 - I - 0x001EF8 00:DEE8: C6        .byte $C6   ; 
- D 2 - I - 0x001EF9 00:DEE9: A6        .byte $A6   ; 
- D 2 - I - 0x001EFA 00:DEEA: 66        .byte $66   ; 
- D 2 - I - 0x001EFB 00:DEEB: 46        .byte $46   ; 
- D 2 - I - 0x001EFC 00:DEEC: 26        .byte $26   ; 
- D 2 - I - 0x001EFD 00:DEED: 96        .byte $96   ; 
- D 2 - I - 0x001EFE 00:DEEE: CF        .byte $CF   ; 
- D 2 - I - 0x001EFF 00:DEEF: CF        .byte $CF   ; 
- D 2 - I - 0x001F00 00:DEF0: 37        .byte $37   ; 
- D 2 - I - 0x001F01 00:DEF1: 37        .byte $37   ; 
- D 2 - I - 0x001F02 00:DEF2: 4F        .byte $4F   ; 
- D 2 - I - 0x001F03 00:DEF3: 5F        .byte $5F   ; 
- D 2 - I - 0x001F04 00:DEF4: 7F        .byte $7F   ; 
- D 2 - I - 0x001F05 00:DEF5: 8F        .byte $8F   ; 
- D 2 - I - 0x001F06 00:DEF6: 9F        .byte $9F   ; 
- D 2 - I - 0x001F07 00:DEF7: AF        .byte $AF   ; 
- D 2 - I - 0x001F08 00:DEF8: E0        .byte $E0   ; 
- D 2 - I - 0x001F09 00:DEF9: E0        .byte $E0   ; 
- D 2 - I - 0x001F0A 00:DEFA: 40        .byte $40   ; 
- D 2 - I - 0x001F0B 00:DEFB: 40        .byte $40   ; 
- D 2 - I - 0x001F0C 00:DEFC: 58        .byte $58   ; 
- D 2 - I - 0x001F0D 00:DEFD: 68        .byte $68   ; 
- D 2 - I - 0x001F0E 00:DEFE: 88        .byte $88   ; 
- D 2 - I - 0x001F0F 00:DEFF: 98        .byte $98   ; 
- D 2 - I - 0x001F10 00:DF00: A8        .byte $A8   ; 
- D 2 - I - 0x001F11 00:DF01: B8        .byte $B8   ; 
off_DF02:
- D 2 - I - 0x001F12 00:DF02: 10        .byte $10   ; 
- D 2 - I - 0x001F13 00:DF03: C8        .byte $C8   ; 
- D 2 - I - 0x001F14 00:DF04: 52        .byte $52   ; 
- D 2 - I - 0x001F15 00:DF05: 92        .byte $92   ; 
- D 2 - I - 0x001F16 00:DF06: 38        .byte $38   ; 
- D 2 - I - 0x001F17 00:DF07: FF        .byte $FF   ; 
- D 2 - I - 0x001F18 00:DF08: 66        .byte $66   ; 
- D 2 - I - 0x001F19 00:DF09: A6        .byte $A6   ; 
- D 2 - I - 0x001F1A 00:DF0A: CF        .byte $CF   ; 
- D 2 - I - 0x001F1B 00:DF0B: CF        .byte $CF   ; 
- D 2 - I - 0x001F1C 00:DF0C: AF        .byte $AF   ; 
- D 2 - I - 0x001F1D 00:DF0D: AF        .byte $AF   ; 
- D 2 - I - 0x001F1E 00:DF0E: E0        .byte $E0   ; 
- D 2 - I - 0x001F1F 00:DF0F: E0        .byte $E0   ; 
- D 2 - I - 0x001F20 00:DF10: B8        .byte $B8   ; 
- D 2 - I - 0x001F21 00:DF11: B8        .byte $B8   ; 
off_DF12:
- D 2 - I - 0x001F22 00:DF12: 10        .byte $10   ; 
- D 2 - I - 0x001F23 00:DF13: C8        .byte $C8   ; 
- D 2 - I - 0x001F24 00:DF14: 40        .byte $40   ; 
- D 2 - I - 0x001F25 00:DF15: A0        .byte $A0   ; 
- D 2 - I - 0x001F26 00:DF16: 28        .byte $28   ; 
- D 2 - I - 0x001F27 00:DF17: D0        .byte $D0   ; 
- D 2 - I - 0x001F28 00:DF18: 60        .byte $60   ; 
- D 2 - I - 0x001F29 00:DF19: 38        .byte $38   ; 
- D 2 - I - 0x001F2A 00:DF1A: FF        .byte $FF   ; 
- D 2 - I - 0x001F2B 00:DF1B: 60        .byte $60   ; 
- D 2 - I - 0x001F2C 00:DF1C: C0        .byte $C0   ; 
- D 2 - I - 0x001F2D 00:DF1D: 30        .byte $30   ; 
- D 2 - I - 0x001F2E 00:DF1E: D8        .byte $D8   ; 
- D 2 - I - 0x001F2F 00:DF1F: 90        .byte $90   ; 
- D 2 - I - 0x001F30 00:DF20: CF        .byte $CF   ; 
- D 2 - I - 0x001F31 00:DF21: CF        .byte $CF   ; 
- D 2 - I - 0x001F32 00:DF22: 3F        .byte $3F   ; 
- D 2 - I - 0x001F33 00:DF23: 3F        .byte $3F   ; 
- D 2 - I - 0x001F34 00:DF24: 67        .byte $67   ; 
- D 2 - I - 0x001F35 00:DF25: 67        .byte $67   ; 
- D 2 - I - 0x001F36 00:DF26: 97        .byte $97   ; 
- D 2 - I - 0x001F37 00:DF27: E0        .byte $E0   ; 
- D 2 - I - 0x001F38 00:DF28: E0        .byte $E0   ; 
- D 2 - I - 0x001F39 00:DF29: 48        .byte $48   ; 
- D 2 - I - 0x001F3A 00:DF2A: 48        .byte $48   ; 
- D 2 - I - 0x001F3B 00:DF2B: 88        .byte $88   ; 
- D 2 - I - 0x001F3C 00:DF2C: 88        .byte $88   ; 
- D 2 - I - 0x001F3D 00:DF2D: A0        .byte $A0   ; 
off_DF2E:
- D 2 - I - 0x001F3E 00:DF2E: 10        .byte $10   ; 
- D 2 - I - 0x001F3F 00:DF2F: C8        .byte $C8   ; 
- D 2 - I - 0x001F40 00:DF30: 50        .byte $50   ; 
- D 2 - I - 0x001F41 00:DF31: C8        .byte $C8   ; 
- D 2 - I - 0x001F42 00:DF32: 88        .byte $88   ; 
- D 2 - I - 0x001F43 00:DF33: 40        .byte $40   ; 
- D 2 - I - 0x001F44 00:DF34: 70        .byte $70   ; 
- D 2 - I - 0x001F45 00:DF35: D0        .byte $D0   ; 
- D 2 - I - 0x001F46 00:DF36: 88        .byte $88   ; 
- D 2 - I - 0x001F47 00:DF37: 48        .byte $48   ; 
- D 2 - I - 0x001F48 00:DF38: 38        .byte $38   ; 
- D 2 - I - 0x001F49 00:DF39: FF        .byte $FF   ; 
- D 2 - I - 0x001F4A 00:DF3A: 60        .byte $60   ; 
- D 2 - I - 0x001F4B 00:DF3B: E0        .byte $E0   ; 
- D 2 - I - 0x001F4C 00:DF3C: 98        .byte $98   ; 
- D 2 - I - 0x001F4D 00:DF3D: 58        .byte $58   ; 
- D 2 - I - 0x001F4E 00:DF3E: 90        .byte $90   ; 
- D 2 - I - 0x001F4F 00:DF3F: D8        .byte $D8   ; 
- D 2 - I - 0x001F50 00:DF40: 90        .byte $90   ; 
- D 2 - I - 0x001F51 00:DF41: 50        .byte $50   ; 
- D 2 - I - 0x001F52 00:DF42: CF        .byte $CF   ; 
- D 2 - I - 0x001F53 00:DF43: CF        .byte $CF   ; 
- D 2 - I - 0x001F54 00:DF44: 37        .byte $37   ; 
- D 2 - I - 0x001F55 00:DF45: 37        .byte $37   ; 
- D 2 - I - 0x001F56 00:DF46: 5F        .byte $5F   ; 
- D 2 - I - 0x001F57 00:DF47: 87        .byte $87   ; 
- D 2 - I - 0x001F58 00:DF48: B7        .byte $B7   ; 
- D 2 - I - 0x001F59 00:DF49: 3C        .byte $3C   ; 
- D 2 - I - 0x001F5A 00:DF4A: 64        .byte $64   ; 
- D 2 - I - 0x001F5B 00:DF4B: 8C        .byte $8C   ; 
- D 2 - I - 0x001F5C 00:DF4C: E0        .byte $E0   ; 
- D 2 - I - 0x001F5D 00:DF4D: E0        .byte $E0   ; 
- D 2 - I - 0x001F5E 00:DF4E: 40        .byte $40   ; 
- D 2 - I - 0x001F5F 00:DF4F: 40        .byte $40   ; 
- D 2 - I - 0x001F60 00:DF50: 68        .byte $68   ; 
- D 2 - I - 0x001F61 00:DF51: 90        .byte $90   ; 
- D 2 - I - 0x001F62 00:DF52: C0        .byte $C0   ; 
- D 2 - I - 0x001F63 00:DF53: 60        .byte $60   ; 
- D 2 - I - 0x001F64 00:DF54: 80        .byte $80   ; 
- D 2 - I - 0x001F65 00:DF55: A8        .byte $A8   ; 
off_DF56:
- D 2 - I - 0x001F66 00:DF56: 10        .byte $10   ; 
- D 2 - I - 0x001F67 00:DF57: C8        .byte $C8   ; 
- D 2 - I - 0x001F68 00:DF58: 20        .byte $20   ; 
- D 2 - I - 0x001F69 00:DF59: C0        .byte $C0   ; 
- D 2 - I - 0x001F6A 00:DF5A: 40        .byte $40   ; 
- D 2 - I - 0x001F6B 00:DF5B: A8        .byte $A8   ; 
- D 2 - I - 0x001F6C 00:DF5C: 60        .byte $60   ; 
- D 2 - I - 0x001F6D 00:DF5D: 98        .byte $98   ; 
- D 2 - I - 0x001F6E 00:DF5E: 38        .byte $38   ; 
- D 2 - I - 0x001F6F 00:DF5F: FF        .byte $FF   ; 
- D 2 - I - 0x001F70 00:DF60: 40        .byte $40   ; 
- D 2 - I - 0x001F71 00:DF61: E0        .byte $E0   ; 
- D 2 - I - 0x001F72 00:DF62: 60        .byte $60   ; 
- D 2 - I - 0x001F73 00:DF63: C8        .byte $C8   ; 
- D 2 - I - 0x001F74 00:DF64: 68        .byte $68   ; 
- D 2 - I - 0x001F75 00:DF65: A0        .byte $A0   ; 
- D 2 - I - 0x001F76 00:DF66: CF        .byte $CF   ; 
- D 2 - I - 0x001F77 00:DF67: CF        .byte $CF   ; 
- D 2 - I - 0x001F78 00:DF68: 37        .byte $37   ; 
- D 2 - I - 0x001F79 00:DF69: 37        .byte $37   ; 
- D 2 - I - 0x001F7A 00:DF6A: 67        .byte $67   ; 
- D 2 - I - 0x001F7B 00:DF6B: 67        .byte $67   ; 
- D 2 - I - 0x001F7C 00:DF6C: 97        .byte $97   ; 
- D 2 - I - 0x001F7D 00:DF6D: 97        .byte $97   ; 
- D 2 - I - 0x001F7E 00:DF6E: E0        .byte $E0   ; 
- D 2 - I - 0x001F7F 00:DF6F: E0        .byte $E0   ; 
- D 2 - I - 0x001F80 00:DF70: 40        .byte $40   ; 
- D 2 - I - 0x001F81 00:DF71: 40        .byte $40   ; 
- D 2 - I - 0x001F82 00:DF72: 70        .byte $70   ; 
- D 2 - I - 0x001F83 00:DF73: 70        .byte $70   ; 
- D 2 - I - 0x001F84 00:DF74: B0        .byte $B0   ; 
- D 2 - I - 0x001F85 00:DF75: B0        .byte $B0   ; 
off_DF76:
- D 2 - I - 0x001F86 00:DF76: 10        .byte $10   ; 
- D 2 - I - 0x001F87 00:DF77: C8        .byte $C8   ; 
- D 2 - I - 0x001F88 00:DF78: 20        .byte $20   ; 
- D 2 - I - 0x001F89 00:DF79: 40        .byte $40   ; 
- D 2 - I - 0x001F8A 00:DF7A: 60        .byte $60   ; 
- D 2 - I - 0x001F8B 00:DF7B: A0        .byte $A0   ; 
- D 2 - I - 0x001F8C 00:DF7C: C0        .byte $C0   ; 
- D 2 - I - 0x001F8D 00:DF7D: E0        .byte $E0   ; 
- D 2 - I - 0x001F8E 00:DF7E: 38        .byte $38   ; 
- D 2 - I - 0x001F8F 00:DF7F: FF        .byte $FF   ; 
- D 2 - I - 0x001F90 00:DF80: 30        .byte $30   ; 
- D 2 - I - 0x001F91 00:DF81: 50        .byte $50   ; 
- D 2 - I - 0x001F92 00:DF82: 70        .byte $70   ; 
- D 2 - I - 0x001F93 00:DF83: B0        .byte $B0   ; 
- D 2 - I - 0x001F94 00:DF84: D0        .byte $D0   ; 
- D 2 - I - 0x001F95 00:DF85: F0        .byte $F0   ; 
- D 2 - I - 0x001F96 00:DF86: CF        .byte $CF   ; 
- D 2 - I - 0x001F97 00:DF87: CF        .byte $CF   ; 
- D 2 - I - 0x001F98 00:DF88: 97        .byte $97   ; 
- D 2 - I - 0x001F99 00:DF89: 7F        .byte $7F   ; 
- D 2 - I - 0x001F9A 00:DF8A: 67        .byte $67   ; 
- D 2 - I - 0x001F9B 00:DF8B: 67        .byte $67   ; 
- D 2 - I - 0x001F9C 00:DF8C: 7F        .byte $7F   ; 
- D 2 - I - 0x001F9D 00:DF8D: 97        .byte $97   ; 
- D 2 - I - 0x001F9E 00:DF8E: E0        .byte $E0   ; 
- D 2 - I - 0x001F9F 00:DF8F: E0        .byte $E0   ; 
- D 2 - I - 0x001FA0 00:DF90: A0        .byte $A0   ; 
- D 2 - I - 0x001FA1 00:DF91: 88        .byte $88   ; 
- D 2 - I - 0x001FA2 00:DF92: 70        .byte $70   ; 
- D 2 - I - 0x001FA3 00:DF93: 70        .byte $70   ; 
- D 2 - I - 0x001FA4 00:DF94: 88        .byte $88   ; 
- D 2 - I - 0x001FA5 00:DF95: A0        .byte $A0   ; 
off_DF96:
- D 2 - I - 0x001FA6 00:DF96: 03        .byte $03   ; 
- D 2 - I - 0x001FA7 00:DF97: 58        .byte $58   ; 
- D 2 - I - 0x001FA8 00:DF98: 78        .byte $78   ; 
- D 2 - I - 0x001FA9 00:DF99: 04        .byte $04   ; 
- D 2 - I - 0x001FAA 00:DF9A: 78        .byte $78   ; 
- D 2 - I - 0x001FAB 00:DF9B: 78        .byte $78   ; 
- D 2 - I - 0x001FAC 00:DF9C: 04        .byte $04   ; 
- D 2 - I - 0x001FAD 00:DF9D: 98        .byte $98   ; 
- D 2 - I - 0x001FAE 00:DF9E: 78        .byte $78   ; 
- D 2 - I - 0x001FAF 00:DF9F: 04        .byte $04   ; 
off_DFA0:
- D 2 - I - 0x001FB0 00:DFA0: 05        .byte $05   ; 
- D 2 - I - 0x001FB1 00:DFA1: C8        .byte $C8   ; 
- D 2 - I - 0x001FB2 00:DFA2: 38        .byte $38   ; 
- D 2 - I - 0x001FB3 00:DFA3: 05        .byte $05   ; 
- D 2 - I - 0x001FB4 00:DFA4: 38        .byte $38   ; 
- D 2 - I - 0x001FB5 00:DFA5: 40        .byte $40   ; 
- D 2 - I - 0x001FB6 00:DFA6: 05        .byte $05   ; 
- D 2 - I - 0x001FB7 00:DFA7: 58        .byte $58   ; 
- D 2 - I - 0x001FB8 00:DFA8: 78        .byte $78   ; 
- D 2 - I - 0x001FB9 00:DFA9: 04        .byte $04   ; 
- D 2 - I - 0x001FBA 00:DFAA: 78        .byte $78   ; 
- D 2 - I - 0x001FBB 00:DFAB: 78        .byte $78   ; 
- D 2 - I - 0x001FBC 00:DFAC: 04        .byte $04   ; 
- D 2 - I - 0x001FBD 00:DFAD: 98        .byte $98   ; 
- D 2 - I - 0x001FBE 00:DFAE: 78        .byte $78   ; 
- D 2 - I - 0x001FBF 00:DFAF: 04        .byte $04   ; 
off_DFB0:
- D 2 - I - 0x001FC0 00:DFB0: 00        .byte $00   ; 
off_DFB1:
- D 2 - I - 0x001FC1 00:DFB1: 05        .byte $05   ; 
- D 2 - I - 0x001FC2 00:DFB2: 94        .byte $94   ; 
- D 2 - I - 0x001FC3 00:DFB3: A0        .byte $A0   ; 
- D 2 - I - 0x001FC4 00:DFB4: 06        .byte $06   ; 
- D 2 - I - 0x001FC5 00:DFB5: 84        .byte $84   ; 
- D 2 - I - 0x001FC6 00:DFB6: 48        .byte $48   ; 
- D 2 - I - 0x001FC7 00:DFB7: 05        .byte $05   ; 
- D 2 - I - 0x001FC8 00:DFB8: 34        .byte $34   ; 
- D 2 - I - 0x001FC9 00:DFB9: 70        .byte $70   ; 
- D 2 - I - 0x001FCA 00:DFBA: 04        .byte $04   ; 
- D 2 - I - 0x001FCB 00:DFBB: 64        .byte $64   ; 
- D 2 - I - 0x001FCC 00:DFBC: 80        .byte $80   ; 
- D 2 - I - 0x001FCD 00:DFBD: 04        .byte $04   ; 
- D 2 - I - 0x001FCE 00:DFBE: BC        .byte $BC   ; 
- D 2 - I - 0x001FCF 00:DFBF: 78        .byte $78   ; 
- D 2 - I - 0x001FD0 00:DFC0: 04        .byte $04   ; 
off_DFC1:
- D 2 - I - 0x001FD1 00:DFC1: 06        .byte $06   ; 
- D 2 - I - 0x001FD2 00:DFC2: 5C        .byte $5C   ; 
- D 2 - I - 0x001FD3 00:DFC3: 18        .byte $18   ; 
- D 2 - I - 0x001FD4 00:DFC4: 06        .byte $06   ; 
- D 2 - I - 0x001FD5 00:DFC5: 24        .byte $24   ; 
- D 2 - I - 0x001FD6 00:DFC6: 50        .byte $50   ; 
- D 2 - I - 0x001FD7 00:DFC7: 05        .byte $05   ; 
- D 2 - I - 0x001FD8 00:DFC8: 64        .byte $64   ; 
- D 2 - I - 0x001FD9 00:DFC9: 40        .byte $40   ; 
- D 2 - I - 0x001FDA 00:DFCA: 05        .byte $05   ; 
- D 2 - I - 0x001FDB 00:DFCB: C4        .byte $C4   ; 
- D 2 - I - 0x001FDC 00:DFCC: 38        .byte $38   ; 
- D 2 - I - 0x001FDD 00:DFCD: 05        .byte $05   ; 
- D 2 - I - 0x001FDE 00:DFCE: 54        .byte $54   ; 
- D 2 - I - 0x001FDF 00:DFCF: 90        .byte $90   ; 
- D 2 - I - 0x001FE0 00:DFD0: 04        .byte $04   ; 
- D 2 - I - 0x001FE1 00:DFD1: 9C        .byte $9C   ; 
- D 2 - I - 0x001FE2 00:DFD2: 90        .byte $90   ; 
- D 2 - I - 0x001FE3 00:DFD3: 04        .byte $04   ; 
off_DFD4:
- D 2 - I - 0x001FE4 00:DFD4: 06        .byte $06   ; 
- D 2 - I - 0x001FE5 00:DFD5: 10        .byte $10   ; 
- D 2 - I - 0x001FE6 00:DFD6: 20        .byte $20   ; 
- D 2 - I - 0x001FE7 00:DFD7: 06        .byte $06   ; 
- D 2 - I - 0x001FE8 00:DFD8: D8        .byte $D8   ; 
- D 2 - I - 0x001FE9 00:DFD9: 20        .byte $20   ; 
- D 2 - I - 0x001FEA 00:DFDA: 06        .byte $06   ; 
- D 2 - I - 0x001FEB 00:DFDB: B8        .byte $B8   ; 
- D 2 - I - 0x001FEC 00:DFDC: 38        .byte $38   ; 
- D 2 - I - 0x001FED 00:DFDD: 05        .byte $05   ; 
- D 2 - I - 0x001FEE 00:DFDE: 98        .byte $98   ; 
- D 2 - I - 0x001FEF 00:DFDF: 48        .byte $48   ; 
- D 2 - I - 0x001FF0 00:DFE0: 05        .byte $05   ; 
- D 2 - I - 0x001FF1 00:DFE1: 58        .byte $58   ; 
- D 2 - I - 0x001FF2 00:DFE2: 68        .byte $68   ; 
- D 2 - I - 0x001FF3 00:DFE3: 05        .byte $05   ; 
- D 2 - I - 0x001FF4 00:DFE4: 38        .byte $38   ; 
- D 2 - I - 0x001FF5 00:DFE5: 78        .byte $78   ; 
- D 2 - I - 0x001FF6 00:DFE6: 04        .byte $04   ; 
off_DFE7:
- D 2 - I - 0x001FF7 00:DFE7: 05        .byte $05   ; 
- D 2 - I - 0x001FF8 00:DFE8: 54        .byte $54   ; 
- D 2 - I - 0x001FF9 00:DFE9: 98        .byte $98   ; 
- D 2 - I - 0x001FFA 00:DFEA: 05        .byte $05   ; 
- D 2 - I - 0x001FFB 00:DFEB: 94        .byte $94   ; 
- D 2 - I - 0x001FFC 00:DFEC: 98        .byte $98   ; 
- D 2 - I - 0x001FFD 00:DFED: 05        .byte $05   ; 
- D 2 - I - 0x001FFE 00:DFEE: 6C        .byte $6C   ; 
- D 2 - I - 0x001FFF 00:DFEF: 39        .byte $39   ; 
- D 2 - I - 0x002000 00:DFF0: 06        .byte $06   ; 
- D 2 - I - 0x002001 00:DFF1: 94        .byte $94   ; 
- D 2 - I - 0x002002 00:DFF2: 31        .byte $31   ; 
- D 2 - I - 0x002003 00:DFF3: 06        .byte $06   ; 
- D 2 - I - 0x002004 00:DFF4: BC        .byte $BC   ; 
- D 2 - I - 0x002005 00:DFF5: 59        .byte $59   ; 
- D 2 - I - 0x002006 00:DFF6: 06        .byte $06   ; 
off_DFF7:
- D 2 - I - 0x002007 00:DFF7: 05        .byte $05   ; 
- D 2 - I - 0x002008 00:DFF8: 50        .byte $50   ; 
- D 2 - I - 0x002009 00:DFF9: 28        .byte $28   ; 
- D 2 - I - 0x00200A 00:DFFA: 06        .byte $06   ; 
- D 2 - I - 0x00200B 00:DFFB: A8        .byte $A8   ; 
- D 2 - I - 0x00200C 00:DFFC: 28        .byte $28   ; 
- D 2 - I - 0x00200D 00:DFFD: 06        .byte $06   ; 
- D 2 - I - 0x00200E 00:DFFE: 24        .byte $24   ; 
- D 2 - I - 0x00200F 00:DFFF: 50        .byte $50   ; 
- D 3 - I - 0x002010 00:E000: 06        .byte $06   ; 
- D 3 - I - 0x002011 00:E001: CC        .byte $CC   ; 
- D 3 - I - 0x002012 00:E002: 50        .byte $50   ; 
- D 3 - I - 0x002013 00:E003: 06        .byte $06   ; 
- D 3 - I - 0x002014 00:E004: 70        .byte $70   ; 
- D 3 - I - 0x002015 00:E005: 80        .byte $80   ; 
- D 3 - I - 0x002016 00:E006: 04        .byte $04   ; 
off_E007:
- D 3 - I - 0x002017 00:E007: 05        .byte $05   ; 
- D 3 - I - 0x002018 00:E008: 50        .byte $50   ; 
- D 3 - I - 0x002019 00:E009: 20        .byte $20   ; 
- D 3 - I - 0x00201A 00:E00A: 06        .byte $06   ; 
- D 3 - I - 0x00201B 00:E00B: C8        .byte $C8   ; 
- D 3 - I - 0x00201C 00:E00C: 20        .byte $20   ; 
- D 3 - I - 0x00201D 00:E00D: 06        .byte $06   ; 
- D 3 - I - 0x00201E 00:E00E: 84        .byte $84   ; 
- D 3 - I - 0x00201F 00:E00F: 48        .byte $48   ; 
- D 3 - I - 0x002020 00:E010: 05        .byte $05   ; 
- D 3 - I - 0x002021 00:E011: 44        .byte $44   ; 
- D 3 - I - 0x002022 00:E012: 70        .byte $70   ; 
- D 3 - I - 0x002023 00:E013: 05        .byte $05   ; 
- D 3 - I - 0x002024 00:E014: 78        .byte $78   ; 
- D 3 - I - 0x002025 00:E015: A0        .byte $A0   ; 
- D 3 - I - 0x002026 00:E016: 04        .byte $04   ; 
off_E017:
- D 3 - I - 0x002027 00:E017: 06        .byte $06   ; 
- D 3 - I - 0x002028 00:E018: 28        .byte $28   ; 
- D 3 - I - 0x002029 00:E019: 20        .byte $20   ; 
- D 3 - I - 0x00202A 00:E01A: 06        .byte $06   ; 
- D 3 - I - 0x00202B 00:E01B: C8        .byte $C8   ; 
- D 3 - I - 0x00202C 00:E01C: 20        .byte $20   ; 
- D 3 - I - 0x00202D 00:E01D: 06        .byte $06   ; 
- D 3 - I - 0x00202E 00:E01E: 48        .byte $48   ; 
- D 3 - I - 0x00202F 00:E01F: 50        .byte $50   ; 
- D 3 - I - 0x002030 00:E020: 05        .byte $05   ; 
- D 3 - I - 0x002031 00:E021: B0        .byte $B0   ; 
- D 3 - I - 0x002032 00:E022: 50        .byte $50   ; 
- D 3 - I - 0x002033 00:E023: 05        .byte $05   ; 
- D 3 - I - 0x002034 00:E024: 5C        .byte $5C   ; 
- D 3 - I - 0x002035 00:E025: 80        .byte $80   ; 
- D 3 - I - 0x002036 00:E026: 04        .byte $04   ; 
- D 3 - I - 0x002037 00:E027: 94        .byte $94   ; 
- D 3 - I - 0x002038 00:E028: 80        .byte $80   ; 
- D 3 - I - 0x002039 00:E029: 04        .byte $04   ; 
off_E02A:
- D 3 - I - 0x00203A 00:E02A: 06        .byte $06   ; 
- D 3 - I - 0x00203B 00:E02B: 20        .byte $20   ; 
- D 3 - I - 0x00203C 00:E02C: 80        .byte $80   ; 
- D 3 - I - 0x00203D 00:E02D: 04        .byte $04   ; 
- D 3 - I - 0x00203E 00:E02E: 40        .byte $40   ; 
- D 3 - I - 0x00203F 00:E02F: 68        .byte $68   ; 
- D 3 - I - 0x002040 00:E030: 05        .byte $05   ; 
- D 3 - I - 0x002041 00:E031: 60        .byte $60   ; 
- D 3 - I - 0x002042 00:E032: 50        .byte $50   ; 
- D 3 - I - 0x002043 00:E033: 06        .byte $06   ; 
- D 3 - I - 0x002044 00:E034: A0        .byte $A0   ; 
- D 3 - I - 0x002045 00:E035: 50        .byte $50   ; 
- D 3 - I - 0x002046 00:E036: 06        .byte $06   ; 
- D 3 - I - 0x002047 00:E037: C0        .byte $C0   ; 
- D 3 - I - 0x002048 00:E038: 68        .byte $68   ; 
- D 3 - I - 0x002049 00:E039: 05        .byte $05   ; 
- D 3 - I - 0x00204A 00:E03A: E0        .byte $E0   ; 
- D 3 - I - 0x00204B 00:E03B: 80        .byte $80   ; 
- D 3 - I - 0x00204C 00:E03C: 04        .byte $04   ; 
tbl_E03D:
- D 3 - - - 0x00204D 00:E03D: 08        .byte $08   ; 
- D 3 - - - 0x00204E 00:E03E: 08        .byte $08   ; 
- D 3 - - - 0x00204F 00:E03F: F0        .byte $F0   ; 
- D 3 - - - 0x002050 00:E040: 08        .byte $08   ; 
- D 3 - - - 0x002051 00:E041: 08        .byte $08   ; 
- D 3 - - - 0x002052 00:E042: 00        .byte $00   ; 
tbl_E043:
- D 3 - I - 0x002053 00:E043: 00        .byte $00   ; 
- D 3 - I - 0x002054 00:E044: 00        .byte $00   ; 
- D 3 - I - 0x002055 00:E045: 00        .byte $00   ; 
- D 3 - I - 0x002056 00:E046: 08        .byte $08   ; 
- D 3 - I - 0x002057 00:E047: 08        .byte $08   ; 
- D 3 - I - 0x002058 00:E048: 08        .byte $08   ; 
- D 3 - I - 0x002059 00:E049: 00        .byte $00   ; 
- D 3 - I - 0x00205A 00:E04A: 00        .byte $00   ; 
- D 3 - I - 0x00205B 00:E04B: 01        .byte $01   ; 
- D 3 - I - 0x00205C 00:E04C: 08        .byte $08   ; 
- D 3 - I - 0x00205D 00:E04D: 08        .byte $08   ; 
- D 3 - I - 0x00205E 00:E04E: 09        .byte $09   ; 
- D 3 - I - 0x00205F 00:E04F: 00        .byte $00   ; 
- D 3 - I - 0x002060 00:E050: 00        .byte $00   ; 
- D 3 - I - 0x002061 00:E051: FF        .byte $FF   ; 
- D 3 - I - 0x002062 00:E052: 08        .byte $08   ; 
- D 3 - I - 0x002063 00:E053: 08        .byte $08   ; 
- D 3 - I - 0x002064 00:E054: 07        .byte $07   ; 
- D 3 - I - 0x002065 00:E055: FE        .byte $FE   ; 
- D 3 - I - 0x002066 00:E056: 00        .byte $00   ; 
- D 3 - I - 0x002067 00:E057: 00        .byte $00   ; 
- D 3 - I - 0x002068 00:E058: 06        .byte $06   ; 
- D 3 - I - 0x002069 00:E059: 08        .byte $08   ; 
- D 3 - I - 0x00206A 00:E05A: 08        .byte $08   ; 
- D 3 - I - 0x00206B 00:E05B: 02        .byte $02   ; 
- D 3 - I - 0x00206C 00:E05C: 00        .byte $00   ; 
- D 3 - I - 0x00206D 00:E05D: 00        .byte $00   ; 
- D 3 - I - 0x00206E 00:E05E: 0A        .byte $0A   ; 
- D 3 - I - 0x00206F 00:E05F: 08        .byte $08   ; 
- D 3 - I - 0x002070 00:E060: 08        .byte $08   ; 
- D 3 - I - 0x002071 00:E061: 02        .byte $02   ; 
- D 3 - I - 0x002072 00:E062: 00        .byte $00   ; 
- D 3 - I - 0x002073 00:E063: 01        .byte $01   ; 
- D 3 - I - 0x002074 00:E064: 0A        .byte $0A   ; 
- D 3 - I - 0x002075 00:E065: 08        .byte $08   ; 
- D 3 - I - 0x002076 00:E066: 09        .byte $09   ; 
- D 3 - I - 0x002077 00:E067: 04        .byte $04   ; 
- D 3 - I - 0x002078 00:E068: 00        .byte $00   ; 
- D 3 - I - 0x002079 00:E069: 00        .byte $00   ; 
- D 3 - I - 0x00207A 00:E06A: 0C        .byte $0C   ; 
- D 3 - I - 0x00207B 00:E06B: 08        .byte $08   ; 
- D 3 - I - 0x00207C 00:E06C: 08        .byte $08   ; 
- D 3 - I - 0x00207D 00:E06D: 00        .byte $00   ; 
- D 3 - I - 0x00207E 00:E06E: 00        .byte $00   ; 
- D 3 - I - 0x00207F 00:E06F: FE        .byte $FE   ; 
- D 3 - I - 0x002080 00:E070: 08        .byte $08   ; 
- D 3 - I - 0x002081 00:E071: 08        .byte $08   ; 
- D 3 - I - 0x002082 00:E072: 06        .byte $06   ; 
- D 3 - I - 0x002083 00:E073: FD        .byte $FD   ; 
- D 3 - I - 0x002084 00:E074: FD        .byte $FD   ; 
- D 3 - I - 0x002085 00:E075: FD        .byte $FD   ; 
- D 3 - I - 0x002086 00:E076: 05        .byte $05   ; 
- D 3 - I - 0x002087 00:E077: 05        .byte $05   ; 
- D 3 - I - 0x002088 00:E078: 05        .byte $05   ; 
tbl_E079:
- D 3 - I - 0x002089 00:E079: 08        .byte $08   ; 
- D 3 - I - 0x00208A 00:E07A: 08        .byte $08   ; 
- D 3 - I - 0x00208B 00:E07B: 08        .byte $08   ; 
- D 3 - I - 0x00208C 00:E07C: 00        .byte $00   ; 
- D 3 - I - 0x00208D 00:E07D: 00        .byte $00   ; 
- D 3 - I - 0x00208E 00:E07E: 00        .byte $00   ; 
- D 3 - I - 0x00208F 00:E07F: 08        .byte $08   ; 
- D 3 - I - 0x002090 00:E080: 08        .byte $08   ; 
- D 3 - I - 0x002091 00:E081: 07        .byte $07   ; 
- D 3 - I - 0x002092 00:E082: 00        .byte $00   ; 
- D 3 - I - 0x002093 00:E083: 00        .byte $00   ; 
- D 3 - I - 0x002094 00:E084: FF        .byte $FF   ; 
- D 3 - I - 0x002095 00:E085: 08        .byte $08   ; 
- D 3 - I - 0x002096 00:E086: 08        .byte $08   ; 
- D 3 - I - 0x002097 00:E087: 09        .byte $09   ; 
- D 3 - I - 0x002098 00:E088: 00        .byte $00   ; 
- D 3 - I - 0x002099 00:E089: 00        .byte $00   ; 
- D 3 - I - 0x00209A 00:E08A: 01        .byte $01   ; 
- D 3 - I - 0x00209B 00:E08B: 0A        .byte $0A   ; 
- D 3 - I - 0x00209C 00:E08C: 08        .byte $08   ; 
- D 3 - I - 0x00209D 00:E08D: 08        .byte $08   ; 
- D 3 - I - 0x00209E 00:E08E: 02        .byte $02   ; 
- D 3 - I - 0x00209F 00:E08F: 00        .byte $00   ; 
- D 3 - I - 0x0020A0 00:E090: 00        .byte $00   ; 
- D 3 - I - 0x0020A1 00:E091: 06        .byte $06   ; 
- D 3 - I - 0x0020A2 00:E092: 08        .byte $08   ; 
- D 3 - I - 0x0020A3 00:E093: 08        .byte $08   ; 
- D 3 - I - 0x0020A4 00:E094: FE        .byte $FE   ; 
- D 3 - I - 0x0020A5 00:E095: 00        .byte $00   ; 
- D 3 - I - 0x0020A6 00:E096: 00        .byte $00   ; 
- D 3 - I - 0x0020A7 00:E097: 06        .byte $06   ; 
- D 3 - I - 0x0020A8 00:E098: 08        .byte $08   ; 
- D 3 - I - 0x0020A9 00:E099: 07        .byte $07   ; 
- D 3 - I - 0x0020AA 00:E09A: FE        .byte $FE   ; 
- D 3 - I - 0x0020AB 00:E09B: 00        .byte $00   ; 
- D 3 - I - 0x0020AC 00:E09C: FF        .byte $FF   ; 
- D 3 - I - 0x0020AD 00:E09D: 04        .byte $04   ; 
- D 3 - I - 0x0020AE 00:E09E: 08        .byte $08   ; 
- D 3 - I - 0x0020AF 00:E09F: 08        .byte $08   ; 
- D 3 - I - 0x0020B0 00:E0A0: FC        .byte $FC   ; 
- D 3 - I - 0x0020B1 00:E0A1: 00        .byte $00   ; 
- D 3 - I - 0x0020B2 00:E0A2: 00        .byte $00   ; 
- D 3 - I - 0x0020B3 00:E0A3: 08        .byte $08   ; 
- D 3 - I - 0x0020B4 00:E0A4: 08        .byte $08   ; 
- D 3 - I - 0x0020B5 00:E0A5: 0A        .byte $0A   ; 
- D 3 - I - 0x0020B6 00:E0A6: 00        .byte $00   ; 
- D 3 - I - 0x0020B7 00:E0A7: 00        .byte $00   ; 
- D 3 - I - 0x0020B8 00:E0A8: 02        .byte $02   ; 
- D 3 - I - 0x0020B9 00:E0A9: 0B        .byte $0B   ; 
- D 3 - I - 0x0020BA 00:E0AA: 0B        .byte $0B   ; 
- D 3 - I - 0x0020BB 00:E0AB: 0B        .byte $0B   ; 
- D 3 - I - 0x0020BC 00:E0AC: 03        .byte $03   ; 
- D 3 - I - 0x0020BD 00:E0AD: 03        .byte $03   ; 
- D 3 - I - 0x0020BE 00:E0AE: 03        .byte $03   ; 
_off004_E0AF_00:
- D 3 - I - 0x0020BF 00:E0AF: 00        .byte $00   ; 
- D 3 - I - 0x0020C0 00:E0B0: 00        .byte $00   ; 
- D 3 - I - 0x0020C1 00:E0B1: 01        .byte $01   ; 
- D 3 - I - 0x0020C2 00:E0B2: 02        .byte $02   ; 
- D 3 - I - 0x0020C3 00:E0B3: 03        .byte $03   ; 
- D 3 - I - 0x0020C4 00:E0B4: 04        .byte $04   ; 
- D 3 - I - 0x0020C5 00:E0B5: 05        .byte $05   ; 
_off004_E0B6_01:
_off004_E0B6_03:
- D 3 - I - 0x0020C6 00:E0B6: 00        .byte $00   ; 
- D 3 - I - 0x0020C7 00:E0B7: 00        .byte $00   ; 
- D 3 - I - 0x0020C8 00:E0B8: 01        .byte $01   ; 
- D 3 - I - 0x0020C9 00:E0B9: 02        .byte $02   ; 
- D 3 - I - 0x0020CA 00:E0BA: 03        .byte $03   ; 
- D 3 - I - 0x0020CB 00:E0BB: 06        .byte $06   ; 
- D 3 - I - 0x0020CC 00:E0BC: 07        .byte $07   ; 
_off004_E0BD_02:
- D 3 - I - 0x0020CD 00:E0BD: 00        .byte $00   ; 
- D 3 - I - 0x0020CE 00:E0BE: 00        .byte $00   ; 
- D 3 - I - 0x0020CF 00:E0BF: 01        .byte $01   ; 
- D 3 - I - 0x0020D0 00:E0C0: 02        .byte $02   ; 
- D 3 - I - 0x0020D1 00:E0C1: 03        .byte $03   ; 
- D 3 - I - 0x0020D2 00:E0C2: 06        .byte $06   ; 
- D 3 - I - 0x0020D3 00:E0C3: 08        .byte $08   ; 
_off004_E0C4_04:
- D 3 - I - 0x0020D4 00:E0C4: 00        .byte $00   ; 
- D 3 - I - 0x0020D5 00:E0C5: 09        .byte $09   ; 
- D 3 - I - 0x0020D6 00:E0C6: 0A        .byte $0A   ; 
- D 3 - I - 0x0020D7 00:E0C7: 02        .byte $02   ; 
- D 3 - I - 0x0020D8 00:E0C8: 0B        .byte $0B   ; 
- D 3 - I - 0x0020D9 00:E0C9: 0C        .byte $0C   ; 
- D 3 - I - 0x0020DA 00:E0CA: 05        .byte $05   ; 
_off004_E0CB_05:
_off004_E0CB_07:
- D 3 - I - 0x0020DB 00:E0CB: 00        .byte $00   ; 
- D 3 - I - 0x0020DC 00:E0CC: 00        .byte $00   ; 
- D 3 - I - 0x0020DD 00:E0CD: 01        .byte $01   ; 
- D 3 - I - 0x0020DE 00:E0CE: 02        .byte $02   ; 
- D 3 - I - 0x0020DF 00:E0CF: 03        .byte $03   ; 
- D 3 - I - 0x0020E0 00:E0D0: 04        .byte $04   ; 
- D 3 - I - 0x0020E1 00:E0D1: 05        .byte $05   ; 
_off004_E0D2_06:
- D 3 - I - 0x0020E2 00:E0D2: 00        .byte $00   ; 
- D 3 - I - 0x0020E3 00:E0D3: 00        .byte $00   ; 
- D 3 - I - 0x0020E4 00:E0D4: 01        .byte $01   ; 
- D 3 - I - 0x0020E5 00:E0D5: 02        .byte $02   ; 
- D 3 - I - 0x0020E6 00:E0D6: 0D        .byte $0D   ; 
- D 3 - I - 0x0020E7 00:E0D7: 0E        .byte $0E   ; 
- D 3 - I - 0x0020E8 00:E0D8: 05        .byte $05   ; 
_off004_E0D9_08:
- D 3 - I - 0x0020E9 00:E0D9: 00        .byte $00   ; 
- D 3 - I - 0x0020EA 00:E0DA: 1A        .byte $1A   ; 
- D 3 - I - 0x0020EB 00:E0DB: 1B        .byte $1B   ; 
- D 3 - I - 0x0020EC 00:E0DC: 1C        .byte $1C   ; 
- D 3 - I - 0x0020ED 00:E0DD: 1D        .byte $1D   ; 
- D 3 - I - 0x0020EE 00:E0DE: 1E        .byte $1E   ; 
- D 3 - I - 0x0020EF 00:E0DF: 1F        .byte $1F   ; 
_off004_E0E0_09:
_off004_E0E0_0B:
- D 3 - I - 0x0020F0 00:E0E0: 01        .byte $01   ; 
- D 3 - I - 0x0020F1 00:E0E1: 1A        .byte $1A   ; 
- D 3 - I - 0x0020F2 00:E0E2: 1B        .byte $1B   ; 
- D 3 - I - 0x0020F3 00:E0E3: 20        .byte $20   ; 
- D 3 - I - 0x0020F4 00:E0E4: 1D        .byte $1D   ; 
- D 3 - I - 0x0020F5 00:E0E5: 1E        .byte $1E   ; 
- D 3 - I - 0x0020F6 00:E0E6: FC        .byte $FC   ; 
_off004_E0E7_0A:
- D 3 - I - 0x0020F7 00:E0E7: 00        .byte $00   ; 
- D 3 - I - 0x0020F8 00:E0E8: 1A        .byte $1A   ; 
- D 3 - I - 0x0020F9 00:E0E9: 21        .byte $21   ; 
- D 3 - I - 0x0020FA 00:E0EA: 22        .byte $22   ; 
- D 3 - I - 0x0020FB 00:E0EB: 1D        .byte $1D   ; 
- D 3 - I - 0x0020FC 00:E0EC: 23        .byte $23   ; 
- D 3 - I - 0x0020FD 00:E0ED: 24        .byte $24   ; 
_off004_E0EE_0C:
- D 3 - I - 0x0020FE 00:E0EE: 00        .byte $00   ; 
- D 3 - I - 0x0020FF 00:E0EF: 00        .byte $00   ; 
- D 3 - I - 0x002100 00:E0F0: 38        .byte $38   ; 
- D 3 - I - 0x002101 00:E0F1: 35        .byte $35   ; 
- D 3 - I - 0x002102 00:E0F2: 0D        .byte $0D   ; 
- D 3 - I - 0x002103 00:E0F3: 39        .byte $39   ; 
- D 3 - I - 0x002104 00:E0F4: 37        .byte $37   ; 
_off001_E0F5_04:
_off001_E0F5_05:
_off001_E0F5_07:
_off004_E0F5_0D:
_off004_E0F5_0F:
- D 3 - I - 0x002105 00:E0F5: 00        .byte $00   ; 
- D 3 - I - 0x002106 00:E0F6: 00        .byte $00   ; 
- D 3 - I - 0x002107 00:E0F7: 34        .byte $34   ; 
- D 3 - I - 0x002108 00:E0F8: 35        .byte $35   ; 
- D 3 - I - 0x002109 00:E0F9: 03        .byte $03   ; 
- D 3 - I - 0x00210A 00:E0FA: 36        .byte $36   ; 
- D 3 - I - 0x00210B 00:E0FB: 37        .byte $37   ; 
_off004_E0FC_0E:
- D 3 - I - 0x00210C 00:E0FC: 00        .byte $00   ; 
- D 3 - I - 0x00210D 00:E0FD: 09        .byte $09   ; 
- D 3 - I - 0x00210E 00:E0FE: 3A        .byte $3A   ; 
- D 3 - I - 0x00210F 00:E0FF: 35        .byte $35   ; 
- D 3 - I - 0x002110 00:E100: 0B        .byte $0B   ; 
- D 3 - I - 0x002111 00:E101: 3B        .byte $3B   ; 
- D 3 - I - 0x002112 00:E102: 37        .byte $37   ; 
_off001_E103_06:
- D 3 - I - 0x002113 00:E103: 00        .byte $00   ; 
- D 3 - I - 0x002114 00:E104: CE        .byte $CE   ; 
- D 3 - I - 0x002115 00:E105: CF        .byte $CF   ; 
- D 3 - I - 0x002116 00:E106: D0        .byte $D0   ; 
- D 3 - I - 0x002117 00:E107: D1        .byte $D1   ; 
- D 3 - I - 0x002118 00:E108: D2        .byte $D2   ; 
- D 3 - I - 0x002119 00:E109: D3        .byte $D3   ; 
_off004_E10A_10:
_off004_E10A_11:
_off004_E10A_12:
_off004_E10A_13:
- D 3 - I - 0x00211A 00:E10A: 00        .byte $00   ; 
- D 3 - I - 0x00211B 00:E10B: 25        .byte $25   ; 
- D 3 - I - 0x00211C 00:E10C: 26        .byte $26   ; 
- D 3 - I - 0x00211D 00:E10D: 27        .byte $27   ; 
- D 3 - I - 0x00211E 00:E10E: 28        .byte $28   ; 
- D 3 - I - 0x00211F 00:E10F: 29        .byte $29   ; 
- D 3 - I - 0x002120 00:E110: 2A        .byte $2A   ; 
_off004_E111_14:
_off004_E111_15:
_off004_E111_16:
_off004_E111_17:
- D 3 - I - 0x002121 00:E111: 01        .byte $01   ; 
- D 3 - I - 0x002122 00:E112: 28        .byte $28   ; 
- D 3 - I - 0x002123 00:E113: 29        .byte $29   ; 
- D 3 - I - 0x002124 00:E114: 2C        .byte $2C   ; 
- D 3 - I - 0x002125 00:E115: 25        .byte $25   ; 
- D 3 - I - 0x002126 00:E116: 26        .byte $26   ; 
- D 3 - I - 0x002127 00:E117: 2B        .byte $2B   ; 
_off004_E118_18:
_off004_E118_19:
_off004_E118_1A:
_off004_E118_1B:
- D 3 - I - 0x002128 00:E118: 00        .byte $00   ; 
- D 3 - I - 0x002129 00:E119: 4C        .byte $4C   ; 
- D 3 - I - 0x00212A 00:E11A: CC        .byte $CC   ; 
- D 3 - I - 0x00212B 00:E11B: 02        .byte $02   ; 
- D 3 - I - 0x00212C 00:E11C: 4D        .byte $4D   ; 
- D 3 - I - 0x00212D 00:E11D: CD        .byte $CD   ; 
- D 3 - I - 0x00212E 00:E11E: 05        .byte $05   ; 
_off004_E11F_1C:
_off004_E11F_1D:
_off004_E11F_1E:
_off004_E11F_1F:
- D 3 - I - 0x00212F 00:E11F: 00        .byte $00   ; 
- D 3 - I - 0x002130 00:E120: 4C        .byte $4C   ; 
- D 3 - I - 0x002131 00:E121: 2F        .byte $2F   ; 
- D 3 - I - 0x002132 00:E122: 22        .byte $22   ; 
- D 3 - I - 0x002133 00:E123: 4D        .byte $4D   ; 
- D 3 - I - 0x002134 00:E124: 30        .byte $30   ; 
- D 3 - I - 0x002135 00:E125: 24        .byte $24   ; 
_off004_E126_20:
_off004_E126_21:
_off004_E126_22:
_off004_E126_23:
- D 3 - I - 0x002136 00:E126: 07        .byte $07   ; 
- D 3 - I - 0x002137 00:E127: 4C        .byte $4C   ; 
- D 3 - I - 0x002138 00:E128: 2F        .byte $2F   ; 
- D 3 - I - 0x002139 00:E129: 35        .byte $35   ; 
- D 3 - I - 0x00213A 00:E12A: 4D        .byte $4D   ; 
- D 3 - I - 0x00213B 00:E12B: 30        .byte $30   ; 
- D 3 - I - 0x00213C 00:E12C: 37        .byte $37   ; 
_off004_E12D_24:
_off004_E12D_25:
_off004_E12D_26:
_off004_E12D_27:
- - - - - - 0x00213D 00:E12D: 02        .byte $02   ; 
- - - - - - 0x00213E 00:E12E: 4C        .byte $4C   ; 
- - - - - - 0x00213F 00:E12F: 2F        .byte $2F   ; 
- - - - - - 0x002140 00:E130: 27        .byte $27   ; 
- - - - - - 0x002141 00:E131: 4D        .byte $4D   ; 
- - - - - - 0x002142 00:E132: 30        .byte $30   ; 
- - - - - - 0x002143 00:E133: 2A        .byte $2A   ; 
_off004_E134_28:
_off004_E134_29:
_off004_E134_2A:
_off004_E134_2B:
- - - - - - 0x002144 00:E134: 07        .byte $07   ; 
- - - - - - 0x002145 00:E135: 4C        .byte $4C   ; 
- - - - - - 0x002146 00:E136: 2F        .byte $2F   ; 
- - - - - - 0x002147 00:E137: 2B        .byte $2B   ; 
- - - - - - 0x002148 00:E138: 4D        .byte $4D   ; 
- - - - - - 0x002149 00:E139: 30        .byte $30   ; 
- - - - - - 0x00214A 00:E13A: 2C        .byte $2C   ; 
_off004_E13B_2C:
- - - - - - 0x00214B 00:E13B: 00        .byte $00   ; 
- - - - - - 0x00214C 00:E13C: 0F        .byte $0F   ; 
- - - - - - 0x00214D 00:E13D: 10        .byte $10   ; 
- - - - - - 0x00214E 00:E13E: 02        .byte $02   ; 
- - - - - - 0x00214F 00:E13F: 11        .byte $11   ; 
- - - - - - 0x002150 00:E140: 12        .byte $12   ; 
- - - - - - 0x002151 00:E141: 05        .byte $05   ; 
_off004_E142_2D:
_off004_E142_2F:
- D 3 - I - 0x002152 00:E142: 00        .byte $00   ; 
- D 3 - I - 0x002153 00:E143: 0F        .byte $0F   ; 
- D 3 - I - 0x002154 00:E144: 10        .byte $10   ; 
- D 3 - I - 0x002155 00:E145: 02        .byte $02   ; 
- D 3 - I - 0x002156 00:E146: 11        .byte $11   ; 
- D 3 - I - 0x002157 00:E147: 19        .byte $19   ; 
- D 3 - I - 0x002158 00:E148: 07        .byte $07   ; 
_off004_E149_2E:
- D 3 - I - 0x002159 00:E149: 00        .byte $00   ; 
- D 3 - I - 0x00215A 00:E14A: 0F        .byte $0F   ; 
- D 3 - I - 0x00215B 00:E14B: 10        .byte $10   ; 
- D 3 - I - 0x00215C 00:E14C: 02        .byte $02   ; 
- D 3 - I - 0x00215D 00:E14D: 11        .byte $11   ; 
- D 3 - I - 0x00215E 00:E14E: 19        .byte $19   ; 
- D 3 - I - 0x00215F 00:E14F: 08        .byte $08   ; 
_off004_E150_30:
- D 3 - I - 0x002160 00:E150: 00        .byte $00   ; 
- D 3 - I - 0x002161 00:E151: 13        .byte $13   ; 
- D 3 - I - 0x002162 00:E152: 14        .byte $14   ; 
- D 3 - I - 0x002163 00:E153: 02        .byte $02   ; 
- D 3 - I - 0x002164 00:E154: 15        .byte $15   ; 
- D 3 - I - 0x002165 00:E155: 16        .byte $16   ; 
- D 3 - I - 0x002166 00:E156: 05        .byte $05   ; 
_off004_E157_31:
_off004_E157_33:
- D 3 - I - 0x002167 00:E157: 00        .byte $00   ; 
- D 3 - I - 0x002168 00:E158: 0F        .byte $0F   ; 
- D 3 - I - 0x002169 00:E159: 10        .byte $10   ; 
- D 3 - I - 0x00216A 00:E15A: 02        .byte $02   ; 
- D 3 - I - 0x00216B 00:E15B: 11        .byte $11   ; 
- D 3 - I - 0x00216C 00:E15C: 12        .byte $12   ; 
- D 3 - I - 0x00216D 00:E15D: 05        .byte $05   ; 
_off004_E15E_32:
- D 3 - I - 0x00216E 00:E15E: 03        .byte $03   ; 
- D 3 - I - 0x00216F 00:E15F: 13        .byte $13   ; 
- D 3 - I - 0x002170 00:E160: 17        .byte $17   ; 
- D 3 - I - 0x002171 00:E161: 02        .byte $02   ; 
- D 3 - I - 0x002172 00:E162: 15        .byte $15   ; 
- D 3 - I - 0x002173 00:E163: 18        .byte $18   ; 
- D 3 - I - 0x002174 00:E164: 05        .byte $05   ; 
_off004_E165_34:
- D 3 - I - 0x002175 00:E165: 04        .byte $04   ; 
- D 3 - I - 0x002176 00:E166: 13        .byte $13   ; 
- D 3 - I - 0x002177 00:E167: 2D        .byte $2D   ; 
- D 3 - I - 0x002178 00:E168: 1C        .byte $1C   ; 
- D 3 - I - 0x002179 00:E169: 15        .byte $15   ; 
- D 3 - I - 0x00217A 00:E16A: 2E        .byte $2E   ; 
- D 3 - I - 0x00217B 00:E16B: 1F        .byte $1F   ; 
_off004_E16C_35:
_off004_E16C_37:
- D 3 - I - 0x00217C 00:E16C: 05        .byte $05   ; 
- D 3 - I - 0x00217D 00:E16D: 13        .byte $13   ; 
- D 3 - I - 0x00217E 00:E16E: 2D        .byte $2D   ; 
- D 3 - I - 0x00217F 00:E16F: 20        .byte $20   ; 
- D 3 - I - 0x002180 00:E170: 15        .byte $15   ; 
- D 3 - I - 0x002181 00:E171: 2E        .byte $2E   ; 
- D 3 - I - 0x002182 00:E172: FC        .byte $FC   ; 
_off004_E173_36:
- D 3 - I - 0x002183 00:E173: 04        .byte $04   ; 
- D 3 - I - 0x002184 00:E174: 13        .byte $13   ; 
- D 3 - I - 0x002185 00:E175: 2F        .byte $2F   ; 
- D 3 - I - 0x002186 00:E176: 22        .byte $22   ; 
- D 3 - I - 0x002187 00:E177: 15        .byte $15   ; 
- D 3 - I - 0x002188 00:E178: 30        .byte $30   ; 
- D 3 - I - 0x002189 00:E179: 24        .byte $24   ; 
_off004_E17A_38:
- D 3 - I - 0x00218A 00:E17A: 00        .byte $00   ; 
- D 3 - I - 0x00218B 00:E17B: 13        .byte $13   ; 
- D 3 - I - 0x00218C 00:E17C: 3C        .byte $3C   ; 
- D 3 - I - 0x00218D 00:E17D: 35        .byte $35   ; 
- D 3 - I - 0x00218E 00:E17E: 15        .byte $15   ; 
- D 3 - I - 0x00218F 00:E17F: 3D        .byte $3D   ; 
- D 3 - I - 0x002190 00:E180: 37        .byte $37   ; 
_off001_E181_00:
_off001_E181_01:
_off001_E181_03:
_off004_E181_39:
_off004_E181_3B:
- D 3 - I - 0x002191 00:E181: 00        .byte $00   ; 
- D 3 - I - 0x002192 00:E182: 0F        .byte $0F   ; 
- D 3 - I - 0x002193 00:E183: 40        .byte $40   ; 
- D 3 - I - 0x002194 00:E184: 35        .byte $35   ; 
- D 3 - I - 0x002195 00:E185: 11        .byte $11   ; 
- D 3 - I - 0x002196 00:E186: 41        .byte $41   ; 
- D 3 - I - 0x002197 00:E187: 37        .byte $37   ; 
_off004_E188_3A:
- D 3 - I - 0x002198 00:E188: 03        .byte $03   ; 
- D 3 - I - 0x002199 00:E189: 13        .byte $13   ; 
- D 3 - I - 0x00219A 00:E18A: 3E        .byte $3E   ; 
- D 3 - I - 0x00219B 00:E18B: 35        .byte $35   ; 
- D 3 - I - 0x00219C 00:E18C: 15        .byte $15   ; 
- D 3 - I - 0x00219D 00:E18D: 3F        .byte $3F   ; 
- D 3 - I - 0x00219E 00:E18E: 37        .byte $37   ; 
_off001_E18F_02:
- D 3 - I - 0x00219F 00:E18F: 00        .byte $00   ; 
- D 3 - I - 0x0021A0 00:E190: D4        .byte $D4   ; 
- D 3 - I - 0x0021A1 00:E191: D5        .byte $D5   ; 
- D 3 - I - 0x0021A2 00:E192: D0        .byte $D0   ; 
- D 3 - I - 0x0021A3 00:E193: D6        .byte $D6   ; 
- D 3 - I - 0x0021A4 00:E194: D7        .byte $D7   ; 
- D 3 - I - 0x0021A5 00:E195: D3        .byte $D3   ; 
_off004_E196_3C:
_off004_E196_3D:
_off004_E196_3E:
_off004_E196_3F:
- D 3 - I - 0x0021A6 00:E196: 00        .byte $00   ; 
- D 3 - I - 0x0021A7 00:E197: 25        .byte $25   ; 
- D 3 - I - 0x0021A8 00:E198: 31        .byte $31   ; 
- D 3 - I - 0x0021A9 00:E199: 27        .byte $27   ; 
- D 3 - I - 0x0021AA 00:E19A: 32        .byte $32   ; 
- D 3 - I - 0x0021AB 00:E19B: 33        .byte $33   ; 
- D 3 - I - 0x0021AC 00:E19C: 2A        .byte $2A   ; 
_off004_E19D_40:
_off004_E19D_41:
_off004_E19D_42:
_off004_E19D_43:
- D 3 - I - 0x0021AD 00:E19D: 02        .byte $02   ; 
- D 3 - I - 0x0021AE 00:E19E: 25        .byte $25   ; 
- D 3 - I - 0x0021AF 00:E19F: 31        .byte $31   ; 
- D 3 - I - 0x0021B0 00:E1A0: 27        .byte $27   ; 
- D 3 - I - 0x0021B1 00:E1A1: 32        .byte $32   ; 
- D 3 - I - 0x0021B2 00:E1A2: 33        .byte $33   ; 
- D 3 - I - 0x0021B3 00:E1A3: 2A        .byte $2A   ; 
_off004_E1A4_44:
_off004_E1A4_49:
_off004_E1A4_4B:
- D 3 - I - 0x0021B4 00:E1A4: 00        .byte $00   ; 
- D 3 - I - 0x0021B5 00:E1A5: FC        .byte $FC   ; 
- D 3 - I - 0x0021B6 00:E1A6: 48        .byte $48   ; 
- D 3 - I - 0x0021B7 00:E1A7: 42        .byte $42   ; 
- D 3 - I - 0x0021B8 00:E1A8: FC        .byte $FC   ; 
- D 3 - I - 0x0021B9 00:E1A9: 49        .byte $49   ; 
- D 3 - I - 0x0021BA 00:E1AA: 43        .byte $43   ; 
_off004_E1AB_45:
_off004_E1AB_47:
- D 3 - I - 0x0021BB 00:E1AB: 00        .byte $00   ; 
- D 3 - I - 0x0021BC 00:E1AC: FC        .byte $FC   ; 
- D 3 - I - 0x0021BD 00:E1AD: 48        .byte $48   ; 
- D 3 - I - 0x0021BE 00:E1AE: 44        .byte $44   ; 
- D 3 - I - 0x0021BF 00:E1AF: FC        .byte $FC   ; 
- D 3 - I - 0x0021C0 00:E1B0: 49        .byte $49   ; 
- D 3 - I - 0x0021C1 00:E1B1: 45        .byte $45   ; 
_off004_E1B2_46:
- D 3 - I - 0x0021C2 00:E1B2: 00        .byte $00   ; 
- D 3 - I - 0x0021C3 00:E1B3: FC        .byte $FC   ; 
- D 3 - I - 0x0021C4 00:E1B4: 4A        .byte $4A   ; 
- D 3 - I - 0x0021C5 00:E1B5: 46        .byte $46   ; 
- D 3 - I - 0x0021C6 00:E1B6: FC        .byte $FC   ; 
- D 3 - I - 0x0021C7 00:E1B7: 4B        .byte $4B   ; 
- D 3 - I - 0x0021C8 00:E1B8: 47        .byte $47   ; 
_off004_E1B9_48:
_off004_E1B9_4A:
- D 3 - I - 0x0021C9 00:E1B9: 00        .byte $00   ; 
- D 3 - I - 0x0021CA 00:E1BA: FC        .byte $FC   ; 
- D 3 - I - 0x0021CB 00:E1BB: A4        .byte $A4   ; 
- D 3 - I - 0x0021CC 00:E1BC: A5        .byte $A5   ; 
- D 3 - I - 0x0021CD 00:E1BD: FC        .byte $FC   ; 
- D 3 - I - 0x0021CE 00:E1BE: A6        .byte $A6   ; 
- D 3 - I - 0x0021CF 00:E1BF: A7        .byte $A7   ; 
_off002_E1C0_10:
_off002_E1C0_11:
_off002_E1C0_12:
_off002_E1C0_13:
_off002_E1C0_14:
_off002_E1C0_16:
- D 3 - I - 0x0021D0 00:E1C0: 08        .byte $08   ; 
- D 3 - I - 0x0021D1 00:E1C1: FC        .byte $FC   ; 
- D 3 - I - 0x0021D2 00:E1C2: 71        .byte $71   ; 
- D 3 - I - 0x0021D3 00:E1C3: FC        .byte $FC   ; 
- D 3 - I - 0x0021D4 00:E1C4: FC        .byte $FC   ; 
- D 3 - I - 0x0021D5 00:E1C5: 72        .byte $72   ; 
- D 3 - I - 0x0021D6 00:E1C6: 73        .byte $73   ; 
_off002_E1C7_15:
_off002_E1C7_17:
- D 3 - I - 0x0021D7 00:E1C7: 08        .byte $08   ; 
- D 3 - I - 0x0021D8 00:E1C8: FC        .byte $FC   ; 
- D 3 - I - 0x0021D9 00:E1C9: 74        .byte $74   ; 
- D 3 - I - 0x0021DA 00:E1CA: FC        .byte $FC   ; 
- D 3 - I - 0x0021DB 00:E1CB: FC        .byte $FC   ; 
- D 3 - I - 0x0021DC 00:E1CC: 75        .byte $75   ; 
- D 3 - I - 0x0021DD 00:E1CD: 76        .byte $76   ; 
_off002_E1CE_18:
_off002_E1CE_1A:
- D 3 - I - 0x0021DE 00:E1CE: 08        .byte $08   ; 
- D 3 - I - 0x0021DF 00:E1CF: FC        .byte $FC   ; 
- D 3 - I - 0x0021E0 00:E1D0: 71        .byte $71   ; 
- D 3 - I - 0x0021E1 00:E1D1: 77        .byte $77   ; 
- D 3 - I - 0x0021E2 00:E1D2: FC        .byte $FC   ; 
- D 3 - I - 0x0021E3 00:E1D3: 72        .byte $72   ; 
- D 3 - I - 0x0021E4 00:E1D4: 73        .byte $73   ; 
_off002_E1D5_19:
_off002_E1D5_1B:
- D 3 - I - 0x0021E5 00:E1D5: 08        .byte $08   ; 
- D 3 - I - 0x0021E6 00:E1D6: FC        .byte $FC   ; 
- D 3 - I - 0x0021E7 00:E1D7: 74        .byte $74   ; 
- D 3 - I - 0x0021E8 00:E1D8: 77        .byte $77   ; 
- D 3 - I - 0x0021E9 00:E1D9: FC        .byte $FC   ; 
- D 3 - I - 0x0021EA 00:E1DA: 75        .byte $75   ; 
- D 3 - I - 0x0021EB 00:E1DB: 76        .byte $76   ; 
_off002_E1DC_1C:
_off002_E1DC_1E:
- D 3 - I - 0x0021EC 00:E1DC: 08        .byte $08   ; 
- D 3 - I - 0x0021ED 00:E1DD: FC        .byte $FC   ; 
- D 3 - I - 0x0021EE 00:E1DE: 71        .byte $71   ; 
- D 3 - I - 0x0021EF 00:E1DF: 78        .byte $78   ; 
- D 3 - I - 0x0021F0 00:E1E0: FC        .byte $FC   ; 
- D 3 - I - 0x0021F1 00:E1E1: 72        .byte $72   ; 
- D 3 - I - 0x0021F2 00:E1E2: 73        .byte $73   ; 
_off002_E1E3_1D:
_off002_E1E3_1F:
- D 3 - I - 0x0021F3 00:E1E3: 08        .byte $08   ; 
- D 3 - I - 0x0021F4 00:E1E4: FC        .byte $FC   ; 
- D 3 - I - 0x0021F5 00:E1E5: 74        .byte $74   ; 
- D 3 - I - 0x0021F6 00:E1E6: 78        .byte $78   ; 
- D 3 - I - 0x0021F7 00:E1E7: FC        .byte $FC   ; 
- D 3 - I - 0x0021F8 00:E1E8: 75        .byte $75   ; 
- D 3 - I - 0x0021F9 00:E1E9: 76        .byte $76   ; 
_off002_E1EA_20:
_off002_E1EA_22:
- D 3 - I - 0x0021FA 00:E1EA: 08        .byte $08   ; 
- D 3 - I - 0x0021FB 00:E1EB: FC        .byte $FC   ; 
- D 3 - I - 0x0021FC 00:E1EC: 71        .byte $71   ; 
- D 3 - I - 0x0021FD 00:E1ED: 79        .byte $79   ; 
- D 3 - I - 0x0021FE 00:E1EE: FC        .byte $FC   ; 
- D 3 - I - 0x0021FF 00:E1EF: 72        .byte $72   ; 
- D 3 - I - 0x002200 00:E1F0: 73        .byte $73   ; 
_off002_E1F1_21:
_off002_E1F1_23:
- D 3 - I - 0x002201 00:E1F1: 08        .byte $08   ; 
- D 3 - I - 0x002202 00:E1F2: FC        .byte $FC   ; 
- D 3 - I - 0x002203 00:E1F3: 74        .byte $74   ; 
- D 3 - I - 0x002204 00:E1F4: 79        .byte $79   ; 
- D 3 - I - 0x002205 00:E1F5: FC        .byte $FC   ; 
- D 3 - I - 0x002206 00:E1F6: 75        .byte $75   ; 
- D 3 - I - 0x002207 00:E1F7: 76        .byte $76   ; 
_off002_E1F8_00:
- D 3 - I - 0x002208 00:E1F8: 00        .byte $00   ; 
- D 3 - I - 0x002209 00:E1F9: 4E        .byte $4E   ; 
- D 3 - I - 0x00220A 00:E1FA: 4F        .byte $4F   ; 
- D 3 - I - 0x00220B 00:E1FB: 50        .byte $50   ; 
- D 3 - I - 0x00220C 00:E1FC: 51        .byte $51   ; 
- D 3 - I - 0x00220D 00:E1FD: 52        .byte $52   ; 
- D 3 - I - 0x00220E 00:E1FE: 53        .byte $53   ; 
_off002_E1FF_01:
_off002_E1FF_03:
- D 3 - I - 0x00220F 00:E1FF: 00        .byte $00   ; 
- D 3 - I - 0x002210 00:E200: 4E        .byte $4E   ; 
- D 3 - I - 0x002211 00:E201: 4F        .byte $4F   ; 
- D 3 - I - 0x002212 00:E202: 50        .byte $50   ; 
- D 3 - I - 0x002213 00:E203: 51        .byte $51   ; 
- D 3 - I - 0x002214 00:E204: 5E        .byte $5E   ; 
- D 3 - I - 0x002215 00:E205: 58        .byte $58   ; 
_off002_E206_02:
- D 3 - I - 0x002216 00:E206: 00        .byte $00   ; 
- D 3 - I - 0x002217 00:E207: 4E        .byte $4E   ; 
- D 3 - I - 0x002218 00:E208: 4F        .byte $4F   ; 
- D 3 - I - 0x002219 00:E209: 50        .byte $50   ; 
- D 3 - I - 0x00221A 00:E20A: 51        .byte $51   ; 
- D 3 - I - 0x00221B 00:E20B: 5E        .byte $5E   ; 
- D 3 - I - 0x00221C 00:E20C: 5D        .byte $5D   ; 
_off002_E20D_04:
- D 3 - I - 0x00221D 00:E20D: 00        .byte $00   ; 
- D 3 - I - 0x00221E 00:E20E: 54        .byte $54   ; 
- D 3 - I - 0x00221F 00:E20F: 55        .byte $55   ; 
- D 3 - I - 0x002220 00:E210: 50        .byte $50   ; 
- D 3 - I - 0x002221 00:E211: 56        .byte $56   ; 
- D 3 - I - 0x002222 00:E212: 57        .byte $57   ; 
- D 3 - I - 0x002223 00:E213: 53        .byte $53   ; 
_off002_E214_05:
_off002_E214_07:
- D 3 - I - 0x002224 00:E214: 00        .byte $00   ; 
- D 3 - I - 0x002225 00:E215: 4E        .byte $4E   ; 
- D 3 - I - 0x002226 00:E216: 4F        .byte $4F   ; 
- D 3 - I - 0x002227 00:E217: 50        .byte $50   ; 
- D 3 - I - 0x002228 00:E218: 51        .byte $51   ; 
- D 3 - I - 0x002229 00:E219: 52        .byte $52   ; 
- D 3 - I - 0x00222A 00:E21A: 53        .byte $53   ; 
_off002_E21B_06:
- D 3 - I - 0x00222B 00:E21B: 00        .byte $00   ; 
- D 3 - I - 0x00222C 00:E21C: 59        .byte $59   ; 
- D 3 - I - 0x00222D 00:E21D: 5A        .byte $5A   ; 
- D 3 - I - 0x00222E 00:E21E: 50        .byte $50   ; 
- D 3 - I - 0x00222F 00:E21F: 5B        .byte $5B   ; 
- D 3 - I - 0x002230 00:E220: 5C        .byte $5C   ; 
- D 3 - I - 0x002231 00:E221: 53        .byte $53   ; 
_off002_E222_0A:
- D 3 - I - 0x002232 00:E222: 00        .byte $00   ; 
- D 3 - I - 0x002233 00:E223: FC        .byte $FC   ; 
- D 3 - I - 0x002234 00:E224: 5F        .byte $5F   ; 
- D 3 - I - 0x002235 00:E225: 60        .byte $60   ; 
- D 3 - I - 0x002236 00:E226: FC        .byte $FC   ; 
- D 3 - I - 0x002237 00:E227: 61        .byte $61   ; 
- D 3 - I - 0x002238 00:E228: 62        .byte $62   ; 
_off002_E229_0B:
- D 3 - I - 0x002239 00:E229: 06        .byte $06   ; 
- D 3 - I - 0x00223A 00:E22A: 63        .byte $63   ; 
- D 3 - I - 0x00223B 00:E22B: 64        .byte $64   ; 
- D 3 - I - 0x00223C 00:E22C: 60        .byte $60   ; 
- D 3 - I - 0x00223D 00:E22D: FC        .byte $FC   ; 
- D 3 - I - 0x00223E 00:E22E: 65        .byte $65   ; 
- D 3 - I - 0x00223F 00:E22F: 62        .byte $62   ; 
_off002_E230_0C:
_off002_E230_0D:
_off002_E230_0E:
_off002_E230_0F:
- D 3 - I - 0x002240 00:E230: 00        .byte $00   ; 
- D 3 - I - 0x002241 00:E231: 66        .byte $66   ; 
- D 3 - I - 0x002242 00:E232: 67        .byte $67   ; 
- D 3 - I - 0x002243 00:E233: 60        .byte $60   ; 
- D 3 - I - 0x002244 00:E234: 68        .byte $68   ; 
- D 3 - I - 0x002245 00:E235: 69        .byte $69   ; 
- D 3 - I - 0x002246 00:E236: 62        .byte $62   ; 
_off002_E237_28:
_off002_E237_29:
_off002_E237_2A:
_off002_E237_2B:
- - - - - - 0x002247 00:E237: 00        .byte $00   ; 
- - - - - - 0x002248 00:E238: 6A        .byte $6A   ; 
- - - - - - 0x002249 00:E239: 67        .byte $67   ; 
- - - - - - 0x00224A 00:E23A: 60        .byte $60   ; 
- - - - - - 0x00224B 00:E23B: 6B        .byte $6B   ; 
- - - - - - 0x00224C 00:E23C: 69        .byte $69   ; 
- - - - - - 0x00224D 00:E23D: 62        .byte $62   ; 
_off002_E23E_08:
_off002_E23E_09:
_off002_E23E_24:
_off002_E23E_26:
- D 3 - I - 0x00224E 00:E23E: 00        .byte $00   ; 
- D 3 - I - 0x00224F 00:E23F: FC        .byte $FC   ; 
- D 3 - I - 0x002250 00:E240: 6C        .byte $6C   ; 
- D 3 - I - 0x002251 00:E241: 6D        .byte $6D   ; 
- D 3 - I - 0x002252 00:E242: FC        .byte $FC   ; 
- D 3 - I - 0x002253 00:E243: 6E        .byte $6E   ; 
- D 3 - I - 0x002254 00:E244: 6F        .byte $6F   ; 
_off002_E245_25:
_off002_E245_27:
- D 3 - I - 0x002255 00:E245: 00        .byte $00   ; 
- D 3 - I - 0x002256 00:E246: FC        .byte $FC   ; 
- D 3 - I - 0x002257 00:E247: 6C        .byte $6C   ; 
- D 3 - I - 0x002258 00:E248: 6D        .byte $6D   ; 
- D 3 - I - 0x002259 00:E249: FC        .byte $FC   ; 
- D 3 - I - 0x00225A 00:E24A: 6E        .byte $6E   ; 
- D 3 - I - 0x00225B 00:E24B: 70        .byte $70   ; 
tbl_E24C_lo:
; 00
- D 3 - - - 0x00225C 00:E24C: AF        .byte < _off004_E0AF_00   ; 
- D 3 - - - 0x00225D 00:E24D: B6        .byte < _off004_E0B6_01   ; 
- D 3 - - - 0x00225E 00:E24E: BD        .byte < _off004_E0BD_02   ; 
- D 3 - - - 0x00225F 00:E24F: B6        .byte < _off004_E0B6_03   ; 
- D 3 - - - 0x002260 00:E250: C4        .byte < _off004_E0C4_04   ; 
- D 3 - - - 0x002261 00:E251: CB        .byte < _off004_E0CB_05   ; 
- D 3 - - - 0x002262 00:E252: D2        .byte < _off004_E0D2_06   ; 
- D 3 - - - 0x002263 00:E253: CB        .byte < _off004_E0CB_07   ; 
- D 3 - - - 0x002264 00:E254: D9        .byte < _off004_E0D9_08   ; 
- D 3 - - - 0x002265 00:E255: E0        .byte < _off004_E0E0_09   ; 
- D 3 - - - 0x002266 00:E256: E7        .byte < _off004_E0E7_0A   ; 
- D 3 - - - 0x002267 00:E257: E0        .byte < _off004_E0E0_0B   ; 
- D 3 - - - 0x002268 00:E258: EE        .byte < _off004_E0EE_0C   ; 
- D 3 - - - 0x002269 00:E259: F5        .byte < _off004_E0F5_0D   ; 
- D 3 - - - 0x00226A 00:E25A: FC        .byte < _off004_E0FC_0E   ; 
- D 3 - - - 0x00226B 00:E25B: F5        .byte < _off004_E0F5_0F   ; 
- D 3 - - - 0x00226C 00:E25C: 0A        .byte < _off004_E10A_10   ; 
- D 3 - - - 0x00226D 00:E25D: 0A        .byte < _off004_E10A_11   ; 
- D 3 - - - 0x00226E 00:E25E: 0A        .byte < _off004_E10A_12   ; 
- D 3 - - - 0x00226F 00:E25F: 0A        .byte < _off004_E10A_13   ; 
- D 3 - - - 0x002270 00:E260: 11        .byte < _off004_E111_14   ; 
- D 3 - - - 0x002271 00:E261: 11        .byte < _off004_E111_15   ; 
- D 3 - - - 0x002272 00:E262: 11        .byte < _off004_E111_16   ; 
- D 3 - - - 0x002273 00:E263: 11        .byte < _off004_E111_17   ; 
- D 3 - - - 0x002274 00:E264: 18        .byte < _off004_E118_18   ; 
- - - - - - 0x002275 00:E265: 18        .byte < _off004_E118_19   ; 
- - - - - - 0x002276 00:E266: 18        .byte < _off004_E118_1A   ; 
- - - - - - 0x002277 00:E267: 18        .byte < _off004_E118_1B   ; 
- D 3 - - - 0x002278 00:E268: 1F        .byte < _off004_E11F_1C   ; 
- - - - - - 0x002279 00:E269: 1F        .byte < _off004_E11F_1D   ; 
- - - - - - 0x00227A 00:E26A: 1F        .byte < _off004_E11F_1E   ; 
- - - - - - 0x00227B 00:E26B: 1F        .byte < _off004_E11F_1F   ; 
- D 3 - - - 0x00227C 00:E26C: 26        .byte < _off004_E126_20   ; 
- - - - - - 0x00227D 00:E26D: 26        .byte < _off004_E126_21   ; 
- - - - - - 0x00227E 00:E26E: 26        .byte < _off004_E126_22   ; 
- - - - - - 0x00227F 00:E26F: 26        .byte < _off004_E126_23   ; 
- - - - - - 0x002280 00:E270: 2D        .byte < _off004_E12D_24   ; 
- - - - - - 0x002281 00:E271: 2D        .byte < _off004_E12D_25   ; 
- - - - - - 0x002282 00:E272: 2D        .byte < _off004_E12D_26   ; 
- - - - - - 0x002283 00:E273: 2D        .byte < _off004_E12D_27   ; 
- - - - - - 0x002284 00:E274: 34        .byte < _off004_E134_28   ; 
- - - - - - 0x002285 00:E275: 34        .byte < _off004_E134_29   ; 
- - - - - - 0x002286 00:E276: 34        .byte < _off004_E134_2A   ; 
- - - - - - 0x002287 00:E277: 34        .byte < _off004_E134_2B   ; 
; 2C
- - - - - - 0x002288 00:E278: 3B        .byte < _off004_E13B_2C   ; 
- D 3 - - - 0x002289 00:E279: 42        .byte < _off004_E142_2D   ; 
- D 3 - - - 0x00228A 00:E27A: 49        .byte < _off004_E149_2E   ; 
- D 3 - - - 0x00228B 00:E27B: 42        .byte < _off004_E142_2F   ; 
- D 3 - - - 0x00228C 00:E27C: 50        .byte < _off004_E150_30   ; 
- D 3 - - - 0x00228D 00:E27D: 57        .byte < _off004_E157_31   ; 
- D 3 - - - 0x00228E 00:E27E: 5E        .byte < _off004_E15E_32   ; 
- D 3 - - - 0x00228F 00:E27F: 57        .byte < _off004_E157_33   ; 
- D 3 - - - 0x002290 00:E280: 65        .byte < _off004_E165_34   ; 
- D 3 - - - 0x002291 00:E281: 6C        .byte < _off004_E16C_35   ; 
- D 3 - - - 0x002292 00:E282: 73        .byte < _off004_E173_36   ; 
- D 3 - - - 0x002293 00:E283: 6C        .byte < _off004_E16C_37   ; 
- D 3 - - - 0x002294 00:E284: 7A        .byte < _off004_E17A_38   ; 
- D 3 - - - 0x002295 00:E285: 81        .byte < _off004_E181_39   ; 
- D 3 - - - 0x002296 00:E286: 88        .byte < _off004_E188_3A   ; 
- D 3 - - - 0x002297 00:E287: 81        .byte < _off004_E181_3B   ; 
- D 3 - - - 0x002298 00:E288: 96        .byte < _off004_E196_3C   ; 
- D 3 - - - 0x002299 00:E289: 96        .byte < _off004_E196_3D   ; 
- D 3 - - - 0x00229A 00:E28A: 96        .byte < _off004_E196_3E   ; 
- D 3 - - - 0x00229B 00:E28B: 96        .byte < _off004_E196_3F   ; 
- D 3 - - - 0x00229C 00:E28C: 9D        .byte < _off004_E19D_40   ; 
- D 3 - - - 0x00229D 00:E28D: 9D        .byte < _off004_E19D_41   ; 
- D 3 - - - 0x00229E 00:E28E: 9D        .byte < _off004_E19D_42   ; 
- D 3 - - - 0x00229F 00:E28F: 9D        .byte < _off004_E19D_43   ; 
; 44
- D 3 - - - 0x0022A0 00:E290: A4        .byte < _off004_E1A4_44   ; 
- D 3 - - - 0x0022A1 00:E291: AB        .byte < _off004_E1AB_45   ; 
- D 3 - - - 0x0022A2 00:E292: B2        .byte < _off004_E1B2_46   ; 
- D 3 - - - 0x0022A3 00:E293: AB        .byte < _off004_E1AB_47   ; 
- D 3 - - - 0x0022A4 00:E294: B9        .byte < _off004_E1B9_48   ; 
- D 3 - - - 0x0022A5 00:E295: A4        .byte < _off004_E1A4_49   ; 
- D 3 - - - 0x0022A6 00:E296: B9        .byte < _off004_E1B9_4A   ; 
- D 3 - - - 0x0022A7 00:E297: A4        .byte < _off004_E1A4_4B   ; 
tbl_E298_hi:
; 00
- D 3 - - - 0x0022A8 00:E298: E0        .byte > _off004_E0AF_00   ; 
- D 3 - - - 0x0022A9 00:E299: E0        .byte > _off004_E0B6_01   ; 
- D 3 - - - 0x0022AA 00:E29A: E0        .byte > _off004_E0BD_02   ; 
- D 3 - - - 0x0022AB 00:E29B: E0        .byte > _off004_E0B6_03   ; 
- D 3 - - - 0x0022AC 00:E29C: E0        .byte > _off004_E0C4_04   ; 
- D 3 - - - 0x0022AD 00:E29D: E0        .byte > _off004_E0CB_05   ; 
- D 3 - - - 0x0022AE 00:E29E: E0        .byte > _off004_E0D2_06   ; 
- D 3 - - - 0x0022AF 00:E29F: E0        .byte > _off004_E0CB_07   ; 
- D 3 - - - 0x0022B0 00:E2A0: E0        .byte > _off004_E0D9_08   ; 
- D 3 - - - 0x0022B1 00:E2A1: E0        .byte > _off004_E0E0_09   ; 
- D 3 - - - 0x0022B2 00:E2A2: E0        .byte > _off004_E0E7_0A   ; 
- D 3 - - - 0x0022B3 00:E2A3: E0        .byte > _off004_E0E0_0B   ; 
- D 3 - - - 0x0022B4 00:E2A4: E0        .byte > _off004_E0EE_0C   ; 
- D 3 - - - 0x0022B5 00:E2A5: E0        .byte > _off004_E0F5_0D   ; 
- D 3 - - - 0x0022B6 00:E2A6: E0        .byte > _off004_E0FC_0E   ; 
- D 3 - - - 0x0022B7 00:E2A7: E0        .byte > _off004_E0F5_0F   ; 
- D 3 - - - 0x0022B8 00:E2A8: E1        .byte > _off004_E10A_10   ; 
- D 3 - - - 0x0022B9 00:E2A9: E1        .byte > _off004_E10A_11   ; 
- D 3 - - - 0x0022BA 00:E2AA: E1        .byte > _off004_E10A_12   ; 
- D 3 - - - 0x0022BB 00:E2AB: E1        .byte > _off004_E10A_13   ; 
- D 3 - - - 0x0022BC 00:E2AC: E1        .byte > _off004_E111_14   ; 
- D 3 - - - 0x0022BD 00:E2AD: E1        .byte > _off004_E111_15   ; 
- D 3 - - - 0x0022BE 00:E2AE: E1        .byte > _off004_E111_16   ; 
- D 3 - - - 0x0022BF 00:E2AF: E1        .byte > _off004_E111_17   ; 
- D 3 - - - 0x0022C0 00:E2B0: E1        .byte > _off004_E118_18   ; 
- - - - - - 0x0022C1 00:E2B1: E1        .byte > _off004_E118_19   ; 
- - - - - - 0x0022C2 00:E2B2: E1        .byte > _off004_E118_1A   ; 
- - - - - - 0x0022C3 00:E2B3: E1        .byte > _off004_E118_1B   ; 
- D 3 - - - 0x0022C4 00:E2B4: E1        .byte > _off004_E11F_1C   ; 
- - - - - - 0x0022C5 00:E2B5: E1        .byte > _off004_E11F_1D   ; 
- - - - - - 0x0022C6 00:E2B6: E1        .byte > _off004_E11F_1E   ; 
- - - - - - 0x0022C7 00:E2B7: E1        .byte > _off004_E11F_1F   ; 
- D 3 - - - 0x0022C8 00:E2B8: E1        .byte > _off004_E126_20   ; 
- - - - - - 0x0022C9 00:E2B9: E1        .byte > _off004_E126_21   ; 
- - - - - - 0x0022CA 00:E2BA: E1        .byte > _off004_E126_22   ; 
- - - - - - 0x0022CB 00:E2BB: E1        .byte > _off004_E126_23   ; 
- - - - - - 0x0022CC 00:E2BC: E1        .byte > _off004_E12D_24   ; 
- - - - - - 0x0022CD 00:E2BD: E1        .byte > _off004_E12D_25   ; 
- - - - - - 0x0022CE 00:E2BE: E1        .byte > _off004_E12D_26   ; 
- - - - - - 0x0022CF 00:E2BF: E1        .byte > _off004_E12D_27   ; 
- - - - - - 0x0022D0 00:E2C0: E1        .byte > _off004_E134_28   ; 
- - - - - - 0x0022D1 00:E2C1: E1        .byte > _off004_E134_29   ; 
- - - - - - 0x0022D2 00:E2C2: E1        .byte > _off004_E134_2A   ; 
- - - - - - 0x0022D3 00:E2C3: E1        .byte > _off004_E134_2B   ; 
; 2C
- - - - - - 0x0022D4 00:E2C4: E1        .byte > _off004_E13B_2C   ; 
- D 3 - - - 0x0022D5 00:E2C5: E1        .byte > _off004_E142_2D   ; 
- D 3 - - - 0x0022D6 00:E2C6: E1        .byte > _off004_E149_2E   ; 
- D 3 - - - 0x0022D7 00:E2C7: E1        .byte > _off004_E142_2F   ; 
- D 3 - - - 0x0022D8 00:E2C8: E1        .byte > _off004_E150_30   ; 
- D 3 - - - 0x0022D9 00:E2C9: E1        .byte > _off004_E157_31   ; 
- D 3 - - - 0x0022DA 00:E2CA: E1        .byte > _off004_E15E_32   ; 
- D 3 - - - 0x0022DB 00:E2CB: E1        .byte > _off004_E157_33   ; 
- D 3 - - - 0x0022DC 00:E2CC: E1        .byte > _off004_E165_34   ; 
- D 3 - - - 0x0022DD 00:E2CD: E1        .byte > _off004_E16C_35   ; 
- D 3 - - - 0x0022DE 00:E2CE: E1        .byte > _off004_E173_36   ; 
- D 3 - - - 0x0022DF 00:E2CF: E1        .byte > _off004_E16C_37   ; 
- D 3 - - - 0x0022E0 00:E2D0: E1        .byte > _off004_E17A_38   ; 
- D 3 - - - 0x0022E1 00:E2D1: E1        .byte > _off004_E181_39   ; 
- D 3 - - - 0x0022E2 00:E2D2: E1        .byte > _off004_E188_3A   ; 
- D 3 - - - 0x0022E3 00:E2D3: E1        .byte > _off004_E181_3B   ; 
- D 3 - - - 0x0022E4 00:E2D4: E1        .byte > _off004_E196_3C   ; 
- D 3 - - - 0x0022E5 00:E2D5: E1        .byte > _off004_E196_3D   ; 
- D 3 - - - 0x0022E6 00:E2D6: E1        .byte > _off004_E196_3E   ; 
- D 3 - - - 0x0022E7 00:E2D7: E1        .byte > _off004_E196_3F   ; 
- D 3 - - - 0x0022E8 00:E2D8: E1        .byte > _off004_E19D_40   ; 
- D 3 - - - 0x0022E9 00:E2D9: E1        .byte > _off004_E19D_41   ; 
- D 3 - - - 0x0022EA 00:E2DA: E1        .byte > _off004_E19D_42   ; 
- D 3 - - - 0x0022EB 00:E2DB: E1        .byte > _off004_E19D_43   ; 
; 44
- D 3 - - - 0x0022EC 00:E2DC: E1        .byte > _off004_E1A4_44   ; 
- D 3 - - - 0x0022ED 00:E2DD: E1        .byte > _off004_E1AB_45   ; 
- D 3 - - - 0x0022EE 00:E2DE: E1        .byte > _off004_E1B2_46   ; 
- D 3 - - - 0x0022EF 00:E2DF: E1        .byte > _off004_E1AB_47   ; 
- D 3 - - - 0x0022F0 00:E2E0: E1        .byte > _off004_E1B9_48   ; 
- D 3 - - - 0x0022F1 00:E2E1: E1        .byte > _off004_E1A4_49   ; 
- D 3 - - - 0x0022F2 00:E2E2: E1        .byte > _off004_E1B9_4A   ; 
- D 3 - - - 0x0022F3 00:E2E3: E1        .byte > _off004_E1A4_4B   ; 
tbl_E2E4_lo:
- D 3 - - - 0x0022F4 00:E2E4: 81        .byte < _off001_E181_00   ; 
- D 3 - - - 0x0022F5 00:E2E5: 81        .byte < _off001_E181_01   ; 
- D 3 - - - 0x0022F6 00:E2E6: 8F        .byte < _off001_E18F_02   ; 
- D 3 - - - 0x0022F7 00:E2E7: 81        .byte < _off001_E181_03   ; 
- D 3 - - - 0x0022F8 00:E2E8: F5        .byte < _off001_E0F5_04   ; 
- D 3 - - - 0x0022F9 00:E2E9: F5        .byte < _off001_E0F5_05   ; 
- D 3 - - - 0x0022FA 00:E2EA: 03        .byte < _off001_E103_06   ; 
- D 3 - - - 0x0022FB 00:E2EB: F5        .byte < _off001_E0F5_07   ; 
tbl_E2EC_hi:
- D 3 - - - 0x0022FC 00:E2EC: E1        .byte > _off001_E181_00   ; 
- D 3 - - - 0x0022FD 00:E2ED: E1        .byte > _off001_E181_01   ; 
- D 3 - - - 0x0022FE 00:E2EE: E1        .byte > _off001_E18F_02   ; 
- D 3 - - - 0x0022FF 00:E2EF: E1        .byte > _off001_E181_03   ; 
- D 3 - - - 0x002300 00:E2F0: E0        .byte > _off001_E0F5_04   ; 
- D 3 - - - 0x002301 00:E2F1: E0        .byte > _off001_E0F5_05   ; 
- D 3 - - - 0x002302 00:E2F2: E1        .byte > _off001_E103_06   ; 
- D 3 - - - 0x002303 00:E2F3: E0        .byte > _off001_E0F5_07   ; 
tbl_E2F4_lo:
- D 3 - - - 0x002304 00:E2F4: F8        .byte < _off002_E1F8_00   ; 
- D 3 - - - 0x002305 00:E2F5: FF        .byte < _off002_E1FF_01   ; 
- D 3 - - - 0x002306 00:E2F6: 06        .byte < _off002_E206_02   ; 
- D 3 - - - 0x002307 00:E2F7: FF        .byte < _off002_E1FF_03   ; 
- D 3 - - - 0x002308 00:E2F8: 0D        .byte < _off002_E20D_04   ; 
- D 3 - - - 0x002309 00:E2F9: 14        .byte < _off002_E214_05   ; 
- D 3 - - - 0x00230A 00:E2FA: 1B        .byte < _off002_E21B_06   ; 
- D 3 - - - 0x00230B 00:E2FB: 14        .byte < _off002_E214_07   ; 
- D 3 - - - 0x00230C 00:E2FC: 3E        .byte < _off002_E23E_08   ; 
- D 3 - - - 0x00230D 00:E2FD: 3E        .byte < _off002_E23E_09   ; 
- D 3 - - - 0x00230E 00:E2FE: 22        .byte < _off002_E222_0A   ; 
- D 3 - - - 0x00230F 00:E2FF: 29        .byte < _off002_E229_0B   ; 
- D 3 - - - 0x002310 00:E300: 30        .byte < _off002_E230_0C   ; 
- D 3 - - - 0x002311 00:E301: 30        .byte < _off002_E230_0D   ; 
- D 3 - - - 0x002312 00:E302: 30        .byte < _off002_E230_0E   ; 
- D 3 - - - 0x002313 00:E303: 30        .byte < _off002_E230_0F   ; 
- D 3 - - - 0x002314 00:E304: C0        .byte < _off002_E1C0_10   ; 
- D 3 - - - 0x002315 00:E305: C0        .byte < _off002_E1C0_11   ; 
- D 3 - - - 0x002316 00:E306: C0        .byte < _off002_E1C0_12   ; 
- D 3 - - - 0x002317 00:E307: C0        .byte < _off002_E1C0_13   ; 
- D 3 - - - 0x002318 00:E308: C0        .byte < _off002_E1C0_14   ; 
- D 3 - - - 0x002319 00:E309: C7        .byte < _off002_E1C7_15   ; 
- D 3 - - - 0x00231A 00:E30A: C0        .byte < _off002_E1C0_16   ; 
- D 3 - - - 0x00231B 00:E30B: C7        .byte < _off002_E1C7_17   ; 
- D 3 - - - 0x00231C 00:E30C: CE        .byte < _off002_E1CE_18   ; 
- D 3 - - - 0x00231D 00:E30D: D5        .byte < _off002_E1D5_19   ; 
- D 3 - - - 0x00231E 00:E30E: CE        .byte < _off002_E1CE_1A   ; 
- D 3 - - - 0x00231F 00:E30F: D5        .byte < _off002_E1D5_1B   ; 
- D 3 - - - 0x002320 00:E310: DC        .byte < _off002_E1DC_1C   ; 
- D 3 - - - 0x002321 00:E311: E3        .byte < _off002_E1E3_1D   ; 
- D 3 - - - 0x002322 00:E312: DC        .byte < _off002_E1DC_1E   ; 
- D 3 - - - 0x002323 00:E313: E3        .byte < _off002_E1E3_1F   ; 
- D 3 - - - 0x002324 00:E314: EA        .byte < _off002_E1EA_20   ; 
- D 3 - - - 0x002325 00:E315: F1        .byte < _off002_E1F1_21   ; 
- D 3 - - - 0x002326 00:E316: EA        .byte < _off002_E1EA_22   ; 
- D 3 - - - 0x002327 00:E317: F1        .byte < _off002_E1F1_23   ; 
- D 3 - - - 0x002328 00:E318: 3E        .byte < _off002_E23E_24   ; 
- D 3 - - - 0x002329 00:E319: 45        .byte < _off002_E245_25   ; 
- D 3 - - - 0x00232A 00:E31A: 3E        .byte < _off002_E23E_26   ; 
- D 3 - - - 0x00232B 00:E31B: 45        .byte < _off002_E245_27   ; 
- - - - - - 0x00232C 00:E31C: 37        .byte < _off002_E237_28   ; 
- - - - - - 0x00232D 00:E31D: 37        .byte < _off002_E237_29   ; 
- - - - - - 0x00232E 00:E31E: 37        .byte < _off002_E237_2A   ; 
- - - - - - 0x00232F 00:E31F: 37        .byte < _off002_E237_2B   ; 
tbl_E320_hi:
- D 3 - - - 0x002330 00:E320: E1        .byte > _off002_E1F8_00   ; 
- D 3 - - - 0x002331 00:E321: E1        .byte > _off002_E1FF_01   ; 
- D 3 - - - 0x002332 00:E322: E2        .byte > _off002_E206_02   ; 
- D 3 - - - 0x002333 00:E323: E1        .byte > _off002_E1FF_03   ; 
- D 3 - - - 0x002334 00:E324: E2        .byte > _off002_E20D_04   ; 
- D 3 - - - 0x002335 00:E325: E2        .byte > _off002_E214_05   ; 
- D 3 - - - 0x002336 00:E326: E2        .byte > _off002_E21B_06   ; 
- D 3 - - - 0x002337 00:E327: E2        .byte > _off002_E214_07   ; 
- D 3 - - - 0x002338 00:E328: E2        .byte > _off002_E23E_08   ; 
- D 3 - - - 0x002339 00:E329: E2        .byte > _off002_E23E_09   ; 
- D 3 - - - 0x00233A 00:E32A: E2        .byte > _off002_E222_0A   ; 
- D 3 - - - 0x00233B 00:E32B: E2        .byte > _off002_E229_0B   ; 
- D 3 - - - 0x00233C 00:E32C: E2        .byte > _off002_E230_0C   ; 
- D 3 - - - 0x00233D 00:E32D: E2        .byte > _off002_E230_0D   ; 
- D 3 - - - 0x00233E 00:E32E: E2        .byte > _off002_E230_0E   ; 
- D 3 - - - 0x00233F 00:E32F: E2        .byte > _off002_E230_0F   ; 
- D 3 - - - 0x002340 00:E330: E1        .byte > _off002_E1C0_10   ; 
- D 3 - - - 0x002341 00:E331: E1        .byte > _off002_E1C0_11   ; 
- D 3 - - - 0x002342 00:E332: E1        .byte > _off002_E1C0_12   ; 
- D 3 - - - 0x002343 00:E333: E1        .byte > _off002_E1C0_13   ; 
- D 3 - - - 0x002344 00:E334: E1        .byte > _off002_E1C0_14   ; 
- D 3 - - - 0x002345 00:E335: E1        .byte > _off002_E1C7_15   ; 
- D 3 - - - 0x002346 00:E336: E1        .byte > _off002_E1C0_16   ; 
- D 3 - - - 0x002347 00:E337: E1        .byte > _off002_E1C7_17   ; 
- D 3 - - - 0x002348 00:E338: E1        .byte > _off002_E1CE_18   ; 
- D 3 - - - 0x002349 00:E339: E1        .byte > _off002_E1D5_19   ; 
- D 3 - - - 0x00234A 00:E33A: E1        .byte > _off002_E1CE_1A   ; 
- D 3 - - - 0x00234B 00:E33B: E1        .byte > _off002_E1D5_1B   ; 
- D 3 - - - 0x00234C 00:E33C: E1        .byte > _off002_E1DC_1C   ; 
- D 3 - - - 0x00234D 00:E33D: E1        .byte > _off002_E1E3_1D   ; 
- D 3 - - - 0x00234E 00:E33E: E1        .byte > _off002_E1DC_1E   ; 
- D 3 - - - 0x00234F 00:E33F: E1        .byte > _off002_E1E3_1F   ; 
- D 3 - - - 0x002350 00:E340: E1        .byte > _off002_E1EA_20   ; 
- D 3 - - - 0x002351 00:E341: E1        .byte > _off002_E1F1_21   ; 
- D 3 - - - 0x002352 00:E342: E1        .byte > _off002_E1EA_22   ; 
- D 3 - - - 0x002353 00:E343: E1        .byte > _off002_E1F1_23   ; 
- D 3 - - - 0x002354 00:E344: E2        .byte > _off002_E23E_24   ; 
- D 3 - - - 0x002355 00:E345: E2        .byte > _off002_E245_25   ; 
- D 3 - - - 0x002356 00:E346: E2        .byte > _off002_E23E_26   ; 
- D 3 - - - 0x002357 00:E347: E2        .byte > _off002_E245_27   ; 
- - - - - - 0x002358 00:E348: E2        .byte > _off002_E237_28   ; 
- - - - - - 0x002359 00:E349: E2        .byte > _off002_E237_29   ; 
- - - - - - 0x00235A 00:E34A: E2        .byte > _off002_E237_2A   ; 
- - - - - - 0x00235B 00:E34B: E2        .byte > _off002_E237_2B   ; 
tbl_E34C:
; case 1
- D 3 - - - 0x00235C 00:E34C: 44        .byte $44   ; 
- D 3 - - - 0x00235D 00:E34D: 2C        .byte $2C   ; 
- D 3 - - - 0x00235E 00:E34E: 00        .byte $00   ; 
; case 2
- - - - - - 0x00235F 00:E34F: 00        .byte $00   ; 
- D 3 - - - 0x002360 00:E350: 00        .byte $00   ; 
- D 3 - - - 0x002361 00:E351: 04        .byte $04   ; 
tbl_E352:
- D 3 - - - 0x002362 00:E352: 24        .byte $24   ; 
- D 3 - - - 0x002363 00:E353: 08        .byte $08   ; 
- D 3 - - - 0x002364 00:E354: 00        .byte $00   ; 
_off003_E355_00:
; cases 01-06 have 7 bytes, while this one only has 5
- - - - - - 0x002365 00:E355: 00        .byte $00   ; 
- - - - - - 0x002366 00:E356: FC        .byte $FC   ; 
- - - - - - 0x002367 00:E357: FC        .byte $FC   ; 
- - - - - - 0x002368 00:E358: FE        .byte $FE   ; 
- - - - - - 0x002369 00:E359: FC        .byte $FC   ; 
_off003_E35A_01:
- D 3 - I - 0x00236A 00:E35A: 00        .byte $00   ; 
- D 3 - I - 0x00236B 00:E35B: 7B        .byte $7B   ; 
- D 3 - I - 0x00236C 00:E35C: FC        .byte $FC   ; 
- D 3 - I - 0x00236D 00:E35D: FC        .byte $FC   ; 
- D 3 - I - 0x00236E 00:E35E: 7C        .byte $7C   ; 
- D 3 - I - 0x00236F 00:E35F: FC        .byte $FC   ; 
- D 3 - I - 0x002370 00:E360: FC        .byte $FC   ; 
_off003_E361_02:
- D 3 - I - 0x002371 00:E361: 00        .byte $00   ; 
- D 3 - I - 0x002372 00:E362: 7D        .byte $7D   ; 
- D 3 - I - 0x002373 00:E363: 7E        .byte $7E   ; 
- D 3 - I - 0x002374 00:E364: FC        .byte $FC   ; 
- D 3 - I - 0x002375 00:E365: 7F        .byte $7F   ; 
- D 3 - I - 0x002376 00:E366: 80        .byte $80   ; 
- D 3 - I - 0x002377 00:E367: FC        .byte $FC   ; 
_off003_E368_03:
- D 3 - I - 0x002378 00:E368: 00        .byte $00   ; 
- D 3 - I - 0x002379 00:E369: 81        .byte $81   ; 
- D 3 - I - 0x00237A 00:E36A: 82        .byte $82   ; 
- D 3 - I - 0x00237B 00:E36B: 83        .byte $83   ; 
- D 3 - I - 0x00237C 00:E36C: 84        .byte $84   ; 
- D 3 - I - 0x00237D 00:E36D: 85        .byte $85   ; 
- D 3 - I - 0x00237E 00:E36E: 86        .byte $86   ; 
_off003_E36F_04:
- D 3 - I - 0x00237F 00:E36F: 00        .byte $00   ; 
- D 3 - I - 0x002380 00:E370: 87        .byte $87   ; 
- D 3 - I - 0x002381 00:E371: 88        .byte $88   ; 
- D 3 - I - 0x002382 00:E372: FC        .byte $FC   ; 
- D 3 - I - 0x002383 00:E373: 89        .byte $89   ; 
- D 3 - I - 0x002384 00:E374: 8A        .byte $8A   ; 
- D 3 - I - 0x002385 00:E375: FC        .byte $FC   ; 
_off003_E376_05:
- D 3 - I - 0x002386 00:E376: 00        .byte $00   ; 
- D 3 - I - 0x002387 00:E377: 8B        .byte $8B   ; 
- D 3 - I - 0x002388 00:E378: 8C        .byte $8C   ; 
- D 3 - I - 0x002389 00:E379: FC        .byte $FC   ; 
- D 3 - I - 0x00238A 00:E37A: 8D        .byte $8D   ; 
- D 3 - I - 0x00238B 00:E37B: 8E        .byte $8E   ; 
- D 3 - I - 0x00238C 00:E37C: FC        .byte $FC   ; 
_off003_E37D_06:
- D 3 - I - 0x00238D 00:E37D: 00        .byte $00   ; 
- D 3 - I - 0x00238E 00:E37E: 8F        .byte $8F   ; 
- D 3 - I - 0x00238F 00:E37F: 90        .byte $90   ; 
- D 3 - I - 0x002390 00:E380: FC        .byte $FC   ; 
- D 3 - I - 0x002391 00:E381: FC        .byte $FC   ; 
- D 3 - I - 0x002392 00:E382: FC        .byte $FC   ; 
- D 3 - I - 0x002393 00:E383: FC        .byte $FC   ; 
tbl_E384_lo:
- - - - - - 0x002394 00:E384: 55        .byte < _off003_E355_00   ; unused index?
- D 3 - - - 0x002395 00:E385: 5A        .byte < _off003_E35A_01   ; 
- D 3 - - - 0x002396 00:E386: 61        .byte < _off003_E361_02   ; 
- D 3 - - - 0x002397 00:E387: 68        .byte < _off003_E368_03   ; 
- D 3 - - - 0x002398 00:E388: 6F        .byte < _off003_E36F_04   ; 
- D 3 - - - 0x002399 00:E389: 76        .byte < _off003_E376_05   ; 
- D 3 - - - 0x00239A 00:E38A: 7D        .byte < _off003_E37D_06   ; 
tbl_E38B_hi:
- - - - - - 0x00239B 00:E38B: E3        .byte > _off003_E355_00   ; unused index?
- D 3 - - - 0x00239C 00:E38C: E3        .byte > _off003_E35A_01   ; 
- D 3 - - - 0x00239D 00:E38D: E3        .byte > _off003_E361_02   ; 
- D 3 - - - 0x00239E 00:E38E: E3        .byte > _off003_E368_03   ; 
- D 3 - - - 0x00239F 00:E38F: E3        .byte > _off003_E36F_04   ; 
- D 3 - - - 0x0023A0 00:E390: E3        .byte > _off003_E376_05   ; 
- D 3 - - - 0x0023A1 00:E391: E3        .byte > _off003_E37D_06   ; 
tbl_E392:
- D 3 - - - 0x0023A2 00:E392: 20        .byte $20   ; 
- D 3 - - - 0x0023A3 00:E393: 38        .byte $38   ; 
- D 3 - - - 0x0023A4 00:E394: 50        .byte $50   ; 
- D 3 - - - 0x0023A5 00:E395: 68        .byte $68   ; 
- D 3 - - - 0x0023A6 00:E396: 80        .byte $80   ; 
- D 3 - - - 0x0023A7 00:E397: 98        .byte $98   ; 
- D 3 - - - 0x0023A8 00:E398: B0        .byte $B0   ; 
- D 3 - - - 0x0023A9 00:E399: C8        .byte $C8   ; 
- D 3 - - - 0x0023AA 00:E39A: 08        .byte $08   ; 
tbl_E39B:
- D 3 - - - 0x0023AB 00:E39B: 20        .byte $20   ; 
- D 3 - - - 0x0023AC 00:E39C: 38        .byte $38   ; 
- D 3 - - - 0x0023AD 00:E39D: C8        .byte $C8   ; 
- D 3 - - - 0x0023AE 00:E39E: B0        .byte $B0   ; 
- D 3 - - - 0x0023AF 00:E39F: 98        .byte $98   ; 
- D 3 - - - 0x0023B0 00:E3A0: 80        .byte $80   ; 
- D 3 - - - 0x0023B1 00:E3A1: 68        .byte $68   ; 
- D 3 - - - 0x0023B2 00:E3A2: 50        .byte $50   ; 
- D 3 - - - 0x0023B3 00:E3A3: 08        .byte $08   ; 
sub_E3A4:
C - - - - - 0x0023B4 00:E3A4: BD 92 E3  LDA tbl_E392,X                                             ; Set Pointer from the first table
C - - - - - 0x0023B7 00:E3A7: 85 1F     STA ram_001F_gfx_enemy_data_pointer_1f
C - - - - - 0x0023B9 00:E3A9: A5 19     LDA ram_0019_f_counter
C - - - - - 0x0023BB 00:E3AB: 4A        LSR                                                        ; Every 2 frames
C - - - - - 0x0023BC 00:E3AC: 90 05     BCC bra_E3B3                                               ; Set Pointer from the second table
C - - - - - 0x0023BE 00:E3AE: BD 9B E3  LDA tbl_E39B,X
C - - - - - 0x0023C1 00:E3B1: 85 1F     STA ram_001F_gfx_enemy_data_pointer_1f
bra_E3B3:
C - - - - - 0x0023C3 00:E3B3: A9 02     LDA #$02                                                   ; Set Pointer to $02xx
C - - - - - 0x0023C5 00:E3B5: 85 20     STA ram_0020_gfx_enemy_data_pointer_1f                     ; (OAM)
C - - - - - 0x0023C7 00:E3B7: B5 88     LDA ram_0088_object_balloons_p1,X                          ; If Object X Balloons >= 0
C - - - - - 0x0023C9 00:E3B9: 10 14     BPL bra_E3CF
C - - - - - 0x0023CB 00:E3BB: C9 FF     CMP #$FF                                                   ; If Object X Balloons == -1
C - - - - - 0x0023CD 00:E3BD: F0 03     BEQ bra_E3C2
C - - - - - 0x0023CF 00:E3BF: 4C D5 E4  JMP loc_E4D5
bra_E3C2:
C - - - - - 0x0023D2 00:E3C2: A0 14     LDY #$14
bra_E3C4:
C - - - - - 0x0023D4 00:E3C4: A9 F0     LDA #$F0
C - - - - - 0x0023D6 00:E3C6: 91 1F     STA (ram_001F_gfx_enemy_data_pointer_1f),Y
C - - - - - 0x0023D8 00:E3C8: 88        DEY                                                        ; Hide 20 sprites
C - - - - - 0x0023D9 00:E3C9: 88        DEY
C - - - - - 0x0023DA 00:E3CA: 88        DEY
C - - - - - 0x0023DB 00:E3CB: 88        DEY
C - - - - - 0x0023DC 00:E3CC: 10 F6     BPL bra_E3C4
C - - - - - 0x0023DE 00:E3CE: 60        RTS
bra_E3CF:
C - - - - - 0x0023DF 00:E3CF: E0 08     CPX #$08                                                   ; If Object is Fish
C - - - - - 0x0023E1 00:E3D1: F0 48     BEQ bra_E41B
C - - - - - 0x0023E3 00:E3D3: B5 7F     LDA ram_007F_object_status_p1,X
C - - - - - 0x0023E5 00:E3D5: 0A        ASL                                                        ; (Object Status * 4) + Animation Frame
C - - - - - 0x0023E6 00:E3D6: 0A        ASL
C - - - - - 0x0023E7 00:E3D7: 7D 36 04  ADC ram_0436_animation_f_p1,X
C - - - - - 0x0023EA 00:E3DA: E0 02     CPX #$02                                                   ; If Object is a Player
C - - - - - 0x0023EC 00:E3DC: B0 2A     BCS bra_E408
C - - - - - 0x0023EE 00:E3DE: B4 88     LDY ram_0088_object_balloons_p1,X
C - - - - - 0x0023F0 00:E3E0: 79 4C E3  ADC tbl_E34C,Y                                             ; Y = (Object Status * 4) + Animation Frame
C - - - - - 0x0023F3 00:E3E3: A8        TAY                                                        ; + [tbl_E34C + Balloons]
C - - - - - 0x0023F4 00:E3E4: B9 4C E2  LDA tbl_E24C_lo,Y
C - - - - - 0x0023F7 00:E3E7: 85 1D     STA ram_001D_loading_pointer_1d                            ; Set Pointer
C - - - - - 0x0023F9 00:E3E9: B9 98 E2  LDA tbl_E298_hi,Y
C - - - - - 0x0023FC 00:E3EC: 85 1E     STA ram_001E_loading_pointer_1d
C - - - - - 0x0023FE 00:E3EE: B5 BD     LDA ram_00BD_p1_invincibility_flag,X                       ; If Player X is invincible
C - - - - - 0x002400 00:E3F0: F0 37     BEQ bra_E429
C - - - - - 0x002402 00:E3F2: B4 88     LDY ram_0088_object_balloons_p1,X                          ; Y = Player X Balloons
C - - - - - 0x002404 00:E3F4: B9 4F E3  LDA tbl_E34C + 3,Y
C - - - - - 0x002407 00:E3F7: 7D 36 04  ADC ram_0436_animation_f_p1,X                              ; Y = [tbl_E34C+3 + Balloons + Frame]
C - - - - - 0x00240A 00:E3FA: A8        TAY
C - - - - - 0x00240B 00:E3FB: B9 E4 E2  LDA tbl_E2E4_lo,Y
C - - - - - 0x00240E 00:E3FE: 85 1D     STA ram_001D_loading_pointer_1d                            ; Set Pointer
C - - - - - 0x002410 00:E400: B9 EC E2  LDA tbl_E2EC_hi,Y
C - - - - - 0x002413 00:E403: 85 1E     STA ram_001E_loading_pointer_1d
C - - - - - 0x002415 00:E405: 4C 29 E4  JMP loc_E429
bra_E408:
C - - - - - 0x002418 00:E408: B4 88     LDY ram_0088_object_balloons_p1,X
C - - - - - 0x00241A 00:E40A: 18        CLC
C - - - - - 0x00241B 00:E40B: 79 52 E3  ADC tbl_E352,Y
C - - - - - 0x00241E 00:E40E: A8        TAY
C - - - - - 0x00241F 00:E40F: B9 F4 E2  LDA tbl_E2F4_lo,Y
C - - - - - 0x002422 00:E412: 85 1D     STA ram_001D_loading_pointer_1d                            ; Set Pointer
C - - - - - 0x002424 00:E414: B9 20 E3  LDA tbl_E320_hi,Y
C - - - - - 0x002427 00:E417: 85 1E     STA ram_001E_loading_pointer_1d
C - - - - - 0x002429 00:E419: D0 0E     BNE bra_E429
bra_E41B:
C - - - - - 0x00242B 00:E41B: B4 7F     LDY ram_007F_object_status_p1,X
C - - - - - 0x00242D 00:E41D: 30 A3     BMI bra_E3C2
C - - - - - 0x00242F 00:E41F: B9 84 E3  LDA tbl_E384_lo,Y
C - - - - - 0x002432 00:E422: 85 1D     STA ram_001D_loading_pointer_1d                            ; Set Pointer
C - - - - - 0x002434 00:E424: B9 8B E3  LDA tbl_E38B_hi,Y
C - - - - - 0x002437 00:E427: 85 1E     STA ram_001E_loading_pointer_1d
bra_E429:
loc_E429:
C D 3 - - - 0x002439 00:E429: B5 91     LDA ram_0091_object_x_pos_int_p1,X
C - - - - - 0x00243B 00:E42B: 85 15     STA ram_0015_temp_player_x_trip_rank_score
C - - - - - 0x00243D 00:E42D: B5 9A     LDA ram_009A_object_y_pos_int_p1,X
C - - - - - 0x00243F 00:E42F: 85 12     STA ram_0012_temp
C - - - - - 0x002441 00:E431: 8A        TXA
C - - - - - 0x002442 00:E432: F0 10     BEQ bra_E444
C - - - - - 0x002444 00:E434: E0 01     CPX #$01
C - - - - - 0x002446 00:E436: D0 04     BNE bra_E43C
C - - - - - 0x002448 00:E438: A9 01     LDA #$01
C - - - - - 0x00244A 00:E43A: D0 08     BNE bra_E444
bra_E43C:
C - - - - - 0x00244C 00:E43C: BD 51 04  LDA ram_0451_object_type_p1,X
C - - - - - 0x00244F 00:E43F: 18        CLC
C - - - - - 0x002450 00:E440: 69 02     ADC #$02
C - - - - - 0x002452 00:E442: 29 03     AND #$03
bra_E444:
C - - - - - 0x002454 00:E444: BC 48 04  LDY ram_0448_direction_p1,X
C - - - - - 0x002457 00:E447: F0 02     BEQ bra_E44B
C - - - - - 0x002459 00:E449: 09 40     ORA #$40
bra_E44B:
C - - - - - 0x00245B 00:E44B: B4 88     LDY ram_0088_object_balloons_p1,X
C - - - - - 0x00245D 00:E44D: C0 02     CPY #$02
C - - - - - 0x00245F 00:E44F: D0 08     BNE bra_E459
C - - - - - 0x002461 00:E451: B4 7F     LDY ram_007F_object_status_p1,X
C - - - - - 0x002463 00:E453: C0 05     CPY #$05
C - - - - - 0x002465 00:E455: D0 02     BNE bra_E459
C - - - - - 0x002467 00:E457: 49 40     EOR #$40
bra_E459:
C - - - - - 0x002469 00:E459: B4 9A     LDY ram_009A_object_y_pos_int_p1,X
C - - - - - 0x00246B 00:E45B: C0 C9     CPY #$C9
C - - - - - 0x00246D 00:E45D: B0 04     BCS bra_E463
C - - - - - 0x00246F 00:E45F: E0 09     CPX #$09
C - - - - - 0x002471 00:E461: D0 02     BNE bra_E465
bra_E463:
C - - - - - 0x002473 00:E463: 09 20     ORA #$20
bra_E465:
C - - - - - 0x002475 00:E465: 85 14     STA ram_0014_temp
C - - - - - 0x002477 00:E467: A9 43     LDA #< tbl_E043
C - - - - - 0x002479 00:E469: 85 21     STA ram_0021_hi_score_pointer_21
C - - - - - 0x00247B 00:E46B: A9 E0     LDA #> tbl_E043
C - - - - - 0x00247D 00:E46D: 85 22     STA ram_0022_hi_score_pointer_21
C - - - - - 0x00247F 00:E46F: BD 48 04  LDA ram_0448_direction_p1,X
C - - - - - 0x002482 00:E472: F0 08     BEQ bra_E47C
C - - - - - 0x002484 00:E474: A9 79     LDA #< tbl_E079
C - - - - - 0x002486 00:E476: 85 21     STA ram_0021_hi_score_pointer_21
C - - - - - 0x002488 00:E478: A9 E0     LDA #> tbl_E079
C - - - - - 0x00248A 00:E47A: 85 22     STA ram_0022_hi_score_pointer_21
bra_E47C:
C - - - - - 0x00248C 00:E47C: A0 00     LDY #$00
C - - - - - 0x00248E 00:E47E: B1 1D     LDA (ram_001D_loading_pointer_1d),Y
C - - - - - 0x002490 00:E480: E6 1D     INC ram_001D_loading_pointer_1d
C - - - - - 0x002492 00:E482: D0 02     BNE bra_E486
C - - - - - 0x002494 00:E484: E6 1E     INC ram_001E_loading_pointer_1d
bra_E486:
C - - - - - 0x002496 00:E486: 0A        ASL
C - - - - - 0x002497 00:E487: 85 13     STA ram_0013_temp
C - - - - - 0x002499 00:E489: 0A        ASL
C - - - - - 0x00249A 00:E48A: 65 13     ADC ram_0013_temp
C - - - - - 0x00249C 00:E48C: 65 21     ADC ram_0021_hi_score_pointer_21
C - - - - - 0x00249E 00:E48E: 85 21     STA ram_0021_hi_score_pointer_21
C - - - - - 0x0024A0 00:E490: 90 02     BCC bra_E494
- - - - - - 0x0024A2 00:E492: E6 22     INC ram_0022_hi_score_pointer_21
bra_E494:
C - - - - - 0x0024A4 00:E494: 8A        TXA
C - - - - - 0x0024A5 00:E495: 48        PHA
C - - - - - 0x0024A6 00:E496: A2 05     LDX #$05
C - - - - - 0x0024A8 00:E498: A0 00     LDY #$00
bra_E49A:
C - - - - - 0x0024AA 00:E49A: A5 12     LDA ram_0012_temp
C - - - - - 0x0024AC 00:E49C: 18        CLC
C - - - - - 0x0024AD 00:E49D: 7D 3D E0  ADC tbl_E03D,X
C - - - - - 0x0024B0 00:E4A0: 91 1F     STA (ram_001F_gfx_enemy_data_pointer_1f),Y
C - - - - - 0x0024B2 00:E4A2: 85 12     STA ram_0012_temp
C - - - - - 0x0024B4 00:E4A4: C8        INY
C - - - - - 0x0024B5 00:E4A5: 84 13     STY ram_0013_temp
C - - - - - 0x0024B7 00:E4A7: A0 00     LDY #$00
C - - - - - 0x0024B9 00:E4A9: B1 1D     LDA (ram_001D_loading_pointer_1d),Y
C - - - - - 0x0024BB 00:E4AB: E6 1D     INC ram_001D_loading_pointer_1d
C - - - - - 0x0024BD 00:E4AD: D0 02     BNE bra_E4B1
C - - - - - 0x0024BF 00:E4AF: E6 1E     INC ram_001E_loading_pointer_1d
bra_E4B1:
C - - - - - 0x0024C1 00:E4B1: A4 13     LDY ram_0013_temp
C - - - - - 0x0024C3 00:E4B3: 91 1F     STA (ram_001F_gfx_enemy_data_pointer_1f),Y
C - - - - - 0x0024C5 00:E4B5: C8        INY
C - - - - - 0x0024C6 00:E4B6: A5 14     LDA ram_0014_temp
C - - - - - 0x0024C8 00:E4B8: 91 1F     STA (ram_001F_gfx_enemy_data_pointer_1f),Y
C - - - - - 0x0024CA 00:E4BA: C8        INY
C - - - - - 0x0024CB 00:E4BB: 84 13     STY ram_0013_temp
C - - - - - 0x0024CD 00:E4BD: A0 00     LDY #$00
C - - - - - 0x0024CF 00:E4BF: A5 15     LDA ram_0015_temp_player_x_trip_rank_score
C - - - - - 0x0024D1 00:E4C1: 18        CLC
C - - - - - 0x0024D2 00:E4C2: 71 21     ADC (ram_0021_hi_score_pointer_21),Y
C - - - - - 0x0024D4 00:E4C4: E6 21     INC ram_0021_hi_score_pointer_21
C - - - - - 0x0024D6 00:E4C6: D0 02     BNE bra_E4CA
- - - - - - 0x0024D8 00:E4C8: E6 22     INC ram_0022_hi_score_pointer_21
bra_E4CA:
C - - - - - 0x0024DA 00:E4CA: A4 13     LDY ram_0013_temp
C - - - - - 0x0024DC 00:E4CC: 91 1F     STA (ram_001F_gfx_enemy_data_pointer_1f),Y
C - - - - - 0x0024DE 00:E4CE: C8        INY
C - - - - - 0x0024DF 00:E4CF: CA        DEX
C - - - - - 0x0024E0 00:E4D0: 10 C8     BPL bra_E49A
C - - - - - 0x0024E2 00:E4D2: 68        PLA
C - - - - - 0x0024E3 00:E4D3: AA        TAX
C - - - - - 0x0024E4 00:E4D4: 60        RTS
loc_E4D5:
C D 3 - - - 0x0024E5 00:E4D5: 8A        TXA
C - - - - - 0x0024E6 00:E4D6: 48        PHA
C - - - - - 0x0024E7 00:E4D7: A4 1F     LDY ram_001F_gfx_enemy_data_pointer_1f
C - - - - - 0x0024E9 00:E4D9: B5 9A     LDA ram_009A_object_y_pos_int_p1,X
C - - - - - 0x0024EB 00:E4DB: 99 00 02  STA ram_0200_sprite_00_y,Y
C - - - - - 0x0024EE 00:E4DE: 99 04 02  STA ram_0204_sprite_01_y,Y
C - - - - - 0x0024F1 00:E4E1: 18        CLC
C - - - - - 0x0024F2 00:E4E2: 69 08     ADC #$08
C - - - - - 0x0024F4 00:E4E4: 99 08 02  STA ram_0208_sprite_02_y,Y
C - - - - - 0x0024F7 00:E4E7: 99 0C 02  STA ram_020C_sprite_03_y,Y
C - - - - - 0x0024FA 00:E4EA: A9 F0     LDA #$F0
C - - - - - 0x0024FC 00:E4EC: 99 10 02  STA ram_0210_sprite_04_y,Y
C - - - - - 0x0024FF 00:E4EF: 99 14 02  STA ram_0214_sprite_05_y,Y
C - - - - - 0x002502 00:E4F2: B5 91     LDA ram_0091_object_x_pos_int_p1,X
C - - - - - 0x002504 00:E4F4: 99 03 02  STA ram_0203_sprite_00_x,Y
C - - - - - 0x002507 00:E4F7: 99 0B 02  STA ram_020B_sprite_02_x,Y
C - - - - - 0x00250A 00:E4FA: 18        CLC
C - - - - - 0x00250B 00:E4FB: 69 08     ADC #$08
C - - - - - 0x00250D 00:E4FD: 99 07 02  STA ram_0207_sprite_01_x,Y
C - - - - - 0x002510 00:E500: 99 0F 02  STA ram_020F_sprite_03_x,Y
C - - - - - 0x002513 00:E503: B5 9A     LDA ram_009A_object_y_pos_int_p1,X
C - - - - - 0x002515 00:E505: C9 D0     CMP #$D0
C - - - - - 0x002517 00:E507: A9 03     LDA #$03
C - - - - - 0x002519 00:E509: 90 02     BCC bra_E50D
C - - - - - 0x00251B 00:E50B: A9 23     LDA #$23
bra_E50D:
C - - - - - 0x00251D 00:E50D: 99 02 02  STA ram_0202_sprite_00_attributes,Y
C - - - - - 0x002520 00:E510: B5 7F     LDA ram_007F_object_status_p1,X
C - - - - - 0x002522 00:E512: D0 3F     BNE bra_E553
C - - - - - 0x002524 00:E514: B9 02 02  LDA ram_0202_sprite_00_attributes,Y
C - - - - - 0x002527 00:E517: 99 06 02  STA ram_0206_sprite_01_attributes,Y
C - - - - - 0x00252A 00:E51A: 99 0A 02  STA ram_020A_sprite_02_attributes,Y
C - - - - - 0x00252D 00:E51D: 99 0E 02  STA ram_020E_sprite_03_attributes,Y
C - - - - - 0x002530 00:E520: A9 DA     LDA #$DA
C - - - - - 0x002532 00:E522: 99 01 02  STA ram_0201_sprite_00_tile,Y
C - - - - - 0x002535 00:E525: A9 DB     LDA #$DB
C - - - - - 0x002537 00:E527: 99 05 02  STA ram_0205_sprite_01_tile,Y
C - - - - - 0x00253A 00:E52A: A9 DC     LDA #$DC
C - - - - - 0x00253C 00:E52C: 99 09 02  STA ram_0209_sprite_02_tile,Y
C - - - - - 0x00253F 00:E52F: A9 DD     LDA #$DD
C - - - - - 0x002541 00:E531: 99 0D 02  STA ram_020D_sprite_03_tile,Y
C - - - - - 0x002544 00:E534: A6 1F     LDX ram_001F_gfx_enemy_data_pointer_1f
C - - - - - 0x002546 00:E536: A5 19     LDA ram_0019_f_counter
C - - - - - 0x002548 00:E538: 29 20     AND #$20
C - - - - - 0x00254A 00:E53A: F0 14     BEQ bra_E550
C - - - - - 0x00254C 00:E53C: A5 19     LDA ram_0019_f_counter
C - - - - - 0x00254E 00:E53E: 29 40     AND #$40
C - - - - - 0x002550 00:E540: D0 08     BNE bra_E54A
C - - - - - 0x002552 00:E542: FE 00 02  INC ram_0200_sprite_00_y,X
C - - - - - 0x002555 00:E545: FE 04 02  INC ram_0204_sprite_01_y,X
C - - - - - 0x002558 00:E548: D0 06     BNE bra_E550
bra_E54A:
C - - - - - 0x00255A 00:E54A: FE 03 02  INC ram_0203_sprite_00_x,X
C - - - - - 0x00255D 00:E54D: FE 0B 02  INC ram_020B_sprite_02_x,X
bra_E550:
C - - - - - 0x002560 00:E550: 68        PLA
C - - - - - 0x002561 00:E551: AA        TAX
C - - - - - 0x002562 00:E552: 60        RTS
bra_E553:
C - - - - - 0x002563 00:E553: B9 02 02  LDA ram_0202_sprite_00_attributes,Y
C - - - - - 0x002566 00:E556: 09 40     ORA #$40
C - - - - - 0x002568 00:E558: 99 06 02  STA ram_0206_sprite_01_attributes,Y
C - - - - - 0x00256B 00:E55B: 09 80     ORA #$80
C - - - - - 0x00256D 00:E55D: 99 0E 02  STA ram_020E_sprite_03_attributes,Y
C - - - - - 0x002570 00:E560: 29 BF     AND #$BF
C - - - - - 0x002572 00:E562: 99 0A 02  STA ram_020A_sprite_02_attributes,Y
C - - - - - 0x002575 00:E565: A9 DE     LDA #$DE
C - - - - - 0x002577 00:E567: 99 01 02  STA ram_0201_sprite_00_tile,Y
C - - - - - 0x00257A 00:E56A: 99 05 02  STA ram_0205_sprite_01_tile,Y
C - - - - - 0x00257D 00:E56D: 99 09 02  STA ram_0209_sprite_02_tile,Y
C - - - - - 0x002580 00:E570: 99 0D 02  STA ram_020D_sprite_03_tile,Y
C - - - - - 0x002583 00:E573: DE 5A 04  DEC ram_045A_shock_timer_p1,X
C - - - - - 0x002586 00:E576: 10 0C     BPL bra_E584
C - - - - - 0x002588 00:E578: A9 FF     LDA #$FF
C - - - - - 0x00258A 00:E57A: 95 88     STA ram_0088_object_balloons_p1,X
C - - - - - 0x00258C 00:E57C: A9 F0     LDA #$F0
C - - - - - 0x00258E 00:E57E: 95 9A     STA ram_009A_object_y_pos_int_p1,X
C - - - - - 0x002590 00:E580: A9 04     LDA #$04
C - - - - - 0x002592 00:E582: 85 F1     STA ram_00F1_sfx_2
bra_E584:
C - - - - - 0x002594 00:E584: 68        PLA
C - - - - - 0x002595 00:E585: AA        TAX
C - - - - - 0x002596 00:E586: 60        RTS
sub_E587:
C - - - - - 0x002597 00:E587: A6 BB     LDX ram_00BB_water_plonk_animation
C - - - - - 0x002599 00:E589: 30 39     BMI bra_E5C4_RTS
C - - - - - 0x00259B 00:E58B: BD C5 E5  LDA tbl_E5C5_lo,X
C - - - - - 0x00259E 00:E58E: 85 1D     STA ram_001D_loading_pointer_1d
C - - - - - 0x0025A0 00:E590: BD CA E5  LDA tbl_E5CA_hi,X
C - - - - - 0x0025A3 00:E593: 85 1E     STA ram_001E_loading_pointer_1d
C - - - - - 0x0025A5 00:E595: A0 00     LDY #$00
C - - - - - 0x0025A7 00:E597: A2 00     LDX #$00
bra_E599:
C - - - - - 0x0025A9 00:E599: B1 1D     LDA (ram_001D_loading_pointer_1d),Y
C - - - - - 0x0025AB 00:E59B: 9D E0 02  STA ram_02E0_sprite_38_y,X
C - - - - - 0x0025AE 00:E59E: C8        INY
C - - - - - 0x0025AF 00:E59F: E8        INX
C - - - - - 0x0025B0 00:E5A0: C9 F0     CMP #$F0
C - - - - - 0x0025B2 00:E5A2: D0 03     BNE bra_E5A7
C - - - - - 0x0025B4 00:E5A4: E8        INX
C - - - - - 0x0025B5 00:E5A5: E8        INX
C - - - - - 0x0025B6 00:E5A6: E8        INX
bra_E5A7:
C - - - - - 0x0025B7 00:E5A7: E0 10     CPX #$10
C - - - - - 0x0025B9 00:E5A9: D0 EE     BNE bra_E599
C - - - - - 0x0025BB 00:E5AB: A0 0F     LDY #$0F
bra_E5AD:
C - - - - - 0x0025BD 00:E5AD: B9 E0 02  LDA ram_02E0_sprite_38_y,Y
C - - - - - 0x0025C0 00:E5B0: 18        CLC
C - - - - - 0x0025C1 00:E5B1: 65 BC     ADC ram_00BC
C - - - - - 0x0025C3 00:E5B3: 99 E0 02  STA ram_02E0_sprite_38_y,Y
C - - - - - 0x0025C6 00:E5B6: 88        DEY
C - - - - - 0x0025C7 00:E5B7: 88        DEY
C - - - - - 0x0025C8 00:E5B8: 88        DEY
C - - - - - 0x0025C9 00:E5B9: 88        DEY
C - - - - - 0x0025CA 00:E5BA: 10 F1     BPL bra_E5AD
C - - - - - 0x0025CC 00:E5BC: A5 19     LDA ram_0019_f_counter
C - - - - - 0x0025CE 00:E5BE: 29 03     AND #$03
C - - - - - 0x0025D0 00:E5C0: D0 02     BNE bra_E5C4_RTS
C - - - - - 0x0025D2 00:E5C2: C6 BB     DEC ram_00BB_water_plonk_animation                         ; Go to next water plonk animation frame
bra_E5C4_RTS:
C - - - - - 0x0025D4 00:E5C4: 60        RTS
tbl_E5C5_lo:
- D 3 - - - 0x0025D5 00:E5C5: FD        .byte < off_E5FD_00   ; 00 = $E5FD
- D 3 - - - 0x0025D6 00:E5C6: ED        .byte < off_E5ED_02   ; 01 = $E5ED
- D 3 - - - 0x0025D7 00:E5C7: E0        .byte < off_E5E0_04   ; 02 = $E5E0
- D 3 - - - 0x0025D8 00:E5C8: D6        .byte < off_E5D6_06   ; 03 = $E5D6
- D 3 - - - 0x0025D9 00:E5C9: CF        .byte < off_E5CF_08   ; 04 = $E5CF
tbl_E5CA_hi:
- D 3 - - - 0x0025DA 00:E5CA: E5        .byte > off_E5FD_00   ; 00 = $E5FD
- D 3 - - - 0x0025DB 00:E5CB: E5        .byte > off_E5ED_02   ; 01 = $E5ED
- D 3 - - - 0x0025DC 00:E5CC: E5        .byte > off_E5E0_04   ; 02 = $E5E0
- D 3 - - - 0x0025DD 00:E5CD: E5        .byte > off_E5D6_06   ; 03 = $E5D6
- D 3 - - - 0x0025DE 00:E5CE: E5        .byte > off_E5CF_08   ; 04 = $E5CF
off_E5CF_08:
- D 3 - I - 0x0025DF 00:E5CF: D0        .byte $D0   ; 
- D 3 - I - 0x0025E0 00:E5D0: AE        .byte $AE   ; 
- D 3 - I - 0x0025E1 00:E5D1: 03        .byte $03   ; 
- D 3 - I - 0x0025E2 00:E5D2: 04        .byte $04   ; 
- D 3 - I - 0x0025E3 00:E5D3: F0        .byte $F0   ; 
- D 3 - I - 0x0025E4 00:E5D4: F0        .byte $F0   ; 
- D 3 - I - 0x0025E5 00:E5D5: F0        .byte $F0   ; 
off_E5D6_06:
- D 3 - I - 0x0025E6 00:E5D6: C8        .byte $C8   ; 
- D 3 - I - 0x0025E7 00:E5D7: AF        .byte $AF   ; 
- D 3 - I - 0x0025E8 00:E5D8: 03        .byte $03   ; 
- D 3 - I - 0x0025E9 00:E5D9: 04        .byte $04   ; 
- D 3 - I - 0x0025EA 00:E5DA: D0        .byte $D0   ; 
- D 3 - I - 0x0025EB 00:E5DB: B0        .byte $B0   ; 
- D 3 - I - 0x0025EC 00:E5DC: 03        .byte $03   ; 
- D 3 - I - 0x0025ED 00:E5DD: 04        .byte $04   ; 
- D 3 - I - 0x0025EE 00:E5DE: F0        .byte $F0   ; 
- D 3 - I - 0x0025EF 00:E5DF: F0        .byte $F0   ; 
off_E5E0_04:
- D 3 - I - 0x0025F0 00:E5E0: C8        .byte $C8   ; 
- D 3 - I - 0x0025F1 00:E5E1: B1        .byte $B1   ; 
- D 3 - I - 0x0025F2 00:E5E2: 03        .byte $03   ; 
- D 3 - I - 0x0025F3 00:E5E3: FC        .byte $FC   ; 
- D 3 - I - 0x0025F4 00:E5E4: C8        .byte $C8   ; 
- D 3 - I - 0x0025F5 00:E5E5: B2        .byte $B2   ; 
- D 3 - I - 0x0025F6 00:E5E6: 03        .byte $03   ; 
- D 3 - I - 0x0025F7 00:E5E7: 04        .byte $04   ; 
- D 3 - I - 0x0025F8 00:E5E8: D0        .byte $D0   ; 
- D 3 - I - 0x0025F9 00:E5E9: B3        .byte $B3   ; 
- D 3 - I - 0x0025FA 00:E5EA: 03        .byte $03   ; 
- D 3 - I - 0x0025FB 00:E5EB: 04        .byte $04   ; 
- D 3 - I - 0x0025FC 00:E5EC: F0        .byte $F0   ; 
off_E5ED_02:
- D 3 - I - 0x0025FD 00:E5ED: C8        .byte $C8   ; 
- D 3 - I - 0x0025FE 00:E5EE: B4        .byte $B4   ; 
- D 3 - I - 0x0025FF 00:E5EF: 03        .byte $03   ; 
- D 3 - I - 0x002600 00:E5F0: 00        .byte $00   ; 
- D 3 - I - 0x002601 00:E5F1: C8        .byte $C8   ; 
- D 3 - I - 0x002602 00:E5F2: B4        .byte $B4   ; 
- D 3 - I - 0x002603 00:E5F3: 43        .byte $43   ; 
- D 3 - I - 0x002604 00:E5F4: 08        .byte $08   ; 
- D 3 - I - 0x002605 00:E5F5: D0        .byte $D0   ; 
- D 3 - I - 0x002606 00:E5F6: B5        .byte $B5   ; 
- D 3 - I - 0x002607 00:E5F7: 03        .byte $03   ; 
- D 3 - I - 0x002608 00:E5F8: 00        .byte $00   ; 
- D 3 - I - 0x002609 00:E5F9: D0        .byte $D0   ; 
- D 3 - I - 0x00260A 00:E5FA: B5        .byte $B5   ; 
- D 3 - I - 0x00260B 00:E5FB: 43        .byte $43   ; 
- D 3 - I - 0x00260C 00:E5FC: 08        .byte $08   ; 
off_E5FD_00:
- D 3 - I - 0x00260D 00:E5FD: F0        .byte $F0   ; 
- D 3 - I - 0x00260E 00:E5FE: F0        .byte $F0   ; 
- D 3 - I - 0x00260F 00:E5FF: F0        .byte $F0   ; 
- D 3 - I - 0x002610 00:E600: F0        .byte $F0   ; 
tbl_E601:
- D 3 - - - 0x002611 00:E601: 04        .byte $04   ; 
- D 3 - - - 0x002612 00:E602: 04        .byte $04   ; 
- D 3 - - - 0x002613 00:E603: 05        .byte $05   ; 
- D 3 - - - 0x002614 00:E604: 06        .byte $06   ; 
- D 3 - - - 0x002615 00:E605: 03        .byte $03   ; 
- D 3 - - - 0x002616 00:E606: 03        .byte $03   ; 
- D 3 - - - 0x002617 00:E607: 03        .byte $03   ; 
- D 3 - - - 0x002618 00:E608: 06        .byte $06   ; 
- D 3 - - - 0x002619 00:E609: 0A        .byte $0A   ; 
- D 3 - - - 0x00261A 00:E60A: 0A        .byte $0A   ; 
- D 3 - - - 0x00261B 00:E60B: 0A        .byte $0A   ; 
- D 3 - - - 0x00261C 00:E60C: 0A        .byte $0A   ; 
tbl_E60D:
- D 3 - - - 0x00261D 00:E60D: 28        .byte $28   ; 
- D 3 - - - 0x00261E 00:E60E: 32        .byte $32   ; 
- D 3 - - - 0x00261F 00:E60F: 46        .byte $46   ; 
- D 3 - - - 0x002620 00:E610: 78        .byte $78   ; 
- - - - - - 0x002621 00:E611: 00        .byte $00   ; 
- - - - - - 0x002622 00:E612: 00        .byte $00   ; 
- - - - - - 0x002623 00:E613: 00        .byte $00   ; 
- D 3 - - - 0x002624 00:E614: 64        .byte $64   ; 
- - - - - - 0x002625 00:E615: 00        .byte $00   ; 
- - - - - - 0x002626 00:E616: 00        .byte $00   ; 
- - - - - - 0x002627 00:E617: 00        .byte $00   ; 
- D 3 - - - 0x002628 00:E618: 00        .byte $00   ; 
tbl_E619:
- D 3 - - - 0x002629 00:E619: 0A        .byte $0A   ; 
- D 3 - - - 0x00262A 00:E61A: 1E        .byte $1E   ; 
- D 3 - - - 0x00262B 00:E61B: 32        .byte $32   ; 
- D 3 - - - 0x00262C 00:E61C: 70        .byte $70   ; 
- - - - - - 0x00262D 00:E61D: 00        .byte $00   ; 
- - - - - - 0x00262E 00:E61E: 00        .byte $00   ; 
- - - - - - 0x00262F 00:E61F: 00        .byte $00   ; 
- D 3 - - - 0x002630 00:E620: 70        .byte $70   ; 
- - - - - - 0x002631 00:E621: 00        .byte $00   ; 
- - - - - - 0x002632 00:E622: 00        .byte $00   ; 
- - - - - - 0x002633 00:E623: 00        .byte $00   ; 
- - - - - - 0x002634 00:E624: 00        .byte $00   ; 
tbl_E625:
- - - - - - 0x002635 00:E625: 14        .byte $14   ; 
- - - - - - 0x002636 00:E626: 3C        .byte $3C   ; 
- - - - - - 0x002637 00:E627: 64        .byte $64   ; 
- D 3 - - - 0x002638 00:E628: A0        .byte $A0   ; 
- - - - - - 0x002639 00:E629: 00        .byte $00   ; 
- - - - - - 0x00263A 00:E62A: 00        .byte $00   ; 
- - - - - - 0x00263B 00:E62B: 00        .byte $00   ; 
- D 3 - - - 0x00263C 00:E62C: A0        .byte $A0   ; 
- - - - - - 0x00263D 00:E62D: 00        .byte $00   ; 
- - - - - - 0x00263E 00:E62E: 00        .byte $00   ; 
- - - - - - 0x00263F 00:E62F: 00        .byte $00   ; 
- - - - - - 0x002640 00:E630: 00        .byte $00   ; 
tbl_E631:
- D 3 - - - 0x002641 00:E631: 70        .byte $70   ; 
- D 3 - - - 0x002642 00:E632: B0        .byte $B0   ; 
- D 3 - - - 0x002643 00:E633: E0        .byte $E0   ; 
- D 3 - - - 0x002644 00:E634: 40        .byte $40   ; 
- D 3 - - - 0x002645 00:E635: 80        .byte $80   ; 
- D 3 - - - 0x002646 00:E636: 80        .byte $80   ; 
- D 3 - - - 0x002647 00:E637: 80        .byte $80   ; 
- D 3 - - - 0x002648 00:E638: 40        .byte $40   ; 
- D 3 - - - 0x002649 00:E639: 00        .byte $00   ; 
- D 3 - - - 0x00264A 00:E63A: 00        .byte $00   ; 
- D 3 - - - 0x00264B 00:E63B: 00        .byte $00   ; 
- D 3 - - - 0x00264C 00:E63C: 00        .byte $00   ; 
tbl_E63D:
- D 3 - - - 0x00264D 00:E63D: 00        .byte $00   ; 
- D 3 - - - 0x00264E 00:E63E: 00        .byte $00   ; 
- D 3 - - - 0x00264F 00:E63F: 00        .byte $00   ; 
- D 3 - - - 0x002650 00:E640: 01        .byte $01   ; 
- D 3 - - - 0x002651 00:E641: 00        .byte $00   ; 
- D 3 - - - 0x002652 00:E642: 00        .byte $00   ; 
- D 3 - - - 0x002653 00:E643: 00        .byte $00   ; 
- D 3 - - - 0x002654 00:E644: 01        .byte $01   ; 
- D 3 - - - 0x002655 00:E645: 00        .byte $00   ; 
- D 3 - - - 0x002656 00:E646: 00        .byte $00   ; 
- D 3 - - - 0x002657 00:E647: 00        .byte $00   ; 
- D 3 - - - 0x002658 00:E648: 00        .byte $00   ; 
tbl_E649:
- D 3 - - - 0x002659 00:E649: 90        .byte $90   ; 
- D 3 - - - 0x00265A 00:E64A: 50        .byte $50   ; 
- D 3 - - - 0x00265B 00:E64B: 20        .byte $20   ; 
- D 3 - - - 0x00265C 00:E64C: C0        .byte $C0   ; 
- D 3 - - - 0x00265D 00:E64D: 80        .byte $80   ; 
- D 3 - - - 0x00265E 00:E64E: 80        .byte $80   ; 
- D 3 - - - 0x00265F 00:E64F: 80        .byte $80   ; 
- D 3 - - - 0x002660 00:E650: C0        .byte $C0   ; 
- - - - - - 0x002661 00:E651: 00        .byte $00   ; 
- - - - - - 0x002662 00:E652: 00        .byte $00   ; 
- - - - - - 0x002663 00:E653: 00        .byte $00   ; 
- - - - - - 0x002664 00:E654: 00        .byte $00   ; 
tbl_E655:
- D 3 - - - 0x002665 00:E655: FF        .byte $FF   ; 
- D 3 - - - 0x002666 00:E656: FF        .byte $FF   ; 
- D 3 - - - 0x002667 00:E657: FF        .byte $FF   ; 
- D 3 - - - 0x002668 00:E658: FE        .byte $FE   ; 
- D 3 - - - 0x002669 00:E659: FF        .byte $FF   ; 
- D 3 - - - 0x00266A 00:E65A: FF        .byte $FF   ; 
- D 3 - - - 0x00266B 00:E65B: FF        .byte $FF   ; 
- D 3 - - - 0x00266C 00:E65C: FE        .byte $FE   ; 
- D 3 - - - 0x00266D 00:E65D: 00        .byte $00   ; 
- D 3 - - - 0x00266E 00:E65E: 00        .byte $00   ; 
- D 3 - - - 0x00266F 00:E65F: 00        .byte $00   ; 
- D 3 - - - 0x002670 00:E660: 00        .byte $00   ; 
tbl_E661:
- D 3 - - - 0x002671 00:E661: 50        .byte $50   ; 
- D 3 - - - 0x002672 00:E662: 90        .byte $90   ; 
- D 3 - - - 0x002673 00:E663: C0        .byte $C0   ; 
- D 3 - - - 0x002674 00:E664: 40        .byte $40   ; 
- D 3 - - - 0x002675 00:E665: 40        .byte $40   ; 
- D 3 - - - 0x002676 00:E666: 40        .byte $40   ; 
- D 3 - - - 0x002677 00:E667: 40        .byte $40   ; 
- D 3 - - - 0x002678 00:E668: 40        .byte $40   ; 
- D 3 - - - 0x002679 00:E669: 00        .byte $00   ; 
- D 3 - - - 0x00267A 00:E66A: 00        .byte $00   ; 
- D 3 - - - 0x00267B 00:E66B: 00        .byte $00   ; 
- D 3 - - - 0x00267C 00:E66C: 00        .byte $00   ; 
tbl_E66D:
- D 3 - - - 0x00267D 00:E66D: 00        .byte $00   ; 
- D 3 - - - 0x00267E 00:E66E: 00        .byte $00   ; 
- D 3 - - - 0x00267F 00:E66F: 00        .byte $00   ; 
- D 3 - - - 0x002680 00:E670: 01        .byte $01   ; 
- D 3 - - - 0x002681 00:E671: 00        .byte $00   ; 
- D 3 - - - 0x002682 00:E672: 00        .byte $00   ; 
- D 3 - - - 0x002683 00:E673: 00        .byte $00   ; 
- D 3 - - - 0x002684 00:E674: 01        .byte $01   ; 
- D 3 - - - 0x002685 00:E675: 02        .byte $02   ; 
- D 3 - - - 0x002686 00:E676: 02        .byte $02   ; 
- D 3 - - - 0x002687 00:E677: 02        .byte $02   ; 
- D 3 - - - 0x002688 00:E678: 02        .byte $02   ; 
tbl_E679:
- D 3 - - - 0x002689 00:E679: B0        .byte $B0   ; 
- D 3 - - - 0x00268A 00:E67A: 70        .byte $70   ; 
- D 3 - - - 0x00268B 00:E67B: 40        .byte $40   ; 
- D 3 - - - 0x00268C 00:E67C: C0        .byte $C0   ; 
- D 3 - - - 0x00268D 00:E67D: C0        .byte $C0   ; 
- D 3 - - - 0x00268E 00:E67E: C0        .byte $C0   ; 
- D 3 - - - 0x00268F 00:E67F: C0        .byte $C0   ; 
- D 3 - - - 0x002690 00:E680: C0        .byte $C0   ; 
- - - - - - 0x002691 00:E681: C0        .byte $C0   ; 
- - - - - - 0x002692 00:E682: C0        .byte $C0   ; 
- - - - - - 0x002693 00:E683: C0        .byte $C0   ; 
- D 3 - - - 0x002694 00:E684: C0        .byte $C0   ; 
tbl_E685:
- D 3 - - - 0x002695 00:E685: FF        .byte $FF   ; 
- D 3 - - - 0x002696 00:E686: FF        .byte $FF   ; 
- D 3 - - - 0x002697 00:E687: FF        .byte $FF   ; 
- D 3 - - - 0x002698 00:E688: FE        .byte $FE   ; 
- D 3 - - - 0x002699 00:E689: FF        .byte $FF   ; 
- D 3 - - - 0x00269A 00:E68A: FF        .byte $FF   ; 
- D 3 - - - 0x00269B 00:E68B: FF        .byte $FF   ; 
- D 3 - - - 0x00269C 00:E68C: FE        .byte $FE   ; 
- D 3 - - - 0x00269D 00:E68D: FE        .byte $FE   ; 
- D 3 - - - 0x00269E 00:E68E: 01        .byte $01   ; 
- D 3 - - - 0x00269F 00:E68F: FE        .byte $FE   ; 
- D 3 - - - 0x0026A0 00:E690: FE        .byte $FE   ; 
sub_E691_Object_Manage:
C - - - - - 0x0026A1 00:E691: 20 25 EE  JSR sub_EE25_Collision
C - - - - - 0x0026A4 00:E694: A2 07     LDX #$07
bra_E696:
C - - - - - 0x0026A6 00:E696: B5 88     LDA ram_0088_object_balloons_p1,X                          ; Check all Objects' Balloons
C - - - - - 0x0026A8 00:E698: 10 0A     BPL bra_E6A4                                               ; If >= 0 then proceed
C - - - - - 0x0026AA 00:E69A: C9 FF     CMP #$FF                                                   ; else if == -1 then go to next Object
C - - - - - 0x0026AC 00:E69C: F0 44     BEQ bra_E6E2                                               ; else ? and go to next Object
C - - - - - 0x0026AE 00:E69E: 20 BA EC  JSR sub_ECBA
C - - - - - 0x0026B1 00:E6A1: 4C E2 E6  JMP loc_E6E2
bra_E6A4:
C - - - - - 0x0026B4 00:E6A4: E0 02     CPX #$02                                                   ; Object is Player
C - - - - - 0x0026B6 00:E6A6: 90 10     BCC bra_E6B8
C - - - - - 0x0026B8 00:E6A8: C9 01     CMP #$01                                                   ; One Balloon
C - - - - - 0x0026BA 00:E6AA: D0 0C     BNE bra_E6B8
C - - - - - 0x0026BC 00:E6AC: B5 7F     LDA ram_007F_object_status_p1,X                            ; Object Status >= 2
C - - - - - 0x0026BE 00:E6AE: C9 02     CMP #$02
C - - - - - 0x0026C0 00:E6B0: B0 06     BCS bra_E6B8
C - - - - - 0x0026C2 00:E6B2: A5 F1     LDA ram_00F1_sfx_2
C - - - - - 0x0026C4 00:E6B4: 09 20     ORA #$20                                                   ; Play SFX
C - - - - - 0x0026C6 00:E6B6: 85 F1     STA ram_00F1_sfx_2
bra_E6B8:
C - - - - - 0x0026C8 00:E6B8: DE 3F 04  DEC ram_043F_animation_f_timer_p1,X                        ; Object's Frame Timer != 0
C - - - - - 0x0026CB 00:E6BB: D0 1C     BNE bra_E6D9
C - - - - - 0x0026CD 00:E6BD: A9 03     LDA #$03                                                   ; Object's Frame Timer = 3
C - - - - - 0x0026CF 00:E6BF: 9D 3F 04  STA ram_043F_animation_f_timer_p1,X
C - - - - - 0x0026D2 00:E6C2: E0 02     CPX #$02                                                   ; Object is not Player
C - - - - - 0x0026D4 00:E6C4: B0 08     BCS bra_E6CE
C - - - - - 0x0026D6 00:E6C6: D6 BF     DEC ram_00BF_p1_invincibility_time,X                       ; Player Invincibility Handle
C - - - - - 0x0026D8 00:E6C8: D0 04     BNE bra_E6CE                                               ; Decrease time until 0
C - - - - - 0x0026DA 00:E6CA: A9 00     LDA #$00                                                   ; Then disable invincibility
C - - - - - 0x0026DC 00:E6CC: 95 BD     STA ram_00BD_p1_invincibility_flag,X
bra_E6CE:
C - - - - - 0x0026DE 00:E6CE: 20 18 EA  JSR sub_EA18_Object_Update_Anim
C - - - - - 0x0026E1 00:E6D1: 86 3E     STX ram_003E_score_id_update
C - - - - - 0x0026E3 00:E6D3: 20 C4 EB  JSR sub_EBC4
C - - - - - 0x0026E6 00:E6D6: 20 96 E7  JSR sub_E796
bra_E6D9:
C - - - - - 0x0026E9 00:E6D9: 20 58 EA  JSR sub_EA58
C - - - - - 0x0026EC 00:E6DC: 20 28 ED  JSR sub_ED28
C - - - - - 0x0026EF 00:E6DF: 20 83 E9  JSR sub_E983
bra_E6E2:
loc_E6E2:
C D 3 - - - 0x0026F2 00:E6E2: 20 A4 E3  JSR sub_E3A4
C - - - - - 0x0026F5 00:E6E5: CA        DEX
C - - - - - 0x0026F6 00:E6E6: 10 AE     BPL bra_E696                                               ; Loop back
C - - - - - 0x0026F8 00:E6E8: 60        RTS
sub_E6E9_Object_Update_Action:
C - - - - - 0x0026F9 00:E6E9: E0 02     CPX #$02                                                   ; If Enemy, then rely on RNG
C - - - - - 0x0026FB 00:E6EB: B0 18     BCS bra_E705                                               ; If Player, then rely on joypad
C - - - - - 0x0026FD 00:E6ED: A5 19     LDA ram_0019_f_counter
C - - - - - 0x0026FF 00:E6EF: 29 0F     AND #$0F                                                   ; Enemy only reacts every 16 frames
C - - - - - 0x002701 00:E6F1: D0 05     BNE bra_E6F8
C - - - - - 0x002703 00:E6F3: 20 B3 F1  JSR sub_F1B3_RNG                                           ; Update Enemy Action
C - - - - - 0x002706 00:E6F6: 95 31     STA ram_0031,X
bra_E6F8:
C - - - - - 0x002708 00:E6F8: A5 3A     LDA ram_003A_demo_flag                                     ; If Demo Play then
C - - - - - 0x00270A 00:E6FA: D0 09     BNE bra_E705                                               ; do automatic inputs
C - - - - - 0x00270C 00:E6FC: 20 6A E7  JSR sub_E76A_Poll_Joypad_X
C - - - - - 0x00270F 00:E6FF: BD 1C 06  LDA ram_061C_controller1_pressed,X                         ; Read Pressed Buttons
C - - - - - 0x002712 00:E702: 95 31     STA ram_0031,X                                             ; into Player Action
bra_E704_RTS:
C - - - - - 0x002714 00:E704: 60        RTS
bra_E705:
; Demo Play.
C - - - - - 0x002715 00:E705: B5 9A     LDA ram_009A_object_y_pos_int_p1,X                         ; If Player Y above #$A0
C - - - - - 0x002717 00:E707: C9 A0     CMP #$A0                                                   ; Then
C - - - - - 0x002719 00:E709: 90 07     BCC bra_E712
C - - - - - 0x00271B 00:E70B: B5 31     LDA ram_0031,X
C - - - - - 0x00271D 00:E70D: 09 40     ORA #$40                                                   ; Do rapid fire
C - - - - - 0x00271F 00:E70F: 95 31     STA ram_0031,X                                             ; (B Button)
C - - - - - 0x002721 00:E711: 60        RTS
bra_E712:
C - - - - - 0x002722 00:E712: DE 5A 04  DEC ram_045A_shock_timer_p1,X
C - - - - - 0x002725 00:E715: D0 ED     BNE bra_E704_RTS                                           ; then return
C - - - - - 0x002727 00:E717: 20 B3 F1  JSR sub_F1B3_RNG
C - - - - - 0x00272A 00:E71A: BC 51 04  LDY ram_0451_object_type_p1,X
C - - - - - 0x00272D 00:E71D: 39 62 E7  AND tbl_E762,Y
C - - - - - 0x002730 00:E720: 79 65 E7  ADC tbl_E762 + 3,Y
C - - - - - 0x002733 00:E723: 9D 5A 04  STA ram_045A_shock_timer_p1,X
C - - - - - 0x002736 00:E726: 86 12     STX ram_0012_temp
C - - - - - 0x002738 00:E728: A5 19     LDA ram_0019_f_counter
C - - - - - 0x00273A 00:E72A: 2A        ROL
C - - - - - 0x00273B 00:E72B: 2A        ROL
C - - - - - 0x00273C 00:E72C: 45 12     EOR ram_0012_temp
C - - - - - 0x00273E 00:E72E: 29 01     AND #$01
C - - - - - 0x002740 00:E730: A8        TAY
C - - - - - 0x002741 00:E731: B9 88 00  LDA ram_0088_object_balloons_p1,Y
C - - - - - 0x002744 00:E734: 30 13     BMI bra_E749
C - - - - - 0x002746 00:E736: B9 BD 00  LDA ram_00BD_p1_invincibility_flag,Y
C - - - - - 0x002749 00:E739: D0 0E     BNE bra_E749
C - - - - - 0x00274B 00:E73B: A9 00     LDA #$00
C - - - - - 0x00274D 00:E73D: 95 31     STA ram_0031,X
C - - - - - 0x00274F 00:E73F: B9 9A 00  LDA ram_009A_object_y_pos_int_p1,Y
C - - - - - 0x002752 00:E742: 38        SEC
C - - - - - 0x002753 00:E743: E9 04     SBC #$04
C - - - - - 0x002755 00:E745: D5 9A     CMP ram_009A_object_y_pos_int_p1,X
C - - - - - 0x002757 00:E747: B0 04     BCS bra_E74D
bra_E749:
C - - - - - 0x002759 00:E749: A9 40     LDA #$40
C - - - - - 0x00275B 00:E74B: 95 31     STA ram_0031,X
bra_E74D:
C - - - - - 0x00275D 00:E74D: B5 91     LDA ram_0091_object_x_pos_int_p1,X
C - - - - - 0x00275F 00:E74F: D9 91 00  CMP ram_0091_object_x_pos_int_p1,Y
C - - - - - 0x002762 00:E752: B0 07     BCS bra_E75B
C - - - - - 0x002764 00:E754: B5 31     LDA ram_0031,X
C - - - - - 0x002766 00:E756: 09 01     ORA #$01
C - - - - - 0x002768 00:E758: 95 31     STA ram_0031,X
C - - - - - 0x00276A 00:E75A: 60        RTS
bra_E75B:
C - - - - - 0x00276B 00:E75B: B5 31     LDA ram_0031,X
C - - - - - 0x00276D 00:E75D: 09 02     ORA #$02
C - - - - - 0x00276F 00:E75F: 95 31     STA ram_0031,X
C - - - - - 0x002771 00:E761: 60        RTS
tbl_E762:
- D 3 - - - 0x002772 00:E762: 1F        .byte $1F   ; 
- D 3 - - - 0x002773 00:E763: 0F        .byte $0F   ; 
- D 3 - - - 0x002774 00:E764: 07        .byte $07   ; 
- D 3 - - - 0x002775 00:E765: 20        .byte $20   ; 
- D 3 - - - 0x002776 00:E766: 10        .byte $10   ; 
- D 3 - - - 0x002777 00:E767: 08        .byte $08   ; 

; Joypad code.

sub_E768_Poll_Joypad_0:
C D 3 - - - 0x002778 00:E768: A2 00     LDX #$00                                                   ; Read Controller 0
sub_E76A_Poll_Joypad_X:
C - - - - - 0x00277A 00:E76A: A9 01     LDA #$01
C D 3 - - - 0x00277C 00:E76C: 8D 16 40  STA $4016                                                  ; Output Strobe to both controllers
C - - - - - 0x00277F 00:E76F: A9 00     LDA #$00
C - - - - - 0x002781 00:E771: 8D 16 40  STA $4016
C - - - - - 0x002784 00:E774: A0 07     LDY #$07
bra_E776:
C - - - - - 0x002786 00:E776: BD 16 40  LDA $4016,X
C - - - - - 0x002789 00:E779: 85 12     STA ram_0012_temp
C - - - - - 0x00278B 00:E77B: 4A        LSR                                                        ; Poll Controller X
C - - - - - 0x00278C 00:E77C: 05 12     ORA ram_0012_temp                                          ; to $061C + X
C - - - - - 0x00278E 00:E77E: 4A        LSR
C - - - - - 0x00278F 00:E77F: 3E 1C 06  ROL ram_061C_controller1_pressed,X
C - - - - - 0x002792 00:E782: 88        DEY
C - - - - - 0x002793 00:E783: 10 F1     BPL bra_E776
C - - - - - 0x002795 00:E785: BC 1E 06  LDY ram_061E_controller1_held,X
C - - - - - 0x002798 00:E788: BD 1C 06  LDA ram_061C_controller1_pressed,X
C - - - - - 0x00279B 00:E78B: 9D 1E 06  STA ram_061E_controller1_held,X                            ; Check for pressed buttons
C - - - - - 0x00279E 00:E78E: 98        TYA
C - - - - - 0x00279F 00:E78F: 5D 1C 06  EOR ram_061C_controller1_pressed,X
C - - - - - 0x0027A2 00:E792: 3D 1C 06  AND ram_061C_controller1_pressed,X
C - - - - - 0x0027A5 00:E795: 60        RTS                                                        ; Returns pressed buttons in A
sub_E796:
C - - - - - 0x0027A6 00:E796: B5 88     LDA ram_0088_object_balloons_p1,X                          ; If Object has Balloons
C - - - - - 0x0027A8 00:E798: D0 09     BNE bra_E7A3                                               ; then continue
bra_E79A:
C - - - - - 0x0027AA 00:E79A: A9 00     LDA #$00                                                   ; If no Balloons:
C - - - - - 0x0027AC 00:E79C: 9D 24 04  STA ram_0424_x_vel_frac_p1,X                               ; X Velocity = 0
C - - - - - 0x0027AF 00:E79F: 9D 2D 04  STA ram_042D_x_vel_int_p1,X
C - - - - - 0x0027B2 00:E7A2: 60        RTS                                                        ; Return
bra_E7A3:
C - - - - - 0x0027B3 00:E7A3: C9 02     CMP #$02                                                   ; If 2 Balloons
C - - - - - 0x0027B5 00:E7A5: F0 41     BEQ bra_E7E8
C - - - - - 0x0027B7 00:E7A7: E0 02     CPX #$02                                                   ; If Object is a Player
C - - - - - 0x0027B9 00:E7A9: 90 3D     BCC bra_E7E8
C - - - - - 0x0027BB 00:E7AB: B5 7F     LDA ram_007F_object_status_p1,X                            ; If Object Status >= 2
C - - - - - 0x0027BD 00:E7AD: C9 02     CMP #$02                                                   ; then zero X Velocity
C - - - - - 0x0027BF 00:E7AF: B0 E9     BCS bra_E79A
sub_E7B1:
C - - - - - 0x0027C1 00:E7B1: BD 24 04  LDA ram_0424_x_vel_frac_p1,X
C - - - - - 0x0027C4 00:E7B4: 85 12     STA ram_0012_temp
C - - - - - 0x0027C6 00:E7B6: BD 2D 04  LDA ram_042D_x_vel_int_p1,X
C - - - - - 0x0027C9 00:E7B9: 85 13     STA ram_0013_temp
C - - - - - 0x0027CB 00:E7BB: 20 A6 F1  JSR sub_F1A6
C - - - - - 0x0027CE 00:E7BE: BD 63 04  LDA ram_0463,X
C - - - - - 0x0027D1 00:E7C1: 18        CLC
C - - - - - 0x0027D2 00:E7C2: 65 12     ADC ram_0012_temp
C - - - - - 0x0027D4 00:E7C4: 9D 63 04  STA ram_0463,X
C - - - - - 0x0027D7 00:E7C7: 85 12     STA ram_0012_temp
C - - - - - 0x0027D9 00:E7C9: BD 6C 04  LDA ram_046C,X
C - - - - - 0x0027DC 00:E7CC: 65 13     ADC ram_0013_temp
C - - - - - 0x0027DE 00:E7CE: 9D 6C 04  STA ram_046C,X
C - - - - - 0x0027E1 00:E7D1: 85 13     STA ram_0013_temp
C - - - - - 0x0027E3 00:E7D3: 20 A6 F1  JSR sub_F1A6
C - - - - - 0x0027E6 00:E7D6: BD 24 04  LDA ram_0424_x_vel_frac_p1,X
C - - - - - 0x0027E9 00:E7D9: 38        SEC
C - - - - - 0x0027EA 00:E7DA: E5 12     SBC ram_0012_temp
C - - - - - 0x0027EC 00:E7DC: 9D 24 04  STA ram_0424_x_vel_frac_p1,X
C - - - - - 0x0027EF 00:E7DF: BD 2D 04  LDA ram_042D_x_vel_int_p1,X
C - - - - - 0x0027F2 00:E7E2: E5 13     SBC ram_0013_temp
C - - - - - 0x0027F4 00:E7E4: 9D 2D 04  STA ram_042D_x_vel_int_p1,X
C - - - - - 0x0027F7 00:E7E7: 60        RTS
bra_E7E8:
C - - - - - 0x0027F8 00:E7E8: B5 7F     LDA ram_007F_object_status_p1,X
C - - - - - 0x0027FA 00:E7EA: C9 06     CMP #$06
C - - - - - 0x0027FC 00:E7EC: 90 01     BCC bra_E7EF
- - - - - - 0x0027FE 00:E7EE: 60        RTS
bra_E7EF:
C - - - - - 0x0027FF 00:E7EF: B5 7F     LDA ram_007F_object_status_p1,X
C - - - - - 0x002801 00:E7F1: C9 04     CMP #$04
C - - - - - 0x002803 00:E7F3: D0 1C     BNE bra_E811
C - - - - - 0x002805 00:E7F5: B5 31     LDA ram_0031,X
C - - - - - 0x002807 00:E7F7: 29 02     AND #$02
C - - - - - 0x002809 00:E7F9: F0 07     BEQ bra_E802
C - - - - - 0x00280B 00:E7FB: BD 48 04  LDA ram_0448_direction_p1,X
C - - - - - 0x00280E 00:E7FE: F0 11     BEQ bra_E811
C - - - - - 0x002810 00:E800: D0 0B     BNE bra_E80D
bra_E802:
C - - - - - 0x002812 00:E802: B5 31     LDA ram_0031,X
C - - - - - 0x002814 00:E804: 29 01     AND #$01
C - - - - - 0x002816 00:E806: F0 09     BEQ bra_E811
C - - - - - 0x002818 00:E808: BD 48 04  LDA ram_0448_direction_p1,X
C - - - - - 0x00281B 00:E80B: D0 04     BNE bra_E811
bra_E80D:
C - - - - - 0x00281D 00:E80D: A9 05     LDA #$05
C - - - - - 0x00281F 00:E80F: 95 7F     STA ram_007F_object_status_p1,X
bra_E811:
C - - - - - 0x002821 00:E811: B5 7F     LDA ram_007F_object_status_p1,X
C - - - - - 0x002823 00:E813: C9 02     CMP #$02
C - - - - - 0x002825 00:E815: D0 1B     BNE bra_E832
C - - - - - 0x002827 00:E817: B5 31     LDA ram_0031,X
C - - - - - 0x002829 00:E819: 29 02     AND #$02
C - - - - - 0x00282B 00:E81B: F0 04     BEQ bra_E821
C - - - - - 0x00282D 00:E81D: A9 00     LDA #$00
C - - - - - 0x00282F 00:E81F: F0 08     BEQ bra_E829
bra_E821:
C - - - - - 0x002831 00:E821: B5 31     LDA ram_0031,X
C - - - - - 0x002833 00:E823: 29 01     AND #$01
C - - - - - 0x002835 00:E825: F0 07     BEQ bra_E82E
C - - - - - 0x002837 00:E827: A9 01     LDA #$01
bra_E829:
C - - - - - 0x002839 00:E829: DD 48 04  CMP ram_0448_direction_p1,X
C - - - - - 0x00283C 00:E82C: F0 04     BEQ bra_E832
bra_E82E:
C - - - - - 0x00283E 00:E82E: A9 04     LDA #$04
C - - - - - 0x002840 00:E830: 95 7F     STA ram_007F_object_status_p1,X
bra_E832:
C - - - - - 0x002842 00:E832: B5 7F     LDA ram_007F_object_status_p1,X
C - - - - - 0x002844 00:E834: C9 04     CMP #$04
C - - - - - 0x002846 00:E836: 90 1C     BCC bra_E854
C - - - - - 0x002848 00:E838: B5 31     LDA ram_0031,X
C - - - - - 0x00284A 00:E83A: 29 02     AND #$02
C - - - - - 0x00284C 00:E83C: F0 07     BEQ bra_E845
C - - - - - 0x00284E 00:E83E: BD 48 04  LDA ram_0448_direction_p1,X
C - - - - - 0x002851 00:E841: D0 11     BNE bra_E854
C - - - - - 0x002853 00:E843: F0 0B     BEQ bra_E850
bra_E845:
C - - - - - 0x002855 00:E845: B5 31     LDA ram_0031,X
C - - - - - 0x002857 00:E847: 29 01     AND #$01
C - - - - - 0x002859 00:E849: F0 09     BEQ bra_E854
C - - - - - 0x00285B 00:E84B: BD 48 04  LDA ram_0448_direction_p1,X
C - - - - - 0x00285E 00:E84E: F0 04     BEQ bra_E854
bra_E850:
C - - - - - 0x002860 00:E850: A9 02     LDA #$02
C - - - - - 0x002862 00:E852: 95 7F     STA ram_007F_object_status_p1,X
bra_E854:
C - - - - - 0x002864 00:E854: B5 7F     LDA ram_007F_object_status_p1,X
C - - - - - 0x002866 00:E856: C9 03     CMP #$03
C - - - - - 0x002868 00:E858: D0 0A     BNE bra_E864
C - - - - - 0x00286A 00:E85A: B5 31     LDA ram_0031,X
C - - - - - 0x00286C 00:E85C: 29 03     AND #$03
C - - - - - 0x00286E 00:E85E: F0 04     BEQ bra_E864
C - - - - - 0x002870 00:E860: A9 02     LDA #$02
C - - - - - 0x002872 00:E862: 95 7F     STA ram_007F_object_status_p1,X
bra_E864:
C - - - - - 0x002874 00:E864: B5 7F     LDA ram_007F_object_status_p1,X
C - - - - - 0x002876 00:E866: C9 04     CMP #$04
C - - - - - 0x002878 00:E868: B0 15     BCS bra_E87F
C - - - - - 0x00287A 00:E86A: B5 31     LDA ram_0031,X
C - - - - - 0x00287C 00:E86C: 29 02     AND #$02
C - - - - - 0x00287E 00:E86E: F0 04     BEQ bra_E874
C - - - - - 0x002880 00:E870: A9 00     LDA #$00
C - - - - - 0x002882 00:E872: F0 08     BEQ bra_E87C
bra_E874:
C - - - - - 0x002884 00:E874: B5 31     LDA ram_0031,X
C - - - - - 0x002886 00:E876: 29 01     AND #$01
C - - - - - 0x002888 00:E878: F0 05     BEQ bra_E87F
C - - - - - 0x00288A 00:E87A: A9 01     LDA #$01
bra_E87C:
C - - - - - 0x00288C 00:E87C: 9D 48 04  STA ram_0448_direction_p1,X
bra_E87F:
C - - - - - 0x00288F 00:E87F: B5 7F     LDA ram_007F_object_status_p1,X
C - - - - - 0x002891 00:E881: C9 04     CMP #$04
C - - - - - 0x002893 00:E883: 90 33     BCC bra_E8B8
C - - - - - 0x002895 00:E885: BD 36 04  LDA ram_0436_animation_f_p1,X
C - - - - - 0x002898 00:E888: C9 01     CMP #$01
C - - - - - 0x00289A 00:E88A: D0 2C     BNE bra_E8B8
C - - - - - 0x00289C 00:E88C: BC 51 04  LDY ram_0451_object_type_p1,X
C - - - - - 0x00289F 00:E88F: BD 48 04  LDA ram_0448_direction_p1,X
C - - - - - 0x0028A2 00:E892: F0 12     BEQ bra_E8A6
C - - - - - 0x0028A4 00:E894: BD 24 04  LDA ram_0424_x_vel_frac_p1,X
C - - - - - 0x0028A7 00:E897: 38        SEC
C - - - - - 0x0028A8 00:E898: F9 25 E6  SBC tbl_E625,Y
C - - - - - 0x0028AB 00:E89B: 9D 24 04  STA ram_0424_x_vel_frac_p1,X
C - - - - - 0x0028AE 00:E89E: BD 2D 04  LDA ram_042D_x_vel_int_p1,X
C - - - - - 0x0028B1 00:E8A1: E9 00     SBC #$00
C - - - - - 0x0028B3 00:E8A3: 4C 01 E9  JMP loc_E901
bra_E8A6:
C - - - - - 0x0028B6 00:E8A6: BD 24 04  LDA ram_0424_x_vel_frac_p1,X
C - - - - - 0x0028B9 00:E8A9: 18        CLC
C - - - - - 0x0028BA 00:E8AA: 79 25 E6  ADC tbl_E625,Y
C - - - - - 0x0028BD 00:E8AD: 9D 24 04  STA ram_0424_x_vel_frac_p1,X
C - - - - - 0x0028C0 00:E8B0: BD 2D 04  LDA ram_042D_x_vel_int_p1,X
C - - - - - 0x0028C3 00:E8B3: 69 00     ADC #$00
C - - - - - 0x0028C5 00:E8B5: 4C 01 E9  JMP loc_E901
bra_E8B8:
C - - - - - 0x0028C8 00:E8B8: B5 7F     LDA ram_007F_object_status_p1,X
C - - - - - 0x0028CA 00:E8BA: F0 0B     BEQ bra_E8C7
C - - - - - 0x0028CC 00:E8BC: C9 02     CMP #$02
C - - - - - 0x0028CE 00:E8BE: F0 47     BEQ bra_E907
C - - - - - 0x0028D0 00:E8C0: C9 03     CMP #$03
C - - - - - 0x0028D2 00:E8C2: F0 03     BEQ bra_E8C7
C - - - - - 0x0028D4 00:E8C4: 4C 51 E9  JMP loc_E951
bra_E8C7:
C - - - - - 0x0028D7 00:E8C7: BD 36 04  LDA ram_0436_animation_f_p1,X
C - - - - - 0x0028DA 00:E8CA: C9 01     CMP #$01
C - - - - - 0x0028DC 00:E8CC: F0 03     BEQ bra_E8D1
C - - - - - 0x0028DE 00:E8CE: 4C 51 E9  JMP loc_E951
bra_E8D1:
C - - - - - 0x0028E1 00:E8D1: BC 51 04  LDY ram_0451_object_type_p1,X
C - - - - - 0x0028E4 00:E8D4: B5 31     LDA ram_0031,X
C - - - - - 0x0028E6 00:E8D6: 29 02     AND #$02
C - - - - - 0x0028E8 00:E8D8: F0 12     BEQ bra_E8EC
C - - - - - 0x0028EA 00:E8DA: BD 24 04  LDA ram_0424_x_vel_frac_p1,X
C - - - - - 0x0028ED 00:E8DD: 38        SEC
C - - - - - 0x0028EE 00:E8DE: F9 19 E6  SBC tbl_E619,Y
C - - - - - 0x0028F1 00:E8E1: 9D 24 04  STA ram_0424_x_vel_frac_p1,X
C - - - - - 0x0028F4 00:E8E4: BD 2D 04  LDA ram_042D_x_vel_int_p1,X
C - - - - - 0x0028F7 00:E8E7: E9 00     SBC #$00
C - - - - - 0x0028F9 00:E8E9: 4C 01 E9  JMP loc_E901
bra_E8EC:
C - - - - - 0x0028FC 00:E8EC: B5 31     LDA ram_0031,X
C - - - - - 0x0028FE 00:E8EE: 29 01     AND #$01
C - - - - - 0x002900 00:E8F0: F0 5F     BEQ bra_E951
C - - - - - 0x002902 00:E8F2: BD 24 04  LDA ram_0424_x_vel_frac_p1,X
C - - - - - 0x002905 00:E8F5: 18        CLC
C - - - - - 0x002906 00:E8F6: 79 19 E6  ADC tbl_E619,Y
C - - - - - 0x002909 00:E8F9: 9D 24 04  STA ram_0424_x_vel_frac_p1,X
C - - - - - 0x00290C 00:E8FC: BD 2D 04  LDA ram_042D_x_vel_int_p1,X
C - - - - - 0x00290F 00:E8FF: 69 00     ADC #$00
loc_E901:
C D 3 - - - 0x002911 00:E901: 9D 2D 04  STA ram_042D_x_vel_int_p1,X
C - - - - - 0x002914 00:E904: 4C 51 E9  JMP loc_E951
bra_E907:
C - - - - - 0x002917 00:E907: BD 36 04  LDA ram_0436_animation_f_p1,X
C - - - - - 0x00291A 00:E90A: C9 01     CMP #$01
C - - - - - 0x00291C 00:E90C: D0 43     BNE bra_E951
C - - - - - 0x00291E 00:E90E: BC 51 04  LDY ram_0451_object_type_p1,X
C - - - - - 0x002921 00:E911: B5 31     LDA ram_0031,X
C - - - - - 0x002923 00:E913: 29 02     AND #$02
C - - - - - 0x002925 00:E915: F0 12     BEQ bra_E929
C - - - - - 0x002927 00:E917: BD 24 04  LDA ram_0424_x_vel_frac_p1,X
C - - - - - 0x00292A 00:E91A: 38        SEC
C - - - - - 0x00292B 00:E91B: F9 25 E6  SBC tbl_E625,Y
C - - - - - 0x00292E 00:E91E: 9D 24 04  STA ram_0424_x_vel_frac_p1,X
C - - - - - 0x002931 00:E921: BD 2D 04  LDA ram_042D_x_vel_int_p1,X
C - - - - - 0x002934 00:E924: E9 00     SBC #$00
C - - - - - 0x002936 00:E926: 4C 3E E9  JMP loc_E93E
bra_E929:
C - - - - - 0x002939 00:E929: B5 31     LDA ram_0031,X
C - - - - - 0x00293B 00:E92B: 29 01     AND #$01
C - - - - - 0x00293D 00:E92D: F0 22     BEQ bra_E951
C - - - - - 0x00293F 00:E92F: BD 24 04  LDA ram_0424_x_vel_frac_p1,X
C - - - - - 0x002942 00:E932: 18        CLC
C - - - - - 0x002943 00:E933: 79 25 E6  ADC tbl_E625,Y
C - - - - - 0x002946 00:E936: 9D 24 04  STA ram_0424_x_vel_frac_p1,X
C - - - - - 0x002949 00:E939: BD 2D 04  LDA ram_042D_x_vel_int_p1,X
C - - - - - 0x00294C 00:E93C: 69 00     ADC #$00
loc_E93E:
C D 3 - - - 0x00294E 00:E93E: 9D 2D 04  STA ram_042D_x_vel_int_p1,X
C - - - - - 0x002951 00:E941: B5 31     LDA ram_0031,X
C - - - - - 0x002953 00:E943: 29 03     AND #$03
C - - - - - 0x002955 00:E945: F0 0A     BEQ bra_E951
C - - - - - 0x002957 00:E947: E0 02     CPX #$02
C - - - - - 0x002959 00:E949: B0 06     BCS bra_E951
C - - - - - 0x00295B 00:E94B: A5 F0     LDA ram_00F0_sfx_1
C - - - - - 0x00295D 00:E94D: 09 08     ORA #$08                                                   ; Play Walk SFX
C - - - - - 0x00295F 00:E94F: 85 F0     STA ram_00F0_sfx_1
bra_E951:
loc_E951:
C D 3 - - - 0x002961 00:E951: B5 7F     LDA ram_007F_object_status_p1,X
C - - - - - 0x002963 00:E953: C9 04     CMP #$04
C - - - - - 0x002965 00:E955: 90 2B     BCC bra_E982_RTS
C - - - - - 0x002967 00:E957: BD 48 04  LDA ram_0448_direction_p1,X
C - - - - - 0x00296A 00:E95A: D0 07     BNE bra_E963
C - - - - - 0x00296C 00:E95C: BD 2D 04  LDA ram_042D_x_vel_int_p1,X
C - - - - - 0x00296F 00:E95F: 30 21     BMI bra_E982_RTS
C - - - - - 0x002971 00:E961: 10 05     BPL bra_E968
bra_E963:
C - - - - - 0x002973 00:E963: BD 2D 04  LDA ram_042D_x_vel_int_p1,X
C - - - - - 0x002976 00:E966: 10 1A     BPL bra_E982_RTS
bra_E968:
C - - - - - 0x002978 00:E968: B5 7F     LDA ram_007F_object_status_p1,X
C - - - - - 0x00297A 00:E96A: C9 05     CMP #$05
C - - - - - 0x00297C 00:E96C: D0 08     BNE bra_E976
C - - - - - 0x00297E 00:E96E: BD 48 04  LDA ram_0448_direction_p1,X
C - - - - - 0x002981 00:E971: 49 01     EOR #$01
C - - - - - 0x002983 00:E973: 9D 48 04  STA ram_0448_direction_p1,X
bra_E976:
C - - - - - 0x002986 00:E976: A9 03     LDA #$03
C - - - - - 0x002988 00:E978: 95 7F     STA ram_007F_object_status_p1,X
C - - - - - 0x00298A 00:E97A: A9 00     LDA #$00
C - - - - - 0x00298C 00:E97C: 9D 24 04  STA ram_0424_x_vel_frac_p1,X
C - - - - - 0x00298F 00:E97F: 9D 2D 04  STA ram_042D_x_vel_int_p1,X
bra_E982_RTS:
C - - - - - 0x002992 00:E982: 60        RTS
sub_E983:
C - - - - - 0x002993 00:E983: A5 CB     LDA ram_00CB
C - - - - - 0x002995 00:E985: D0 2F     BNE bra_E9B6
C - - - - - 0x002997 00:E987: B5 BD     LDA ram_00BD_p1_invincibility_flag,X
C - - - - - 0x002999 00:E989: F0 0F     BEQ bra_E99A
C - - - - - 0x00299B 00:E98B: AD 88 04  LDA ram_0488_trip_plat_start_x_pos
C - - - - - 0x00299E 00:E98E: F0 0A     BEQ bra_E99A
C - - - - - 0x0029A0 00:E990: 38        SEC
C - - - - - 0x0029A1 00:E991: F5 91     SBC ram_0091_object_x_pos_int_p1,X
C - - - - - 0x0029A3 00:E993: 20 8E F0  JSR sub_F08E_Absolute
C - - - - - 0x0029A6 00:E996: C9 05     CMP #$05
C - - - - - 0x0029A8 00:E998: 90 1C     BCC bra_E9B6
bra_E99A:
C - - - - - 0x0029AA 00:E99A: E0 02     CPX #$02
C - - - - - 0x0029AC 00:E99C: 90 06     BCC bra_E9A4
C - - - - - 0x0029AE 00:E99E: B5 88     LDA ram_0088_object_balloons_p1,X
C - - - - - 0x0029B0 00:E9A0: C9 02     CMP #$02
C - - - - - 0x0029B2 00:E9A2: D0 4E     BNE bra_E9F2_RTS
bra_E9A4:
C - - - - - 0x0029B4 00:E9A4: B5 7F     LDA ram_007F_object_status_p1,X
C - - - - - 0x0029B6 00:E9A6: C9 02     CMP #$02
C - - - - - 0x0029B8 00:E9A8: 90 48     BCC bra_E9F2_RTS
C - - - - - 0x0029BA 00:E9AA: C9 06     CMP #$06
C - - - - - 0x0029BC 00:E9AC: B0 44     BCS bra_E9F2_RTS
C - - - - - 0x0029BE 00:E9AE: A9 01     LDA #$01
C - - - - - 0x0029C0 00:E9B0: 95 7F     STA ram_007F_object_status_p1,X
C - - - - - 0x0029C2 00:E9B2: 9D 5A 04  STA ram_045A_shock_timer_p1,X
C - - - - - 0x0029C5 00:E9B5: 60        RTS
bra_E9B6:
C - - - - - 0x0029C6 00:E9B6: A9 00     LDA #$00
C - - - - - 0x0029C8 00:E9B8: 9D 12 04  STA ram_0412_y_vel_frac_p1,X
C - - - - - 0x0029CB 00:E9BB: 9D 1B 04  STA ram_041B_y_vel_int_p1,X
C - - - - - 0x0029CE 00:E9BE: 9D 09 04  STA ram_0409_y_pos_frac_p1,X
C - - - - - 0x0029D1 00:E9C1: 85 CB     STA ram_00CB
C - - - - - 0x0029D3 00:E9C3: E0 02     CPX #$02
C - - - - - 0x0029D5 00:E9C5: 90 36     BCC bra_E9FD
C - - - - - 0x0029D7 00:E9C7: B5 88     LDA ram_0088_object_balloons_p1,X
C - - - - - 0x0029D9 00:E9C9: C9 02     CMP #$02
C - - - - - 0x0029DB 00:E9CB: F0 26     BEQ bra_E9F3
C - - - - - 0x0029DD 00:E9CD: C9 01     CMP #$01
C - - - - - 0x0029DF 00:E9CF: D0 21     BNE bra_E9F2_RTS
C - - - - - 0x0029E1 00:E9D1: B5 7F     LDA ram_007F_object_status_p1,X
C - - - - - 0x0029E3 00:E9D3: C9 02     CMP #$02
C - - - - - 0x0029E5 00:E9D5: B0 1B     BCS bra_E9F2_RTS
C - - - - - 0x0029E7 00:E9D7: A9 02     LDA #$02
C - - - - - 0x0029E9 00:E9D9: 95 7F     STA ram_007F_object_status_p1,X
C - - - - - 0x0029EB 00:E9DB: A5 C6     LDA ram_00C6
C - - - - - 0x0029ED 00:E9DD: 9D 3F 04  STA ram_043F_animation_f_timer_p1,X
C - - - - - 0x0029F0 00:E9E0: A9 00     LDA #$00
C - - - - - 0x0029F2 00:E9E2: 9D 24 04  STA ram_0424_x_vel_frac_p1,X
C - - - - - 0x0029F5 00:E9E5: 9D 2D 04  STA ram_042D_x_vel_int_p1,X
C - - - - - 0x0029F8 00:E9E8: 9D 63 04  STA ram_0463,X
C - - - - - 0x0029FB 00:E9EB: 9D 6C 04  STA ram_046C,X
C - - - - - 0x0029FE 00:E9EE: A9 40     LDA #$40                                                   ; Play SFX
C - - - - - 0x002A00 00:E9F0: 85 F1     STA ram_00F1_sfx_2
bra_E9F2_RTS:
C - - - - - 0x002A02 00:E9F2: 60        RTS
bra_E9F3:
C - - - - - 0x002A03 00:E9F3: A9 00     LDA #$00
C - - - - - 0x002A05 00:E9F5: 95 7F     STA ram_007F_object_status_p1,X
C - - - - - 0x002A07 00:E9F7: A9 01     LDA #$01
C - - - - - 0x002A09 00:E9F9: 9D 5A 04  STA ram_045A_shock_timer_p1,X
C - - - - - 0x002A0C 00:E9FC: 60        RTS
bra_E9FD:
C - - - - - 0x002A0D 00:E9FD: B5 7F     LDA ram_007F_object_status_p1,X
C - - - - - 0x002A0F 00:E9FF: C9 01     CMP #$01
C - - - - - 0x002A11 00:EA01: D0 14     BNE bra_EA17_RTS
C - - - - - 0x002A13 00:EA03: C9 06     CMP #$06
C - - - - - 0x002A15 00:EA05: B0 10     BCS bra_EA17_RTS
C - - - - - 0x002A17 00:EA07: BD 24 04  LDA ram_0424_x_vel_frac_p1,X
C - - - - - 0x002A1A 00:EA0A: 1D 2D 04  ORA ram_042D_x_vel_int_p1,X
C - - - - - 0x002A1D 00:EA0D: D0 04     BNE bra_EA13
C - - - - - 0x002A1F 00:EA0F: A9 03     LDA #$03
C - - - - - 0x002A21 00:EA11: D0 02     BNE bra_EA15
bra_EA13:
C - - - - - 0x002A23 00:EA13: A9 02     LDA #$02
bra_EA15:
C - - - - - 0x002A25 00:EA15: 95 7F     STA ram_007F_object_status_p1,X
bra_EA17_RTS:
C - - - - - 0x002A27 00:EA17: 60        RTS

; Object code.

sub_EA18_Object_Update_Anim:
C - - - - - 0x002A28 00:EA18: E0 02     CPX #$02                                                   ; Object is not Player
C - - - - - 0x002A2A 00:EA1A: B0 10     BCS bra_EA2C
C - - - - - 0x002A2C 00:EA1C: B5 BD     LDA ram_00BD_p1_invincibility_flag,X                       ; If Player X Invincible
C - - - - - 0x002A2E 00:EA1E: D0 24     BNE bra_EA44
C - - - - - 0x002A30 00:EA20: B5 7F     LDA ram_007F_object_status_p1,X                            ; If Player X Status == 1
C - - - - - 0x002A32 00:EA22: C9 01     CMP #$01                                                   ; then update animation every 8th frame
C - - - - - 0x002A34 00:EA24: F0 18     BEQ bra_EA3E
C - - - - - 0x002A36 00:EA26: C9 03     CMP #$03                                                   ; If Player X Status != 3
C - - - - - 0x002A38 00:EA28: D0 1A     BNE bra_EA44                                               ; Then update animation
C - - - - - 0x002A3A 00:EA2A: F0 12     BEQ bra_EA3E                                               ; Else update animation every 8th frame
bra_EA2C:
C - - - - - 0x002A3C 00:EA2C: B5 7F     LDA ram_007F_object_status_p1,X                            ; If Enemy Status == 1
C - - - - - 0x002A3E 00:EA2E: C9 01     CMP #$01                                                   ; Then update animation every 8th frame
C - - - - - 0x002A40 00:EA30: F0 0C     BEQ bra_EA3E
C - - - - - 0x002A42 00:EA32: C9 03     CMP #$03                                                   ; If Enemy Status < 1
C - - - - - 0x002A44 00:EA34: 90 0E     BCC bra_EA44                                               ; Then update animation
C - - - - - 0x002A46 00:EA36: A5 19     LDA ram_0019_f_counter
C - - - - - 0x002A48 00:EA38: 29 03     AND #$03                                                   ; Update Animation Frame
C - - - - - 0x002A4A 00:EA3A: D0 0B     BNE bra_EA47                                               ; every 4 frames
C - - - - - 0x002A4C 00:EA3C: F0 06     BEQ bra_EA44
bra_EA3E:
C - - - - - 0x002A4E 00:EA3E: A5 19     LDA ram_0019_f_counter                                     ; Update Animation Frame
C - - - - - 0x002A50 00:EA40: 29 07     AND #$07                                                   ; every 8 frames
C - - - - - 0x002A52 00:EA42: D0 03     BNE bra_EA47
bra_EA44:
C - - - - - 0x002A54 00:EA44: FE 36 04  INC ram_0436_animation_f_p1,X                              ; Increment Animation Frame
bra_EA47:
C - - - - - 0x002A57 00:EA47: BD 36 04  LDA ram_0436_animation_f_p1,X
C - - - - - 0x002A5A 00:EA4A: 29 03     AND #$03                                                   ; Stay within Frame 0 to 3
C - - - - - 0x002A5C 00:EA4C: 9D 36 04  STA ram_0436_animation_f_p1,X
C - - - - - 0x002A5F 00:EA4F: D0 06     BNE bra_EA57_RTS
C - - - - - 0x002A61 00:EA51: B5 7F     LDA ram_007F_object_status_p1,X
C - - - - - 0x002A63 00:EA53: D0 02     BNE bra_EA57_RTS                                           ; Increment Status if not 0
C - - - - - 0x002A65 00:EA55: F6 7F     INC ram_007F_object_status_p1,X
bra_EA57_RTS:
C - - - - - 0x002A67 00:EA57: 60        RTS
sub_EA58:
C - - - - - 0x002A68 00:EA58: BD 75 04  LDA ram_0475_invulnerability_p1,X
C - - - - - 0x002A6B 00:EA5B: F0 03     BEQ bra_EA60
C - - - - - 0x002A6D 00:EA5D: DE 75 04  DEC ram_0475_invulnerability_p1,X
bra_EA60:
C - - - - - 0x002A70 00:EA60: E0 02     CPX #$02
C - - - - - 0x002A72 00:EA62: B0 28     BCS bra_EA8C
C - - - - - 0x002A74 00:EA64: B5 C1     LDA ram_00C1_p1_freeze_flag,X
C - - - - - 0x002A76 00:EA66: F0 24     BEQ bra_EA8C
C - - - - - 0x002A78 00:EA68: A5 19     LDA ram_0019_f_counter
C - - - - - 0x002A7A 00:EA6A: 4A        LSR
C - - - - - 0x002A7B 00:EA6B: 90 1E     BCC bra_EA8B_RTS
C - - - - - 0x002A7D 00:EA6D: FE 36 04  INC ram_0436_animation_f_p1,X
C - - - - - 0x002A80 00:EA70: BD 36 04  LDA ram_0436_animation_f_p1,X
C - - - - - 0x002A83 00:EA73: 29 03     AND #$03
C - - - - - 0x002A85 00:EA75: 9D 36 04  STA ram_0436_animation_f_p1,X
C - - - - - 0x002A88 00:EA78: A9 01     LDA #$01
C - - - - - 0x002A8A 00:EA7A: 95 7F     STA ram_007F_object_status_p1,X
C - - - - - 0x002A8C 00:EA7C: DE 5A 04  DEC ram_045A_shock_timer_p1,X
C - - - - - 0x002A8F 00:EA7F: D0 0A     BNE bra_EA8B_RTS
C - - - - - 0x002A91 00:EA81: A9 00     LDA #$00
C - - - - - 0x002A93 00:EA83: 95 C1     STA ram_00C1_p1_freeze_flag,X
C - - - - - 0x002A95 00:EA85: 95 7F     STA ram_007F_object_status_p1,X
C - - - - - 0x002A97 00:EA87: A9 20     LDA #$20                                                   ; \ Play Fall SFX
C - - - - - 0x002A99 00:EA89: 85 F0     STA ram_00F0_sfx_1                                         ; /
bra_EA8B_RTS:
C - - - - - 0x002A9B 00:EA8B: 60        RTS
bra_EA8C:
C - - - - - 0x002A9C 00:EA8C: BD 12 04  LDA ram_0412_y_vel_frac_p1,X
C - - - - - 0x002A9F 00:EA8F: 18        CLC
C - - - - - 0x002AA0 00:EA90: BC 51 04  LDY ram_0451_object_type_p1,X
C - - - - - 0x002AA3 00:EA93: 79 01 E6  ADC tbl_E601,Y
C - - - - - 0x002AA6 00:EA96: 9D 12 04  STA ram_0412_y_vel_frac_p1,X
C - - - - - 0x002AA9 00:EA99: 90 03     BCC bra_EA9E
C - - - - - 0x002AAB 00:EA9B: FE 1B 04  INC ram_041B_y_vel_int_p1,X
bra_EA9E:
C - - - - - 0x002AAE 00:EA9E: BD 1B 04  LDA ram_041B_y_vel_int_p1,X
C - - - - - 0x002AB1 00:EAA1: 30 1E     BMI bra_EAC1
C - - - - - 0x002AB3 00:EAA3: D9 6D E6  CMP tbl_E66D,Y
C - - - - - 0x002AB6 00:EAA6: 90 34     BCC bra_EADC
C - - - - - 0x002AB8 00:EAA8: D0 08     BNE bra_EAB2
C - - - - - 0x002ABA 00:EAAA: BD 12 04  LDA ram_0412_y_vel_frac_p1,X
C - - - - - 0x002ABD 00:EAAD: D9 61 E6  CMP tbl_E661,Y
C - - - - - 0x002AC0 00:EAB0: 90 2A     BCC bra_EADC
bra_EAB2:
C - - - - - 0x002AC2 00:EAB2: B9 61 E6  LDA tbl_E661,Y
C - - - - - 0x002AC5 00:EAB5: 9D 12 04  STA ram_0412_y_vel_frac_p1,X
C - - - - - 0x002AC8 00:EAB8: B9 6D E6  LDA tbl_E66D,Y
C - - - - - 0x002ACB 00:EABB: 9D 1B 04  STA ram_041B_y_vel_int_p1,X
C - - - - - 0x002ACE 00:EABE: 4C DC EA  JMP loc_EADC
bra_EAC1:
C - - - - - 0x002AD1 00:EAC1: D9 85 E6  CMP tbl_E685,Y
C - - - - - 0x002AD4 00:EAC4: 90 0A     BCC bra_EAD0
C - - - - - 0x002AD6 00:EAC6: D0 14     BNE bra_EADC
C - - - - - 0x002AD8 00:EAC8: BD 12 04  LDA ram_0412_y_vel_frac_p1,X
C - - - - - 0x002ADB 00:EACB: D9 79 E6  CMP tbl_E679,Y
C - - - - - 0x002ADE 00:EACE: B0 0C     BCS bra_EADC
bra_EAD0:
C - - - - - 0x002AE0 00:EAD0: B9 79 E6  LDA tbl_E679,Y
C - - - - - 0x002AE3 00:EAD3: 9D 12 04  STA ram_0412_y_vel_frac_p1,X
C - - - - - 0x002AE6 00:EAD6: B9 85 E6  LDA tbl_E685,Y
C - - - - - 0x002AE9 00:EAD9: 9D 1B 04  STA ram_041B_y_vel_int_p1,X
bra_EADC:
loc_EADC:
C D 3 - - - 0x002AEC 00:EADC: 20 A0 EB  JSR sub_EBA0_Object_Apply_Y_Velocity
C - - - - - 0x002AEF 00:EADF: C9 F8     CMP #$F8
C - - - - - 0x002AF1 00:EAE1: B0 2A     BCS bra_EB0D
C - - - - - 0x002AF3 00:EAE3: C9 E8     CMP #$E8
C - - - - - 0x002AF5 00:EAE5: 90 26     BCC bra_EB0D
C - - - - - 0x002AF7 00:EAE7: A9 FF     LDA #$FF
C - - - - - 0x002AF9 00:EAE9: 95 88     STA ram_0088_object_balloons_p1,X
C - - - - - 0x002AFB 00:EAEB: A9 04     LDA #$04                                                   ; Do Water Plonk
C - - - - - 0x002AFD 00:EAED: 85 BB     STA ram_00BB_water_plonk_animation                         ; Animation
C - - - - - 0x002AFF 00:EAEF: B5 91     LDA ram_0091_object_x_pos_int_p1,X
C - - - - - 0x002B01 00:EAF1: 85 BC     STA ram_00BC
C - - - - - 0x002B03 00:EAF3: E0 02     CPX #$02
C - - - - - 0x002B05 00:EAF5: 90 0E     BCC bra_EB05
C - - - - - 0x002B07 00:EAF7: A9 80     LDA #$80
C - - - - - 0x002B09 00:EAF9: 95 88     STA ram_0088_object_balloons_p1,X
C - - - - - 0x002B0B 00:EAFB: A9 00     LDA #$00
C - - - - - 0x002B0D 00:EAFD: 95 7F     STA ram_007F_object_status_p1,X
C - - - - - 0x002B0F 00:EAFF: A9 01     LDA #$01                                                   ; \ Play Bubble Rising SFX
C - - - - - 0x002B11 00:EB01: 85 F3     STA ram_00F3_sfx_3                                         ; /
C - - - - - 0x002B13 00:EB03: D0 08     BNE bra_EB0D
bra_EB05:
C - - - - - 0x002B15 00:EB05: A5 C8     LDA ram_00C8_phase_type
C - - - - - 0x002B17 00:EB07: D0 04     BNE bra_EB0D
C - - - - - 0x002B19 00:EB09: A9 40     LDA #$40                                                   ; \ Play Splash SFX
C - - - - - 0x002B1B 00:EB0B: 85 F0     STA ram_00F0_sfx_1                                         ; /
bra_EB0D:
C - - - - - 0x002B1D 00:EB0D: BD 2D 04  LDA ram_042D_x_vel_int_p1,X
C - - - - - 0x002B20 00:EB10: 30 1E     BMI bra_EB30
C - - - - - 0x002B22 00:EB12: D9 3D E6  CMP tbl_E63D,Y
C - - - - - 0x002B25 00:EB15: 90 34     BCC bra_EB4B
C - - - - - 0x002B27 00:EB17: D0 08     BNE bra_EB21
C - - - - - 0x002B29 00:EB19: BD 24 04  LDA ram_0424_x_vel_frac_p1,X
C - - - - - 0x002B2C 00:EB1C: D9 31 E6  CMP tbl_E631,Y
C - - - - - 0x002B2F 00:EB1F: 90 2A     BCC bra_EB4B
bra_EB21:
C - - - - - 0x002B31 00:EB21: B9 31 E6  LDA tbl_E631,Y
C - - - - - 0x002B34 00:EB24: 9D 24 04  STA ram_0424_x_vel_frac_p1,X
C - - - - - 0x002B37 00:EB27: B9 3D E6  LDA tbl_E63D,Y
C - - - - - 0x002B3A 00:EB2A: 9D 2D 04  STA ram_042D_x_vel_int_p1,X
C - - - - - 0x002B3D 00:EB2D: 4C 4B EB  JMP loc_EB4B
bra_EB30:
C - - - - - 0x002B40 00:EB30: D9 55 E6  CMP tbl_E655,Y
C - - - - - 0x002B43 00:EB33: 90 0A     BCC bra_EB3F
C - - - - - 0x002B45 00:EB35: D0 14     BNE bra_EB4B
C - - - - - 0x002B47 00:EB37: BD 24 04  LDA ram_0424_x_vel_frac_p1,X
C - - - - - 0x002B4A 00:EB3A: D9 49 E6  CMP tbl_E649,Y
C - - - - - 0x002B4D 00:EB3D: B0 0C     BCS bra_EB4B
bra_EB3F:
C - - - - - 0x002B4F 00:EB3F: B9 49 E6  LDA tbl_E649,Y
C - - - - - 0x002B52 00:EB42: 9D 24 04  STA ram_0424_x_vel_frac_p1,X
C - - - - - 0x002B55 00:EB45: B9 55 E6  LDA tbl_E655,Y
C - - - - - 0x002B58 00:EB48: 9D 2D 04  STA ram_042D_x_vel_int_p1,X
bra_EB4B:
loc_EB4B:
C D 3 - - - 0x002B5B 00:EB4B: 20 8E EB  JSR sub_EB8E_Object_Apply_X_Velocity
C - - - - - 0x002B5E 00:EB4E: A5 16     LDA ram_0016_game_mode
C - - - - - 0x002B60 00:EB50: F0 10     BEQ bra_EB62
C - - - - - 0x002B62 00:EB52: B5 91     LDA ram_0091_object_x_pos_int_p1,X
C - - - - - 0x002B64 00:EB54: C9 10     CMP #$10
C - - - - - 0x002B66 00:EB56: B0 02     BCS bra_EB5A
C - - - - - 0x002B68 00:EB58: A9 10     LDA #$10
bra_EB5A:
C - - - - - 0x002B6A 00:EB5A: C9 E0     CMP #$E0
C - - - - - 0x002B6C 00:EB5C: 90 02     BCC bra_EB60
C - - - - - 0x002B6E 00:EB5E: A9 E0     LDA #$E0
bra_EB60:
C - - - - - 0x002B70 00:EB60: 95 91     STA ram_0091_object_x_pos_int_p1,X
bra_EB62:
C - - - - - 0x002B72 00:EB62: A5 C8     LDA ram_00C8_phase_type
C - - - - - 0x002B74 00:EB64: F0 27     BEQ bra_EB8D_RTS
C - - - - - 0x002B76 00:EB66: B5 88     LDA ram_0088_object_balloons_p1,X
C - - - - - 0x002B78 00:EB68: D0 23     BNE bra_EB8D_RTS
C - - - - - 0x002B7A 00:EB6A: B5 9A     LDA ram_009A_object_y_pos_int_p1,X
C - - - - - 0x002B7C 00:EB6C: C9 C8     CMP #$C8
C - - - - - 0x002B7E 00:EB6E: 90 1D     BCC bra_EB8D_RTS
C - - - - - 0x002B80 00:EB70: A9 C7     LDA #$C7
C - - - - - 0x002B82 00:EB72: 95 9A     STA ram_009A_object_y_pos_int_p1,X
C - - - - - 0x002B84 00:EB74: BD 51 04  LDA ram_0451_object_type_p1,X
C - - - - - 0x002B87 00:EB77: C9 0B     CMP #$0B
C - - - - - 0x002B89 00:EB79: D0 09     BNE bra_EB84
C - - - - - 0x002B8B 00:EB7B: DE 51 04  DEC ram_0451_object_type_p1,X
C - - - - - 0x002B8E 00:EB7E: 20 07 F1  JSR sub_F107_Reverse_Y_Velocity
C - - - - - 0x002B91 00:EB81: 4C 8C F1  JMP loc_F18C
bra_EB84:
C - - - - - 0x002B94 00:EB84: A9 02     LDA #$02
C - - - - - 0x002B96 00:EB86: 95 88     STA ram_0088_object_balloons_p1,X
C - - - - - 0x002B98 00:EB88: A9 03     LDA #$03
C - - - - - 0x002B9A 00:EB8A: 9D 51 04  STA ram_0451_object_type_p1,X
bra_EB8D_RTS:
C - - - - - 0x002B9D 00:EB8D: 60        RTS
sub_EB8E_Object_Apply_X_Velocity:
C - - - - - 0x002B9E 00:EB8E: BD 00 04  LDA ram_0400_x_pos_frac_p1,X                               ; \
C - - - - - 0x002BA1 00:EB91: 18        CLC                                                        ; | Apply Velocity to
C - - - - - 0x002BA2 00:EB92: 7D 24 04  ADC ram_0424_x_vel_frac_p1,X                               ; | X Position (Frac)
C - - - - - 0x002BA5 00:EB95: 9D 00 04  STA ram_0400_x_pos_frac_p1,X                               ; /
C - - - - - 0x002BA8 00:EB98: B5 91     LDA ram_0091_object_x_pos_int_p1,X                         ; \ Apply Velocity to
C - - - - - 0x002BAA 00:EB9A: 7D 2D 04  ADC ram_042D_x_vel_int_p1,X                                ; | X Position (Int)
C - - - - - 0x002BAD 00:EB9D: 95 91     STA ram_0091_object_x_pos_int_p1,X                         ; /
C - - - - - 0x002BAF 00:EB9F: 60        RTS
sub_EBA0_Object_Apply_Y_Velocity:
C - - - - - 0x002BB0 00:EBA0: BD 09 04  LDA ram_0409_y_pos_frac_p1,X                               ; \
C - - - - - 0x002BB3 00:EBA3: 18        CLC                                                        ; | Apply Velocity to
C - - - - - 0x002BB4 00:EBA4: 7D 12 04  ADC ram_0412_y_vel_frac_p1,X                               ; | Y Position (Frac)
C - - - - - 0x002BB7 00:EBA7: 9D 09 04  STA ram_0409_y_pos_frac_p1,X                               ; /
C - - - - - 0x002BBA 00:EBAA: B5 9A     LDA ram_009A_object_y_pos_int_p1,X                         ; \ Apply Velocity to
C - - - - - 0x002BBC 00:EBAC: 7D 1B 04  ADC ram_041B_y_vel_int_p1,X                                ; | Y Position (Int)
C - - - - - 0x002BBF 00:EBAF: 95 9A     STA ram_009A_object_y_pos_int_p1,X                         ; /
C - - - - - 0x002BC1 00:EBB1: 60        RTS
sub_EBB2:
C - - - - - 0x002BC2 00:EBB2: 20 B4 F0  JSR sub_F0B4_Swap_X_Y
C - - - - - 0x002BC5 00:EBB5: 20 8E EB  JSR sub_EB8E_Object_Apply_X_Velocity
C - - - - - 0x002BC8 00:EBB8: 4C B4 F0  JMP loc_F0B4_Swap_X_Y
sub_EBBB:
C - - - - - 0x002BCB 00:EBBB: 20 B4 F0  JSR sub_F0B4_Swap_X_Y
C - - - - - 0x002BCE 00:EBBE: 20 A0 EB  JSR sub_EBA0_Object_Apply_Y_Velocity
C - - - - - 0x002BD1 00:EBC1: 4C B4 F0  JMP loc_F0B4_Swap_X_Y
sub_EBC4:
C - - - - - 0x002BD4 00:EBC4: E0 02     CPX #$02                                                   ; \ If not Player
C - - - - - 0x002BD6 00:EBC6: B0 1B     BCS bra_EBE3                                               ; /
C - - - - - 0x002BD8 00:EBC8: B5 88     LDA ram_0088_object_balloons_p1,X                          ; \ If Player still has Balloons
C - - - - - 0x002BDA 00:EBCA: D0 0A     BNE bra_EBD6                                               ; /
C - - - - - 0x002BDC 00:EBCC: BD 36 04  LDA ram_0436_animation_f_p1,X                              ; \ If Player Animation Frame != 0
C - - - - - 0x002BDF 00:EBCF: D0 05     BNE bra_EBD6                                               ; /
C - - - - - 0x002BE1 00:EBD1: A9 00     LDA #$00                                                   ; \ Then Player Status = 0 (Dead)
C - - - - - 0x002BE3 00:EBD3: 95 7F     STA ram_007F_object_status_p1,X                            ; /
C - - - - - 0x002BE5 00:EBD5: 60        RTS
bra_EBD6:
; Player.
C - - - - - 0x002BE6 00:EBD6: B5 7F     LDA ram_007F_object_status_p1,X                            ; \ If Player Status < 6
C - - - - - 0x002BE8 00:EBD8: C9 06     CMP #$06                                                   ; | Then ?
C - - - - - 0x002BEA 00:EBDA: 90 5C     BCC bra_EC38                                               ; /
C - - - - - 0x002BEC 00:EBDC: A9 01     LDA #$01                                                   ; \ Else Status = 1
C - - - - - 0x002BEE 00:EBDE: 95 7F     STA ram_007F_object_status_p1,X                            ; /
C - - - - - 0x002BF0 00:EBE0: D6 88     DEC ram_0088_object_balloons_p1,X                          ; Decrease one Balloon
C - - - - - 0x002BF2 00:EBE2: 60        RTS
bra_EBE3:
; Enemy.
C - - - - - 0x002BF3 00:EBE3: B5 88     LDA ram_0088_object_balloons_p1,X                          ; \ If Enemy Status == 2
C - - - - - 0x002BF5 00:EBE5: C9 02     CMP #$02                                                   ; | Then ?
C - - - - - 0x002BF7 00:EBE7: F0 4F     BEQ bra_EC38                                               ; /
C - - - - - 0x002BF9 00:EBE9: BD 36 04  LDA ram_0436_animation_f_p1,X                              ; \ If Enemy Animation Frames != 0
C - - - - - 0x002BFC 00:EBEC: D0 0F     BNE bra_EBFD_RTS                                           ; / Then
C - - - - - 0x002BFE 00:EBEE: B5 88     LDA ram_0088_object_balloons_p1,X                          ; \ If Enemy Status != 0
C - - - - - 0x002C00 00:EBF0: D0 05     BNE bra_EBF7                                               ; / Then
C - - - - - 0x002C02 00:EBF2: A9 00     LDA #$00                                                   ; \ Enemy Status = 0 (Dead)
C - - - - - 0x002C04 00:EBF4: 95 7F     STA ram_007F_object_status_p1,X                            ; /
C - - - - - 0x002C06 00:EBF6: 60        RTS
bra_EBF7:
C - - - - - 0x002C07 00:EBF7: B5 7F     LDA ram_007F_object_status_p1,X                            ; \ If Enemy Status != 0
C - - - - - 0x002C09 00:EBF9: D0 03     BNE bra_EBFE                                               ; / Then
- - - - - - 0x002C0B 00:EBFB: F6 7F     INC ram_007F_object_status_p1,X                            ; Increase Enemy Status
bra_EBFD_RTS:
C - - - - - 0x002C0D 00:EBFD: 60        RTS
bra_EBFE:
C - - - - - 0x002C0E 00:EBFE: C9 02     CMP #$02                                                   ; If Player
C - - - - - 0x002C10 00:EC00: 90 FB     BCC bra_EBFD_RTS                                           ; then return
C - - - - - 0x002C12 00:EC02: DE 5A 04  DEC ram_045A_shock_timer_p1,X
C - - - - - 0x002C15 00:EC05: D0 30     BNE bra_EC37_RTS
C - - - - - 0x002C17 00:EC07: A5 C7     LDA ram_00C7
C - - - - - 0x002C19 00:EC09: 9D 5A 04  STA ram_045A_shock_timer_p1,X
C - - - - - 0x002C1C 00:EC0C: F6 7F     INC ram_007F_object_status_p1,X
C - - - - - 0x002C1E 00:EC0E: B5 7F     LDA ram_007F_object_status_p1,X
C - - - - - 0x002C20 00:EC10: C9 07     CMP #$07
C - - - - - 0x002C22 00:EC12: 90 23     BCC bra_EC37_RTS
C - - - - - 0x002C24 00:EC14: A9 02     LDA #$02
C - - - - - 0x002C26 00:EC16: 95 88     STA ram_0088_object_balloons_p1,X
C - - - - - 0x002C28 00:EC18: A9 00     LDA #$00
C - - - - - 0x002C2A 00:EC1A: 95 7F     STA ram_007F_object_status_p1,X
C - - - - - 0x002C2C 00:EC1C: BC 51 04  LDY ram_0451_object_type_p1,X                              ; Check Object Type
C - - - - - 0x002C2F 00:EC1F: B9 AE EC  LDA tbl_ECAE,Y
C - - - - - 0x002C32 00:EC22: BC 7E 04  LDY ram_047E_popped_p1,X
C - - - - - 0x002C35 00:EC25: D0 08     BNE bra_EC2F
C - - - - - 0x002C37 00:EC27: DE 7E 04  DEC ram_047E_popped_p1,X
C - - - - - 0x002C3A 00:EC2A: BD 51 04  LDA ram_0451_object_type_p1,X
C - - - - - 0x002C3D 00:EC2D: 29 03     AND #$03
bra_EC2F:
C - - - - - 0x002C3F 00:EC2F: 9D 51 04  STA ram_0451_object_type_p1,X
C - - - - - 0x002C42 00:EC32: A9 FE     LDA #$FE
C - - - - - 0x002C44 00:EC34: 9D 1B 04  STA ram_041B_y_vel_int_p1,X
bra_EC37_RTS:
C - - - - - 0x002C47 00:EC37: 60        RTS
bra_EC38:
C - - - - - 0x002C48 00:EC38: 20 E9 E6  JSR sub_E6E9_Object_Update_Action
C - - - - - 0x002C4B 00:EC3B: B5 31     LDA ram_0031,X                                             ; \ Check valid actions
C - - - - - 0x002C4D 00:EC3D: 29 C3     AND #$C3                                                   ; | Left/Right/B/A
C - - - - - 0x002C4F 00:EC3F: F0 08     BEQ bra_EC49                                               ; /
C - - - - - 0x002C51 00:EC41: E0 02     CPX #$02                                                   ; \ If Enemy
C - - - - - 0x002C53 00:EC43: B0 04     BCS bra_EC49                                               ; / Skip
C - - - - - 0x002C55 00:EC45: A9 00     LDA #$00                                                   ; \ If Player
C - - - - - 0x002C57 00:EC47: 95 BD     STA ram_00BD_p1_invincibility_flag,X                       ; / Disable invincibility
bra_EC49:
C - - - - - 0x002C59 00:EC49: B5 31     LDA ram_0031,X                                             ; \
C - - - - - 0x002C5B 00:EC4B: 29 40     AND #$40                                                   ; | B button
C - - - - - 0x002C5D 00:EC4D: D0 12     BNE bra_EC61                                               ; /
C - - - - - 0x002C5F 00:EC4F: B5 31     LDA ram_0031,X                                             ; \
C - - - - - 0x002C61 00:EC51: 29 80     AND #$80                                                   ; | A button
C - - - - - 0x002C63 00:EC53: D0 07     BNE bra_EC5C                                               ; /
C - - - - - 0x002C65 00:EC55: A9 00     LDA #$00                                                   ; \
C - - - - - 0x002C67 00:EC57: 9D 20 06  STA ram_0620,X                                             ; | ?
C - - - - - 0x002C6A 00:EC5A: F0 51     BEQ bra_ECAD_RTS                                           ; / Return
bra_EC5C:
C - - - - - 0x002C6C 00:EC5C: BD 20 06  LDA ram_0620,X                                             ; \
C - - - - - 0x002C6F 00:EC5F: D0 4C     BNE bra_ECAD_RTS                                           ; / Return
bra_EC61:
C - - - - - 0x002C71 00:EC61: B5 7F     LDA ram_007F_object_status_p1,X
C - - - - - 0x002C73 00:EC63: C9 02     CMP #$02
C - - - - - 0x002C75 00:EC65: 90 0E     BCC bra_EC75
C - - - - - 0x002C77 00:EC67: D6 9A     DEC ram_009A_object_y_pos_int_p1,X
C - - - - - 0x002C79 00:EC69: D6 9A     DEC ram_009A_object_y_pos_int_p1,X
C - - - - - 0x002C7B 00:EC6B: A9 00     LDA #$00
C - - - - - 0x002C7D 00:EC6D: 9D 12 04  STA ram_0412_y_vel_frac_p1,X
C - - - - - 0x002C80 00:EC70: 9D 1B 04  STA ram_041B_y_vel_int_p1,X
C - - - - - 0x002C83 00:EC73: F0 09     BEQ bra_EC7E
bra_EC75:
C - - - - - 0x002C85 00:EC75: C9 01     CMP #$01
C - - - - - 0x002C87 00:EC77: F0 05     BEQ bra_EC7E
C - - - - - 0x002C89 00:EC79: BD 36 04  LDA ram_0436_animation_f_p1,X
C - - - - - 0x002C8C 00:EC7C: D0 2F     BNE bra_ECAD_RTS                                           ; Return
bra_EC7E:
C - - - - - 0x002C8E 00:EC7E: A9 00     LDA #$00
C - - - - - 0x002C90 00:EC80: 95 7F     STA ram_007F_object_status_p1,X
C - - - - - 0x002C92 00:EC82: A9 01     LDA #$01
C - - - - - 0x002C94 00:EC84: 9D 36 04  STA ram_0436_animation_f_p1,X
C - - - - - 0x002C97 00:EC87: A9 01     LDA #$01
C - - - - - 0x002C99 00:EC89: 9D 20 06  STA ram_0620,X
C - - - - - 0x002C9C 00:EC8C: A0 00     LDY #$00
C - - - - - 0x002C9E 00:EC8E: E0 02     CPX #$02
C - - - - - 0x002CA0 00:EC90: 90 01     BCC bra_EC93
C - - - - - 0x002CA2 00:EC92: C8        INY
bra_EC93:
C - - - - - 0x002CA3 00:EC93: B9 F0 00  LDA ram_00F0_sfx_1,Y
C - - - - - 0x002CA6 00:EC96: 09 10     ORA #$10                                                   ; Play Flutter SFX
C - - - - - 0x002CA8 00:EC98: 99 F0 00  STA ram_00F0_sfx_1,Y
C - - - - - 0x002CAB 00:EC9B: BD 12 04  LDA ram_0412_y_vel_frac_p1,X
C - - - - - 0x002CAE 00:EC9E: 38        SEC
C - - - - - 0x002CAF 00:EC9F: BC 51 04  LDY ram_0451_object_type_p1,X
C - - - - - 0x002CB2 00:ECA2: F9 0D E6  SBC tbl_E60D,Y
C - - - - - 0x002CB5 00:ECA5: 9D 12 04  STA ram_0412_y_vel_frac_p1,X
C - - - - - 0x002CB8 00:ECA8: B0 03     BCS bra_ECAD_RTS
C - - - - - 0x002CBA 00:ECAA: DE 1B 04  DEC ram_041B_y_vel_int_p1,X
bra_ECAD_RTS:
C - - - - - 0x002CBD 00:ECAD: 60        RTS
tbl_ECAE:
- - - - - - 0x002CBE 00:ECAE: 01        .byte $01   ; 
- - - - - - 0x002CBF 00:ECAF: 02        .byte $02   ; 
- - - - - - 0x002CC0 00:ECB0: 02        .byte $02   ; 
- - - - - - 0x002CC1 00:ECB1: 03        .byte $03   ; 
- D 3 - - - 0x002CC2 00:ECB2: 01        .byte $01   ; 
- D 3 - - - 0x002CC3 00:ECB3: 02        .byte $02   ; 
- D 3 - - - 0x002CC4 00:ECB4: 02        .byte $02   ; 
- - - - - - 0x002CC5 00:ECB5: 03        .byte $03   ; 
- - - - - - 0x002CC6 00:ECB6: 01        .byte $01   ; 
- - - - - - 0x002CC7 00:ECB7: 02        .byte $02   ; 
- - - - - - 0x002CC8 00:ECB8: 02        .byte $02   ; 
- - - - - - 0x002CC9 00:ECB9: 03        .byte $03   ; 
sub_ECBA:
C - - - - - 0x002CCA 00:ECBA: B5 7F     LDA ram_007F_object_status_p1,X                            ; \ If Object(x).Status != 0
C - - - - - 0x002CCC 00:ECBC: D0 69     BNE bra_ED27_RTS                                           ; / then don't do anything
C - - - - - 0x002CCE 00:ECBE: 20 B1 E7  JSR sub_E7B1
C - - - - - 0x002CD1 00:ECC1: 20 8E EB  JSR sub_EB8E_Object_Apply_X_Velocity
C - - - - - 0x002CD4 00:ECC4: BD 09 04  LDA ram_0409_y_pos_frac_p1,X
C - - - - - 0x002CD7 00:ECC7: 38        SEC
C - - - - - 0x002CD8 00:ECC8: E9 60     SBC #$60
C - - - - - 0x002CDA 00:ECCA: 9D 09 04  STA ram_0409_y_pos_frac_p1,X
C - - - - - 0x002CDD 00:ECCD: B5 9A     LDA ram_009A_object_y_pos_int_p1,X
C - - - - - 0x002CDF 00:ECCF: E9 00     SBC #$00
C - - - - - 0x002CE1 00:ECD1: 95 9A     STA ram_009A_object_y_pos_int_p1,X
C - - - - - 0x002CE3 00:ECD3: C9 F1     CMP #$F1
C - - - - - 0x002CE5 00:ECD5: 90 04     BCC bra_ECDB
C - - - - - 0x002CE7 00:ECD7: A9 FF     LDA #$FF
C - - - - - 0x002CE9 00:ECD9: 95 88     STA ram_0088_object_balloons_p1,X
bra_ECDB:
C - - - - - 0x002CEB 00:ECDB: 8A        TXA
C - - - - - 0x002CEC 00:ECDC: 48        PHA
C - - - - - 0x002CED 00:ECDD: A0 01     LDY #$01
bra_ECDF:
C - - - - - 0x002CEF 00:ECDF: B9 88 00  LDA ram_0088_object_balloons_p1,Y
C - - - - - 0x002CF2 00:ECE2: F0 3E     BEQ bra_ED22
C - - - - - 0x002CF4 00:ECE4: 30 3C     BMI bra_ED22
C - - - - - 0x002CF6 00:ECE6: B5 9A     LDA ram_009A_object_y_pos_int_p1,X
C - - - - - 0x002CF8 00:ECE8: 38        SEC
C - - - - - 0x002CF9 00:ECE9: F9 9A 00  SBC ram_009A_object_y_pos_int_p1,Y
C - - - - - 0x002CFC 00:ECEC: 20 8E F0  JSR sub_F08E_Absolute
C - - - - - 0x002CFF 00:ECEF: C9 18     CMP #$18
C - - - - - 0x002D01 00:ECF1: B0 2F     BCS bra_ED22
C - - - - - 0x002D03 00:ECF3: B5 91     LDA ram_0091_object_x_pos_int_p1,X
C - - - - - 0x002D05 00:ECF5: 38        SEC
C - - - - - 0x002D06 00:ECF6: F9 91 00  SBC ram_0091_object_x_pos_int_p1,Y
C - - - - - 0x002D09 00:ECF9: 20 8E F0  JSR sub_F08E_Absolute
C - - - - - 0x002D0C 00:ECFC: C9 10     CMP #$10
C - - - - - 0x002D0E 00:ECFE: B0 22     BCS bra_ED22
C - - - - - 0x002D10 00:ED00: A9 FF     LDA #$FF
C - - - - - 0x002D12 00:ED02: 95 7F     STA ram_007F_object_status_p1,X
C - - - - - 0x002D14 00:ED04: A9 03     LDA #$03
C - - - - - 0x002D16 00:ED06: 9D 5A 04  STA ram_045A_shock_timer_p1,X
C - - - - - 0x002D19 00:ED09: A9 78     LDA #$78
C - - - - - 0x002D1B 00:ED0B: 85 C5     STA ram_00C5_lock_scroll_time
C - - - - - 0x002D1D 00:ED0D: A9 02     LDA #$02                                                   ; \ Play Balloon Pop SFX
C - - - - - 0x002D1F 00:ED0F: 85 F0     STA ram_00F0_sfx_1                                         ; /
C - - - - - 0x002D21 00:ED11: A9 32     LDA #$32
C - - - - - 0x002D23 00:ED13: 84 3E     STY ram_003E_score_id_update
C - - - - - 0x002D25 00:ED15: 20 DE D6  JSR sub_D6DE_Score_Add
C - - - - - 0x002D28 00:ED18: A9 01     LDA #$01
C - - - - - 0x002D2A 00:ED1A: A6 3E     LDX ram_003E_score_id_update
C - - - - - 0x002D2C 00:ED1C: 20 71 D8  JSR sub_D871
C - - - - - 0x002D2F 00:ED1F: 68        PLA
C - - - - - 0x002D30 00:ED20: AA        TAX
C - - - - - 0x002D31 00:ED21: 60        RTS
bra_ED22:
C - - - - - 0x002D32 00:ED22: 88        DEY
C - - - - - 0x002D33 00:ED23: 10 BA     BPL bra_ECDF
C - - - - - 0x002D35 00:ED25: 68        PLA
C - - - - - 0x002D36 00:ED26: AA        TAX
bra_ED27_RTS:
C - - - - - 0x002D37 00:ED27: 60        RTS
sub_ED28:
C - - - - - 0x002D38 00:ED28: B4 88     LDY ram_0088_object_balloons_p1,X
C - - - - - 0x002D3A 00:ED2A: 88        DEY
C - - - - - 0x002D3B 00:ED2B: 10 01     BPL bra_ED2E
bra_ED2D_RTS:
C - - - - - 0x002D3D 00:ED2D: 60        RTS
bra_ED2E:
C - - - - - 0x002D3E 00:ED2E: B5 9A     LDA ram_009A_object_y_pos_int_p1,X
C - - - - - 0x002D40 00:ED30: C9 F9     CMP #$F9
C - - - - - 0x002D42 00:ED32: 90 0C     BCC bra_ED40
C - - - - - 0x002D44 00:ED34: BD 1B 04  LDA ram_041B_y_vel_int_p1,X
C - - - - - 0x002D47 00:ED37: 10 F4     BPL bra_ED2D_RTS
C - - - - - 0x002D49 00:ED39: A9 00     LDA #$00
C - - - - - 0x002D4B 00:ED3B: 85 CC     STA ram_00CC_collision_related
C - - - - - 0x002D4D 00:ED3D: 4C E1 ED  JMP loc_EDE1
bra_ED40:
C - - - - - 0x002D50 00:ED40: A4 CD     LDY ram_00CD_amount_of_platforms
C - - - - - 0x002D52 00:ED42: 30 E3     BMI bra_ED27_RTS
loc_ED44:
C D 3 - - - 0x002D54 00:ED44: A9 00     LDA #$00
C - - - - - 0x002D56 00:ED46: 85 CC     STA ram_00CC_collision_related
C - - - - - 0x002D58 00:ED48: B1 27     LDA (ram_0027_plat_coll_pointer_top),Y
C - - - - - 0x002D5A 00:ED4A: 38        SEC
C - - - - - 0x002D5B 00:ED4B: E9 18     SBC #$18
C - - - - - 0x002D5D 00:ED4D: D5 9A     CMP ram_009A_object_y_pos_int_p1,X
C - - - - - 0x002D5F 00:ED4F: B0 65     BCS bra_EDB6
C - - - - - 0x002D61 00:ED51: 69 03     ADC #$03
C - - - - - 0x002D63 00:ED53: D5 9A     CMP ram_009A_object_y_pos_int_p1,X
C - - - - - 0x002D65 00:ED55: 90 04     BCC bra_ED5B
C - - - - - 0x002D67 00:ED57: A9 01     LDA #$01
C - - - - - 0x002D69 00:ED59: D0 0E     BNE bra_ED69
bra_ED5B:
C - - - - - 0x002D6B 00:ED5B: B1 29     LDA (ram_0029_plat_coll_pointer_bottom),Y
C - - - - - 0x002D6D 00:ED5D: D5 9A     CMP ram_009A_object_y_pos_int_p1,X
C - - - - - 0x002D6F 00:ED5F: 90 55     BCC bra_EDB6
C - - - - - 0x002D71 00:ED61: E9 03     SBC #$03
C - - - - - 0x002D73 00:ED63: D5 9A     CMP ram_009A_object_y_pos_int_p1,X
C - - - - - 0x002D75 00:ED65: B0 22     BCS bra_ED89
C - - - - - 0x002D77 00:ED67: A9 02     LDA #$02
bra_ED69:
C - - - - - 0x002D79 00:ED69: 85 CC     STA ram_00CC_collision_related
C - - - - - 0x002D7B 00:ED6B: B1 23     LDA (ram_0023_plat_coll_pointer_left),Y
C - - - - - 0x002D7D 00:ED6D: C9 10     CMP #$10
C - - - - - 0x002D7F 00:ED6F: F0 07     BEQ bra_ED78
C - - - - - 0x002D81 00:ED71: 38        SEC
C - - - - - 0x002D82 00:ED72: E9 0C     SBC #$0C
C - - - - - 0x002D84 00:ED74: D5 91     CMP ram_0091_object_x_pos_int_p1,X
C - - - - - 0x002D86 00:ED76: B0 0D     BCS bra_ED85
bra_ED78:
C - - - - - 0x002D88 00:ED78: B1 25     LDA (ram_0025_plat_coll_pointer_right),Y
C - - - - - 0x002D8A 00:ED7A: C9 FF     CMP #$FF
C - - - - - 0x002D8C 00:ED7C: F0 0B     BEQ bra_ED89
C - - - - - 0x002D8E 00:ED7E: 38        SEC
C - - - - - 0x002D8F 00:ED7F: E9 04     SBC #$04
C - - - - - 0x002D91 00:ED81: D5 91     CMP ram_0091_object_x_pos_int_p1,X
C - - - - - 0x002D93 00:ED83: B0 04     BCS bra_ED89
bra_ED85:
C - - - - - 0x002D95 00:ED85: A9 00     LDA #$00
C - - - - - 0x002D97 00:ED87: 85 CC     STA ram_00CC_collision_related
bra_ED89:
C - - - - - 0x002D99 00:ED89: B1 23     LDA (ram_0023_plat_coll_pointer_left),Y
C - - - - - 0x002D9B 00:ED8B: 38        SEC
C - - - - - 0x002D9C 00:ED8C: E9 10     SBC #$10
C - - - - - 0x002D9E 00:ED8E: F0 10     BEQ bra_EDA0
C - - - - - 0x002DA0 00:ED90: D5 91     CMP ram_0091_object_x_pos_int_p1,X
C - - - - - 0x002DA2 00:ED92: B0 22     BCS bra_EDB6
C - - - - - 0x002DA4 00:ED94: 69 04     ADC #$04
C - - - - - 0x002DA6 00:ED96: D5 91     CMP ram_0091_object_x_pos_int_p1,X
C - - - - - 0x002DA8 00:ED98: 90 06     BCC bra_EDA0
C - - - - - 0x002DAA 00:ED9A: A5 CC     LDA ram_00CC_collision_related
C - - - - - 0x002DAC 00:ED9C: 09 04     ORA #$04
C - - - - - 0x002DAE 00:ED9E: D0 14     BNE bra_EDB4
bra_EDA0:
C - - - - - 0x002DB0 00:EDA0: B1 25     LDA (ram_0025_plat_coll_pointer_right),Y
C - - - - - 0x002DB2 00:EDA2: C9 FF     CMP #$FF
C - - - - - 0x002DB4 00:EDA4: F0 10     BEQ bra_EDB6
C - - - - - 0x002DB6 00:EDA6: D5 91     CMP ram_0091_object_x_pos_int_p1,X
C - - - - - 0x002DB8 00:EDA8: 90 0C     BCC bra_EDB6
C - - - - - 0x002DBA 00:EDAA: E9 04     SBC #$04
C - - - - - 0x002DBC 00:EDAC: D5 91     CMP ram_0091_object_x_pos_int_p1,X
C - - - - - 0x002DBE 00:EDAE: B0 06     BCS bra_EDB6
C - - - - - 0x002DC0 00:EDB0: A5 CC     LDA ram_00CC_collision_related
C - - - - - 0x002DC2 00:EDB2: 09 08     ORA #$08
bra_EDB4:
C - - - - - 0x002DC4 00:EDB4: 85 CC     STA ram_00CC_collision_related
bra_EDB6:
C - - - - - 0x002DC6 00:EDB6: A5 CC     LDA ram_00CC_collision_related
C - - - - - 0x002DC8 00:EDB8: D0 07     BNE bra_EDC1
C - - - - - 0x002DCA 00:EDBA: 88        DEY
C - - - - - 0x002DCB 00:EDBB: 30 03     BMI bra_EDC0_RTS
C - - - - - 0x002DCD 00:EDBD: 4C 44 ED  JMP loc_ED44
bra_EDC0_RTS:
C - - - - - 0x002DD0 00:EDC0: 60        RTS
bra_EDC1:
C - - - - - 0x002DD1 00:EDC1: 46 CC     LSR ram_00CC_collision_related
C - - - - - 0x002DD3 00:EDC3: 90 11     BCC bra_EDD6
C - - - - - 0x002DD5 00:EDC5: BD 1B 04  LDA ram_041B_y_vel_int_p1,X
C - - - - - 0x002DD8 00:EDC8: 30 0C     BMI bra_EDD6
C - - - - - 0x002DDA 00:EDCA: B1 27     LDA (ram_0027_plat_coll_pointer_top),Y
C - - - - - 0x002DDC 00:EDCC: E9 18     SBC #$18
C - - - - - 0x002DDE 00:EDCE: 95 9A     STA ram_009A_object_y_pos_int_p1,X
C - - - - - 0x002DE0 00:EDD0: F6 9A     INC ram_009A_object_y_pos_int_p1,X
C - - - - - 0x002DE2 00:EDD2: A9 01     LDA #$01
C - - - - - 0x002DE4 00:EDD4: 85 CB     STA ram_00CB
bra_EDD6:
C - - - - - 0x002DE6 00:EDD6: 46 CC     LSR ram_00CC_collision_related
C - - - - - 0x002DE8 00:EDD8: 90 1A     BCC bra_EDF4
C - - - - - 0x002DEA 00:EDDA: BD 1B 04  LDA ram_041B_y_vel_int_p1,X
C - - - - - 0x002DED 00:EDDD: 10 15     BPL bra_EDF4
C - - - - - 0x002DEF 00:EDDF: B1 29     LDA (ram_0029_plat_coll_pointer_bottom),Y
loc_EDE1:
C D 3 - - - 0x002DF1 00:EDE1: 95 9A     STA ram_009A_object_y_pos_int_p1,X
C - - - - - 0x002DF3 00:EDE3: 20 07 F1  JSR sub_F107_Reverse_Y_Velocity
C - - - - - 0x002DF6 00:EDE6: 20 8C F1  JSR sub_F18C
C - - - - - 0x002DF9 00:EDE9: E0 02     CPX #$02
C - - - - - 0x002DFB 00:EDEB: B0 03     BCS bra_EDF0
C - - - - - 0x002DFD 00:EDED: 20 33 CC  JSR sub_CC33
bra_EDF0:
C - - - - - 0x002E00 00:EDF0: A5 CB     LDA ram_00CB
C - - - - - 0x002E02 00:EDF2: D0 30     BNE bra_EE24_RTS
bra_EDF4:
C - - - - - 0x002E04 00:EDF4: 46 CC     LSR ram_00CC_collision_related
C - - - - - 0x002E06 00:EDF6: 90 07     BCC bra_EDFF
C - - - - - 0x002E08 00:EDF8: BD 2D 04  LDA ram_042D_x_vel_int_p1,X
C - - - - - 0x002E0B 00:EDFB: 30 02     BMI bra_EDFF
C - - - - - 0x002E0D 00:EDFD: 10 09     BPL bra_EE08
bra_EDFF:
C - - - - - 0x002E0F 00:EDFF: 46 CC     LSR ram_00CC_collision_related
C - - - - - 0x002E11 00:EE01: 90 21     BCC bra_EE24_RTS
C - - - - - 0x002E13 00:EE03: BD 2D 04  LDA ram_042D_x_vel_int_p1,X
C - - - - - 0x002E16 00:EE06: 10 1C     BPL bra_EE24_RTS
bra_EE08:
C - - - - - 0x002E18 00:EE08: 20 DE F0  JSR sub_F0DE_Reverse_X_Velocity
C - - - - - 0x002E1B 00:EE0B: 20 72 F1  JSR sub_F172
C - - - - - 0x002E1E 00:EE0E: BD 2D 04  LDA ram_042D_x_vel_int_p1,X
C - - - - - 0x002E21 00:EE11: 1D 24 04  ORA ram_0424_x_vel_frac_p1,X
C - - - - - 0x002E24 00:EE14: F0 0E     BEQ bra_EE24_RTS
C - - - - - 0x002E26 00:EE16: BD 48 04  LDA ram_0448_direction_p1,X
C - - - - - 0x002E29 00:EE19: 49 01     EOR #$01
C - - - - - 0x002E2B 00:EE1B: 9D 48 04  STA ram_0448_direction_p1,X
C - - - - - 0x002E2E 00:EE1E: A5 F1     LDA ram_00F1_sfx_2
C - - - - - 0x002E30 00:EE20: 09 02     ORA #$02
C - - - - - 0x002E32 00:EE22: 85 F1     STA ram_00F1_sfx_2
bra_EE24_RTS:
C - - - - - 0x002E34 00:EE24: 60        RTS
sub_EE25_Collision:
C - - - - - 0x002E35 00:EE25: A2 07     LDX #$07                                                   ; Seems to compare Balloons from Objects
loc_EE27:
C D 3 - - - 0x002E37 00:EE27: 86 12     STX ram_0012_temp
C - - - - - 0x002E39 00:EE29: A4 12     LDY ram_0012_temp
C - - - - - 0x002E3B 00:EE2B: 88        DEY
C - - - - - 0x002E3C 00:EE2C: 10 03     BPL bra_EE31
bra_EE2E:
C - - - - - 0x002E3E 00:EE2E: 4C 2A EF  JMP loc_EF2A
bra_EE31:
loc_EE31:
C D 3 - - - 0x002E41 00:EE31: B5 88     LDA ram_0088_object_balloons_p1,X                          ; \ If Object(x).Balloon <= 0
C - - - - - 0x002E43 00:EE33: 30 F9     BMI bra_EE2E                                               ; | then skip
C - - - - - 0x002E45 00:EE35: F0 F7     BEQ bra_EE2E                                               ; /
C - - - - - 0x002E47 00:EE37: B9 88 00  LDA ram_0088_object_balloons_p1,Y                          ; \ If Object(y).Balloon <= 0
C - - - - - 0x002E4A 00:EE3A: 30 F2     BMI bra_EE2E                                               ; | then skip
C - - - - - 0x002E4C 00:EE3C: F0 F0     BEQ bra_EE2E                                               ; /
C - - - - - 0x002E4E 00:EE3E: A9 00     LDA #$00
C - - - - - 0x002E50 00:EE40: 85 CC     STA ram_00CC_collision_related
C - - - - - 0x002E52 00:EE42: B9 9A 00  LDA ram_009A_object_y_pos_int_p1,Y                         ; \
C - - - - - 0x002E55 00:EE45: 38        SEC                                                        ; |
C - - - - - 0x002E56 00:EE46: F5 9A     SBC ram_009A_object_y_pos_int_p1,X                         ; | If abs(Object(y).Y - Object(x).Y)
C - - - - - 0x002E58 00:EE48: 20 8E F0  JSR sub_F08E_Absolute                                      ; | <= #$18
C - - - - - 0x002E5B 00:EE4B: C9 18     CMP #$18                                                   ; | then
C - - - - - 0x002E5D 00:EE4D: B0 71     BCS bra_EEC0                                               ; /
C - - - - - 0x002E5F 00:EE4F: B5 9A     LDA ram_009A_object_y_pos_int_p1,X                         ; \
C - - - - - 0x002E61 00:EE51: 18        CLC                                                        ; | If
C - - - - - 0x002E62 00:EE52: 69 18     ADC #$18                                                   ; | abs((Object(y).Y + 7)
C - - - - - 0x002E64 00:EE54: 85 12     STA ram_0012_temp                                          ; |   - (Object(x).Y + #$18))
C - - - - - 0x002E66 00:EE56: B9 9A 00  LDA ram_009A_object_y_pos_int_p1,Y                         ; | >= 4 then
C - - - - - 0x002E69 00:EE59: 18        CLC                                                        ; |
C - - - - - 0x002E6A 00:EE5A: 69 07     ADC #$07                                                   ; |
C - - - - - 0x002E6C 00:EE5C: 38        SEC                                                        ; |
C - - - - - 0x002E6D 00:EE5D: E5 12     SBC ram_0012_temp                                          ; |
C - - - - - 0x002E6F 00:EE5F: 20 8E F0  JSR sub_F08E_Absolute                                      ; |
C - - - - - 0x002E72 00:EE62: C9 04     CMP #$04                                                   ; |
C - - - - - 0x002E74 00:EE64: B0 04     BCS bra_EE6A                                               ; /
C - - - - - 0x002E76 00:EE66: A9 01     LDA #$01
C - - - - - 0x002E78 00:EE68: D0 12     BNE bra_EE7C
bra_EE6A:
C - - - - - 0x002E7A 00:EE6A: B9 9A 00  LDA ram_009A_object_y_pos_int_p1,Y                         ; \
C - - - - - 0x002E7D 00:EE6D: 18        CLC                                                        ; | If abs(Object(y).Y + #$11 - Object(x).Y)
C - - - - - 0x002E7E 00:EE6E: 69 11     ADC #$11                                                   ; | >= 4 then
C - - - - - 0x002E80 00:EE70: 38        SEC                                                        ; |
C - - - - - 0x002E81 00:EE71: F5 9A     SBC ram_009A_object_y_pos_int_p1,X                         ; |
C - - - - - 0x002E83 00:EE73: 20 8E F0  JSR sub_F08E_Absolute                                      ; |
C - - - - - 0x002E86 00:EE76: C9 04     CMP #$04                                                   ; |
C - - - - - 0x002E88 00:EE78: B0 15     BCS bra_EE8F                                               ; /
C - - - - - 0x002E8A 00:EE7A: A9 02     LDA #$02
bra_EE7C:
C - - - - - 0x002E8C 00:EE7C: 85 CC     STA ram_00CC_collision_related
C - - - - - 0x002E8E 00:EE7E: B9 91 00  LDA ram_0091_object_x_pos_int_p1,Y                         ; \
C - - - - - 0x002E91 00:EE81: 38        SEC                                                        ; | If abs(Object(y).X - Object(x).X)
C - - - - - 0x002E92 00:EE82: F5 91     SBC ram_0091_object_x_pos_int_p1,X                         ; | < #$10 then
C - - - - - 0x002E94 00:EE84: 20 8E F0  JSR sub_F08E_Absolute                                      ; |
C - - - - - 0x002E97 00:EE87: C9 10     CMP #$10                                                   ; |
C - - - - - 0x002E99 00:EE89: 90 04     BCC bra_EE8F                                               ; /
C - - - - - 0x002E9B 00:EE8B: A9 00     LDA #$00
C - - - - - 0x002E9D 00:EE8D: 85 CC     STA ram_00CC_collision_related
bra_EE8F:
C - - - - - 0x002E9F 00:EE8F: B5 91     LDA ram_0091_object_x_pos_int_p1,X                         ; \
C - - - - - 0x002EA1 00:EE91: 18        CLC                                                        ; |
C - - - - - 0x002EA2 00:EE92: 69 10     ADC #$10                                                   ; | If abs((Object(y).X + 7)
C - - - - - 0x002EA4 00:EE94: 85 12     STA ram_0012_temp                                          ; |      - (Object(x).X + #$10))
C - - - - - 0x002EA6 00:EE96: B9 91 00  LDA ram_0091_object_x_pos_int_p1,Y                         ; | >= 4 then
C - - - - - 0x002EA9 00:EE99: 18        CLC                                                        ; |
C - - - - - 0x002EAA 00:EE9A: 69 07     ADC #$07                                                   ; |
C - - - - - 0x002EAC 00:EE9C: 38        SEC                                                        ; |
C - - - - - 0x002EAD 00:EE9D: E5 12     SBC ram_0012_temp                                          ; |
C - - - - - 0x002EAF 00:EE9F: 20 8E F0  JSR sub_F08E_Absolute                                      ; |
C - - - - - 0x002EB2 00:EEA2: C9 04     CMP #$04                                                   ; |
C - - - - - 0x002EB4 00:EEA4: B0 04     BCS bra_EEAA                                               ; /
C - - - - - 0x002EB6 00:EEA6: A9 04     LDA #$04
C - - - - - 0x002EB8 00:EEA8: D0 12     BNE bra_EEBC
bra_EEAA:
C - - - - - 0x002EBA 00:EEAA: B9 91 00  LDA ram_0091_object_x_pos_int_p1,Y                         ; \
C - - - - - 0x002EBD 00:EEAD: 18        CLC                                                        ; | If abs(Object(y).X + 9 - Object(x).X)
C - - - - - 0x002EBE 00:EEAE: 69 09     ADC #$09                                                   ; | >= 4 then
C - - - - - 0x002EC0 00:EEB0: 38        SEC                                                        ; |
C - - - - - 0x002EC1 00:EEB1: F5 91     SBC ram_0091_object_x_pos_int_p1,X                         ; |
C - - - - - 0x002EC3 00:EEB3: 20 8E F0  JSR sub_F08E_Absolute                                      ; |
C - - - - - 0x002EC6 00:EEB6: C9 04     CMP #$04                                                   ; |
C - - - - - 0x002EC8 00:EEB8: B0 06     BCS bra_EEC0                                               ; /
C - - - - - 0x002ECA 00:EEBA: A9 08     LDA #$08
bra_EEBC:
C - - - - - 0x002ECC 00:EEBC: 05 CC     ORA ram_00CC_collision_related
C - - - - - 0x002ECE 00:EEBE: 85 CC     STA ram_00CC_collision_related
bra_EEC0:
C - - - - - 0x002ED0 00:EEC0: A9 00     LDA #$00                                                   ; \
C - - - - - 0x002ED2 00:EEC2: 85 4B     STA ram_004B                                               ; /
C - - - - - 0x002ED4 00:EEC4: 46 CC     LSR ram_00CC_collision_related                             ; \ [$CC].bit0 = Velocity Y related
C - - - - - 0x002ED6 00:EEC6: 90 05     BCC bra_EECD                                               ; |
C - - - - - 0x002ED8 00:EEC8: 20 A6 F0  JSR sub_F0A6                                               ; |
C - - - - - 0x002EDB 00:EECB: 30 09     BMI bra_EED6                                               ; /
bra_EECD:
C - - - - - 0x002EDD 00:EECD: 46 CC     LSR ram_00CC_collision_related                             ; \ [$CC].bit1 = Velocity Y related
C - - - - - 0x002EDF 00:EECF: 90 20     BCC bra_EEF1                                               ; |
C - - - - - 0x002EE1 00:EED1: 20 A6 F0  JSR sub_F0A6                                               ; |
C - - - - - 0x002EE4 00:EED4: 30 1B     BMI bra_EEF1                                               ; /
bra_EED6:
C - - - - - 0x002EE6 00:EED6: 20 BD F0  JSR sub_F0BD                                               ; \ Do both object X and Y exist?
C - - - - - 0x002EE9 00:EED9: B0 12     BCS bra_EEED                                               ; /
C - - - - - 0x002EEB 00:EEDB: 20 07 F1  JSR sub_F107_Reverse_Y_Velocity
C - - - - - 0x002EEE 00:EEDE: 20 8C F1  JSR sub_F18C
C - - - - - 0x002EF1 00:EEE1: 20 B4 F0  JSR sub_F0B4_Swap_X_Y
C - - - - - 0x002EF4 00:EEE4: 20 07 F1  JSR sub_F107_Reverse_Y_Velocity
C - - - - - 0x002EF7 00:EEE7: 20 8C F1  JSR sub_F18C
C - - - - - 0x002EFA 00:EEEA: 20 B4 F0  JSR sub_F0B4_Swap_X_Y
bra_EEED:
C - - - - - 0x002EFD 00:EEED: A9 01     LDA #$01
C - - - - - 0x002EFF 00:EEEF: 85 4B     STA ram_004B
bra_EEF1:
C - - - - - 0x002F01 00:EEF1: 46 CC     LSR ram_00CC_collision_related                             ; \ [$CC].bit2 = Velocity X related
C - - - - - 0x002F03 00:EEF3: 90 05     BCC bra_EEFA                                               ; |
C - - - - - 0x002F05 00:EEF5: 20 98 F0  JSR sub_F098                                               ; |
C - - - - - 0x002F08 00:EEF8: 30 09     BMI bra_EF03                                               ; /
bra_EEFA:
C - - - - - 0x002F0A 00:EEFA: 46 CC     LSR ram_00CC_collision_related                             ; \ [$CC].bit3 = Velocity X related
C - - - - - 0x002F0C 00:EEFC: 90 20     BCC bra_EF1E                                               ; |
C - - - - - 0x002F0E 00:EEFE: 20 98 F0  JSR sub_F098                                               ; |
C - - - - - 0x002F11 00:EF01: 30 1B     BMI bra_EF1E                                               ; /
bra_EF03:
C - - - - - 0x002F13 00:EF03: 20 BD F0  JSR sub_F0BD                                               ; \ Do both object X and Y exist?
C - - - - - 0x002F16 00:EF06: B0 12     BCS bra_EF1A                                               ; /
C - - - - - 0x002F18 00:EF08: 20 DE F0  JSR sub_F0DE_Reverse_X_Velocity
C - - - - - 0x002F1B 00:EF0B: 20 72 F1  JSR sub_F172
C - - - - - 0x002F1E 00:EF0E: 20 B4 F0  JSR sub_F0B4_Swap_X_Y
C - - - - - 0x002F21 00:EF11: 20 DE F0  JSR sub_F0DE_Reverse_X_Velocity
C - - - - - 0x002F24 00:EF14: 20 72 F1  JSR sub_F172
C - - - - - 0x002F27 00:EF17: 20 B4 F0  JSR sub_F0B4_Swap_X_Y
bra_EF1A:
C - - - - - 0x002F2A 00:EF1A: A9 01     LDA #$01
C - - - - - 0x002F2C 00:EF1C: 85 4B     STA ram_004B
bra_EF1E:
C - - - - - 0x002F2E 00:EF1E: 20 37 EF  JSR sub_EF37
C - - - - - 0x002F31 00:EF21: 20 B4 F0  JSR sub_F0B4_Swap_X_Y
C - - - - - 0x002F34 00:EF24: 20 37 EF  JSR sub_EF37
C - - - - - 0x002F37 00:EF27: 20 B4 F0  JSR sub_F0B4_Swap_X_Y
loc_EF2A:
C D 3 - - - 0x002F3A 00:EF2A: 88        DEY                                                        ; \
C - - - - - 0x002F3B 00:EF2B: 30 03     BMI bra_EF30                                               ; | Loop Y Objects
C - - - - - 0x002F3D 00:EF2D: 4C 31 EE  JMP loc_EE31                                               ; /
bra_EF30:
C - - - - - 0x002F40 00:EF30: CA        DEX                                                        ; \
C - - - - - 0x002F41 00:EF31: 30 03     BMI bra_EF36_RTS                                           ; | Loop X Objects
C - - - - - 0x002F43 00:EF33: 4C 27 EE  JMP loc_EE27                                               ; /
bra_EF36_RTS:
C - - - - - 0x002F46 00:EF36: 60        RTS
sub_EF37:
C - - - - - 0x002F47 00:EF37: E0 02     CPX #$02                                                   ; \ Is Object X a player?
C - - - - - 0x002F49 00:EF39: 90 07     BCC bra_EF42                                               ; |
C - - - - - 0x002F4B 00:EF3B: C0 02     CPY #$02                                                   ; | Is Object Y a player?
C - - - - - 0x002F4D 00:EF3D: 90 03     BCC bra_EF42                                               ; /
C - - - - - 0x002F4F 00:EF3F: 4C 43 F0  JMP loc_F043                                               ; Skip
bra_EF42:
C - - - - - 0x002F52 00:EF42: A9 00     LDA #$00
C - - - - - 0x002F54 00:EF44: 8D 87 04  STA ram_0487
C - - - - - 0x002F57 00:EF47: BD 75 04  LDA ram_0475_invulnerability_p1,X
C - - - - - 0x002F5A 00:EF4A: F0 03     BEQ bra_EF4F
C - - - - - 0x002F5C 00:EF4C: 4C 43 F0  JMP loc_F043                                               ; Skip
bra_EF4F:
C - - - - - 0x002F5F 00:EF4F: A5 4B     LDA ram_004B
C - - - - - 0x002F61 00:EF51: D0 03     BNE bra_EF56
C - - - - - 0x002F63 00:EF53: 4C 43 F0  JMP loc_F043                                               ; Skip
bra_EF56:
C - - - - - 0x002F66 00:EF56: E0 02     CPX #$02
C - - - - - 0x002F68 00:EF58: B0 07     BCS bra_EF61
C - - - - - 0x002F6A 00:EF5A: B5 BD     LDA ram_00BD_p1_invincibility_flag,X
C - - - - - 0x002F6C 00:EF5C: F0 14     BEQ bra_EF72
C - - - - - 0x002F6E 00:EF5E: 4C 43 F0  JMP loc_F043                                               ; Skip
bra_EF61:
C - - - - - 0x002F71 00:EF61: B5 88     LDA ram_0088_object_balloons_p1,X
C - - - - - 0x002F73 00:EF63: C9 01     CMP #$01
C - - - - - 0x002F75 00:EF65: D0 0B     BNE bra_EF72
C - - - - - 0x002F77 00:EF67: B5 7F     LDA ram_007F_object_status_p1,X
C - - - - - 0x002F79 00:EF69: C9 02     CMP #$02
C - - - - - 0x002F7B 00:EF6B: B0 12     BCS bra_EF7F
C - - - - - 0x002F7D 00:EF6D: A9 01     LDA #$01
C - - - - - 0x002F7F 00:EF6F: 8D 87 04  STA ram_0487
bra_EF72:
C - - - - - 0x002F82 00:EF72: B9 9A 00  LDA ram_009A_object_y_pos_int_p1,Y
C - - - - - 0x002F85 00:EF75: 18        CLC
C - - - - - 0x002F86 00:EF76: 69 04     ADC #$04
C - - - - - 0x002F88 00:EF78: D5 9A     CMP ram_009A_object_y_pos_int_p1,X
C - - - - - 0x002F8A 00:EF7A: 90 03     BCC bra_EF7F
C - - - - - 0x002F8C 00:EF7C: 4C 43 F0  JMP loc_F043                                               ; Skip
bra_EF7F:
C - - - - - 0x002F8F 00:EF7F: A9 14     LDA #$14                                                   ; #$14 ticks after Balloon is popped before vulnerable again
C - - - - - 0x002F91 00:EF81: 9D 75 04  STA ram_0475_invulnerability_p1,X
C - - - - - 0x002F94 00:EF84: A9 00     LDA #$00
C - - - - - 0x002F96 00:EF86: 9D 36 04  STA ram_0436_animation_f_p1,X
C - - - - - 0x002F99 00:EF89: C0 02     CPY #$02
C - - - - - 0x002F9B 00:EF8B: 90 0A     BCC bra_EF97
C - - - - - 0x002F9D 00:EF8D: B9 88 00  LDA ram_0088_object_balloons_p1,Y
C - - - - - 0x002FA0 00:EF90: C9 02     CMP #$02
C - - - - - 0x002FA2 00:EF92: F0 03     BEQ bra_EF97
C - - - - - 0x002FA4 00:EF94: 4C 43 F0  JMP loc_F043                                               ; Skip
bra_EF97:
C - - - - - 0x002FA7 00:EF97: A5 F0     LDA ram_00F0_sfx_1
C - - - - - 0x002FA9 00:EF99: 09 02     ORA #$02                                                   ; Play Balloon Pop SFX
C - - - - - 0x002FAB 00:EF9B: 85 F0     STA ram_00F0_sfx_1
C - - - - - 0x002FAD 00:EF9D: B5 88     LDA ram_0088_object_balloons_p1,X
C - - - - - 0x002FAF 00:EF9F: C9 02     CMP #$02
C - - - - - 0x002FB1 00:EFA1: D0 1D     BNE bra_EFC0
C - - - - - 0x002FB3 00:EFA3: E0 02     CPX #$02
C - - - - - 0x002FB5 00:EFA5: B0 19     BCS bra_EFC0
C - - - - - 0x002FB7 00:EFA7: 84 12     STY ram_0012_temp
C - - - - - 0x002FB9 00:EFA9: B4 7F     LDY ram_007F_object_status_p1,X
C - - - - - 0x002FBB 00:EFAB: B9 53 F0  LDA tbl_F053,Y
C - - - - - 0x002FBE 00:EFAE: A4 12     LDY ram_0012_temp
C - - - - - 0x002FC0 00:EFB0: 48        PHA
C - - - - - 0x002FC1 00:EFB1: 68        PLA
C - - - - - 0x002FC2 00:EFB2: D0 03     BNE bra_EFB7
- - - - - - 0x002FC4 00:EFB4: 4C 43 F0  JMP loc_F043                                               ; Skip
bra_EFB7:
C - - - - - 0x002FC7 00:EFB7: 95 7F     STA ram_007F_object_status_p1,X
C - - - - - 0x002FC9 00:EFB9: A9 00     LDA #$00
C - - - - - 0x002FCB 00:EFBB: 9D 36 04  STA ram_0436_animation_f_p1,X
C - - - - - 0x002FCE 00:EFBE: F0 2A     BEQ bra_EFEA
bra_EFC0:
C - - - - - 0x002FD0 00:EFC0: D6 88     DEC ram_0088_object_balloons_p1,X
C - - - - - 0x002FD2 00:EFC2: D0 0A     BNE bra_EFCE
C - - - - - 0x002FD4 00:EFC4: A9 FF     LDA #$FF
C - - - - - 0x002FD6 00:EFC6: 9D 1B 04  STA ram_041B_y_vel_int_p1,X
C - - - - - 0x002FD9 00:EFC9: A9 00     LDA #$00
C - - - - - 0x002FDB 00:EFCB: 9D 12 04  STA ram_0412_y_vel_frac_p1,X
bra_EFCE:
C - - - - - 0x002FDE 00:EFCE: A9 00     LDA #$00
C - - - - - 0x002FE0 00:EFD0: 95 7F     STA ram_007F_object_status_p1,X
C - - - - - 0x002FE2 00:EFD2: 9D 24 04  STA ram_0424_x_vel_frac_p1,X
C - - - - - 0x002FE5 00:EFD5: 9D 2D 04  STA ram_042D_x_vel_int_p1,X
C - - - - - 0x002FE8 00:EFD8: B5 91     LDA ram_0091_object_x_pos_int_p1,X
C - - - - - 0x002FEA 00:EFDA: 30 04     BMI bra_EFE0
C - - - - - 0x002FEC 00:EFDC: A9 FF     LDA #$FF
C - - - - - 0x002FEE 00:EFDE: D0 02     BNE bra_EFE2
bra_EFE0:
C - - - - - 0x002FF0 00:EFE0: A9 00     LDA #$00
bra_EFE2:
C - - - - - 0x002FF2 00:EFE2: 9D 6C 04  STA ram_046C,X
C - - - - - 0x002FF5 00:EFE5: A9 80     LDA #$80
C - - - - - 0x002FF7 00:EFE7: 9D 63 04  STA ram_0463,X
bra_EFEA:
C - - - - - 0x002FFA 00:EFEA: 84 12     STY ram_0012_temp
C - - - - - 0x002FFC 00:EFEC: BC 51 04  LDY ram_0451_object_type_p1,X
C - - - - - 0x002FFF 00:EFEF: B9 5E F0  LDA tbl_F05E,Y
C - - - - - 0x003002 00:EFF2: 9D 51 04  STA ram_0451_object_type_p1,X
C - - - - - 0x003005 00:EFF5: A9 01     LDA #$01
C - - - - - 0x003007 00:EFF7: 9D 7E 04  STA ram_047E_popped_p1,X
C - - - - - 0x00300A 00:EFFA: A4 12     LDY ram_0012_temp
C - - - - - 0x00300C 00:EFFC: C0 02     CPY #$02
C - - - - - 0x00300E 00:EFFE: B0 43     BCS bra_F043                                               ; Skip
C - - - - - 0x003010 00:F000: BD 51 04  LDA ram_0451_object_type_p1,X
C - - - - - 0x003013 00:F003: C9 07     CMP #$07
C - - - - - 0x003015 00:F005: F0 0A     BEQ bra_F011
C - - - - - 0x003017 00:F007: C9 08     CMP #$08
C - - - - - 0x003019 00:F009: 90 06     BCC bra_F011
C - - - - - 0x00301B 00:F00B: A5 F1     LDA ram_00F1_sfx_2
C - - - - - 0x00301D 00:F00D: 09 80     ORA #$80
C - - - - - 0x00301F 00:F00F: 85 F1     STA ram_00F1_sfx_2
bra_F011:
C - - - - - 0x003021 00:F011: BC 51 04  LDY ram_0451_object_type_p1,X
C - - - - - 0x003024 00:F014: B9 6A F0  LDA tbl_F06A,Y
C - - - - - 0x003027 00:F017: 85 13     STA ram_0013_temp
C - - - - - 0x003029 00:F019: AD 87 04  LDA ram_0487
C - - - - - 0x00302C 00:F01C: F0 05     BEQ bra_F023
C - - - - - 0x00302E 00:F01E: B9 76 F0  LDA tbl_F076,Y
C - - - - - 0x003031 00:F021: 85 13     STA ram_0013_temp
bra_F023:
C - - - - - 0x003033 00:F023: B9 82 F0  LDA tbl_F082,Y
C - - - - - 0x003036 00:F026: 18        CLC
C - - - - - 0x003037 00:F027: 6D 87 04  ADC ram_0487
C - - - - - 0x00303A 00:F02A: 85 14     STA ram_0014_temp
C - - - - - 0x00303C 00:F02C: A5 12     LDA ram_0012_temp
C - - - - - 0x00303E 00:F02E: 85 3E     STA ram_003E_score_id_update
C - - - - - 0x003040 00:F030: 48        PHA
C - - - - - 0x003041 00:F031: 8A        TXA
C - - - - - 0x003042 00:F032: 48        PHA
C - - - - - 0x003043 00:F033: A5 13     LDA ram_0013_temp
C - - - - - 0x003045 00:F035: 48        PHA
C - - - - - 0x003046 00:F036: A5 14     LDA ram_0014_temp
C - - - - - 0x003048 00:F038: 20 71 D8  JSR sub_D871
C - - - - - 0x00304B 00:F03B: 68        PLA
C - - - - - 0x00304C 00:F03C: 20 DE D6  JSR sub_D6DE_Score_Add
C - - - - - 0x00304F 00:F03F: 68        PLA
C - - - - - 0x003050 00:F040: AA        TAX
C - - - - - 0x003051 00:F041: 68        PLA
C - - - - - 0x003052 00:F042: A8        TAY
bra_F043:
loc_F043:
C D 3 - - - 0x003053 00:F043: BD 51 04  LDA ram_0451_object_type_p1,X                              ; \ If Object X is not dead
C - - - - - 0x003056 00:F046: C9 0B     CMP #$0B                                                   ; | then don't play any SFX
C - - - - - 0x003058 00:F048: D0 08     BNE bra_F052_RTS                                           ; /
C - - - - - 0x00305A 00:F04A: A5 C8     LDA ram_00C8_phase_type                                    ; \ If it's Bonus Phase
C - - - - - 0x00305C 00:F04C: D0 04     BNE bra_F052_RTS                                           ; / then don't play any SFX
C - - - - - 0x00305E 00:F04E: A9 20     LDA #$20                                                   ; \ Play Fall SFX
C - - - - - 0x003060 00:F050: 85 F0     STA ram_00F0_sfx_1                                         ; /
bra_F052_RTS:
C - - - - - 0x003062 00:F052: 60        RTS
tbl_F053:
- D 3 - - - 0x003063 00:F053: 06        .byte $06   ; 
- D 3 - - - 0x003064 00:F054: 06        .byte $06   ; 
- D 3 - - - 0x003065 00:F055: 07        .byte $07   ; 
- D 3 - - - 0x003066 00:F056: 08        .byte $08   ; 
- D 3 - - - 0x003067 00:F057: 09        .byte $09   ; 
- D 3 - - - 0x003068 00:F058: 0A        .byte $0A   ; 
- - - - - - 0x003069 00:F059: 00        .byte $00   ; 
- - - - - - 0x00306A 00:F05A: 00        .byte $00   ; 
- - - - - - 0x00306B 00:F05B: 00        .byte $00   ; 
- - - - - - 0x00306C 00:F05C: 00        .byte $00   ; 
- - - - - - 0x00306D 00:F05D: 00        .byte $00   ; 
tbl_F05E:
- D 3 - - - 0x00306E 00:F05E: 04        .byte $04   ; 
- D 3 - - - 0x00306F 00:F05F: 05        .byte $05   ; 
- D 3 - - - 0x003070 00:F060: 06        .byte $06   ; 
- D 3 - - - 0x003071 00:F061: 07        .byte $07   ; 
- D 3 - - - 0x003072 00:F062: 08        .byte $08   ; 
- D 3 - - - 0x003073 00:F063: 09        .byte $09   ; 
- D 3 - - - 0x003074 00:F064: 0A        .byte $0A   ; 
- D 3 - - - 0x003075 00:F065: 0B        .byte $0B   ; 
- - - - - - 0x003076 00:F066: 08        .byte $08   ; 
- - - - - - 0x003077 00:F067: 09        .byte $09   ; 
- - - - - - 0x003078 00:F068: 0A        .byte $0A   ; 
- - - - - - 0x003079 00:F069: 0B        .byte $0B   ; 
tbl_F06A:
- - - - - - 0x00307A 00:F06A: 00        .byte $00   ; 
- - - - - - 0x00307B 00:F06B: 00        .byte $00   ; 
- - - - - - 0x00307C 00:F06C: 00        .byte $00   ; 
- - - - - - 0x00307D 00:F06D: 00        .byte $00   ; 
- D 3 - - - 0x00307E 00:F06E: 32        .byte $32   ; 
- D 3 - - - 0x00307F 00:F06F: 4B        .byte $4B   ; 
- D 3 - - - 0x003080 00:F070: 64        .byte $64   ; 
- D 3 - - - 0x003081 00:F071: 64        .byte $64   ; 
- D 3 - - - 0x003082 00:F072: 4B        .byte $4B   ; 
- D 3 - - - 0x003083 00:F073: 64        .byte $64   ; 
- D 3 - - - 0x003084 00:F074: 96        .byte $96   ; 
- D 3 - - - 0x003085 00:F075: 64        .byte $64   ; 
tbl_F076:
- - - - - - 0x003086 00:F076: 00        .byte $00   ; 
- - - - - - 0x003087 00:F077: 00        .byte $00   ; 
- - - - - - 0x003088 00:F078: 00        .byte $00   ; 
- - - - - - 0x003089 00:F079: 00        .byte $00   ; 
- - - - - - 0x00308A 00:F07A: 32        .byte $32   ; 
- - - - - - 0x00308B 00:F07B: 4B        .byte $4B   ; 
- - - - - - 0x00308C 00:F07C: 64        .byte $64   ; 
- - - - - - 0x00308D 00:F07D: 64        .byte $64   ; 
- D 3 - - - 0x00308E 00:F07E: 64        .byte $64   ; 
- D 3 - - - 0x00308F 00:F07F: 96        .byte $96   ; 
- D 3 - - - 0x003090 00:F080: C8        .byte $C8   ; 
- - - - - - 0x003091 00:F081: 64        .byte $64   ; 
tbl_F082:
- - - - - - 0x003092 00:F082: 00        .byte $00   ; 
- - - - - - 0x003093 00:F083: 00        .byte $00   ; 
- - - - - - 0x003094 00:F084: 00        .byte $00   ; 
- - - - - - 0x003095 00:F085: 00        .byte $00   ; 
- D 3 - - - 0x003096 00:F086: 01        .byte $01   ; 
- D 3 - - - 0x003097 00:F087: 02        .byte $02   ; 
- D 3 - - - 0x003098 00:F088: 03        .byte $03   ; 
- D 3 - - - 0x003099 00:F089: 03        .byte $03   ; 
- D 3 - - - 0x00309A 00:F08A: 02        .byte $02   ; 
- D 3 - - - 0x00309B 00:F08B: 03        .byte $03   ; 
- D 3 - - - 0x00309C 00:F08C: 04        .byte $04   ; 
- D 3 - - - 0x00309D 00:F08D: 03        .byte $03   ; 
sub_F08E_Absolute:
C - - - - - 0x00309E 00:F08E: 48        PHA                                                        ; \
C - - - - - 0x00309F 00:F08F: 68        PLA                                                        ; |
C - - - - - 0x0030A0 00:F090: 10 05     BPL bra_F097_RTS                                           ; | Get Absolute Value of A
C - - - - - 0x0030A2 00:F092: 49 FF     EOR #$FF                                                   ; |
C - - - - - 0x0030A4 00:F094: 18        CLC                                                        ; |
C - - - - - 0x0030A5 00:F095: 69 01     ADC #$01                                                   ; |
bra_F097_RTS:
C - - - - - 0x0030A7 00:F097: 60        RTS                                                        ; /
sub_F098:
C - - - - - 0x0030A8 00:F098: B9 24 04  LDA ram_0424_x_vel_frac_p1,Y                               ; \ Object(y).XVelocityFrac - Object(x).XVelocityFrac
C - - - - - 0x0030AB 00:F09B: 38        SEC                                                        ; |
C - - - - - 0x0030AC 00:F09C: FD 24 04  SBC ram_0424_x_vel_frac_p1,X                               ; /
C - - - - - 0x0030AF 00:F09F: B9 2D 04  LDA ram_042D_x_vel_int_p1,Y                                ; \ Object(y).XVelocity - Object(x).XVelocity
C - - - - - 0x0030B2 00:F0A2: FD 2D 04  SBC ram_042D_x_vel_int_p1,X                                ; /
C - - - - - 0x0030B5 00:F0A5: 60        RTS
sub_F0A6:
C - - - - - 0x0030B6 00:F0A6: B9 12 04  LDA ram_0412_y_vel_frac_p1,Y                               ; \ Object(y).YVelocityFrac - Object(x).YVelocityFrac
C - - - - - 0x0030B9 00:F0A9: 38        SEC                                                        ; |
C - - - - - 0x0030BA 00:F0AA: FD 12 04  SBC ram_0412_y_vel_frac_p1,X                               ; /
C - - - - - 0x0030BD 00:F0AD: B9 1B 04  LDA ram_041B_y_vel_int_p1,Y                                ; \ Object(y).YVelocity - Object(x).YVelocity
C - - - - - 0x0030C0 00:F0B0: FD 1B 04  SBC ram_041B_y_vel_int_p1,X                                ; /
C - - - - - 0x0030C3 00:F0B3: 60        RTS
sub_F0B4_Swap_X_Y:
loc_F0B4_Swap_X_Y:
C D 3 - - - 0x0030C4 00:F0B4: 86 12     STX ram_0012_temp
C - - - - - 0x0030C6 00:F0B6: 84 13     STY ram_0013_temp
C - - - - - 0x0030C8 00:F0B8: A6 13     LDX ram_0013_temp
C - - - - - 0x0030CA 00:F0BA: A4 12     LDY ram_0012_temp
C - - - - - 0x0030CC 00:F0BC: 60        RTS
sub_F0BD:
C - - - - - 0x0030CD 00:F0BD: E0 02     CPX #$02                                                   ; \
C - - - - - 0x0030CF 00:F0BF: 90 1C     BCC bra_F0DD_RTS                                           ; /
C - - - - - 0x0030D1 00:F0C1: B5 7F     LDA ram_007F_object_status_p1,X                            ; \ If Object(x).Status < 2
C - - - - - 0x0030D3 00:F0C3: C9 02     CMP #$02                                                   ; |
C - - - - - 0x0030D5 00:F0C5: 90 16     BCC bra_F0DD_RTS                                           ; /
C - - - - - 0x0030D7 00:F0C7: A9 01     LDA #$01                                                   ; \ If 1 - Object(x).Balloons >= 0
C - - - - - 0x0030D9 00:F0C9: D5 88     CMP ram_0088_object_balloons_p1,X                          ; |
C - - - - - 0x0030DB 00:F0CB: B0 10     BCS bra_F0DD_RTS                                           ; |
- - - - - - 0x0030DD 00:F0CD: C0 02     CPY #$02                                                   ; | If 1 - Object(x).Balloons - 2 < 0
- - - - - - 0x0030DF 00:F0CF: 90 0C     BCC bra_F0DD_RTS                                           ; /
- - - - - - 0x0030E1 00:F0D1: B9 7F 00  LDA ram_007F_object_status_p1,Y                            ; \ If Object(y).Status < 2
- - - - - - 0x0030E4 00:F0D4: C9 02     CMP #$02                                                   ; |
- - - - - - 0x0030E6 00:F0D6: 90 05     BCC bra_F0DD_RTS                                           ; /
- - - - - - 0x0030E8 00:F0D8: A9 01     LDA #$01                                                   ; \ If 1 - Object(y).Balloons
- - - - - - 0x0030EA 00:F0DA: D9 88 00  CMP ram_0088_object_balloons_p1,Y                          ; /
bra_F0DD_RTS:
C - - - - - 0x0030ED 00:F0DD: 60        RTS
sub_F0DE_Reverse_X_Velocity:
C - - - - - 0x0030EE 00:F0DE: A9 00     LDA #$00                                                   ; \
C - - - - - 0x0030F0 00:F0E0: 38        SEC                                                        ; |
C - - - - - 0x0030F1 00:F0E1: FD 24 04  SBC ram_0424_x_vel_frac_p1,X                               ; | Reverse X Velocity of Object X
C - - - - - 0x0030F4 00:F0E4: 9D 24 04  STA ram_0424_x_vel_frac_p1,X                               ; | (Bounce Horizontally)
C - - - - - 0x0030F7 00:F0E7: A9 00     LDA #$00                                                   ; |
C - - - - - 0x0030F9 00:F0E9: FD 2D 04  SBC ram_042D_x_vel_int_p1,X                                ; |
C - - - - - 0x0030FC 00:F0EC: 9D 2D 04  STA ram_042D_x_vel_int_p1,X                                ; /
C - - - - - 0x0030FF 00:F0EF: A9 00     LDA #$00                                                   ; \
C - - - - - 0x003101 00:F0F1: 38        SEC                                                        ; | ?
C - - - - - 0x003102 00:F0F2: FD 63 04  SBC ram_0463,X                                             ; |
C - - - - - 0x003105 00:F0F5: 9D 63 04  STA ram_0463,X                                             ; /
C - - - - - 0x003108 00:F0F8: A9 00     LDA #$00                                                   ; \
C - - - - - 0x00310A 00:F0FA: FD 6C 04  SBC ram_046C,X                                             ; | ?
C - - - - - 0x00310D 00:F0FD: 9D 6C 04  STA ram_046C,X                                             ; /
C - - - - - 0x003110 00:F100: B5 31     LDA ram_0031,X                                             ; \
C - - - - - 0x003112 00:F102: 29 40     AND #$40                                                   ; | ?
C - - - - - 0x003114 00:F104: 95 31     STA ram_0031,X                                             ; /
C - - - - - 0x003116 00:F106: 60        RTS
sub_F107_Reverse_Y_Velocity:
C - - - - - 0x003117 00:F107: A9 00     LDA #$00                                                   ; \
C - - - - - 0x003119 00:F109: 38        SEC                                                        ; |
C - - - - - 0x00311A 00:F10A: FD 12 04  SBC ram_0412_y_vel_frac_p1,X                               ; | Reverse Y Velocity of Object X
C - - - - - 0x00311D 00:F10D: 9D 12 04  STA ram_0412_y_vel_frac_p1,X                               ; | (Bounce Vertically)
C - - - - - 0x003120 00:F110: A9 00     LDA #$00                                                   ; |
C - - - - - 0x003122 00:F112: FD 1B 04  SBC ram_041B_y_vel_int_p1,X                                ; |
C - - - - - 0x003125 00:F115: 9D 1B 04  STA ram_041B_y_vel_int_p1,X                                ; |
C - - - - - 0x003128 00:F118: 60        RTS                                                        ; /
sub_F119:
C - - - - - 0x003129 00:F119: 85 2D     STA ram_002D
C - - - - - 0x00312B 00:F11B: A5 2C     LDA ram_002C_temp_y_vel_int                                ; \ If Velocity Int >= 0
C - - - - - 0x00312D 00:F11D: 10 24     BPL bra_F143                                               ; / then go to bra_F143
C - - - - - 0x00312F 00:F11F: A9 00     LDA #$00                                                   ; \
C - - - - - 0x003131 00:F121: 38        SEC                                                        ; | Get absolute value of Velocity Frac
C - - - - - 0x003132 00:F122: E5 2B     SBC ram_002B_temp_y_vel_frac                               ; |
C - - - - - 0x003134 00:F124: 85 2B     STA ram_002B_temp_y_vel_frac                               ; /
C - - - - - 0x003136 00:F126: A9 00     LDA #$00                                                   ; \
C - - - - - 0x003138 00:F128: E5 2C     SBC ram_002C_temp_y_vel_int                                ; | Get absolute value of Velocity Int
C - - - - - 0x00313A 00:F12A: 85 2C     STA ram_002C_temp_y_vel_int                                ; /
C - - - - - 0x00313C 00:F12C: 20 43 F1  JSR sub_F143
C - - - - - 0x00313F 00:F12F: A9 00     LDA #$00
C - - - - - 0x003141 00:F131: 38        SEC
C - - - - - 0x003142 00:F132: E5 2E     SBC ram_002E
C - - - - - 0x003144 00:F134: 85 2E     STA ram_002E
C - - - - - 0x003146 00:F136: A9 00     LDA #$00
C - - - - - 0x003148 00:F138: E5 2F     SBC ram_002F
C - - - - - 0x00314A 00:F13A: 85 2F     STA ram_002F
C - - - - - 0x00314C 00:F13C: A9 00     LDA #$00
C - - - - - 0x00314E 00:F13E: E5 30     SBC ram_0030
C - - - - - 0x003150 00:F140: 85 30     STA ram_0030
C - - - - - 0x003152 00:F142: 60        RTS
bra_F143:
sub_F143:
C - - - - - 0x003153 00:F143: 8A        TXA                                                        ; \ Push X
C - - - - - 0x003154 00:F144: 48        PHA                                                        ; /
C - - - - - 0x003155 00:F145: A9 00     LDA #$00                                                   ; \
C - - - - - 0x003157 00:F147: 85 2E     STA ram_002E                                               ; | Init
C - - - - - 0x003159 00:F149: 85 2F     STA ram_002F                                               ; |
C - - - - - 0x00315B 00:F14B: 85 30     STA ram_0030                                               ; /
C - - - - - 0x00315D 00:F14D: A2 08     LDX #$08                                                   ; \ -Loop 8 times
bra_F14F:
C - - - - - 0x00315F 00:F14F: 06 2E     ASL ram_002E                                               ; |
C - - - - - 0x003161 00:F151: 26 2F     ROL ram_002F                                               ; |
C - - - - - 0x003163 00:F153: 26 30     ROL ram_0030                                               ; |
C - - - - - 0x003165 00:F155: 06 2D     ASL ram_002D                                               ; |
C - - - - - 0x003167 00:F157: 90 13     BCC bra_F16C                                               ; |
C - - - - - 0x003169 00:F159: 18        CLC                                                        ; |
C - - - - - 0x00316A 00:F15A: A5 2B     LDA ram_002B_temp_y_vel_frac                               ; | Old Velocity Frac
C - - - - - 0x00316C 00:F15C: 65 2E     ADC ram_002E                                               ; |
C - - - - - 0x00316E 00:F15E: 85 2E     STA ram_002E                                               ; |
C - - - - - 0x003170 00:F160: A5 2C     LDA ram_002C_temp_y_vel_int                                ; | Old Velocity Int
C - - - - - 0x003172 00:F162: 65 2F     ADC ram_002F                                               ; |
C - - - - - 0x003174 00:F164: 85 2F     STA ram_002F                                               ; |
C - - - - - 0x003176 00:F166: A9 00     LDA #$00                                                   ; |
C - - - - - 0x003178 00:F168: 65 30     ADC ram_0030                                               ; |
C - - - - - 0x00317A 00:F16A: 85 30     STA ram_0030                                               ; |
bra_F16C:
C - - - - - 0x00317C 00:F16C: CA        DEX                                                        ; |
C - - - - - 0x00317D 00:F16D: D0 E0     BNE bra_F14F                                               ; /
C - - - - - 0x00317F 00:F16F: 68        PLA                                                        ; \ Pull X
C - - - - - 0x003180 00:F170: AA        TAX                                                        ; /
C - - - - - 0x003181 00:F171: 60        RTS
sub_F172:
C - - - - - 0x003182 00:F172: BD 24 04  LDA ram_0424_x_vel_frac_p1,X                               ; \ X Velocity Frac
C - - - - - 0x003185 00:F175: 85 2B     STA ram_002B_temp_y_vel_frac                               ; /
C - - - - - 0x003187 00:F177: BD 2D 04  LDA ram_042D_x_vel_int_p1,X                                ; \ X Velocity Int
C - - - - - 0x00318A 00:F17A: 85 2C     STA ram_002C_temp_y_vel_int                                ; /
C - - - - - 0x00318C 00:F17C: A9 CD     LDA #$CD                                                   ; \ ?
C - - - - - 0x00318E 00:F17E: 20 19 F1  JSR sub_F119                                               ; /
C - - - - - 0x003191 00:F181: A5 2F     LDA ram_002F                                               ; \ Update X Velocity Frac
C - - - - - 0x003193 00:F183: 9D 24 04  STA ram_0424_x_vel_frac_p1,X                               ; /
C - - - - - 0x003196 00:F186: A5 30     LDA ram_0030                                               ; \ Update X Velocity Int
C - - - - - 0x003198 00:F188: 9D 2D 04  STA ram_042D_x_vel_int_p1,X                                ; /
C - - - - - 0x00319B 00:F18B: 60        RTS
loc_F18C:
sub_F18C:
C D 3 - - - 0x00319C 00:F18C: BD 12 04  LDA ram_0412_y_vel_frac_p1,X                               ; \ Y Velocity Frac
C - - - - - 0x00319F 00:F18F: 85 2B     STA ram_002B_temp_y_vel_frac                               ; /
C - - - - - 0x0031A1 00:F191: BD 1B 04  LDA ram_041B_y_vel_int_p1,X                                ; \ Y Velocity Int
C - - - - - 0x0031A4 00:F194: 85 2C     STA ram_002C_temp_y_vel_int                                ; /
C - - - - - 0x0031A6 00:F196: A9 CD     LDA #$CD                                                   ; \ ?
C - - - - - 0x0031A8 00:F198: 20 19 F1  JSR sub_F119                                               ; /
C - - - - - 0x0031AB 00:F19B: A5 2F     LDA ram_002F                                               ; \ Update Y Velocity Frac
C - - - - - 0x0031AD 00:F19D: 9D 12 04  STA ram_0412_y_vel_frac_p1,X                               ; /
C - - - - - 0x0031B0 00:F1A0: A5 30     LDA ram_0030                                               ; \ Update Y Velocity Int
C - - - - - 0x0031B2 00:F1A2: 9D 1B 04  STA ram_041B_y_vel_int_p1,X                                ; /
C - - - - - 0x0031B5 00:F1A5: 60        RTS
sub_F1A6:
C - - - - - 0x0031B6 00:F1A6: A0 04     LDY #$04
bra_F1A8:
C - - - - - 0x0031B8 00:F1A8: A5 13     LDA ram_0013_temp
C - - - - - 0x0031BA 00:F1AA: 0A        ASL
C - - - - - 0x0031BB 00:F1AB: 66 13     ROR ram_0013_temp
C - - - - - 0x0031BD 00:F1AD: 66 12     ROR ram_0012_temp
C - - - - - 0x0031BF 00:F1AF: 88        DEY
C - - - - - 0x0031C0 00:F1B0: D0 F6     BNE bra_F1A8
C - - - - - 0x0031C2 00:F1B2: 60        RTS
sub_F1B3_RNG:
C - - - - - 0x0031C3 00:F1B3: 8A        TXA                                                        ; \ Push X
C - - - - - 0x0031C4 00:F1B4: 48        PHA                                                        ; /
C - - - - - 0x0031C5 00:F1B5: A2 0B     LDX #$0B                                                   ; \ Loop 11 times
bra_F1B7:
C - - - - - 0x0031C7 00:F1B7: 06 1B     ASL ram_001B_rng_output_seed                               ; |
C - - - - - 0x0031C9 00:F1B9: 26 1C     ROL ram_001C_rng_seed                                      ; |
C - - - - - 0x0031CB 00:F1BB: 2A        ROL                                                        ; |
C - - - - - 0x0031CC 00:F1BC: 2A        ROL                                                        ; | Do Pseudo Random
C - - - - - 0x0031CD 00:F1BD: 45 1B     EOR ram_001B_rng_output_seed                               ; | Number Generator stuff?
C - - - - - 0x0031CF 00:F1BF: 2A        ROL                                                        ; |
C - - - - - 0x0031D0 00:F1C0: 45 1B     EOR ram_001B_rng_output_seed                               ; |
C - - - - - 0x0031D2 00:F1C2: 4A        LSR                                                        ; |
C - - - - - 0x0031D3 00:F1C3: 4A        LSR                                                        ; |
C - - - - - 0x0031D4 00:F1C4: 49 FF     EOR #$FF                                                   ; |
C - - - - - 0x0031D6 00:F1C6: 29 01     AND #$01                                                   ; |
C - - - - - 0x0031D8 00:F1C8: 05 1B     ORA ram_001B_rng_output_seed                               ; |
C - - - - - 0x0031DA 00:F1CA: 85 1B     STA ram_001B_rng_output_seed                               ; |
C - - - - - 0x0031DC 00:F1CC: CA        DEX                                                        ; |
C - - - - - 0x0031DD 00:F1CD: D0 E8     BNE bra_F1B7                                               ; |
C - - - - - 0x0031DF 00:F1CF: 68        PLA                                                        ; \ Pull X
C - - - - - 0x0031E0 00:F1D0: AA        TAX                                                        ; /
C - - - - - 0x0031E1 00:F1D1: A5 1B     LDA ram_001B_rng_output_seed                               ; Return A = [$1B]
C - - - - - 0x0031E3 00:F1D3: 60        RTS
loc_F1D4:
C D 3 - - - 0x0031E4 00:F1D4: 20 C1 DA  JSR sub_DAC1_Title_Screen_Loop
C - - - - - 0x0031E7 00:F1D7: A2 09     LDX #$09                                                   ; \
bra_F1D9:
C - - - - - 0x0031E9 00:F1D9: A9 00     LDA #$00                                                   ; | Player 1 Score to 000000
C - - - - - 0x0031EB 00:F1DB: 95 03     STA ram_0003_p1_score_0000x,X                              ; |
C - - - - - 0x0031ED 00:F1DD: CA        DEX                                                        ; |
C - - - - - 0x0031EE 00:F1DE: 10 F9     BPL bra_F1D9                                               ; /
C - - - - - 0x0031F0 00:F1E0: 85 3E     STA ram_003E_score_id_update                               ; Update Player 1 Score
C - - - - - 0x0031F2 00:F1E2: E6 41     INC ram_0041_p1_lives                                      ; +1 Life to Player 1
C - - - - - 0x0031F4 00:F1E4: 20 DE D6  JSR sub_D6DE_Score_Add                                     ; Update Player Score
C - - - - - 0x0031F7 00:F1E7: A9 0F     LDA #$0F                                                   ; \ Enable Sound Channels
C - - - - - 0x0031F9 00:F1E9: 8D 15 40  STA $4015                                                  ; /
C - - - - - 0x0031FC 00:F1EC: A9 01     LDA #$01                                                   ; \ Stop All Sounds
C - - - - - 0x0031FE 00:F1EE: 85 F0     STA ram_00F0_sfx_1                                         ; /
C - - - - - 0x003200 00:F1F0: A9 02     LDA #$02
sub_F1F2:
C - - - - - 0x003202 00:F1F2: 85 41     STA ram_0041_p1_lives                                      ; Set Player 1 Lives to 2
C - - - - - 0x003204 00:F1F4: A4 40     LDY ram_0040_2p_flag                                       ; \ If it's 2 players
C - - - - - 0x003206 00:F1F6: D0 02     BNE bra_F1FA                                               ; | Then give lives to Player 2
C - - - - - 0x003208 00:F1F8: A9 FF     LDA #$FF                                                   ; / Else no lives
bra_F1FA:
C - - - - - 0x00320A 00:F1FA: 85 42     STA ram_0042_p2_lives                                      ; Set Player 2 Lives to -1 or 2
C - - - - - 0x00320C 00:F1FC: A2 00     LDX #$00
C - - - - - 0x00320E 00:F1FE: 8E 88 04  STX ram_0488_trip_plat_start_x_pos
C - - - - - 0x003211 00:F201: 86 3B     STX ram_003B_current_level_header                          ; Current Level Header = 0
C - - - - - 0x003213 00:F203: 86 3C     STX ram_003C_current_phase                                 ; Current Phase = 0
C - - - - - 0x003215 00:F205: 8E 58 05  STX ram_0558_bonus_phase_intensity_level                   ; Bonus Phase Level = 0
C - - - - - 0x003218 00:F208: CA        DEX
C - - - - - 0x003219 00:F209: 86 89     STX ram_0089_object_balloons_p2                            ; Set Player 2 Balloons to -1
C - - - - - 0x00321B 00:F20B: A6 40     LDX ram_0040_2p_flag                                       ; \
bra_F20D:
C - - - - - 0x00321D 00:F20D: 20 B0 F3  JSR sub_F3B0_Init_Player_Type                              ; | Set up both Player Types
C - - - - - 0x003220 00:F210: CA        DEX                                                        ; |
C - - - - - 0x003221 00:F211: 10 FA     BPL bra_F20D                                               ; /
loc_F213:
C D 3 - - - 0x003223 00:F213: A9 00     LDA #$00                                                   ; \ Set to Regular Phase
C - - - - - 0x003225 00:F215: 85 C8     STA ram_00C8_phase_type                                    ; /
C - - - - - 0x003227 00:F217: A5 3C     LDA ram_003C_current_phase                                 ; \
C - - - - - 0x003229 00:F219: 4A        LSR                                                        ; |
C - - - - - 0x00322A 00:F21A: 4A        LSR                                                        ; | (Current Phase >> 2) cannot
C - - - - - 0x00322B 00:F21B: C9 08     CMP #$08                                                   ; | be higher than 8
C - - - - - 0x00322D 00:F21D: 90 02     BCC bra_F221                                               ; |
- - - - - - 0x00322F 00:F21F: A9 08     LDA #$08                                                   ; /
bra_F221:
C - - - - - 0x003231 00:F221: AA        TAX
C - - - - - 0x003232 00:F222: BD BA F3  LDA tbl_F3BA,X
C - - - - - 0x003235 00:F225: 85 C6     STA ram_00C6
C - - - - - 0x003237 00:F227: BD C3 F3  LDA tbl_F3C3,X
C - - - - - 0x00323A 00:F22A: 85 C7     STA ram_00C7
C - - - - - 0x00323C 00:F22C: A5 3C     LDA ram_003C_current_phase                                 ; \
C - - - - - 0x00323E 00:F22E: C9 02     CMP #$02                                                   ; | If Current Phase >= 2
C - - - - - 0x003240 00:F230: B0 06     BCS bra_F238                                               ; / then
C - - - - - 0x003242 00:F232: A9 03     LDA #$03
C - - - - - 0x003244 00:F234: 85 C6     STA ram_00C6
C - - - - - 0x003246 00:F236: 85 C7     STA ram_00C7
bra_F238:
C - - - - - 0x003248 00:F238: A2 07     LDX #$07                                                   ; \
bra_F23A:
C - - - - - 0x00324A 00:F23A: A9 00     LDA #$00                                                   ; | Initialize variables for each object (except Fish?)
C - - - - - 0x00324C 00:F23C: 9D 48 04  STA ram_0448_direction_p1,X                                ; | - Direction (0 = Left, 1 = Right)
C - - - - - 0x00324F 00:F23F: 9D 75 04  STA ram_0475_invulnerability_p1,X                          ; | - Invulnerability
C - - - - - 0x003252 00:F242: 9D 7E 04  STA ram_047E_popped_p1,X                                   ; | - Popped Flag (Enemy Can Level Up)
C - - - - - 0x003255 00:F245: 9D 24 04  STA ram_0424_x_vel_frac_p1,X                               ; | - X Velocity (Frac)
C - - - - - 0x003258 00:F248: 9D 2D 04  STA ram_042D_x_vel_int_p1,X                                ; | - X Velocity (Int)
C - - - - - 0x00325B 00:F24B: 9D 12 04  STA ram_0412_y_vel_frac_p1,X                               ; | - Y Velocity (Frac)
C - - - - - 0x00325E 00:F24E: 9D 1B 04  STA ram_041B_y_vel_int_p1,X                                ; | - Y Velocity (Int)
C - - - - - 0x003261 00:F251: 9D 63 04  STA ram_0463,X                                             ; | - Related to bouncing back from enemy, balloon, or side
C - - - - - 0x003264 00:F254: 9D 6C 04  STA ram_046C,X                                             ; | - Related to bouncing back from enemy, balloon, or side
C - - - - - 0x003267 00:F257: 9D 00 04  STA ram_0400_x_pos_frac_p1,X                               ; | - X Positions (Frac)
C - - - - - 0x00326A 00:F25A: 9D 09 04  STA ram_0409_y_pos_frac_p1,X                               ; | - Y Positions (Frac)
C - - - - - 0x00326D 00:F25D: A9 01     LDA #$01                                                   ; |
C - - - - - 0x00326F 00:F25F: 9D 3F 04  STA ram_043F_animation_f_timer_p1,X                        ; | - Animation Frame Timer
C - - - - - 0x003272 00:F262: 9D 5A 04  STA ram_045A_shock_timer_p1,X                              ; | - Shock Timer
C - - - - - 0x003275 00:F265: A9 03     LDA #$03                                                   ; |
C - - - - - 0x003277 00:F267: 9D 36 04  STA ram_0436_animation_f_p1,X                              ; | - Animation Frame
C - - - - - 0x00327A 00:F26A: CA        DEX                                                        ; |
C - - - - - 0x00327B 00:F26B: 10 CD     BPL bra_F23A                                               ; /
C - - - - - 0x00327D 00:F26D: A2 05     LDX #$05                                                   ; \
bra_F26F:
C - - - - - 0x00327F 00:F26F: A9 FF     LDA #$FF                                                   ; | Initialize Enemies
C - - - - - 0x003281 00:F271: 95 8A     STA ram_008A_object_balloons_enemy1,X                      ; |
C - - - - - 0x003283 00:F273: CA        DEX                                                        ; |
C - - - - - 0x003284 00:F274: 10 F9     BPL bra_F26F                                               ; /
C - - - - - 0x003286 00:F276: A6 40     LDX ram_0040_2p_flag                                       ; \
bra_F278:
C - - - - - 0x003288 00:F278: 20 86 F3  JSR sub_F386_Initialize_Player_X                           ; | Initialize Players
C - - - - - 0x00328B 00:F27B: CA        DEX                                                        ; |
C - - - - - 0x00328C 00:F27C: 10 FA     BPL bra_F278                                               ; /
C - - - - - 0x00328E 00:F27E: 20 46 D2  JSR sub_D246_Clear_PPU
C - - - - - 0x003291 00:F281: 20 93 D2  JSR sub_D293_Init_Game_Mode
C - - - - - 0x003294 00:F284: A5 C6     LDA ram_00C6
C - - - - - 0x003296 00:F286: C9 10     CMP #$10
C - - - - - 0x003298 00:F288: B0 04     BCS bra_F28E
C - - - - - 0x00329A 00:F28A: A9 58     LDA #$58
C - - - - - 0x00329C 00:F28C: 85 C6     STA ram_00C6
bra_F28E:
C - - - - - 0x00329E 00:F28E: 20 A5 F4  JSR sub_F4A5_Init_Fish
C - - - - - 0x0032A1 00:F291: 20 FF D8  JSR sub_D8FF
C - - - - - 0x0032A4 00:F294: A5 16     LDA ram_0016_game_mode
C - - - - - 0x0032A6 00:F296: F0 03     BEQ bra_F29B                                               ; Balloon Fight Game Mode
C - - - - - 0x0032A8 00:F298: 4C C5 C1  JMP loc_C1C5_Balloon_Trip                                  ; Balloon Trip Game Mode
bra_F29B:
C - - - - - 0x0032AB 00:F29B: A5 C8     LDA ram_00C8_phase_type
C - - - - - 0x0032AD 00:F29D: F0 03     BEQ bra_F2A2_Balloon_Fight_Load                            ; Normal Phase Type
C - - - - - 0x0032AF 00:F29F: 4C 13 CF  JMP loc_CF13                                               ; Bonus Phase Type
bra_F2A2_Balloon_Fight_Load:
C - - - - - 0x0032B2 00:F2A2: 20 16 C7  JSR sub_C716_Init_Cloud_Bolt
C - - - - - 0x0032B5 00:F2A5: A5 3B     LDA ram_003B_current_level_header                          ; \ Level Header
C - - - - - 0x0032B7 00:F2A7: 29 03     AND #$03                                                   ; | For the first of every four Normal Phases
C - - - - - 0x0032B9 00:F2A9: D0 08     BNE bra_F2B3                                               ; /
C - - - - - 0x0032BB 00:F2AB: A9 08     LDA #$08                                                   ; \ Play the Stage Start Jingle
C - - - - - 0x0032BD 00:F2AD: 85 F2     STA ram_00F2_music_jingle                                  ; /
C - - - - - 0x0032BF 00:F2AF: A6 3A     LDX ram_003A_demo_flag
C - - - - - 0x0032C1 00:F2B1: D0 06     BNE bra_F2B9_Balloon_Fight_Loop                            ; Demo Flag
bra_F2B3:
C - - - - - 0x0032C3 00:F2B3: A9 FF     LDA #$FF                                                   ; \ Show Phase Number for
C - - - - - 0x0032C5 00:F2B5: 85 3D     STA ram_003D_phase_number_display_time                     ; / 255 frames
C - - - - - 0x0032C7 00:F2B7: E6 3C     INC ram_003C_current_phase                                 ; Increment Current Phase Number
bra_F2B9_Balloon_Fight_Loop:
loc_F2B9_Balloon_Fight_Loop:
; Balloon Fight Game Loop
C D 3 - - - 0x0032C9 00:F2B9: 20 70 F4  JSR sub_F470_Pause
C - - - - - 0x0032CC 00:F2BC: A5 3D     LDA ram_003D_phase_number_display_time                     ; \
C - - - - - 0x0032CE 00:F2BE: F0 05     BEQ bra_F2C5                                               ; | Display Phase Number
C - - - - - 0x0032D0 00:F2C0: C6 3D     DEC ram_003D_phase_number_display_time                     ; | if the time is not 0
C - - - - - 0x0032D2 00:F2C2: 20 CC F3  JSR sub_F3CC_Phase_Display                                 ; /
bra_F2C5:
C - - - - - 0x0032D5 00:F2C5: 20 B3 F1  JSR sub_F1B3_RNG
C - - - - - 0x0032D8 00:F2C8: 20 91 E6  JSR sub_E691_Object_Manage
C - - - - - 0x0032DB 00:F2CB: 20 F9 C6  JSR sub_C6F9_Fish_Manage
C - - - - - 0x0032DE 00:F2CE: 20 90 C7  JSR sub_C790_Cloud_Bolt
C - - - - - 0x0032E1 00:F2D1: 20 31 C8  JSR sub_C831_Cloud_Blink
C - - - - - 0x0032E4 00:F2D4: 20 B7 C8  JSR sub_C8B7
C - - - - - 0x0032E7 00:F2D7: 20 DD D8  JSR sub_D8DD
C - - - - - 0x0032EA 00:F2DA: 20 87 E5  JSR sub_E587
C - - - - - 0x0032ED 00:F2DD: 20 74 CB  JSR sub_CB74_Flipper_Manage
C - - - - - 0x0032F0 00:F2E0: E6 4C     INC ram_004C_star_update
C - - - - - 0x0032F2 00:F2E2: A6 40     LDX ram_0040_2p_flag                                       ; X = 2 Player Flag
bra_F2E4:
C - - - - - 0x0032F4 00:F2E4: B5 88     LDA ram_0088_object_balloons_p1,X                          ; \ If Player X has balloons
C - - - - - 0x0032F6 00:F2E6: 10 25     BPL bra_F30D                                               ; / then skip respawn code
C - - - - - 0x0032F8 00:F2E8: A5 3A     LDA ram_003A_demo_flag                                     ; \ If Demo Play
C - - - - - 0x0032FA 00:F2EA: D0 3A     BNE bra_F326_RTS                                           ; / then return
C - - - - - 0x0032FC 00:F2EC: B5 41     LDA ram_0041_p1_lives,X                                    ; \ If Player X Lives < 0
C - - - - - 0x0032FE 00:F2EE: 30 1D     BMI bra_F30D                                               ; / then skip respawn code
C - - - - - 0x003300 00:F2F0: D6 C3     DEC ram_00C3_p1_respawn_delay,X                            ; \ Decrease Player X Respawn Delay
C - - - - - 0x003302 00:F2F2: D0 33     BNE bra_F327                                               ; / If not 0 then ?
C - - - - - 0x003304 00:F2F4: 8A        TXA                                                        ; \ Push X
C - - - - - 0x003305 00:F2F5: 48        PHA                                                        ; /
C - - - - - 0x003306 00:F2F6: 20 26 C7  JSR sub_C726
C - - - - - 0x003309 00:F2F9: 68        PLA                                                        ; \ Pull X
C - - - - - 0x00330A 00:F2FA: AA        TAX                                                        ; /
C - - - - - 0x00330B 00:F2FB: A0 02     LDY #$02
C - - - - - 0x00330D 00:F2FD: D6 41     DEC ram_0041_p1_lives,X                                    ; Decrement Player X Lives
C - - - - - 0x00330F 00:F2FF: 84 46     STY ram_0046_status_bar_update_flag                        ; Update Status Bar
C - - - - - 0x003311 00:F301: 30 0A     BMI bra_F30D                                               ; If Player X has no more lives then don't respawn
C - - - - - 0x003313 00:F303: 20 86 F3  JSR sub_F386_Initialize_Player_X
C - - - - - 0x003316 00:F306: 20 B0 F3  JSR sub_F3B0_Init_Player_Type
C - - - - - 0x003319 00:F309: A9 80     LDA #$80                                                   ; \ Play Respawn Jingle
C - - - - - 0x00331B 00:F30B: 85 F2     STA ram_00F2_music_jingle                                  ; /
bra_F30D:
C - - - - - 0x00331D 00:F30D: CA        DEX                                                        ; \ Loop with Player 1
C - - - - - 0x00331E 00:F30E: 10 D4     BPL bra_F2E4                                               ; /
C - - - - - 0x003320 00:F310: A5 41     LDA ram_0041_p1_lives                                      ; \ If Player 1 has lives
C - - - - - 0x003322 00:F312: 10 04     BPL bra_F318                                               ; / continue
C - - - - - 0x003324 00:F314: A5 42     LDA ram_0042_p2_lives                                      ; \ If Player 1 & 2 have 0 lives
C - - - - - 0x003326 00:F316: 30 4E     BMI bra_F366                                               ; / then game over
bra_F318:
C - - - - - 0x003328 00:F318: A5 3A     LDA ram_003A_demo_flag                                     ; \ If Demo Play
C - - - - - 0x00332A 00:F31A: F0 0B     BEQ bra_F327                                               ; / then skip joypad read
C - - - - - 0x00332C 00:F31C: 20 68 E7  JSR sub_E768_Poll_Joypad_0
C - - - - - 0x00332F 00:F31F: AD 1C 06  LDA ram_061C_controller1_pressed                           ; \
C - - - - - 0x003332 00:F322: 29 30     AND #$30                                                   ; | If START or SELECT is pressed
C - - - - - 0x003334 00:F324: F0 93     BEQ bra_F2B9_Balloon_Fight_Loop                            ; / then loop
bra_F326_RTS:
C - - - - - 0x003336 00:F326: 60        RTS
bra_F327:
C - - - - - 0x003337 00:F327: A2 05     LDX #$05                                                   ; Enemy Check
bra_F329:
C - - - - - 0x003339 00:F329: B5 8A     LDA ram_008A_object_balloons_enemy1,X                      ; \ If Enemy Balloons
C - - - - - 0x00333B 00:F32B: F0 02     BEQ bra_F32F                                               ; | == 0 then ?
C - - - - - 0x00333D 00:F32D: 10 8A     BPL bra_F2B9_Balloon_Fight_Loop                            ; / > 0 then loop
bra_F32F:
C - - - - - 0x00333F 00:F32F: CA        DEX                                                        ; \ Check next enemy
C - - - - - 0x003340 00:F330: 10 F7     BPL bra_F329                                               ; /
C - - - - - 0x003342 00:F332: A5 BB     LDA ram_00BB_water_plonk_animation                         ; \ Loop if water plonk effect
C - - - - - 0x003344 00:F334: 10 83     BPL bra_F2B9_Balloon_Fight_Loop                            ; / is not finished yet
C - - - - - 0x003346 00:F336: A6 40     LDX ram_0040_2p_flag                                       ; Player Check
bra_F338:
C - - - - - 0x003348 00:F338: B4 88     LDY ram_0088_object_balloons_p1,X                          ; \ If Player X has balloons
C - - - - - 0x00334A 00:F33A: 88        DEY                                                        ; | then
C - - - - - 0x00334B 00:F33B: 10 0F     BPL bra_F34C                                               ; /
C - - - - - 0x00334D 00:F33D: B5 41     LDA ram_0041_p1_lives,X                                    ; \ If Player X has no lives
C - - - - - 0x00334F 00:F33F: 30 0B     BMI bra_F34C                                               ; / then skip
C - - - - - 0x003351 00:F341: A9 FF     LDA #$FF                                                   ; \ Set Player X balloons
C - - - - - 0x003353 00:F343: 95 88     STA ram_0088_object_balloons_p1,X                          ; / to none
C - - - - - 0x003355 00:F345: A9 01     LDA #$01                                                   ; \ Set Player X Respawn Delay
C - - - - - 0x003357 00:F347: 95 C3     STA ram_00C3_p1_respawn_delay,X                            ; / to 1 frame
C - - - - - 0x003359 00:F349: 4C B9 F2  JMP loc_F2B9_Balloon_Fight_Loop                            ; Loop
bra_F34C:
C - - - - - 0x00335C 00:F34C: CA        DEX                                                        ; \ Loop player checks until
C - - - - - 0x00335D 00:F34D: 10 E9     BPL bra_F338                                               ; / we can assume Phase is cleared.
C - - - - - 0x00335F 00:F34F: A9 02     LDA #$02                                                   ; \ Play Stage Clear Jingle
C - - - - - 0x003361 00:F351: 85 F2     STA ram_00F2_music_jingle                                  ; /
loc_F353:
C D 3 - - - 0x003363 00:F353: A2 96     LDX #$96                                                   ; \ Wait 150 frames
C - - - - - 0x003365 00:F355: 20 5E F4  JSR sub_F45E_Wait_Y_Ticks                                  ; /
C - - - - - 0x003368 00:F358: A6 3B     LDX ram_003B_current_level_header                          ; \
C - - - - - 0x00336A 00:F35A: E8        INX                                                        ; | Get to next level
C - - - - - 0x00336B 00:F35B: E0 10     CPX #$10                                                   ; | if past level ID #$10
C - - - - - 0x00336D 00:F35D: D0 02     BNE bra_F361                                               ; |
C - - - - - 0x00336F 00:F35F: A2 04     LDX #$04                                                   ; | then loop back to level ID #$04
bra_F361:
C - - - - - 0x003371 00:F361: 86 3B     STX ram_003B_current_level_header                          ; /
C - - - - - 0x003373 00:F363: 4C 13 F2  JMP loc_F213                                               ; Load Next Level
bra_F366:
; Manage Game Over
C - - - - - 0x003376 00:F366: A9 01     LDA #$01                                                   ; \ Play Game Over Jingle
C - - - - - 0x003378 00:F368: 85 F2     STA ram_00F2_music_jingle                                  ; /
loc_F36A:
C D 3 - - - 0x00337A 00:F36A: A9 00     LDA #$00
C - - - - - 0x00337C 00:F36C: 85 17     STA ram_0017_ppuscroll_shadow_trip                         ; Reset PPUSCROLL Shadow
C - - - - - 0x00337E 00:F36E: 85 18     STA ram_0018_ppuctrl_shadow_trip                           ; Reset PPUCTRL Shadow
C - - - - - 0x003380 00:F370: 85 15     STA ram_0015_temp_player_x_trip_rank_score                 ; Set time
C - - - - - 0x003382 00:F372: 20 0B F4  JSR sub_F40B_Upload_Game_Over_Text
bra_F375:
C - - - - - 0x003385 00:F375: 20 65 F4  JSR sub_F465_Clear_F_Flag                                  ; Clear Frame Flag
C - - - - - 0x003388 00:F378: 20 68 E7  JSR sub_E768_Poll_Joypad_0                                 ; \ Press START or SELECT
C - - - - - 0x00338B 00:F37B: 29 30     AND #$30                                                   ; | to come back to Title Screen
C - - - - - 0x00338D 00:F37D: D0 04     BNE bra_F383                                               ; /
C - - - - - 0x00338F 00:F37F: C6 15     DEC ram_0015_temp_player_x_trip_rank_score                 ; \ Wait for 256 frames
C - - - - - 0x003391 00:F381: D0 F2     BNE bra_F375                                               ; / to come back to Title Screen
bra_F383:
C - - - - - 0x003393 00:F383: 4C D4 F1  JMP loc_F1D4                                               ; Back to Title Screen
sub_F386_Initialize_Player_X:
C - - - - - 0x003396 00:F386: B5 41     LDA ram_0041_p1_lives,X                                    ; \ If Player X has negative lives
C - - - - - 0x003398 00:F388: 30 23     BMI bra_F3AD_RTS                                           ; / Then don't do anything
C - - - - - 0x00339A 00:F38A: BD AE F3  LDA tbl_F3AE_Player_X_Spawn,X                              ; \ Set up X coordinate for Player X
C - - - - - 0x00339D 00:F38D: 95 91     STA ram_0091_object_x_pos_int_p1,X                         ; /
C - - - - - 0x00339F 00:F38F: A9 B8     LDA #$B8                                                   ; \ Set up Y coordinate for Player X
C - - - - - 0x0033A1 00:F391: 95 9A     STA ram_009A_object_y_pos_int_p1,X                         ; /
C - - - - - 0x0033A3 00:F393: 95 BD     STA ram_00BD_p1_invincibility_flag,X                       ; Set up Invincibility for Player X
C - - - - - 0x0033A5 00:F395: A9 C8     LDA #$C8                                                   ; \ Set up Invincibility Time
C - - - - - 0x0033A7 00:F397: 95 BF     STA ram_00BF_p1_invincibility_time,X                       ; / for Player X
C - - - - - 0x0033A9 00:F399: A9 5A     LDA #$5A                                                   ; \
C - - - - - 0x0033AB 00:F39B: B4 41     LDY ram_0041_p1_lives,X                                    ; | If Player X has lives
C - - - - - 0x0033AD 00:F39D: 10 02     BPL bra_F3A1                                               ; | Then set respawn delay to #$5A
- - - - - - 0x0033AF 00:F39F: A9 01     LDA #$01                                                   ; | Else set respawn delay to #$01
bra_F3A1:
C - - - - - 0x0033B1 00:F3A1: 95 C3     STA ram_00C3_p1_respawn_delay,X                            ; /
C - - - - - 0x0033B3 00:F3A3: A9 00     LDA #$00
C - - - - - 0x0033B5 00:F3A5: 95 C1     STA ram_00C1_p1_freeze_flag,X                              ; Clear Player X Freeze Flag
C - - - - - 0x0033B7 00:F3A7: 9D 2D 04  STA ram_042D_x_vel_int_p1,X                                ; \ Set up Player X's X Velocity to $00
C - - - - - 0x0033BA 00:F3AA: 9D 24 04  STA ram_0424_x_vel_frac_p1,X                               ; /
bra_F3AD_RTS:
C - - - - - 0x0033BD 00:F3AD: 60        RTS
tbl_F3AE_Player_X_Spawn:
- D 3 - - - 0x0033BE 00:F3AE: 20        .byte $20   ; Player 1
- D 3 - - - 0x0033BF 00:F3AF: D0        .byte $D0   ; Player 2
sub_F3B0_Init_Player_Type:
C D 3 - - - 0x0033C0 00:F3B0: A9 03     LDA #$03                                                   ; \ Set Player X type to 03 (2 Balloons)
C - - - - - 0x0033C2 00:F3B2: 9D 51 04  STA ram_0451_object_type_p1,X                              ; /
C - - - - - 0x0033C5 00:F3B5: A9 02     LDA #$02                                                   ; \ Set Player X Balloons to 02
C - - - - - 0x0033C7 00:F3B7: 95 88     STA ram_0088_object_balloons_p1,X                          ; /
C - - - - - 0x0033C9 00:F3B9: 60        RTS
tbl_F3BA:
- D 3 - - - 0x0033CA 00:F3BA: 58        .byte $58   ; 
- D 3 - - - 0x0033CB 00:F3BB: 50        .byte $50   ; 
- D 3 - - - 0x0033CC 00:F3BC: 58        .byte $58   ; 
- D 3 - - - 0x0033CD 00:F3BD: 50        .byte $50   ; 
- D 3 - - - 0x0033CE 00:F3BE: 50        .byte $50   ; 
- D 3 - - - 0x0033CF 00:F3BF: 40        .byte $40   ; 
- - - - - - 0x0033D0 00:F3C0: 38        .byte $38   ; 
- - - - - - 0x0033D1 00:F3C1: 30        .byte $30   ; 
- - - - - - 0x0033D2 00:F3C2: 28        .byte $28   ; 
tbl_F3C3:
- D 3 - - - 0x0033D3 00:F3C3: 04        .byte $04   ; 
- D 3 - - - 0x0033D4 00:F3C4: 04        .byte $04   ; 
- D 3 - - - 0x0033D5 00:F3C5: 03        .byte $03   ; 
- D 3 - - - 0x0033D6 00:F3C6: 03        .byte $03   ; 
- D 3 - - - 0x0033D7 00:F3C7: 02        .byte $02   ; 
- D 3 - - - 0x0033D8 00:F3C8: 02        .byte $02   ; 
- - - - - - 0x0033D9 00:F3C9: 02        .byte $02   ; 
- - - - - - 0x0033DA 00:F3CA: 02        .byte $02   ; 
- - - - - - 0x0033DB 00:F3CB: 02        .byte $02   ; 
sub_F3CC_Phase_Display:
C - - - - - 0x0033DC 00:F3CC: A5 3D     LDA ram_003D_phase_number_display_time                     ; \ Toggle between "PHASE-??"
C - - - - - 0x0033DE 00:F3CE: 29 20     AND #$20                                                   ; | and empty
C - - - - - 0x0033E0 00:F3D0: F0 1C     BEQ bra_F3EE                                               ; / every #$20 frames?
C - - - - - 0x0033E2 00:F3D2: A2 0A     LDX #$0A                                                   ; \
bra_F3D4:
C - - - - - 0x0033E4 00:F3D4: BD F5 F3  LDA tbl_F3F5_PHASE_PPU,X                                   ; | Copy "PHASE-  " PPU Block
C - - - - - 0x0033E7 00:F3D7: 95 57     STA ram_0057_ppu_buffer_upload_data,X                      ; |
C - - - - - 0x0033E9 00:F3D9: CA        DEX                                                        ; |
C - - - - - 0x0033EA 00:F3DA: 10 F8     BPL bra_F3D4                                               ; /
C - - - - - 0x0033EC 00:F3DC: A0 0A     LDY #$0A                                                   ; \
C - - - - - 0x0033EE 00:F3DE: A5 3C     LDA ram_003C_current_phase                                 ; | Add 1st digit of
C - - - - - 0x0033F0 00:F3E0: 85 43     STA ram_0043_div_mod_result                                ; | Phase Number
C - - - - - 0x0033F2 00:F3E2: 20 7C D7  JSR sub_D77C_Divide                                        ; | (Divide by 10)
C - - - - - 0x0033F5 00:F3E5: 85 60     STA ram_0060                                               ; /
C - - - - - 0x0033F7 00:F3E7: A5 43     LDA ram_0043_div_mod_result                                ; \ Add 2nd digit of
C - - - - - 0x0033F9 00:F3E9: 85 61     STA ram_0061                                               ; / Phase Number
C - - - - - 0x0033FB 00:F3EB: 4C 2D C1  JMP loc_C12D_Copy_PPU_Temp_Block
bra_F3EE:
C - - - - - 0x0033FE 00:F3EE: A9 00     LDA #< tbl_F400_EMPTY_PPU                                  ; \
C - - - - - 0x003400 00:F3F0: A0 F4     LDY #> tbl_F400_EMPTY_PPU                                  ; | Copy Empty PPU Block
C - - - - - 0x003402 00:F3F2: 4C 31 C1  JMP sub_C131_Copy_PPU_Block                                ; /
tbl_F3F5_PHASE_PPU:
- D 3 - - - 0x003405 00:F3F5: 20        .dbyt $206C                                                ; PPUADDR = $206C
- D 3 - - - 0x003407 00:F3F7: 08        .byte $08                                                  ; 8 bytes
- D 3 - - - 0x003408 00:F3F8: 19        .byte $19, $11, $0A, $1C, $0E, $25, $00, $00               ; "PHASE-  "
tbl_F400_EMPTY_PPU:
- D 3 - I - 0x003410 00:F400: 20        .dbyt $206C                                                ; PPUADDR = $206C
- D 3 - I - 0x003412 00:F402: 08        .byte $08                                                  ; 8 bytes
- D 3 - I - 0x003413 00:F403: 24        .byte $24, $24, $24, $24, $24, $24, $24, $24               ; "        "
sub_F40B_Upload_Game_Over_Text:
C - - - - - 0x00341B 00:F40B: 20 65 F4  JSR sub_F465_Clear_F_Flag                                  ; Clear Frame Flag
C - - - - - 0x00341E 00:F40E: A2 01     LDX #$01                                                   ; \
bra_F410:
C - - - - - 0x003420 00:F410: BD 3B F4  LDA tbl_F43B,X                                             ; | Prepare Game Over
C - - - - - 0x003423 00:F413: BC 3D F4  LDY tbl_F43B + 2,X                                         ; | PPU blocks
C - - - - - 0x003426 00:F416: 20 31 C1  JSR sub_C131_Copy_PPU_Block                                ; | to upload
C - - - - - 0x003429 00:F419: CA        DEX                                                        ; |
C - - - - - 0x00342A 00:F41A: 10 F4     BPL bra_F410                                               ; /
C - - - - - 0x00342C 00:F41C: A2 0F     LDX #$0F                                                   ; \
bra_F41E:
C - - - - - 0x00342E 00:F41E: A9 24     LDA #$24                                                   ; | Prepare 16 empty tiles
C - - - - - 0x003430 00:F420: 95 5A     STA ram_005A,X                                             ; | to upload
C - - - - - 0x003432 00:F422: CA        DEX                                                        ; |
C - - - - - 0x003433 00:F423: 10 F9     BPL bra_F41E                                               ; /
C - - - - - 0x003435 00:F425: A9 10     LDA #$10                                                   ; \ Size: 16 bytes
C - - - - - 0x003437 00:F427: 85 59     STA ram_0059                                               ; /
C - - - - - 0x003439 00:F429: A9 21     LDA #$21                                                   ; \ PPUADDR = $21xx
C - - - - - 0x00343B 00:F42B: 85 57     STA ram_0057_ppu_buffer_upload_data                        ; /
C - - - - - 0x00343D 00:F42D: A2 02     LDX #$02                                                   ; \
bra_F42F:
C - - - - - 0x00343F 00:F42F: BD 3F F4  LDA tbl_F43F,X                                             ; | Prepare uploading
C - - - - - 0x003442 00:F432: 85 58     STA ram_0058                                               ; | empty tiles to nametable
C - - - - - 0x003444 00:F434: 20 2D C1  JSR sub_C12D_Copy_PPU_Temp_Block                           ; | ($2188, $21A8, $21E8)
C - - - - - 0x003447 00:F437: CA        DEX                                                        ; | to PPU Buffer
C - - - - - 0x003448 00:F438: 10 F5     BPL bra_F42F                                               ; /
C - - - - - 0x00344A 00:F43A: 60        RTS
tbl_F43B:                                                                                          ; Pointers to PPU Blocks
- D 3 - - - 0x00344B 00:F43B: 42        .byte < tbl_F442   ; 
- D 3 - - - 0x00344C 00:F43C: 55        .byte < tbl_F455   ; 
- D 3 - - - 0x00344D 00:F43D: F4        .byte > tbl_F442   ; 
- D 3 - - - 0x00344E 00:F43E: F4        .byte > tbl_F455   ; 
tbl_F43F:                                                                                          ; Empty tiles lower PPUADDR
- D 3 - - - 0x00344F 00:F43F: 88        .byte < $2188   ; PPUADDR = $2188
- D 3 - - - 0x003450 00:F440: A8        .byte < $21A8   ; PPUADDR = $21A8
- D 3 - - - 0x003451 00:F441: E8        .byte < $21E8   ; PPUADDR = $21E8
tbl_F442:                                                                                          ; GAME OVER text
- D 3 - I - 0x003452 00:F442: 21        .dbyt $21C8     ; PPUADDR = $21C8
- D 3 - I - 0x003454 00:F444: 10        .byte $10   ; 16 bytes
- D 3 - I - 0x003455 00:F445: 24        .byte $24, $24, $24, $10, $0A, $16, $0E, $24, $24, $18, $1F, $0E, $1B, $24, $24, $24   ; "   GAME  OVER   "
tbl_F455:                                                                                          ; GAME OVER tile attributes
- D 3 - I - 0x003465 00:F455: 23        .dbyt $23DA     ; PPUADDR = $23DA
- D 3 - I - 0x003467 00:F457: 04        .byte $04   ; 4 bytes
- D 3 - I - 0x003468 00:F458: AA        .byte $AA, $AA, $AA, $AA   ; $23DA-$23DD tile attribute values
sub_F45C_Wait_20_Ticks:
C - - - - - 0x00346C 00:F45C: A2 14     LDX #$14
bra_F45E_Wait_Y_Ticks:
sub_F45E_Wait_Y_Ticks:
C - - - - - 0x00346E 00:F45E: 20 65 F4  JSR sub_F465_Clear_F_Flag                                  ; Clear Frame Flag
C - - - - - 0x003471 00:F461: CA        DEX
C - - - - - 0x003472 00:F462: D0 FA     BNE bra_F45E_Wait_Y_Ticks
C - - - - - 0x003474 00:F464: 60        RTS
sub_F465_Clear_F_Flag:
; Clear Frame Flag.
C - - - - - 0x003475 00:F465: A9 00     LDA #$00
C - - - - - 0x003477 00:F467: 85 02     STA ram_0002_video_processed_flag
bra_F469_Wait_Next_F:
sub_F469_Wait_Next_F:
; Wait next frame.
C - - - - - 0x003479 00:F469: A5 02     LDA ram_0002_video_processed_flag
C - - - - - 0x00347B 00:F46B: F0 FC     BEQ bra_F469_Wait_Next_F
C - - - - - 0x00347D 00:F46D: C6 02     DEC ram_0002_video_processed_flag
bra_F46F_RTS:
C - - - - - 0x00347F 00:F46F: 60        RTS
sub_F470_Pause:
C - - - - - 0x003480 00:F470: 20 69 F4  JSR sub_F469_Wait_Next_F
C - - - - - 0x003483 00:F473: A5 3A     LDA ram_003A_demo_flag                                     ; \ If Demo Flag Set
C - - - - - 0x003485 00:F475: D0 F8     BNE bra_F46F_RTS                                           ; / then don't do anything
C - - - - - 0x003487 00:F477: 20 68 E7  JSR sub_E768_Poll_Joypad_0                                 ; \
C - - - - - 0x00348A 00:F47A: 29 10     AND #$10                                                   ; | If START is not pressed
C - - - - - 0x00348C 00:F47C: F0 F1     BEQ bra_F46F_RTS                                           ; / then don't do anything
C - - - - - 0x00348E 00:F47E: A9 04     LDA #$04                                                   ; \ Play Pause Jingle
C - - - - - 0x003490 00:F480: 85 F2     STA ram_00F2_music_jingle                                  ; /
C - - - - - 0x003492 00:F482: A5 01     LDA ram_0001_ppumask_shadow                                ; \
C - - - - - 0x003494 00:F484: 29 EF     AND #$EF                                                   ; | Hide Sprites
C - - - - - 0x003496 00:F486: 8D 01 20  STA $2001                                                  ; /
bra_F489:
C - - - - - 0x003499 00:F489: 20 65 F4  JSR sub_F465_Clear_F_Flag                                  ; \ Clear Frame Flag
C - - - - - 0x00349C 00:F48C: 20 68 E7  JSR sub_E768_Poll_Joypad_0                                 ; |
C - - - - - 0x00349F 00:F48F: 29 10     AND #$10                                                   ; | If START is not pressed
C - - - - - 0x0034A1 00:F491: F0 F6     BEQ bra_F489                                               ; / then loop
C - - - - - 0x0034A3 00:F493: A5 01     LDA ram_0001_ppumask_shadow                                ; \
C - - - - - 0x0034A5 00:F495: 8D 01 20  STA $2001                                                  ; / Show Sprites
C - - - - - 0x0034A8 00:F498: A0 04     LDY #$04                                                   ; \
C - - - - - 0x0034AA 00:F49A: A5 C8     LDA ram_00C8_phase_type                                    ; | Play Pause Jingle if
C - - - - - 0x0034AC 00:F49C: 05 16     ORA ram_0016_game_mode                                     ; | it's a Normal Phase in Balloon Fight Game Mode
C - - - - - 0x0034AE 00:F49E: F0 02     BEQ bra_F4A2                                               ; | Else play Balloon Trip / Bonus Phase music
C - - - - - 0x0034B0 00:F4A0: A0 20     LDY #$20                                                   ; |
bra_F4A2:
C - - - - - 0x0034B2 00:F4A2: 84 F2     STY ram_00F2_music_jingle                                  ; /
C - - - - - 0x0034B4 00:F4A4: 60        RTS
sub_F4A5_Init_Fish:
C - - - - - 0x0034B5 00:F4A5: A9 01     LDA #$01
C - - - - - 0x0034B7 00:F4A7: 8D 8E 04  STA ram_048E_fish_unused                                   ; \ Set Unused Variables?
C - - - - - 0x0034BA 00:F4AA: 8D 8F 04  STA ram_048F_fish_unused                                   ; /
C - - - - - 0x0034BD 00:F4AD: A9 FF     LDA #$FF                                                   ; \ Reset Water Plonk Animation
C - - - - - 0x0034BF 00:F4AF: 85 BB     STA ram_00BB_water_plonk_animation                         ; /
C - - - - - 0x0034C1 00:F4B1: 85 87     STA ram_0087_object_status_fish                            ; Fish Status = #$FF
C - - - - - 0x0034C3 00:F4B3: 8D 8C 04  STA ram_048C_fish_target_eaten_flag                        ; Fish Target Eaten Flag = #$FF
C - - - - - 0x0034C6 00:F4B6: A2 01     LDX #$01
C - - - - - 0x0034C8 00:F4B8: 8E 59 04  STX ram_0459_object_type_fish                              ; Fish Type = #$01
C - - - - - 0x0034CB 00:F4BB: 86 90     STX ram_0090_object_balloons_fish                          ; Fish Balloons = #$01
C - - - - - 0x0034CD 00:F4BD: E8        INX                                                        ; \ Update Status Bar
C - - - - - 0x0034CE 00:F4BE: 86 46     STX ram_0046_status_bar_update_flag                        ; /
C - - - - - 0x0034D0 00:F4C0: A9 40     LDA #$40                                                   ; \ Set Fish X position
C - - - - - 0x0034D2 00:F4C2: 85 99     STA ram_0099_object_x_pos_int_fish                         ; / to #$40
C - - - - - 0x0034D4 00:F4C4: 60        RTS

; Empty space/unused bytes.
; bzk garbage
- - - - - - 0x0034D5 00:F4C5: FF        .byte $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF   ; 
- - - - - - 0x0034E0 00:F4D0: FF        .byte $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF   ; 
- - - - - - 0x0034F0 00:F4E0: FF        .byte $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF   ; 
- - - - - - 0x003500 00:F4F0: FF        .byte $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF   ; 

bra_F500:
C - - - - - 0x003510 00:F500: 20 8F F7  JSR sub_F78F
bra_F503_RTS:
C - - - - - 0x003513 00:F503: 60        RTS
loc_F504:
C D 3 - - - 0x003514 00:F504: A9 00     LDA #$00
C - - - - - 0x003516 00:F506: AA        TAX
C - - - - - 0x003517 00:F507: 85 FD     STA ram_00FD
C - - - - - 0x003519 00:F509: F0 10     BEQ bra_F51B
bra_F50B:
C - - - - - 0x00351B 00:F50B: 8A        TXA
C - - - - - 0x00351C 00:F50C: 4A        LSR
C - - - - - 0x00351D 00:F50D: AA        TAX
bra_F50E:
loc_F50E:
C D 3 - - - 0x00351E 00:F50E: E8        INX
C - - - - - 0x00351F 00:F50F: 8A        TXA
C - - - - - 0x003520 00:F510: C9 04     CMP #$04
C - - - - - 0x003522 00:F512: F0 EF     BEQ bra_F503_RTS
C - - - - - 0x003524 00:F514: A5 FD     LDA ram_00FD
C - - - - - 0x003526 00:F516: 18        CLC
C - - - - - 0x003527 00:F517: 69 04     ADC #$04
C - - - - - 0x003529 00:F519: 85 FD     STA ram_00FD
bra_F51B:
C - - - - - 0x00352B 00:F51B: 8A        TXA
C - - - - - 0x00352C 00:F51C: 0A        ASL
C - - - - - 0x00352D 00:F51D: AA        TAX
C - - - - - 0x00352E 00:F51E: B5 E0     LDA ram_00E0_track_pointer,X
C - - - - - 0x003530 00:F520: 85 FE     STA ram_00FE
C - - - - - 0x003532 00:F522: B5 E1     LDA ram_00E1,X
C - - - - - 0x003534 00:F524: 85 FF     STA ram_00FF
C - - - - - 0x003536 00:F526: B5 E1     LDA ram_00E1,X
C - - - - - 0x003538 00:F528: F0 E1     BEQ bra_F50B
C - - - - - 0x00353A 00:F52A: 8A        TXA
C - - - - - 0x00353B 00:F52B: 4A        LSR
C - - - - - 0x00353C 00:F52C: AA        TAX
C - - - - - 0x00353D 00:F52D: D6 D0     DEC ram_00D0_sound,X
C - - - - - 0x00353F 00:F52F: D0 DD     BNE bra_F50E
loc_F531:
C D 3 - - - 0x003541 00:F531: B4 E8     LDY ram_00E8,X
C - - - - - 0x003543 00:F533: F6 E8     INC ram_00E8,X
C - - - - - 0x003545 00:F535: B1 FE     LDA (ram_00FE),Y
C - - - - - 0x003547 00:F537: F0 C7     BEQ bra_F500
C - - - - - 0x003549 00:F539: A8        TAY
C - - - - - 0x00354A 00:F53A: C9 FF     CMP #$FF
C - - - - - 0x00354C 00:F53C: F0 09     BEQ bra_F547
C - - - - - 0x00354E 00:F53E: 29 C0     AND #$C0
C - - - - - 0x003550 00:F540: C9 C0     CMP #$C0
C - - - - - 0x003552 00:F542: F0 0F     BEQ bra_F553
C - - - - - 0x003554 00:F544: 4C 61 F5  JMP loc_F561
bra_F547:
C - - - - - 0x003557 00:F547: B5 D8     LDA ram_00D8,X
C - - - - - 0x003559 00:F549: F0 13     BEQ bra_F55E
C - - - - - 0x00355B 00:F54B: D6 D8     DEC ram_00D8,X
C - - - - - 0x00355D 00:F54D: B5 EC     LDA ram_00EC,X
C - - - - - 0x00355F 00:F54F: 95 E8     STA ram_00E8,X
C - - - - - 0x003561 00:F551: D0 0B     BNE bra_F55E
bra_F553:
C - - - - - 0x003563 00:F553: 98        TYA
C - - - - - 0x003564 00:F554: 29 3F     AND #$3F
C - - - - - 0x003566 00:F556: 95 D8     STA ram_00D8,X
C - - - - - 0x003568 00:F558: D6 D8     DEC ram_00D8,X
C - - - - - 0x00356A 00:F55A: B5 E8     LDA ram_00E8,X
C - - - - - 0x00356C 00:F55C: 95 EC     STA ram_00EC,X
bra_F55E:
C - - - - - 0x00356E 00:F55E: 4C 31 F5  JMP loc_F531
loc_F561:
C D 3 - - - 0x003571 00:F561: 98        TYA
C - - - - - 0x003572 00:F562: 10 17     BPL bra_F57B
C - - - - - 0x003574 00:F564: 29 0F     AND #$0F
C - - - - - 0x003576 00:F566: 18        CLC
C - - - - - 0x003577 00:F567: 65 DF     ADC ram_00DF
C - - - - - 0x003579 00:F569: A8        TAY
C - - - - - 0x00357A 00:F56A: B9 60 F6  LDA tbl_F660,Y
C - - - - - 0x00357D 00:F56D: 95 D4     STA ram_00D4,X
C - - - - - 0x00357F 00:F56F: A8        TAY
C - - - - - 0x003580 00:F570: 8A        TXA
C - - - - - 0x003581 00:F571: C9 02     CMP #$02
C - - - - - 0x003583 00:F573: F0 4F     BEQ bra_F5C4
loc_F575:
C D 3 - - - 0x003585 00:F575: B4 E8     LDY ram_00E8,X
C - - - - - 0x003587 00:F577: F6 E8     INC ram_00E8,X
C - - - - - 0x003589 00:F579: B1 FE     LDA (ram_00FE),Y
bra_F57B:
C - - - - - 0x00358B 00:F57B: A8        TAY
C - - - - - 0x00358C 00:F57C: 8A        TXA
C - - - - - 0x00358D 00:F57D: C9 03     CMP #$03
C - - - - - 0x00358F 00:F57F: F0 60     BEQ bra_F5E1
C - - - - - 0x003591 00:F581: 48        PHA
C - - - - - 0x003592 00:F582: AA        TAX
C - - - - - 0x003593 00:F583: C9 01     CMP #$01
C - - - - - 0x003595 00:F585: F0 32     BEQ bra_F5B9
bra_F587:
C - - - - - 0x003597 00:F587: A6 FD     LDX ram_00FD
C - - - - - 0x003599 00:F589: B9 01 F6  LDA tbl_F600_Pulse_Channel_Frequency + 1,Y                 ; Pulse Channel Frequency - Timer Low
C - - - - - 0x00359C 00:F58C: F0 0B     BEQ bra_F599
C - - - - - 0x00359E 00:F58E: 9D 02 40  STA $4002,X
C - - - - - 0x0035A1 00:F591: B9 00 F6  LDA tbl_F600_Pulse_Channel_Frequency,Y                     ; Pulse Channel Frequency - Timer High (d2-d0)
C - - - - - 0x0035A4 00:F594: 09 08     ORA #$08
C - - - - - 0x0035A6 00:F596: 9D 03 40  STA $4003,X
bra_F599:
C - - - - - 0x0035A9 00:F599: A8        TAY
C - - - - - 0x0035AA 00:F59A: 68        PLA
C - - - - - 0x0035AB 00:F59B: AA        TAX
C - - - - - 0x0035AC 00:F59C: 98        TYA
C - - - - - 0x0035AD 00:F59D: D0 0B     BNE bra_F5AA
C - - - - - 0x0035AF 00:F59F: A0 00     LDY #$00
C - - - - - 0x0035B1 00:F5A1: 8A        TXA
C - - - - - 0x0035B2 00:F5A2: C9 02     CMP #$02
C - - - - - 0x0035B4 00:F5A4: F0 06     BEQ bra_F5AC
C - - - - - 0x0035B6 00:F5A6: A0 10     LDY #$10
C - - - - - 0x0035B8 00:F5A8: D0 02     BNE bra_F5AC
bra_F5AA:
C - - - - - 0x0035BA 00:F5AA: B4 DC     LDY ram_00DC,X
bra_F5AC:
C - - - - - 0x0035BC 00:F5AC: 98        TYA
C - - - - - 0x0035BD 00:F5AD: A4 FD     LDY ram_00FD
C - - - - - 0x0035BF 00:F5AF: 99 00 40  STA $4000,Y
loc_F5B2:
C D 3 - - - 0x0035C2 00:F5B2: B5 D4     LDA ram_00D4,X
C - - - - - 0x0035C4 00:F5B4: 95 D0     STA ram_00D0_sound,X
C - - - - - 0x0035C6 00:F5B6: 4C 0E F5  JMP loc_F50E
bra_F5B9:
C - - - - - 0x0035C9 00:F5B9: A5 F5     LDA ram_00F5
C - - - - - 0x0035CB 00:F5BB: 29 02     AND #$02
C - - - - - 0x0035CD 00:F5BD: F0 C8     BEQ bra_F587
C - - - - - 0x0035CF 00:F5BF: 68        PLA
C - - - - - 0x0035D0 00:F5C0: AA        TAX
C - - - - - 0x0035D1 00:F5C1: 4C B2 F5  JMP loc_F5B2
bra_F5C4:
C - - - - - 0x0035D4 00:F5C4: 98        TYA
C - - - - - 0x0035D5 00:F5C5: AC F0 07  LDY ram_07F0_audio_related
C - - - - - 0x0035D8 00:F5C8: F0 04     BEQ bra_F5CE
C - - - - - 0x0035DA 00:F5CA: A9 FF     LDA #$FF
C - - - - - 0x0035DC 00:F5CC: D0 0B     BNE bra_F5D9
bra_F5CE:
C - - - - - 0x0035DE 00:F5CE: 18        CLC
C - - - - - 0x0035DF 00:F5CF: 69 FE     ADC #$FE
C - - - - - 0x0035E1 00:F5D1: 0A        ASL
C - - - - - 0x0035E2 00:F5D2: 0A        ASL
C - - - - - 0x0035E3 00:F5D3: C9 3C     CMP #$3C
C - - - - - 0x0035E5 00:F5D5: 90 02     BCC bra_F5D9
C - - - - - 0x0035E7 00:F5D7: A9 3C     LDA #$3C
bra_F5D9:
C - - - - - 0x0035E9 00:F5D9: 8D 08 40  STA $4008
C - - - - - 0x0035EC 00:F5DC: 85 DE     STA ram_00DE
C - - - - - 0x0035EE 00:F5DE: 4C 75 F5  JMP loc_F575
bra_F5E1:
C - - - - - 0x0035F1 00:F5E1: A5 F4     LDA ram_00F4
C - - - - - 0x0035F3 00:F5E3: C9 02     CMP #$02
C - - - - - 0x0035F5 00:F5E5: F0 12     BEQ bra_F5F9
C - - - - - 0x0035F7 00:F5E7: B9 00 F7  LDA tbl_F700,Y
C - - - - - 0x0035FA 00:F5EA: 8D 0C 40  STA $400C
C - - - - - 0x0035FD 00:F5ED: B9 01 F7  LDA tbl_F700 + 1,Y
C - - - - - 0x003600 00:F5F0: 8D 0E 40  STA $400E
C - - - - - 0x003603 00:F5F3: B9 02 F7  LDA tbl_F700 + 2,Y
C - - - - - 0x003606 00:F5F6: 8D 0F 40  STA $400F
bra_F5F9:
C - - - - - 0x003609 00:F5F9: 4C B2 F5  JMP loc_F5B2
tbl_F5FC:
- D 3 - I - 0x00360C 00:F5FC: 16        .byte $16   ; 
- D 3 - I - 0x00360D 00:F5FD: FF        .byte $FF   ; 
- D 3 - I - 0x00360E 00:F5FE: 10        .byte $10   ; 
- D 3 - I - 0x00360F 00:F5FF: C5        .byte $C5   ; 
tbl_F600_Pulse_Channel_Frequency:
- - - - - - 0x003610 00:F600: 07        .byte $07, $F0   ; 00: A-1
- - - - - - 0x003612 00:F602: 00        .byte $00, $00   ; 02: Rest
- D 3 - - - 0x003614 00:F604: 00        .byte $00, $D4   ; 04: C-5
- D 3 - - - 0x003616 00:F606: 00        .byte $00, $C8   ; 06: C#5
- D 3 - - - 0x003618 00:F608: 00        .byte $00, $BD   ; 08: D-5
- D 3 - - - 0x00361A 00:F60A: 00        .byte $00, $B2   ; 0A: Eb5
- D 3 - - - 0x00361C 00:F60C: 00        .byte $00, $A8   ; 0C: E-5
- D 3 - - - 0x00361E 00:F60E: 00        .byte $00, $9F   ; 0E: F-5
- D 3 - - - 0x003620 00:F610: 00        .byte $00, $96   ; 10: F#5
- D 3 - - - 0x003622 00:F612: 00        .byte $00, $8D   ; 12: G-5
- D 3 - - - 0x003624 00:F614: 00        .byte $00, $85   ; 14: Ab5
- D 3 - - - 0x003626 00:F616: 00        .byte $00, $7E   ; 16: A-5
- D 3 - - - 0x003628 00:F618: 00        .byte $00, $76   ; 18: Bb5
- D 3 - - - 0x00362A 00:F61A: 00        .byte $00, $70   ; 1A: B-5
- D 3 - - - 0x00362C 00:F61C: 01        .byte $01, $AB   ; 1C: C-4
- D 3 - - - 0x00362E 00:F61E: 01        .byte $01, $93   ; 1E: C#4
- D 3 - - - 0x003630 00:F620: 01        .byte $01, $7C   ; 20: D-4
- D 3 - - - 0x003632 00:F622: 01        .byte $01, $67   ; 22: Eb4
- D 3 - - - 0x003634 00:F624: 01        .byte $01, $52   ; 24: E-4
- D 3 - - - 0x003636 00:F626: 01        .byte $01, $3F   ; 26: F-4
- D 3 - - - 0x003638 00:F628: 01        .byte $01, $2D   ; 28: F#4
- D 3 - - - 0x00363A 00:F62A: 01        .byte $01, $1C   ; 2A: G-4
- D 3 - - - 0x00363C 00:F62C: 01        .byte $01, $0C   ; 2C: Ab4
- D 3 - - - 0x00363E 00:F62E: 00        .byte $00, $FD   ; 2E: A-4
- D 3 - - - 0x003640 00:F630: 00        .byte $00, $EE   ; 30: Bb4
- D 3 - - - 0x003642 00:F632: 00        .byte $00, $E1   ; 32: B-4
- D 3 - - - 0x003644 00:F634: 03        .byte $03, $57   ; 34: C-3
- D 3 - - - 0x003646 00:F636: 03        .byte $03, $27   ; 36: C#3
- D 3 - - - 0x003648 00:F638: 02        .byte $02, $F9   ; 38: D-3
- D 3 - - - 0x00364A 00:F63A: 02        .byte $02, $CF   ; 3A: Eb3
- D 3 - - - 0x00364C 00:F63C: 02        .byte $02, $A6   ; 3C: E-3
- D 3 - - - 0x00364E 00:F63E: 02        .byte $02, $80   ; 3E: F-3
- D 3 - - - 0x003650 00:F640: 02        .byte $02, $5C   ; 40: F#3
- D 3 - - - 0x003652 00:F642: 02        .byte $02, $3A   ; 42: G-3
- D 3 - - - 0x003654 00:F644: 02        .byte $02, $1A   ; 44: Ab3
- D 3 - - - 0x003656 00:F646: 01        .byte $01, $FC   ; 46: A-3
- D 3 - - - 0x003658 00:F648: 01        .byte $01, $DF   ; 48: Bb3
- D 3 - - - 0x00365A 00:F64A: 01        .byte $01, $C4   ; 4A: B-3
- D 3 - - - 0x00365C 00:F64C: 03        .byte $03, $F8   ; 4C: A-2
- D 3 - - - 0x00365E 00:F64E: 00        .byte $00, $69   ; 4E: C-6
- D 3 - - - 0x003660 00:F650: 00        .byte $00, $63   ; 50: C#6
- D 3 - - - 0x003662 00:F652: 00        .byte $00, $5E   ; 52: D-6
- D 3 - - - 0x003664 00:F654: 00        .byte $00, $58   ; 54: Eb6
- D 3 - - - 0x003666 00:F656: 00        .byte $00, $53   ; 56: E-6
- D 3 - - - 0x003668 00:F658: 00        .byte $00, $4F   ; 58: F-6
- D 3 - - - 0x00366A 00:F65A: 00        .byte $00, $4A   ; 5A: F#6
- - - - - - 0x00366C 00:F65C: 00        .byte $00, $46   ; 5C: G-6
- D 3 - - - 0x00366E 00:F65E: 00        .byte $00, $42   ; 5E: Ab6
tbl_F660:
- D 3 - - - 0x003670 00:F660: 03        .byte $03   ; 
- D 3 - - - 0x003671 00:F661: 06        .byte $06   ; 
- D 3 - - - 0x003672 00:F662: 0C        .byte $0C   ; 
- - - - - - 0x003673 00:F663: 18        .byte $18   ; 
- - - - - - 0x003674 00:F664: 30        .byte $30   ; 
- D 3 - - - 0x003675 00:F665: 12        .byte $12   ; 
- - - - - - 0x003676 00:F666: 24        .byte $24   ; 
- - - - - - 0x003677 00:F667: 09        .byte $09   ; 
- - - - - - 0x003678 00:F668: 08        .byte $08   ; 
- - - - - - 0x003679 00:F669: 04        .byte $04   ; 
- - - - - - 0x00367A 00:F66A: 07        .byte $07   ; 
- D 3 - - - 0x00367B 00:F66B: 01        .byte $01   ; 
- D 3 - - - 0x00367C 00:F66C: 04        .byte $04   ; 
- D 3 - - - 0x00367D 00:F66D: 08        .byte $08   ; 
- D 3 - - - 0x00367E 00:F66E: 10        .byte $10   ; 
- D 3 - - - 0x00367F 00:F66F: 20        .byte $20   ; 
- - - - - - 0x003680 00:F670: 40        .byte $40   ; 
- - - - - - 0x003681 00:F671: 18        .byte $18   ; 
- - - - - - 0x003682 00:F672: 30        .byte $30   ; 
- - - - - - 0x003683 00:F673: 0C        .byte $0C   ; 
- D 3 - - - 0x003684 00:F674: 01        .byte $01   ; 
- D 3 - - - 0x003685 00:F675: 06        .byte $06   ; 
- D 3 - - - 0x003686 00:F676: 0C        .byte $0C   ; 
- D 3 - - - 0x003687 00:F677: 18        .byte $18   ; 
- D 3 - - - 0x003688 00:F678: 30        .byte $30   ; 
- D 3 - - - 0x003689 00:F679: 60        .byte $60   ; 
- D 3 - - - 0x00368A 00:F67A: 24        .byte $24   ; 
- D 3 - - - 0x00368B 00:F67B: 48        .byte $48   ; 
- - - - - - 0x00368C 00:F67C: 12        .byte $12   ; 
- D 3 - - - 0x00368D 00:F67D: 10        .byte $10   ; 
- D 3 - - - 0x00368E 00:F67E: 08        .byte $08   ; 
- D 3 - - - 0x00368F 00:F67F: 0E        .byte $0E   ; 
- D 3 - - - 0x003690 00:F680: 02        .byte $02   ; 
- D 3 - - - 0x003691 00:F681: 03        .byte $03   ; 
- - - - - - 0x003692 00:F682: 04        .byte $04   ; 
sub_F683:
C - - - - - 0x003693 00:F683: A9 00     LDA #$00
C - - - - - 0x003695 00:F685: F0 0A     BEQ bra_F691
sub_F687:
C - - - - - 0x003697 00:F687: A9 08     LDA #$08
C - - - - - 0x003699 00:F689: D0 06     BNE bra_F691
sub_F68B:
C - - - - - 0x00369B 00:F68B: A9 0C     LDA #$0C
C - - - - - 0x00369D 00:F68D: D0 02     BNE bra_F691
sub_F68F:
C - - - - - 0x00369F 00:F68F: A9 04     LDA #$04
bra_F691:
C - - - - - 0x0036A1 00:F691: 85 F9     STA ram_00F9
C - - - - - 0x0036A3 00:F693: A9 40     LDA #$40
C - - - - - 0x0036A5 00:F695: 85 FA     STA ram_00FA
C - - - - - 0x0036A7 00:F697: 86 FB     STX ram_00FB
C - - - - - 0x0036A9 00:F699: 84 FC     STY ram_00FC
C - - - - - 0x0036AB 00:F69B: A0 00     LDY #$00
bra_F69D:
C - - - - - 0x0036AD 00:F69D: B1 FB     LDA (ram_00FB),Y
C - - - - - 0x0036AF 00:F69F: 91 F9     STA (ram_00F9),Y
C - - - - - 0x0036B1 00:F6A1: C8        INY
C - - - - - 0x0036B2 00:F6A2: 98        TYA
C - - - - - 0x0036B3 00:F6A3: C9 04     CMP #$04
C - - - - - 0x0036B5 00:F6A5: D0 F6     BNE bra_F69D
C - - - - - 0x0036B7 00:F6A7: 60        RTS
sub_F6A8_Load_Snd_Seq:
C - - - - - 0x0036B8 00:F6A8: AA        TAX                                                        ; \
C - - - - - 0x0036B9 00:F6A9: 20 98 F7  JSR sub_F798                                               ; | Initialize Sound Channels
C - - - - - 0x0036BC 00:F6AC: 86 F6     STX ram_00F6_current_music                                 ; / and Sound Variables
C - - - - - 0x0036BE 00:F6AE: AD F5 07  LDA ram_07F5_f0_sfx_flag_trip                              ; \
C - - - - - 0x0036C1 00:F6B1: F0 0B     BEQ bra_F6BE                                               ; | Check [$07F5] == $00 or $02
C - - - - - 0x0036C3 00:F6B3: C9 02     CMP #$02                                                   ; |
C - - - - - 0x0036C5 00:F6B5: D0 07     BNE bra_F6BE                                               ; /
C - - - - - 0x0036C7 00:F6B7: 85 F0     STA ram_00F0_sfx_1                                         ; [$00F0] = [$07F5] (!= 00 or 02)
C - - - - - 0x0036C9 00:F6B9: A9 00     LDA #$00                                                   ; \
C - - - - - 0x0036CB 00:F6BB: 8D F5 07  STA ram_07F5_f0_sfx_flag_trip                              ; / Clear [$07F5]
bra_F6BE:
C - - - - - 0x0036CE 00:F6BE: B9 CA FB  LDA tbl_FBCA,Y                                             ; \ Load Sound Sequence Pointer to Y
C - - - - - 0x0036D1 00:F6C1: A8        TAY                                                        ; /
C - - - - - 0x0036D2 00:F6C2: A2 00     LDX #$00                                                   ; \
bra_F6C4:
C - - - - - 0x0036D4 00:F6C4: B9 CA FB  LDA tbl_FBCA,Y                                             ; |
C - - - - - 0x0036D7 00:F6C7: 95 DF     STA ram_00DF,X                                             ; | Load Sound Sequence Header
C - - - - - 0x0036D9 00:F6C9: C8        INY                                                        ; | (9 bytes)
C - - - - - 0x0036DA 00:F6CA: E8        INX                                                        ; |
C - - - - - 0x0036DB 00:F6CB: 8A        TXA                                                        ; |
C - - - - - 0x0036DC 00:F6CC: C9 09     CMP #$09                                                   ; |
C - - - - - 0x0036DE 00:F6CE: D0 F4     BNE bra_F6C4                                               ; /
C - - - - - 0x0036E0 00:F6D0: A9 01     LDA #$01                                                   ; \
C - - - - - 0x0036E2 00:F6D2: 85 D0     STA ram_00D0_sound                                         ; |
C - - - - - 0x0036E4 00:F6D4: 85 D1     STA ram_00D1_sound                                         ; |
C - - - - - 0x0036E6 00:F6D6: 85 D2     STA ram_00D2_sound                                         ; | Initialize Sequence stuff
C - - - - - 0x0036E8 00:F6D8: 85 D3     STA ram_00D3_sound                                         ; |
C - - - - - 0x0036EA 00:F6DA: A9 00     LDA #$00                                                   ; |
C - - - - - 0x0036EC 00:F6DC: 85 E8     STA ram_00E8                                               ; |
C - - - - - 0x0036EE 00:F6DE: 85 E9     STA ram_00E9                                               ; |
C - - - - - 0x0036F0 00:F6E0: 85 EA     STA ram_00EA                                               ; |
C - - - - - 0x0036F2 00:F6E2: 85 EB     STA ram_00EB                                               ; /
C - - - - - 0x0036F4 00:F6E4: 60        RTS
tbl_F6E5:
- D 3 - I - 0x0036F5 00:F6E5: 94        .byte $94   ; 
- D 3 - I - 0x0036F6 00:F6E6: AB        .byte $AB   ; 
- D 3 - I - 0x0036F7 00:F6E7: FD        .byte $FD   ; 
- D 3 - I - 0x0036F8 00:F6E8: 58        .byte $58   ; 
tbl_F6E9:
- D 3 - I - 0x0036F9 00:F6E9: 00        .byte $00   ; 
- D 3 - I - 0x0036FA 00:F6EA: 7F        .byte $7F   ; 
- D 3 - I - 0x0036FB 00:F6EB: 04        .byte $04   ; 
- D 3 - I - 0x0036FC 00:F6EC: 18        .byte $18   ; 
tbl_F6ED:
- D 3 - I - 0x0036FD 00:F6ED: 3F        .byte $3F   ; 
- D 3 - I - 0x0036FE 00:F6EE: 7F        .byte $7F   ; 
- D 3 - I - 0x0036FF 00:F6EF: 00        .byte $00   ; 
- D 3 - I - 0x003700 00:F6F0: 00        .byte $00   ; 
tbl_F6F1:
- D 3 - I - 0x003701 00:F6F1: 06        .byte $06   ; 
- D 3 - I - 0x003702 00:F6F2: 7F        .byte $7F   ; 
- D 3 - I - 0x003703 00:F6F3: 0A        .byte $0A   ; 
- D 3 - I - 0x003704 00:F6F4: C0        .byte $C0   ; 
tbl_F6F5:
- D 3 - I - 0x003705 00:F6F5: 08        .byte $08   ; 
- D 3 - I - 0x003706 00:F6F6: 7F        .byte $7F   ; 
- D 3 - I - 0x003707 00:F6F7: 05        .byte $05   ; 
- D 3 - I - 0x003708 00:F6F8: C0        .byte $C0   ; 
tbl_F6F9:
- D 3 - I - 0x003709 00:F6F9: C1        .byte $C1   ; 
- D 3 - I - 0x00370A 00:F6FA: 89        .byte $89   ; 
- D 3 - I - 0x00370B 00:F6FB: 02        .byte $02   ; 
- D 3 - I - 0x00370C 00:F6FC: 0F        .byte $0F   ; 


; bzk garbage
- - - - - - 0x00370D 00:F6FD: FF        .byte $FF   ; 
- - - - - - 0x00370E 00:F6FE: FF        .byte $FF   ; 
- - - - - - 0x00370F 00:F6FF: FF        .byte $FF   ; 



tbl_F700:
- - - - - - 0x003710 00:F700: 10        .byte $10   ; 
- - - - - - 0x003711 00:F701: 00        .byte $00   ; 
- D 3 - - - 0x003712 00:F702: 18        .byte $18   ; 
- D 3 - - - 0x003713 00:F703: 10        .byte $10   ; 
- D 3 - - - 0x003714 00:F704: 01        .byte $01   ; 
- D 3 - - - 0x003715 00:F705: 18        .byte $18   ; 
- D 3 - - - 0x003716 00:F706: 00        .byte $00   ; 
- D 3 - - - 0x003717 00:F707: 01        .byte $01   ; 
- D 3 - - - 0x003718 00:F708: 88        .byte $88   ; 
- D 3 - - - 0x003719 00:F709: 02        .byte $02   ; 
- D 3 - - - 0x00371A 00:F70A: 02        .byte $02   ; 
- D 3 - - - 0x00371B 00:F70B: 40        .byte $40   ; 
- D 3 - - - 0x00371C 00:F70C: 03        .byte $03   ; 
- D 3 - - - 0x00371D 00:F70D: 05        .byte $05   ; 
- D 3 - - - 0x00371E 00:F70E: 40        .byte $40   ; 
- D 3 - - - 0x00371F 00:F70F: 04        .byte $04   ; 
- D 3 - - - 0x003720 00:F710: 07        .byte $07   ; 
- D 3 - - - 0x003721 00:F711: 40        .byte $40   ; 
sub_F712:
C - - - - - 0x003722 00:F712: A9 7F     LDA #$7F                                                   ; \ Set Pulse Channels:
C - - - - - 0x003724 00:F714: 8D 01 40  STA $4001                                                  ; | No Sweep
C - - - - - 0x003727 00:F717: 8D 05 40  STA $4005                                                  ; /
sub_F71A:
C - - - - - 0x00372A 00:F71A: 86 DC     STX ram_00DC
C - - - - - 0x00372C 00:F71C: 84 DD     STY ram_00DD
C - - - - - 0x00372E 00:F71E: 60        RTS
bra_F71F:
C - - - - - 0x00372F 00:F71F: A2 E5     LDX #< tbl_F6E5
C - - - - - 0x003731 00:F721: A0 F6     LDY #> tbl_F6E5
C - - - - - 0x003733 00:F723: D0 20     BNE bra_F745
sub_F725:
C - - - - - 0x003735 00:F725: A5 F3     LDA ram_00F3_sfx_3
C - - - - - 0x003737 00:F727: 4A        LSR
C - - - - - 0x003738 00:F728: B0 0C     BCS bra_F736
; bzk bug? something is reading these bytes of code
C D 3 - - - 0x00373A 00:F72A: A5 F7     LDA ram_00F7
C D 3 - - - 0x00373C 00:F72C: 4A        LSR
C - - - - - 0x00373D 00:F72D: B0 1A     BCS bra_F749
C - - - - - 0x00373F 00:F72F: A5 F0     LDA ram_00F0_sfx_1
C - - - - - 0x003741 00:F731: 29 10     AND #$10
C - - - - - 0x003743 00:F733: D0 EA     BNE bra_F71F
C - - - - - 0x003745 00:F735: 60        RTS
bra_F736:
C - - - - - 0x003746 00:F736: A5 F7     LDA ram_00F7
C - - - - - 0x003748 00:F738: 09 01     ORA #$01
C - - - - - 0x00374A 00:F73A: 85 F7     STA ram_00F7
C - - - - - 0x00374C 00:F73C: A9 00     LDA #$00
C - - - - - 0x00374E 00:F73E: 8D E4 07  STA ram_07E4
C - - - - - 0x003751 00:F741: A2 FC     LDX #< tbl_F5FC
C - - - - - 0x003753 00:F743: A0 F5     LDY #> tbl_F5FC
bra_F745:
C - - - - - 0x003755 00:F745: 20 83 F6  JSR sub_F683
C - - - - - 0x003758 00:F748: 60        RTS
bra_F749:
C - - - - - 0x003759 00:F749: EE E4 07  INC ram_07E4
C - - - - - 0x00375C 00:F74C: AD E4 07  LDA ram_07E4
C - - - - - 0x00375F 00:F74F: C9 58     CMP #$58
C - - - - - 0x003761 00:F751: D0 28     BNE bra_F77B_RTS
C - - - - - 0x003763 00:F753: A9 00     LDA #$00
C - - - - - 0x003765 00:F755: 85 F7     STA ram_00F7
C - - - - - 0x003767 00:F757: 60        RTS
loc_F758:
C D 3 - - - 0x003768 00:F758: A9 C0     LDA #$C0                                                   ; \ Set Frame Counter
C - - - - - 0x00376A 00:F75A: 8D 17 40  STA $4017                                                  ; / to 4-step sequence, clear Frame Interrupt Flag
C - - - - - 0x00376D 00:F75D: 20 25 FB  JSR sub_FB25_Music
C - - - - - 0x003770 00:F760: 20 0A F9  JSR sub_F90A
C - - - - - 0x003773 00:F763: 20 38 FA  JSR sub_FA38
C - - - - - 0x003776 00:F766: 20 AC FA  JSR sub_FAAC
C - - - - - 0x003779 00:F769: 20 24 F8  JSR sub_F824
C - - - - - 0x00377C 00:F76C: A5 F1     LDA ram_00F1_sfx_2
C - - - - - 0x00377E 00:F76E: 8D E9 07  STA ram_07E9
C - - - - - 0x003781 00:F771: A9 00     LDA #$00
C - - - - - 0x003783 00:F773: 85 F0     STA ram_00F0_sfx_1                                         ; \
C - - - - - 0x003785 00:F775: 85 F1     STA ram_00F1_sfx_2                                         ; | Clear Music/SFX Flags
C - - - - - 0x003787 00:F777: 85 F2     STA ram_00F2_music_jingle                                  ; |
C - - - - - 0x003789 00:F779: 85 F3     STA ram_00F3_sfx_3                                         ; /
bra_F77B_RTS:
C - - - - - 0x00378B 00:F77B: 60        RTS
loc_F77C:
C D 3 - - - 0x00378C 00:F77C: A5 F4     LDA ram_00F4
C - - - - - 0x00378E 00:F77E: 29 06     AND #$06
C - - - - - 0x003790 00:F780: D0 F9     BNE bra_F77B_RTS
C - - - - - 0x003792 00:F782: A5 F4     LDA ram_00F4
C - - - - - 0x003794 00:F784: 29 F0     AND #$F0
C - - - - - 0x003796 00:F786: 85 F4     STA ram_00F4
C - - - - - 0x003798 00:F788: A2 E9     LDX #< tbl_F6E9
C - - - - - 0x00379A 00:F78A: A0 F6     LDY #> tbl_F6E9
C - - - - - 0x00379C 00:F78C: 4C 79 F8  JMP loc_F879
sub_F78F:
C - - - - - 0x00379F 00:F78F: A5 F6     LDA ram_00F6_current_music
C - - - - - 0x0037A1 00:F791: C9 20     CMP #$20
C - - - - - 0x0037A3 00:F793: D0 0A     BNE bra_F79F
C - - - - - 0x0037A5 00:F795: EE E8 07  INC ram_07E8_ball_trip_music_flag
sub_F798:
C - - - - - 0x0037A8 00:F798: 29 0F     AND #$0F                                                   ; \ Initialize Sound Channels
C - - - - - 0x0037AA 00:F79A: C9 0F     CMP #$0F                                                   ; | differently depending on
C - - - - - 0x0037AC 00:F79C: D0 07     BNE bra_F7A5                                               ; / Music/Jingle needs
C - - - - - 0x0037AE 00:F79E: 8A        TXA
bra_F79F:
loc_F79F:
C D 3 - - - 0x0037AF 00:F79F: A5 F4     LDA ram_00F4
C - - - - - 0x0037B1 00:F7A1: 29 20     AND #$20
C - - - - - 0x0037B3 00:F7A3: D0 22     BNE bra_F7C7
bra_F7A5:
loc_F7A5:
sub_F7A5:
C D 3 - - - 0x0037B5 00:F7A5: A9 10     LDA #$10                                                   ; \ Constant volume on:
C - - - - - 0x0037B7 00:F7A7: 8D 0C 40  STA $400C                                                  ; | - Noise Channel
C - - - - - 0x0037BA 00:F7AA: 8D 00 40  STA $4000                                                  ; | - Pulse 1 Channel
C - - - - - 0x0037BD 00:F7AD: 8D 04 40  STA $4004                                                  ; / - Pulse 2 Channel
C - - - - - 0x0037C0 00:F7B0: A9 00     LDA #$00
C - - - - - 0x0037C2 00:F7B2: 85 F4     STA ram_00F4
bra_F7B4:
C - - - - - 0x0037C4 00:F7B4: 85 F5     STA ram_00F5
C - - - - - 0x0037C6 00:F7B6: 85 F6     STA ram_00F6_current_music                                 ; Clear Current Music/Jingle
C - - - - - 0x0037C8 00:F7B8: 8D FA 07  STA ram_07FA_hal_reset_check
C - - - - - 0x0037CB 00:F7BB: 85 F7     STA ram_00F7
C - - - - - 0x0037CD 00:F7BD: 8D 08 40  STA $4008                                                  ; Clear Triangle Channel Linear Counter
C - - - - - 0x0037D0 00:F7C0: 8D 11 40  STA $4011                                                  ; Clear DMC Channel Load Counter
C - - - - - 0x0037D3 00:F7C3: 8D F0 07  STA ram_07F0_audio_related
C - - - - - 0x0037D6 00:F7C6: 60        RTS
bra_F7C7:
C - - - - - 0x0037D7 00:F7C7: A9 10     LDA #$10                                                   ; \ Constant volume on:
C - - - - - 0x0037D9 00:F7C9: 8D 04 40  STA $4004                                                  ; | - Pulse 2 Channel
C - - - - - 0x0037DC 00:F7CC: 8D 0C 40  STA $400C                                                  ; / - Noise Channel
C - - - - - 0x0037DF 00:F7CF: A9 00     LDA #$00
C - - - - - 0x0037E1 00:F7D1: F0 E1     BEQ bra_F7B4
bra_F7D3:
C - - - - - 0x0037E3 00:F7D3: A9 00     LDA #$00
C - - - - - 0x0037E5 00:F7D5: 8D FA 07  STA ram_07FA_hal_reset_check
C - - - - - 0x0037E8 00:F7D8: 60        RTS
bra_F7D9:
C - - - - - 0x0037E9 00:F7D9: A2 F5     LDX #< tbl_F6F5
C - - - - - 0x0037EB 00:F7DB: A0 F6     LDY #> tbl_F6F5
C - - - - - 0x0037ED 00:F7DD: 4C 79 F8  JMP loc_F879
bra_F7E0:
C - - - - - 0x0037F0 00:F7E0: EE FB 07  INC ram_07FB
C - - - - - 0x0037F3 00:F7E3: AD FB 07  LDA ram_07FB
C - - - - - 0x0037F6 00:F7E6: C9 10     CMP #$10
C - - - - - 0x0037F8 00:F7E8: F0 EF     BEQ bra_F7D9
C - - - - - 0x0037FA 00:F7EA: C9 20     CMP #$20
C - - - - - 0x0037FC 00:F7EC: F0 E5     BEQ bra_F7D3
C - - - - - 0x0037FE 00:F7EE: 60        RTS
bra_F7EF:
C - - - - - 0x0037FF 00:F7EF: A9 00     LDA #$00
C - - - - - 0x003801 00:F7F1: 8D FB 07  STA ram_07FB
C - - - - - 0x003804 00:F7F4: A9 F0     LDA #$F0
C - - - - - 0x003806 00:F7F6: 8D FA 07  STA ram_07FA_hal_reset_check
C - - - - - 0x003809 00:F7F9: A2 F1     LDX #< tbl_F6F1
C - - - - - 0x00380B 00:F7FB: A0 F6     LDY #> tbl_F6F1
C - - - - - 0x00380D 00:F7FD: 4C 79 F8  JMP loc_F879
bra_F800:
C - - - - - 0x003810 00:F800: A5 F4     LDA ram_00F4
C - - - - - 0x003812 00:F802: 29 F0     AND #$F0
C - - - - - 0x003814 00:F804: 09 02     ORA #$02
C - - - - - 0x003816 00:F806: 85 F4     STA ram_00F4
C - - - - - 0x003818 00:F808: A9 00     LDA #$00
C - - - - - 0x00381A 00:F80A: 8D F7 07  STA ram_07F7
C - - - - - 0x00381D 00:F80D: A2 F1     LDX #< tbl_F6F1
C - - - - - 0x00381F 00:F80F: A0 F6     LDY #> tbl_F6F1
C - - - - - 0x003821 00:F811: 4C 79 F8  JMP loc_F879
bra_F814:
C - - - - - 0x003824 00:F814: EE F7 07  INC ram_07F7
C - - - - - 0x003827 00:F817: AD F7 07  LDA ram_07F7
C - - - - - 0x00382A 00:F81A: C9 10     CMP #$10
C - - - - - 0x00382C 00:F81C: D0 3D     BNE bra_F85B_RTS
C - - - - - 0x00382E 00:F81E: 4C 9B F8  JMP loc_F89B
bra_F821:
C - - - - - 0x003831 00:F821: 4C A5 F7  JMP loc_F7A5
sub_F824:
C - - - - - 0x003834 00:F824: A5 F0     LDA ram_00F0_sfx_1
C - - - - - 0x003836 00:F826: 4A        LSR
C - - - - - 0x003837 00:F827: B0 F8     BCS bra_F821
C - - - - - 0x003839 00:F829: A5 F6     LDA ram_00F6_current_music
C - - - - - 0x00383B 00:F82B: C9 DF     CMP #$DF
C - - - - - 0x00383D 00:F82D: F0 0C     BEQ bra_F83B
C - - - - - 0x00383F 00:F82F: C9 7F     CMP #$7F
C - - - - - 0x003841 00:F831: F0 08     BEQ bra_F83B
C - - - - - 0x003843 00:F833: C9 20     CMP #$20
C - - - - - 0x003845 00:F835: F0 04     BEQ bra_F83B
C - - - - - 0x003847 00:F837: A5 F6     LDA ram_00F6_current_music
C - - - - - 0x003849 00:F839: D0 20     BNE bra_F85B_RTS
bra_F83B:
C - - - - - 0x00384B 00:F83B: AD FA 07  LDA ram_07FA_hal_reset_check
C - - - - - 0x00384E 00:F83E: C9 0F     CMP #$0F
C - - - - - 0x003850 00:F840: F0 AD     BEQ bra_F7EF
C - - - - - 0x003852 00:F842: C9 F0     CMP #$F0
C - - - - - 0x003854 00:F844: F0 9A     BEQ bra_F7E0
C - - - - - 0x003856 00:F846: A5 F0     LDA ram_00F0_sfx_1
C - - - - - 0x003858 00:F848: 4A        LSR
C - - - - - 0x003859 00:F849: 4A        LSR
C - - - - - 0x00385A 00:F84A: B0 B4     BCS bra_F800
C - - - - - 0x00385C 00:F84C: 4A        LSR
C - - - - - 0x00385D 00:F84D: B0 10     BCS bra_F85F
C - - - - - 0x00385F 00:F84F: 4A        LSR
C - - - - - 0x003860 00:F850: B0 0A     BCS bra_F85C
C - - - - - 0x003862 00:F852: A5 F4     LDA ram_00F4
C - - - - - 0x003864 00:F854: 4A        LSR
C - - - - - 0x003865 00:F855: 4A        LSR
C - - - - - 0x003866 00:F856: B0 BC     BCS bra_F814
C - - - - - 0x003868 00:F858: 4A        LSR
C - - - - - 0x003869 00:F859: B0 22     BCS bra_F87D
bra_F85B_RTS:
C - - - - - 0x00386B 00:F85B: 60        RTS
bra_F85C:
C - - - - - 0x00386C 00:F85C: 4C 7C F7  JMP loc_F77C
bra_F85F:
C - - - - - 0x00386F 00:F85F: A5 F4     LDA ram_00F4
C - - - - - 0x003871 00:F861: 29 80     AND #$80
C - - - - - 0x003873 00:F863: D0 F6     BNE bra_F85B_RTS
C - - - - - 0x003875 00:F865: A5 F4     LDA ram_00F4
C - - - - - 0x003877 00:F867: 29 F0     AND #$F0
C - - - - - 0x003879 00:F869: 09 04     ORA #$04
C - - - - - 0x00387B 00:F86B: 85 F4     STA ram_00F4
C - - - - - 0x00387D 00:F86D: A9 00     LDA #$00
C - - - - - 0x00387F 00:F86F: 8D F3 07  STA ram_07F3
C - - - - - 0x003882 00:F872: 8D F1 07  STA ram_07F1
C - - - - - 0x003885 00:F875: A2 ED     LDX #< tbl_F6ED
C - - - - - 0x003887 00:F877: A0 F6     LDY #> tbl_F6ED
loc_F879:
C D 3 - - - 0x003889 00:F879: 20 8B F6  JSR sub_F68B
C - - - - - 0x00388C 00:F87C: 60        RTS
bra_F87D:
C - - - - - 0x00388D 00:F87D: EE F3 07  INC ram_07F3
C - - - - - 0x003890 00:F880: AD F3 07  LDA ram_07F3
C - - - - - 0x003893 00:F883: C9 03     CMP #$03
C - - - - - 0x003895 00:F885: D0 1A     BNE bra_F8A1_RTS
C - - - - - 0x003897 00:F887: A9 00     LDA #$00
C - - - - - 0x003899 00:F889: 8D F3 07  STA ram_07F3
C - - - - - 0x00389C 00:F88C: EE F1 07  INC ram_07F1
C - - - - - 0x00389F 00:F88F: AD F1 07  LDA ram_07F1
C - - - - - 0x0038A2 00:F892: C9 10     CMP #$10
C - - - - - 0x0038A4 00:F894: D0 0C     BNE bra_F8A2
C - - - - - 0x0038A6 00:F896: A9 10     LDA #$10
C - - - - - 0x0038A8 00:F898: 8D 0C 40  STA $400C
loc_F89B:
C D 3 - - - 0x0038AB 00:F89B: A5 F4     LDA ram_00F4
C - - - - - 0x0038AD 00:F89D: 29 F0     AND #$F0
C - - - - - 0x0038AF 00:F89F: 85 F4     STA ram_00F4
bra_F8A1_RTS:
C - - - - - 0x0038B1 00:F8A1: 60        RTS
bra_F8A2:
C - - - - - 0x0038B2 00:F8A2: 8D 0E 40  STA $400E
C - - - - - 0x0038B5 00:F8A5: 60        RTS
loc_F8A6:
C D 3 - - - 0x0038B6 00:F8A6: A9 00     LDA #$00
C - - - - - 0x0038B8 00:F8A8: 8D E0 07  STA ram_07E0
C - - - - - 0x0038BB 00:F8AB: 18        CLC
C - - - - - 0x0038BC 00:F8AC: A5 1B     LDA ram_001B_rng_output_seed
C - - - - - 0x0038BE 00:F8AE: 29 07     AND #$07
C - - - - - 0x0038C0 00:F8B0: 69 02     ADC #$02
C - - - - - 0x0038C2 00:F8B2: 8D E1 07  STA ram_07E1
C - - - - - 0x0038C5 00:F8B5: A5 F7     LDA ram_00F7
C - - - - - 0x0038C7 00:F8B7: 29 00     AND #$00
C - - - - - 0x0038C9 00:F8B9: 09 80     ORA #$80
C - - - - - 0x0038CB 00:F8BB: 85 F7     STA ram_00F7
C - - - - - 0x0038CD 00:F8BD: D0 29     BNE bra_F8E8
loc_F8BF:
C D 3 - - - 0x0038CF 00:F8BF: EE E0 07  INC ram_07E0
C - - - - - 0x0038D2 00:F8C2: AD E0 07  LDA ram_07E0
C - - - - - 0x0038D5 00:F8C5: CD E1 07  CMP ram_07E1
C - - - - - 0x0038D8 00:F8C8: D0 1E     BNE bra_F8E8
loc_F8CA:
C D 3 - - - 0x0038DA 00:F8CA: A9 10     LDA #$10                                                   ; \ Constant volume on:
C - - - - - 0x0038DC 00:F8CC: 8D 00 40  STA $4000                                                  ; | - Pulse 1 Channel
C - - - - - 0x0038DF 00:F8CF: 8D 04 40  STA $4004                                                  ; / - Pulse 2 Channel
C - - - - - 0x0038E2 00:F8D2: A9 00     LDA #$00
C - - - - - 0x0038E4 00:F8D4: 85 F7     STA ram_00F7
C - - - - - 0x0038E6 00:F8D6: A5 F4     LDA ram_00F4
C - - - - - 0x0038E8 00:F8D8: 29 0F     AND #$0F
C - - - - - 0x0038EA 00:F8DA: 85 F4     STA ram_00F4
C - - - - - 0x0038EC 00:F8DC: 60        RTS
bra_F8DD:
C - - - - - 0x0038ED 00:F8DD: 20 A5 F7  JSR sub_F7A5
C - - - - - 0x0038F0 00:F8E0: A9 80     LDA #$80
C - - - - - 0x0038F2 00:F8E2: 85 F4     STA ram_00F4
C - - - - - 0x0038F4 00:F8E4: A9 02     LDA #$02                                                   ; \ Play Balloon Pop SFX
C - - - - - 0x0038F6 00:F8E6: 85 F0     STA ram_00F0_sfx_1                                         ; /
bra_F8E8:
C - - - - - 0x0038F8 00:F8E8: A2 F9     LDX #< tbl_F6F9
C - - - - - 0x0038FA 00:F8EA: A0 F6     LDY #> tbl_F6F9
C - - - - - 0x0038FC 00:F8EC: 20 83 F6  JSR sub_F683
C - - - - - 0x0038FF 00:F8EF: A5 1B     LDA ram_001B_rng_output_seed
C - - - - - 0x003901 00:F8F1: 29 0F     AND #$0F
C - - - - - 0x003903 00:F8F3: 8D 02 40  STA $4002
C - - - - - 0x003906 00:F8F6: A2 F9     LDX #< tbl_F6F9
C - - - - - 0x003908 00:F8F8: A0 F6     LDY #> tbl_F6F9
C - - - - - 0x00390A 00:F8FA: 20 8F F6  JSR sub_F68F
C - - - - - 0x00390D 00:F8FD: A5 1B     LDA ram_001B_rng_output_seed
C - - - - - 0x00390F 00:F8FF: 4A        LSR
C - - - - - 0x003910 00:F900: 4A        LSR
C - - - - - 0x003911 00:F901: 29 0F     AND #$0F
C - - - - - 0x003913 00:F903: 8D 06 40  STA $4006
C - - - - - 0x003916 00:F906: 60        RTS
bra_F907:
C - - - - - 0x003917 00:F907: 4C A6 F8  JMP loc_F8A6
sub_F90A:
C - - - - - 0x00391A 00:F90A: A5 F6     LDA ram_00F6_current_music                                 ; \ Check if music is not playing
C - - - - - 0x00391C 00:F90C: F0 0D     BEQ bra_F91B                                               ; / If not playing then continue as normal
C - - - - - 0x00391E 00:F90E: C9 DF     CMP #$DF                                                   ; \ Songs #$DF?
C - - - - - 0x003920 00:F910: F0 09     BEQ bra_F91B                                               ; / Wouldn't that be redundant?
C - - - - - 0x003922 00:F912: A5 F0     LDA ram_00F0_sfx_1                                         ; \
C - - - - - 0x003924 00:F914: 29 E0     AND #$E0                                                   ; | Check for sound effects that stop the music
C - - - - - 0x003926 00:F916: F0 36     BEQ bra_F94E_RTS                                           ; / if found, then return
C - - - - - 0x003928 00:F918: 20 A5 F7  JSR sub_F7A5
bra_F91B:
C - - - - - 0x00392B 00:F91B: A5 F0     LDA ram_00F0_sfx_1
C - - - - - 0x00392D 00:F91D: 0A        ASL
C - - - - - 0x00392E 00:F91E: B0 BD     BCS bra_F8DD
C - - - - - 0x003930 00:F920: 0A        ASL
C - - - - - 0x003931 00:F921: B0 2F     BCS bra_F952
C - - - - - 0x003933 00:F923: 0A        ASL
C - - - - - 0x003934 00:F924: B0 3F     BCS bra_F965
C - - - - - 0x003936 00:F926: A5 F4     LDA ram_00F4
C - - - - - 0x003938 00:F928: 0A        ASL
C - - - - - 0x003939 00:F929: B0 BD     BCS bra_F8E8
C - - - - - 0x00393B 00:F92B: A5 F4     LDA ram_00F4
C - - - - - 0x00393D 00:F92D: 29 E0     AND #$E0
C - - - - - 0x00393F 00:F92F: D0 1D     BNE bra_F94E_RTS
C - - - - - 0x003941 00:F931: A5 F6     LDA ram_00F6_current_music
C - - - - - 0x003943 00:F933: C9 DF     CMP #$DF
C - - - - - 0x003945 00:F935: F0 14     BEQ bra_F94B
C - - - - - 0x003947 00:F937: A5 F6     LDA ram_00F6_current_music
C - - - - - 0x003949 00:F939: D0 13     BNE bra_F94E_RTS
C - - - - - 0x00394B 00:F93B: A5 F3     LDA ram_00F3_sfx_3
C - - - - - 0x00394D 00:F93D: 0A        ASL
C - - - - - 0x00394E 00:F93E: B0 C7     BCS bra_F907
C - - - - - 0x003950 00:F940: 0A        ASL
C - - - - - 0x003951 00:F941: B0 34     BCS bra_F977
C - - - - - 0x003953 00:F943: A5 F7     LDA ram_00F7
C - - - - - 0x003955 00:F945: 0A        ASL
C - - - - - 0x003956 00:F946: B0 07     BCS bra_F94F
C - - - - - 0x003958 00:F948: 0A        ASL
C - - - - - 0x003959 00:F949: B0 48     BCS bra_F993
bra_F94B:
C - - - - - 0x00395B 00:F94B: 20 25 F7  JSR sub_F725
bra_F94E_RTS:
C - - - - - 0x00395E 00:F94E: 60        RTS
bra_F94F:
C - - - - - 0x00395F 00:F94F: 4C BF F8  JMP loc_F8BF
bra_F952:
C - - - - - 0x003962 00:F952: A9 0F     LDA #$0F
C - - - - - 0x003964 00:F954: 8D FA 07  STA ram_07FA_hal_reset_check
C - - - - - 0x003967 00:F957: A5 F4     LDA ram_00F4
C - - - - - 0x003969 00:F959: 29 0F     AND #$0F
C - - - - - 0x00396B 00:F95B: 09 40     ORA #$40
C - - - - - 0x00396D 00:F95D: 85 F4     STA ram_00F4
C - - - - - 0x00396F 00:F95F: A2 D1     LDX #< tbl_F9D1
C - - - - - 0x003971 00:F961: A0 F9     LDY #> tbl_F9D1
C - - - - - 0x003973 00:F963: D0 2A     BNE bra_F98F
bra_F965:
C - - - - - 0x003975 00:F965: A9 02     LDA #$02                                                   ; \ Play Balloon Pop SFX
C - - - - - 0x003977 00:F967: 85 F0     STA ram_00F0_sfx_1                                         ; /
C - - - - - 0x003979 00:F969: A5 F4     LDA ram_00F4
C - - - - - 0x00397B 00:F96B: 29 0F     AND #$0F
C - - - - - 0x00397D 00:F96D: 09 20     ORA #$20
C - - - - - 0x00397F 00:F96F: 85 F4     STA ram_00F4
C - - - - - 0x003981 00:F971: A2 CD     LDX #< tbl_F9CD
C - - - - - 0x003983 00:F973: A0 F9     LDY #> tbl_F9CD
C - - - - - 0x003985 00:F975: D0 18     BNE bra_F98F
bra_F977:
C - - - - - 0x003987 00:F977: A9 00     LDA #$00
C - - - - - 0x003989 00:F979: 8D FC 07  STA ram_07FC
C - - - - - 0x00398C 00:F97C: A5 F7     LDA ram_00F7
C - - - - - 0x00398E 00:F97E: 29 00     AND #$00
C - - - - - 0x003990 00:F980: 09 40     ORA #$40
C - - - - - 0x003992 00:F982: 85 F7     STA ram_00F7
C - - - - - 0x003994 00:F984: A2 D5     LDX #< tbl_F9D5
C - - - - - 0x003996 00:F986: A0 F9     LDY #> tbl_F9D5
C - - - - - 0x003998 00:F988: 20 8F F6  JSR sub_F68F
C - - - - - 0x00399B 00:F98B: A2 D9     LDX #< tbl_F9D9
C - - - - - 0x00399D 00:F98D: A0 F9     LDY #> tbl_F9D9
bra_F98F:
C - - - - - 0x00399F 00:F98F: 20 83 F6  JSR sub_F683
C - - - - - 0x0039A2 00:F992: 60        RTS
bra_F993:
C - - - - - 0x0039A3 00:F993: EE FC 07  INC ram_07FC
C - - - - - 0x0039A6 00:F996: AD FC 07  LDA ram_07FC
C - - - - - 0x0039A9 00:F999: C9 12     CMP #$12
C - - - - - 0x0039AB 00:F99B: F0 2D     BEQ bra_F9CA
C - - - - - 0x0039AD 00:F99D: C9 06     CMP #$06
C - - - - - 0x0039AF 00:F99F: 90 10     BCC bra_F9B1
C - - - - - 0x0039B1 00:F9A1: A5 1B     LDA ram_001B_rng_output_seed
C - - - - - 0x0039B3 00:F9A3: 09 10     ORA #$10
C - - - - - 0x0039B5 00:F9A5: 29 7F     AND #$7F
C - - - - - 0x0039B7 00:F9A7: 8D FE 07  STA ram_07FE
C - - - - - 0x0039BA 00:F9AA: 2A        ROL
C - - - - - 0x0039BB 00:F9AB: 8D FD 07  STA ram_07FD
C - - - - - 0x0039BE 00:F9AE: 4C BD F9  JMP loc_F9BD
bra_F9B1:
C - - - - - 0x0039C1 00:F9B1: EE FD 07  INC ram_07FD
C - - - - - 0x0039C4 00:F9B4: EE FD 07  INC ram_07FD
C - - - - - 0x0039C7 00:F9B7: EE FE 07  INC ram_07FE
C - - - - - 0x0039CA 00:F9BA: EE FE 07  INC ram_07FE
loc_F9BD:
C D 3 - - - 0x0039CD 00:F9BD: AD FD 07  LDA ram_07FD
C - - - - - 0x0039D0 00:F9C0: 8D 06 40  STA $4006
C - - - - - 0x0039D3 00:F9C3: AD FE 07  LDA ram_07FE
C - - - - - 0x0039D6 00:F9C6: 8D 02 40  STA $4002
C - - - - - 0x0039D9 00:F9C9: 60        RTS
bra_F9CA:
C - - - - - 0x0039DA 00:F9CA: 4C CA F8  JMP loc_F8CA
tbl_F9CD:
- D 3 - I - 0x0039DD 00:F9CD: B8        .byte $B8   ; 
- D 3 - I - 0x0039DE 00:F9CE: D5        .byte $D5   ; 
- D 3 - I - 0x0039DF 00:F9CF: 20        .byte $20   ; 
- D 3 - I - 0x0039E0 00:F9D0: 00        .byte $00   ; 
tbl_F9D1:
- D 3 - I - 0x0039E1 00:F9D1: 9F        .byte $9F   ; 
- D 3 - I - 0x0039E2 00:F9D2: 93        .byte $93   ; 
- D 3 - I - 0x0039E3 00:F9D3: 80        .byte $80   ; 
- D 3 - I - 0x0039E4 00:F9D4: 22        .byte $22   ; 
tbl_F9D5:
- D 3 - I - 0x0039E5 00:F9D5: 3F        .byte $3F   ; 
- D 3 - I - 0x0039E6 00:F9D6: BA        .byte $BA   ; 
- D 3 - I - 0x0039E7 00:F9D7: E0        .byte $E0   ; 
- D 3 - I - 0x0039E8 00:F9D8: 06        .byte $06   ; 
tbl_F9D9:
- D 3 - I - 0x0039E9 00:F9D9: 3F        .byte $3F   ; 
- D 3 - I - 0x0039EA 00:F9DA: BB        .byte $BB   ; 
- D 3 - I - 0x0039EB 00:F9DB: CE        .byte $CE   ; 
- D 3 - I - 0x0039EC 00:F9DC: 06        .byte $06   ; 
tbl_F9DD:
- D 3 - I - 0x0039ED 00:F9DD: B8        .byte $B8   ; 
- D 3 - I - 0x0039EE 00:F9DE: 93        .byte $93   ; 
- D 3 - I - 0x0039EF 00:F9DF: 50        .byte $50   ; 
- D 3 - I - 0x0039F0 00:F9E0: 02        .byte $02   ; 
tbl_F9E1:
- D 3 - I - 0x0039F1 00:F9E1: 80        .byte $80   ; 
- D 3 - I - 0x0039F2 00:F9E2: 7F        .byte $7F   ; 
- D 3 - I - 0x0039F3 00:F9E3: 60        .byte $60   ; 
- D 3 - I - 0x0039F4 00:F9E4: 68        .byte $68   ; 
tbl_F9E5:
- D 3 - I - 0x0039F5 00:F9E5: 80        .byte $80   ; 
- D 3 - I - 0x0039F6 00:F9E6: 7F        .byte $7F   ; 
- D 3 - I - 0x0039F7 00:F9E7: 62        .byte $62   ; 
- D 3 - I - 0x0039F8 00:F9E8: 68        .byte $68   ; 
bra_F9E9:
C - - - - - 0x0039F9 00:F9E9: A5 F5     LDA ram_00F5
C - - - - - 0x0039FB 00:F9EB: 29 02     AND #$02
C - - - - - 0x0039FD 00:F9ED: D0 37     BNE bra_FA26_RTS
C - - - - - 0x0039FF 00:F9EF: A5 F6     LDA ram_00F6_current_music
C - - - - - 0x003A01 00:F9F1: C9 DF     CMP #$DF
C - - - - - 0x003A03 00:F9F3: F0 04     BEQ bra_F9F9
C - - - - - 0x003A05 00:F9F5: A5 F6     LDA ram_00F6_current_music
C - - - - - 0x003A07 00:F9F7: D0 2D     BNE bra_FA26_RTS
bra_F9F9:
C - - - - - 0x003A09 00:F9F9: A9 00     LDA #$00
C - - - - - 0x003A0B 00:F9FB: 8D F9 07  STA ram_07F9
C - - - - - 0x003A0E 00:F9FE: A5 F5     LDA ram_00F5
C - - - - - 0x003A10 00:FA00: 29 E0     AND #$E0
C - - - - - 0x003A12 00:FA02: 09 02     ORA #$02
C - - - - - 0x003A14 00:FA04: 85 F5     STA ram_00F5
C - - - - - 0x003A16 00:FA06: A2 DD     LDX #< tbl_F9DD
C - - - - - 0x003A18 00:FA08: A0 F9     LDY #> tbl_F9DD
C - - - - - 0x003A1A 00:FA0A: D0 73     BNE bra_FA7F
bra_FA0C:
C - - - - - 0x003A1C 00:FA0C: EE F9 07  INC ram_07F9
C - - - - - 0x003A1F 00:FA0F: AD F9 07  LDA ram_07F9
C - - - - - 0x003A22 00:FA12: C9 07     CMP #$07
C - - - - - 0x003A24 00:FA14: D0 10     BNE bra_FA26_RTS
C - - - - - 0x003A26 00:FA16: A9 7F     LDA #$7F
C - - - - - 0x003A28 00:FA18: 8D 05 40  STA $4005
C - - - - - 0x003A2B 00:FA1B: A9 10     LDA #$10
C - - - - - 0x003A2D 00:FA1D: 8D 04 40  STA $4004
C - - - - - 0x003A30 00:FA20: A5 F5     LDA ram_00F5
C - - - - - 0x003A32 00:FA22: 29 E0     AND #$E0
C - - - - - 0x003A34 00:FA24: 85 F5     STA ram_00F5
bra_FA26_RTS:
C - - - - - 0x003A36 00:FA26: 60        RTS
bra_FA27:
C - - - - - 0x003A37 00:FA27: 20 A5 F7  JSR sub_F7A5
C - - - - - 0x003A3A 00:FA2A: A2 E1     LDX #< tbl_F9E1
C - - - - - 0x003A3C 00:FA2C: A0 F9     LDY #> tbl_F9E1
C - - - - - 0x003A3E 00:FA2E: 20 83 F6  JSR sub_F683
C - - - - - 0x003A41 00:FA31: A2 E5     LDX #< tbl_F9E5
C - - - - - 0x003A43 00:FA33: A0 F9     LDY #> tbl_F9E5
C - - - - - 0x003A45 00:FA35: 4C 7F FA  JMP loc_FA7F
sub_FA38:
C - - - - - 0x003A48 00:FA38: A5 F6     LDA ram_00F6_current_music
C - - - - - 0x003A4A 00:FA3A: F0 06     BEQ bra_FA42
C - - - - - 0x003A4C 00:FA3C: 29 0F     AND #$0F
C - - - - - 0x003A4E 00:FA3E: C9 0F     CMP #$0F
C - - - - - 0x003A50 00:FA40: D0 21     BNE bra_FA63_RTS
bra_FA42:
C - - - - - 0x003A52 00:FA42: A5 F4     LDA ram_00F4
C - - - - - 0x003A54 00:FA44: 29 80     AND #$80
C - - - - - 0x003A56 00:FA46: D0 1B     BNE bra_FA63_RTS
C - - - - - 0x003A58 00:FA48: A5 F7     LDA ram_00F7
C - - - - - 0x003A5A 00:FA4A: 29 C0     AND #$C0
C - - - - - 0x003A5C 00:FA4C: D0 15     BNE bra_FA63_RTS
C - - - - - 0x003A5E 00:FA4E: A5 F1     LDA ram_00F1_sfx_2
C - - - - - 0x003A60 00:FA50: 4A        LSR
C - - - - - 0x003A61 00:FA51: B0 D4     BCS bra_FA27
C - - - - - 0x003A63 00:FA53: 4A        LSR
C - - - - - 0x003A64 00:FA54: B0 93     BCS bra_F9E9
C - - - - - 0x003A66 00:FA56: 4A        LSR
C - - - - - 0x003A67 00:FA57: B0 2A     BCS bra_FA83
C - - - - - 0x003A69 00:FA59: 4A        LSR
C - - - - - 0x003A6A 00:FA5A: 4A        LSR
C - - - - - 0x003A6B 00:FA5B: B0 07     BCS bra_FA64
C - - - - - 0x003A6D 00:FA5D: A5 F5     LDA ram_00F5
C - - - - - 0x003A6F 00:FA5F: 4A        LSR
C - - - - - 0x003A70 00:FA60: 4A        LSR
C - - - - - 0x003A71 00:FA61: B0 A9     BCS bra_FA0C
bra_FA63_RTS:
C - - - - - 0x003A73 00:FA63: 60        RTS
bra_FA64:
C - - - - - 0x003A74 00:FA64: A5 F6     LDA ram_00F6_current_music
C - - - - - 0x003A76 00:FA66: D0 FB     BNE bra_FA63_RTS
C - - - - - 0x003A78 00:FA68: A5 F5     LDA ram_00F5
C - - - - - 0x003A7A 00:FA6A: 29 02     AND #$02
C - - - - - 0x003A7C 00:FA6C: D0 F5     BNE bra_FA63_RTS
C - - - - - 0x003A7E 00:FA6E: A2 8A     LDX #< tbl_FA8A
C - - - - - 0x003A80 00:FA70: A0 FA     LDY #> tbl_FA8A
C - - - - - 0x003A82 00:FA72: 20 8F F6  JSR sub_F68F
C - - - - - 0x003A85 00:FA75: A5 1B     LDA ram_001B_rng_output_seed
C - - - - - 0x003A87 00:FA77: 29 3F     AND #$3F
C - - - - - 0x003A89 00:FA79: 09 10     ORA #$10
C - - - - - 0x003A8B 00:FA7B: 8D 06 40  STA $4006
C - - - - - 0x003A8E 00:FA7E: 60        RTS
bra_FA7F:
loc_FA7F:
C D 3 - - - 0x003A8F 00:FA7F: 20 8F F6  JSR sub_F68F
C - - - - - 0x003A92 00:FA82: 60        RTS
bra_FA83:
C - - - - - 0x003A93 00:FA83: A0 0A     LDY #$0A
C - - - - - 0x003A95 00:FA85: A9 EF     LDA #$EF
C - - - - - 0x003A97 00:FA87: 4C A5 FB  JMP loc_FBA5
tbl_FA8A:
- D 3 - I - 0x003A9A 00:FA8A: D9        .byte $D9   ; 
- D 3 - I - 0x003A9B 00:FA8B: 86        .byte $86   ; 
- D 3 - I - 0x003A9C 00:FA8C: A8        .byte $A8   ; 
- D 3 - I - 0x003A9D 00:FA8D: 48        .byte $48   ; 
tbl_FA8E:
- D 3 - I - 0x003A9E 00:FA8E: 08        .byte $08   ; 
- D 3 - I - 0x003A9F 00:FA8F: 7F        .byte $7F   ; 
- D 3 - I - 0x003AA0 00:FA90: 40        .byte $40   ; 
- D 3 - I - 0x003AA1 00:FA91: 28        .byte $28   ; 
tbl_FA92:
- D 3 - I - 0x003AA2 00:FA92: 08        .byte $08   ; 
- D 3 - I - 0x003AA3 00:FA93: 7F        .byte $7F   ; 
- D 3 - I - 0x003AA4 00:FA94: 45        .byte $45   ; 
- D 3 - I - 0x003AA5 00:FA95: 28        .byte $28   ; 
bra_FA96:
C - - - - - 0x003AA6 00:FA96: EE F6 07  INC ram_07F6
C - - - - - 0x003AA9 00:FA99: AD F6 07  LDA ram_07F6
C - - - - - 0x003AAC 00:FA9C: C9 04     CMP #$04
C - - - - - 0x003AAE 00:FA9E: D0 38     BNE bra_FAD8_RTS
C - - - - - 0x003AB0 00:FAA0: A5 F5     LDA ram_00F5
C - - - - - 0x003AB2 00:FAA2: 29 1F     AND #$1F
C - - - - - 0x003AB4 00:FAA4: 85 F5     STA ram_00F5
C - - - - - 0x003AB6 00:FAA6: A2 92     LDX #< tbl_FA92
C - - - - - 0x003AB8 00:FAA8: A0 FA     LDY #> tbl_FA92
C - - - - - 0x003ABA 00:FAAA: D0 54     BNE bra_FB00
sub_FAAC:
C - - - - - 0x003ABC 00:FAAC: A5 F6     LDA ram_00F6_current_music
C - - - - - 0x003ABE 00:FAAE: F0 0A     BEQ bra_FABA
C - - - - - 0x003AC0 00:FAB0: C9 08     CMP #$08
C - - - - - 0x003AC2 00:FAB2: F0 06     BEQ bra_FABA
C - - - - - 0x003AC4 00:FAB4: 29 0F     AND #$0F
C - - - - - 0x003AC6 00:FAB6: C9 0F     CMP #$0F
C - - - - - 0x003AC8 00:FAB8: D0 1E     BNE bra_FAD8_RTS
bra_FABA:
C - - - - - 0x003ACA 00:FABA: A5 F4     LDA ram_00F4
C - - - - - 0x003ACC 00:FABC: 29 80     AND #$80
C - - - - - 0x003ACE 00:FABE: D0 18     BNE bra_FAD8_RTS
C - - - - - 0x003AD0 00:FAC0: A5 F1     LDA ram_00F1_sfx_2
C - - - - - 0x003AD2 00:FAC2: 0A        ASL
C - - - - - 0x003AD3 00:FAC3: B0 52     BCS bra_FB17
C - - - - - 0x003AD5 00:FAC5: 0A        ASL
C - - - - - 0x003AD6 00:FAC6: B0 1A     BCS bra_FAE2
C - - - - - 0x003AD8 00:FAC8: A5 F5     LDA ram_00F5
C - - - - - 0x003ADA 00:FACA: 0A        ASL
C - - - - - 0x003ADB 00:FACB: 0A        ASL
C - - - - - 0x003ADC 00:FACC: B0 C8     BCS bra_FA96
C - - - - - 0x003ADE 00:FACE: A5 F1     LDA ram_00F1_sfx_2
C - - - - - 0x003AE0 00:FAD0: 29 20     AND #$20
C - - - - - 0x003AE2 00:FAD2: F0 05     BEQ bra_FAD9
C - - - - - 0x003AE4 00:FAD4: A5 F6     LDA ram_00F6_current_music
C - - - - - 0x003AE6 00:FAD6: F0 2C     BEQ bra_FB04
bra_FAD8_RTS:
C - - - - - 0x003AE8 00:FAD8: 60        RTS
bra_FAD9:
C - - - - - 0x003AE9 00:FAD9: A5 F6     LDA ram_00F6_current_music
C - - - - - 0x003AEB 00:FADB: C9 DF     CMP #$DF
C - - - - - 0x003AED 00:FADD: D0 F9     BNE bra_FAD8_RTS
C - - - - - 0x003AEF 00:FADF: 4C 9F F7  JMP loc_F79F
bra_FAE2:
C - - - - - 0x003AF2 00:FAE2: A5 F5     LDA ram_00F5
C - - - - - 0x003AF4 00:FAE4: 29 1F     AND #$1F
C - - - - - 0x003AF6 00:FAE6: 09 40     ORA #$40
C - - - - - 0x003AF8 00:FAE8: 85 F5     STA ram_00F5
C - - - - - 0x003AFA 00:FAEA: A9 00     LDA #$00
C - - - - - 0x003AFC 00:FAEC: 8D 08 40  STA $4008
C - - - - - 0x003AFF 00:FAEF: 85 F6     STA ram_00F6_current_music
C - - - - - 0x003B01 00:FAF1: 8D F6 07  STA ram_07F6
C - - - - - 0x003B04 00:FAF4: A9 10     LDA #$10
C - - - - - 0x003B06 00:FAF6: 8D 04 40  STA $4004
C - - - - - 0x003B09 00:FAF9: 8D 0C 40  STA $400C
C - - - - - 0x003B0C 00:FAFC: A2 8E     LDX #< tbl_FA8E
C - - - - - 0x003B0E 00:FAFE: A0 FA     LDY #> tbl_FA8E
bra_FB00:
C - - - - - 0x003B10 00:FB00: 20 87 F6  JSR sub_F687
C - - - - - 0x003B13 00:FB03: 60        RTS
bra_FB04:
C - - - - - 0x003B14 00:FB04: AD E9 07  LDA ram_07E9
C - - - - - 0x003B17 00:FB07: 29 20     AND #$20
C - - - - - 0x003B19 00:FB09: D0 05     BNE bra_FB10
C - - - - - 0x003B1B 00:FB0B: A9 02     LDA #$02
C - - - - - 0x003B1D 00:FB0D: 8D F5 07  STA ram_07F5_f0_sfx_flag_trip
bra_FB10:
C - - - - - 0x003B20 00:FB10: A0 08     LDY #$08
C - - - - - 0x003B22 00:FB12: A9 DF     LDA #$DF
C - - - - - 0x003B24 00:FB14: 4C A5 FB  JMP loc_FBA5
bra_FB17:
C - - - - - 0x003B27 00:FB17: A0 04     LDY #$04
C - - - - - 0x003B29 00:FB19: A9 7F     LDA #$7F
C - - - - - 0x003B2B 00:FB1B: 4C A5 FB  JMP loc_FBA5
bra_FB1E:
; Music/Jingle: Stage Clear
C - - - - - 0x003B2E 00:FB1E: A0 00     LDY #$00
C - - - - - 0x003B30 00:FB20: A9 02     LDA #$02
C - - - - - 0x003B32 00:FB22: 4C C1 FB  JMP loc_FBC1
sub_FB25_Music:
C - - - - - 0x003B35 00:FB25: AD E8 07  LDA ram_07E8_ball_trip_music_flag                          ; \ Play Balloon Trip Music
C - - - - - 0x003B38 00:FB28: D0 34     BNE bra_FB5E                                               ; /
C - - - - - 0x003B3A 00:FB2A: A5 F2     LDA ram_00F2_music_jingle                                  ; \ Play Music/Jingle:
C - - - - - 0x003B3C 00:FB2C: 4A        LSR                                                        ; |
C - - - - - 0x003B3D 00:FB2D: B0 53     BCS bra_FB82                                               ; | #$01 = Game Over
C - - - - - 0x003B3F 00:FB2F: 4A        LSR                                                        ; |
C - - - - - 0x003B40 00:FB30: B0 EC     BCS bra_FB1E                                               ; | #$02 = Stage Clear
C - - - - - 0x003B42 00:FB32: 4A        LSR                                                        ; |
C - - - - - 0x003B43 00:FB33: B0 17     BCS bra_FB4C                                               ; | #$04 = Pause
C - - - - - 0x003B45 00:FB35: 4A        LSR                                                        ; |
C - - - - - 0x003B46 00:FB36: B0 44     BCS bra_FB7C                                               ; | #$08 = Stage Start
C - - - - - 0x003B48 00:FB38: 4A        LSR                                                        ; |
C - - - - - 0x003B49 00:FB39: B0 2E     BCS bra_FB69                                               ; | #$10 = Bonus Phase Perfect
C - - - - - 0x003B4B 00:FB3B: 4A        LSR                                                        ; |
C - - - - - 0x003B4C 00:FB3C: B0 20     BCS bra_FB5E                                               ; | #$20 = Balloon Trip / Bonus Phase Music
C - - - - - 0x003B4E 00:FB3E: 4A        LSR                                                        ; |
C - - - - - 0x003B4F 00:FB3F: B0 17     BCS bra_FB58                                               ; | #$40 = Fish
C - - - - - 0x003B51 00:FB41: 4A        LSR                                                        ; |
C - - - - - 0x003B52 00:FB42: B0 0E     BCS bra_FB52                                               ; / #$80 = Respawn
C - - - - - 0x003B54 00:FB44: A5 F6     LDA ram_00F6_current_music                                 ; \
C - - - - - 0x003B56 00:FB46: D0 01     BNE bra_FB49                                               ; / Current Music/Jingle
C - - - - - 0x003B58 00:FB48: 60        RTS
bra_FB49:
C - - - - - 0x003B59 00:FB49: 4C 04 F5  JMP loc_F504
bra_FB4C:
; Music/Jingle: Pause
C - - - - - 0x003B5C 00:FB4C: A0 02     LDY #$02
C - - - - - 0x003B5E 00:FB4E: A9 04     LDA #$04
C - - - - - 0x003B60 00:FB50: D0 53     BNE bra_FBA5
bra_FB52:
; Music/Jingle: Respawn
C - - - - - 0x003B62 00:FB52: A0 09     LDY #$09
C - - - - - 0x003B64 00:FB54: A9 80     LDA #$80
C - - - - - 0x003B66 00:FB56: D0 15     BNE bra_FB6D
bra_FB58:
; Music/Jingle: Fish
C - - - - - 0x003B68 00:FB58: A0 07     LDY #$07
C - - - - - 0x003B6A 00:FB5A: A9 40     LDA #$40
C - - - - - 0x003B6C 00:FB5C: D0 0F     BNE bra_FB6D
bra_FB5E:
; Music/Jingle: Balloon Trip / Bonus Phase
C - - - - - 0x003B6E 00:FB5E: A9 00     LDA #$00
C - - - - - 0x003B70 00:FB60: 8D E8 07  STA ram_07E8_ball_trip_music_flag
C - - - - - 0x003B73 00:FB63: A0 06     LDY #$06
C - - - - - 0x003B75 00:FB65: A9 20     LDA #$20
C - - - - - 0x003B77 00:FB67: D0 58     BNE bra_FBC1
bra_FB69:
; Music/Jingle: Bonus Phase Perfect
C - - - - - 0x003B79 00:FB69: A0 05     LDY #$05
C - - - - - 0x003B7B 00:FB6B: A9 10     LDA #$10
bra_FB6D:
C - - - - - 0x003B7D 00:FB6D: 20 A8 F6  JSR sub_F6A8_Load_Snd_Seq
C - - - - - 0x003B80 00:FB70: A2 FC     LDX #$FC
C - - - - - 0x003B82 00:FB72: A0 FC     LDY #$FC
C - - - - - 0x003B84 00:FB74: 20 12 F7  JSR sub_F712
C - - - - - 0x003B87 00:FB77: EE F0 07  INC ram_07F0_audio_related
C - - - - - 0x003B8A 00:FB7A: D0 CD     BNE bra_FB49
bra_FB7C:
; Music/Jingle: Stage Start
C - - - - - 0x003B8C 00:FB7C: A0 03     LDY #$03
C - - - - - 0x003B8E 00:FB7E: A9 08     LDA #$08
C - - - - - 0x003B90 00:FB80: D0 04     BNE bra_FB86
bra_FB82:
; Music/Jingle: Game Over
C - - - - - 0x003B92 00:FB82: A0 01     LDY #$01
C - - - - - 0x003B94 00:FB84: A9 01     LDA #$01
bra_FB86:
C - - - - - 0x003B96 00:FB86: 20 A8 F6  JSR sub_F6A8_Load_Snd_Seq
C - - - - - 0x003B99 00:FB89: A2 80     LDX #$80
C - - - - - 0x003B9B 00:FB8B: A0 80     LDY #$80
bra_FB8D:
C - - - - - 0x003B9D 00:FB8D: 20 1A F7  JSR sub_F71A
C - - - - - 0x003BA0 00:FB90: A9 83     LDA #$83                                                   ; \ Pulse 1 Channel:
C - - - - - 0x003BA2 00:FB92: 8D 01 40  STA $4001                                                  ; / Sweep, Shift = 3
C - - - - - 0x003BA5 00:FB95: A9 7F     LDA #$7F                                                   ; \ Pulse 2 Channel:
C - - - - - 0x003BA7 00:FB97: 8D 05 40  STA $4005                                                  ; / No Sweep
C - - - - - 0x003BAA 00:FB9A: D0 13     BNE bra_FBAF
- - - - - - 0x003BAC 00:FB9C: 20 A8 F6  JSR sub_F6A8_Load_Snd_Seq
- - - - - - 0x003BAF 00:FB9F: A2 04     LDX #$04
- - - - - - 0x003BB1 00:FBA1: A0 04     LDY #$04
- - - - - - 0x003BB3 00:FBA3: D0 07     BNE bra_FBAC
bra_FBA5:
loc_FBA5:
C D 3 - - - 0x003BB5 00:FBA5: 20 A8 F6  JSR sub_F6A8_Load_Snd_Seq
C - - - - - 0x003BB8 00:FBA8: A2 80     LDX #$80
C - - - - - 0x003BBA 00:FBAA: A0 80     LDY #$80
bra_FBAC:
C - - - - - 0x003BBC 00:FBAC: 20 12 F7  JSR sub_F712
bra_FBAF:
C - - - - - 0x003BBF 00:FBAF: A9 00     LDA #$00
C - - - - - 0x003BC1 00:FBB1: 8D F0 07  STA ram_07F0_audio_related
C - - - - - 0x003BC4 00:FBB4: A5 F4     LDA ram_00F4
C - - - - - 0x003BC6 00:FBB6: 29 20     AND #$20
C - - - - - 0x003BC8 00:FBB8: F0 8F     BEQ bra_FB49
C - - - - - 0x003BCA 00:FBBA: A9 D5     LDA #$D5
C - - - - - 0x003BCC 00:FBBC: 8D 01 40  STA $4001
C - - - - - 0x003BCF 00:FBBF: D0 88     BNE bra_FB49
bra_FBC1:
loc_FBC1:
C D 3 - - - 0x003BD1 00:FBC1: 20 A8 F6  JSR sub_F6A8_Load_Snd_Seq
C - - - - - 0x003BD4 00:FBC4: A2 80     LDX #$80
C - - - - - 0x003BD6 00:FBC6: A0 BA     LDY #$BA
C - - - - - 0x003BD8 00:FBC8: D0 C3     BNE bra_FB8D
tbl_FBCA:
- D 3 - - - 0x003BDA 00:FBCA: 0B        .byte $0B   ; 
- D 3 - - - 0x003BDB 00:FBCB: 14        .byte $14   ; 
- D 3 - - - 0x003BDC 00:FBCC: 1D        .byte $1D   ; 
- D 3 - - - 0x003BDD 00:FBCD: 26        .byte $26   ; 
- D 3 - - - 0x003BDE 00:FBCE: 2F        .byte $2F   ; 
- D 3 - - - 0x003BDF 00:FBCF: 38        .byte $38   ; 
- D 3 - - - 0x003BE0 00:FBD0: 41        .byte $41   ; 
- D 3 - - - 0x003BE1 00:FBD1: 4A        .byte $4A   ; 
- D 3 - - - 0x003BE2 00:FBD2: 53        .byte $53   ; 
- D 3 - - - 0x003BE3 00:FBD3: 5C        .byte $5C   ; 
- D 3 - - - 0x003BE4 00:FBD4: 65        .byte $65   ; 
- D 3 - - - 0x003BE5 00:FBD5: 0C        .byte $0C   ; 
- D 3 - - - 0x003BE6 00:FBD6: 02 FF     .word off_FF02
- D 3 - - - 0x003BE8 00:FBD8: 0B FF     .word off_FF0B
- D 3 - - - 0x003BEA 00:FBDA: 1E FF     .word off_FF1E
- D 3 - - - 0x003BEC 00:FBDC: 31 FF     .word off_FF31
- D 3 - - - 0x003BEE 00:FBDE: 15        .byte $15   ; 
- D 3 - - - 0x003BEF 00:FBDF: 18 FE     .word off_FE18
- D 3 - - - 0x003BF1 00:FBE1: 2A FE     .word off_FE2A
- D 3 - - - 0x003BF3 00:FBE3: 65 FE     .word off_FE65
- D 3 - - - 0x003BF5 00:FBE5: 86 FE     .word off_FE86
- D 3 - - - 0x003BF7 00:FBE7: 0C        .byte $0C   ; 
- D 3 - - - 0x003BF8 00:FBE8: 0D FE     .word off_FE0D
- D 3 - - - 0x003BFA 00:FBEA: 00        .byte $00   ; 
- D 3 - - - 0x003BFB 00:FBEB: 00        .byte $00   ; 
- D 3 - - - 0x003BFC 00:FBEC: 13 FE     .word off_FE13
- D 3 - - - 0x003BFE 00:FBEE: 00        .byte $00   ; 
- D 3 - - - 0x003BFF 00:FBEF: 00        .byte $00   ; 
- D 3 - - - 0x003C00 00:FBF0: 15        .byte $15   ; 
- D 3 - - - 0x003C01 00:FBF1: 38 FF     .word off_FF38
- D 3 - - - 0x003C03 00:FBF3: 5A FF     .word off_FF5A
- D 3 - - - 0x003C05 00:FBF5: 79 FF     .word off_FF79
- D 3 - - - 0x003C07 00:FBF7: 94 FF     .word off_FF94
- D 3 - - - 0x003C09 00:FBF9: 00        .byte $00   ; 
- D 3 - - - 0x003C0A 00:FBFA: 00        .byte $00   ; 
- D 3 - - - 0x003C0B 00:FBFB: 00        .byte $00   ; 
- D 3 - - - 0x003C0C 00:FBFC: D7 FE     .word off_FED7
- D 3 - - - 0x003C0E 00:FBFE: ED FE     .word off_FEED
- D 3 - - - 0x003C10 00:FC00: 00        .byte $00   ; 
- D 3 - - - 0x003C11 00:FC01: 00        .byte $00   ; 
- D 3 - - - 0x003C12 00:FC02: 00        .byte $00   ; 
- D 3 - - - 0x003C13 00:FC03: B3 FF     .word off_FFB3
- D 3 - - - 0x003C15 00:FC05: C9 FF     .word off_FFC9
- D 3 - - - 0x003C17 00:FC07: DA FF     .word off_FFDA
- D 3 - - - 0x003C19 00:FC09: EF FF     .word off_FFEF
- D 3 - - - 0x003C1B 00:FC0B: 15        .byte $15   ; 
- D 3 - - - 0x003C1C 00:FC0C: A5 FC     .word off_FCA5
- D 3 - - - 0x003C1E 00:FC0E: 0A FD     .word off_FD0A
- D 3 - - - 0x003C20 00:FC10: 98 FD     .word off_FD98
- D 3 - - - 0x003C22 00:FC12: E0 FD     .word off_FDE0
- D 3 - - - 0x003C24 00:FC14: 15        .byte $15   ; 
- D 3 - - - 0x003C25 00:FC15: B2 FE     .word off_FEB2
- D 3 - - - 0x003C27 00:FC17: 00        .byte $00   ; 
- D 3 - - - 0x003C28 00:FC18: 00        .byte $00   ; 
- D 3 - - - 0x003C29 00:FC19: C5 FE     .word off_FEC5
- D 3 - - - 0x003C2B 00:FC1B: 00        .byte $00   ; 
- D 3 - - - 0x003C2C 00:FC1C: 00        .byte $00   ; 
- D 3 - - - 0x003C2D 00:FC1D: 15        .byte $15   ; 
- D 3 - - - 0x003C2E 00:FC1E: 00        .byte $00   ; 
- D 3 - - - 0x003C2F 00:FC1F: 00        .byte $00   ; 
- D 3 - - - 0x003C30 00:FC20: 92 FE     .word off_FE92
- D 3 - - - 0x003C32 00:FC22: A1 FE     .word off_FEA1
- D 3 - - - 0x003C34 00:FC24: 00        .byte $00   ; 
- D 3 - - - 0x003C35 00:FC25: 00        .byte $00   ; 
- D 3 - - - 0x003C36 00:FC26: 0C        .byte $0C   ; 
- D 3 - - - 0x003C37 00:FC27: 59 FC     .word off_FC59
- D 3 - - - 0x003C39 00:FC29: 72 FC     .word off_FC72
- D 3 - - - 0x003C3B 00:FC2B: 8C FC     .word off_FC8C
- D 3 - - - 0x003C3D 00:FC2D: 00        .byte $00   ; 
- D 3 - - - 0x003C3E 00:FC2E: 00        .byte $00   ; 
- D 3 - - - 0x003C3F 00:FC2F: 00        .byte $00   ; 
- D 3 - - - 0x003C40 00:FC30: 00        .byte $00   ; 
- D 3 - - - 0x003C41 00:FC31: 00        .byte $00   ; 
- D 3 - - - 0x003C42 00:FC32: 38 FC     .word off_FC38
- D 3 - - - 0x003C44 00:FC34: 49 FC     .word off_FC49
- D 3 - - - 0x003C46 00:FC36: 00        .byte $00   ; 
- D 3 - - - 0x003C47 00:FC37: 00        .byte $00   ; 
off_FC38:
- D 3 - I - 0x003C48 00:FC38: 82        .byte $82   ; 
- D 3 - I - 0x003C49 00:FC39: 02        .byte $02   ; 
- D 3 - I - 0x003C4A 00:FC3A: 8B        .byte $8B   ; 
- D 3 - I - 0x003C4B 00:FC3B: 02        .byte $02   ; 
- D 3 - I - 0x003C4C 00:FC3C: 80        .byte $80   ; 
- D 3 - I - 0x003C4D 00:FC3D: 08        .byte $08   ; 
- D 3 - I - 0x003C4E 00:FC3E: 02        .byte $02   ; 
- D 3 - I - 0x003C4F 00:FC3F: 10        .byte $10   ; 
- D 3 - I - 0x003C50 00:FC40: 02        .byte $02   ; 
- D 3 - I - 0x003C51 00:FC41: 16        .byte $16   ; 
- D 3 - I - 0x003C52 00:FC42: 02        .byte $02   ; 
- D 3 - I - 0x003C53 00:FC43: 52        .byte $52   ; 
- D 3 - I - 0x003C54 00:FC44: 02        .byte $02   ; 
- D 3 - I - 0x003C55 00:FC45: 02        .byte $02   ; 
- D 3 - I - 0x003C56 00:FC46: 02        .byte $02   ; 
- D 3 - I - 0x003C57 00:FC47: 1A        .byte $1A   ; 
- D 3 - I - 0x003C58 00:FC48: 00        .byte $00   ; 
off_FC49:
- D 3 - I - 0x003C59 00:FC49: 82        .byte $82   ; 
- D 3 - I - 0x003C5A 00:FC4A: 02        .byte $02   ; 
- D 3 - I - 0x003C5B 00:FC4B: 80        .byte $80   ; 
- D 3 - I - 0x003C5C 00:FC4C: 10        .byte $10   ; 
- D 3 - I - 0x003C5D 00:FC4D: 02        .byte $02   ; 
- D 3 - I - 0x003C5E 00:FC4E: 16        .byte $16   ; 
- D 3 - I - 0x003C5F 00:FC4F: 02        .byte $02   ; 
- D 3 - I - 0x003C60 00:FC50: 52        .byte $52   ; 
- D 3 - I - 0x003C61 00:FC51: 02        .byte $02   ; 
- D 3 - I - 0x003C62 00:FC52: 5A        .byte $5A   ; 
- D 3 - I - 0x003C63 00:FC53: 02        .byte $02   ; 
- D 3 - I - 0x003C64 00:FC54: 02        .byte $02   ; 
- D 3 - I - 0x003C65 00:FC55: 02        .byte $02   ; 
- D 3 - I - 0x003C66 00:FC56: 56        .byte $56   ; 
- D 3 - I - 0x003C67 00:FC57: 81        .byte $81   ; 
- D 3 - I - 0x003C68 00:FC58: 02        .byte $02   ; 
off_FC59:
- D 3 - I - 0x003C69 00:FC59: 80        .byte $80   ; 
- D 3 - I - 0x003C6A 00:FC5A: 12        .byte $12   ; 
- D 3 - I - 0x003C6B 00:FC5B: 02        .byte $02   ; 
- D 3 - I - 0x003C6C 00:FC5C: 0C        .byte $0C   ; 
- D 3 - I - 0x003C6D 00:FC5D: 02        .byte $02   ; 
- D 3 - I - 0x003C6E 00:FC5E: 04        .byte $04   ; 
- D 3 - I - 0x003C6F 00:FC5F: 02        .byte $02   ; 
- D 3 - I - 0x003C70 00:FC60: 0C        .byte $0C   ; 
- D 3 - I - 0x003C71 00:FC61: 02        .byte $02   ; 
- D 3 - I - 0x003C72 00:FC62: 04        .byte $04   ; 
- D 3 - I - 0x003C73 00:FC63: 02        .byte $02   ; 
- D 3 - I - 0x003C74 00:FC64: 2A        .byte $2A   ; 
- D 3 - I - 0x003C75 00:FC65: 02        .byte $02   ; 
- D 3 - I - 0x003C76 00:FC66: 81        .byte $81   ; 
- D 3 - I - 0x003C77 00:FC67: 04        .byte $04   ; 
- D 3 - I - 0x003C78 00:FC68: 02        .byte $02   ; 
- D 3 - I - 0x003C79 00:FC69: 80        .byte $80   ; 
- D 3 - I - 0x003C7A 00:FC6A: 04        .byte $04   ; 
- D 3 - I - 0x003C7B 00:FC6B: 02        .byte $02   ; 
- D 3 - I - 0x003C7C 00:FC6C: 81        .byte $81   ; 
- D 3 - I - 0x003C7D 00:FC6D: 04        .byte $04   ; 
- D 3 - I - 0x003C7E 00:FC6E: 88        .byte $88   ; 
- D 3 - I - 0x003C7F 00:FC6F: 02        .byte $02   ; 
- D 3 - I - 0x003C80 00:FC70: 02        .byte $02   ; 
- D 3 - I - 0x003C81 00:FC71: 00        .byte $00   ; 
off_FC72:
- D 3 - I - 0x003C82 00:FC72: 88        .byte $88   ; 
- D 3 - I - 0x003C83 00:FC73: 02        .byte $02   ; 
- D 3 - I - 0x003C84 00:FC74: 02        .byte $02   ; 
- D 3 - I - 0x003C85 00:FC75: 80        .byte $80   ; 
- D 3 - I - 0x003C86 00:FC76: 04        .byte $04   ; 
- D 3 - I - 0x003C87 00:FC77: 02        .byte $02   ; 
- D 3 - I - 0x003C88 00:FC78: 2A        .byte $2A   ; 
- D 3 - I - 0x003C89 00:FC79: 02        .byte $02   ; 
- D 3 - I - 0x003C8A 00:FC7A: 24        .byte $24   ; 
- D 3 - I - 0x003C8B 00:FC7B: 02        .byte $02   ; 
- D 3 - I - 0x003C8C 00:FC7C: 2A        .byte $2A   ; 
- D 3 - I - 0x003C8D 00:FC7D: 02        .byte $02   ; 
- D 3 - I - 0x003C8E 00:FC7E: 24        .byte $24   ; 
- D 3 - I - 0x003C8F 00:FC7F: 02        .byte $02   ; 
- D 3 - I - 0x003C90 00:FC80: 1C        .byte $1C   ; 
- D 3 - I - 0x003C91 00:FC81: 02        .byte $02   ; 
- D 3 - I - 0x003C92 00:FC82: 81        .byte $81   ; 
- D 3 - I - 0x003C93 00:FC83: 22        .byte $22   ; 
- D 3 - I - 0x003C94 00:FC84: 02        .byte $02   ; 
- D 3 - I - 0x003C95 00:FC85: 80        .byte $80   ; 
- D 3 - I - 0x003C96 00:FC86: 22        .byte $22   ; 
- D 3 - I - 0x003C97 00:FC87: 02        .byte $02   ; 
- D 3 - I - 0x003C98 00:FC88: 81        .byte $81   ; 
- D 3 - I - 0x003C99 00:FC89: 24        .byte $24   ; 
- - - - - - 0x003C9A 00:FC8A: 88        .byte $88   ; 
- - - - - - 0x003C9B 00:FC8B: 02        .byte $02   ; 
off_FC8C:
- D 3 - I - 0x003C9C 00:FC8C: 88        .byte $88   ; 
- D 3 - I - 0x003C9D 00:FC8D: 02        .byte $02   ; 
- D 3 - I - 0x003C9E 00:FC8E: 80        .byte $80   ; 
- D 3 - I - 0x003C9F 00:FC8F: 56        .byte $56   ; 
- D 3 - I - 0x003CA0 00:FC90: 02        .byte $02   ; 
- D 3 - I - 0x003CA1 00:FC91: 4E        .byte $4E   ; 
- D 3 - I - 0x003CA2 00:FC92: 02        .byte $02   ; 
- D 3 - I - 0x003CA3 00:FC93: 12        .byte $12   ; 
- D 3 - I - 0x003CA4 00:FC94: 02        .byte $02   ; 
- D 3 - I - 0x003CA5 00:FC95: 4E        .byte $4E   ; 
- D 3 - I - 0x003CA6 00:FC96: 02        .byte $02   ; 
- D 3 - I - 0x003CA7 00:FC97: 12        .byte $12   ; 
- D 3 - I - 0x003CA8 00:FC98: 02        .byte $02   ; 
- D 3 - I - 0x003CA9 00:FC99: 0C        .byte $0C   ; 
- D 3 - I - 0x003CAA 00:FC9A: 02        .byte $02   ; 
- D 3 - I - 0x003CAB 00:FC9B: 81        .byte $81   ; 
- D 3 - I - 0x003CAC 00:FC9C: 10        .byte $10   ; 
- D 3 - I - 0x003CAD 00:FC9D: 02        .byte $02   ; 
- D 3 - I - 0x003CAE 00:FC9E: 80        .byte $80   ; 
- D 3 - I - 0x003CAF 00:FC9F: 10        .byte $10   ; 
- D 3 - I - 0x003CB0 00:FCA0: 02        .byte $02   ; 
- D 3 - I - 0x003CB1 00:FCA1: 81        .byte $81   ; 
- D 3 - I - 0x003CB2 00:FCA2: 12        .byte $12   ; 
- D 3 - I - 0x003CB3 00:FCA3: 88        .byte $88   ; 
- D 3 - I - 0x003CB4 00:FCA4: 02        .byte $02   ; 
off_FCA5:
- D 3 - I - 0x003CB5 00:FCA5: C3        .byte $C3   ; 
- D 3 - I - 0x003CB6 00:FCA6: 81        .byte $81   ; 
- D 3 - I - 0x003CB7 00:FCA7: 02        .byte $02   ; 
- D 3 - I - 0x003CB8 00:FCA8: 02        .byte $02   ; 
- D 3 - I - 0x003CB9 00:FCA9: 1C        .byte $1C   ; 
- D 3 - I - 0x003CBA 00:FCAA: 02        .byte $02   ; 
- D 3 - I - 0x003CBB 00:FCAB: 02        .byte $02   ; 
- D 3 - I - 0x003CBC 00:FCAC: 02        .byte $02   ; 
- D 3 - I - 0x003CBD 00:FCAD: 1C        .byte $1C   ; 
- D 3 - I - 0x003CBE 00:FCAE: 1C        .byte $1C   ; 
- D 3 - I - 0x003CBF 00:FCAF: FF        .byte $FF   ; 
- D 3 - I - 0x003CC0 00:FCB0: C6        .byte $C6   ; 
- D 3 - I - 0x003CC1 00:FCB1: 88        .byte $88   ; 
- D 3 - I - 0x003CC2 00:FCB2: 1C        .byte $1C   ; 
- D 3 - I - 0x003CC3 00:FCB3: FF        .byte $FF   ; 
- D 3 - I - 0x003CC4 00:FCB4: C7        .byte $C7   ; 
- D 3 - I - 0x003CC5 00:FCB5: 82        .byte $82   ; 
- D 3 - I - 0x003CC6 00:FCB6: 4C        .byte $4C   ; 
- D 3 - I - 0x003CC7 00:FCB7: 4C        .byte $4C   ; 
- D 3 - I - 0x003CC8 00:FCB8: 2A        .byte $2A   ; 
- D 3 - I - 0x003CC9 00:FCB9: 4C        .byte $4C   ; 
- D 3 - I - 0x003CCA 00:FCBA: FF        .byte $FF   ; 
- D 3 - I - 0x003CCB 00:FCBB: C6        .byte $C6   ; 
- D 3 - I - 0x003CCC 00:FCBC: 88        .byte $88   ; 
- D 3 - I - 0x003CCD 00:FCBD: 1C        .byte $1C   ; 
- D 3 - I - 0x003CCE 00:FCBE: FF        .byte $FF   ; 
- D 3 - I - 0x003CCF 00:FCBF: C4        .byte $C4   ; 
- D 3 - I - 0x003CD0 00:FCC0: 81        .byte $81   ; 
- D 3 - I - 0x003CD1 00:FCC1: 46        .byte $46   ; 
- D 3 - I - 0x003CD2 00:FCC2: 02        .byte $02   ; 
- D 3 - I - 0x003CD3 00:FCC3: 46        .byte $46   ; 
- D 3 - I - 0x003CD4 00:FCC4: 02        .byte $02   ; 
- D 3 - I - 0x003CD5 00:FCC5: 32        .byte $32   ; 
- D 3 - I - 0x003CD6 00:FCC6: 02        .byte $02   ; 
- D 3 - I - 0x003CD7 00:FCC7: 46        .byte $46   ; 
- D 3 - I - 0x003CD8 00:FCC8: 80        .byte $80   ; 
- D 3 - I - 0x003CD9 00:FCC9: 2E        .byte $2E   ; 
- D 3 - I - 0x003CDA 00:FCCA: 2E        .byte $2E   ; 
- D 3 - I - 0x003CDB 00:FCCB: FF        .byte $FF   ; 
- D 3 - I - 0x003CDC 00:FCCC: C3        .byte $C3   ; 
- D 3 - I - 0x003CDD 00:FCCD: 82        .byte $82   ; 
- D 3 - I - 0x003CDE 00:FCCE: 46        .byte $46   ; 
- D 3 - I - 0x003CDF 00:FCCF: 46        .byte $46   ; 
- D 3 - I - 0x003CE0 00:FCD0: 81        .byte $81   ; 
- D 3 - I - 0x003CE1 00:FCD1: 32        .byte $32   ; 
- D 3 - I - 0x003CE2 00:FCD2: 32        .byte $32   ; 
- D 3 - I - 0x003CE3 00:FCD3: 46        .byte $46   ; 
- D 3 - I - 0x003CE4 00:FCD4: 2E        .byte $2E   ; 
- D 3 - I - 0x003CE5 00:FCD5: FF        .byte $FF   ; 
- D 3 - I - 0x003CE6 00:FCD6: 80        .byte $80   ; 
- D 3 - I - 0x003CE7 00:FCD7: 0C        .byte $0C   ; 
- D 3 - I - 0x003CE8 00:FCD8: 0C        .byte $0C   ; 
- D 3 - I - 0x003CE9 00:FCD9: 81        .byte $81   ; 
- D 3 - I - 0x003CEA 00:FCDA: 46        .byte $46   ; 
- D 3 - I - 0x003CEB 00:FCDB: 46        .byte $46   ; 
- D 3 - I - 0x003CEC 00:FCDC: 46        .byte $46   ; 
- D 3 - I - 0x003CED 00:FCDD: 80        .byte $80   ; 
- D 3 - I - 0x003CEE 00:FCDE: 04        .byte $04   ; 
- D 3 - I - 0x003CEF 00:FCDF: 04        .byte $04   ; 
- D 3 - I - 0x003CF0 00:FCE0: 81        .byte $81   ; 
- D 3 - I - 0x003CF1 00:FCE1: 46        .byte $46   ; 
- D 3 - I - 0x003CF2 00:FCE2: 46        .byte $46   ; 
- D 3 - I - 0x003CF3 00:FCE3: 02        .byte $02   ; 
- D 3 - I - 0x003CF4 00:FCE4: C8        .byte $C8   ; 
- D 3 - I - 0x003CF5 00:FCE5: 82        .byte $82   ; 
- D 3 - I - 0x003CF6 00:FCE6: 4C        .byte $4C   ; 
- D 3 - I - 0x003CF7 00:FCE7: 4C        .byte $4C   ; 
- D 3 - I - 0x003CF8 00:FCE8: 2A        .byte $2A   ; 
- D 3 - I - 0x003CF9 00:FCE9: 4C        .byte $4C   ; 
- D 3 - I - 0x003CFA 00:FCEA: FF        .byte $FF   ; 
- D 3 - I - 0x003CFB 00:FCEB: C2        .byte $C2   ; 
- D 3 - I - 0x003CFC 00:FCEC: 81        .byte $81   ; 
- D 3 - I - 0x003CFD 00:FCED: 46        .byte $46   ; 
- D 3 - I - 0x003CFE 00:FCEE: 80        .byte $80   ; 
- D 3 - I - 0x003CFF 00:FCEF: 32        .byte $32   ; 
- D 3 - I - 0x003D00 00:FCF0: 32        .byte $32   ; 
- D 3 - I - 0x003D01 00:FCF1: 82        .byte $82   ; 
- D 3 - I - 0x003D02 00:FCF2: 46        .byte $46   ; 
- D 3 - I - 0x003D03 00:FCF3: 04        .byte $04   ; 
- D 3 - I - 0x003D04 00:FCF4: 81        .byte $81   ; 
- D 3 - I - 0x003D05 00:FCF5: 46        .byte $46   ; 
- D 3 - I - 0x003D06 00:FCF6: 2A        .byte $2A   ; 
- D 3 - I - 0x003D07 00:FCF7: FF        .byte $FF   ; 
- D 3 - I - 0x003D08 00:FCF8: C2        .byte $C2   ; 
- D 3 - I - 0x003D09 00:FCF9: 81        .byte $81   ; 
- D 3 - I - 0x003D0A 00:FCFA: 0C        .byte $0C   ; 
- D 3 - I - 0x003D0B 00:FCFB: 0C        .byte $0C   ; 
- D 3 - I - 0x003D0C 00:FCFC: 80        .byte $80   ; 
- D 3 - I - 0x003D0D 00:FCFD: 04        .byte $04   ; 
- D 3 - I - 0x003D0E 00:FCFE: 04        .byte $04   ; 
- D 3 - I - 0x003D0F 00:FCFF: 81        .byte $81   ; 
- D 3 - I - 0x003D10 00:FD00: 04        .byte $04   ; 
- D 3 - I - 0x003D11 00:FD01: 80        .byte $80   ; 
- D 3 - I - 0x003D12 00:FD02: 2E        .byte $2E   ; 
- D 3 - I - 0x003D13 00:FD03: 2E        .byte $2E   ; 
- D 3 - I - 0x003D14 00:FD04: 81        .byte $81   ; 
- D 3 - I - 0x003D15 00:FD05: 2E        .byte $2E   ; 
- D 3 - I - 0x003D16 00:FD06: 82        .byte $82   ; 
- D 3 - I - 0x003D17 00:FD07: 24        .byte $24   ; 
- D 3 - I - 0x003D18 00:FD08: FF        .byte $FF   ; 
- D 3 - I - 0x003D19 00:FD09: 00        .byte $00   ; 
off_FD0A:
- D 3 - I - 0x003D1A 00:FD0A: 81        .byte $81   ; 
- D 3 - I - 0x003D1B 00:FD0B: 32        .byte $32   ; 
- D 3 - I - 0x003D1C 00:FD0C: 02        .byte $02   ; 
- D 3 - I - 0x003D1D 00:FD0D: 02        .byte $02   ; 
- D 3 - I - 0x003D1E 00:FD0E: 06        .byte $06   ; 
- D 3 - I - 0x003D1F 00:FD0F: 0C        .byte $0C   ; 
- D 3 - I - 0x003D20 00:FD10: 32        .byte $32   ; 
- D 3 - I - 0x003D21 00:FD11: 02        .byte $02   ; 
- D 3 - I - 0x003D22 00:FD12: 02        .byte $02   ; 
- D 3 - I - 0x003D23 00:FD13: 8A        .byte $8A   ; 
- D 3 - I - 0x003D24 00:FD14: 2E        .byte $2E   ; 
- D 3 - I - 0x003D25 00:FD15: 8B        .byte $8B   ; 
- D 3 - I - 0x003D26 00:FD16: 02        .byte $02   ; 
- D 3 - I - 0x003D27 00:FD17: 8A        .byte $8A   ; 
- D 3 - I - 0x003D28 00:FD18: 2E        .byte $2E   ; 
- D 3 - I - 0x003D29 00:FD19: 8B        .byte $8B   ; 
- D 3 - I - 0x003D2A 00:FD1A: 02        .byte $02   ; 
- D 3 - I - 0x003D2B 00:FD1B: 8A        .byte $8A   ; 
- D 3 - I - 0x003D2C 00:FD1C: 2E        .byte $2E   ; 
- D 3 - I - 0x003D2D 00:FD1D: 8B        .byte $8B   ; 
- D 3 - I - 0x003D2E 00:FD1E: 02        .byte $02   ; 
- D 3 - I - 0x003D2F 00:FD1F: 88        .byte $88   ; 
- D 3 - I - 0x003D30 00:FD20: 2E        .byte $2E   ; 
- D 3 - I - 0x003D31 00:FD21: 32        .byte $32   ; 
- D 3 - I - 0x003D32 00:FD22: 2E        .byte $2E   ; 
- D 3 - I - 0x003D33 00:FD23: D0        .byte $D0   ; 
- D 3 - I - 0x003D34 00:FD24: 8C        .byte $8C   ; 
- D 3 - I - 0x003D35 00:FD25: 2C        .byte $2C   ; 
- D 3 - I - 0x003D36 00:FD26: 24        .byte $24   ; 
- D 3 - I - 0x003D37 00:FD27: FF        .byte $FF   ; 
- D 3 - I - 0x003D38 00:FD28: D0        .byte $D0   ; 
- D 3 - I - 0x003D39 00:FD29: 2E        .byte $2E   ; 
- D 3 - I - 0x003D3A 00:FD2A: 20        .byte $20   ; 
- D 3 - I - 0x003D3B 00:FD2B: FF        .byte $FF   ; 
- D 3 - I - 0x003D3C 00:FD2C: C3        .byte $C3   ; 
- D 3 - I - 0x003D3D 00:FD2D: 80        .byte $80   ; 
- D 3 - I - 0x003D3E 00:FD2E: 28        .byte $28   ; 
- D 3 - I - 0x003D3F 00:FD2F: 02        .byte $02   ; 
- D 3 - I - 0x003D40 00:FD30: 82        .byte $82   ; 
- D 3 - I - 0x003D41 00:FD31: 02        .byte $02   ; 
- D 3 - I - 0x003D42 00:FD32: 80        .byte $80   ; 
- D 3 - I - 0x003D43 00:FD33: 2C        .byte $2C   ; 
- D 3 - I - 0x003D44 00:FD34: 02        .byte $02   ; 
- D 3 - I - 0x003D45 00:FD35: 32        .byte $32   ; 
- D 3 - I - 0x003D46 00:FD36: 02        .byte $02   ; 
- D 3 - I - 0x003D47 00:FD37: 24        .byte $24   ; 
- D 3 - I - 0x003D48 00:FD38: 02        .byte $02   ; 
- D 3 - I - 0x003D49 00:FD39: 82        .byte $82   ; 
- D 3 - I - 0x003D4A 00:FD3A: 02        .byte $02   ; 
- D 3 - I - 0x003D4B 00:FD3B: 81        .byte $81   ; 
- D 3 - I - 0x003D4C 00:FD3C: 02        .byte $02   ; 
- D 3 - I - 0x003D4D 00:FD3D: 80        .byte $80   ; 
- D 3 - I - 0x003D4E 00:FD3E: 28        .byte $28   ; 
- D 3 - I - 0x003D4F 00:FD3F: 02        .byte $02   ; 
- D 3 - I - 0x003D50 00:FD40: 06        .byte $06   ; 
- D 3 - I - 0x003D51 00:FD41: 02        .byte $02   ; 
- D 3 - I - 0x003D52 00:FD42: 28        .byte $28   ; 
- D 3 - I - 0x003D53 00:FD43: 02        .byte $02   ; 
- D 3 - I - 0x003D54 00:FD44: 81        .byte $81   ; 
- D 3 - I - 0x003D55 00:FD45: 02        .byte $02   ; 
- D 3 - I - 0x003D56 00:FD46: 80        .byte $80   ; 
- D 3 - I - 0x003D57 00:FD47: 24        .byte $24   ; 
- D 3 - I - 0x003D58 00:FD48: 02        .byte $02   ; 
- D 3 - I - 0x003D59 00:FD49: 32        .byte $32   ; 
- D 3 - I - 0x003D5A 00:FD4A: 02        .byte $02   ; 
- D 3 - I - 0x003D5B 00:FD4B: 24        .byte $24   ; 
- D 3 - I - 0x003D5C 00:FD4C: 02        .byte $02   ; 
- D 3 - I - 0x003D5D 00:FD4D: FF        .byte $FF   ; 
- D 3 - I - 0x003D5E 00:FD4E: 80        .byte $80   ; 
- D 3 - I - 0x003D5F 00:FD4F: 28        .byte $28   ; 
- D 3 - I - 0x003D60 00:FD50: 02        .byte $02   ; 
- D 3 - I - 0x003D61 00:FD51: 82        .byte $82   ; 
- D 3 - I - 0x003D62 00:FD52: 02        .byte $02   ; 
- D 3 - I - 0x003D63 00:FD53: 80        .byte $80   ; 
- D 3 - I - 0x003D64 00:FD54: 2C        .byte $2C   ; 
- D 3 - I - 0x003D65 00:FD55: 02        .byte $02   ; 
- D 3 - I - 0x003D66 00:FD56: 32        .byte $32   ; 
- D 3 - I - 0x003D67 00:FD57: 02        .byte $02   ; 
- D 3 - I - 0x003D68 00:FD58: 24        .byte $24   ; 
- D 3 - I - 0x003D69 00:FD59: 02        .byte $02   ; 
- D 3 - I - 0x003D6A 00:FD5A: 82        .byte $82   ; 
- D 3 - I - 0x003D6B 00:FD5B: 02        .byte $02   ; 
- D 3 - I - 0x003D6C 00:FD5C: 89        .byte $89   ; 
- D 3 - I - 0x003D6D 00:FD5D: 0C        .byte $0C   ; 
- D 3 - I - 0x003D6E 00:FD5E: 0A        .byte $0A   ; 
- D 3 - I - 0x003D6F 00:FD5F: 08        .byte $08   ; 
- D 3 - I - 0x003D70 00:FD60: 06        .byte $06   ; 
- D 3 - I - 0x003D71 00:FD61: 32        .byte $32   ; 
- D 3 - I - 0x003D72 00:FD62: 30        .byte $30   ; 
- D 3 - I - 0x003D73 00:FD63: 2E        .byte $2E   ; 
- D 3 - I - 0x003D74 00:FD64: 2C        .byte $2C   ; 
- D 3 - I - 0x003D75 00:FD65: 2A        .byte $2A   ; 
- D 3 - I - 0x003D76 00:FD66: 28        .byte $28   ; 
- D 3 - I - 0x003D77 00:FD67: 26        .byte $26   ; 
- D 3 - I - 0x003D78 00:FD68: 24        .byte $24   ; 
- D 3 - I - 0x003D79 00:FD69: 02        .byte $02   ; 
- D 3 - I - 0x003D7A 00:FD6A: 02        .byte $02   ; 
- D 3 - I - 0x003D7B 00:FD6B: 02        .byte $02   ; 
- D 3 - I - 0x003D7C 00:FD6C: 86        .byte $86   ; 
- D 3 - I - 0x003D7D 00:FD6D: 02        .byte $02   ; 
- D 3 - I - 0x003D7E 00:FD6E: C7        .byte $C7   ; 
- D 3 - I - 0x003D7F 00:FD6F: 84        .byte $84   ; 
- D 3 - I - 0x003D80 00:FD70: 02        .byte $02   ; 
- D 3 - I - 0x003D81 00:FD71: FF        .byte $FF   ; 
- D 3 - I - 0x003D82 00:FD72: C4        .byte $C4   ; 
- D 3 - I - 0x003D83 00:FD73: 80        .byte $80   ; 
- D 3 - I - 0x003D84 00:FD74: 28        .byte $28   ; 
- D 3 - I - 0x003D85 00:FD75: 02        .byte $02   ; 
- D 3 - I - 0x003D86 00:FD76: 82        .byte $82   ; 
- D 3 - I - 0x003D87 00:FD77: 02        .byte $02   ; 
- D 3 - I - 0x003D88 00:FD78: 80        .byte $80   ; 
- D 3 - I - 0x003D89 00:FD79: 2C        .byte $2C   ; 
- D 3 - I - 0x003D8A 00:FD7A: 02        .byte $02   ; 
- D 3 - I - 0x003D8B 00:FD7B: 32        .byte $32   ; 
- D 3 - I - 0x003D8C 00:FD7C: 02        .byte $02   ; 
- D 3 - I - 0x003D8D 00:FD7D: 24        .byte $24   ; 
- D 3 - I - 0x003D8E 00:FD7E: 02        .byte $02   ; 
- D 3 - I - 0x003D8F 00:FD7F: 82        .byte $82   ; 
- D 3 - I - 0x003D90 00:FD80: 02        .byte $02   ; 
- D 3 - I - 0x003D91 00:FD81: 81        .byte $81   ; 
- D 3 - I - 0x003D92 00:FD82: 02        .byte $02   ; 
- D 3 - I - 0x003D93 00:FD83: 80        .byte $80   ; 
- D 3 - I - 0x003D94 00:FD84: 28        .byte $28   ; 
- D 3 - I - 0x003D95 00:FD85: 02        .byte $02   ; 
- D 3 - I - 0x003D96 00:FD86: 06        .byte $06   ; 
- D 3 - I - 0x003D97 00:FD87: 02        .byte $02   ; 
- D 3 - I - 0x003D98 00:FD88: 28        .byte $28   ; 
- D 3 - I - 0x003D99 00:FD89: 02        .byte $02   ; 
- D 3 - I - 0x003D9A 00:FD8A: 81        .byte $81   ; 
- D 3 - I - 0x003D9B 00:FD8B: 02        .byte $02   ; 
- D 3 - I - 0x003D9C 00:FD8C: 80        .byte $80   ; 
- D 3 - I - 0x003D9D 00:FD8D: 24        .byte $24   ; 
- D 3 - I - 0x003D9E 00:FD8E: 02        .byte $02   ; 
- D 3 - I - 0x003D9F 00:FD8F: 32        .byte $32   ; 
- D 3 - I - 0x003DA0 00:FD90: 02        .byte $02   ; 
- D 3 - I - 0x003DA1 00:FD91: 24        .byte $24   ; 
- D 3 - I - 0x003DA2 00:FD92: 02        .byte $02   ; 
- D 3 - I - 0x003DA3 00:FD93: FF        .byte $FF   ; 
- D 3 - I - 0x003DA4 00:FD94: C8        .byte $C8   ; 
- D 3 - I - 0x003DA5 00:FD95: 84        .byte $84   ; 
- D 3 - I - 0x003DA6 00:FD96: 02        .byte $02   ; 
- D 3 - I - 0x003DA7 00:FD97: FF        .byte $FF   ; 
off_FD98:
- D 3 - I - 0x003DA8 00:FD98: 81        .byte $81   ; 
- D 3 - I - 0x003DA9 00:FD99: 14        .byte $14   ; 
- D 3 - I - 0x003DAA 00:FD9A: 02        .byte $02   ; 
- D 3 - I - 0x003DAB 00:FD9B: 02        .byte $02   ; 
- D 3 - I - 0x003DAC 00:FD9C: 14        .byte $14   ; 
- D 3 - I - 0x003DAD 00:FD9D: 1A        .byte $1A   ; 
- D 3 - I - 0x003DAE 00:FD9E: 14        .byte $14   ; 
- D 3 - I - 0x003DAF 00:FD9F: 02        .byte $02   ; 
- D 3 - I - 0x003DB0 00:FDA0: 02        .byte $02   ; 
- D 3 - I - 0x003DB1 00:FDA1: 88        .byte $88   ; 
- D 3 - I - 0x003DB2 00:FDA2: 10        .byte $10   ; 
- D 3 - I - 0x003DB3 00:FDA3: 10        .byte $10   ; 
- D 3 - I - 0x003DB4 00:FDA4: 10        .byte $10   ; 
- D 3 - I - 0x003DB5 00:FDA5: 10        .byte $10   ; 
- D 3 - I - 0x003DB6 00:FDA6: 14        .byte $14   ; 
- D 3 - I - 0x003DB7 00:FDA7: 10        .byte $10   ; 
- D 3 - I - 0x003DB8 00:FDA8: 85        .byte $85   ; 
- D 3 - I - 0x003DB9 00:FDA9: 3C        .byte $3C   ; 
- D 3 - I - 0x003DBA 00:FDAA: 81        .byte $81   ; 
- D 3 - I - 0x003DBB 00:FDAB: 44        .byte $44   ; 
- D 3 - I - 0x003DBC 00:FDAC: 85        .byte $85   ; 
- D 3 - I - 0x003DBD 00:FDAD: 4A        .byte $4A   ; 
- D 3 - I - 0x003DBE 00:FDAE: 81        .byte $81   ; 
- D 3 - I - 0x003DBF 00:FDAF: 44        .byte $44   ; 
- D 3 - I - 0x003DC0 00:FDB0: 88        .byte $88   ; 
- D 3 - I - 0x003DC1 00:FDB1: 28        .byte $28   ; 
- D 3 - I - 0x003DC2 00:FDB2: 24        .byte $24   ; 
- D 3 - I - 0x003DC3 00:FDB3: 20        .byte $20   ; 
- D 3 - I - 0x003DC4 00:FDB4: 46        .byte $46   ; 
- D 3 - I - 0x003DC5 00:FDB5: 42        .byte $42   ; 
- D 3 - I - 0x003DC6 00:FDB6: 40        .byte $40   ; 
- D 3 - I - 0x003DC7 00:FDB7: C6        .byte $C6   ; 
- D 3 - I - 0x003DC8 00:FDB8: 81        .byte $81   ; 
- D 3 - I - 0x003DC9 00:FDB9: 3C        .byte $3C   ; 
- D 3 - I - 0x003DCA 00:FDBA: 02        .byte $02   ; 
- D 3 - I - 0x003DCB 00:FDBB: 02        .byte $02   ; 
- D 3 - I - 0x003DCC 00:FDBC: 44        .byte $44   ; 
- D 3 - I - 0x003DCD 00:FDBD: 02        .byte $02   ; 
- D 3 - I - 0x003DCE 00:FDBE: 02        .byte $02   ; 
- D 3 - I - 0x003DCF 00:FDBF: 02        .byte $02   ; 
- D 3 - I - 0x003DD0 00:FDC0: 4A        .byte $4A   ; 
- D 3 - I - 0x003DD1 00:FDC1: 02        .byte $02   ; 
- D 3 - I - 0x003DD2 00:FDC2: 46        .byte $46   ; 
- D 3 - I - 0x003DD3 00:FDC3: 36        .byte $36   ; 
- D 3 - I - 0x003DD4 00:FDC4: 36        .byte $36   ; 
- D 3 - I - 0x003DD5 00:FDC5: 38        .byte $38   ; 
- D 3 - I - 0x003DD6 00:FDC6: 38        .byte $38   ; 
- D 3 - I - 0x003DD7 00:FDC7: 02        .byte $02   ; 
- D 3 - I - 0x003DD8 00:FDC8: 3A        .byte $3A   ; 
- D 3 - I - 0x003DD9 00:FDC9: 02        .byte $02   ; 
- D 3 - I - 0x003DDA 00:FDCA: 80        .byte $80   ; 
- D 3 - I - 0x003DDB 00:FDCB: 3C        .byte $3C   ; 
- D 3 - I - 0x003DDC 00:FDCC: 3C        .byte $3C   ; 
- D 3 - I - 0x003DDD 00:FDCD: 81        .byte $81   ; 
- D 3 - I - 0x003DDE 00:FDCE: 02        .byte $02   ; 
- D 3 - I - 0x003DDF 00:FDCF: 24        .byte $24   ; 
- D 3 - I - 0x003DE0 00:FDD0: 02        .byte $02   ; 
- D 3 - I - 0x003DE1 00:FDD1: 02        .byte $02   ; 
- D 3 - I - 0x003DE2 00:FDD2: 2C        .byte $2C   ; 
- D 3 - I - 0x003DE3 00:FDD3: 24        .byte $24   ; 
- D 3 - I - 0x003DE4 00:FDD4: 88        .byte $88   ; 
- D 3 - I - 0x003DE5 00:FDD5: 24        .byte $24   ; 
- D 3 - I - 0x003DE6 00:FDD6: 1E        .byte $1E   ; 
- D 3 - I - 0x003DE7 00:FDD7: 46        .byte $46   ; 
- D 3 - I - 0x003DE8 00:FDD8: 36        .byte $36   ; 
- D 3 - I - 0x003DE9 00:FDD9: 38        .byte $38   ; 
- D 3 - I - 0x003DEA 00:FDDA: 3A        .byte $3A   ; 
- D 3 - I - 0x003DEB 00:FDDB: FF        .byte $FF   ; 
- D 3 - I - 0x003DEC 00:FDDC: C4        .byte $C4   ; 
- D 3 - I - 0x003DED 00:FDDD: 84        .byte $84   ; 
- D 3 - I - 0x003DEE 00:FDDE: 02        .byte $02   ; 
- D 3 - I - 0x003DEF 00:FDDF: FF        .byte $FF   ; 
off_FDE0:
- D 3 - I - 0x003DF0 00:FDE0: D8        .byte $D8   ; 
- D 3 - I - 0x003DF1 00:FDE1: 81        .byte $81   ; 
- D 3 - I - 0x003DF2 00:FDE2: 06        .byte $06   ; 
- D 3 - I - 0x003DF3 00:FDE3: FF        .byte $FF   ; 
- D 3 - I - 0x003DF4 00:FDE4: C6        .byte $C6   ; 
- D 3 - I - 0x003DF5 00:FDE5: 88        .byte $88   ; 
- D 3 - I - 0x003DF6 00:FDE6: 06        .byte $06   ; 
- D 3 - I - 0x003DF7 00:FDE7: FF        .byte $FF   ; 
- D 3 - I - 0x003DF8 00:FDE8: C7        .byte $C7   ; 
- D 3 - I - 0x003DF9 00:FDE9: 81        .byte $81   ; 
- D 3 - I - 0x003DFA 00:FDEA: 06        .byte $06   ; 
- D 3 - I - 0x003DFB 00:FDEB: 06        .byte $06   ; 
- D 3 - I - 0x003DFC 00:FDEC: 80        .byte $80   ; 
- D 3 - I - 0x003DFD 00:FDED: 06        .byte $06   ; 
- D 3 - I - 0x003DFE 00:FDEE: 06        .byte $06   ; 
- D 3 - I - 0x003DFF 00:FDEF: 81        .byte $81   ; 
- D 3 - I - 0x003E00 00:FDF0: 06        .byte $06   ; 
- D 3 - I - 0x003E01 00:FDF1: 06        .byte $06   ; 
- D 3 - I - 0x003E02 00:FDF2: 80        .byte $80   ; 
- D 3 - I - 0x003E03 00:FDF3: 06        .byte $06   ; 
- D 3 - I - 0x003E04 00:FDF4: 06        .byte $06   ; 
- D 3 - I - 0x003E05 00:FDF5: 81        .byte $81   ; 
- D 3 - I - 0x003E06 00:FDF6: 06        .byte $06   ; 
- D 3 - I - 0x003E07 00:FDF7: 06        .byte $06   ; 
- D 3 - I - 0x003E08 00:FDF8: FF        .byte $FF   ; 
- D 3 - I - 0x003E09 00:FDF9: C6        .byte $C6   ; 
- D 3 - I - 0x003E0A 00:FDFA: 88        .byte $88   ; 
- D 3 - I - 0x003E0B 00:FDFB: 06        .byte $06   ; 
- D 3 - I - 0x003E0C 00:FDFC: FF        .byte $FF   ; 
- D 3 - I - 0x003E0D 00:FDFD: E0        .byte $E0   ; 
- D 3 - I - 0x003E0E 00:FDFE: 81        .byte $81   ; 
- D 3 - I - 0x003E0F 00:FDFF: 06        .byte $06   ; 
- D 3 - I - 0x003E10 00:FE00: 06        .byte $06   ; 
- D 3 - I - 0x003E11 00:FE01: FF        .byte $FF   ; 
- D 3 - I - 0x003E12 00:FE02: 82        .byte $82   ; 
- D 3 - I - 0x003E13 00:FE03: 0F        .byte $0F   ; 
- D 3 - I - 0x003E14 00:FE04: 81        .byte $81   ; 
- D 3 - I - 0x003E15 00:FE05: 06        .byte $06   ; 
- D 3 - I - 0x003E16 00:FE06: 06        .byte $06   ; 
- D 3 - I - 0x003E17 00:FE07: EA        .byte $EA   ; 
- D 3 - I - 0x003E18 00:FE08: 06        .byte $06   ; 
- D 3 - I - 0x003E19 00:FE09: 06        .byte $06   ; 
- D 3 - I - 0x003E1A 00:FE0A: 06        .byte $06   ; 
- D 3 - I - 0x003E1B 00:FE0B: 06        .byte $06   ; 
- D 3 - I - 0x003E1C 00:FE0C: FF        .byte $FF   ; 
off_FE0D:
- D 3 - I - 0x003E1D 00:FE0D: C5        .byte $C5   ; 
- D 3 - I - 0x003E1E 00:FE0E: 80        .byte $80   ; 
- D 3 - I - 0x003E1F 00:FE0F: 0E        .byte $0E   ; 
- D 3 - I - 0x003E20 00:FE10: 58        .byte $58   ; 
- D 3 - I - 0x003E21 00:FE11: FF        .byte $FF   ; 
- D 3 - I - 0x003E22 00:FE12: 00        .byte $00   ; 
off_FE13:
- D 3 - I - 0x003E23 00:FE13: C5        .byte $C5   ; 
- D 3 - I - 0x003E24 00:FE14: 80        .byte $80   ; 
- D 3 - I - 0x003E25 00:FE15: 0E        .byte $0E   ; 
- D 3 - I - 0x003E26 00:FE16: 58        .byte $58   ; 
- D 3 - I - 0x003E27 00:FE17: FF        .byte $FF   ; 
off_FE18:
- D 3 - I - 0x003E28 00:FE18: 82        .byte $82   ; 
- D 3 - I - 0x003E29 00:FE19: 1C        .byte $1C   ; 
- D 3 - I - 0x003E2A 00:FE1A: 1C        .byte $1C   ; 
- D 3 - I - 0x003E2B 00:FE1B: C3        .byte $C3   ; 
- D 3 - I - 0x003E2C 00:FE1C: 82        .byte $82   ; 
- D 3 - I - 0x003E2D 00:FE1D: 1C        .byte $1C   ; 
- D 3 - I - 0x003E2E 00:FE1E: 1C        .byte $1C   ; 
- D 3 - I - 0x003E2F 00:FE1F: 81        .byte $81   ; 
- D 3 - I - 0x003E30 00:FE20: 1C        .byte $1C   ; 
- D 3 - I - 0x003E31 00:FE21: 1C        .byte $1C   ; 
- D 3 - I - 0x003E32 00:FE22: 1C        .byte $1C   ; 
- D 3 - I - 0x003E33 00:FE23: 02        .byte $02   ; 
- D 3 - I - 0x003E34 00:FE24: FF        .byte $FF   ; 
- D 3 - I - 0x003E35 00:FE25: C7        .byte $C7   ; 
- D 3 - I - 0x003E36 00:FE26: 88        .byte $88   ; 
- D 3 - I - 0x003E37 00:FE27: 1C        .byte $1C   ; 
- D 3 - I - 0x003E38 00:FE28: FF        .byte $FF   ; 
- D 3 - I - 0x003E39 00:FE29: 00        .byte $00   ; 
off_FE2A:
- D 3 - I - 0x003E3A 00:FE2A: 83        .byte $83   ; 
- D 3 - I - 0x003E3B 00:FE2B: 02        .byte $02   ; 
- D 3 - I - 0x003E3C 00:FE2C: 80        .byte $80   ; 
- D 3 - I - 0x003E3D 00:FE2D: 0E        .byte $0E   ; 
- D 3 - I - 0x003E3E 00:FE2E: 02        .byte $02   ; 
- D 3 - I - 0x003E3F 00:FE2F: 0E        .byte $0E   ; 
- D 3 - I - 0x003E40 00:FE30: 02        .byte $02   ; 
- D 3 - I - 0x003E41 00:FE31: 0C        .byte $0C   ; 
- D 3 - I - 0x003E42 00:FE32: 02        .byte $02   ; 
- D 3 - I - 0x003E43 00:FE33: 0E        .byte $0E   ; 
- D 3 - I - 0x003E44 00:FE34: 02        .byte $02   ; 
- D 3 - I - 0x003E45 00:FE35: 4E        .byte $4E   ; 
- D 3 - I - 0x003E46 00:FE36: 02        .byte $02   ; 
- D 3 - I - 0x003E47 00:FE37: 02        .byte $02   ; 
- D 3 - I - 0x003E48 00:FE38: 02        .byte $02   ; 
- D 3 - I - 0x003E49 00:FE39: 0E        .byte $0E   ; 
- D 3 - I - 0x003E4A 00:FE3A: 02        .byte $02   ; 
- D 3 - I - 0x003E4B 00:FE3B: 0C        .byte $0C   ; 
- D 3 - I - 0x003E4C 00:FE3C: 02        .byte $02   ; 
- D 3 - I - 0x003E4D 00:FE3D: 02        .byte $02   ; 
- D 3 - I - 0x003E4E 00:FE3E: 02        .byte $02   ; 
- D 3 - I - 0x003E4F 00:FE3F: 0E        .byte $0E   ; 
- D 3 - I - 0x003E50 00:FE40: 02        .byte $02   ; 
- D 3 - I - 0x003E51 00:FE41: 0C        .byte $0C   ; 
- D 3 - I - 0x003E52 00:FE42: 02        .byte $02   ; 
- D 3 - I - 0x003E53 00:FE43: 0E        .byte $0E   ; 
- D 3 - I - 0x003E54 00:FE44: 02        .byte $02   ; 
- D 3 - I - 0x003E55 00:FE45: 4E        .byte $4E   ; 
- D 3 - I - 0x003E56 00:FE46: 02        .byte $02   ; 
- D 3 - I - 0x003E57 00:FE47: 02        .byte $02   ; 
- D 3 - I - 0x003E58 00:FE48: 02        .byte $02   ; 
- D 3 - I - 0x003E59 00:FE49: 0E        .byte $0E   ; 
- D 3 - I - 0x003E5A 00:FE4A: 02        .byte $02   ; 
- D 3 - I - 0x003E5B 00:FE4B: 0C        .byte $0C   ; 
- D 3 - I - 0x003E5C 00:FE4C: 02        .byte $02   ; 
- D 3 - I - 0x003E5D 00:FE4D: 0E        .byte $0E   ; 
- D 3 - I - 0x003E5E 00:FE4E: 02        .byte $02   ; 
- D 3 - I - 0x003E5F 00:FE4F: 0E        .byte $0E   ; 
- D 3 - I - 0x003E60 00:FE50: 02        .byte $02   ; 
- D 3 - I - 0x003E61 00:FE51: 0C        .byte $0C   ; 
- D 3 - I - 0x003E62 00:FE52: 02        .byte $02   ; 
- D 3 - I - 0x003E63 00:FE53: 0E        .byte $0E   ; 
- D 3 - I - 0x003E64 00:FE54: 02        .byte $02   ; 
- D 3 - I - 0x003E65 00:FE55: 4E        .byte $4E   ; 
- D 3 - I - 0x003E66 00:FE56: 02        .byte $02   ; 
- D 3 - I - 0x003E67 00:FE57: 02        .byte $02   ; 
- D 3 - I - 0x003E68 00:FE58: 02        .byte $02   ; 
- D 3 - I - 0x003E69 00:FE59: 0E        .byte $0E   ; 
- D 3 - I - 0x003E6A 00:FE5A: 02        .byte $02   ; 
- D 3 - I - 0x003E6B 00:FE5B: 0C        .byte $0C   ; 
- D 3 - I - 0x003E6C 00:FE5C: 02        .byte $02   ; 
- D 3 - I - 0x003E6D 00:FE5D: 88        .byte $88   ; 
- D 3 - I - 0x003E6E 00:FE5E: 4E        .byte $4E   ; 
- D 3 - I - 0x003E6F 00:FE5F: 18        .byte $18   ; 
- D 3 - I - 0x003E70 00:FE60: 16        .byte $16   ; 
- D 3 - I - 0x003E71 00:FE61: 12        .byte $12   ; 
- D 3 - I - 0x003E72 00:FE62: 0E        .byte $0E   ; 
- D 3 - I - 0x003E73 00:FE63: 0C        .byte $0C   ; 
- D 3 - I - 0x003E74 00:FE64: 0E        .byte $0E   ; 
off_FE65:
- D 3 - I - 0x003E75 00:FE65: 83        .byte $83   ; 
- D 3 - I - 0x003E76 00:FE66: 02        .byte $02   ; 
- D 3 - I - 0x003E77 00:FE67: 81        .byte $81   ; 
- D 3 - I - 0x003E78 00:FE68: 3E        .byte $3E   ; 
- D 3 - I - 0x003E79 00:FE69: 3E        .byte $3E   ; 
- D 3 - I - 0x003E7A 00:FE6A: 82        .byte $82   ; 
- D 3 - I - 0x003E7B 00:FE6B: 46        .byte $46   ; 
- D 3 - I - 0x003E7C 00:FE6C: 1C        .byte $1C   ; 
- D 3 - I - 0x003E7D 00:FE6D: 46        .byte $46   ; 
- D 3 - I - 0x003E7E 00:FE6E: 81        .byte $81   ; 
- D 3 - I - 0x003E7F 00:FE6F: 02        .byte $02   ; 
- D 3 - I - 0x003E80 00:FE70: 38        .byte $38   ; 
- D 3 - I - 0x003E81 00:FE71: 3E        .byte $3E   ; 
- D 3 - I - 0x003E82 00:FE72: 02        .byte $02   ; 
- D 3 - I - 0x003E83 00:FE73: 82        .byte $82   ; 
- D 3 - I - 0x003E84 00:FE74: 46        .byte $46   ; 
- D 3 - I - 0x003E85 00:FE75: 1C        .byte $1C   ; 
- D 3 - I - 0x003E86 00:FE76: 82        .byte $82   ; 
- D 3 - I - 0x003E87 00:FE77: 48        .byte $48   ; 
- D 3 - I - 0x003E88 00:FE78: 48        .byte $48   ; 
- D 3 - I - 0x003E89 00:FE79: 81        .byte $81   ; 
- D 3 - I - 0x003E8A 00:FE7A: 3E        .byte $3E   ; 
- D 3 - I - 0x003E8B 00:FE7B: 3E        .byte $3E   ; 
- D 3 - I - 0x003E8C 00:FE7C: 82        .byte $82   ; 
- D 3 - I - 0x003E8D 00:FE7D: 38        .byte $38   ; 
- D 3 - I - 0x003E8E 00:FE7E: 88        .byte $88   ; 
- D 3 - I - 0x003E8F 00:FE7F: 24        .byte $24   ; 
- D 3 - I - 0x003E90 00:FE80: 20        .byte $20   ; 
- D 3 - I - 0x003E91 00:FE81: 1C        .byte $1C   ; 
- D 3 - I - 0x003E92 00:FE82: 48        .byte $48   ; 
- D 3 - I - 0x003E93 00:FE83: 46        .byte $46   ; 
- D 3 - I - 0x003E94 00:FE84: 42        .byte $42   ; 
- D 3 - I - 0x003E95 00:FE85: 3E        .byte $3E   ; 
off_FE86:
- D 3 - I - 0x003E96 00:FE86: 82        .byte $82   ; 
- D 3 - I - 0x003E97 00:FE87: 09        .byte $09   ; 
- D 3 - I - 0x003E98 00:FE88: 09        .byte $09   ; 
- D 3 - I - 0x003E99 00:FE89: C6        .byte $C6   ; 
- D 3 - I - 0x003E9A 00:FE8A: 82        .byte $82   ; 
- D 3 - I - 0x003E9B 00:FE8B: 03        .byte $03   ; 
- D 3 - I - 0x003E9C 00:FE8C: 0C        .byte $0C   ; 
- D 3 - I - 0x003E9D 00:FE8D: FF        .byte $FF   ; 
- D 3 - I - 0x003E9E 00:FE8E: C6        .byte $C6   ; 
- D 3 - I - 0x003E9F 00:FE8F: 88        .byte $88   ; 
- D 3 - I - 0x003EA0 00:FE90: 06        .byte $06   ; 
- D 3 - I - 0x003EA1 00:FE91: FF        .byte $FF   ; 
off_FE92:
- D 3 - I - 0x003EA2 00:FE92: ED        .byte $ED   ; 
- D 3 - I - 0x003EA3 00:FE93: 89        .byte $89   ; 
- D 3 - I - 0x003EA4 00:FE94: 2A        .byte $2A   ; 
- D 3 - I - 0x003EA5 00:FE95: 02        .byte $02   ; 
- D 3 - I - 0x003EA6 00:FE96: 04        .byte $04   ; 
- D 3 - I - 0x003EA7 00:FE97: 0C        .byte $0C   ; 
- D 3 - I - 0x003EA8 00:FE98: 02        .byte $02   ; 
- D 3 - I - 0x003EA9 00:FE99: 04        .byte $04   ; 
- D 3 - I - 0x003EAA 00:FE9A: 08        .byte $08   ; 
- D 3 - I - 0x003EAB 00:FE9B: 02        .byte $02   ; 
- D 3 - I - 0x003EAC 00:FE9C: 30        .byte $30   ; 
- D 3 - I - 0x003EAD 00:FE9D: 26        .byte $26   ; 
- D 3 - I - 0x003EAE 00:FE9E: 02        .byte $02   ; 
- D 3 - I - 0x003EAF 00:FE9F: 30        .byte $30   ; 
- D 3 - I - 0x003EB0 00:FEA0: FF        .byte $FF   ; 
off_FEA1:
- D 3 - I - 0x003EB1 00:FEA1: 80        .byte $80   ; 
- D 3 - I - 0x003EB2 00:FEA2: 02        .byte $02   ; 
- D 3 - I - 0x003EB3 00:FEA3: ED        .byte $ED   ; 
- D 3 - I - 0x003EB4 00:FEA4: 89        .byte $89   ; 
- D 3 - I - 0x003EB5 00:FEA5: 0C        .byte $0C   ; 
- D 3 - I - 0x003EB6 00:FEA6: 02        .byte $02   ; 
- D 3 - I - 0x003EB7 00:FEA7: 12        .byte $12   ; 
- D 3 - I - 0x003EB8 00:FEA8: 4E        .byte $4E   ; 
- D 3 - I - 0x003EB9 00:FEA9: 02        .byte $02   ; 
- D 3 - I - 0x003EBA 00:FEAA: 12        .byte $12   ; 
- D 3 - I - 0x003EBB 00:FEAB: 18        .byte $18   ; 
- D 3 - I - 0x003EBC 00:FEAC: 02        .byte $02   ; 
- D 3 - I - 0x003EBD 00:FEAD: 0E        .byte $0E   ; 
- D 3 - I - 0x003EBE 00:FEAE: 08        .byte $08   ; 
- D 3 - I - 0x003EBF 00:FEAF: 02        .byte $02   ; 
- D 3 - I - 0x003EC0 00:FEB0: 0E        .byte $0E   ; 
- D 3 - I - 0x003EC1 00:FEB1: FF        .byte $FF   ; 
off_FEB2:
- D 3 - I - 0x003EC2 00:FEB2: 80        .byte $80   ; 
- D 3 - I - 0x003EC3 00:FEB3: 42        .byte $42   ; 
- D 3 - I - 0x003EC4 00:FEB4: 02        .byte $02   ; 
- D 3 - I - 0x003EC5 00:FEB5: 48        .byte $48   ; 
- D 3 - I - 0x003EC6 00:FEB6: 02        .byte $02   ; 
- D 3 - I - 0x003EC7 00:FEB7: 1E        .byte $1E   ; 
- D 3 - I - 0x003EC8 00:FEB8: 02        .byte $02   ; 
- D 3 - I - 0x003EC9 00:FEB9: 24        .byte $24   ; 
- D 3 - I - 0x003ECA 00:FEBA: 02        .byte $02   ; 
- D 3 - I - 0x003ECB 00:FEBB: 02        .byte $02   ; 
- D 3 - I - 0x003ECC 00:FEBC: 02        .byte $02   ; 
- D 3 - I - 0x003ECD 00:FEBD: 2A        .byte $2A   ; 
- D 3 - I - 0x003ECE 00:FEBE: 02        .byte $02   ; 
- D 3 - I - 0x003ECF 00:FEBF: C6        .byte $C6   ; 
- D 3 - I - 0x003ED0 00:FEC0: 8C        .byte $8C   ; 
- D 3 - I - 0x003ED1 00:FEC1: 30        .byte $30   ; 
- D 3 - I - 0x003ED2 00:FEC2: 2A        .byte $2A   ; 
- D 3 - I - 0x003ED3 00:FEC3: FF        .byte $FF   ; 
- D 3 - I - 0x003ED4 00:FEC4: 00        .byte $00   ; 
off_FEC5:
- D 3 - I - 0x003ED5 00:FEC5: 80        .byte $80   ; 
- D 3 - I - 0x003ED6 00:FEC6: 24        .byte $24   ; 
- D 3 - I - 0x003ED7 00:FEC7: 02        .byte $02   ; 
- D 3 - I - 0x003ED8 00:FEC8: 2A        .byte $2A   ; 
- D 3 - I - 0x003ED9 00:FEC9: 02        .byte $02   ; 
- D 3 - I - 0x003EDA 00:FECA: 30        .byte $30   ; 
- D 3 - I - 0x003EDB 00:FECB: 02        .byte $02   ; 
- D 3 - I - 0x003EDC 00:FECC: 06        .byte $06   ; 
- D 3 - I - 0x003EDD 00:FECD: 02        .byte $02   ; 
- D 3 - I - 0x003EDE 00:FECE: 02        .byte $02   ; 
- D 3 - I - 0x003EDF 00:FECF: 02        .byte $02   ; 
- D 3 - I - 0x003EE0 00:FED0: 0C        .byte $0C   ; 
- D 3 - I - 0x003EE1 00:FED1: 02        .byte $02   ; 
- D 3 - I - 0x003EE2 00:FED2: C6        .byte $C6   ; 
- D 3 - I - 0x003EE3 00:FED3: 8C        .byte $8C   ; 
- D 3 - I - 0x003EE4 00:FED4: 12        .byte $12   ; 
- D 3 - I - 0x003EE5 00:FED5: 18        .byte $18   ; 
- D 3 - I - 0x003EE6 00:FED6: FF        .byte $FF   ; 
off_FED7:
- D 3 - I - 0x003EE7 00:FED7: 80        .byte $80   ; 
- D 3 - I - 0x003EE8 00:FED8: 56        .byte $56   ; 
- D 3 - I - 0x003EE9 00:FED9: 54        .byte $54   ; 
- D 3 - I - 0x003EEA 00:FEDA: 52        .byte $52   ; 
- D 3 - I - 0x003EEB 00:FEDB: 50        .byte $50   ; 
- D 3 - I - 0x003EEC 00:FEDC: 81        .byte $81   ; 
- D 3 - I - 0x003EED 00:FEDD: 02        .byte $02   ; 
- D 3 - I - 0x003EEE 00:FEDE: 80        .byte $80   ; 
- D 3 - I - 0x003EEF 00:FEDF: 5E        .byte $5E   ; 
- D 3 - I - 0x003EF0 00:FEE0: 5A        .byte $5A   ; 
- D 3 - I - 0x003EF1 00:FEE1: 54        .byte $54   ; 
- D 3 - I - 0x003EF2 00:FEE2: 50        .byte $50   ; 
- D 3 - I - 0x003EF3 00:FEE3: 18        .byte $18   ; 
- D 3 - I - 0x003EF4 00:FEE4: 14        .byte $14   ; 
- D 3 - I - 0x003EF5 00:FEE5: 10        .byte $10   ; 
- D 3 - I - 0x003EF6 00:FEE6: 0A        .byte $0A   ; 
- D 3 - I - 0x003EF7 00:FEE7: 06        .byte $06   ; 
- D 3 - I - 0x003EF8 00:FEE8: 30        .byte $30   ; 
- D 3 - I - 0x003EF9 00:FEE9: 2C        .byte $2C   ; 
- D 3 - I - 0x003EFA 00:FEEA: 28        .byte $28   ; 
- D 3 - I - 0x003EFB 00:FEEB: 02        .byte $02   ; 
- D 3 - I - 0x003EFC 00:FEEC: 00        .byte $00   ; 
off_FEED:
- D 3 - I - 0x003EFD 00:FEED: 80        .byte $80   ; 
- D 3 - I - 0x003EFE 00:FEEE: 1A        .byte $1A   ; 
- D 3 - I - 0x003EFF 00:FEEF: 18        .byte $18   ; 
- D 3 - I - 0x003F00 00:FEF0: 16        .byte $16   ; 
- D 3 - I - 0x003F01 00:FEF1: 14        .byte $14   ; 
- D 3 - I - 0x003F02 00:FEF2: 81        .byte $81   ; 
- D 3 - I - 0x003F03 00:FEF3: 02        .byte $02   ; 
- D 3 - I - 0x003F04 00:FEF4: 80        .byte $80   ; 
- D 3 - I - 0x003F05 00:FEF5: 02        .byte $02   ; 
- D 3 - I - 0x003F06 00:FEF6: 5E        .byte $5E   ; 
- D 3 - I - 0x003F07 00:FEF7: 5A        .byte $5A   ; 
- D 3 - I - 0x003F08 00:FEF8: 54        .byte $54   ; 
- D 3 - I - 0x003F09 00:FEF9: 50        .byte $50   ; 
- D 3 - I - 0x003F0A 00:FEFA: 18        .byte $18   ; 
- D 3 - I - 0x003F0B 00:FEFB: 14        .byte $14   ; 
- D 3 - I - 0x003F0C 00:FEFC: 10        .byte $10   ; 
- D 3 - I - 0x003F0D 00:FEFD: 0A        .byte $0A   ; 
- D 3 - I - 0x003F0E 00:FEFE: 06        .byte $06   ; 
- D 3 - I - 0x003F0F 00:FEFF: 30        .byte $30   ; 
- D 3 - I - 0x003F10 00:FF00: 2C        .byte $2C   ; 
- D 3 - I - 0x003F11 00:FF01: 28        .byte $28   ; 
off_FF02:
- D 3 - I - 0x003F12 00:FF02: 82        .byte $82   ; 
- D 3 - I - 0x003F13 00:FF03: 1C        .byte $1C   ; 
- D 3 - I - 0x003F14 00:FF04: 02        .byte $02   ; 
- D 3 - I - 0x003F15 00:FF05: 1C        .byte $1C   ; 
- D 3 - I - 0x003F16 00:FF06: 02        .byte $02   ; 
- D 3 - I - 0x003F17 00:FF07: 02        .byte $02   ; 
- D 3 - I - 0x003F18 00:FF08: 1C        .byte $1C   ; 
- D 3 - I - 0x003F19 00:FF09: 1C        .byte $1C   ; 
- D 3 - I - 0x003F1A 00:FF0A: 00        .byte $00   ; 
off_FF0B:
- D 3 - I - 0x003F1B 00:FF0B: 81        .byte $81   ; 
- D 3 - I - 0x003F1C 00:FF0C: 10        .byte $10   ; 
- D 3 - I - 0x003F1D 00:FF0D: 0A        .byte $0A   ; 
- D 3 - I - 0x003F1E 00:FF0E: 32        .byte $32   ; 
- D 3 - I - 0x003F1F 00:FF0F: 28        .byte $28   ; 
- D 3 - I - 0x003F20 00:FF10: 80        .byte $80   ; 
- D 3 - I - 0x003F21 00:FF11: 32        .byte $32   ; 
- D 3 - I - 0x003F22 00:FF12: 02        .byte $02   ; 
- D 3 - I - 0x003F23 00:FF13: 32        .byte $32   ; 
- D 3 - I - 0x003F24 00:FF14: 02        .byte $02   ; 
- D 3 - I - 0x003F25 00:FF15: 82        .byte $82   ; 
- D 3 - I - 0x003F26 00:FF16: 32        .byte $32   ; 
- D 3 - I - 0x003F27 00:FF17: 81        .byte $81   ; 
- D 3 - I - 0x003F28 00:FF18: 06        .byte $06   ; 
- D 3 - I - 0x003F29 00:FF19: 02        .byte $02   ; 
- D 3 - I - 0x003F2A 00:FF1A: 06        .byte $06   ; 
- D 3 - I - 0x003F2B 00:FF1B: 02        .byte $02   ; 
- D 3 - I - 0x003F2C 00:FF1C: 82        .byte $82   ; 
- D 3 - I - 0x003F2D 00:FF1D: 32        .byte $32   ; 
off_FF1E:
- D 3 - I - 0x003F2E 00:FF1E: 81        .byte $81   ; 
- D 3 - I - 0x003F2F 00:FF1F: 54        .byte $54   ; 
- D 3 - I - 0x003F30 00:FF20: 1A        .byte $1A   ; 
- D 3 - I - 0x003F31 00:FF21: 10        .byte $10   ; 
- D 3 - I - 0x003F32 00:FF22: 0A        .byte $0A   ; 
- D 3 - I - 0x003F33 00:FF23: 80        .byte $80   ; 
- D 3 - I - 0x003F34 00:FF24: 10        .byte $10   ; 
- D 3 - I - 0x003F35 00:FF25: 02        .byte $02   ; 
- D 3 - I - 0x003F36 00:FF26: 10        .byte $10   ; 
- D 3 - I - 0x003F37 00:FF27: 02        .byte $02   ; 
- D 3 - I - 0x003F38 00:FF28: 82        .byte $82   ; 
- D 3 - I - 0x003F39 00:FF29: 10        .byte $10   ; 
- D 3 - I - 0x003F3A 00:FF2A: 81        .byte $81   ; 
- D 3 - I - 0x003F3B 00:FF2B: 16        .byte $16   ; 
- D 3 - I - 0x003F3C 00:FF2C: 02        .byte $02   ; 
- D 3 - I - 0x003F3D 00:FF2D: 16        .byte $16   ; 
- D 3 - I - 0x003F3E 00:FF2E: 02        .byte $02   ; 
- D 3 - I - 0x003F3F 00:FF2F: 82        .byte $82   ; 
- D 3 - I - 0x003F40 00:FF30: 0A        .byte $0A   ; 
off_FF31:
- D 3 - I - 0x003F41 00:FF31: 83        .byte $83   ; 
- D 3 - I - 0x003F42 00:FF32: 03        .byte $03   ; 
- D 3 - I - 0x003F43 00:FF33: 0C        .byte $0C   ; 
- D 3 - I - 0x003F44 00:FF34: 82        .byte $82   ; 
- D 3 - I - 0x003F45 00:FF35: 03        .byte $03   ; 
- D 3 - I - 0x003F46 00:FF36: 0C        .byte $0C   ; 
- D 3 - I - 0x003F47 00:FF37: 0C        .byte $0C   ; 
off_FF38:
- D 3 - I - 0x003F48 00:FF38: C2        .byte $C2   ; 
- D 3 - I - 0x003F49 00:FF39: 88        .byte $88   ; 
- D 3 - I - 0x003F4A 00:FF3A: 1C        .byte $1C   ; 
- D 3 - I - 0x003F4B 00:FF3B: 1C        .byte $1C   ; 
- D 3 - I - 0x003F4C 00:FF3C: 1C        .byte $1C   ; 
- D 3 - I - 0x003F4D 00:FF3D: 1C        .byte $1C   ; 
- D 3 - I - 0x003F4E 00:FF3E: 1C        .byte $1C   ; 
- D 3 - I - 0x003F4F 00:FF3F: 1C        .byte $1C   ; 
- D 3 - I - 0x003F50 00:FF40: 83        .byte $83   ; 
- D 3 - I - 0x003F51 00:FF41: 1C        .byte $1C   ; 
- D 3 - I - 0x003F52 00:FF42: 80        .byte $80   ; 
- D 3 - I - 0x003F53 00:FF43: 04        .byte $04   ; 
- D 3 - I - 0x003F54 00:FF44: 04        .byte $04   ; 
- D 3 - I - 0x003F55 00:FF45: 2A        .byte $2A   ; 
- D 3 - I - 0x003F56 00:FF46: 02        .byte $02   ; 
- D 3 - I - 0x003F57 00:FF47: 82        .byte $82   ; 
- D 3 - I - 0x003F58 00:FF48: 1C        .byte $1C   ; 
- D 3 - I - 0x003F59 00:FF49: FF        .byte $FF   ; 
- D 3 - I - 0x003F5A 00:FF4A: 81        .byte $81   ; 
- D 3 - I - 0x003F5B 00:FF4B: 4C        .byte $4C   ; 
- D 3 - I - 0x003F5C 00:FF4C: 02        .byte $02   ; 
- D 3 - I - 0x003F5D 00:FF4D: 4C        .byte $4C   ; 
- D 3 - I - 0x003F5E 00:FF4E: 02        .byte $02   ; 
- D 3 - I - 0x003F5F 00:FF4F: 2A        .byte $2A   ; 
- D 3 - I - 0x003F60 00:FF50: 02        .byte $02   ; 
- D 3 - I - 0x003F61 00:FF51: 4C        .byte $4C   ; 
- D 3 - I - 0x003F62 00:FF52: 1C        .byte $1C   ; 
- D 3 - I - 0x003F63 00:FF53: 81        .byte $81   ; 
- D 3 - I - 0x003F64 00:FF54: 4C        .byte $4C   ; 
- D 3 - I - 0x003F65 00:FF55: 02        .byte $02   ; 
- D 3 - I - 0x003F66 00:FF56: 4C        .byte $4C   ; 
- D 3 - I - 0x003F67 00:FF57: 02        .byte $02   ; 
- D 3 - I - 0x003F68 00:FF58: 4C        .byte $4C   ; 
- D 3 - I - 0x003F69 00:FF59: 00        .byte $00   ; 
off_FF5A:
- D 3 - I - 0x003F6A 00:FF5A: 88        .byte $88   ; 
- D 3 - I - 0x003F6B 00:FF5B: 2E        .byte $2E   ; 
- D 3 - I - 0x003F6C 00:FF5C: 2E        .byte $2E   ; 
- D 3 - I - 0x003F6D 00:FF5D: 2E        .byte $2E   ; 
- D 3 - I - 0x003F6E 00:FF5E: 30        .byte $30   ; 
- D 3 - I - 0x003F6F 00:FF5F: 04        .byte $04   ; 
- D 3 - I - 0x003F70 00:FF60: 30        .byte $30   ; 
- D 3 - I - 0x003F71 00:FF61: C4        .byte $C4   ; 
- D 3 - I - 0x003F72 00:FF62: 80        .byte $80   ; 
- D 3 - I - 0x003F73 00:FF63: 2E        .byte $2E   ; 
- D 3 - I - 0x003F74 00:FF64: 04        .byte $04   ; 
- D 3 - I - 0x003F75 00:FF65: FF        .byte $FF   ; 
- D 3 - I - 0x003F76 00:FF66: 83        .byte $83   ; 
- D 3 - I - 0x003F77 00:FF67: 02        .byte $02   ; 
- D 3 - I - 0x003F78 00:FF68: 88        .byte $88   ; 
- D 3 - I - 0x003F79 00:FF69: 2E        .byte $2E   ; 
- D 3 - I - 0x003F7A 00:FF6A: 2E        .byte $2E   ; 
- D 3 - I - 0x003F7B 00:FF6B: 2E        .byte $2E   ; 
- D 3 - I - 0x003F7C 00:FF6C: 30        .byte $30   ; 
- D 3 - I - 0x003F7D 00:FF6D: 04        .byte $04   ; 
- D 3 - I - 0x003F7E 00:FF6E: 30        .byte $30   ; 
- D 3 - I - 0x003F7F 00:FF6F: C4        .byte $C4   ; 
- D 3 - I - 0x003F80 00:FF70: 80        .byte $80   ; 
- D 3 - I - 0x003F81 00:FF71: 2E        .byte $2E   ; 
- D 3 - I - 0x003F82 00:FF72: 04        .byte $04   ; 
- D 3 - I - 0x003F83 00:FF73: FF        .byte $FF   ; 
- D 3 - I - 0x003F84 00:FF74: 83        .byte $83   ; 
- D 3 - I - 0x003F85 00:FF75: 02        .byte $02   ; 
- D 3 - I - 0x003F86 00:FF76: 84        .byte $84   ; 
- D 3 - I - 0x003F87 00:FF77: 02        .byte $02   ; 
- D 3 - I - 0x003F88 00:FF78: 02        .byte $02   ; 
off_FF79:
- D 3 - I - 0x003F89 00:FF79: C2        .byte $C2   ; 
- D 3 - I - 0x003F8A 00:FF7A: 88        .byte $88   ; 
- D 3 - I - 0x003F8B 00:FF7B: 3E        .byte $3E   ; 
- D 3 - I - 0x003F8C 00:FF7C: 3E        .byte $3E   ; 
- D 3 - I - 0x003F8D 00:FF7D: 3E        .byte $3E   ; 
- D 3 - I - 0x003F8E 00:FF7E: 42        .byte $42   ; 
- D 3 - I - 0x003F8F 00:FF7F: 46        .byte $46   ; 
- D 3 - I - 0x003F90 00:FF80: 42        .byte $42   ; 
- D 3 - I - 0x003F91 00:FF81: 84        .byte $84   ; 
- D 3 - I - 0x003F92 00:FF82: 3E        .byte $3E   ; 
- D 3 - I - 0x003F93 00:FF83: FF        .byte $FF   ; 
- D 3 - I - 0x003F94 00:FF84: 85        .byte $85   ; 
- D 3 - I - 0x003F95 00:FF85: 3E        .byte $3E   ; 
- D 3 - I - 0x003F96 00:FF86: 81        .byte $81   ; 
- D 3 - I - 0x003F97 00:FF87: 3E        .byte $3E   ; 
- D 3 - I - 0x003F98 00:FF88: 88        .byte $88   ; 
- D 3 - I - 0x003F99 00:FF89: 1C        .byte $1C   ; 
- D 3 - I - 0x003F9A 00:FF8A: 46        .byte $46   ; 
- D 3 - I - 0x003F9B 00:FF8B: 1C        .byte $1C   ; 
- D 3 - I - 0x003F9C 00:FF8C: 81        .byte $81   ; 
- D 3 - I - 0x003F9D 00:FF8D: 02        .byte $02   ; 
- D 3 - I - 0x003F9E 00:FF8E: 3E        .byte $3E   ; 
- D 3 - I - 0x003F9F 00:FF8F: 3E        .byte $3E   ; 
- D 3 - I - 0x003FA0 00:FF90: 3E        .byte $3E   ; 
- D 3 - I - 0x003FA1 00:FF91: 82        .byte $82   ; 
- D 3 - I - 0x003FA2 00:FF92: 34        .byte $34   ; 
- - - - - - 0x003FA3 00:FF93: 02        .byte $02   ; 
off_FF94:
- D 3 - I - 0x003FA4 00:FF94: C2        .byte $C2   ; 
- D 3 - I - 0x003FA5 00:FF95: 88        .byte $88   ; 
- D 3 - I - 0x003FA6 00:FF96: 06        .byte $06   ; 
- D 3 - I - 0x003FA7 00:FF97: 06        .byte $06   ; 
- D 3 - I - 0x003FA8 00:FF98: 06        .byte $06   ; 
- D 3 - I - 0x003FA9 00:FF99: 06        .byte $06   ; 
- D 3 - I - 0x003FAA 00:FF9A: 06        .byte $06   ; 
- D 3 - I - 0x003FAB 00:FF9B: 06        .byte $06   ; 
- D 3 - I - 0x003FAC 00:FF9C: 82        .byte $82   ; 
- D 3 - I - 0x003FAD 00:FF9D: 06        .byte $06   ; 
- D 3 - I - 0x003FAE 00:FF9E: 06        .byte $06   ; 
- D 3 - I - 0x003FAF 00:FF9F: 06        .byte $06   ; 
- D 3 - I - 0x003FB0 00:FFA0: 06        .byte $06   ; 
- D 3 - I - 0x003FB1 00:FFA1: FF        .byte $FF   ; 
- D 3 - I - 0x003FB2 00:FFA2: C2        .byte $C2   ; 
- D 3 - I - 0x003FB3 00:FFA3: 81        .byte $81   ; 
- D 3 - I - 0x003FB4 00:FFA4: 06        .byte $06   ; 
- D 3 - I - 0x003FB5 00:FFA5: 06        .byte $06   ; 
- D 3 - I - 0x003FB6 00:FFA6: 80        .byte $80   ; 
- D 3 - I - 0x003FB7 00:FFA7: 06        .byte $06   ; 
- D 3 - I - 0x003FB8 00:FFA8: 06        .byte $06   ; 
- D 3 - I - 0x003FB9 00:FFA9: 81        .byte $81   ; 
- D 3 - I - 0x003FBA 00:FFAA: 06        .byte $06   ; 
- D 3 - I - 0x003FBB 00:FFAB: 06        .byte $06   ; 
- D 3 - I - 0x003FBC 00:FFAC: 06        .byte $06   ; 
- D 3 - I - 0x003FBD 00:FFAD: 06        .byte $06   ; 
- D 3 - I - 0x003FBE 00:FFAE: 80        .byte $80   ; 
- D 3 - I - 0x003FBF 00:FFAF: 06        .byte $06   ; 
- D 3 - I - 0x003FC0 00:FFB0: 06        .byte $06   ; 
- D 3 - I - 0x003FC1 00:FFB1: FF        .byte $FF   ; 
- - - - - - 0x003FC2 00:FFB2: 09        .byte $09   ; 
off_FFB3:
- D 3 - I - 0x003FC3 00:FFB3: 80        .byte $80   ; 
- D 3 - I - 0x003FC4 00:FFB4: 10        .byte $10   ; 
- D 3 - I - 0x003FC5 00:FFB5: 02        .byte $02   ; 
- D 3 - I - 0x003FC6 00:FFB6: 10        .byte $10   ; 
- D 3 - I - 0x003FC7 00:FFB7: 02        .byte $02   ; 
- D 3 - I - 0x003FC8 00:FFB8: 10        .byte $10   ; 
- D 3 - I - 0x003FC9 00:FFB9: 02        .byte $02   ; 
- D 3 - I - 0x003FCA 00:FFBA: 0C        .byte $0C   ; 
- D 3 - I - 0x003FCB 00:FFBB: 0C        .byte $0C   ; 
- D 3 - I - 0x003FCC 00:FFBC: 0C        .byte $0C   ; 
- D 3 - I - 0x003FCD 00:FFBD: 02        .byte $02   ; 
- D 3 - I - 0x003FCE 00:FFBE: 0C        .byte $0C   ; 
- D 3 - I - 0x003FCF 00:FFBF: 02        .byte $02   ; 
- D 3 - I - 0x003FD0 00:FFC0: 14        .byte $14   ; 
- D 3 - I - 0x003FD1 00:FFC1: 14        .byte $14   ; 
- D 3 - I - 0x003FD2 00:FFC2: 14        .byte $14   ; 
- D 3 - I - 0x003FD3 00:FFC3: 02        .byte $02   ; 
- D 3 - I - 0x003FD4 00:FFC4: 14        .byte $14   ; 
- D 3 - I - 0x003FD5 00:FFC5: 02        .byte $02   ; 
- D 3 - I - 0x003FD6 00:FFC6: 85        .byte $85   ; 
- D 3 - I - 0x003FD7 00:FFC7: 10        .byte $10   ; 
- D 3 - I - 0x003FD8 00:FFC8: 00        .byte $00   ; 
off_FFC9:
- D 3 - I - 0x003FD9 00:FFC9: 80        .byte $80   ; 
- D 3 - I - 0x003FDA 00:FFCA: 32        .byte $32   ; 
- D 3 - I - 0x003FDB 00:FFCB: 02        .byte $02   ; 
- D 3 - I - 0x003FDC 00:FFCC: 32        .byte $32   ; 
- D 3 - I - 0x003FDD 00:FFCD: 02        .byte $02   ; 
- D 3 - I - 0x003FDE 00:FFCE: 32        .byte $32   ; 
- D 3 - I - 0x003FDF 00:FFCF: 02        .byte $02   ; 
- D 3 - I - 0x003FE0 00:FFD0: C2        .byte $C2   ; 
- D 3 - I - 0x003FE1 00:FFD1: 32        .byte $32   ; 
- D 3 - I - 0x003FE2 00:FFD2: 32        .byte $32   ; 
- D 3 - I - 0x003FE3 00:FFD3: 32        .byte $32   ; 
- D 3 - I - 0x003FE4 00:FFD4: 02        .byte $02   ; 
- D 3 - I - 0x003FE5 00:FFD5: 32        .byte $32   ; 
- D 3 - I - 0x003FE6 00:FFD6: 02        .byte $02   ; 
- D 3 - I - 0x003FE7 00:FFD7: FF        .byte $FF   ; 
- D 3 - I - 0x003FE8 00:FFD8: 85        .byte $85   ; 
- D 3 - I - 0x003FE9 00:FFD9: 32        .byte $32   ; 
off_FFDA:
- D 3 - I - 0x003FEA 00:FFDA: 80        .byte $80   ; 
- D 3 - I - 0x003FEB 00:FFDB: 54        .byte $54   ; 
- D 3 - I - 0x003FEC 00:FFDC: 02        .byte $02   ; 
- D 3 - I - 0x003FED 00:FFDD: 54        .byte $54   ; 
- D 3 - I - 0x003FEE 00:FFDE: 02        .byte $02   ; 
- D 3 - I - 0x003FEF 00:FFDF: 54        .byte $54   ; 
- D 3 - I - 0x003FF0 00:FFE0: 02        .byte $02   ; 
- D 3 - I - 0x003FF1 00:FFE1: 50        .byte $50   ; 
- D 3 - I - 0x003FF2 00:FFE2: 50        .byte $50   ; 
- D 3 - I - 0x003FF3 00:FFE3: 50        .byte $50   ; 
- D 3 - I - 0x003FF4 00:FFE4: 02        .byte $02   ; 
- D 3 - I - 0x003FF5 00:FFE5: 50        .byte $50   ; 
- D 3 - I - 0x003FF6 00:FFE6: 02        .byte $02   ; 
- D 3 - I - 0x003FF7 00:FFE7: 56        .byte $56   ; 
- D 3 - I - 0x003FF8 00:FFE8: 56        .byte $56   ; 
- D 3 - I - 0x003FF9 00:FFE9: 56        .byte $56   ; 
- D 3 - I - 0x003FFA 00:FFEA: 02        .byte $02   ; 
- D 3 - I - 0x003FFB 00:FFEB: 56        .byte $56   ; 
- D 3 - I - 0x003FFC 00:FFEC: 02        .byte $02   ; 
- D 3 - I - 0x003FFD 00:FFED: 85        .byte $85   ; 
- D 3 - I - 0x003FFE 00:FFEE: 54        .byte $54   ; 
off_FFEF:
- D 3 - I - 0x003FFF 00:FFEF: C4        .byte $C4   ; 
- D 3 - I - 0x004000 00:FFF0: 85        .byte $85   ; 
- D 3 - I - 0x004001 00:FFF1: 0C        .byte $0C   ; 
- D 3 - I - 0x004002 00:FFF2: FF        .byte $FF   ; 


; bzk garbage
- - - - - - 0x004003 00:FFF3: FF        .byte $FF, $FF, $FF, $FF   ; 



sub_FFF7:
C - - - - - 0x004007 00:FFF7: 4C 58 F7  JMP loc_F758

.segment "VECTORS"
- D 3 - - - 0x00400A 00:FFFA: 94 C0     .word vec_C094_NMI
- D 3 - - - 0x00400C 00:FFFC: 00 C0     .word vec_C000_RESET
- - - - - - 0x00400E 00:FFFE: F7 C0     .word vec_C0F7_IRQ



