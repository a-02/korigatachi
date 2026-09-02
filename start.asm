  processor 6502
  include vcs.h
  org $f000
Start
  sei 
  cld 
  ldx #$ff
  txs 
  lda #$0
ClearMem
  sta ($0,x)
  dex 
  bne ClearMem
  lda #$0
  sta $81
  sta $9
  lda #$21
  sta $6
MainLoop
  lda #$2
  sta $0
  sta $2
  sta $2
  sta $2
  lda #$2b
  sta $96
  lda #$0
  sta $0
WaitForVblankEnd
  lda $84
  bne WaitForVblankEnd
  ldy #$bf
  sta $2
  sta $1
  lda #$f0
  sta $22
  sta $2
  sta $2a
ScanLoop
  lda $80
  sta $9
  sta $2
  dey 
  bne ScanLoop
  lda #$2
  sta $2
  sta $1
  ldx #$1e
OverScanWait
  sta $2
  dex 
  bne OverScanWait
  jmp MainLoop
  org $fffc
  .word $f000
  .word $f000

