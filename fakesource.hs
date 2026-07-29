adc :: T.Text -> Korigatachi ()
adc oprText = K.do
  let parseADC = parseIndirectX <|> parseZeroPage <|> parseImmediate <|> parseAbsolute <|> parseIndirectY <|> parseZeroPageX <|> parseAbsoluteY <|> parseAbsoluteX <|> parseLabel
  case Attoparsec.parseOnly parseADC oprText of
    Left _ -> K.log K.Warn ("Failed to parse operand: " <> oprText)
    Right (IndirectX w8) -> K.do
      void $ traverse assembleROM [0x61, w8]
      K.codeGen (spacing <> "adc " <> oprText)
    Right (ZeroPage w8) -> K.do
      void $ traverse assembleROM [0x65, w8]
      K.codeGen (spacing <> "adc " <> oprText)
    Right (Immediate w8) -> K.do
      void $ traverse assembleROM [0x69, w8]
      K.codeGen (spacing <> "adc " <> oprText)
    Right (Absolute ll hh) -> K.do
      void $ traverse assembleROM [0x6d, ll, hh]
      K.codeGen (spacing <> "adc " <> oprText)
    Right (IndirectY w8) -> K.do
      void $ traverse assembleROM [0x71, w8]
      K.codeGen (spacing <> "adc " <> oprText)
    Right (ZeroPageX w8) -> K.do
      void $ traverse assembleROM [0x75, w8]
      K.codeGen (spacing <> "adc " <> oprText)
    Right (AbsoluteY ll hh) -> K.do
      void $ traverse assembleROM [0x79, ll, hh]
      K.codeGen (spacing <> "adc " <> oprText)
    Right (AbsoluteX ll hh) -> K.do
      void $ traverse assembleROM [0x7d, ll, hh]
      K.codeGen (spacing <> "adc " <> oprText)
    Right (Label text) -> K.do
      res <- resolveLabel text
      adc res
    _ -> K.log K.Warn ("Invalid operand for ADC: " <> oprText)
anc :: T.Text -> Korigatachi ()
anc oprText = K.do
  let parseANC = parseImmediate <|> parseImmediate <|> parseLabel
  case Attoparsec.parseOnly parseANC oprText of
    Left _ -> K.log K.Warn ("Failed to parse operand: " <> oprText)
    Right (Immediate w8) -> K.do
      void $ traverse assembleROM [0x0b, w8]
      K.codeGen (spacing <> "anc " <> oprText)
    Right (Immediate w8) -> K.do
      void $ traverse assembleROM [0x2b, w8]
      K.codeGen (spacing <> "anc " <> oprText)
    Right (Label text) -> K.do
      res <- resolveLabel text
      anc res
    _ -> K.log K.Warn ("Invalid operand for ANC: " <> oprText)
and :: T.Text -> Korigatachi ()
and oprText = K.do
  let parseAND = parseIndirectX <|> parseZeroPage <|> parseImmediate <|> parseAbsolute <|> parseIndirectY <|> parseZeroPageX <|> parseAbsoluteY <|> parseAbsoluteX <|> parseLabel
  case Attoparsec.parseOnly parseAND oprText of
    Left _ -> K.log K.Warn ("Failed to parse operand: " <> oprText)
    Right (IndirectX w8) -> K.do
      void $ traverse assembleROM [0x21, w8]
      K.codeGen (spacing <> "and " <> oprText)
    Right (ZeroPage w8) -> K.do
      void $ traverse assembleROM [0x25, w8]
      K.codeGen (spacing <> "and " <> oprText)
    Right (Immediate w8) -> K.do
      void $ traverse assembleROM [0x29, w8]
      K.codeGen (spacing <> "and " <> oprText)
    Right (Absolute ll hh) -> K.do
      void $ traverse assembleROM [0x2d, ll, hh]
      K.codeGen (spacing <> "and " <> oprText)
    Right (IndirectY w8) -> K.do
      void $ traverse assembleROM [0x31, w8]
      K.codeGen (spacing <> "and " <> oprText)
    Right (ZeroPageX w8) -> K.do
      void $ traverse assembleROM [0x35, w8]
      K.codeGen (spacing <> "and " <> oprText)
    Right (AbsoluteY ll hh) -> K.do
      void $ traverse assembleROM [0x39, ll, hh]
      K.codeGen (spacing <> "and " <> oprText)
    Right (AbsoluteX ll hh) -> K.do
      void $ traverse assembleROM [0x3d, ll, hh]
      K.codeGen (spacing <> "and " <> oprText)
    Right (Label text) -> K.do
      res <- resolveLabel text
      and res
    _ -> K.log K.Warn ("Invalid operand for AND: " <> oprText)
ane :: T.Text -> Korigatachi ()
ane oprText = K.do
  let parseANE = parseImmediate <|> parseLabel
  case Attoparsec.parseOnly parseANE oprText of
    Left _ -> K.log K.Warn ("Failed to parse operand: " <> oprText)
    Right (Immediate w8) -> K.do
      void $ traverse assembleROM [0x8b, w8]
      K.codeGen (spacing <> "ane " <> oprText)
    Right (Label text) -> K.do
      res <- resolveLabel text
      ane res
    _ -> K.log K.Warn ("Invalid operand for ANE: " <> oprText)
alr :: T.Text -> Korigatachi ()
alr oprText = K.do
  let parseALR = parseImmediate <|> parseLabel
  case Attoparsec.parseOnly parseALR oprText of
    Left _ -> K.log K.Warn ("Failed to parse operand: " <> oprText)
    Right (Immediate w8) -> K.do
      void $ traverse assembleROM [0x4b, w8]
      K.codeGen (spacing <> "alr " <> oprText)
    Right (Label text) -> K.do
      res <- resolveLabel text
      alr res
    _ -> K.log K.Warn ("Invalid operand for ALR: " <> oprText)
arr :: T.Text -> Korigatachi ()
arr oprText = K.do
  let parseARR = parseImmediate <|> parseLabel
  case Attoparsec.parseOnly parseARR oprText of
    Left _ -> K.log K.Warn ("Failed to parse operand: " <> oprText)
    Right (Immediate w8) -> K.do
      void $ traverse assembleROM [0x6b, w8]
      K.codeGen (spacing <> "arr " <> oprText)
    Right (Label text) -> K.do
      res <- resolveLabel text
      arr res
    _ -> K.log K.Warn ("Invalid operand for ARR: " <> oprText)
asl :: T.Text -> Korigatachi ()
asl oprText = K.do
  let parseASL = parseZeroPage <|> parseAccumulator <|> parseAbsolute <|> parseZeroPageX <|> parseAbsoluteX <|> parseLabel
  case Attoparsec.parseOnly parseASL oprText of
    Left _ -> K.log K.Warn ("Failed to parse operand: " <> oprText)
    Right (ZeroPage w8) -> K.do
      void $ traverse assembleROM [0x06, w8]
      K.codeGen (spacing <> "asl " <> oprText)
    Right Accumulator-> K.do
      assembleROM 0x0a
      K.codeGen (spacing <> "asl")
    Right (Absolute ll hh) -> K.do
      void $ traverse assembleROM [0x0e, ll, hh]
      K.codeGen (spacing <> "asl " <> oprText)
    Right (ZeroPageX w8) -> K.do
      void $ traverse assembleROM [0x16, w8]
      K.codeGen (spacing <> "asl " <> oprText)
    Right (AbsoluteX ll hh) -> K.do
      void $ traverse assembleROM [0x1e, ll, hh]
      K.codeGen (spacing <> "asl " <> oprText)
    Right (Label text) -> K.do
      res <- resolveLabel text
      asl res
    _ -> K.log K.Warn ("Invalid operand for ASL: " <> oprText)
bcc :: T.Text -> Korigatachi ()
bcc oprText = K.do
  let parseBCC = parseRelative <|> parseLabel
  case Attoparsec.parseOnly parseBCC oprText of
    Left _ -> K.log K.Warn ("Failed to parse operand: " <> oprText)
    Right (Relative w8) -> K.do
      void $ traverse assembleROM [0x90, w8]
      K.codeGen (spacing <> "bcc " <> oprText)
    Right (Label text) -> K.do
      res <- resolveLabel text
      bcc res
    _ -> K.log K.Warn ("Invalid operand for BCC: " <> oprText)
bcs :: T.Text -> Korigatachi ()
bcs oprText = K.do
  let parseBCS = parseRelative <|> parseLabel
  case Attoparsec.parseOnly parseBCS oprText of
    Left _ -> K.log K.Warn ("Failed to parse operand: " <> oprText)
    Right (Relative w8) -> K.do
      void $ traverse assembleROM [0xb0, w8]
      K.codeGen (spacing <> "bcs " <> oprText)
    Right (Label text) -> K.do
      res <- resolveLabel text
      bcs res
    _ -> K.log K.Warn ("Invalid operand for BCS: " <> oprText)
beq :: T.Text -> Korigatachi ()
beq oprText = K.do
  let parseBEQ = parseRelative <|> parseLabel
  case Attoparsec.parseOnly parseBEQ oprText of
    Left _ -> K.log K.Warn ("Failed to parse operand: " <> oprText)
    Right (Relative w8) -> K.do
      void $ traverse assembleROM [0xf0, w8]
      K.codeGen (spacing <> "beq " <> oprText)
    Right (Label text) -> K.do
      res <- resolveLabel text
      beq res
    _ -> K.log K.Warn ("Invalid operand for BEQ: " <> oprText)
bit :: T.Text -> Korigatachi ()
bit oprText = K.do
  let parseBIT = parseZeroPage <|> parseAbsolute <|> parseLabel
  case Attoparsec.parseOnly parseBIT oprText of
    Left _ -> K.log K.Warn ("Failed to parse operand: " <> oprText)
    Right (ZeroPage w8) -> K.do
      void $ traverse assembleROM [0x24, w8]
      K.codeGen (spacing <> "bit " <> oprText)
    Right (Absolute ll hh) -> K.do
      void $ traverse assembleROM [0x2c, ll, hh]
      K.codeGen (spacing <> "bit " <> oprText)
    Right (Label text) -> K.do
      res <- resolveLabel text
      bit res
    _ -> K.log K.Warn ("Invalid operand for BIT: " <> oprText)
bmi :: T.Text -> Korigatachi ()
bmi oprText = K.do
  let parseBMI = parseRelative <|> parseLabel
  case Attoparsec.parseOnly parseBMI oprText of
    Left _ -> K.log K.Warn ("Failed to parse operand: " <> oprText)
    Right (Relative w8) -> K.do
      void $ traverse assembleROM [0x30, w8]
      K.codeGen (spacing <> "bmi " <> oprText)
    Right (Label text) -> K.do
      res <- resolveLabel text
      bmi res
    _ -> K.log K.Warn ("Invalid operand for BMI: " <> oprText)
bne :: T.Text -> Korigatachi ()
bne oprText = K.do
  let parseBNE = parseRelative <|> parseLabel
  case Attoparsec.parseOnly parseBNE oprText of
    Left _ -> K.log K.Warn ("Failed to parse operand: " <> oprText)
    Right (Relative w8) -> K.do
      void $ traverse assembleROM [0xd0, w8]
      K.codeGen (spacing <> "bne " <> oprText)
    Right (Label text) -> K.do
      res <- resolveLabel text
      bne res
    _ -> K.log K.Warn ("Invalid operand for BNE: " <> oprText)
bpl :: T.Text -> Korigatachi ()
bpl oprText = K.do
  let parseBPL = parseRelative <|> parseLabel
  case Attoparsec.parseOnly parseBPL oprText of
    Left _ -> K.log K.Warn ("Failed to parse operand: " <> oprText)
    Right (Relative w8) -> K.do
      void $ traverse assembleROM [0x10, w8]
      K.codeGen (spacing <> "bpl " <> oprText)
    Right (Label text) -> K.do
      res <- resolveLabel text
      bpl res
    _ -> K.log K.Warn ("Invalid operand for BPL: " <> oprText)
brk :: Korigatachi ()
brk = K.do
  assembleROM 0x00
bvc :: T.Text -> Korigatachi ()
bvc oprText = K.do
  let parseBVC = parseRelative <|> parseLabel
  case Attoparsec.parseOnly parseBVC oprText of
    Left _ -> K.log K.Warn ("Failed to parse operand: " <> oprText)
    Right (Relative w8) -> K.do
      void $ traverse assembleROM [0x50, w8]
      K.codeGen (spacing <> "bvc " <> oprText)
    Right (Label text) -> K.do
      res <- resolveLabel text
      bvc res
    _ -> K.log K.Warn ("Invalid operand for BVC: " <> oprText)
bvs :: T.Text -> Korigatachi ()
bvs oprText = K.do
  let parseBVS = parseRelative <|> parseLabel
  case Attoparsec.parseOnly parseBVS oprText of
    Left _ -> K.log K.Warn ("Failed to parse operand: " <> oprText)
    Right (Relative w8) -> K.do
      void $ traverse assembleROM [0x70, w8]
      K.codeGen (spacing <> "bvs " <> oprText)
    Right (Label text) -> K.do
      res <- resolveLabel text
      bvs res
    _ -> K.log K.Warn ("Invalid operand for BVS: " <> oprText)
clc :: Korigatachi ()
clc = K.do
  assembleROM 0x18
cld :: Korigatachi ()
cld = K.do
  assembleROM 0xd8
cli :: Korigatachi ()
cli = K.do
  assembleROM 0x58
clv :: Korigatachi ()
clv = K.do
  assembleROM 0xb8
cmp :: T.Text -> Korigatachi ()
cmp oprText = K.do
  let parseCMP = parseIndirectX <|> parseZeroPage <|> parseImmediate <|> parseAbsolute <|> parseIndirectY <|> parseZeroPageX <|> parseAbsoluteY <|> parseAbsoluteX <|> parseLabel
  case Attoparsec.parseOnly parseCMP oprText of
    Left _ -> K.log K.Warn ("Failed to parse operand: " <> oprText)
    Right (IndirectX w8) -> K.do
      void $ traverse assembleROM [0xc1, w8]
      K.codeGen (spacing <> "cmp " <> oprText)
    Right (ZeroPage w8) -> K.do
      void $ traverse assembleROM [0xc5, w8]
      K.codeGen (spacing <> "cmp " <> oprText)
    Right (Immediate w8) -> K.do
      void $ traverse assembleROM [0xc9, w8]
      K.codeGen (spacing <> "cmp " <> oprText)
    Right (Absolute ll hh) -> K.do
      void $ traverse assembleROM [0xcd, ll, hh]
      K.codeGen (spacing <> "cmp " <> oprText)
    Right (IndirectY w8) -> K.do
      void $ traverse assembleROM [0xd1, w8]
      K.codeGen (spacing <> "cmp " <> oprText)
    Right (ZeroPageX w8) -> K.do
      void $ traverse assembleROM [0xd5, w8]
      K.codeGen (spacing <> "cmp " <> oprText)
    Right (AbsoluteY ll hh) -> K.do
      void $ traverse assembleROM [0xd9, ll, hh]
      K.codeGen (spacing <> "cmp " <> oprText)
    Right (AbsoluteX ll hh) -> K.do
      void $ traverse assembleROM [0xdd, ll, hh]
      K.codeGen (spacing <> "cmp " <> oprText)
    Right (Label text) -> K.do
      res <- resolveLabel text
      cmp res
    _ -> K.log K.Warn ("Invalid operand for CMP: " <> oprText)
cpx :: T.Text -> Korigatachi ()
cpx oprText = K.do
  let parseCPX = parseImmediate <|> parseZeroPage <|> parseAbsolute <|> parseLabel
  case Attoparsec.parseOnly parseCPX oprText of
    Left _ -> K.log K.Warn ("Failed to parse operand: " <> oprText)
    Right (Immediate w8) -> K.do
      void $ traverse assembleROM [0xe0, w8]
      K.codeGen (spacing <> "cpx " <> oprText)
    Right (ZeroPage w8) -> K.do
      void $ traverse assembleROM [0xe4, w8]
      K.codeGen (spacing <> "cpx " <> oprText)
    Right (Absolute ll hh) -> K.do
      void $ traverse assembleROM [0xec, ll, hh]
      K.codeGen (spacing <> "cpx " <> oprText)
    Right (Label text) -> K.do
      res <- resolveLabel text
      cpx res
    _ -> K.log K.Warn ("Invalid operand for CPX: " <> oprText)
cpy :: T.Text -> Korigatachi ()
cpy oprText = K.do
  let parseCPY = parseImmediate <|> parseZeroPage <|> parseAbsolute <|> parseLabel
  case Attoparsec.parseOnly parseCPY oprText of
    Left _ -> K.log K.Warn ("Failed to parse operand: " <> oprText)
    Right (Immediate w8) -> K.do
      void $ traverse assembleROM [0xc0, w8]
      K.codeGen (spacing <> "cpy " <> oprText)
    Right (ZeroPage w8) -> K.do
      void $ traverse assembleROM [0xc4, w8]
      K.codeGen (spacing <> "cpy " <> oprText)
    Right (Absolute ll hh) -> K.do
      void $ traverse assembleROM [0xcc, ll, hh]
      K.codeGen (spacing <> "cpy " <> oprText)
    Right (Label text) -> K.do
      res <- resolveLabel text
      cpy res
    _ -> K.log K.Warn ("Invalid operand for CPY: " <> oprText)
dcp :: T.Text -> Korigatachi ()
dcp oprText = K.do
  let parseDCP = parseIndirectX <|> parseZeroPage <|> parseAbsolute <|> parseIndirectY <|> parseZeroPageX <|> parseAbsoluteY <|> parseAbsoluteX <|> parseLabel
  case Attoparsec.parseOnly parseDCP oprText of
    Left _ -> K.log K.Warn ("Failed to parse operand: " <> oprText)
    Right (IndirectX w8) -> K.do
      void $ traverse assembleROM [0xc3, w8]
      K.codeGen (spacing <> "dcp " <> oprText)
    Right (ZeroPage w8) -> K.do
      void $ traverse assembleROM [0xc7, w8]
      K.codeGen (spacing <> "dcp " <> oprText)
    Right (Absolute ll hh) -> K.do
      void $ traverse assembleROM [0xcf, ll, hh]
      K.codeGen (spacing <> "dcp " <> oprText)
    Right (IndirectY w8) -> K.do
      void $ traverse assembleROM [0xd3, w8]
      K.codeGen (spacing <> "dcp " <> oprText)
    Right (ZeroPageX w8) -> K.do
      void $ traverse assembleROM [0xd7, w8]
      K.codeGen (spacing <> "dcp " <> oprText)
    Right (AbsoluteY ll hh) -> K.do
      void $ traverse assembleROM [0xdb, ll, hh]
      K.codeGen (spacing <> "dcp " <> oprText)
    Right (AbsoluteX ll hh) -> K.do
      void $ traverse assembleROM [0xdf, ll, hh]
      K.codeGen (spacing <> "dcp " <> oprText)
    Right (Label text) -> K.do
      res <- resolveLabel text
      dcp res
    _ -> K.log K.Warn ("Invalid operand for DCP: " <> oprText)
dec :: T.Text -> Korigatachi ()
dec oprText = K.do
  let parseDEC = parseZeroPage <|> parseAbsolute <|> parseZeroPageX <|> parseAbsoluteX <|> parseLabel
  case Attoparsec.parseOnly parseDEC oprText of
    Left _ -> K.log K.Warn ("Failed to parse operand: " <> oprText)
    Right (ZeroPage w8) -> K.do
      void $ traverse assembleROM [0xc6, w8]
      K.codeGen (spacing <> "dec " <> oprText)
    Right (Absolute ll hh) -> K.do
      void $ traverse assembleROM [0xce, ll, hh]
      K.codeGen (spacing <> "dec " <> oprText)
    Right (ZeroPageX w8) -> K.do
      void $ traverse assembleROM [0xd6, w8]
      K.codeGen (spacing <> "dec " <> oprText)
    Right (AbsoluteX ll hh) -> K.do
      void $ traverse assembleROM [0xde, ll, hh]
      K.codeGen (spacing <> "dec " <> oprText)
    Right (Label text) -> K.do
      res <- resolveLabel text
      dec res
    _ -> K.log K.Warn ("Invalid operand for DEC: " <> oprText)
dex :: Korigatachi ()
dex = K.do
  assembleROM 0xca
dey :: Korigatachi ()
dey = K.do
  assembleROM 0x88
eor :: T.Text -> Korigatachi ()
eor oprText = K.do
  let parseEOR = parseIndirectX <|> parseZeroPage <|> parseImmediate <|> parseAbsolute <|> parseIndirectY <|> parseZeroPageX <|> parseAbsoluteY <|> parseAbsoluteX <|> parseLabel
  case Attoparsec.parseOnly parseEOR oprText of
    Left _ -> K.log K.Warn ("Failed to parse operand: " <> oprText)
    Right (IndirectX w8) -> K.do
      void $ traverse assembleROM [0x41, w8]
      K.codeGen (spacing <> "eor " <> oprText)
    Right (ZeroPage w8) -> K.do
      void $ traverse assembleROM [0x45, w8]
      K.codeGen (spacing <> "eor " <> oprText)
    Right (Immediate w8) -> K.do
      void $ traverse assembleROM [0x49, w8]
      K.codeGen (spacing <> "eor " <> oprText)
    Right (Absolute ll hh) -> K.do
      void $ traverse assembleROM [0x4d, ll, hh]
      K.codeGen (spacing <> "eor " <> oprText)
    Right (IndirectY w8) -> K.do
      void $ traverse assembleROM [0x51, w8]
      K.codeGen (spacing <> "eor " <> oprText)
    Right (ZeroPageX w8) -> K.do
      void $ traverse assembleROM [0x55, w8]
      K.codeGen (spacing <> "eor " <> oprText)
    Right (AbsoluteY ll hh) -> K.do
      void $ traverse assembleROM [0x59, ll, hh]
      K.codeGen (spacing <> "eor " <> oprText)
    Right (AbsoluteX ll hh) -> K.do
      void $ traverse assembleROM [0x5d, ll, hh]
      K.codeGen (spacing <> "eor " <> oprText)
    Right (Label text) -> K.do
      res <- resolveLabel text
      eor res
    _ -> K.log K.Warn ("Invalid operand for EOR: " <> oprText)
inc :: T.Text -> Korigatachi ()
inc oprText = K.do
  let parseINC = parseZeroPage <|> parseAbsolute <|> parseZeroPageX <|> parseAbsoluteX <|> parseLabel
  case Attoparsec.parseOnly parseINC oprText of
    Left _ -> K.log K.Warn ("Failed to parse operand: " <> oprText)
    Right (ZeroPage w8) -> K.do
      void $ traverse assembleROM [0xe6, w8]
      K.codeGen (spacing <> "inc " <> oprText)
    Right (Absolute ll hh) -> K.do
      void $ traverse assembleROM [0xee, ll, hh]
      K.codeGen (spacing <> "inc " <> oprText)
    Right (ZeroPageX w8) -> K.do
      void $ traverse assembleROM [0xf6, w8]
      K.codeGen (spacing <> "inc " <> oprText)
    Right (AbsoluteX ll hh) -> K.do
      void $ traverse assembleROM [0xfe, ll, hh]
      K.codeGen (spacing <> "inc " <> oprText)
    Right (Label text) -> K.do
      res <- resolveLabel text
      inc res
    _ -> K.log K.Warn ("Invalid operand for INC: " <> oprText)
inx :: Korigatachi ()
inx = K.do
  assembleROM 0xe8
iny :: Korigatachi ()
iny = K.do
  assembleROM 0xc8
isc :: T.Text -> Korigatachi ()
isc oprText = K.do
  let parseISC = parseIndirectX <|> parseZeroPage <|> parseAbsolute <|> parseIndirectY <|> parseZeroPageX <|> parseAbsoluteY <|> parseAbsoluteX <|> parseLabel
  case Attoparsec.parseOnly parseISC oprText of
    Left _ -> K.log K.Warn ("Failed to parse operand: " <> oprText)
    Right (IndirectX w8) -> K.do
      void $ traverse assembleROM [0xe3, w8]
      K.codeGen (spacing <> "isc " <> oprText)
    Right (ZeroPage w8) -> K.do
      void $ traverse assembleROM [0xe7, w8]
      K.codeGen (spacing <> "isc " <> oprText)
    Right (Absolute ll hh) -> K.do
      void $ traverse assembleROM [0xef, ll, hh]
      K.codeGen (spacing <> "isc " <> oprText)
    Right (IndirectY w8) -> K.do
      void $ traverse assembleROM [0xf3, w8]
      K.codeGen (spacing <> "isc " <> oprText)
    Right (ZeroPageX w8) -> K.do
      void $ traverse assembleROM [0xf7, w8]
      K.codeGen (spacing <> "isc " <> oprText)
    Right (AbsoluteY ll hh) -> K.do
      void $ traverse assembleROM [0xfb, ll, hh]
      K.codeGen (spacing <> "isc " <> oprText)
    Right (AbsoluteX ll hh) -> K.do
      void $ traverse assembleROM [0xff, ll, hh]
      K.codeGen (spacing <> "isc " <> oprText)
    Right (Label text) -> K.do
      res <- resolveLabel text
      isc res
    _ -> K.log K.Warn ("Invalid operand for ISC: " <> oprText)
jam :: Korigatachi ()
jam = K.do
  assembleROM 0x02
jmp :: T.Text -> Korigatachi ()
jmp oprText = K.do
  let parseJMP = parseAbsolute <|> parseIndirect <|> parseLabel
  case Attoparsec.parseOnly parseJMP oprText of
    Left _ -> K.log K.Warn ("Failed to parse operand: " <> oprText)
    Right (Absolute ll hh) -> K.do
      void $ traverse assembleROM [0x4c, ll, hh]
      K.codeGen (spacing <> "jmp " <> oprText)
    Right (Indirect ll hh) -> K.do
      void $ traverse assembleROM [0x6c, ll, hh]
      K.codeGen (spacing <> "jmp " <> oprText)
    Right (Label text) -> K.do
      res <- resolveLabel text
      jmp res
    _ -> K.log K.Warn ("Invalid operand for JMP: " <> oprText)
jsr :: T.Text -> Korigatachi ()
jsr oprText = K.do
  let parseJSR = parseAbsolute <|> parseLabel
  case Attoparsec.parseOnly parseJSR oprText of
    Left _ -> K.log K.Warn ("Failed to parse operand: " <> oprText)
    Right (Absolute ll hh) -> K.do
      void $ traverse assembleROM [0x20, ll, hh]
      K.codeGen (spacing <> "jsr " <> oprText)
    Right (Label text) -> K.do
      res <- resolveLabel text
      jsr res
    _ -> K.log K.Warn ("Invalid operand for JSR: " <> oprText)
las :: T.Text -> Korigatachi ()
las oprText = K.do
  let parseLAS = parseAbsoluteY <|> parseLabel
  case Attoparsec.parseOnly parseLAS oprText of
    Left _ -> K.log K.Warn ("Failed to parse operand: " <> oprText)
    Right (AbsoluteY ll hh) -> K.do
      void $ traverse assembleROM [0xbb, ll, hh]
      K.codeGen (spacing <> "las " <> oprText)
    Right (Label text) -> K.do
      res <- resolveLabel text
      las res
    _ -> K.log K.Warn ("Invalid operand for LAS: " <> oprText)
lax :: T.Text -> Korigatachi ()
lax oprText = K.do
  let parseLAX = parseIndirectX <|> parseZeroPage <|> parseAbsolute <|> parseIndirectY <|> parseZeroPageY <|> parseAbsoluteY <|> parseLabel
  case Attoparsec.parseOnly parseLAX oprText of
    Left _ -> K.log K.Warn ("Failed to parse operand: " <> oprText)
    Right (IndirectX w8) -> K.do
      void $ traverse assembleROM [0xa3, w8]
      K.codeGen (spacing <> "lax " <> oprText)
    Right (ZeroPage w8) -> K.do
      void $ traverse assembleROM [0xa7, w8]
      K.codeGen (spacing <> "lax " <> oprText)
    Right (Absolute ll hh) -> K.do
      void $ traverse assembleROM [0xaf, ll, hh]
      K.codeGen (spacing <> "lax " <> oprText)
    Right (IndirectY w8) -> K.do
      void $ traverse assembleROM [0xb3, w8]
      K.codeGen (spacing <> "lax " <> oprText)
    Right (ZeroPageY w8) -> K.do
      void $ traverse assembleROM [0xb7, w8]
      K.codeGen (spacing <> "lax " <> oprText)
    Right (AbsoluteY ll hh) -> K.do
      void $ traverse assembleROM [0xbf, ll, hh]
      K.codeGen (spacing <> "lax " <> oprText)
    Right (Label text) -> K.do
      res <- resolveLabel text
      lax res
    _ -> K.log K.Warn ("Invalid operand for LAX: " <> oprText)
lda :: T.Text -> Korigatachi ()
lda oprText = K.do
  let parseLDA = parseIndirectX <|> parseZeroPage <|> parseImmediate <|> parseAbsolute <|> parseIndirectY <|> parseZeroPageX <|> parseAbsoluteY <|> parseAbsoluteX <|> parseLabel
  case Attoparsec.parseOnly parseLDA oprText of
    Left _ -> K.log K.Warn ("Failed to parse operand: " <> oprText)
    Right (IndirectX w8) -> K.do
      void $ traverse assembleROM [0xa1, w8]
      K.codeGen (spacing <> "lda " <> oprText)
    Right (ZeroPage w8) -> K.do
      void $ traverse assembleROM [0xa5, w8]
      K.codeGen (spacing <> "lda " <> oprText)
    Right (Immediate w8) -> K.do
      void $ traverse assembleROM [0xa9, w8]
      K.codeGen (spacing <> "lda " <> oprText)
    Right (Absolute ll hh) -> K.do
      void $ traverse assembleROM [0xad, ll, hh]
      K.codeGen (spacing <> "lda " <> oprText)
    Right (IndirectY w8) -> K.do
      void $ traverse assembleROM [0xb1, w8]
      K.codeGen (spacing <> "lda " <> oprText)
    Right (ZeroPageX w8) -> K.do
      void $ traverse assembleROM [0xb5, w8]
      K.codeGen (spacing <> "lda " <> oprText)
    Right (AbsoluteY ll hh) -> K.do
      void $ traverse assembleROM [0xb9, ll, hh]
      K.codeGen (spacing <> "lda " <> oprText)
    Right (AbsoluteX ll hh) -> K.do
      void $ traverse assembleROM [0xbd, ll, hh]
      K.codeGen (spacing <> "lda " <> oprText)
    Right (Label text) -> K.do
      res <- resolveLabel text
      lda res
    _ -> K.log K.Warn ("Invalid operand for LDA: " <> oprText)
ldx :: T.Text -> Korigatachi ()
ldx oprText = K.do
  let parseLDX = parseImmediate <|> parseZeroPage <|> parseAbsolute <|> parseZeroPageY <|> parseAbsoluteY <|> parseLabel
  case Attoparsec.parseOnly parseLDX oprText of
    Left _ -> K.log K.Warn ("Failed to parse operand: " <> oprText)
    Right (Immediate w8) -> K.do
      void $ traverse assembleROM [0xa2, w8]
      K.codeGen (spacing <> "ldx " <> oprText)
    Right (ZeroPage w8) -> K.do
      void $ traverse assembleROM [0xa6, w8]
      K.codeGen (spacing <> "ldx " <> oprText)
    Right (Absolute ll hh) -> K.do
      void $ traverse assembleROM [0xae, ll, hh]
      K.codeGen (spacing <> "ldx " <> oprText)
    Right (ZeroPageY w8) -> K.do
      void $ traverse assembleROM [0xb6, w8]
      K.codeGen (spacing <> "ldx " <> oprText)
    Right (AbsoluteY ll hh) -> K.do
      void $ traverse assembleROM [0xbe, ll, hh]
      K.codeGen (spacing <> "ldx " <> oprText)
    Right (Label text) -> K.do
      res <- resolveLabel text
      ldx res
    _ -> K.log K.Warn ("Invalid operand for LDX: " <> oprText)
ldy :: T.Text -> Korigatachi ()
ldy oprText = K.do
  let parseLDY = parseImmediate <|> parseZeroPage <|> parseAbsolute <|> parseZeroPageX <|> parseAbsoluteX <|> parseLabel
  case Attoparsec.parseOnly parseLDY oprText of
    Left _ -> K.log K.Warn ("Failed to parse operand: " <> oprText)
    Right (Immediate w8) -> K.do
      void $ traverse assembleROM [0xa0, w8]
      K.codeGen (spacing <> "ldy " <> oprText)
    Right (ZeroPage w8) -> K.do
      void $ traverse assembleROM [0xa4, w8]
      K.codeGen (spacing <> "ldy " <> oprText)
    Right (Absolute ll hh) -> K.do
      void $ traverse assembleROM [0xac, ll, hh]
      K.codeGen (spacing <> "ldy " <> oprText)
    Right (ZeroPageX w8) -> K.do
      void $ traverse assembleROM [0xb4, w8]
      K.codeGen (spacing <> "ldy " <> oprText)
    Right (AbsoluteX ll hh) -> K.do
      void $ traverse assembleROM [0xbc, ll, hh]
      K.codeGen (spacing <> "ldy " <> oprText)
    Right (Label text) -> K.do
      res <- resolveLabel text
      ldy res
    _ -> K.log K.Warn ("Invalid operand for LDY: " <> oprText)
lsr :: T.Text -> Korigatachi ()
lsr oprText = K.do
  let parseLSR = parseZeroPage <|> parseAccumulator <|> parseAbsolute <|> parseZeroPageX <|> parseAbsoluteX <|> parseLabel
  case Attoparsec.parseOnly parseLSR oprText of
    Left _ -> K.log K.Warn ("Failed to parse operand: " <> oprText)
    Right (ZeroPage w8) -> K.do
      void $ traverse assembleROM [0x46, w8]
      K.codeGen (spacing <> "lsr " <> oprText)
    Right Accumulator-> K.do
      assembleROM 0x4a
      K.codeGen (spacing <> "lsr")
    Right (Absolute ll hh) -> K.do
      void $ traverse assembleROM [0x4e, ll, hh]
      K.codeGen (spacing <> "lsr " <> oprText)
    Right (ZeroPageX w8) -> K.do
      void $ traverse assembleROM [0x56, w8]
      K.codeGen (spacing <> "lsr " <> oprText)
    Right (AbsoluteX ll hh) -> K.do
      void $ traverse assembleROM [0x5e, ll, hh]
      K.codeGen (spacing <> "lsr " <> oprText)
    Right (Label text) -> K.do
      res <- resolveLabel text
      lsr res
    _ -> K.log K.Warn ("Invalid operand for LSR: " <> oprText)
lxa :: T.Text -> Korigatachi ()
lxa oprText = K.do
  let parseLXA = parseImmediate <|> parseLabel
  case Attoparsec.parseOnly parseLXA oprText of
    Left _ -> K.log K.Warn ("Failed to parse operand: " <> oprText)
    Right (Immediate w8) -> K.do
      void $ traverse assembleROM [0xab, w8]
      K.codeGen (spacing <> "lxa " <> oprText)
    Right (Label text) -> K.do
      res <- resolveLabel text
      lxa res
    _ -> K.log K.Warn ("Invalid operand for LXA: " <> oprText)
nop :: T.Text -> Korigatachi ()
nop oprText = K.do
  let parseNOP = parseZeroPage <|> parseAbsolute <|> parseZeroPageX <|> parseImplied <|> parseAbsoluteX <|> parseZeroPageX <|> parseImplied <|> parseAbsoluteX <|> parseZeroPage <|> parseZeroPageX <|> parseImplied <|> parseAbsoluteX <|> parseZeroPage <|> parseZeroPageX <|> parseImplied <|> parseAbsoluteX <|> parseImmediate <|> parseImmediate <|> parseImmediate <|> parseImmediate <|> parseZeroPageX <|> parseImplied <|> parseAbsoluteX <|> parseImmediate <|> parseImmediate <|> parseZeroPageX <|> parseImplied <|> parseAbsoluteX <|> parseLabel
  case Attoparsec.parseOnly parseNOP oprText of
    Left _ -> K.log K.Warn ("Failed to parse operand: " <> oprText)
    Right (ZeroPage w8) -> K.do
      void $ traverse assembleROM [0x04, w8]
      K.codeGen (spacing <> "nop " <> oprText)
    Right (Absolute ll hh) -> K.do
      void $ traverse assembleROM [0x0c, ll, hh]
      K.codeGen (spacing <> "nop " <> oprText)
    Right (ZeroPageX w8) -> K.do
      void $ traverse assembleROM [0x14, w8]
      K.codeGen (spacing <> "nop " <> oprText)
    Right Implied-> K.do
      assembleROM 0x1a
      K.codeGen (spacing <> "nop")
    Right (AbsoluteX ll hh) -> K.do
      void $ traverse assembleROM [0x1c, ll, hh]
      K.codeGen (spacing <> "nop " <> oprText)
    Right (ZeroPageX w8) -> K.do
      void $ traverse assembleROM [0x34, w8]
      K.codeGen (spacing <> "nop " <> oprText)
    Right Implied-> K.do
      assembleROM 0x3a
      K.codeGen (spacing <> "nop")
    Right (AbsoluteX ll hh) -> K.do
      void $ traverse assembleROM [0x3c, ll, hh]
      K.codeGen (spacing <> "nop " <> oprText)
    Right (ZeroPage w8) -> K.do
      void $ traverse assembleROM [0x44, w8]
      K.codeGen (spacing <> "nop " <> oprText)
    Right (ZeroPageX w8) -> K.do
      void $ traverse assembleROM [0x54, w8]
      K.codeGen (spacing <> "nop " <> oprText)
    Right Implied-> K.do
      assembleROM 0x5a
      K.codeGen (spacing <> "nop")
    Right (AbsoluteX ll hh) -> K.do
      void $ traverse assembleROM [0x5c, ll, hh]
      K.codeGen (spacing <> "nop " <> oprText)
    Right (ZeroPage w8) -> K.do
      void $ traverse assembleROM [0x64, w8]
      K.codeGen (spacing <> "nop " <> oprText)
    Right (ZeroPageX w8) -> K.do
      void $ traverse assembleROM [0x74, w8]
      K.codeGen (spacing <> "nop " <> oprText)
    Right Implied-> K.do
      assembleROM 0x7a
      K.codeGen (spacing <> "nop")
    Right (AbsoluteX ll hh) -> K.do
      void $ traverse assembleROM [0x7c, ll, hh]
      K.codeGen (spacing <> "nop " <> oprText)
    Right (Immediate w8) -> K.do
      void $ traverse assembleROM [0x80, w8]
      K.codeGen (spacing <> "nop " <> oprText)
    Right (Immediate w8) -> K.do
      void $ traverse assembleROM [0x82, w8]
      K.codeGen (spacing <> "nop " <> oprText)
    Right (Immediate w8) -> K.do
      void $ traverse assembleROM [0x89, w8]
      K.codeGen (spacing <> "nop " <> oprText)
    Right (Immediate w8) -> K.do
      void $ traverse assembleROM [0xc2, w8]
      K.codeGen (spacing <> "nop " <> oprText)
    Right (ZeroPageX w8) -> K.do
      void $ traverse assembleROM [0xd4, w8]
      K.codeGen (spacing <> "nop " <> oprText)
    Right Implied-> K.do
      assembleROM 0xda
      K.codeGen (spacing <> "nop")
    Right (AbsoluteX ll hh) -> K.do
      void $ traverse assembleROM [0xdc, ll, hh]
      K.codeGen (spacing <> "nop " <> oprText)
    Right (Immediate w8) -> K.do
      void $ traverse assembleROM [0xe2, w8]
      K.codeGen (spacing <> "nop " <> oprText)
    Right (Immediate w8) -> K.do
      void $ traverse assembleROM [0xea, w8]
      K.codeGen (spacing <> "nop " <> oprText)
    Right (ZeroPageX w8) -> K.do
      void $ traverse assembleROM [0xf4, w8]
      K.codeGen (spacing <> "nop " <> oprText)
    Right Implied-> K.do
      assembleROM 0xfa
      K.codeGen (spacing <> "nop")
    Right (AbsoluteX ll hh) -> K.do
      void $ traverse assembleROM [0xfc, ll, hh]
      K.codeGen (spacing <> "nop " <> oprText)
    Right (Label text) -> K.do
      res <- resolveLabel text
      nop res
    _ -> K.log K.Warn ("Invalid operand for NOP: " <> oprText)
ora :: T.Text -> Korigatachi ()
ora oprText = K.do
  let parseORA = parseIndirectX <|> parseZeroPage <|> parseImmediate <|> parseAbsolute <|> parseIndirectY <|> parseZeroPageX <|> parseAbsoluteY <|> parseAbsoluteX <|> parseLabel
  case Attoparsec.parseOnly parseORA oprText of
    Left _ -> K.log K.Warn ("Failed to parse operand: " <> oprText)
    Right (IndirectX w8) -> K.do
      void $ traverse assembleROM [0x01, w8]
      K.codeGen (spacing <> "ora " <> oprText)
    Right (ZeroPage w8) -> K.do
      void $ traverse assembleROM [0x05, w8]
      K.codeGen (spacing <> "ora " <> oprText)
    Right (Immediate w8) -> K.do
      void $ traverse assembleROM [0x09, w8]
      K.codeGen (spacing <> "ora " <> oprText)
    Right (Absolute ll hh) -> K.do
      void $ traverse assembleROM [0x0d, ll, hh]
      K.codeGen (spacing <> "ora " <> oprText)
    Right (IndirectY w8) -> K.do
      void $ traverse assembleROM [0x11, w8]
      K.codeGen (spacing <> "ora " <> oprText)
    Right (ZeroPageX w8) -> K.do
      void $ traverse assembleROM [0x15, w8]
      K.codeGen (spacing <> "ora " <> oprText)
    Right (AbsoluteY ll hh) -> K.do
      void $ traverse assembleROM [0x19, ll, hh]
      K.codeGen (spacing <> "ora " <> oprText)
    Right (AbsoluteX ll hh) -> K.do
      void $ traverse assembleROM [0x1d, ll, hh]
      K.codeGen (spacing <> "ora " <> oprText)
    Right (Label text) -> K.do
      res <- resolveLabel text
      ora res
    _ -> K.log K.Warn ("Invalid operand for ORA: " <> oprText)
pha :: Korigatachi ()
pha = K.do
  assembleROM 0x48
php :: Korigatachi ()
php = K.do
  assembleROM 0x08
pla :: Korigatachi ()
pla = K.do
  assembleROM 0x68
plp :: Korigatachi ()
plp = K.do
  assembleROM 0x28
rla :: T.Text -> Korigatachi ()
rla oprText = K.do
  let parseRLA = parseIndirectX <|> parseZeroPage <|> parseAbsolute <|> parseIndirectY <|> parseZeroPageX <|> parseAbsoluteY <|> parseAbsoluteX <|> parseLabel
  case Attoparsec.parseOnly parseRLA oprText of
    Left _ -> K.log K.Warn ("Failed to parse operand: " <> oprText)
    Right (IndirectX w8) -> K.do
      void $ traverse assembleROM [0x23, w8]
      K.codeGen (spacing <> "rla " <> oprText)
    Right (ZeroPage w8) -> K.do
      void $ traverse assembleROM [0x27, w8]
      K.codeGen (spacing <> "rla " <> oprText)
    Right (Absolute ll hh) -> K.do
      void $ traverse assembleROM [0x2f, ll, hh]
      K.codeGen (spacing <> "rla " <> oprText)
    Right (IndirectY w8) -> K.do
      void $ traverse assembleROM [0x33, w8]
      K.codeGen (spacing <> "rla " <> oprText)
    Right (ZeroPageX w8) -> K.do
      void $ traverse assembleROM [0x37, w8]
      K.codeGen (spacing <> "rla " <> oprText)
    Right (AbsoluteY ll hh) -> K.do
      void $ traverse assembleROM [0x3b, ll, hh]
      K.codeGen (spacing <> "rla " <> oprText)
    Right (AbsoluteX ll hh) -> K.do
      void $ traverse assembleROM [0x3f, ll, hh]
      K.codeGen (spacing <> "rla " <> oprText)
    Right (Label text) -> K.do
      res <- resolveLabel text
      rla res
    _ -> K.log K.Warn ("Invalid operand for RLA: " <> oprText)
rol :: T.Text -> Korigatachi ()
rol oprText = K.do
  let parseROL = parseZeroPage <|> parseAccumulator <|> parseAbsolute <|> parseZeroPageX <|> parseAbsoluteX <|> parseLabel
  case Attoparsec.parseOnly parseROL oprText of
    Left _ -> K.log K.Warn ("Failed to parse operand: " <> oprText)
    Right (ZeroPage w8) -> K.do
      void $ traverse assembleROM [0x26, w8]
      K.codeGen (spacing <> "rol " <> oprText)
    Right Accumulator-> K.do
      assembleROM 0x2a
      K.codeGen (spacing <> "rol")
    Right (Absolute ll hh) -> K.do
      void $ traverse assembleROM [0x2e, ll, hh]
      K.codeGen (spacing <> "rol " <> oprText)
    Right (ZeroPageX w8) -> K.do
      void $ traverse assembleROM [0x36, w8]
      K.codeGen (spacing <> "rol " <> oprText)
    Right (AbsoluteX ll hh) -> K.do
      void $ traverse assembleROM [0x3e, ll, hh]
      K.codeGen (spacing <> "rol " <> oprText)
    Right (Label text) -> K.do
      res <- resolveLabel text
      rol res
    _ -> K.log K.Warn ("Invalid operand for ROL: " <> oprText)
ror :: T.Text -> Korigatachi ()
ror oprText = K.do
  let parseROR = parseZeroPage <|> parseAccumulator <|> parseAbsolute <|> parseZeroPageX <|> parseAbsoluteX <|> parseLabel
  case Attoparsec.parseOnly parseROR oprText of
    Left _ -> K.log K.Warn ("Failed to parse operand: " <> oprText)
    Right (ZeroPage w8) -> K.do
      void $ traverse assembleROM [0x66, w8]
      K.codeGen (spacing <> "ror " <> oprText)
    Right Accumulator-> K.do
      assembleROM 0x6a
      K.codeGen (spacing <> "ror")
    Right (Absolute ll hh) -> K.do
      void $ traverse assembleROM [0x6e, ll, hh]
      K.codeGen (spacing <> "ror " <> oprText)
    Right (ZeroPageX w8) -> K.do
      void $ traverse assembleROM [0x76, w8]
      K.codeGen (spacing <> "ror " <> oprText)
    Right (AbsoluteX ll hh) -> K.do
      void $ traverse assembleROM [0x7e, ll, hh]
      K.codeGen (spacing <> "ror " <> oprText)
    Right (Label text) -> K.do
      res <- resolveLabel text
      ror res
    _ -> K.log K.Warn ("Invalid operand for ROR: " <> oprText)
rra :: T.Text -> Korigatachi ()
rra oprText = K.do
  let parseRRA = parseIndirectX <|> parseZeroPage <|> parseAbsolute <|> parseIndirectY <|> parseZeroPageX <|> parseAbsoluteY <|> parseAbsoluteX <|> parseLabel
  case Attoparsec.parseOnly parseRRA oprText of
    Left _ -> K.log K.Warn ("Failed to parse operand: " <> oprText)
    Right (IndirectX w8) -> K.do
      void $ traverse assembleROM [0x63, w8]
      K.codeGen (spacing <> "rra " <> oprText)
    Right (ZeroPage w8) -> K.do
      void $ traverse assembleROM [0x67, w8]
      K.codeGen (spacing <> "rra " <> oprText)
    Right (Absolute ll hh) -> K.do
      void $ traverse assembleROM [0x6f, ll, hh]
      K.codeGen (spacing <> "rra " <> oprText)
    Right (IndirectY w8) -> K.do
      void $ traverse assembleROM [0x73, w8]
      K.codeGen (spacing <> "rra " <> oprText)
    Right (ZeroPageX w8) -> K.do
      void $ traverse assembleROM [0x77, w8]
      K.codeGen (spacing <> "rra " <> oprText)
    Right (AbsoluteY ll hh) -> K.do
      void $ traverse assembleROM [0x7b, ll, hh]
      K.codeGen (spacing <> "rra " <> oprText)
    Right (AbsoluteX ll hh) -> K.do
      void $ traverse assembleROM [0x7f, ll, hh]
      K.codeGen (spacing <> "rra " <> oprText)
    Right (Label text) -> K.do
      res <- resolveLabel text
      rra res
    _ -> K.log K.Warn ("Invalid operand for RRA: " <> oprText)
rti :: Korigatachi ()
rti = K.do
  assembleROM 0x40
rts :: Korigatachi ()
rts = K.do
  assembleROM 0x60
sax :: T.Text -> Korigatachi ()
sax oprText = K.do
  let parseSAX = parseIndirectX <|> parseZeroPage <|> parseAbsolute <|> parseZeroPageY <|> parseLabel
  case Attoparsec.parseOnly parseSAX oprText of
    Left _ -> K.log K.Warn ("Failed to parse operand: " <> oprText)
    Right (IndirectX w8) -> K.do
      void $ traverse assembleROM [0x83, w8]
      K.codeGen (spacing <> "sax " <> oprText)
    Right (ZeroPage w8) -> K.do
      void $ traverse assembleROM [0x87, w8]
      K.codeGen (spacing <> "sax " <> oprText)
    Right (Absolute ll hh) -> K.do
      void $ traverse assembleROM [0x8f, ll, hh]
      K.codeGen (spacing <> "sax " <> oprText)
    Right (ZeroPageY w8) -> K.do
      void $ traverse assembleROM [0x97, w8]
      K.codeGen (spacing <> "sax " <> oprText)
    Right (Label text) -> K.do
      res <- resolveLabel text
      sax res
    _ -> K.log K.Warn ("Invalid operand for SAX: " <> oprText)
sbc :: T.Text -> Korigatachi ()
sbc oprText = K.do
  let parseSBC = parseIndirectX <|> parseZeroPage <|> parseImmediate <|> parseAbsolute <|> parseIndirectY <|> parseZeroPageX <|> parseAbsoluteY <|> parseAbsoluteX <|> parseLabel
  case Attoparsec.parseOnly parseSBC oprText of
    Left _ -> K.log K.Warn ("Failed to parse operand: " <> oprText)
    Right (IndirectX w8) -> K.do
      void $ traverse assembleROM [0xe1, w8]
      K.codeGen (spacing <> "sbc " <> oprText)
    Right (ZeroPage w8) -> K.do
      void $ traverse assembleROM [0xe5, w8]
      K.codeGen (spacing <> "sbc " <> oprText)
    Right (Immediate w8) -> K.do
      void $ traverse assembleROM [0xe9, w8]
      K.codeGen (spacing <> "sbc " <> oprText)
    Right (Absolute ll hh) -> K.do
      void $ traverse assembleROM [0xed, ll, hh]
      K.codeGen (spacing <> "sbc " <> oprText)
    Right (IndirectY w8) -> K.do
      void $ traverse assembleROM [0xf1, w8]
      K.codeGen (spacing <> "sbc " <> oprText)
    Right (ZeroPageX w8) -> K.do
      void $ traverse assembleROM [0xf5, w8]
      K.codeGen (spacing <> "sbc " <> oprText)
    Right (AbsoluteY ll hh) -> K.do
      void $ traverse assembleROM [0xf9, ll, hh]
      K.codeGen (spacing <> "sbc " <> oprText)
    Right (AbsoluteX ll hh) -> K.do
      void $ traverse assembleROM [0xfd, ll, hh]
      K.codeGen (spacing <> "sbc " <> oprText)
    Right (Label text) -> K.do
      res <- resolveLabel text
      sbc res
    _ -> K.log K.Warn ("Invalid operand for SBC: " <> oprText)
sbx :: T.Text -> Korigatachi ()
sbx oprText = K.do
  let parseSBX = parseImmediate <|> parseLabel
  case Attoparsec.parseOnly parseSBX oprText of
    Left _ -> K.log K.Warn ("Failed to parse operand: " <> oprText)
    Right (Immediate w8) -> K.do
      void $ traverse assembleROM [0xcb, w8]
      K.codeGen (spacing <> "sbx " <> oprText)
    Right (Label text) -> K.do
      res <- resolveLabel text
      sbx res
    _ -> K.log K.Warn ("Invalid operand for SBX: " <> oprText)
sec :: Korigatachi ()
sec = K.do
  assembleROM 0x38
sed :: Korigatachi ()
sed = K.do
  assembleROM 0xf8
sei :: Korigatachi ()
sei = K.do
  assembleROM 0x78
sha :: T.Text -> Korigatachi ()
sha oprText = K.do
  let parseSHA = parseIndirectY <|> parseAbsoluteY <|> parseLabel
  case Attoparsec.parseOnly parseSHA oprText of
    Left _ -> K.log K.Warn ("Failed to parse operand: " <> oprText)
    Right (IndirectY w8) -> K.do
      void $ traverse assembleROM [0x93, w8]
      K.codeGen (spacing <> "sha " <> oprText)
    Right (AbsoluteY ll hh) -> K.do
      void $ traverse assembleROM [0x9f, ll, hh]
      K.codeGen (spacing <> "sha " <> oprText)
    Right (Label text) -> K.do
      res <- resolveLabel text
      sha res
    _ -> K.log K.Warn ("Invalid operand for SHA: " <> oprText)
shy :: T.Text -> Korigatachi ()
shy oprText = K.do
  let parseSHY = parseAbsoluteX <|> parseLabel
  case Attoparsec.parseOnly parseSHY oprText of
    Left _ -> K.log K.Warn ("Failed to parse operand: " <> oprText)
    Right (AbsoluteX ll hh) -> K.do
      void $ traverse assembleROM [0x9c, ll, hh]
      K.codeGen (spacing <> "shy " <> oprText)
    Right (Label text) -> K.do
      res <- resolveLabel text
      shy res
    _ -> K.log K.Warn ("Invalid operand for SHY: " <> oprText)
slo :: T.Text -> Korigatachi ()
slo oprText = K.do
  let parseSLO = parseIndirectX <|> parseZeroPage <|> parseAbsolute <|> parseIndirectY <|> parseZeroPageX <|> parseAbsoluteY <|> parseAbsoluteX <|> parseLabel
  case Attoparsec.parseOnly parseSLO oprText of
    Left _ -> K.log K.Warn ("Failed to parse operand: " <> oprText)
    Right (IndirectX w8) -> K.do
      void $ traverse assembleROM [0x03, w8]
      K.codeGen (spacing <> "slo " <> oprText)
    Right (ZeroPage w8) -> K.do
      void $ traverse assembleROM [0x07, w8]
      K.codeGen (spacing <> "slo " <> oprText)
    Right (Absolute ll hh) -> K.do
      void $ traverse assembleROM [0x0f, ll, hh]
      K.codeGen (spacing <> "slo " <> oprText)
    Right (IndirectY w8) -> K.do
      void $ traverse assembleROM [0x13, w8]
      K.codeGen (spacing <> "slo " <> oprText)
    Right (ZeroPageX w8) -> K.do
      void $ traverse assembleROM [0x17, w8]
      K.codeGen (spacing <> "slo " <> oprText)
    Right (AbsoluteY ll hh) -> K.do
      void $ traverse assembleROM [0x1b, ll, hh]
      K.codeGen (spacing <> "slo " <> oprText)
    Right (AbsoluteX ll hh) -> K.do
      void $ traverse assembleROM [0x1f, ll, hh]
      K.codeGen (spacing <> "slo " <> oprText)
    Right (Label text) -> K.do
      res <- resolveLabel text
      slo res
    _ -> K.log K.Warn ("Invalid operand for SLO: " <> oprText)
sre :: T.Text -> Korigatachi ()
sre oprText = K.do
  let parseSRE = parseIndirectX <|> parseZeroPage <|> parseAbsolute <|> parseIndirectY <|> parseZeroPageX <|> parseAbsoluteY <|> parseAbsoluteX <|> parseLabel
  case Attoparsec.parseOnly parseSRE oprText of
    Left _ -> K.log K.Warn ("Failed to parse operand: " <> oprText)
    Right (IndirectX w8) -> K.do
      void $ traverse assembleROM [0x43, w8]
      K.codeGen (spacing <> "sre " <> oprText)
    Right (ZeroPage w8) -> K.do
      void $ traverse assembleROM [0x47, w8]
      K.codeGen (spacing <> "sre " <> oprText)
    Right (Absolute ll hh) -> K.do
      void $ traverse assembleROM [0x4f, ll, hh]
      K.codeGen (spacing <> "sre " <> oprText)
    Right (IndirectY w8) -> K.do
      void $ traverse assembleROM [0x53, w8]
      K.codeGen (spacing <> "sre " <> oprText)
    Right (ZeroPageX w8) -> K.do
      void $ traverse assembleROM [0x57, w8]
      K.codeGen (spacing <> "sre " <> oprText)
    Right (AbsoluteY ll hh) -> K.do
      void $ traverse assembleROM [0x5b, ll, hh]
      K.codeGen (spacing <> "sre " <> oprText)
    Right (AbsoluteX ll hh) -> K.do
      void $ traverse assembleROM [0x5f, ll, hh]
      K.codeGen (spacing <> "sre " <> oprText)
    Right (Label text) -> K.do
      res <- resolveLabel text
      sre res
    _ -> K.log K.Warn ("Invalid operand for SRE: " <> oprText)
sta :: T.Text -> Korigatachi ()
sta oprText = K.do
  let parseSTA = parseIndirectX <|> parseZeroPage <|> parseAbsolute <|> parseIndirectY <|> parseZeroPageX <|> parseAbsoluteY <|> parseAbsoluteX <|> parseLabel
  case Attoparsec.parseOnly parseSTA oprText of
    Left _ -> K.log K.Warn ("Failed to parse operand: " <> oprText)
    Right (IndirectX w8) -> K.do
      void $ traverse assembleROM [0x81, w8]
      K.codeGen (spacing <> "sta " <> oprText)
    Right (ZeroPage w8) -> K.do
      void $ traverse assembleROM [0x85, w8]
      K.codeGen (spacing <> "sta " <> oprText)
    Right (Absolute ll hh) -> K.do
      void $ traverse assembleROM [0x8d, ll, hh]
      K.codeGen (spacing <> "sta " <> oprText)
    Right (IndirectY w8) -> K.do
      void $ traverse assembleROM [0x91, w8]
      K.codeGen (spacing <> "sta " <> oprText)
    Right (ZeroPageX w8) -> K.do
      void $ traverse assembleROM [0x95, w8]
      K.codeGen (spacing <> "sta " <> oprText)
    Right (AbsoluteY ll hh) -> K.do
      void $ traverse assembleROM [0x99, ll, hh]
      K.codeGen (spacing <> "sta " <> oprText)
    Right (AbsoluteX ll hh) -> K.do
      void $ traverse assembleROM [0x9d, ll, hh]
      K.codeGen (spacing <> "sta " <> oprText)
    Right (Label text) -> K.do
      res <- resolveLabel text
      sta res
    _ -> K.log K.Warn ("Invalid operand for STA: " <> oprText)
stx :: T.Text -> Korigatachi ()
stx oprText = K.do
  let parseSTX = parseZeroPage <|> parseAbsolute <|> parseZeroPageY <|> parseLabel
  case Attoparsec.parseOnly parseSTX oprText of
    Left _ -> K.log K.Warn ("Failed to parse operand: " <> oprText)
    Right (ZeroPage w8) -> K.do
      void $ traverse assembleROM [0x86, w8]
      K.codeGen (spacing <> "stx " <> oprText)
    Right (Absolute ll hh) -> K.do
      void $ traverse assembleROM [0x8e, ll, hh]
      K.codeGen (spacing <> "stx " <> oprText)
    Right (ZeroPageY w8) -> K.do
      void $ traverse assembleROM [0x96, w8]
      K.codeGen (spacing <> "stx " <> oprText)
    Right (Label text) -> K.do
      res <- resolveLabel text
      stx res
    _ -> K.log K.Warn ("Invalid operand for STX: " <> oprText)
sty :: T.Text -> Korigatachi ()
sty oprText = K.do
  let parseSTY = parseZeroPage <|> parseAbsolute <|> parseZeroPageX <|> parseLabel
  case Attoparsec.parseOnly parseSTY oprText of
    Left _ -> K.log K.Warn ("Failed to parse operand: " <> oprText)
    Right (ZeroPage w8) -> K.do
      void $ traverse assembleROM [0x84, w8]
      K.codeGen (spacing <> "sty " <> oprText)
    Right (Absolute ll hh) -> K.do
      void $ traverse assembleROM [0x8c, ll, hh]
      K.codeGen (spacing <> "sty " <> oprText)
    Right (ZeroPageX w8) -> K.do
      void $ traverse assembleROM [0x94, w8]
      K.codeGen (spacing <> "sty " <> oprText)
    Right (Label text) -> K.do
      res <- resolveLabel text
      sty res
    _ -> K.log K.Warn ("Invalid operand for STY: " <> oprText)
tas :: T.Text -> Korigatachi ()
tas oprText = K.do
  let parseTAS = parseAbsoluteY <|> parseLabel
  case Attoparsec.parseOnly parseTAS oprText of
    Left _ -> K.log K.Warn ("Failed to parse operand: " <> oprText)
    Right (AbsoluteY ll hh) -> K.do
      void $ traverse assembleROM [0x9b, ll, hh]
      K.codeGen (spacing <> "tas " <> oprText)
    Right (Label text) -> K.do
      res <- resolveLabel text
      tas res
    _ -> K.log K.Warn ("Invalid operand for TAS: " <> oprText)
tax :: Korigatachi ()
tax = K.do
  assembleROM 0xaa
tay :: Korigatachi ()
tay = K.do
  assembleROM 0xa8
tsx :: Korigatachi ()
tsx = K.do
  assembleROM 0xba
txa :: Korigatachi ()
txa = K.do
  assembleROM 0x8a
txs :: Korigatachi ()
txs = K.do
  assembleROM 0x9a
tya :: Korigatachi ()
tya = K.do
  assembleROM 0x98
ubc :: T.Text -> Korigatachi ()
ubc oprText = K.do
  let parseUBC = parseImmediate <|> parseLabel
  case Attoparsec.parseOnly parseUBC oprText of
    Left _ -> K.log K.Warn ("Failed to parse operand: " <> oprText)
    Right (Immediate w8) -> K.do
      void $ traverse assembleROM [0xeb, w8]
      K.codeGen (spacing <> "ubc " <> oprText)
    Right (Label text) -> K.do
      res <- resolveLabel text
      ubc res
    _ -> K.log K.Warn ("Invalid operand for UBC: " <> oprText)
