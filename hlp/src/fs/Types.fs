namespace ARMSim
module Types =  
    type RegName= |R0 |R1 |R2 | R3 |R4 | R5 |R6 | R7 |R8 | R9 |R10 | R11 |R12 | R13 |PC |LR |WRONG of string
    //type Inst = |MOV|MVN|ADR|ADD|ADC|SUB|SBC|RSB|RSC|AND|EOR|BIC|ORR|LSL|LSR|ASR|ROR|RRX|CMP|CMN|TST|TEQ|LDR|STR|BL|B|DCD|EQU|FILL|END
    type Registers= Map<RegName,int32>
    type Cond = |EQ |NE |CS |HI |LS |GE |LT |GT |LE
    type S = bool
    //type littype = |BIN |DEC |HEX
    type RegorLiteral = |RName of RegName |Number of int32
    type RegorLit = RegorLiteral option
    type FlagName = N | C | S |V
    type FlagVal= High | Low
    type Flags= Map<FlagName,FlagVal>
    type Address= Address of int32
    type Memory = Map<Address,int32>
  //Add more as we go on
    type Instruction = 
        |MOV of  S * Cond option * RegName * RegorLiteral
        |ADD of  S * Cond option * RegName * RegName *RegorLiteral
        |ADC of  S * Cond option * RegName * RegName *RegorLiteral
        |SUB of  S * Cond option * RegName * RegName *RegorLiteral
        |SBC of  S * Cond option * RegName * RegName *RegorLiteral  //had to break it down earlier cause i felt it was too verbose 
    type Instructions = (int32*Instruction) list
    type MachineState = Registers * Flags* Memory



