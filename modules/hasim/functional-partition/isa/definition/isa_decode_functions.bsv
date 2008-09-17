
// isa_decode_functions

// This file contains functions which decode an architectural instruction for both the
// functional and timing partition.

// TODO: Support decoding variable-width instructions.

// isaGetSrc

// Given an instruction, return the nth source register.
// Or return Invalid if there is no such source for this instruction.

typedef Bit#(6) OPCODE;
typedef Bit#(7) FUNCT;
typedef Bit#(16) MEM_FUNC;
typedef Bit#(11) FP_FUNC;

OPCODE call_pal = 'h00;
OPCODE opc01    = 'h01;
OPCODE opc02    = 'h02;
OPCODE opc03    = 'h03;
OPCODE opc04    = 'h04;
OPCODE opc05    = 'h05;
OPCODE opc06    = 'h06;
OPCODE opc07    = 'h07;
OPCODE lda      = 'h08;
OPCODE ldah     = 'h09;
OPCODE ldbu     = 'h0a;
OPCODE ldq_u    = 'h0b;
OPCODE ldwu     = 'h0c;
OPCODE stw      = 'h0d;
OPCODE stb      = 'h0e;
OPCODE stq_u    = 'h0f;
OPCODE opc10    = 'h10;
OPCODE opc11    = 'h11;
OPCODE opc12    = 'h12;
OPCODE opc13    = 'h13;
OPCODE opc14    = 'h14;
OPCODE opc15    = 'h15;
OPCODE opc16    = 'h16;
OPCODE opc17    = 'h17;
OPCODE opc18    = 'h18;
OPCODE pal19    = 'h19;
OPCODE jmp      = 'h1a;
OPCODE pal1b    = 'h1b;
OPCODE opc1c    = 'h1c;
OPCODE pal1d    = 'h1d;
OPCODE pal1e    = 'h1e;
OPCODE pal1f    = 'h1f;
OPCODE ldf      = 'h20;
OPCODE ldg      = 'h21;
OPCODE lds      = 'h22;
OPCODE ldt      = 'h23;
OPCODE stf      = 'h24;
OPCODE stg      = 'h25;
OPCODE sts      = 'h26;
OPCODE stt      = 'h27;
OPCODE ldl      = 'h28;
OPCODE ldq      = 'h29;
OPCODE ldl_l    = 'h2a;
OPCODE ldq_l    = 'h2b;
OPCODE stl      = 'h2c;
OPCODE stq      = 'h2d;
OPCODE stl_c    = 'h2e;
OPCODE stq_c    = 'h2f;
OPCODE br       = 'h30;
OPCODE fbeq     = 'h31;
OPCODE fblt     = 'h32;
OPCODE fble     = 'h33;
OPCODE bsr      = 'h34;
OPCODE fbne     = 'h35;
OPCODE fbge     = 'h36;
OPCODE fbgt     = 'h37;
OPCODE blbc     = 'h38;
OPCODE beq      = 'h39;
OPCODE blt      = 'h3a;
OPCODE ble      = 'h3b;
OPCODE blbs     = 'h3c;
OPCODE bne      = 'h3d;
OPCODE bge      = 'h3e;
OPCODE bgt      = 'h3f;

// opc01
MEM_FUNC exit   = 'h21;

// opc10
FUNCT addl      = 'h00;
FUNCT s4addl    = 'h02;
FUNCT subl      = 'h09;
FUNCT s4subl    = 'h0b;
FUNCT cmpbge    = 'h0f;
FUNCT s8addl    = 'h12;
FUNCT s8subl    = 'h1b;
FUNCT cmpult    = 'h1d;
FUNCT addq      = 'h20;
FUNCT s4addq    = 'h22;
FUNCT subq      = 'h29;
FUNCT s4subq    = 'h2b;
FUNCT cmpeq     = 'h2d;
FUNCT s8addq    = 'h32;
FUNCT s8subq    = 'h3b;
FUNCT cmpule    = 'h3d;
FUNCT addlv     = 'h40;
FUNCT sublv     = 'h49;
FUNCT cmplt     = 'h4d;
FUNCT addqv     = 'h60;
FUNCT subqv     = 'h69;
FUNCT cmple     = 'h6d;

// opc11
FUNCT andOp     = 'h00;
FUNCT bic       = 'h08;
FUNCT cmovlbs   = 'h14;
FUNCT cmovlbc   = 'h16;
FUNCT bis       = 'h20;
FUNCT cmoveq    = 'h24;
FUNCT cmovne    = 'h26;
FUNCT ornot     = 'h28;
FUNCT xorOp     = 'h40;
FUNCT cmovlt    = 'h44;
FUNCT cmovge    = 'h46;
FUNCT eqv       = 'h48;
FUNCT amask     = 'h61;
FUNCT cmovle    = 'h64;
FUNCT cmovgt    = 'h66;
FUNCT implver   = 'h6c;

// opc12
FUNCT mskbl     = 'h02;
FUNCT extbl     = 'h06;
FUNCT insbl     = 'h0b;
FUNCT mskwl     = 'h12;
FUNCT extwl     = 'h16;
FUNCT inswl     = 'h1b;
FUNCT mskll     = 'h22;
FUNCT extll     = 'h26;
FUNCT insll     = 'h2b;
FUNCT zap       = 'h30;
FUNCT zapnot    = 'h31;
FUNCT mskql     = 'h32;
FUNCT srl       = 'h34;
FUNCT extql     = 'h36;
FUNCT sll       = 'h39;
FUNCT insql     = 'h3b;
FUNCT sra       = 'h3c;
FUNCT mskwh     = 'h52;
FUNCT inswh     = 'h57;
FUNCT extwh     = 'h5a;
FUNCT msklh     = 'h62;
FUNCT inslh     = 'h67;
FUNCT extlh     = 'h6a;
FUNCT mskqh     = 'h72;
FUNCT insqh     = 'h77;
FUNCT extqh     = 'h7a;

// opc13
FUNCT mull      = 'h00;
FUNCT mulq      = 'h20;
FUNCT umulh     = 'h30;
FUNCT mullv     = 'h40;
FUNCT mulqv     = 'h60;

// opc14 opc15 opc16 opc17 are floating point instructions. Not implemented yet.
FP_FUNC cpys    = 'h020;        // opc17.cpys with f31 target is fnop

// opc18
MEM_FUNC trapb  = 'h0000;
MEM_FUNC excb   = 'h0400;
MEM_FUNC mb     = 'h4000;
MEM_FUNC wmb    = 'h4400;
MEM_FUNC fetch  = 'h8000;
MEM_FUNC fetch_m= 'ha000;
MEM_FUNC rpcc   = 'hc000;
MEM_FUNC rc     = 'he000;
MEM_FUNC ecb    = 'he800;
MEM_FUNC rs     = 'hf000;
MEM_FUNC wh64   = 'hf800;

// opc1c
FUNCT sextb     = 'h00;
FUNCT sextw     = 'h01;
FUNCT ctpop     = 'h30;
FUNCT perr      = 'h31;
FUNCT ctlz      = 'h32;
FUNCT cttz      = 'h33;
FUNCT unpkbw    = 'h34;
FUNCT unpkbl    = 'h35;
FUNCT pkwb      = 'h36;
FUNCT pklb      = 'h37;
FUNCT minsb8    = 'h38;   // TODO have to implement the rest from here
FUNCT minsw4    = 'h39;
FUNCT minub8    = 'h3a;
FUNCT minuw4    = 'h3b;
FUNCT maxub8    = 'h3c;
FUNCT maxuw4    = 'h3d;
FUNCT maxsb8    = 'h3e;
FUNCT maxsw4    = 'h3f;
FUNCT ftoit     = 'h70;
FUNCT ftois     = 'h78;

function Maybe#(ISA_REG_INDEX) isaGetSrc0(ISA_INSTRUCTION i);
    OPCODE    opcode = i[31:26];
    Bool      useLit = unpack(i[12]);
    FUNCT      funct = i[11:5];
    MEM_FUNC memFunc = i[15:0];

    let           ra = i[25:21];
    let           rb = i[20:16];
    let           rc = i[4:0];

    Maybe#(ISA_REG_INDEX) ret = tagged Invalid;

    case (opcode)
        opc01:
        begin
            case (memFunc)
                exit:
                    ret = tagged Valid (tagged ArchReg ra);
            endcase
        end
        lda, ldah, ldbu, ldl, ldq, ldwu, ldq_u, ldl_l, ldq_l,
        stl_c, stq_c, stb, stl, stq, stw, stq_u,
        jmp:
            ret = tagged Valid (tagged ArchReg rb);

        beq, bge, bgt, blbc, blbs, ble, blt, bne,
        opc10, opc12, opc13:
            ret = tagged Valid (tagged ArchReg ra);

        opc11:
        begin
            case (funct)
                amask, implver: ret = tagged Invalid;
                default: ret = tagged Valid (tagged ArchReg ra);
            endcase
        end

        opc1c:
        begin
            case (funct)
                perr:
                    ret = tagged Valid (tagged ArchReg ra);
                default:
                    ret = tagged Valid (tagged ArchReg rb);
            endcase
        end
    endcase

    return ret;
endfunction

function Maybe#(ISA_REG_INDEX) isaGetSrc1(ISA_INSTRUCTION i);
    OPCODE    opcode = i[31:26];
    Bool      useLit = unpack(i[12]);
    FUNCT      funct = i[11:5];
    MEM_FUNC memFunc = i[15:0];

    let           ra = i[25:21];
    let           rb = i[20:16];
    let           rc = i[4:0];

    Maybe#(ISA_REG_INDEX) ret = tagged Invalid;

    case (opcode)
        stl_c, stq_c, stb, stl, stq, stw, stq_u:
            ret = tagged Valid (tagged ArchReg ra);

        opc10, opc12, opc13:
        begin
            if(!useLit)
                ret = tagged Valid (tagged ArchReg rb);
        end

        opc11:
        begin
            case (funct)
                default:
                begin
                    if(!useLit)
                        ret = tagged Valid (tagged ArchReg rb);
                end
            endcase
        end

        opc1c:
        begin
            case (funct)
                perr:
                    ret = tagged Valid (tagged ArchReg rb);
            endcase
        end
    endcase

    return ret;
endfunction

function Maybe#(ISA_REG_INDEX) isaGetSrc2(ISA_INSTRUCTION i);
    OPCODE    opcode = i[31:26];
    Bool      useLit = unpack(i[12]);
    FUNCT      funct = i[11:5];
    MEM_FUNC memFunc = i[15:0];

    let           ra = i[25:21];
    let           rb = i[20:16];
    let           rc = i[4:0];

    Maybe#(ISA_REG_INDEX) ret = tagged Invalid;

    case (opcode)
        stl_c, stq_c:
            ret = tagged Valid (tagged LockReg);

        opc10:
        begin
            case (funct)
                addlv, addqv, sublv, subqv:
                    ret = tagged Valid (tagged ControlReg); // TODO must properly write this
            endcase
        end

        opc11:
        begin
            case (funct)
                cmovlbs, cmovlbc, cmoveq, cmovne, cmovlt, cmovge, cmovle, cmovgt:
                    ret = tagged Valid (tagged ArchReg rc);
            endcase
        end

        opc13:
        begin
            case (funct)
                mullv, mulqv:
                   ret = tagged Valid (tagged ControlReg); // TODO must properly write this
            endcase
        end
    endcase

    return ret;
endfunction

function Maybe#(ISA_REG_INDEX) isaGetSrc(ISA_INSTRUCTION i, Integer n);

    return case (n)
               0: return isaGetSrc0(i);
               1: return isaGetSrc1(i);
               2: return isaGetSrc2(i);
               default: return tagged Invalid; 
           endcase;

endfunction

function Maybe#(ISA_REG_INDEX) isaGetDst0(ISA_INSTRUCTION i);
    OPCODE    opcode = i[31:26];
    FUNCT      funct = i[11:5];
    MEM_FUNC memFunc = i[15:0];

    let           ra = i[25:21];
    let           rb = i[20:16];
    let           rc = i[4:0];

    Maybe#(ISA_REG_INDEX) ret = tagged Invalid;

    case (opcode)
        lda, ldah, ldbu, ldl, ldq, ldwu, ldq_u,
        ldl_l, ldq_l,
        br, bsr, jmp:
            ret = tagged Valid (tagged ArchReg ra);

        opc10, opc11, opc12:
            ret = tagged Valid (tagged ArchReg rc);

`ifdef HW_MULTIPLY
        opc13:
            ret = tagged Valid (tagged ArchReg rc);
`endif

        opc1c:
        begin
            if (funct < 'h38) ret = tagged Valid (tagged ArchReg rc);
        end
    endcase

    return ret;

endfunction

function Maybe#(ISA_REG_INDEX) isaGetDst1(ISA_INSTRUCTION i);
    OPCODE    opcode = i[31:26];
    FUNCT      funct = i[11:5];
    MEM_FUNC memFunc = i[15:0];

    let           ra = i[25:21];
    let           rb = i[20:16];
    let           rc = i[4:0];

    Maybe#(ISA_REG_INDEX) ret = tagged Invalid;

    case (opcode)
        ldl_l, ldq_l:
            ret = tagged Valid (tagged LockReg);

        stl_c, stq_c:
            ret = tagged Valid (tagged ArchReg ra);

        opc10, opc11, opc12, opc13:
        begin
            case (opcode)
                opc10:
                begin
                    case (funct)
                        addlv, addqv, sublv, subqv: ret = tagged Valid (tagged ControlReg); // TODO correct this
                    endcase
                end

`ifdef HW_MULTIPLY
                opc13:
                begin
                    case (funct)
                        mullv, mulqv: ret = tagged Valid (tagged ControlReg);
                    endcase
                end
`endif
            endcase
        end
    endcase

    return ret;

endfunction



function Maybe#(ISA_REG_INDEX) isaGetDst2(ISA_INSTRUCTION i);
    OPCODE    opcode = i[31:26];
    FUNCT      funct = i[11:5];
    MEM_FUNC memFunc = i[15:0];

    let           ra = i[25:21];
    let           rb = i[20:16];
    let           rc = i[4:0];

    Maybe#(ISA_REG_INDEX) ret = tagged Invalid;

    case (opcode)
        ldl_l, ldq_l:
            ret = tagged Valid (tagged LockAddrReg);

         stl_c, stq_c:
            ret = tagged Valid (tagged LockReg);
    endcase

    return ret;

endfunction



// isaGetDst


// Given an instruction, return the nth destination register.
// Or return Invalid if there is no such destination for this instruction.

function Maybe#(ISA_REG_INDEX) isaGetDst(ISA_INSTRUCTION i, Integer n);

    let ret = case (n)
                   0: return isaGetDst0(i);
                   1: return isaGetDst1(i);
                   2: return isaGetDst2(i);
                   default: return tagged Invalid; 
              endcase;

    return case (ret) matches
               tagged Valid .v:
               begin
                  return case (v) matches
                             tagged ArchReg 31: return tagged Invalid;
                             default: return ret;
                         endcase;
               end
               default: return ret;
           endcase;
 
endfunction

// isaGetNumDsts

// Given an instruction, return how many destinations it has.

function Integer isaGetNumDsts(ISA_INSTRUCTION i);

    OPCODE    opcode = i[31:26];
    FUNCT      funct = i[11:5];
    MEM_FUNC memFunc = i[15:0];

    let           ra = i[25:21];
    let           rb = i[20:16];
    let           rc = i[4:0];

    return case (opcode)
               lda, ldah, ldbu, ldl, ldq, ldwu, ldq_u: return 1;
               ldl_l, ldq_l: return 3;
               stl_c, stq_c: return 3;
               br, bsr, jmp: return 1;
               opc10:
               begin
                   case (funct)
                       addlv, addqv, sublv, subqv: return 2;
                       default: return 1;
                   endcase
               end
               opc11: return 1;
               opc12: return 1;

`ifdef HW_MULTIPLY
               opc13:
               begin
                   case (funct)
                       mullv, mulqv: return 2;
                       default: return 1;
                   endcase
               end
`endif

               opc1c: return (funct >= 'h38) ? 0 : 1;
               default: return 0;
           endcase;
endfunction


// isaIsLoad

// Returns true if the given instruction is a load.

function Bool isaIsLoad(ISA_INSTRUCTION i);

    OPCODE    opcode = i[31:26];
    FUNCT      funct = i[11:5];
    MEM_FUNC memFunc = i[15:0];

    let           ra = i[25:21];
    let           rb = i[20:16];
    let           rc = i[4:0];

    return case (opcode)
               ldbu, ldl, ldq, ldwu, ldq_u, ldl_l, ldq_l: return True;
               default: return False; 
           endcase;

endfunction


// isaIsStore

// Returns true if the given instruction is a store.

function Bool isaIsStore(ISA_INSTRUCTION i);

    OPCODE    opcode = i[31:26];
    FUNCT      funct = i[11:5];
    MEM_FUNC memFunc = i[15:0];

    let           ra = i[25:21];
    let           rb = i[20:16];
    let           rc = i[4:0];

    return case (opcode)
               stl_c, stq_c, stb, stl, stq, stw, stq_u: return True;
               default: return False;
           endcase;

endfunction

// isaLoadType

// Returns the ISA_LOAD_TYPE (which you defined in isa_datatypes.bsv) of a given instruction.
// This will only be called on instructions where isaIsLoad() returns True.

function ISA_MEMOP_TYPE isaLoadType(ISA_INSTRUCTION i);

    OPCODE    opcode = i[31:26];
    Bool      useLit = unpack(i[12]);
    FUNCT      funct = i[11:5];
    MEM_FUNC memFunc = i[15:0];

    let           ra = i[25:21];
    let           rb = i[20:16];
    let           rc = i[4:0];

    return case (opcode)
               ldbu: return LOAD_ZERO_8;
               ldl: return LOAD_SIGN_32;
               ldq: return LOAD_64;
               ldwu: return LOAD_ZERO_16;
               ldq_u: return LOAD_UNALIGNED_64;
               ldl_l: return LOAD_SIGN_32;
               ldq_l: return LOAD_64;
               default: return LOAD_64;
           endcase;

endfunction


// isaStoreType

// Returns the ISA_MEMOP_TYPE (which you defined in isa_datatypes.bsv) of a given instruction.
// This will only be called on instructions where isaIsStore() returns True.

function ISA_MEMOP_TYPE isaStoreType(ISA_INSTRUCTION i);

    OPCODE    opcode = i[31:26];
    Bool      useLit = unpack(i[12]);
    FUNCT      funct = i[11:5];
    MEM_FUNC memFunc = i[15:0];

    let           ra = i[25:21];
    let           rb = i[20:16];
    let           rc = i[4:0];

    return case (opcode)
               stl_c: return STORE_32;
               stq_c: return STORE_64;
               stb: return STORE_8;
               stl: return STORE_32;
               stq: return STORE_64;
               stw: return STORE_16;
               default: return STORE_64;
           endcase;

endfunction


// isaIsBranch

// Returns true if the given instruction is a branch.

function Bool isaIsBranch(ISA_INSTRUCTION i);

    OPCODE    opcode = i[31:26];
    Bool      useLit = unpack(i[12]);
    FUNCT      funct = i[11:5];
    MEM_FUNC memFunc = i[15:0];

    let           ra = i[25:21];
    let           rb = i[20:16];
    let           rc = i[4:0];

    return case (opcode)
               br, bsr, jmp, beq, bge, bgt, blbc, blbs, ble, blt, bne: True;
               default: return False;
           endcase;

endfunction


// isaDrainBefore

// Returns true if the timing model should drain the pipeline before executing this
// instuction.

// Note that both isaDrainBefore() and isaDrainAfter() may be true for a given instruction.

function Bool isaDrainBefore(ISA_INSTRUCTION i);

    return isaEmulateInstruction(i); // For now we drain before and after every emulated instruction.

endfunction


// isaDrainAfter

// Returns true if the timing model should drain the pipeline after executing this
// instruction.

// Note that both isaDrainBefore() and isaDrainAfter() may be true for a given instruction.

function Bool isaDrainAfter(ISA_INSTRUCTION i);

    return isaEmulateInstruction(i); // For now we drain before and after every emulated instruction.

endfunction


// isaEmulateInstruction

// Returns true if the given instruction should be emulated in software.

function Bool isaEmulateInstruction(ISA_INSTRUCTION i);

    OPCODE    opcode = i[31:26];
    Bool      useLit = unpack(i[12]);
    FUNCT      funct = i[11:5];
    MEM_FUNC memFunc = i[15:0];
    FP_FUNC   fpFunc = i[15:5];

    let           ra = i[25:21];
    let           rb = i[20:16];
    let           rc = i[4:0];

    return case (opcode)

               // No clue category. TODO look if any of these can be implemented
               call_pal, opc02, opc03, opc04, opc05, opc07: return True;
               opc18: return True;
               pal19, pal1d, pal1e, pal1f: return True;

               // Expensive to implement
               // ldq_u, stq_u: return True;
               // ldl_l, ldq_l, stl_c, stq_c: return True;

               // Floating point
               ldf, ldg, lds, ldt, stf, stg, sts, stt: return True;
               opc14, opc15, opc16: return True;

               // opc17.cpys to f31 is a fnop, otherwise emulate
               opc17: return ((fpFunc != cpys) || (rc != 31));

               fbeq, fblt, fble, fbne, fbge, fbgt: return True;

               opc01: return (memFunc != exit);

               // TODO implement the rest ( except floating point )
               opc1c: return (funct >= 'h38);

`ifndef HW_MULTIPLY
               opc13: return True;
`endif

               default: return False;
           endcase;

endfunction
