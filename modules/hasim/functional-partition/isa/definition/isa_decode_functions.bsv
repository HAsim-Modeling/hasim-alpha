
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
typedef Bit#(4)  FP_OP;
typedef Bit#(4)  FP_MODE;
typedef Bit#(2)  FP_SRC;

FP_SRC fpSrc_S = 2'b00;
FP_SRC fpSrc_T = 2'b10;
FP_SRC fpSrc_Q = 2'b11;
FP_MODE fpMode_IEEE = 4'hb;
FP_MODE fpMode_VAX = 4'ha;


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

// opc14 is floating point, VAX modes will be emulated.
FP_FUNC itofs   = 'h004;
FP_FUNC itoff   = 'h014;
FP_FUNC itoft   = 'h024;
FP_OP   sqrtx   = 'hb;

// opc15 is VAX floating point instructions which are emulated 

// opc16 is IEEE floating point instructions (S and T).
// All are non-emulated. We case on the FP_OP to ease decoding.
// Note that dynamic rounding mode /d has an extra source of FPCR.
// Here x == s || t.
FP_OP addx   = 'h0;
FP_OP subx   = 'h1;
FP_OP mulx   = 'h2;
FP_OP divx   = 'h3;
FP_OP cmpxun  = 'h4;
FP_OP cmpxeq  = 'h5;
FP_OP cmpxlt  = 'h6;
FP_OP cmpxle  = 'h7;
FP_OP cvtxs   = 'hc;
FP_OP cvtxt   = 'he;
FP_OP cvtxq   = 'hf;

// opc17 are floating point instructions which are non-emulated.
FP_FUNC cvtlq   = 'h010;
FP_FUNC cpys    = 'h020;
FP_FUNC cpysn   = 'h021;
FP_FUNC cpyse   = 'h022;
FP_FUNC mt_fpcr = 'h024;
FP_FUNC mf_fpcr = 'h025;
FP_FUNC fcmoveq = 'h02a;
FP_FUNC fcmovne = 'h02b;
FP_FUNC fcmovlt = 'h02c;
FP_FUNC fcmovge = 'h02d;
FP_FUNC fcmovle = 'h02e;
FP_FUNC fcmovgt = 'h02f;
FP_FUNC cvtql   = 'h030;

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



function OPCODE isaGetOpcode(ISA_INSTRUCTION i);
    return i[31:26];
endfunction

function FUNCT isaGetFunct(ISA_INSTRUCTION i);
    return i[11:5];
endfunction

function FP_FUNC isaGetFPFunc(ISA_INSTRUCTION i);
    return i[15:5];
endfunction

function FP_OP isaGetFPOp(ISA_INSTRUCTION i);
    return i[8:5];
endfunction

function FP_SRC isaGetFPSrc(ISA_INSTRUCTION i);
    return i[10:9];
endfunction

function Bool isaIsDefaultFPRounding(ISA_INSTRUCTION i);
    return i[12:11] == 2'b10;
endfunction

function MEM_FUNC isaGetMemFunc(ISA_INSTRUCTION i);
    return signExtend(i[15:0]);
endfunction

function Bit#(64) isaGetMemDisp(ISA_INSTRUCTION i);
    return signExtend(i[15:0]);
endfunction

function Bit#(64) isaGetBranchImmediate(ISA_INSTRUCTION i);
    return signExtend(i[20:0]);
endfunction

function Maybe#(Bit#(64)) isaGetLiteral(ISA_INSTRUCTION i);
    Bool use_lit = unpack(i[12]);
    if (use_lit)
        return tagged Valid (zeroExtend(i[20:13]));
    else
        return tagged Invalid;
endfunction

function Maybe#(ISA_REG_INDEX) isaGetSrc0(ISA_INSTRUCTION i);
    OPCODE    opcode = isaGetOpcode(i);
    Bool      useLit = unpack(i[12]);
    FUNCT      funct = i[11:5];
    MEM_FUNC memFunc = i[15:0];
    FP_FUNC   fpFunc = i[15:5];

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
        ldt, lds, stt, sts,
        jmp:
            ret = tagged Valid (tagged ArchReg rb);

        beq, bge, bgt, blbc, blbs, ble, blt, bne,
        opc10, opc12, opc13:
            ret = tagged Valid (tagged ArchReg ra);

        fbeq, fblt, fble, fbne, fbge, fbgt:
            ret = tagged Valid (tagged FPReg ra);

        opc11:
        begin
            case (funct)
                amask, implver: ret = tagged Invalid;
                default: ret = tagged Valid (tagged ArchReg ra);
            endcase
        end

        opc14:
            case (fpFunc)
                itofs, itoff, itoft:
                    ret = tagged Valid (tagged ArchReg ra);
                default:
                    ret = tagged Valid (tagged FPReg rb);
            endcase

        opc15, opc16:
            ret = tagged Valid (tagged FPReg ra);

        opc17:
            case (fpFunc)
                cvtlq, cvtql:
                    ret = tagged Valid (tagged FPReg rb);
                cpys, cpysn, cpyse:
                    ret = tagged Valid (tagged FPReg ra);
                fcmoveq, fcmovne, fcmovlt, fcmovge, fcmovle, fcmovgt:
                    ret = tagged Valid (tagged FPReg ra);
                mt_fpcr:
                    ret = tagged Valid (tagged FPReg ra); // Actually it's supposed to be in ra, rb, AND rc!
                mf_fpcr:
                    ret = tagged Valid (tagged FPControlReg);
            endcase

        opc1c:
        begin
            case (funct)
                perr:
                    ret = tagged Valid (tagged ArchReg ra);
                ftoit, ftois:
                    ret = tagged Valid (tagged FPReg ra);
                default:
                    ret = tagged Valid (tagged ArchReg rb);
            endcase
        end
    endcase

    return ret;
endfunction

function Maybe#(ISA_REG_INDEX) isaGetSrc1(ISA_INSTRUCTION i);
    OPCODE    opcode = isaGetOpcode(i);
    Bool      useLit = unpack(i[12]);
    FUNCT      funct = i[11:5];
    FP_FUNC   fpFunc = i[15:5];
    MEM_FUNC memFunc = i[15:0];

    let           ra = i[25:21];
    let           rb = i[20:16];
    let           rc = i[4:0];

    Maybe#(ISA_REG_INDEX) ret = tagged Invalid;

    case (opcode)
        stl_c, stq_c, stb, stl, stq, stw, stq_u:
            ret = tagged Valid (tagged ArchReg ra);

        stt, sts:
            ret = tagged Valid (tagged FPReg ra);

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

        opc15, opc16:
            ret = tagged Valid (tagged FPReg rb);

        opc17:
            case (fpFunc)
                cpys, cpysn, cpyse:
                    ret = tagged Valid (tagged FPReg rb);
                fcmoveq, fcmovne, fcmovlt, fcmovge, fcmovle, fcmovgt:
                    ret = tagged Valid (tagged FPReg rb);
            endcase

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
    OPCODE    opcode = isaGetOpcode(i);
    Bool      useLit = unpack(i[12]);
    FUNCT      funct = i[11:5];
    FP_FUNC   fpFunc = i[15:5];
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
                    ret = tagged Valid (tagged ControlReg);
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
                   ret = tagged Valid (tagged ControlReg);
            endcase
        end

        // For opcode 16, bits 12:11 == 'b11 should mean it gets
        // its rounding mode from the FPControlReg. However the
        // reading of this register is very imprecise and must be
        // controlled by barriers. So we should be able to leave it out,
        // since dynamic rounding mode will be emulated.

        opc17:
        begin
            case (fpFunc)
                fcmoveq, fcmovne, fcmovlt, fcmovge, fcmovle, fcmovgt:
                    ret = tagged Valid (tagged FPReg rc);
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
    OPCODE    opcode = isaGetOpcode(i);
    FUNCT      funct = i[11:5];
    FP_FUNC   fpFunc = i[15:5];
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

        ldt, lds:
            ret = tagged Valid (tagged FPReg ra);

        opc10, opc11, opc12, opc13:
            ret = tagged Valid (tagged ArchReg rc);

        ldt, lds, opc14, opc15, opc16:
            ret = tagged Valid (tagged FPReg rc);

        opc17:
            case (fpFunc)
                cvtlq, cpys, cpysn, cpyse, cvtql:
                    ret = tagged Valid (tagged FPReg rc);
                fcmoveq, fcmovne, fcmovlt, fcmovge, fcmovle, fcmovgt:
                    ret = tagged Valid (tagged FPReg rc);
                mt_fpcr:
                    ret = tagged Valid (tagged FPControlReg);
                mf_fpcr:
                    ret = tagged Valid (tagged FPReg rc); // Actually it's supposed to be in ra, rb, AND rc!
            endcase

        opc1c:
        begin
            if ((funct >= 0 && funct < 'h38) || (funct >= 'h70))
                ret = tagged Valid (tagged ArchReg rc);
        end
    endcase

    return ret;

endfunction

function Maybe#(ISA_REG_INDEX) isaGetDst1(ISA_INSTRUCTION i);
    OPCODE    opcode = isaGetOpcode(i);
    FUNCT      funct = i[11:5];
    MEM_FUNC memFunc = i[15:0];
    FP_OP      fp_op = i[8:5];

    let           ra = i[25:21];
    let           rb = i[20:16];
    let           rc = i[4:0];

    Maybe#(ISA_REG_INDEX) ret = tagged Invalid;

    case (opcode)
        ldl_l, ldq_l:
            ret = tagged Valid (tagged LockReg);

        stl_c, stq_c:
            ret = tagged Valid (tagged ArchReg ra);


        opc10:
        begin
            case (funct)
                addlv, addqv, sublv, subqv: ret = tagged Valid (tagged ControlReg);
            endcase
        end

        opc13:
        begin
            case (funct)
                mullv, mulqv: ret = tagged Valid (tagged ControlReg);
            endcase
        end

        opc14:
        begin
            case (fp_op)
                sqrtx: ret = tagged Valid (tagged FPControlReg);
            endcase
        end

        opc16:
        begin
            ret = tagged Valid (tagged FPControlReg);
        end

    endcase

    return ret;

endfunction



function Maybe#(ISA_REG_INDEX) isaGetDst2(ISA_INSTRUCTION i);
    OPCODE    opcode = isaGetOpcode(i);
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
                             tagged FPReg 31: return tagged Invalid;
                             default: return ret;
                         endcase;
               end
               default: return ret;
           endcase;
 
endfunction

// isaGetNumDsts

// Given an instruction, return how many destinations it has.

function Integer isaGetNumDsts(ISA_INSTRUCTION i);

    OPCODE    opcode = isaGetOpcode(i);
    FUNCT      funct = i[11:5];
    FP_FUNC   fpFunc = i[15:5];
    MEM_FUNC memFunc = i[15:0];

    let           ra = i[25:21];
    let           rb = i[20:16];
    let           rc = i[4:0];
    FP_OP      fp_op = i[8:5];

    return case (opcode)
               lda, ldah, ldbu, ldl, ldq, ldwu, ldq_u: return 1;
               ldt, lds: return 1;
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

               opc13:
               begin
                   case (funct)
                       mullv, mulqv: return 2;
                       default: return 1;
                   endcase
               end

               opc14:
               begin
                   case (fp_op)
                       sqrtx: return 2;
                       default: return 1;
                   endcase
               end

               opc15: return 1;
               opc16: return 2;

               opc17:
                   case (fpFunc)
                       cvtlq, cpys, cpysn, cpyse, cvtql: return 1;
                       fcmoveq, fcmovne, fcmovlt, fcmovge, fcmovle, fcmovgt, mt_fpcr, mf_fpcr: return 1;
                       default: return 0;
                   endcase

               opc1c: return ((funct >= 'h38) && (funct < 'h70)) ? 0 : 1;
               default: return 0;
           endcase;
endfunction


// isaIsLoad

// Returns true if the given instruction is a load.

function Bool isaIsLoad(ISA_INSTRUCTION i);

    OPCODE    opcode = isaGetOpcode(i);
    FUNCT      funct = i[11:5];
    MEM_FUNC memFunc = i[15:0];

    let           ra = i[25:21];
    let           rb = i[20:16];
    let           rc = i[4:0];

    return case (opcode)
               ldbu, ldl, ldq, ldwu, ldq_u, ldl_l, ldq_l: return True;
               ldt, lds: return True;
               default: return False; 
           endcase;

endfunction


// isaIsStore

// Returns true if the given instruction is a store.

function Bool isaIsStore(ISA_INSTRUCTION i);

    OPCODE    opcode = isaGetOpcode(i);
    FUNCT      funct = i[11:5];
    MEM_FUNC memFunc = i[15:0];

    let           ra = i[25:21];
    let           rb = i[20:16];
    let           rc = i[4:0];

    return case (opcode)
               stl_c, stq_c, stb, stl, stq, stw, stq_u: return True;
               stt, sts: return True;
               default: return False;
           endcase;

endfunction

// isaLoadType

// Returns the ISA_LOAD_TYPE (which you defined in isa_datatypes.bsv) of a given instruction.
// This will only be called on instructions where isaIsLoad() returns True.

function ISA_MEMOP_TYPE isaLoadType(ISA_INSTRUCTION i);

    OPCODE    opcode = isaGetOpcode(i);
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
               ldt: return LOAD_64;
               lds: return LOAD_CVT_T_32;
               default: return LOAD_64;
           endcase;

endfunction


// isaStoreType

// Returns the ISA_MEMOP_TYPE (which you defined in isa_datatypes.bsv) of a given instruction.
// This will only be called on instructions where isaIsStore() returns True.

function ISA_MEMOP_TYPE isaStoreType(ISA_INSTRUCTION i);

    OPCODE    opcode = isaGetOpcode(i);
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
               stt: return STORE_64;
               sts: return STORE_32;
               default: return STORE_64;
           endcase;

endfunction


// isaIsBranch

// Returns true if the given instruction is a branch.

function Bool isaIsBranch(ISA_INSTRUCTION i);

    OPCODE    opcode = isaGetOpcode(i);
    Bool      useLit = unpack(i[12]);
    FUNCT      funct = i[11:5];
    MEM_FUNC memFunc = i[15:0];

    let           ra = i[25:21];
    let           rb = i[20:16];
    let           rc = i[4:0];

    return case (opcode)
               br, bsr, jmp, beq, bge, bgt, blbc, blbs, ble, blt, bne: True;
               fbeq, fblt, fble, fbne, fbge, fbgt: True;
               default: return False;
           endcase;

endfunction


// isaDrainBefore

// Returns true if the timing model should drain the pipeline before executing this
// instuction.

// Note that both isaDrainBefore() and isaDrainAfter() may be true for a given instruction.

function Bool isaDrainBefore(ISA_INSTRUCTION i);

    OPCODE    opcode = isaGetOpcode(i);
    MEM_FUNC   funct = i[15:0];
    return case (opcode)
        opc18:
            return case (funct)
                trapb, excb, mb, wmb: // Barrier instructions drain the pipeline for now. This is a bit conservative.
                    True;
                default:
                    isaEmulateInstruction(i); // For now we drain before and after every emulated instruction.
            endcase;
        default:
            isaEmulateInstruction(i); // For now we drain before and after every emulated instruction.
    endcase;

endfunction


// isaDrainAfter

// Returns true if the timing model should drain the pipeline after executing this
// instruction.

// Note that both isaDrainBefore() and isaDrainAfter() may be true for a given instruction.

function Bool isaDrainAfter(ISA_INSTRUCTION i);

    OPCODE    opcode = isaGetOpcode(i);
    MEM_FUNC   funct = i[15:0];
    return case (opcode)
        opc18:
            return case (funct)
                trapb, excb, mb, wmb: // Barrier instructions drain the pipeline for now. This is a bit conservative.
                    True;
                default:
                    isaEmulateInstruction(i); // For now we drain before and after every emulated instruction.
            endcase;
        default:
            isaEmulateInstruction(i); // For now we drain before and after every emulated instruction.
    endcase;
endfunction


// isaEmulateInstruction

// Returns true if the given instruction should be emulated in software.

function Bool isaEmulateInstruction(ISA_INSTRUCTION i);

    OPCODE    opcode = isaGetOpcode(i);
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
               pal19, pal1d, pal1e, pal1f: return True;

               // VAX floating point loads/stores
               ldf, ldg, stf, stg: return True;

               opc01: return (memFunc != exit);

               // Most of opc18 functions have no functional side effects
               // and can be treated as NOPs.
               opc18: return (memFunc == rpcc);

               // TODO implement the rest ( except floating point )
               opc1c: return ((funct >= 'h38) && (funct < 'h70));

               default: return False;
           endcase;

endfunction

function Bool isBranchImm(ISA_INSTRUCTION inst);
    let opcode = inst[31:26];
    return case (opcode)
               beq, bge, bgt, blbc, blbs, ble, blt, bne: True;
               fbeq, fblt, fble, fbne, fbge, fbgt: True;
               default: return False;
           endcase;
endfunction

function ISA_ADDRESS predPcBranchImm(ISA_ADDRESS addr, ISA_INSTRUCTION inst);
    let    opcode = inst[31:26];
    let branchImm = inst[20:0];
    return addr + 4 + (signExtend(branchImm) << 2);
endfunction

function Bool isJumpImm(ISA_INSTRUCTION inst);
    let opcode = inst[31:26];
    return opcode == br || opcode == bsr;
endfunction

function ISA_ADDRESS predPcJumpImm(ISA_ADDRESS addr, ISA_INSTRUCTION inst);
    let    opcode = inst[31:26];
    let branchImm = inst[20:0];
    return addr + 4 + (signExtend(branchImm) << 2);
endfunction
