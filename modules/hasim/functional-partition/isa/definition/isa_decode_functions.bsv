
// isa_decode_functions

// This file contains functions which decode an architectural instruction for both the
// functional and timing partition.

// TODO: Support decoding variable-width instructions.

// isaGetSrc

// Given an instruction, return the nth source register.
// Or return Invalid if there is no such source for this instruction.

typedef Bit#(6) Opcode;
typedef Bit#(7) Funct;
typedef Bit#(16) MemImm;

Opcode opcode01 = 'h01;
Opcode br = 'h39;
Opcode bsr = 'h34;
Opcode beq = 'h39;
Opcode bge = 'h3e;
Opcode bgt ='h3f;
Opcode blbc = 'h38;
Opcode blbs = 'h3c;
Opcode ble = 'h3b;
Opcode blt = 'h3a;
Opcode bne = 'h3d;
Opcode jmp = 'h1a;
Opcode lda = 'h08;
Opcode ldl = 'h28;
Opcode ldq = 'h29;
Opcode ldwu = 'h0c;
Opcode ldbu = 'h0a;
Opcode ldq_u = 'h0b;
Opcode arith = 'h10;
Opcode logical = 'h11;
Opcode byteManipulation = 'h12;

Funct addl = 'h00;
Funct s4addl = 'h02;
Funct s8addl = 'h12;
Funct addq = 'h20;
Funct s4addq = 'h22;
Funct s8addq = 'h32;
Funct cmpeq = 'h2d;
Funct cmple = 'h6d;
Funct cmplt = 'h4d;
Funct cmpule = 'h3d;
Funct cmpult = 'h1d;
Funct mull = 'h00;
Funct mulq = 'h20;
Funct umulh = 'h30;
Funct subl = 'h09;
Funct s4subl = 'h0B;
Funct s8subl = 'h1B;
Funct subq = 'h29;
Funct s4subq = 'h2B;
Funct s8subq = 'h3B;

Funct andOp = 'h00;
Funct bicOp = 'h08;
Funct bisOp = 'h20;
Funct eqvOp = 'h48;
Funct orNotOp = 'h28;
Funct xorOp = 'h40;
Funct cmoveq = 'h24;
Funct cmovge = 'h46;
Funct cmovgt = 'h66;
Funct cmovlbc = 'h16;
Funct cmovlbs = 'h14;
Funct cmovle = 'h64;
Funct cmovlt = 'h44;
Funct cmovne = 'h26;
Funct sll = 'h39;
Funct srl = 'h34;
Funct sra = 'h3c;

Funct zapnot = 'h31;

MemImm exit = 'h21;

function Maybe#(Bit#(rname_SZ)) isaGetSrc(ISA_INSTRUCTION i, Integer n) provisos(Bits#(ISA_REG_INDEX, rname_SZ));

    let    opcode = i[31:26];
    Bool   useLit = unpack(i[12]);
    let     funct = i[11:5];
    let    memImm = i[15:0];
    let        ra = i[25:21];
    let        rb = i[20:16];
    let        rc = i[4:0];

    Maybe#(Bit#(rname_SZ)) ret = tagged Invalid;

    case (opcode)
        opcode01:
        begin
            case (memImm)
                exit:
                begin
                    if(n == 1)
                        ret = tagged Valid pack(tagged ArchReg ra);
                end
            endcase
        end

        jmp, lda, ldl, ldq, ldwu, ldbu, ldq_u:
        begin
            if(n == 1)
                ret = tagged Valid pack(tagged ArchReg rb);
        end

        arith, logical, byteManipulation:
        begin
            if(n == 1)
                ret = tagged Valid pack(tagged ArchReg ra);
            else if(n == 2 && useLit)
                ret = tagged Valid pack(tagged ArchReg rb);
            else if(n == 3)
            begin
                ret = case (funct)
                          cmoveq, cmovge, cmovgt, cmovlbc, cmovlbs, cmovle, cmovlt, cmovne: return tagged Valid pack(tagged ArchReg rc);
                      endcase;
            end
        end
    endcase

    return ret;
 
endfunction


// isaGetDst

// Given an instruction, return the nth destination register.
// Or return Invalid if there is no such destination for this instruction.

function Maybe#(Bit#(rname_SZ)) isaGetDst(ISA_INSTRUCTION i, Integer n) provisos(Bits#(ISA_REG_INDEX, rname_SZ));

    let    opcode = i[31:26];
    Bool   useLit = unpack(i[12]);
    let     funct = i[11:5];
    let    memImm = i[15:0];
    let        ra = i[25:21];
    let        rb = i[20:16];
    let        rc = i[4:0];

    Maybe#(Bit#(rname_SZ)) ret = tagged Invalid;

    case (opcode)
        br, bsr, jmp, lda, ldl, ldq, ldwu, ldbu, ldq_u:
        begin
            if(n == 1)
                ret = tagged Valid pack(tagged ArchReg ra);   
        end

        arith, logical, byteManipulation:
        begin
            if(n == 1)
                ret = tagged Valid pack(tagged ArchReg rc);
        end
    endcase

    return ret;
 
endfunction

// isaGetNumDsts

// Given an instruction, return how many destinations it has.

function Integer isaGetNumDsts(ISA_INSTRUCTION i);

    let    opcode = i[31:26];
    let     funct = i[11:5];
    let    memImm = i[15:0];

    return case (opcode)
               opcode01, beq, bge, bgt, blbc, blbs, ble, blt, bne: return 0;
               br, bsr, lda, ldl, ldq, ldwu, ldbu, ldq_u, arith, logical, byteManipulation: return 1;
           endcase;
 
endfunction


// isaIsLoad

// Returns true if the given instruction is a load.

function Bool isaIsLoad(ISA_INSTRUCTION i);

    let opcode = i[31:26];
    let   funct = i[11:5];
    let memImm = i[15:0];

    return case (opcode)
               lda, ldl, ldq, ldwu, ldbu, ldq_u: return True;
               default: return False; 
           endcase;

endfunction


// isaIsStore

// Returns true if the given instruction is a store.

function Bool isaIsStore(ISA_INSTRUCTION i);

    return False; // You should write this.

endfunction

// isaLoadType

// Returns the ISA_LOAD_TYPE (which you defined in isa_datatypes.bsv) of a given instruction.
// This will only be called on instructions where isaIsLoad() returns True.

function ISA_MEMOP_TYPE isaLoadType(ISA_INSTRUCTION i);

    Opcode opcode = i[31:26];

    return case (opcode)
               ldl: return MEM_SIGN_32;
               ldq: return MEM_64;
               ldwu: return MEM_ZERO_16;
               ldbu: return MEM_ZERO_8;
           endcase;

endfunction


// isaStoreType

// Returns the ISA_MEMOP_TYPE (which you defined in isa_datatypes.bsv) of a given instruction.
// This will only be called on instructions where isaIsStore() returns True.

function ISA_MEMOP_TYPE isaStoreType(ISA_INSTRUCTION i);

    return MEM_64; // You should write this.

endfunction


// isaIsBranch

// Returns true if the given instruction is a branch.

function Bool isaIsBranch(ISA_INSTRUCTION i);

    Opcode opcode = i[31:26];

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

    return False; // You should write this.

endfunction


// isaDrainAfter

// Returns true if the timing model should drain the pipeline after executing this
// instruction.

// Note that both isaDrainBefore() and isaDrainAfter() may be true for a given instruction.

function Bool isaDrainAfter(ISA_INSTRUCTION i);

    return False; // You should write this.

endfunction


// isaEmulateInstruction

// Returns true if the given instruction should be emulated in software.

function Bool isaEmulateInstruction(ISA_INSTRUCTION i);

    Opcode opcode = i[31:26];

    return case (opcode)
               opcode01, br, bsr, jmp, beq, bge, bgt, blbc, blbs, ble, blt, bne, lda, ldl, ldq, ldwu, ldbu, ldq_u, arith, logical, byteManipulation: return False;
               default: return True;
           endcase;

endfunction

