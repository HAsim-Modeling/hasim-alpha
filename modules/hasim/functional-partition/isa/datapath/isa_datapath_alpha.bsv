
// isa_datapath_template

// This file contains a template ISA datapath.

// You should fill this in with with an ISA-specific ALU, 
// which may be pipelined if you so choose.


// ***** Imports *****

import Vector::*;

import hasim_common::*;
import soft_connections::*;

import hasim_isa::*;

`define CPU_FEATURE_MASK 0
`define IMPL_VER 0

// ***** Modules *****

// mkISA_Datapath

// The datapath module itself.

module [HASim_Module] mkISA_Datapath 
  //interface:
              ();

    // ***** Soft Connections *****

    // Connection to the functional partition.
    
    Connection_Server#(Tuple3#(ISA_INSTRUCTION, ISA_ADDRESS, ISA_SOURCE_VALUES), 
                       Tuple3#(ISA_EXECUTION_RESULT, ISA_ADDRESS, ISA_RESULT_VALUES)) link_fp <- mkConnection_Server("isa_datapath");

    // ***** Debugging Log *****
    
    // This logfile is available for debugging during Bluesim simulation.
    // It has no affect on the FPGA.
    
    let debug_log <- mkReg(InvalidFile);


    // ***** Rules ******

    
    // openLog
    
    // Opens the logfile for writing. The filename is an AWB parameter.

    rule open_log (debug_log == InvalidFile);

        let fd <- $fopen(`HASIM_ISA_DP_LOGFILE, "w");

        if (fd == InvalidFile)
        begin
          $display("ERROR: ISA: Datapath: Could not create logfile %s", `HASIM_ISA_DP_LOGFILE);
          $finish(1);
        end

        debug_log <= fd;

    endrule


    // datapathExec

    // You should write this rule to execute your ISA-specific instructions.
    // Parameters: 
    //    * Instruction
    //    * Address (program counter)
    //    * List of source values from registers
    // Returns: 
    //    * Instruction result (given to timing partition)
    //    * Effective address (unused if not a load/store)
    //    * List of values to writeback

    // Note that you may choose to break this rule up into a pipeline for efficiency.

    rule datapathExec (True);

        // Get the request from the functional partition.
        match {.inst, .addr, .srcs} = link_fp.getReq();

        link_fp.deq();

        // Some convenient variables to return.

        // The result for the timing partition.
        ISA_EXECUTION_RESULT timep_result = tagged RNop;
        
        // The effective address for Loads/Stores
        ISA_ADDRESS effective_addr = 0;
        
        // The writebacks which are sent to the register file.
        ISA_RESULT_VALUES writebacks_wrong = Vector::replicate(Invalid);
        Vector#(ISA_MAX_DSTS, Maybe#(Bit#(64))) writebacks = replicate(Invalid);


        OPCODE    opcode = inst[31:26];
        FUNCT      funct = inst[11:5];
        MEM_FUNC memDisp = inst[15:0];
        let          lit = inst[20:13];
        Bool      useLit = unpack(inst[12]);
        let    branchImm = inst[20:0];

        Bit#(64) src0 = srcs[0];
        Bit#(64) src1 = useLit? signExtend(lit): srcs[1];
        Bit#(64) src2 = srcs[2];

        function Bit#(64) byteZap(Bit#(64) srcBits, Bit#(8) mask);
            Vector#(8, Bit#(8)) res = newVector();
            Vector#(8, Bit#(8)) src = unpack(srcBits);
            for(Integer i = 0; i < 8; i = i + 1)
                res[i] = (mask[i] == 1)? src[i]: 0;
            return pack(res);
        endfunction

        function Bool unsignedSumOverflow(Bit#(1) a, Bit#(1) b, Bit#(1) res);
            return ((a != b)? res == 0: a == 1);
        endfunction

        function Bool signedSumOverflow(Bit#(1) a, Bit#(1) b, Bit#(1) res);
            return (a == b && res != a);
        endfunction

        function Bool signedMulOverflow(Bit#(sz) res);
            Bit#(TDiv#(sz,2)) check = res[(valueOf(sz)-1):(valueOf(sz)/2)];
            return res[(valueOf(sz)/2) - 1] == 0? check != 0: check != maxBound;
        endfunction

        function Bit#(64) setOverflowBit(Bit#(64) controlReg, Bool overflow);
            return overflow? 1: 0;
        endfunction

        case (opcode)
            opc01:
            begin
                case (memDisp)
                    exit: timep_result = tagged RTerminate unpack(truncate(src0));
                endcase
            end

            lda, ldah:
            begin
                Bit#(64) disp = case(opcode)
                                    lda: signExtend(memDisp);
                                    ldah: signExtend(memDisp<<16);
                                endcase;
                writebacks[0] = tagged Valid (src0 + disp);
            end

            ldbu, ldl, ldq, ldwu: effective_addr = src0 + signExtend(memDisp);
            ldq_u: effective_addr = ((src0 + signExtend(memDisp)) & ~7);
            ldl_l, ldq_l:
            begin
                effective_addr = src0 + signExtend(memDisp);
                writebacks[1] = tagged Valid 1;
                writebacks[2] = tagged Valid effective_addr;
            end

            stl_c, stq_c:
            begin
                effective_addr = src0 + signExtend(memDisp);
                writebacks[0] = tagged Valid src2;
                writebacks[1] = tagged Valid 0;
            end
            stb, stl, stq, stw: effective_addr = src0 + signExtend(memDisp);
            stq_u: effective_addr = ((src0 + signExtend(memDisp)) & ~7);

            beq, bge, bgt, blbc, blbs, ble, blt, bne:
            begin
                let newAddr = addr + 4 + (signExtend(branchImm) << 2);
                Bool taken = case (opcode)
                                 beq : return src0 == 0;
                                 bge : return src0 >= 0;
                                 bgt : return src0 > 0;
                                 blbc: return truncate(src0) == 1'b0;
                                 blbs: return truncate(src0) == 1'b1;
                                 ble : return src0 <= 0;
                                 blt : return src0 < 0;
                                 bne : return src0 != 0;
                             endcase;
                debug(2, $fdisplay(debug_log, "[0x%x] Bxx to 0x%x, %staken", addr, newAddr, taken? "": "not "));
                timep_result = taken? tagged RBranchTaken truncate(newAddr): tagged RBranchNotTaken truncate(addr + 4);
            end

            br, bsr:
            begin
                writebacks[0] = tagged Valid (addr + 4);
                let newAddr = addr + 4 + (signExtend(branchImm) << 2);
                debug(2, $fdisplay(debug_log, "[0x%x] BxR to 0x%x", addr, newAddr));
                timep_result = tagged RBranchTaken truncate(newAddr);
            end

            jmp:
            begin
                let newAddr = src0 & ~3;
                writebacks[0] = tagged Valid (addr + 4);
                debug(2, $fdisplay(debug_log, "[0x%x] JMP to 0x%x", addr, newAddr));
                timep_result = tagged RBranchTaken truncate(newAddr);
            end

            opc10:
            begin
                case (funct)
                    addl, addlv:
                    begin
                        let res = (src0 + src1)[31:0];
                        writebacks[0] = tagged Valid signExtend(res);
                        if(funct == addlv)
                            writebacks[1] = tagged Valid setOverflowBit(src2, signedSumOverflow(src0[31], src1[31], res[31]));
                    end

                    s4addl: writebacks[0] = tagged Valid signExtend(((src0 << 2) + src1)[31:0]);

                    subl, sublv:
                    begin
                        let subSrc1 = 0 - src1;
                        let res = (src0 + subSrc1)[31:0];
                        writebacks[0] = tagged Valid signExtend(res);
                        if(funct == sublv)
                            writebacks[1] = tagged Valid setOverflowBit(src2, signedSumOverflow(src0[31], subSrc1[31], res[31]));
                    end

                    s4subl: writebacks[0] = tagged Valid signExtend(((src0 << 2) - src1)[31:0]);

                    cmpbge:
                    begin
                        Vector#(8, Bit#(8)) vec0 = unpack(src0);
                        Vector#(8, Bit#(8)) vec1 = unpack(src1);
                        Vector#(8, Bit#(1)) vecRes = newVector();
                        for(Integer i = 0; i < 8; i = i + 1)
                            vecRes[i] = pack(vec0[i] >= vec1[i]);
                        writebacks[0] = tagged Valid zeroExtend(pack(vecRes));
                    end

                    s8addl: writebacks[0] = tagged Valid signExtend(((src0 << 3) + src1)[31:0]);
                    s8subl: writebacks[0] = tagged Valid signExtend(((src0 << 3) - src1)[31:0]);

                    cmpult: writebacks[0] = tagged Valid zeroExtend(pack(src0 < src1));

                    addq, addqv:
                    begin
                        let res = (src0 + src1);
                        writebacks[0] = tagged Valid signExtend(res);
                        if(funct == addlv)
                            writebacks[1] = tagged Valid setOverflowBit(src2, signedSumOverflow(src0[63], src1[63], res[63]));
                    end

                    s4addq: writebacks[0] = tagged Valid ((src0 << 2) + src1);

                    subq, subqv:
                    begin
                        let subSrc1 = 0 - src1;
                        let res = (src0 + subSrc1);
                        writebacks[0] = tagged Valid signExtend(res);
                        if(funct == sublv)
                            writebacks[1] = tagged Valid setOverflowBit(src2, signedSumOverflow(src0[63], subSrc1[63], res[63]));
                    end

                    s4subq: writebacks[0] = tagged Valid ((src0 << 2) - src1);

                    cmpeq: writebacks[0] = tagged Valid zeroExtend(pack(src0 == src1));

                    s8addq: writebacks[0] = tagged Valid ((src0 << 3) + src1);
                    s8subq: writebacks[0] = tagged Valid ((src0 << 3) - src1);

                    cmpule: writebacks[0] = tagged Valid zeroExtend(pack(src0 <= src1));
                    cmplt: writebacks[0] = tagged Valid zeroExtend(pack(signedLT(src0, src1)));
                    cmple: writebacks[0] = tagged Valid zeroExtend(pack(signedLE(src0, src1)));
                endcase
            end

            opc11:
            begin
                case (funct)
                    andOp: writebacks[0] = tagged Valid (src0 & src1);
                    bic: writebacks[0] = tagged Valid (src0 & ~src1);
                    cmovlbs: writebacks[0] = tagged Valid ((truncate(src0) == 1'b1)? src1: src2);
                    cmovlbc: writebacks[0] = tagged Valid ((truncate(src0) == 1'b0)? src1: src2);
                    bis: writebacks[0] = tagged Valid (src0 | src1);
                    cmoveq: writebacks[0] = tagged Valid ((src0 == 0)? src1: src2);
                    cmovne: writebacks[0] = tagged Valid ((src0 != 0)? src1: src2);
                    ornot: writebacks[0] = tagged Valid (src0 | ~src1);
                    xorOp: writebacks[0] = tagged Valid (src0 ^ src1);
                    cmovlt: writebacks[0] = tagged Valid ((src0 < 0)? src1: src2);
                    cmovge: writebacks[0] = tagged Valid ((src0 >= 0)? src1: src2);
                    eqv: writebacks[0] = tagged Valid (src0 ^ ~src1);
                    amask: writebacks[0] = tagged Valid (src0 & ~(`CPU_FEATURE_MASK)); // Implemetation specific. Looks like all alphas simply move the source to dest
                    cmovle: writebacks[0] = tagged Valid ((src0 <= 0)? src1: src2);
                    cmovgt: writebacks[0] = tagged Valid ((src0 > 0)? src1: src2);
                    implver: writebacks[0] = tagged Valid `IMPL_VER; // Implementation version (21064 -> 0, 21164 -> 1, 21264 -> 2)
                endcase
            end

            opc12:
            begin
                case (funct)
                    mskbl, mskwl, mskll, mskql, mskwh, msklh, mskqh:
                    begin
                        Bit#(16) byteMask = case (funct)
                                                mskbl: 'b1;
                                                mskwl, mskwh: 'b11;
                                                mskll, msklh: 'b1111;
                                                mskql, mskqh: 'b11111111;
                                            endcase;
                        Bit#(16) shiftedMask = byteMask << src1[2:0];
                        case (funct)
                            mskbl, mskwl, mskll, mskql: writebacks[0] = tagged Valid byteZap(src0, byteMask[7:0]);
                            mskwh, msklh, mskqh: writebacks[0] = tagged Valid byteZap(src0, byteMask[15:8]);
                        endcase
                    end

                    extbl, extwl, extll, extql, extwh, extlh, extqh:
                    begin
                        Bit#(8) byteMask = case (funct)
                                               extbl: 'b1;
                                               extwl, extwh: 'b11;
                                               extll, extlh: 'b1111;
                                               extql, extqh: 'b11111111;
                                           endcase;
                        case (funct)
                            extbl, extwl, extll, extql:
                            begin
                                let temp = src0 >> ({src1[2:0], 3'b0})[5:0];
                                writebacks[0] = tagged Valid byteZap(temp, ~byteMask);
                            end
                            extwh, extlh, extqh:
                            begin
                                Bit#(7) shift = 64 - signExtend({src1[2:0], 3'b0});
                                let temp = src0 << shift[5:0];
                                writebacks[0] = tagged Valid byteZap(temp, ~byteMask);
                            end
                        endcase
                    end

                    insbl, inswl, insll, insql, inswh, inslh, insqh:
                    begin
                        Bit#(16) byteMask = case (funct)
                                                insbl: 'b1;
                                                inswl, inswh: 'b11;
                                                insll, inslh: 'b1111;
                                                insql, insqh: 'b11111111;
                                            endcase;
                        Bit#(16) shiftedMask = byteMask << src1[2:0];
                        case (funct)
                            insbl, inswl, insll, insql:
                            begin
                                let temp = src0 >> ({src1[2:0], 3'b0})[5:0];
                                writebacks[0] = tagged Valid byteZap(temp, ~byteMask[7:0]);
                            end
                            inswh, inslh, insqh:
                            begin
                                Bit#(7) shift = 64 - signExtend({src1[2:0], 3'b0});
                                let temp = src0 << shift[5:0];
                                writebacks[0] = tagged Valid byteZap(temp, ~byteMask[15:8]);
                            end
                        endcase
                    end

                    zap: writebacks[0] = tagged Valid byteZap(src0, src1[7:0]);
                    zapnot: writebacks[0] = tagged Valid byteZap(src0, ~src1[7:0]);

                    srl: writebacks[0] = tagged Valid (src0 >> src1[5:0]);
                    sll: writebacks[0] = tagged Valid (src0 << src1[5:0]);
                    sra: writebacks[0] = tagged Valid signedShiftRight(src0, src1[5:0]);
                endcase
            end

            opc13:
            begin
                case (funct)
                    mull, mullv:
                    begin
                        Bit#(64) mulRes = signExtend(src0[31:0]) * signExtend(src1[31:0]);
                        Bool overflow = signedMulOverflow(mulRes);
                        writebacks[0] = tagged Valid signExtend(mulRes[31:0]);
                        if(funct == mullv)
                            writebacks[1] = tagged Valid setOverflowBit(src2, overflow);
                    end

                    mulq, mulqv:
                    begin
                        Bit#(128) mulRes = signExtend(src0) * signExtend(src1);
                        Bool overflow = signedMulOverflow(mulRes);
                        writebacks[0] = tagged Valid mulRes[63:0];
                        if(funct == mulqv)
                            writebacks[1] = tagged Valid setOverflowBit(src2, overflow);
                    end

                    umulh:
                    begin
                        Bit#(128) mulRes = zeroExtend(src0) * zeroExtend(src1);
                        writebacks[0] = tagged Valid mulRes[127:64];
                    end
                endcase
            end

            opc1c:
            begin
                case (funct)
                    sextb: writebacks[0] = tagged Valid signExtend(src0[7:0]);
                    sextw: writebacks[0] = tagged Valid signExtend(src0[15:0]);

                    ctpop:
                    begin
                        Bit#(64) temp = 0;
                        for(Integer i = 0; i < 64; i = i + 1)
                        begin
                            if(src0[i] == 1)
                                temp = temp + 1;
                        end
                        writebacks[0] = tagged Valid temp;
                    end

                    perr:
                    begin
                        Bit#(64) temp = 0;
                        Vector#(8, Bit#(8)) vec0 = unpack(src0);
                        Vector#(8, Bit#(8)) vec1 = unpack(src1);
                        for(Integer i = 0; i < 8; i = i + 1)
                        begin
                            if(vec0[i] >= vec1[i])
                                temp = temp + zeroExtend(vec0[i] - vec1[i]);
                            else
                                temp = temp + zeroExtend(vec1[i] - vec0[i]);
                        end
                        writebacks[0] = tagged Valid temp;
                    end

                    ctlz:
                    begin
                        Bit#(64) temp = 0;
                        Bool done = False;
                        for(Integer i = 0; i < 64; i = i + 1)
                        begin
                            if(!done)
                            begin
                                if(src0[i] != 1)
                                    temp = temp + 1;
                                else
                                    done = False;
                            end
                        end
                        writebacks[0] = tagged Valid temp;
                    end

                    ctlz:
                    begin
                        Bit#(64) temp = 0;
                        Bool done = False;
                        for(Integer i = 63; i >= 0; i = i - 1)
                        begin
                            if(!done)
                            begin
                                if(src0[i] != 1)
                                    temp = temp + 1;
                                else
                                    done = False;
                            end
                        end
                        writebacks[0] = tagged Valid temp;
                    end

                    unpkbw:
                    begin
                        Bit#(64) temp = 0;
                        temp[7:0] = src0[7:0];
                        temp[23:16] = src0[15:8];
                        temp[39:32] = src0[23:16];
                        temp[55:48] = src0[31:24];
                        writebacks[0] = tagged Valid temp;
                    end

                    unpkbl:
                    begin
                        Bit#(64) temp = 0;
                        temp[7:0] = src0[7:0];
                        temp[39:32] = src0[15:8];
                        writebacks[0] = tagged Valid temp;
                    end

                    pkwb:
                    begin
                        Bit#(64) temp;
                        temp[7:0] = src0[7:0];
                        temp [15:8] = src0[23:16];
                        temp[23:16] = src0[39:32];
                        temp[31:24] = src0[55:48];
                        writebacks[0] = tagged Valid temp;
                    end

                    pklb:
                    begin
                        Bit#(64) temp = 0;
                        temp[7:0] = src0[7:0];
                        temp[15:8] = src0[39:32];
                        writebacks[0] = tagged Valid temp;
                    end
                endcase
            end
        endcase

        // Return the result to the functional partition.
        link_fp.makeResp(tuple3(timep_result, effective_addr, writebacks));

    endrule

endmodule
