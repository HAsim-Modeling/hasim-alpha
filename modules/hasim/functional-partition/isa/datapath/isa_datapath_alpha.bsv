//
// Copyright (C) 2008 Intel Corporation
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation; either version 2
// of the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
//

// isa_datapath_template

// This file contains a template ISA datapath.

// You should fill this in with with an ISA-specific ALU, 
// which may be pipelined if you so choose.


// ***** Imports *****

import FIFO::*;
import FIFOF::*;
import Vector::*;

// Project foundation includes.

`include "asim/provides/hasim_common.bsh"
`include "asim/provides/fpga_components.bsh"
`include "asim/provides/soft_connections.bsh"
`include "asim/provides/common_services.bsh"

`include "asim/provides/hasim_isa.bsh"
`include "asim/provides/funcp_interface.bsh"
`include "asim/provides/isa_emulator.bsh"

`include "asim/rrr/remote_client_stub_ISA_REGOP_EMULATOR.bsh"
`include "asim/dict/STATS_ISA_DATAPATH_ALPHA.bsh"
`include "asim/dict/ASSERTIONS_ISA_DATAPATH_ALPHA.bsh"

`define CPU_FEATURE_MASK 0
`define IMPL_VER 0


//
// Primary pipelines
//
typedef enum
{
    ISA_DP_PIPE_ADD,
    ISA_DP_PIPE_BITOPS,
    ISA_DP_PIPE_BITOPS_SLOW,
    ISA_DP_PIPE_BRANCH,
    ISA_DP_PIPE_CMOV,
    ISA_DP_PIPE_CMP,
    ISA_DP_PIPE_CONTROL,
    ISA_DP_PIPE_FP_CMOV,
    ISA_DP_PIPE_FP_EMUL,
    ISA_DP_PIPE_FP_INT,
    ISA_DP_PIPE_ILLEGAL,
    ISA_DP_PIPE_LOGICAL,
    ISA_DP_PIPE_MEMADDR,
    ISA_DP_PIPE_MUL,
    ISA_DP_PIPE_NOP,
    ISA_DP_PIPE_SHIFT,
    ISA_DP_PIPE_XFER_FROM_FP,
    ISA_DP_PIPE_XFER_TO_FP
}
ISA_DP_PIPE
    deriving (Eq, Bits);


//
// Primiary pipeline FIFO structure
//
typedef struct
{
    FUNCP_ISA_DATAPATH_REQ req;
    ISA_DP_PIPE pipe;
}
ISA_DP_QUEUE
    deriving (Eq, Bits);


//
// Secondary bit operations pipelines
//
typedef enum
{
    ISA_DP_BITOPS_CTPOP,
    ISA_DP_BITOPS_PERR,
    ISA_DP_BITOPS_CTLZ,
    ISA_DP_BITOPS_CTTZ
}
ISA_DP_BITOPS_SLOW_PIPE
    deriving (Eq, Bits);

//
// Secondary bit operations FIFO structure
//
typedef struct
{
    ISA_DP_BITOPS_SLOW_PIPE pipe;
    FUNCP_ISA_DATAPATH_REQ req;
    ISA_ADDRESS addr;
    FUNCT funct;
    Bit#(64) src0;
    Bit#(64) src1;
}
ISA_DP_BITOPS_SLOW_QUEUE
    deriving (Eq, Bits);


//
// Multiply operations queue
//

typedef enum
{
    ISA_DP_MUL_MULL,
    ISA_DP_MUL_MULQ,
    ISA_DP_MUL_UMULH
}
ISA_DP_MUL_PIPE
    deriving (Eq, Bits);

typedef struct
{
    ISA_DP_MUL_PIPE pipe;
    FUNCP_ISA_DATAPATH_REQ req;
    Bool testOverflow;
    Bool isNegative;
    ISA_ADDRESS addr;
    FUNCT funct;
    Bit#(64) src0;
    Bit#(64) src1;
}
ISA_DP_MUL_QUEUE
    deriving (Eq, Bits);


// ***** Modules *****

// mkISA_Datapath

// The datapath module itself.

module [HASIM_MODULE] mkISA_Datapath 
  //interface:
              ();

    // ***** Soft Connections *****

    // Connection to the functional partition.
    
    Connection_Server#(FUNCP_ISA_DATAPATH_REQ, FUNCP_ISA_DATAPATH_RSP) link_fp <- mkConnection_Server("isa_datapath");
    Connection_Receive#(FUNCP_ISA_DATAPATH_SRCVALS) link_fp_srcvals <- mkConnection_Receive("isa_datapath_srcvals");
    Connection_Send#(FUNCP_ISA_WRITEBACK) link_fp_writeback <- mkConnection_Send("isa_datapath_writeback");
    

    // ***** Debugging Log *****
    
    // This logfile is available for debugging during Bluesim simulation.
    // It has no affect on the FPGA.
    
    DEBUG_FILE debugLog <- mkDebugFile(`HASIM_ISA_DP_LOGFILE);


    // ***** Statistics *****

    STAT statISAEmul <- mkStatCounter(`STATS_ISA_DATAPATH_ALPHA_REGOP_EMULATED_INSTRS);


    // ***** Local state *****

    ASSERTION_NODE assertNode <- mkAssertionNode(`ASSERTIONS_ISA_DATAPATH_ALPHA__BASE);
    ASSERTION assertUnexpectedOpcode <- mkAssertionChecker(`ASSERTIONS_ISA_DATAPATH_ALPHA_UNEXPECTED_OPCODE, ASSERT_ERROR, assertNode);

    // Floating point state
    Wire#(Bool) src0IsFPZero <- mkWire();

    // Queue coming out of initial decode step
    FIFO#(ISA_DP_QUEUE) dpQ <- mkFIFO();
    
    // Response queue to guarantee ordered resonses among multpile queues
    FIFO#(TOKEN_INDEX) dpResponseQ <- mkFIFO();

    function ActionValue#(FUNCP_ISA_DATAPATH_SRCVALS) getRegSources();
    actionvalue
        let req_srcs = link_fp_srcvals.receive();
        link_fp_srcvals.deq();
        return req_srcs;
    endactionvalue
    endfunction


    function Bit#(64) byteZap(Bit#(64) srcBits, Bit#(8) mask);
        Vector#(8, Bit#(8)) res = newVector();
        Vector#(8, Bit#(8)) src = unpack(srcBits);
        for(Integer i = 0; i < 8; i = i + 1)
            res[i] = (mask[i] == 0)? src[i]: 0;
        return pack(res);
    endfunction

    function Bool signedMulOverflow(Bit#(sz) res);
        Bit#(TDiv#(sz,2)) check = res[(valueOf(sz)-1):(valueOf(sz)/2)];
        return res[(valueOf(sz)/2) - 1] == 0? check != 0: check != maxBound;
    endfunction


    function Action debug_ALU (ISA_ADDRESS addr_d, OPCODE opcode_d, FUNCT funct_d, Maybe#(Bit#(64)) val_d, Bit#(64) src0_d, Bit#(64) src1_d);
    action
        Bit#(6) op = pack(opcode_d);
        Bit#(7) fu = pack(funct_d);
        if (val_d matches tagged Valid .alu_d)
        begin
            debugLog.record($format("[0x%x] ALU (op 0x%x / func 0x%x) 0x%x <- src0 0x%x, src1 0x%x", addr_d, op, fu, alu_d, src0_d, src1_d));
        end
        else
        begin
            debugLog.record($format("[0x%x] ALU (op 0x%x / func 0x%x) Invalid", addr_d, op, fu));
        end
    endaction
    endfunction

    function Action debug_ALU3 (ISA_ADDRESS addr_d, OPCODE opcode_d, FUNCT funct_d, Maybe#(Bit#(64)) val_d, Bit#(64) src0_d, Bit#(64) src1_d, Bit#(64) src2_d);
    action
        Bit#(6) op = pack(opcode_d);
        Bit#(7) fu = pack(funct_d);
        if (val_d matches tagged Valid .alu_d)
        begin
            debugLog.record($format("[0x%x] ALU (op 0x%x / func 0x%x) 0x%x <- src0 0x%x, src1 0x%x, src2 0x%x", addr_d, op, fu, alu_d, src0_d, src1_d, src2_d));
        end
        else
        begin
            debugLog.record($format("[0x%x] ALU (op 0x%x / func 0x%x) Invalid", addr_d, op, fu));
        end
    endaction
    endfunction

    function Action debug_FP (ISA_ADDRESS addr_d, OPCODE opcode_d, FP_FUNC funct_d, Maybe#(Bit#(64)) val_d, Bit#(64) src0_d, Bit#(64) src1_d);
    action
        Bit#(6) op = pack(opcode_d);
        Bit#(11) fu = pack(funct_d);
        if (val_d matches tagged Valid .alu_d)
        begin
            debugLog.record($format("[0x%x] FP (op 0x%x / func 0x%x) 0x%x <- src0 0x%x, src1 0x%x", addr_d, op, fu, alu_d, src0_d, src1_d));
        end
        else
        begin
            debugLog.record($format("[0x%x] FP (op 0x%x / func 0x%x) Invalid", addr_d, op, fu));
        end
    endaction
    endfunction

    function Action debug_FP3 (ISA_ADDRESS addr_d, OPCODE opcode_d, FP_FUNC funct_d, Maybe#(Bit#(64)) val_d, Bit#(64) src0_d, Bit#(64) src1_d, Bit#(64) src2_d);
    action
        Bit#(6) op = pack(opcode_d);
        Bit#(11) fu = pack(funct_d);
        if (val_d matches tagged Valid .alu_d)
        begin
            debugLog.record($format("[0x%x] FP (op 0x%x / func 0x%x) 0x%x <- src0 0x%x, src1 0x%x, src2 0x%x", addr_d, op, fu, alu_d, src0_d, src1_d, src2_d));
        end
        else
        begin
            debugLog.record($format("[0x%x] FP (op 0x%x / func 0x%x) Invalid", addr_d, op, fu));
        end
    endaction
    endfunction

    function Bool readyToRespondTok(TOKEN_INDEX tokIdx);
        return tokIdx == dpResponseQ.first();
    endfunction

    function Bool readyToRespondStd();
        return readyToRespondTok(dpQ.first().req.token.index);
    endfunction


    // ====================================================================
    //
    // Writeback helper data and functions.
    //
    // Writebacks are sent to the register state manager on a soft
    // connection that is separate from the ISA datapath server.  This
    // permits the ISA datapath to return a control response early and
    // forward the data later.  The register state scoreboard will ensure
    // that instructions reading the data block until writeback is
    // complete.
    //
    // ====================================================================

    //
    // One register may be written per cycle.  This merge FIFO collapses
    // the max. number of registers written by an instruction into a FIFO.
    //
    MERGE_FIFOF#(ISA_MAX_DSTS,
                 Tuple3#(TOKEN, Maybe#(FUNCP_PHYSICAL_REG_INDEX), ISA_VALUE)) writebackQ <- mkMergeFIFOF();

    //
    // forwardWritebacks --
    //     Helper function for passing all writebacks from an operation to
    //     the writebackQ.
    //
    function Action forwardWritebacks(FUNCP_ISA_DATAPATH_REQ req,
                                      ISA_RESULT_VALUES writebacks);
    action
        Bool did_write = False;

        for (Integer d = 0; d < valueOf(ISA_MAX_DSTS); d = d + 1)
        begin
            // Valid destination physical register and value?
            if (req.instDstPhysRegs[d] matches tagged Valid .pr &&&
                writebacks[d] matches tagged Valid .val)
            begin
                writebackQ.ports[d].enq(tuple3(req.token, tagged Valid pr, val));
                did_write = True;
            end
        end
        
        if (! did_write)
        begin
            // Token has no written registers.  Must send a "done" message
            // anyway.
            writebackQ.ports[0].enq(tuple3(req.token, tagged Invalid, ?));
        end
    endaction
    endfunction


    // ====================================================================
    //
    // Stage 1:  Decode.
    //
    //     The request arrives from the execute one cycle before the
    //     register values arrive, so there is time to do decode here.
    //     The decoder routes the request to the right pipeline.
    //
    // ====================================================================

    rule dpDecode (True);
        // Get the request from the functional partition.
        let req  = link_fp.getReq();
        link_fp.deq();
        
        ISA_DP_PIPE pipeline = ISA_DP_PIPE_ILLEGAL;

        case (isaGetOpcode(req.instruction))
            opc01:
            begin
                pipeline = ISA_DP_PIPE_CONTROL;
            end

            ldbu, ldl, ldq, ldwu,
            ldq_u,
            ldl_l, ldq_l,
            ldt, lds:
            begin
                pipeline = ISA_DP_PIPE_MEMADDR;
            end

            stl_c, stq_c,
            stb, stl, stq, stw,
            stq_u,
            stt, sts:
            begin
                pipeline = ISA_DP_PIPE_MEMADDR;
            end

            beq, bge, bgt, blbc, blbs, ble, blt, bne,
            fbeq, fblt, fble, fbne, fbge, fbgt,
            br, bsr,
            jmp:
            begin
                pipeline = ISA_DP_PIPE_BRANCH;
            end

            lda, ldah:
            begin
                pipeline = ISA_DP_PIPE_ADD;
            end

            opc10:
            begin
                //
                // opc10 has 3 groups: add, subtract and compare.  The group
                // is a function of bits 0, 2 and 3 of the function code.
                //
                let funct = isaGetFunct(req.instruction);
                if ((funct[3:2] != 3) || (funct[0] != 1))
                    pipeline = ISA_DP_PIPE_ADD;
                else
                    pipeline = ISA_DP_PIPE_CMP;
            end

            opc11:
            begin
                //
                // opc11 has 2 groups: cmov and logical operations.
                //
                let funct = isaGetFunct(req.instruction);
                if (funct[2] == 0)
                    pipeline = ISA_DP_PIPE_LOGICAL;
                else
                    pipeline = ISA_DP_PIPE_CMOV;
            end

            opc12:
            begin
                pipeline = ISA_DP_PIPE_SHIFT;
            end

            opc13:
            begin
                pipeline = ISA_DP_PIPE_MUL;
            end

            opc14:
            begin
                case (isaGetFPFunc(req.instruction))
                    itofs, itoff, itoft:
                        pipeline = ISA_DP_PIPE_XFER_TO_FP;
                    default:
                        pipeline = ISA_DP_PIPE_FP_EMUL;
                endcase
            end

            opc15, opc16:
            begin
                if (req.instDstPhysRegs[0] matches tagged Valid .dst_pr)
                    pipeline = ISA_DP_PIPE_FP_EMUL;
                else
                    pipeline = ISA_DP_PIPE_NOP;
            end

            opc17:
            begin
                case (isaGetFPFunc(req.instruction))
                    cvtlq, cpys, cpysn, cpyse, cvtql:
                        pipeline = ISA_DP_PIPE_FP_INT;
                    fcmoveq, fcmovne, fcmovlt, fcmovge, fcmovle, fcmovgt:
                        pipeline = ISA_DP_PIPE_FP_CMOV;
                    default:
                        pipeline = ISA_DP_PIPE_FP_EMUL;
                endcase
            end

            opc18:
            begin
                // RPCC is emulated.  Every other opc18 is a NOP in the
                // functional model.  (Cache prefetch, etc. may have timing
                // model side effects.)
                pipeline = ISA_DP_PIPE_NOP;
            end

            opc1c:
            begin
                case (isaGetFunct(req.instruction))
                ctpop, perr, ctlz, cttz:
                    pipeline = ISA_DP_PIPE_BITOPS_SLOW;
                    
                ftoit, ftois:
                    pipeline = ISA_DP_PIPE_XFER_FROM_FP;

                default:
                    pipeline = ISA_DP_PIPE_BITOPS;
                endcase
            end

        endcase

        // Send request to the pipelines
        dpQ.enq(ISA_DP_QUEUE { req: req, pipe: pipeline });

        // Response queue is used to enforce ordered responses back to the
        // functional partition even when the pipelines below have different
        // depths.
        dpResponseQ.enq(req.token.index);
    endrule


    // ====================================================================
    //
    //   ADD pipeline
    //
    //      For opc10 the function codes are:
    //
    //      addl      = 'h00:   0 0 0 0 0 0 0
    //      addlv     = 'h40:   1 0 0 0 0 0 0
    //      s4addl    = 'h02:   0 0 0 0 0 1 0
    //      s8addl    = 'h12:   0 0 1 0 0 1 0
    //      addq      = 'h20:   0 1 0 0 0 0 0
    //      addqv     = 'h60:   1 1 0 0 0 0 0
    //      s4addq    = 'h22:   0 1 0 0 0 1 0
    //      s8addq    = 'h32:   0 1 1 0 0 1 0
    //
    //      subl      = 'h09:   0 0 0 1 0 0 1
    //      sublv     = 'h49:   1 0 0 1 0 0 1
    //      s4subl    = 'h0b:   0 0 0 1 0 1 1
    //      s8subl    = 'h1b:   0 0 1 1 0 1 1
    //      subq      = 'h29:   0 1 0 1 0 0 1
    //      subqv     = 'h69:   1 1 0 1 0 0 1
    //      s4subq    = 'h2b:   0 1 0 1 0 1 1
    //      s8subq    = 'h3b:   0 1 1 1 0 1 1
    //
    // ====================================================================

    rule dpADD ((dpQ.first().pipe == ISA_DP_PIPE_ADD) && readyToRespondStd());
        let dp = dpQ.first();
        dpQ.deq();

        let addr = dp.req.instAddress;

        // Get sources from physical register file
        let reg_srcs <- getRegSources();

        Bit#(64) src0 = reg_srcs.srcValues[0];
        Bit#(64) src1 = reg_srcs.srcValues[1];

        let opcode = isaGetOpcode(dp.req.instruction);
        let funct = isaGetFunct(dp.req.instruction);

        if (isaGetLiteral(dp.req.instruction) matches tagged Valid .lit)
            src1 = lit;

        // The writebacks that are sent to the register file.
        ISA_RESULT_VALUES writebacks = replicate(tagged Invalid);

        if (opcode != opc10)
        begin
            // lda or ldah
            let mem_disp = isaGetMemDisp(dp.req.instruction);
            if (opcode == ldah)
                mem_disp = mem_disp << 16;
            src1 = mem_disp;
        end
        else
        begin
            // Test for shifted inputs for opc10
            if (funct[1] == 1)
            begin
                // Shifted src0
                if (funct[4] == 0)
                    src0 = src0 << 2;
                else
                    src0 = src0 << 3;
            end
        end

        // Do the add or subtract (with overflow)
        Bit#(65) sum;
        if ((opcode == opc10) && funct[0] == 1)
            sum = { 1'b0, src0 } - { 1'b0, src1 };
        else
            sum = { 1'b0, src0 } + { 1'b0, src1 };

        if (opcode != opc10)
        begin
            // lda or ldah
            writebacks[0] = tagged Valid sum[63:0];
            debugLog.record($format("[0x%x] LDAx 0x%x <- 0x%x + 0x%x", addr, sum[63:0], src0, src1));
        end
        else
        begin
            // 32 or 64 bit result?
            if (funct[5] == 0)
            begin
                writebacks[0] = tagged Valid signExtend(sum[31:0]);

                // Overflow check?
                if (funct[6] == 1)
                    writebacks[1] = tagged Valid zeroExtend(sum[32]);
            end
            else
            begin
                writebacks[0] = tagged Valid sum[63:0];

                // Overflow check?
                if (funct[6] == 1)
                    writebacks[1] = tagged Valid zeroExtend(sum[64]);
            end

            debug_ALU(addr, opcode, funct, writebacks[0], reg_srcs.srcValues[0], src1);
        end

        // Return the result to the functional partition.
        dpResponseQ.deq();
        link_fp.makeResp(initISADatapathRspOp(tagged RNop));
        forwardWritebacks(dp.req, writebacks);
    endrule


    // ====================================================================
    //
    //   Compare pipeline
    //
    // ====================================================================

    rule dpCMP ((dpQ.first().pipe == ISA_DP_PIPE_CMP) && readyToRespondStd());
        let dp = dpQ.first();
        dpQ.deq();

        let addr = dp.req.instAddress;

        // Get sources from physical register file
        let reg_srcs <- getRegSources();

        Bit#(64) src0 = reg_srcs.srcValues[0];
        Bit#(64) src1 = reg_srcs.srcValues[1];

        let opcode = isaGetOpcode(dp.req.instruction);
        let funct = isaGetFunct(dp.req.instruction);

        if (isaGetLiteral(dp.req.instruction) matches tagged Valid .lit)
            src1 = lit;

        // The writebacks that are sent to the register file.
        ISA_RESULT_VALUES writebacks = replicate(tagged Invalid);
        FUNCP_ISA_DATAPATH_EXCEPTIONS except = FUNCP_ISA_EXCEPT_NONE;

        case (funct)
            cmpbge:
            begin
                Vector#(8, Bit#(8)) vec0 = unpack(src0);
                Vector#(8, Bit#(8)) vec1 = unpack(src1);
                Vector#(8, Bit#(1)) vecRes = newVector();
                for(Integer i = 0; i < 8; i = i + 1)
                    vecRes[i] = pack(vec0[i] >= vec1[i]);
                writebacks[0] = tagged Valid zeroExtend(pack(vecRes));
            end

            cmpult: writebacks[0] = tagged Valid zeroExtend(pack(src0 < src1));

            cmpeq: writebacks[0] = tagged Valid zeroExtend(pack(src0 == src1));

            cmpule: writebacks[0] = tagged Valid zeroExtend(pack(src0 <= src1));
            cmplt: writebacks[0] = tagged Valid zeroExtend(pack(signedLT(src0, src1)));
            cmple: writebacks[0] = tagged Valid zeroExtend(pack(signedLE(src0, src1)));

            default:
            begin
                except = FUNCP_ISA_EXCEPT_ILLEGAL_INSTR;
                debugLog.record($format("[0x%x]   Marked instr ILLEGAL", addr));
            end
        endcase

        debug_ALU(addr, opcode, funct, writebacks[0], src0, src1);

        // Return the result to the functional partition.
        dpResponseQ.deq();
        link_fp.makeResp(initISADatapathRsp(except, tagged RNop));
        forwardWritebacks(dp.req, writebacks);
    endrule


    // ====================================================================
    //
    //   CMOV pipeline
    //
    // ====================================================================

    rule dpCMOV ((dpQ.first().pipe == ISA_DP_PIPE_CMOV) && readyToRespondStd());
        let dp = dpQ.first();
        dpQ.deq();

        let addr = dp.req.instAddress;

        // Get sources from physical register file
        let reg_srcs <- getRegSources();

        Bit#(64) src0 = reg_srcs.srcValues[0];
        Bit#(64) src1 = reg_srcs.srcValues[1];
        Bit#(64) src2 = reg_srcs.srcValues[2];

        OPCODE opcode = isaGetOpcode(dp.req.instruction);

        // The writebacks that are sent to the register file.
        ISA_RESULT_VALUES writebacks = replicate(tagged Invalid);
        FUNCP_ISA_DATAPATH_EXCEPTIONS except = FUNCP_ISA_EXCEPT_NONE;

        if (isaGetLiteral(dp.req.instruction) matches tagged Valid .lit)
            src1 = lit;

        let funct = isaGetFunct(dp.req.instruction);

        case (funct)
            cmovlbs: writebacks[0] = tagged Valid ((truncate(src0) == 1'b1) ? src1 : src2);
            cmovlbc: writebacks[0] = tagged Valid ((truncate(src0) == 1'b0) ? src1 : src2);
            cmoveq: writebacks[0] = tagged Valid ((src0 == 0) ? src1 : src2);
            cmovne: writebacks[0] = tagged Valid ((src0 != 0) ? src1 : src2);
            cmovlt: writebacks[0] = tagged Valid (signedLT(src0, 0) ? src1 : src2);
            cmovge: writebacks[0] = tagged Valid (signedGE(src0, 0) ? src1 : src2);
            cmovle: writebacks[0] = tagged Valid (signedLE(src0, 0) ? src1 : src2);
            cmovgt: writebacks[0] = tagged Valid (signedGT(src0, 0) ? src1 : src2);
            implver: writebacks[0] = tagged Valid `IMPL_VER; // Implementation version (21064 -> 0, 21164 -> 1, 21264 -> 2)

            default:
            begin
                except = FUNCP_ISA_EXCEPT_ILLEGAL_INSTR;
                debugLog.record($format("[0x%x]   Marked instr ILLEGAL", addr));
            end
        endcase

        debug_ALU3(addr, opcode, funct, writebacks[0], src0, src1, src2);

        // Return the result to the functional partition.
        dpResponseQ.deq();
        link_fp.makeResp(initISADatapathRsp(except, tagged RNop));
        forwardWritebacks(dp.req, writebacks);
    endrule


    // ====================================================================
    //
    //   Logical operations pipeline
    //
    //      andOp     = 'h00:   0 0 0 0 0 0 0
    //      bic       = 'h08:   0 0 0 1 0 0 0
    //      bis       = 'h20:   0 1 0 0 0 0 0
    //      ornot     = 'h28:   0 1 0 1 0 0 0
    //      xorOp     = 'h40:   1 0 0 0 0 0 0
    //      eqv       = 'h48:   1 0 0 1 0 0 0
    //      amask     = 'h61:   1 1 0 0 0 0 1
    //
    // ====================================================================

    rule dpLOGICAL ((dpQ.first().pipe == ISA_DP_PIPE_LOGICAL) && readyToRespondStd());
        let dp = dpQ.first();
        dpQ.deq();

        let addr = dp.req.instAddress;

        // Get sources from physical register file
        let reg_srcs <- getRegSources();

        Bit#(64) src0 = reg_srcs.srcValues[0];
        Bit#(64) src1 = reg_srcs.srcValues[1];

        OPCODE opcode = isaGetOpcode(dp.req.instruction);

        // The writebacks that are sent to the register file.
        ISA_RESULT_VALUES writebacks = replicate(tagged Invalid);

        if (isaGetLiteral(dp.req.instruction) matches tagged Valid .lit)
            src1 = lit;

        let funct = isaGetFunct(dp.req.instruction);

        // amask?
        if (funct[0] == 1)
            src1 = ~(`CPU_FEATURE_MASK);

        // Negate src1?
        let inp1 = (funct[3] == 0 ? src1 : ~src1);
        
        // Operate
        if (funct[6:5] == 'b01)
            writebacks[0] = tagged Valid (src0 | inp1);
        else if (funct[6:5] == 'b10)
            writebacks[0] = tagged Valid (src0 ^ inp1);
        else
            writebacks[0] = tagged Valid (src0 & inp1);

        debug_ALU(addr, opcode, funct, writebacks[0], src0, src1);

        // Return the result to the functional partition.
        dpResponseQ.deq();
        link_fp.makeResp(initISADatapathRspOp(tagged RNop));
        forwardWritebacks(dp.req, writebacks);
    endrule


    // ====================================================================
    //
    //   Bit operations pipeline (fast functions).  Slow functions are
    //   handled in multi-cycle rules below.
    //
    // ====================================================================

    rule dpBITOPS ((dpQ.first().pipe == ISA_DP_PIPE_BITOPS) && readyToRespondStd());
        let dp = dpQ.first();
        dpQ.deq();

        let addr = dp.req.instAddress;

        // Get sources from physical register file
        let reg_srcs <- getRegSources();

        Bit#(64) src0 = reg_srcs.srcValues[0];
        Bit#(64) src1 = reg_srcs.srcValues[1];

        OPCODE opcode = isaGetOpcode(dp.req.instruction);

        // The writebacks that are sent to the register file.
        ISA_RESULT_VALUES writebacks = replicate(tagged Invalid);
        FUNCP_ISA_DATAPATH_EXCEPTIONS except = FUNCP_ISA_EXCEPT_NONE;

        if (isaGetLiteral(dp.req.instruction) matches tagged Valid .lit)
            src1 = lit;

        let funct = isaGetFunct(dp.req.instruction);

        case (funct)
            sextb: writebacks[0] = tagged Valid signExtend(src0[7:0]);
            sextw: writebacks[0] = tagged Valid signExtend(src0[15:0]);

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
                temp[15:8] = src0[23:16];
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

            default:
            begin
                except = FUNCP_ISA_EXCEPT_ILLEGAL_INSTR;
                debugLog.record($format("[0x%x]   Marked instr ILLEGAL", addr));
            end
        endcase

        debug_ALU(addr, opcode, funct, writebacks[0], src0, src1);

        // Return the result to the functional partition.
        dpResponseQ.deq();
        link_fp.makeResp(initISADatapathRsp(except, tagged RNop));
        forwardWritebacks(dp.req, writebacks);
    endrule


    // ====================================================================
    //
    //   Slow bit operations.  Unpipelined using multiple cycles.
    //
    // ====================================================================

    // Hold input register values while operating one byte at a time
    FIFOF#(ISA_DP_BITOPS_SLOW_QUEUE) bitopsQ <- mkFIFOF1();

    // Pass intermediate results from byte-sized operations to an adder
    FIFO#(Tuple2#(Bit#(8), Bool)) bitopsSumQ <- mkFIFO();

    // Current byte index for operate rule
    Reg#(Maybe#(Bit#(3))) bitopsIdx <- mkRegU();

    // Intermediate sum
    Reg#(Bit#(11)) bitopsSum <- mkRegU();

    //
    // dpBITOPS_SLOW --
    //     Entry point for slow bit operations.
    //
    rule dpBITOPS_SLOW ((dpQ.first().pipe == ISA_DP_PIPE_BITOPS_SLOW) &&
                        ! bitopsQ.notEmpty());
        let dp = dpQ.first();
        dpQ.deq();

        let addr = dp.req.instAddress;

        // Get sources from physical register file
        let reg_srcs <- getRegSources();

        Bit#(64) src0 = reg_srcs.srcValues[0];
        Bit#(64) src1 = reg_srcs.srcValues[1];

        if (isaGetLiteral(dp.req.instruction) matches tagged Valid .lit)
            src1 = lit;

        let funct = isaGetFunct(dp.req.instruction);

        ISA_DP_BITOPS_SLOW_PIPE pipe = case (funct)
                                           ctpop: ISA_DP_BITOPS_CTPOP;
                                           perr: ISA_DP_BITOPS_PERR;
                                           ctlz: ISA_DP_BITOPS_CTLZ;
                                           cttz: ISA_DP_BITOPS_CTTZ;
                                       endcase;

        bitopsSum <= 0;
        bitopsIdx <= tagged Valid 7;
        bitopsQ.enq(ISA_DP_BITOPS_SLOW_QUEUE { pipe: pipe,
                                               req: dp.req,
                                               addr: addr,
                                               funct: funct,
                                               src0: src0,
                                               src1: src1 });
    endrule


    //
    // dpBITOPS_SLOW_Operate --
    //     Main work of slow bit operations.  Isolate one byte of the
    //     inputs, compute the result for the byte and pass the result
    //     on to the adder.
    //
    rule dpBITOPS_SLOW_Operate (bitopsIdx matches tagged Valid .vecIdx);
        let bit_op = bitopsQ.first();

        //
        // Operate on one 8 bit chunk for each iteration.
        //
        Vector#(8, Bit#(8)) vec0 = unpack(bit_op.src0);
        Vector#(8, Bit#(8)) vec1 = unpack(bit_op.src1);
        
        Bit#(3) idx;
        if (bit_op.pipe == ISA_DP_BITOPS_CTLZ)
            idx = vecIdx;
        else
            idx = 7 - vecIdx;
        
        Bit#(8) s0 = vec0[idx];
        Bit#(8) s1 = vec1[idx];

        Bit#(8) r = ?;
        Bool done = (vecIdx == 0);

        case (bit_op.pipe)
            ISA_DP_BITOPS_CTPOP:
            begin
                r = zeroExtend(pack(countOnes(s0)));
            end

            ISA_DP_BITOPS_PERR:
            begin
                r = (s0 >= s1) ? s0 - s1 : s1 - s0;
            end

            ISA_DP_BITOPS_CTLZ,
            ISA_DP_BITOPS_CTTZ:
            begin
                UInt#(4) z;
                if (bit_op.pipe == ISA_DP_BITOPS_CTLZ)
                    z = countZerosMSB(s0);
                else
                    z = countZerosLSB(s0);

                done = done || (z != 8);
                r = zeroExtend(pack(z));
            end
        endcase

        if (done)
            bitopsIdx <= tagged Invalid;
        else
            bitopsIdx <= tagged Valid (vecIdx - 1);

        bitopsSumQ.enq(tuple2(r, done));
    endrule


    //
    // dpBITOPS_SLOW_Sum --
    //     Add intermediate results from operate stage.  Send result to
    //     functional partition if done.
    //
    rule dpBITOPS_SLOW_Sum (readyToRespondTok(bitopsQ.first().req.token.index));
        match { .r, .done } = bitopsSumQ.first();
        bitopsSumQ.deq();

        // Add to sum from previous portions of the input
        Bit#(11) sum = bitopsSum + zeroExtend(r);
        bitopsSum <= sum;

        // Return the result to the functional partition.
        if (done)
        begin
            let bit_op = bitopsQ.first();
            bitopsQ.deq();

            // The writebacks that are sent to the register file.
            ISA_RESULT_VALUES writebacks = replicate(tagged Invalid);
            writebacks[0] = tagged Valid zeroExtend(sum);

            debug_ALU(bit_op.addr, opc1c, bit_op.funct, writebacks[0], bit_op.src0, bit_op.src1);

            // Return the result to the functional partition.
            dpResponseQ.deq();
            link_fp.makeResp(initISADatapathRspOp(tagged RNop));
            forwardWritebacks(bit_op.req, writebacks);
        end
    endrule


    // ====================================================================
    //
    //   Branch pipeline
    //
    // ====================================================================

    rule dpBRANCH ((dpQ.first().pipe == ISA_DP_PIPE_BRANCH) && readyToRespondStd());
        let dp = dpQ.first();
        dpQ.deq();

        let addr = dp.req.instAddress;

        // Get sources from physical register file
        let reg_srcs <- getRegSources();

        Bit#(64) src0 = reg_srcs.srcValues[0];

        OPCODE opcode = isaGetOpcode(dp.req.instruction);
        let branch_imm = isaGetBranchImmediate(dp.req.instruction);

        // The writebacks that are sent to the register file.
        ISA_RESULT_VALUES writebacks = replicate(tagged Invalid);

        // The result for the timing partition.
        FUNCP_ISA_EXECUTION_RESULT timep_result = tagged RNop;
        
        case (opcode)
            beq, bge, bgt, blbc, blbs, ble, blt, bne:
            begin
                let newAddr = addr + 4 + (signExtend(branch_imm) << 2);
                Bool taken = case (opcode)
                                 beq : return src0 == 0;
                                 bge : return signedGE(src0, 0);
                                 bgt : return signedGT(src0, 0);
                                 blbc: return truncate(src0) == 1'b0;
                                 blbs: return truncate(src0) == 1'b1;
                                 ble : return signedLE(src0, 0);
                                 blt : return signedLT(src0, 0);
                                 bne : return src0 != 0;
                             endcase;
                debugLog.record($format("[0x%x] Bxx to 0x%x, src0=0x%x, %staken", addr, newAddr, src0, taken? "": "not "));
                timep_result = taken? tagged RBranchTaken truncate(newAddr): tagged RBranchNotTaken truncate(addr + 4);
            end

            fbeq, fblt, fble, fbne, fbge, fbgt:
            begin
                let newAddr = addr + 4 + (signExtend(branch_imm) << 2);
                let sign_bit = src0[63];
                Bool taken = case (opcode)
                                 // src0IsFPZero includes test for negative zero
                                 fbeq: return src0IsFPZero;
                                 fblt: return (sign_bit == 1) && ! src0IsFPZero;
                                 fble: return (sign_bit == 1) || src0IsFPZero;
                                 fbne: return ! src0IsFPZero;
                                 fbge: return (sign_bit == 0) || src0IsFPZero;
                                 fbgt: return (sign_bit == 0) && ! src0IsFPZero;
                             endcase;
                debugLog.record($format("[0x%x] FBxx to 0x%x, src0=0x%x, %staken", addr, newAddr, src0, taken? "": "not "));
                timep_result = taken? tagged RBranchTaken truncate(newAddr): tagged RBranchNotTaken truncate(addr + 4);
            end

            br, bsr:
            begin
                writebacks[0] = tagged Valid (addr + 4);
                let newAddr = addr + 4 + (signExtend(branch_imm) << 2);
                debugLog.record($format("[0x%x] BxR to 0x%x", addr, newAddr));
                timep_result = tagged RBranchTaken truncate(newAddr);
            end

            jmp:
            begin
                let newAddr = src0 & ~3;
                writebacks[0] = tagged Valid (addr + 4);
                debugLog.record($format("[0x%x] JMP to 0x%x", addr, newAddr));
                timep_result = tagged RBranchTaken truncate(newAddr);
            end

            default:
            begin
                debugLog.record($format("[0x%x] Unexpected OPCODE in BRANCH pipeline", addr));
                assertUnexpectedOpcode(False);
            end
        endcase

        // Return the result to the functional partition.
        dpResponseQ.deq();
        link_fp.makeResp(initISADatapathRspOp(timep_result));
        forwardWritebacks(dp.req, writebacks);
    endrule


    // ====================================================================
    //
    //   Multiply pipeline
    //
    // ====================================================================

    FIFO#(ISA_DP_MUL_QUEUE) mulQ <- mkFIFO();

    // Pipelined multiplier
    HASIM_COMPACT_MUL#(64) multiplier <- mkCompactUnsignedMul();

    rule dpMUL (dpQ.first().pipe == ISA_DP_PIPE_MUL);
        let dp = dpQ.first();
        dpQ.deq();

        let addr = dp.req.instAddress;

        // Get sources from physical register file
        let reg_srcs <- getRegSources();

        Bit#(64) src0 = reg_srcs.srcValues[0];
        Bit#(64) src1 = reg_srcs.srcValues[1];

        if (isaGetLiteral(dp.req.instruction) matches tagged Valid .lit)
            src1 = lit;

        let funct = isaGetFunct(dp.req.instruction);

        ISA_DP_MUL_QUEUE mulReq = ?;
        mulReq.req = dp.req;
        mulReq.isNegative = False;
        mulReq.addr = addr;
        mulReq.funct = funct;
        mulReq.src0 = src0;
        mulReq.src1 = src1;

        Bool is_signed = True;

        Bit#(64) inp0 = src0;
        Bit#(64) inp1 = src1;

        case (funct)
            mull, mullv:
            begin
                inp0 = signExtend(src0[31:0]);
                inp1 = signExtend(src1[31:0]);
                mulReq.pipe = ISA_DP_MUL_MULL;
                mulReq.testOverflow = (funct == mullv);
            end

            mulq, mulqv:
            begin
                mulReq.pipe = ISA_DP_MUL_MULQ;
                mulReq.testOverflow = (funct == mulqv);
            end

            umulh:
            begin
                is_signed = False;
                mulReq.pipe = ISA_DP_MUL_UMULH;
                mulReq.testOverflow = False;
            end
        endcase

        //
        // Multiplier is unsigned.  For signed multiply record whether
        // the result will be negative, multiply the absolute
        // value of the inputs and invert the result if needed.
        //

        if (is_signed)
        begin
            // Result will be negative if exactly one input is negative
            mulReq.isNegative = (inp0[63] ^ inp1[63]) == 1;

            // Make arguments positive.  Sign will be set on output.
            inp0 = inp0[63] == 0 ? inp0 : -inp0;
            inp1 = inp1[63] == 0 ? inp1 : -inp1;
        end

        multiplier.req(inp0, inp1);
        mulQ.enq(mulReq);
    endrule


    //
    // dpMULResult --
    //     Consume the output of the pipelined multiplier.
    //
    rule dpMULResult (readyToRespondTok(mulQ.first().req.token.index));
        let prod <- multiplier.resp();

        let mul_req = mulQ.first();
        mulQ.deq();

        if (mul_req.isNegative)
            prod = -prod;

        // The writebacks that are sent to the register file.
        ISA_RESULT_VALUES writebacks = replicate(tagged Invalid);

        case (mul_req.pipe)
            ISA_DP_MUL_MULL:
            begin
                writebacks[0] = tagged Valid signExtend(prod[31:0]);
                if (mul_req.testOverflow)
                    writebacks[1] = tagged Valid (signedMulOverflow(prod[63:0]) ? 1 : 0);
            end

            ISA_DP_MUL_MULQ:
            begin
                writebacks[0] = tagged Valid prod[63:0];
                if (mul_req.testOverflow)
                    writebacks[1] = tagged Valid (signedMulOverflow(prod) ? 1 : 0);
            end

            ISA_DP_MUL_UMULH:
            begin
                writebacks[0] = tagged Valid prod[127:64];
            end
        endcase

        debug_ALU(mul_req.addr, opc13, mul_req.funct, writebacks[0], mul_req.src0, mul_req.src1);

        // Return the result to the functional partition.
        dpResponseQ.deq();
        link_fp.makeResp(initISADatapathRspOp(tagged RNop));
        forwardWritebacks(mul_req.req, writebacks);
    endrule


    // ====================================================================
    //
    //   HAsim control pipeline (simulator control pseudo instructions)
    //
    // ====================================================================

    rule dpCONTROL ((dpQ.first().pipe == ISA_DP_PIPE_CONTROL) && readyToRespondStd());
        let dp = dpQ.first();
        dpQ.deq();

        let addr = dp.req.instAddress;

        // Get sources from physical register file
        let reg_srcs <- getRegSources();

        if (isaGetMemFunc(dp.req.instruction) == exit)
        begin
            Bit#(64) src0 = reg_srcs.srcValues[0];
            debugLog.record($format("[0x%x] EXIT src0 = 0x%x", addr, src0));
            dpResponseQ.deq();
            link_fp.makeResp(initISADatapathRspOp(tagged RTerminate unpack(truncate(src0))));
        end
        else
        begin
            debugLog.record($format("[0x%x] CONTROL NOP", addr));
            dpResponseQ.deq();
            link_fp.makeResp(initISADatapathRspNop());
        end

        forwardWritebacks(dp.req, replicate(Invalid));
    endrule


    // ====================================================================
    //
    //   Illegal opcode pipeline
    //
    // ====================================================================

    rule dpILLEGAL ((dpQ.first().pipe == ISA_DP_PIPE_ILLEGAL) && readyToRespondStd());
        let dp = dpQ.first();
        dpQ.deq();

        let addr = dp.req.instAddress;

        // Get sources from physical register file
        let reg_srcs <- getRegSources();

        debugLog.record($format("[0x%x]   Marked instr ILLEGAL", addr));

        // Return the result to the functional partition.
        dpResponseQ.deq();
        link_fp.makeResp(initISADatapathRspException(FUNCP_ISA_EXCEPT_ILLEGAL_INSTR));
        forwardWritebacks(dp.req, replicate(Invalid));
    endrule


    // ====================================================================
    //
    //   Memory address pipeline
    //
    // ====================================================================

    rule dpMEMADDR ((dpQ.first().pipe == ISA_DP_PIPE_MEMADDR) && readyToRespondStd());
        let dp = dpQ.first();
        dpQ.deq();

        let addr = dp.req.instAddress;

        // Get sources from physical register file
        let reg_srcs <- getRegSources();

        Bit#(64) src0 = reg_srcs.srcValues[0];
        Bit#(64) src1 = reg_srcs.srcValues[1];
        Bit#(64) src2 = reg_srcs.srcValues[2];

        OPCODE opcode = isaGetOpcode(dp.req.instruction);
        let mem_disp = isaGetMemDisp(dp.req.instruction);

        // The writebacks that are sent to the register file.
        ISA_RESULT_VALUES writebacks = replicate(tagged Invalid);

        // The effective address for Loads/Stores
        ISA_ADDRESS effective_addr = src0 + mem_disp;

        if ((opcode == ldq_u) || (opcode == stq_u))
            effective_addr = effective_addr & ~7;

        case (opcode)
            ldbu, ldl, ldq, ldwu,
            ldt, lds:
            begin
                debugLog.record($format("[0x%x] LD [0x%x]", addr, effective_addr));
            end

            ldq_u:
            begin
                debugLog.record($format("[0x%x] LDQ_U [0x%x]", addr, effective_addr));
            end

            ldl_l, ldq_l:
            begin
                writebacks[1] = tagged Valid 1;
                writebacks[2] = tagged Valid effective_addr;
                debugLog.record($format("[0x%x] LD_L [0x%x]", addr, effective_addr));
            end

            stl_c, stq_c:
            begin
                writebacks[0] = tagged Valid src1;
                writebacks[1] = tagged Valid src2;
                writebacks[2] = tagged Valid 0;
                debugLog.record($format("[0x%x] ST_C [0x%x] <- 0x%x", addr, effective_addr, src1));
            end

            stb, stl, stq, stw,
            stt:
            begin
                writebacks[0] = tagged Valid src1;
                debugLog.record($format("[0x%x] ST [0x%x] <- 0x%x", addr, effective_addr, src1));
            end

            sts:
            begin
                // Convert T to S
                Bit#(32) single_fp = { src1[63:62],   // Sign bit and high exponent bit
                                       src1[58:29] }; // Rest of exponent and fraction
                writebacks[0] = tagged Valid zeroExtend(single_fp);
                debugLog.record($format("[0x%x] STS [0x%x] <- 0x%x", addr, effective_addr, single_fp));
            end

            stq_u:
            begin
                writebacks[0] = tagged Valid src1;
                debugLog.record($format("[0x%x] STQ_U [0x%x] <- 0x%x", addr, effective_addr, src1));
            end

            default:
            begin
                debugLog.record($format("[0x%x] Unexpected OPCODE in MEMADDR pipeline", addr));
                assertUnexpectedOpcode(False);
            end
        endcase

        // Effective address for timing partition
        FUNCP_ISA_EXECUTION_RESULT timep_result = REffectiveAddr(effective_addr);

        // Return the result to the functional partition.
        dpResponseQ.deq();
        link_fp.makeResp(initISADatapathRspOp(timep_result));
        forwardWritebacks(dp.req, writebacks);
    endrule


    // ====================================================================
    //
    //   NOP pipeline
    //
    // ====================================================================

    rule dpNOP ((dpQ.first().pipe == ISA_DP_PIPE_NOP) && readyToRespondStd());
        let dp = dpQ.first();
        dpQ.deq();

        let addr = dp.req.instAddress;

        // Get sources from physical register file
        let reg_srcs <- getRegSources();

        debugLog.record($format("[0x%x] NOP", addr));

        // Return the result to the functional partition.
        dpResponseQ.deq();
        link_fp.makeResp(initISADatapathRspNop());
        forwardWritebacks(dp.req, replicate(Invalid));
    endrule


    // ====================================================================
    //
    //   Shift pipeline
    //
    // ====================================================================

    rule dpSHIFT ((dpQ.first().pipe == ISA_DP_PIPE_SHIFT) && readyToRespondStd());
        let dp = dpQ.first();
        dpQ.deq();

        let addr = dp.req.instAddress;

        // Get sources from physical register file
        let reg_srcs <- getRegSources();

        Bit#(64) src0 = reg_srcs.srcValues[0];
        Bit#(64) src1 = reg_srcs.srcValues[1];

        OPCODE opcode = isaGetOpcode(dp.req.instruction);

        // The writebacks that are sent to the register file.
        ISA_RESULT_VALUES writebacks = replicate(tagged Invalid);
        FUNCP_ISA_DATAPATH_EXCEPTIONS except = FUNCP_ISA_EXCEPT_NONE;

        if (isaGetLiteral(dp.req.instruction) matches tagged Valid .lit)
            src1 = lit;

        let funct = isaGetFunct(dp.req.instruction);

        case (funct)
            mskbl, mskwl, mskll, mskql, mskwh, msklh, mskqh:
            begin
                Bit#(16) byteMask = case (funct)
                                        mskbl: 'b1;
                                        mskwl, mskwh: 'b11;
                                        mskll, msklh: 'b1111;
                                        mskql, mskqh: 'b11111111;
                                    endcase;
                byteMask = byteMask << src1[2:0];
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
                byteMask = byteMask << src1[2:0];
                case (funct)
                    insbl, inswl, insll, insql:
                    begin
                        let temp = src0 << ({src1[2:0], 3'b0})[5:0];
                        writebacks[0] = tagged Valid byteZap(temp, ~byteMask[7:0]);
                    end
                    inswh, inslh, insqh:
                    begin
                        Bit#(7) shift = 64 - signExtend({src1[2:0], 3'b0});
                        let temp = src0 >> shift[5:0];
                        writebacks[0] = tagged Valid byteZap(temp, ~byteMask[15:8]);
                    end
                endcase
            end

            zap: writebacks[0] = tagged Valid byteZap(src0, src1[7:0]);
            zapnot: writebacks[0] = tagged Valid byteZap(src0, ~src1[7:0]);

            srl: writebacks[0] = tagged Valid (src0 >> src1[5:0]);
            sll: writebacks[0] = tagged Valid (src0 << src1[5:0]);
            sra: writebacks[0] = tagged Valid signedShiftRight(src0, src1[5:0]);

            default:
            begin
                except = FUNCP_ISA_EXCEPT_ILLEGAL_INSTR;
                debugLog.record($format("[0x%x]   Marked instr ILLEGAL", addr));
            end
        endcase

        debug_ALU(addr, opcode, funct, writebacks[0], src0, src1);

        // Return the result to the functional partition.
        dpResponseQ.deq();
        link_fp.makeResp(initISADatapathRsp(except, tagged RNop));
        forwardWritebacks(dp.req, writebacks);
    endrule

    
    // ====================================================================
    //
    //   Integer copies on FP registers
    //
    // ====================================================================

    rule dpIntOnFP ((dpQ.first().pipe == ISA_DP_PIPE_FP_INT) && readyToRespondStd());
        let dp = dpQ.first();
        dpQ.deq();

        let addr = dp.req.instAddress;

        // Get sources from physical register file
        let reg_srcs <- getRegSources();

        Bit#(64) src0 = reg_srcs.srcValues[0];
        Bit#(64) src1 = reg_srcs.srcValues[1];

        OPCODE opcode = isaGetOpcode(dp.req.instruction);

        // The writebacks that are sent to the register file.
        ISA_RESULT_VALUES writebacks = replicate(tagged Invalid);

        let funct = isaGetFPFunc(dp.req.instruction);
        
        Bit#(64) d = ?;

        case (funct)
            cvtlq:
            begin
                d = signExtend({ src0[63:62], src0[58:29] });
            end

            cpys:
            begin
                d = { src0[63], src1[62:0] };
            end

            cpysn:
            begin
                d = { ~src0[63], src1[62:0] };
            end

            cpyse:
            begin
                d = { src0[63:52], src1[51:0] };
            end

            cvtql:
            begin
                d = { src0[31:30], 3'b0, src0[29:0], 29'b0 };
            end
        endcase

        writebacks[0] = tagged Valid d;

        debug_FP(addr, opcode, funct, writebacks[0], src0, src1);

        // Return the result to the functional partition.
        dpResponseQ.deq();
        link_fp.makeResp(initISADatapathRspOp(tagged RNop));
        forwardWritebacks(dp.req, writebacks);
    endrule


    // ====================================================================
    //
    //   Register transfer between FP and integer registers.
    //
    // ====================================================================

    rule dpFromFP ((dpQ.first().pipe == ISA_DP_PIPE_XFER_FROM_FP) && readyToRespondStd());
        let dp = dpQ.first();
        dpQ.deq();

        let addr = dp.req.instAddress;

        // Get sources from physical register file
        let reg_srcs <- getRegSources();

        Bit#(64) src0 = reg_srcs.srcValues[0];

        OPCODE opcode = isaGetOpcode(dp.req.instruction);

        // The writebacks that are sent to the register file.
        ISA_RESULT_VALUES writebacks = replicate(tagged Invalid);

        let funct = isaGetFunct(dp.req.instruction);
        
        case (funct)
            ftois:
            begin
                Bit#(32) s = signExtend(src0[63]);
                Bit#(64) d = { s, src0[63:62], src0[58:29] };
                writebacks[0] = tagged Valid d;
            end

            ftoit:
            begin
                writebacks[0] = tagged Valid src0;
            end
        endcase

        debug_ALU(addr, opcode, funct, writebacks[0], src0, 0);

        // Return the result to the functional partition.
        dpResponseQ.deq();
        link_fp.makeResp(initISADatapathRspOp(tagged RNop));
        forwardWritebacks(dp.req, writebacks);
    endrule


    rule dpToFP ((dpQ.first().pipe == ISA_DP_PIPE_XFER_TO_FP) && readyToRespondStd());
        let dp = dpQ.first();
        dpQ.deq();

        let addr = dp.req.instAddress;

        // Get sources from physical register file
        let reg_srcs <- getRegSources();

        Bit#(64) src0 = reg_srcs.srcValues[0];

        OPCODE opcode = isaGetOpcode(dp.req.instruction);

        // The writebacks that are sent to the register file.
        ISA_RESULT_VALUES writebacks = replicate(tagged Invalid);

        let funct = isaGetFPFunc(dp.req.instruction);
        
        case (funct)
            itoff:
            begin
                Bit#(64) d = { src0[31], isaMAP_F(src0[30:23]), src0[22:0], 29'b0 };
                writebacks[0] = tagged Valid d;
            end

            itofs:
            begin
                Bit#(64) d = { src0[31], isaMAP_S(src0[30:23]), src0[22:0], 29'b0 };
                writebacks[0] = tagged Valid d;
            end

            itoft:
            begin
                writebacks[0] = tagged Valid src0;
            end
        endcase

        debug_FP(addr, opcode, funct, writebacks[0], src0, 0);

        // Return the result to the functional partition.
        dpResponseQ.deq();
        link_fp.makeResp(initISADatapathRspOp(tagged RNop));
        forwardWritebacks(dp.req, writebacks);
    endrule


    // ====================================================================
    //
    //   Floating point CMOV.
    //
    // ====================================================================

    //    
    // fpZeroTest --
    //     Generate a wire with tests for floating point zero.  The result is
    //     shared by multiple rules (CMOV and floating branch).
    //
    rule fpZeroTest (True);
        let reg_srcs = link_fp_srcvals.receive();
        Bit#(64) src0 = reg_srcs.srcValues[0];

        // Test for both zero and negative zero by ignoring the sign bit
        src0IsFPZero <= (src0[62:0] == 63'b0);
    endrule


    //
    // dpFPCMOV --
    //     Floating point conditional move.
    //
    rule dpFPCMOV ((dpQ.first().pipe == ISA_DP_PIPE_FP_CMOV) && readyToRespondStd());
        let dp = dpQ.first();
        dpQ.deq();

        let addr = dp.req.instAddress;

        // Get sources from physical register file
        let reg_srcs <- getRegSources();

        Bit#(64) src0 = reg_srcs.srcValues[0];
        Bit#(64) src1 = reg_srcs.srcValues[1];
        Bit#(64) src2 = reg_srcs.srcValues[2];

        OPCODE opcode = isaGetOpcode(dp.req.instruction);

        // The writebacks that are sent to the register file.
        ISA_RESULT_VALUES writebacks = replicate(tagged Invalid);
        FUNCP_ISA_DATAPATH_EXCEPTIONS except = FUNCP_ISA_EXCEPT_NONE;

        let funct = isaGetFPFunc(dp.req.instruction);
        let sign_bit = src0[63];

        case (funct)
            fcmoveq: writebacks[0] = tagged Valid ((src0IsFPZero) ? src1 : src2);
            fcmovne: writebacks[0] = tagged Valid ((! src0IsFPZero) ? src1 : src2);
            fcmovlt: writebacks[0] = tagged Valid (((sign_bit == 1) && ! src0IsFPZero) ? src1 : src2);
            fcmovge: writebacks[0] = tagged Valid (((sign_bit == 0) || src0IsFPZero) ? src1 : src2);
            fcmovle: writebacks[0] = tagged Valid (((sign_bit == 1) || src0IsFPZero) ? src1 : src2);
            fcmovgt: writebacks[0] = tagged Valid (((sign_bit == 0) && ! src0IsFPZero) ? src1 : src2);

            default:
            begin
                except = FUNCP_ISA_EXCEPT_ILLEGAL_INSTR;
                debugLog.record($format("[0x%x]   Marked instr ILLEGAL", addr));
            end
        endcase

        debug_FP3(addr, opcode, funct, writebacks[0], src0, src1, src2);

        // Return the result to the functional partition.
        dpResponseQ.deq();
        link_fp.makeResp(initISADatapathRsp(except, tagged RNop));
        forwardWritebacks(dp.req, writebacks);
    endrule


    // ====================================================================
    //
    //   Floating point emulation pipeline.
    //
    // ====================================================================

    // Emulation RRR Stubs
    ClientStub_ISA_REGOP_EMULATOR emulClient <- mkClientStub_ISA_REGOP_EMULATOR();

    // Internal communication with details of emulation requests
    FIFO#(Tuple2#(TOKEN, Maybe#(FUNCP_PHYSICAL_REG_INDEX))) dpFPEmulQ <- mkSizedFIFO(4);


    //
    // dpFPEmulReq --
    //     Pass the instruction and input values to software.  Send an early
    //     response to the timing model that the instruction is a "normal"
    //     operation.  The destination register will be written when the
    //     result comes back from software.
    //
    (* descending_urgency = "dpFPEmulReq, dpFPCMOV, dpToFP, dpFromFP, dpIntOnFP, dpSHIFT, dpNOP, dpMEMADDR, dpILLEGAL, dpCONTROL, dpMULResult, dpBRANCH, dpBITOPS_SLOW_Sum, dpBITOPS, dpLOGICAL, dpCMOV, dpCMP, dpADD" *)
    rule dpFPEmulReq ((dpQ.first().pipe == ISA_DP_PIPE_FP_EMUL) && readyToRespondStd());
        let dp = dpQ.first();
        dpQ.deq();

        let addr = dp.req.instAddress;

        // Get sources from physical register file
        let reg_srcs <- getRegSources();

        Bit#(64) src0 = reg_srcs.srcValues[0];
        Bit#(64) src1 = reg_srcs.srcValues[1];

        ISA_REG_INDEX src0_reg = tagged FPReg dp.req.instruction[25:21];
        ISA_REG_INDEX src1_reg = tagged FPReg dp.req.instruction[20:16];
        ISA_REG_INDEX dst_reg = tagged FPReg dp.req.instruction[4:0];

        // The writebacks that are sent to the register file.
        ISA_RESULT_VALUES writebacks = replicate(tagged Invalid);

        debugLog.record($format("[0x%x] FPEmul F%0d <- 0x%x op 0x%x", addr, dp.req.instruction[4:0], src0, src1));

        //
        // Request emulation
        //
        emulClient.makeRequest_emulateRegOp(
            contextIdToRRR(tokContextId(dp.req.token)),
            dp.req.instruction,
            dp.req.instAddress,
            src0,
            src1,
            zeroExtend(pack(src0_reg)),
            zeroExtend(pack(src1_reg)),
            zeroExtend(pack(dst_reg)));

        // Return the result to the functional partition.  For floating point
        // emulation we assume there is no exception and no control changes.
        // The register write will happen when the result is returned from
        // software.
        dpResponseQ.deq();
        link_fp.makeResp(initISADatapathRspOp(tagged RNop));

        //
        // Pass details of the request to the stage that will receive the response
        // from emulation.  Responses will be returned in order.
        //
        dpFPEmulQ.enq(tuple2(dp.req.token, dp.req.instDstPhysRegs[0]));
        statISAEmul.incr();
    endrule


    //
    // dpFPEmulResp --
    //     Get the floating point emulation response from software.
    //
    rule dpFPEmulResp (True);
        let dstVal <- emulClient.getResponse_emulateRegOp();

        match {.tok, .dst_pr} = dpFPEmulQ.first();
        dpFPEmulQ.deq();
        
        debugLog.record($format("FPEmulResp PR%0d <- 0x%x", validValue(dst_pr), dstVal));

        link_fp_writeback.send(initISAWriteback(tok, dst_pr, dstVal, True));
    endrule


    // ====================================================================
    //
    // Writeback pipeline
    //
    // ====================================================================

    //    
    // handleWritebacks --
    //     Read register write requests from the writebackQ and forward them
    //     to the register state manager.
    //
    (* descending_urgency = "dpFPEmulResp, handleWritebacks" *)
    rule handleWritebacks (True);
        match {.tok, .m_pr, .val} = writebackQ.first();
        writebackQ.deq();

        // All register writes requested in the same cycle (group) are for
        // the same token.  A token is done if the write request is the
        // last in the group.
        link_fp_writeback.send(initISAWriteback(tok,
                                                m_pr,
                                                val,
                                                writebackQ.lastInGroup()));
    endrule
endmodule
