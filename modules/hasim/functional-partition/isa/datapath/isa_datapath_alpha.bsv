
// isa_datapath_template

// This file contains a template ISA datapath.

// You should fill this in with with an ISA-specific ALU, 
// which may be pipelined if you so choose.


// ***** Imports *****

import Vector::*;

import hasim_common::*;
import soft_connections::*;

import hasim_isa::*;

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
        ISA_RESULT_VALUES writebacks = Vector::replicate(Invalid);

        let    opcode = inst[31:26];
        let        ra = inst[25:21];
        let        rb = inst[20:16];
        let       lit = inst[20:13];
        Bool   useLit = unpack(inst[12]);
        let     funct = inst[11:5];
        let        rc = inst[4:0];
        let branchImm = inst[20:0];
        let    memImm = inst[15:0];

        ISA_VALUE src1 = useLit? signExtend(lit): srcs[1];

        function ISA_VALUE byteZap(ISA_VALUE srcBits, Bit#(ISA_MASK_NUM) mask);
            Vector#(ISA_MASK_NUM, Bit#(ISA_MASK_SIZE)) res = newVector();
            Vector#(ISA_MASK_NUM, Bit#(ISA_MASK_SIZE)) src = unpack(srcBits);
            for(Integer i = 0; i < valueOf(ISA_MASK_NUM); i=i+1)
                res[i] = (mask[i] == 1)? src[i]: 0;
            return pack(res);
        endfunction

        case (opcode)
            opcode01:
            begin
                case (memImm)
                    exit: timep_result = tagged RTerminate unpack(truncate(srcs[0]));
                endcase
            end

            //Memory Format Jump instructions
            br, bsr:
            begin
                writebacks[0] = tagged Valid addr;
                let newAddr = addr + (srcs[0] << 2);
                timep_result = tagged RBranchTaken newAddr;
            end

            //Branch Instructions
            beq, bge, bgt, blbc, blbs, ble, blt, bne:
            begin
                let newAddr = addr + (srcs[0] << 2);
                Bool taken = case (opcode)
                                 beq : return srcs[0] == 0;
                                 bge : return srcs[0] >= 0;
                                 bgt : return srcs[0] > 0;
                                 blbc: return truncate(srcs[0]) == 1'b0;
                                 blbs: return truncate(srcs[0]) == 1'b1;
                                 ble : return srcs[0] <= 0;
                                 blt : return srcs[0] < 0;
                                 bne : return srcs[0] != 0;
                             endcase;
                timep_result = taken? tagged RBranchTaken newAddr: tagged RBranchNotTaken (addr + 4);
            end

            jmp:
            begin
                let newAddr = srcs[1] & ~3;
                writebacks[0] = tagged Valid addr;
                timep_result = tagged RBranchTaken newAddr;
            end

            lda:
            begin
                writebacks[0] = tagged Valid (srcs[0] + signExtend(memImm));
            end

            ldl, ldq, ldwu, ldbu, ldq_u:
            begin
                effective_addr = case (opcode)
                                     ldq_u  : return (srcs[0] + signExtend(memImm)) & ~7;
                                     default: return srcs[0] + signExtend(memImm);
                                 endcase;
                timep_result = tagged REffectiveAddr effective_addr;
            end

            arith:
            begin
                case (funct)
                    addl  : writebacks[0] = tagged Valid signExtend((srcs[0] + src1)[31:0]);
                    s4addl: writebacks[0] = tagged Valid signExtend(((srcs[0] << 2) + src1)[31:0]);
                    s8addl: writebacks[0] = tagged Valid signExtend(((srcs[0] << 3) + src1)[31:0]);
                    addq  : writebacks[0] = tagged Valid (srcs[0] + src1);
                    s4addq: writebacks[0] = tagged Valid ((srcs[0] << 2) + src1);
                    s8addq: writebacks[0] = tagged Valid ((srcs[0] << 3) + src1);
                    cmpeq : writebacks[0] = tagged Valid zeroExtend(pack(srcs[0] == src1));
                    cmple : writebacks[0] = tagged Valid zeroExtend(pack(signedLE(srcs[0], src1)));
                    cmplt : writebacks[0] = tagged Valid zeroExtend(pack(signedLT(srcs[0], src1)));
                    cmpule: writebacks[0] = tagged Valid zeroExtend(pack(srcs[0] <= src1));
                    cmpult: writebacks[0] = tagged Valid zeroExtend(pack(srcs[0] < src1));
                    mull  : writebacks[0] = tagged Valid signExtend((srcs[0] * src1)[31:0]);
                    mulq  : writebacks[0] = tagged Valid (srcs[0] * src1);
                    umulh :
                    begin
                        Bit#(128) res = zeroExtend(srcs[0]) * zeroExtend(src1);
                        writebacks[0] = tagged Valid truncate(res[127:64]);
                    end
                    subl  : writebacks[0] = tagged Valid signExtend((srcs[0] - src1)[31:0]);
                    s4subl: writebacks[0] = tagged Valid signExtend(((srcs[0] << 2) - src1)[31:0]);
                    s8subl: writebacks[0] = tagged Valid signExtend(((srcs[0] << 3) - src1)[31:0]);
                    subq  : writebacks[0] = tagged Valid (srcs[0] - src1);
                    s4subq: writebacks[0] = tagged Valid ((srcs[0] << 2) - src1);
                    s8subq: writebacks[0] = tagged Valid ((srcs[0] << 3) - src1);
                endcase
            end

            logical:
            begin
                case (funct)
                    andOp  : writebacks[0] = tagged Valid (srcs[0] & src1);
                    bicOp  : writebacks[0] = tagged Valid (srcs[0] & ~src1);
                    bisOp  : writebacks[0] = tagged Valid (srcs[0] | src1);
                    eqvOp  : writebacks[0] = tagged Valid (srcs[0] ^ ~src1);
                    orNotOp: writebacks[0] = tagged Valid (srcs[0] | ~src1);
                    xorOp  : writebacks[0] = tagged Valid (srcs[0] ^ src1);
                    cmoveq : writebacks[0] = tagged Valid ((srcs[0] == 0)? src1: srcs[2]);
                    cmovge : writebacks[0] = tagged Valid ((srcs[0] >= 0)? src1: srcs[2]);
                    cmovgt : writebacks[0] = tagged Valid ((srcs[0] > 0)? src1: srcs[2]);
                    cmovlbc : writebacks[0] = tagged Valid ((truncate(srcs[0]) == 1'b0)? src1: srcs[2]);
                    cmovlbs : writebacks[0] = tagged Valid ((truncate(srcs[0]) == 1'b1)? src1: srcs[2]);
                    cmovle : writebacks[0] = tagged Valid ((srcs[0] <= 0)? src1: srcs[2]);
                    cmovlt : writebacks[0] = tagged Valid ((srcs[0] < 0)? src1: srcs[2]);
                    cmovne : writebacks[0] = tagged Valid ((srcs[0] != 0)? src1: srcs[2]);
                    sll    : writebacks[0] = tagged Valid (srcs[0] << src1[5:0]);
                    srl    : writebacks[0] = tagged Valid (srcs[0] >> src1[5:0]);
                    sra    : writebacks[0] = tagged Valid signedShiftRight(srcs[0], src1[5:0]);
                endcase
            end

            byteManipulation:
            begin
                case (funct)
                    zapnot: writebacks[0] = tagged Valid (byteZap(srcs[0], src1[7:0]));
                endcase
            end
        endcase

        // Return the result to the functional partition.
        link_fp.makeResp(tuple3(timep_result, effective_addr, writebacks));

    endrule

endmodule
