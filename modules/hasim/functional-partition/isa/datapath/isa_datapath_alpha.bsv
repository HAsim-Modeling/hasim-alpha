
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
            Vector#(ISA_MASK_NUM, Bit#(ISA_MASK_SIZE)) ret = newVector();
            Vector#(ISA_MASK_NUM, Bit#(ISA_MASK_SIZE)) src = unpack(srcBits);
            for(Integer i = 0; i < `ISA_MASK_NUM; i=i+1)
                res[i] = mask[i] == 1? src[i]: 0;
            return pack(res);
        endfunction

        case (opcode)
            opcode01:
            begin
                case (memImm)
                    exit: timep_result = tagged RTerminate src[0];
                endcase
            end

            bsr:
            begin
                writebacks[0] = tagged Valid addr;
                let newAddr = addr + (srcs[0] << 2);
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
                    addq  : writebacks[0] = tagged Valid (src[0] + src1);
                    cmpeq : writebacks[0] = tagged Valid zeroExtend(pack(src[0] == src1));
                endcase
            end

            logical:
            begin
                case (funct)
                    andOp : writebacks[0] = tagged Valid (src[0] & src1);
                    xorOp : writebacks[0] = tagged Valid (src[0] ^ src1);
                    eqvOp : writebacks[0] = tagged Valid (~(src[0] ^ src1));
                    bisOp : writebacks[0] = tagged Valid (src[0] | src1);
                endcase
            end

            byteManipulation:
            begin
                case (funct)
                    zapnot: byteZap(src[0], src1[7:0]);
                endcase
            end
        endcase

        // Return the result to the functional partition.
        link_fp.makeResp(tuple3(timep_result, effective_addr, writebacks));

    endrule

endmodule
