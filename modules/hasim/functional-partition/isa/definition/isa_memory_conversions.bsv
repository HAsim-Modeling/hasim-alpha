
`include "funcp_memory.bsh"

// isaAlignAddress

// This function takes an arbitrary address and aligns it to the standard
// memory reference size for the ISA.  The result will typically be passed
// to the TLB.

function Tuple2#(ISA_ADDRESS, MEM_OFFSET) isaAlignAddress(ISA_ADDRESS a);

    return tuple2({a[63:3], 3'b0}, a[2:0]);

endfunction


// isaInstructionFromMemValue

// This function takes a value from the memory virtual device and turns
// it into an isa-specific instruction.
// TODO: Support turning multiple memory values into an instruction.

function ISA_INSTRUCTION isaInstructionFromMemValue(MEM_VALUE v, MEM_OFFSET offset);
    return offset[2] == 1? v[63:32]: v[31:0];
endfunction



function ISA_INSTRUCTION isaInstructionFromSpanningMemValues(MEM_VALUE v1, MEM_VALUE v2, MEM_OFFSET offset);

    case (offset)
        3'b101:  return {v2[7:0],  v1[63:40]};
        3'b110:  return {v2[15:0], v1[63:48]};
        3'b111:  return {v2[23:0], v1[63:56]};
        default: return isaInstructionFromMemValue(v1, offset);
    endcase

endfunction


// isaStoreRequiresReadModifyWrite

// This function returns True if the MEMOP_TYPE (which you have defined) requires
// a read-modify-write to implement. An example of this would be updating a single
// byte in an existing word. The result of this function will determine which of
// the following two functions are called.

function Bool isaStoreRequiresReadModifyWrite(ISA_MEMOP_TYPE memtype);

    return case (memtype)
               STORE_64: return False;
               default: return True;
           endcase;

endfunction

// isaStoreValueToMemValue

// This function takes an ISA-specific value and turns it into a value
// that the memory state understands. You are given the memtype and the
// original address, so all byte selection and extension can be performed here.

// This function is called ONLY if the above function returns False.

function MEM_VALUE isaStoreValueToMemValue(ISA_VALUE v, ISA_MEMOP_TYPE memtype);

    return zeroExtend(v);

endfunction

// isaStoreValueToMemValueRMW

// This function takes an ISA-specific value and an existing memory value. 
// The function should update the existing memory value appropriately for writeback.

// This function is called ONLY if the above function returns True.

function MEM_VALUE isaStoreValueToMemValueRMW(MEM_VALUE e, ISA_VALUE v, MEM_OFFSET offset, ISA_MEMOP_TYPE memtype);

    Bit#(8) v8 = v[7:0];
    Bit#(16) v16 = v[15:0];
    Bit#(32) v32 = v[31:0];

    Vector#(8, Bit#(8)) vec8 = newVector();
    vec8[0] = offset[2:0] == 0? v8: e[7:0];
    vec8[1] = offset[2:0] == 1? v8: e[15:8];
    vec8[2] = offset[2:0] == 2? v8: e[23:16];
    vec8[3] = offset[2:0] == 3? v8: e[31:24];
    vec8[4] = offset[2:0] == 4? v8: e[39:32];
    vec8[5] = offset[2:0] == 5? v8: e[47:40];
    vec8[6] = offset[2:0] == 6? v8: e[55:48];
    vec8[7] = offset[2:0] == 7? v8: e[63:56];

    Vector#(4, Bit#(16)) vec16 = newVector();
    vec16[0] = offset[2:1] == 0? v16: e[15:0];
    vec16[1] = offset[2:1] == 1? v16: e[31:16];
    vec16[2] = offset[2:1] == 2? v16: e[47:32];
    vec16[3] = offset[2:1] == 3? v16: e[63:48];

    Vector#(2, Bit#(32)) vec32 = newVector();
    vec32[0] = offset[2] == 0? v32: e[31:0];
    vec32[1] = offset[2] == 1? v32: e[63:32];

    return  case (memtype) matches
                STORE_8 : return {vec8[7], vec8[6], vec8[5], vec8[4], vec8[3], vec8[2], vec8[1], vec8[0]};
                STORE_16: return {vec16[3], vec16[2], vec16[1], vec16[0]};
                STORE_32: return {vec32[1], vec32[0]};
            endcase;

endfunction

function Tuple2#(MEM_VALUE, MEM_VALUE) isaStoreValueToSpanningMemValues(MEM_VALUE existing_value1, MEM_VALUE existing_value2, MEM_OFFSET offset, ISA_VALUE store_val, ISA_MEMOP_TYPE st_type);

    Bit#(128) combined_val = {existing_value2, existing_value1}; // Because of endian-ness, the second value goes first.

    Bit#(6) temp = zeroExtend(offset);
    Bit#(6) shift_amount = temp << 3; // shift_val = offset* 8
    MEM_VALUE shifted_v = (combined_val >> shift_amount)[63:0];

    MEM_VALUE updated_v = isaStoreValueToMemValueRMW(shifted_v, store_val, offset, st_type);
    
    Bit#(128) deshifted_v = zeroExtend(updated_v) << shift_amount;

    Bit#(128) mask = zeroExtend(~(64'b0)) << shift_amount;

    Bit#(128) masked_val = combined_val & mask;

    Bit#(128) final_v = masked_val | deshifted_v;
    
    return tuple2(final_v[63:0], final_v[127:64]);

endfunction

function ISA_VALUE isaLoadValueFromMemValue(MEM_VALUE val, ISA_MEMOP_TYPE memtype, MEM_OFFSET offset);

    Vector#(8, Bit#(8))   val8 = newVector();
    val8[0] = val[7:0];
    val8[1] = val[15:8];
    val8[2] = val[23:16];
    val8[3] = val[31:24];
    val8[4] = val[39:32];
    val8[5] = val[47:40];
    val8[6] = val[55:48];
    val8[7] = val[63:56];
    Vector#(4, Bit#(16)) val16 = newVector();
    val16[0] = val[15:0];
    val16[1] = val[31:16];
    val16[2] = val[47:32];
    val16[3] = val[63:48];
    Vector#(2, Bit#(32)) val32 = newVector();
    val32[0] = val[31:0];
    val32[1] = val[63:32];

    return  case (memtype) matches
                LOAD_ZERO_8 : return zeroExtend(val8[offset[2:0]]);
                LOAD_ZERO_16: return zeroExtend(val16[offset[2:1]]);
                LOAD_SIGN_32: return signExtend(val32[offset[2]]);
                default: return val;
            endcase;
endfunction

function ISA_VALUE isaLoadValueFromSpanningMemValues(MEM_VALUE v1, MEM_VALUE v2, MEM_OFFSET offset, ISA_MEMOP_TYPE memtype);
 
    Bit#(128) combined_val = {v2, v1}; // Because of endian-ness, the second value goes first.

    Bit#(6) temp = zeroExtend(offset);
    Bit#(6) shift_amount = temp << 3; // shift_val = offset * 8
    MEM_VALUE shifted_v = (combined_val >> shift_amount)[63:0];

    return isaLoadValueFromMemValue(shifted_v, memtype, 0);

endfunction

function Bool isaFetchSpansTwoMemValues(ISA_ADDRESS vaddr);

    return False;

endfunction
    
function Bool isaMemOpSpansTwoMemValues(ISA_ADDRESS vaddr, ISA_MEMOP_TYPE op_type);
    
    match {.addr, .offset} = isaAlignAddress(vaddr);
    
    case (op_type)

        LOAD_ZERO_8,
        STORE_8:         return False;    // Bytes never span.
                                             
        LOAD_ZERO_16,
        STORE_16:

            case (offset)
                3'b111:  return True;     // Only the last Halfword spans.
                default: return False;
            endcase

        LOAD_SIGN_32,
        STORE_32:
        
            case (offset)
                3'b101:  return True;     // Words 5-7 span.
                3'b110:  return True;
                3'b111:  return True;
                default: return False;
            endcase

        default:
        
            case (offset)
                3'b000:  return False;    // Only doubleword 0 does not span.
                default: return True;
            endcase

    endcase

endfunction

