
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

// This function is called ONLY if isaStoreRequiresReadModifyWrite() returns False.

function MEM_VALUE isaStoreValueToMemValue(ISA_VALUE v, ISA_MEMOP_TYPE memtype);

    return zeroExtend(v);

endfunction

// isaStoreValueToMemValueRMW

// This function takes an ISA-specific value and an existing memory value. 
// The function should update the existing memory value appropriately for writeback.

// This function is called ONLY if isaStoreRequiresReadModifyWrite() returns True.

function MEM_VALUE isaStoreValueToMemValueRMW(MEM_VALUE existing_value,
                                              ISA_VALUE store_val,
                                              MEM_OFFSET offset,
                                              ISA_MEMOP_TYPE memtype);

    match { .r, .dummy } = isaStoreValueToSpanningMemValues(existing_value, 0, store_val, offset, memtype);
    return r;

endfunction

//
// General store (potentially unaligned) of up to 64 bits into an existing 128 bits
//
function Tuple2#(MEM_VALUE, MEM_VALUE) isaStoreValueToSpanningMemValues(MEM_VALUE existing_value1,
                                                                        MEM_VALUE existing_value2,
                                                                        ISA_VALUE store_val,
                                                                        MEM_OFFSET offset,
                                                                        ISA_MEMOP_TYPE memtype);

    Bit#(128) current_val = {existing_value2, existing_value1}; // Because of endian-ness, the second value goes first.

    // Mask of the new store_val
    Bit#(64) mask = case (memtype) matches
                        STORE_8 : 'hff;
                        STORE_16: 'hffff;
                        STORE_32: 'hffffffff;
                        STORE_64: 'hffffffffffffffff;
                    endcase;

    // Amount to shift inserted store_val
    Bit#(6) shift_amount      = zeroExtend(offset) << 3; // shift_val = offset* 8

    // Shift the mask of new bits into position
    Bit#(128) shifted_mask    = zeroExtend(mask) << shift_amount;

    // Shift the new value into position
    Bit#(128) shifted_new_val = zeroExtend(store_val & mask) << shift_amount;

    Bit#(128) final_val = (current_val & ~shifted_mask) | shifted_new_val;
    
    return tuple2(final_val[63:0], final_val[127:64]);

endfunction


function ISA_VALUE isaLoadValueFromMemValue(MEM_VALUE val, MEM_OFFSET offset, ISA_MEMOP_TYPE memtype);

    return isaLoadValueFromSpanningMemValues(val, 0, offset, memtype);

endfunction

function ISA_VALUE isaLoadValueFromSpanningMemValues(MEM_VALUE v1, MEM_VALUE v2, MEM_OFFSET offset, ISA_MEMOP_TYPE memtype);
 
    Bit#(128) combined_val = {v2, v1}; // Because of endian-ness, the second value goes first.

    Bit#(6) shift_amount = zeroExtend(offset) << 3; // shift_val = offset * 8
    MEM_VALUE shifted_v = (combined_val >> shift_amount)[63:0];

    return  case (memtype) matches
                LOAD_ZERO_8 : return zeroExtend(shifted_v[7:0]);
                LOAD_ZERO_16: return zeroExtend(shifted_v[15:0]);
                LOAD_SIGN_32: return signExtend(shifted_v[31:0]);
                default: return shifted_v;
            endcase;

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

