
`include "funcp_memory.bsh"

// isaAddressToMemAddress

// This function takes an isa-specific address and turns it into
// an address the memory virtual device understands.

function MEM_ADDRESS isaAddressToMemAddress(ISA_ADDRESS a);

    return {a[63:3], 3'b0};

endfunction


// isaAddressFromMemoryAddress

// This function takes an address from the memory virtual device and turns
// it into an isa-specific address.

function ISA_ADDRESS isaAddressFromMemAddress(MEM_ADDRESS a);

    // TODO: write this correctly
    return zeroExtend(a); // If you need more than this write it here.

endfunction


// isaInstructionFromMemValue

// This function takes a value from the memory virtual device and turns
// it into an isa-specific instruction.
// TODO: Support turning multiple memory values into an instruction.

function ISA_INSTRUCTION isaInstructionFromMemValue(MEM_VALUE v, ISA_ADDRESS addr);
    return addr[2] == 1? v[63:32]: v[31:0];
endfunction


// isaInstructionToMemValue

// This function takes an isa-specific instruction and turns it into a memory value.
// TODO: Support turning an instruction into more than one memory value.

function MEM_VALUE isaInstructionToMemValue(ISA_INSTRUCTION i);

    // TODO: write this correctly
    return zeroExtend(i); // If you need more than this write it here.

endfunction


// isaValueFromMemValue

// This function takes a value from the memory device and turns it into an
// isa-specific value. You are given the memtype and the
// original address, so all byte selection and extension can be performed here.


function ISA_VALUE isaValueFromMemValue(MEM_VALUE val, ISA_MEMOP_TYPE memtype, ISA_ADDRESS addr);
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
                LOAD_ZERO_8 : return zeroExtend(val8[addr[2:0]]);
                LOAD_ZERO_16: return zeroExtend(val16[addr[2:1]]);
                LOAD_SIGN_32: return signExtend(val32[addr[2]]);
                default: return val;
            endcase;
    /*
    return  case (memtype)
               LOAD_ZERO_8: return zeroExtend(val[7:0]);
               LOAD_ZERO_16: return zeroExtend(val[15:0]);
               LOAD_SIGN_32: return signExtend(val[31:0]);
               default: return val;
           endcase;
    */

endfunction

// isaMemOpRequiresReadModifyWrite

// This function returns True if the MEMOP_TYPE (which you have defined) requires
// a read-modify-write to implement. An example of this would be updating a single
// byte in an existing word. The result of this function will determine which of
// the following two functions are called.

function Bool isaMemOpRequiresReadModifyWrite(ISA_MEMOP_TYPE memtype);

    return case (memtype)
               STORE_64: return False;
               default: return True;
           endcase;

endfunction

// isaValueToMemValue

// This function takes an ISA-specific value and turns it into a value
// that the memory state understands. You are given the memtype and the
// original address, so all byte selection and extension can be performed here.

// This function is called ONLY if the above function returns False.

function MEM_VALUE isaValueToMemValue(ISA_VALUE v, ISA_MEMOP_TYPE memtype, ISA_ADDRESS addr);

    // TODO: write this correctly
    return truncate(v);
    
endfunction

// isaValueToMemValueRMW

// This function takes an ISA-specific value and an existing memory value. 
// The function should update the existing memory value appropriately for writeback.

// This function is called ONLY if the above function returns True.

function MEM_VALUE isaValueToMemValueRMW(ISA_VALUE v, ISA_MEMOP_TYPE memtype, ISA_ADDRESS addr, MEM_VALUE e);
    Bit#(8) v8 = v[7:0];
    Bit#(16) v16 = v[15:0];
    Bit#(32) v32 = v[31:0];

    Vector#(8, Bit#(8)) vec8 = newVector();
    vec8[0] = addr[2:0] == 0? v8: e[7:0];
    vec8[1] = addr[2:0] == 1? v8: e[15:8];
    vec8[2] = addr[2:0] == 2? v8: e[23:16];
    vec8[3] = addr[2:0] == 3? v8: e[31:24];
    vec8[4] = addr[2:0] == 4? v8: e[39:32];
    vec8[5] = addr[2:0] == 5? v8: e[47:40];
    vec8[6] = addr[2:0] == 6? v8: e[55:48];
    vec8[7] = addr[2:0] == 7? v8: e[63:56];

    Vector#(4, Bit#(16)) vec16 = newVector();
    vec16[0] = addr[2:1] == 0? v16: e[15:0];
    vec16[1] = addr[2:1] == 1? v16: e[31:16];
    vec16[2] = addr[2:1] == 2? v16: e[47:32];
    vec16[3] = addr[2:1] == 3? v16: e[63:48];

    Vector#(2, Bit#(32)) vec32 = newVector();
    vec32[0] = addr[2] == 0? v32: e[31:0];
    vec32[1] = addr[2] == 1? v32: e[63:32];

    return  case (memtype) matches
                STORE_8 : return {vec8[7], vec8[6], vec8[5], vec8[4], vec8[3], vec8[2], vec8[1], vec8[0]};
                STORE_16: return {vec16[3], vec16[2], vec16[1], vec16[0]};
                STORE_32: return {vec32[1], vec32[0]};
            endcase;

endfunction

