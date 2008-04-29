// isa_datatypes

// This file contains datatype definitions that you should fill in to create
// an ISA that the functional partition supports.


// ISA_ADDRESS

// An ISA-specific address.

typedef Bit#(64) ISA_ADDRESS;


// ISA_VALUE

// The value stored in registers.
// TODO: Support: multiple value lengths for registers.

typedef Bit#(64) ISA_VALUE;

// ISA_INSTRUCTION

// An ISA-specific instruction.

typedef Bit#(32) ISA_INSTRUCTION;


// ISA_MAX_SRCS

// The maximum number of source registers an instruction can read.

typedef 3 ISA_MAX_SRCS;


// ISA_MAX_DSTS

// The maximum number of destination registers an instruction can write.

typedef 3 ISA_MAX_DSTS;


// ISA_MEMOP_TYPE

// An ISA-specific memory operation. This would include things like distinguishing 
// between bytes, halfwords, etc. Later you'll convert standard memory-op types into
// these types.

typedef enum
{
    STORE_8,
    STORE_16,
    STORE_32,
    STORE_64,
    STORE_UNALIGNED_64,

    LOAD_ZERO_8,
    LOAD_ZERO_16,
    LOAD_SIGN_32,
    LOAD_64,
    LOAD_UNALIGNED_64
}
  ISA_MEMOP_TYPE
     deriving (Eq, Bits);

// ISA_REG_INDEX

// An ISA-specific register index. This could be simple (Bit#(5)) or a complex tagged union
// encapsulating every addressable register in the system. This should pack into an efficient
// number of bits, so you may want to define a custom instance of bits.

typedef union tagged {
    Bit#(5) ArchReg;
    void ControlReg;
    void LockReg;
    void LockAddrReg;
} ISA_REG_INDEX deriving (Eq);

instance Bits#(ISA_REG_INDEX, 6);
    function Bit#(6) pack(ISA_REG_INDEX x);
        return case (x) matches
                   tagged ArchReg .v : return {1'b1, v};
                   tagged ControlReg : return {3'b000, 3'b0};
                   tagged LockReg    : return {3'b001, 3'b0};
                   tagged LockAddrReg: return {3'b010, 3'b0};
               endcase;
    endfunction

    function ISA_REG_INDEX unpack(Bit#(6) x);
        if(x[5] == 1)
            return tagged ArchReg x[4:0];
        else if(x[5:3] == 0)
            return tagged ControlReg;
        else if(x[5:3] == 1)
            return tagged LockReg;
        else
            return tagged LockAddrReg;
    endfunction
endinstance
