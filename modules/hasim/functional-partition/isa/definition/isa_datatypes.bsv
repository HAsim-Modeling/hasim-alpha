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
    LOAD_CVT_T_32,      // lds (single precision floating point load)
    LOAD_64,
    LOAD_UNALIGNED_64
}
ISA_MEMOP_TYPE
    deriving (Eq, Bits, Bounded);

// ISA_REG_INDEX

// An ISA-specific register index. This could be simple (Bit#(5)) or a complex tagged union
// encapsulating every addressable register in the system. This should pack into an efficient
// number of bits, so you may want to define a custom instance of bits.

typedef 67 ISA_NUM_REGS;

typedef union tagged {
    Bit#(5) ArchReg;
    Bit#(5) FPReg;
    void ControlReg;
    void LockReg;
    void LockAddrReg;
} ISA_REG_INDEX deriving (Eq);

instance Bits#(ISA_REG_INDEX, 7);
    function Bit#(7) pack(ISA_REG_INDEX x);
        return case (x) matches
                   tagged ArchReg .v : return {2'b00, v};
                   tagged FPReg .v   : return {2'b01, v};
                   tagged ControlReg : return {7'b1000000};
                   tagged LockReg    : return {7'b1000001};
                   tagged LockAddrReg: return {7'b1000010};
               endcase;
    endfunction

    function ISA_REG_INDEX unpack(Bit#(7) x);
        if (x[6:5] == 0)
            return tagged ArchReg x[4:0];
        else if (x[6:5] == 1)
            return tagged FPReg x[4:0];
        else if (x[1:0] == 0)
            return tagged ControlReg;
        else if (x[1:0] == 1)
            return tagged LockReg;
        else
            return tagged LockAddrReg;
    endfunction
endinstance

instance Bounded#(ISA_REG_INDEX);

    function ISA_REG_INDEX minBound() = tagged ArchReg 0;
    function ISA_REG_INDEX maxBound() = tagged LockAddrReg;

endinstance

instance Literal#(ISA_REG_INDEX);

    function ISA_REG_INDEX fromInteger(Integer x);
    
        if (x < 32)
            return tagged ArchReg fromInteger(x);
        else if (x < 64)
            return tagged FPReg fromInteger(x);
        else if (x == 64)
            return tagged ControlReg;
        else if (x == 65)
            return tagged LockReg;
        else if (x == 66)
            return tagged LockAddrReg;
        else 
            return error("ISA_REG_INDEX: Literal out of bounds: " + integerToString(x));
        
    endfunction
    
    function Bool inLiteralRange(ISA_REG_INDEX x, Integer y);
    
        return (y < valueof(ISA_NUM_REGS));
    
    endfunction

endinstance

instance Arith#(ISA_REG_INDEX);

    function ISA_REG_INDEX \+ (ISA_REG_INDEX a, ISA_REG_INDEX b);
    
        return unpack(pack(a) + pack(b));
    
    endfunction

    function ISA_REG_INDEX \- (ISA_REG_INDEX a, ISA_REG_INDEX b);
    
        return unpack(pack(a) - pack(b));
    
    endfunction
    
    function ISA_REG_INDEX \* (ISA_REG_INDEX a, ISA_REG_INDEX b);
    
        return unpack(pack(a) * pack(b));
    
    endfunction

    function ISA_REG_INDEX \/ (ISA_REG_INDEX a, ISA_REG_INDEX b);
    
        return unpack(pack(a) / pack(b));
    
    endfunction

    function ISA_REG_INDEX \% (ISA_REG_INDEX a, ISA_REG_INDEX b);
    
        return unpack(pack(a) % pack(b));
    
    endfunction
    
    function ISA_REG_INDEX negate(ISA_REG_INDEX a);

        return unpack(negate(pack(a)));
    
    endfunction

endinstance
