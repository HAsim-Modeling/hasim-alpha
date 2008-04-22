// isa_datatypes

// This file contains datatype definitions that you should fill in to create
// an ISA that the functional partition supports.


// ISA_ADDRESS

// An ISA-specific address.

typedef Bit#(32) ISA_ADDRESS;


// ISA_VALUE

// The value stored in registers.
// TODO: Support: multiple value lengths for registers.

typedef Bit#(32) ISA_VALUE;

typedef 8 ISA_MASK_NUM;
typedef 4 ISA_MASK_SIZE;


// ISA_INSTRUCTION

// An ISA-specific instruction.

typedef Bit#(32) ISA_INSTRUCTION;


// ISA_MAX_SRCS

// The maximum number of source registers an instruction can read.

typedef 3 ISA_MAX_SRCS;


// ISA_MAX_DSTS

// The maximum number of destination registers an instruction can write.

typedef 1 ISA_MAX_DSTS;


// ISA_MEMOP_TYPE

// An ISA-specific memory operation. This would include things like distinguishing 
// between bytes, halfwords, etc. Later you'll convert standard memory-op types into
// these types.

typedef enum
{
    MEM_ZERO_8,
    MEM_ZERO_16,
    MEM_SIGN_32,
    MEM_64
}
  ISA_MEMOP_TYPE
     deriving (Eq, Bits);

// ISA_REG_INDEX

// An ISA-specific register index. This could be simple (Bit#(5)) or a complex tagged union
// encapsulating every addressable register in the system. This should pack into an efficient
// number of bits, so you may want to define a custom instance of bits.

typedef union tagged {
    Bit#(5) ArchReg;
} ISA_REG_INDEX deriving (Bits, Eq);
//typedef Bit#(5) ISA_REG_INDEX;

