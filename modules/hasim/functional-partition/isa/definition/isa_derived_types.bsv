//
// Copyright (c) 2014, Intel Corporation
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
//
// Redistributions of source code must retain the above copyright notice, this
// list of conditions and the following disclaimer.
//
// Redistributions in binary form must reproduce the above copyright notice,
// this list of conditions and the following disclaimer in the documentation
// and/or other materials provided with the distribution.
//
// Neither the name of the Intel Corporation nor the names of its contributors
// may be used to endorse or promote products derived from this software
// without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
// ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
// LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
// SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
// INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
// CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
// ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.
//

import Vector::*;

`include "asim/provides/funcp_base_types.bsh"

// isa_derived_types

// This file contains type definitions that are derived from types defined elsewhere.
// You probably don't need to change the contents of this file unless you know what
// you are doing.

// Note: Changes to this file generally imply changing the functional and/or timing
// partitions in order to reflect the new changes.


// FUNCP_PHYSICAL_REG_INDEX

// The number of functional partition physical registers available.
// This isn't really an ISA-specific thing, but still lives here for now since
// both the timing partition and functional partition need to know about it.

typedef Bit#(`FUNCP_PHYSICAL_REG_INDEX_BITS) FUNCP_PHYSICAL_REG_INDEX;


// FUNCP_PHYSICAL_REGS

// The total number of physical regs available.

typedef TExp#(`FUNCP_PHYSICAL_REG_INDEX_BITS) FUNCP_NUM_PHYSICAL_REGS;


// ISA_REG_MAPPING

// A mapping of architectural reg -> physical reg.

typedef Tuple2#(ISA_REG_INDEX, FUNCP_PHYSICAL_REG_INDEX) ISA_REG_MAPPING;


// ISA_SOURCE_VALUES

// A Vector of source values. Passed to the isa-datapath for execution.

typedef Vector#(ISA_MAX_SRCS, ISA_VALUE) ISA_SOURCE_VALUES;


// ISA_RESULT_VALUES

// A Vector of (possible) result values. Returned from the isa-datapath for writeback.

typedef Vector#(ISA_MAX_DSTS, Maybe#(ISA_VALUE)) ISA_RESULT_VALUES;


// ISA_INST_SRCS

// A Vector of source registers that the functional partiton reads for a given instruction.

typedef Vector#(ISA_MAX_SRCS, Maybe#(FUNCP_PHYSICAL_REG_INDEX)) ISA_INST_SRCS;


// ISA_INST_DSTS

// A Vector of destination registers that an instruction writes back after execution.

typedef Vector#(ISA_MAX_DSTS, Maybe#(FUNCP_PHYSICAL_REG_INDEX)) ISA_INST_DSTS;


// ISA_INST_DSTS_MASK

// A mask of destination slots

typedef Vector#(ISA_MAX_DSTS, Bool) ISA_INST_DSTS_MASK;


// ISA_SRC_INDEX

// A reference to an instruction source. (IE 3 srcs => bit 2)

typedef Bit#(TLog#(ISA_MAX_SRCS)) ISA_SRC_INDEX;


// ISA_SRC_COUNTER

// A counter of sources.  (One bit larger than ISA_SRC_INDEX).

typedef Bit#(TLog#(TAdd#(1, ISA_MAX_SRCS))) ISA_SRC_COUNTER;


// ISA_DST_INDEX

// A reference to an instruction dest. (IE 5 dsts => bit 3)

typedef Bit#(TLog#(ISA_MAX_DSTS)) ISA_DST_INDEX;


// ISA_DST_COUNTER

// A counter of destinations.  (One bit larger than ISA_DST_INDEX).

typedef Bit#(TLog#(TAdd#(1, ISA_MAX_DSTS))) ISA_DST_COUNTER;


// ISA_SRC_MAPPING

// A Vector which says for each instruction source architectural register, which
// physical register it maps to.

typedef Vector#(ISA_MAX_SRCS, Maybe#(ISA_REG_MAPPING)) ISA_SRC_MAPPING;


// ISA_DST_MAPPING

// A Vector which says for each instruction destination architecural register, which
// physical register it maps to.

typedef Vector#(ISA_MAX_DSTS, Maybe#(ISA_REG_MAPPING)) ISA_DST_MAPPING;


// ISA_DEPENDENCY_INFO

// All the information about an instruction's source and dest mappings.
// Returned by getDependencies().

typedef Tuple2#(ISA_SRC_MAPPING, ISA_DST_MAPPING) ISA_DEPENDENCY_INFO;

