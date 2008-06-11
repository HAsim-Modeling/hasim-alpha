//
// INTEL CONFIDENTIAL
// Copyright (c) 2008 Intel Corp.  Recipient is granted a non-sublicensable 
// copyright license under Intel copyrights to copy and distribute this code 
// internally only. This code is provided "AS IS" with no support and with no 
// warranties of any kind, including warranties of MERCHANTABILITY,
// FITNESS FOR ANY PARTICULAR PURPOSE or INTELLECTUAL PROPERTY INFRINGEMENT. 
// By making any use of this code, Recipient agrees that no other licenses 
// to any Intel patents, trade secrets, copyrights or other intellectual 
// property rights are granted herein, and no other licenses shall arise by 
// estoppel, implication or by operation of law. Recipient accepts all risks 
// of use.
//
 
//
// @file isa_datatypes.h
// @brief Equivalent to Bluespec ISA datatypes
//
// @author Michael Adler
//

#include "asim/syntax.h"
#include "asim/mesg.h"

typedef UINT64 ISA_ADDRESS;
typedef UINT64 ISA_VALUE;
typedef UINT32 ISA_INSTRUCTION;

typedef class ISA_REG_INDEX_CLASS *ISA_REG_INDEX;

class ISA_REG_INDEX_CLASS
{
  private:
    UINT32 regIdx;

  public:
    ISA_REG_INDEX_CLASS(UINT32 idx = 0) { SetMasked(idx); }
    ~ISA_REG_INDEX_CLASS() {}

    // Operators for easy conversion to integers
    operator UINT32() { return regIdx; };
    ISA_REG_INDEX_CLASS& operator=(UINT32 idx) { SetMasked(idx); }

    // Assignments by type
    void SetArchReg(UINT32 r) { regIdx = (r & 0x1f) | 0x20; }
    void SetControlReg() { regIdx = 0; }
    void SetLockreg() { regIdx = 0x8; }
    void SetLockAddrReg() { regIdx = 0x10; }

    // Queries
    bool IsArchReg() const { return ((regIdx & 0x20) != 0); }
    UINT32 ArchRegNum() const
    {
        ASSERTX(IsArchReg());
        return regIdx & 0x1f;
    };

    bool IsControlReg() const { return (regIdx == 0); }
    bool IsLockReg() const { return ((regIdx & 0x38) == 0x8); }
    bool IsLockAddrReg() const { return ((regIdx & 0x38) == 0x10); }

    bool IsIllegalReg() const
    {
        return ! (IsArchReg() || IsControlReg() || IsLockReg() || IsLockAddrReg());
    }

  private:
    void SetMasked(UINT32 idx)
    {
        regIdx = idx & 0x3f;
    }
};