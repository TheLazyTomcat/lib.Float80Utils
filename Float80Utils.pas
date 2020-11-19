{$message 'write some sensible explanations here'}
{
  Functions and constants provided in this sections are meant for explicit
  conversion between double precission (8-byte/64bit) floating point numbers
  and double-extended precission (10-byte/80bit) floating point numbers.

  It is here because in some situations (eg. compilation for x86-64 CPU), there
  is no 80bit float supported by the compiler, and therefore no way of easy
  conversion. Yet, there may be a need for it.

  Main routines are ConvertFloat64ToFloat80 and ConvertFloat80ToFloat64, others
  are here only as an infrastructure for them.
}
unit Float80Utils; 
{
  Float80_PurePascal

  If you want to compile this unit without ASM, don't want to or cannot define
  PurePascal for the entire project and at the same time you don't want to or
  cannot make changes to this unit, define this symbol for the entire project
  and this unit will be compiled in PurePascal mode.
}
{$DEFINE Float80_PurePascal}
{$IFDEF Float80_PurePascal}
  {$DEFINE PurePascal}
{$ENDIF}

{$IF defined(CPUX86_64) or defined(CPUX64)}
  {$DEFINE x64}
{$ELSEIF defined(CPU386)}
  {$DEFINE x86}
{$ELSE}
  {$DEFINE PurePascal}
{$IFEND}

{$IFDEF ENDIAN_BIG}
  // it might work in BE systems, but I cannot test it
  {$MESSAGE FATAL 'Big-endian system not supported'}
{$ENDIF}

{$IFDEF FPC}
  {$MODE ObjFPC}
  {$INLINE ON}
  {$DEFINE CanInline}
  {$IFNDEF PurePascal}
    {$ASMMODE Intel}
  {$ENDIF}
  {$DEFINE FPC_DisableWarns}
  {$MACRO ON}
{$ELSE}
  {$IF CompilerVersion >= 17 then}  // Delphi 2005+
    {$DEFINE CanInline}
  {$ELSE}
    {$UNDEF CanInline}
  {$IFEND}
{$ENDIF}
{$H+}

interface

uses
  SysUtils,
  AuxTypes;

{===============================================================================
    Library-specific exceptions - declaration
===============================================================================}

type
  EF80Exception = class(Exception);

  EF80InvalidFlag  = class(EF80Exception);

{-------------------------------------------------------------------------------
    Library-specific exceptions - FPU exceptions
-------------------------------------------------------------------------------}
type
  EF80FPUException = class(EF80Exception)
  protected
    Function DefaultMessage: String; virtual; abstract;
  public
    constructor CreateDefMsg;
  end;

  // FPU stack errors
  EF80StackFault = class(EF80FPUException);

{-------------------------------------------------------------------------------
    Library-specific exceptions - individual FPU exception classes
-------------------------------------------------------------------------------}
type
  EF80StackOverflow = class(EF80StackFault)
  protected
    Function DefaultMessage: String; override;
  end;

  EF80StackUnderflow = class(EF80StackFault)
  protected
    Function DefaultMessage: String; override;
  end;

  EF80InvalidOp = class(EF80FPUException) // invalid operation/operand
  protected
    Function DefaultMessage: String; override;
  end;

  EF80Denormal = class(EF80FPUException)
  protected
    Function DefaultMessage: String; override;
  end;

  EF80DivByZero = class(EF80FPUException)
  protected
    Function DefaultMessage: String; override;
  end;

  EF80Overflow = class(EF80FPUException)
  protected
    Function DefaultMessage: String; override;
  end;

  EF80Underflow = class(EF80FPUException)
  protected
    Function DefaultMessage: String; override;
  end;

  EF80Precision = class(EF80FPUException)
  protected
    Function DefaultMessage: String; override;
  end;

{===============================================================================
    Auxiliary routines - declaration
===============================================================================}
{-------------------------------------------------------------------------------
    Auxiliary routines - x87 status word access
-------------------------------------------------------------------------------}
{
  Note that x87 status register is read only - it can only be changed by
  clearing.
}

const
  // status word masks
  X87SW_EX_InvalidOP = UInt16($0001);
  X87SW_EX_Denormal  = UInt16($0002);
  X87SW_EX_DivByZero = UInt16($0004);
  X87SW_EX_Overflow  = UInt16($0008);
  X87SW_EX_Underflow = UInt16($0010);
  X87SW_EX_Precision = UInt16($0020);

  X87SW_StackFault       = UInt16($0040);
  X87SW_ExceptionSummary = UInt16($0080);
  X87SW_FPUBusy          = UInt16($8000);

  X87SW_ConditionCode_C0 = UInt16($0100);
  X87SW_ConditionCode_C1 = UInt16($0200);
  X87SW_ConditionCode_C2 = UInt16($0400);
  X87SW_ConditionCode_C3 = UInt16($4000);

  X87SW_TopOfStack = UInt16($3800); // bits 11..13

  X87SW_SHIFT_TopOfStack = 11;

{
  EmulatedX87StatusWord

  Returns true when a real x87 status register is used, false when operating
  on an emulated local implementation of status word.
}
Function EmulatedX87StatusWord: Boolean;{$IFDEF CanInline} inline;{$ENDIF}

{
  GetX87StatusWord

  Returns current value of status word.
}
Function GetX87StatusWord: UInt16;{$IFNDEF PurePascal} register; assembler;{$ENDIF}

{-------------------------------------------------------------------------------
    Auxiliary routines - x87 control word access
-------------------------------------------------------------------------------}
const
  // control word masks
  X87CW_EMASK_InvalidOP = UInt16($0001);
  X87CW_EMASK_Denormal  = UInt16($0002);
  X87CW_EMASK_DivByZero = UInt16($0004);
  X87CW_EMASK_Overflow  = UInt16($0008);
  X87CW_EMASK_Underflow = UInt16($0010);
  X87CW_EMASK_Precision = UInt16($0020);

  X87CW_InfinityControl = UInt16($1000);

  X87CW_Precision = UInt16($0300);  // bits 8..9
  X87CW_Rounding  = UInt16($0C00);  // bits 10..11

  X87CW_SHIFT_Precision = 8;
  X87CW_SHIFT_Rounding  = 10;

{
  EmulatedX87ControlWord

  Returns true when a real x87 control register is used, false when operating
  on an emulated local implementation of control word.
}
Function EmulatedX87ControlWord: Boolean;{$IFDEF CanInline} inline;{$ENDIF}

{
  GetX87ControlWord

  Returns current value of control word.
}
Function GetX87ControlWord: UInt16;{$IFNDEF PurePascal} register; assembler;{$ENDIF}
{
  SetX87ControlWord

  Set control word to a passed value.
}
procedure SetX87ControlWord(NewValue: UInt16);{$IFNDEF PurePascal} register; assembler;{$ENDIF}

{
  Sets x87 control word to $1372 - denormal, underflow and precision exceptions
  are masked (others are unmasked), precision is set to extended, rounding is
  set to nearest and infinity control bit is 1.

  Call this routine only when the control word is NOT emulated (ie. a real CPU
  register is used) and the program is compiled so that x87 FPU is not used as
  primary mean of floating point arithmetics and/or is not automatically
  initialized (typically when compiled for Windows 64bit).
}
procedure InitX87ControlWord;{$IFDEF CanInline} inline;{$ENDIF}

{-------------------------------------------------------------------------------
    Auxiliary routines - abstracted SW and CW access
-------------------------------------------------------------------------------}
type
  TX87PrecisionMode = (pmSingle,pmReserved,pmDouble,pmExtended);

  TX87RoundingMode = (rmNearest,rmDown,rmUp,rmTruncate);

  TX87StatusFlag = (sfStackFault,sfExceptionSummary,sfFPUBusy,sfConditionCodeC0,
                    sfConditionCodeC1,sfConditionCodeC2,sfConditionCodeC3);

  TX87StatusFlags = set of TX87StatusFlag;

  TX87ControlFlag = (cfInfinityControl);

  TX87ControlFlags = set of TX87ControlFlag;

Function GetX87PrecisionMode: TX87PrecisionMode;
Function SetX87PrecisionMode(NewValue: TX87PrecisionMode): TX87PrecisionMode;

Function GetX87RoundingMode: TX87RoundingMode;
Function SetX87RoundingMode(NewValue: TX87RoundingMode): TX87RoundingMode;

Function GetX87TopOfStack: Integer;
Function SetX87TopOfStack(NewValue: Integer): Integer;

Function GetX87StatusFlag(Flag: TX87StatusFlag): Boolean;
Function SetX87StatusFlag(Flag: TX87StatusFlag; NewValue: Boolean): Boolean;

Function GetX87StatusFlags: TX87StatusFlags;
Function SetX87StatusFlags(NewValue: TX87StatusFlags): TX87StatusFlags;

Function GetX87ControlFlag(Flag: TX87ControlFlag): Boolean;
Function SetX87ControlFlag(Flag: TX87ControlFlag; NewValue: Boolean): Boolean;

Function GetX87ControlFlags: TX87ControlFlags;
Function SetX87ControlFlags(NewValue: TX87ControlFlags): TX87ControlFlags;

{-------------------------------------------------------------------------------
    Auxiliary routines - abstracted x87 exception flags access
-------------------------------------------------------------------------------}
type
  TX87Exception = (excInvalidOp,excDenormal,excDivByZero,excOverflow,
                   excUnderflow,excPrecision);

  TX87Exceptions = set of TX87Exception;

Function GetX87ExceptionMask(Exception: TX87Exception): Boolean;
Function SetX87ExceptionMask(Exception: TX87Exception; NewValue: Boolean): Boolean;

Function GetX87ExceptionMasks: TX87Exceptions;
Function SetX87ExceptionMasks(NewValue: TX87Exceptions): TX87Exceptions;

Function GetX87ExceptionState(Exception: TX87Exception): Boolean;
Function SetX87ExceptionState(Exception: TX87Exception; NewValue: Boolean): Boolean;

Function GetX87ExceptionStates: TX87Exceptions;
Function SetX87ExceptionStates(NewValue: TX87Exceptions): TX87Exceptions;

procedure ClearX87Exceptions;{$IFNDEF PurePascal} register; assembler;{$ENDIF}
procedure RaiseX87Exceptions;

{===============================================================================
--------------------------------------------------------------------------------
                         Float80 <-> Float64 conversions
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    Conversion routines - declaration
===============================================================================}

procedure Float64ToFloat80(Float64Ptr,Float80Ptr: Pointer); overload;{$IFNDEF PurePascal} register; assembler;{$ENDIF}

Function Float64ToFloat80(Value: Float64): AuxTypes.Float80; overload;

//------------------------------------------------------------------------------

procedure Float80ToFloat64(Float80Ptr,Float64Ptr: Pointer); overload;{$IFNDEF PurePascal} register; assembler;{$ENDIF}

Function Float80ToFloat64(Value: AuxTypes.Float80): Float64; overload;

{===============================================================================
--------------------------------------------------------------------------------
                         Float80 utilities
--------------------------------------------------------------------------------
===============================================================================}

implementation

{$IFDEF FPC_DisableWarns}
  {$DEFINE FPCDWM}
  {$DEFINE W4055:={$WARN 4055 OFF}}   // Conversion between ordinals and pointers is not portable
{$ENDIF}

{===============================================================================
    Internal constants and types
===============================================================================}
const
  F64_MASK_SIGN = UInt64($8000000000000000);  // sign bit
  F64_MASK_EXP  = UInt64($7FF0000000000000);  // exponent
  F64_MASK_FRAC = UInt64($000FFFFFFFFFFFFF);  // fraction/mantissa
  F64_MASK_FHB  = UInt64($0008000000000000);  // highest bit of the mantissa
{$IFNDEF FPC} // to remove hints that cannot be suppressed :/
  F64_MASK_NSGN = UInt64($7FFFFFFFFFFFFFFF);  // non-sign bits
  F64_MASK_INTB = UInt64($0010000000000000);  // otherwise implicit integer bit of the mantissa
{$ENDIF}

  F80_MASK16_EXP  = UInt16($7FFF);
  F80_MASK64_FRAC = UInt64($7FFFFFFFFFFFFFFF);
  F80_MASK64_FHB  = UInt64($4000000000000000);
  F80_MASK64_INTB = UInt64($8000000000000000);
{$IFNDEF FPC}
  F80_MASK16_SIGN = UInt16($8000);
  F80_MASK16_NSGN = UInt16($7FFF);
{$ENDIF}

{===============================================================================
    Library-specific exceptions - implementation
===============================================================================}
{-------------------------------------------------------------------------------
    Library-specific exceptions - FPU exceptions
-------------------------------------------------------------------------------}

constructor EF80FPUException.CreateDefMsg;
begin
Create(DefaultMessage);
end;

{-------------------------------------------------------------------------------
    Library-specific exceptions - individual FPU exception classes
-------------------------------------------------------------------------------}

Function EF80StackOverflow.DefaultMessage: String;
begin
Result := 'Floting point unit stack overflow';
end;

//------------------------------------------------------------------------------

Function EF80StackUnderflow.DefaultMessage: String;
begin
Result := 'Floting point unit stack underflow';
end;

//------------------------------------------------------------------------------

Function EF80InvalidOp.DefaultMessage: String;
begin
Result := 'Invalid floating point operand';
end;

//------------------------------------------------------------------------------

Function EF80Denormal.DefaultMessage: String;
begin
Result := 'Denormal floating point operand';
end;

//------------------------------------------------------------------------------

Function EF80DivByZero.DefaultMessage: String;
begin
Result := 'Floating point division by zero';
end;

//------------------------------------------------------------------------------

Function EF80Overflow.DefaultMessage: String;
begin
Result := 'Floating point arithmetic overflow';
end;

//------------------------------------------------------------------------------

Function EF80Underflow.DefaultMessage: String;
begin
Result := 'Floating point arithmetic underflow';
end;

//------------------------------------------------------------------------------

Function EF80Precision.DefaultMessage: String;
begin
Result := 'Inexact floating point result';
end;


{===============================================================================
    Auxiliary routines - implementation
===============================================================================}
{-------------------------------------------------------------------------------
    Auxiliary routines - x87 status word access
-------------------------------------------------------------------------------}

{$IFDEF PurePascal}
var
  Pas_X87SW: UInt16 = 0;
{$ENDIF}

//------------------------------------------------------------------------------

Function EmulatedX87StatusWord: Boolean;
begin
{$IFDEF PurePascal}
Result := True;
{$ELSE}
Result := False;
{$ENDIF}
end;

//------------------------------------------------------------------------------

Function GetX87StatusWord: UInt16;
{$IFNDEF PurePascal}
asm
    FSTSW   AX
end;
{$ELSE}
begin
Result := Pas_X87SW;
end;
{$ENDIF}

{-------------------------------------------------------------------------------
    Auxiliary routines - x87 control word access
-------------------------------------------------------------------------------}

{$IFDEF PurePascal}
var
{
  denormal, underflow and precision exceptions are masked, precision set to
  extended, rounding set to nearest and infinity control bit set
}
  Pas_X87CW: UInt16 = $1372;
{$ENDIF}

//------------------------------------------------------------------------------

Function EmulatedX87ControlWord: Boolean;
begin
{$IFDEF PurePascal}
Result := True;
{$ELSE}
Result := False;
{$ENDIF}
end;

//------------------------------------------------------------------------------

Function GetX87ControlWord: UInt16;
{$IFNDEF PurePascal}
var
  Temp: UInt16;
asm
    FSTCW   word ptr [Temp]
    MOV     AX, word ptr [Temp]
end;
{$ELSE}
begin
Result := Pas_X87CW;
end;
{$ENDIF}

//------------------------------------------------------------------------------

procedure SetX87ControlWord(NewValue: UInt16);
{$IFNDEF PurePascal}
var
  Temp: UInt16;
asm
    MOV     word ptr [Temp], NewValue
    FLDCW   word ptr [Temp]
end;
{$ELSE}
begin
Pas_X87CW := NewValue;
end;
{$ENDIF}

//------------------------------------------------------------------------------

procedure InitX87ControlWord;
begin
SetX87ControlWord($1372);
end;

{-------------------------------------------------------------------------------
    Auxiliary routines - abstracted SW and CW access
-------------------------------------------------------------------------------}

Function GetX87PrecisionMode: TX87PrecisionMode;
begin
case (GetX87ControlWord and X87CW_Precision) shr X87CW_SHIFT_Precision of
  0:  Result := pmSingle;
  2:  Result := pmDouble;
  3:  Result := pmExtended;
else
  Result := pmReserved;
end;
end;

//------------------------------------------------------------------------------

Function SetX87PrecisionMode(NewValue: TX87PrecisionMode): TX87PrecisionMode;
var
  Num:  UInt16;
begin
Result := GetX87PrecisionMode;
case NewValue of
  pmSingle:   Num := 0;
  pmDouble:   Num := 2;
  pmExtended: Num := 3;
else
  Num := 1;
end;
SetX87ControlWord((GetX87ControlWord and not X87CW_Precision) or UInt16(Num shl X87CW_SHIFT_Precision));
end;

//------------------------------------------------------------------------------

Function GetX87RoundingMode: TX87RoundingMode;
begin
case (GetX87ControlWord and X87CW_Rounding) shr X87CW_SHIFT_Rounding of
  1:  Result := rmDown;
  2:  Result := rmUp;
  3:  Result := rmTruncate;
else
  Result := rmNearest;
end;
end;

//------------------------------------------------------------------------------

Function SetX87RoundingMode(NewValue: TX87RoundingMode): TX87RoundingMode;
var
  Num:  UInt16;
begin
Result := GetX87RoundingMode;
case NewValue of
  rmDown:     Num := 1;
  rmUp:       Num := 2;
  rmTruncate: Num := 3;
else
  Num := 0;
end;
SetX87ControlWord((GetX87ControlWord and not X87CW_Rounding) or UInt16(Num shl X87CW_SHIFT_Rounding));
end;

//------------------------------------------------------------------------------

Function GetX87TopOfStack: Integer;
begin
Result := (GetX87StatusWord and X87SW_TopOfStack) shr X87SW_SHIFT_TopOfStack;
end;

//------------------------------------------------------------------------------

Function SetX87TopOfStack(NewValue: Integer): Integer;
begin
Result := GetX87TopOfStack;
{$IFDEF PurePascal}
Pas_X87SW := (Pas_X87SW and not X87SW_TopOfStack) or ((NewValue shl X87SW_SHIFT_TopOfStack) and X87SW_TopOfStack);
{$ENDIF}
end;

//------------------------------------------------------------------------------

Function GetX87StatusFlag(Flag: TX87StatusFlag): Boolean;
begin
case Flag of
  sfStackFault:       Result := (GetX87StatusWord and X87SW_StackFault) <> 0;
  sfExceptionSummary: Result := (GetX87StatusWord and X87SW_ExceptionSummary) <> 0;
  sfFPUBusy:          Result := (GetX87StatusWord and X87SW_FPUBusy) <> 0;
  sfConditionCodeC0:  Result := (GetX87StatusWord and X87SW_ConditionCode_C0) <> 0;
  sfConditionCodeC1:  Result := (GetX87StatusWord and X87SW_ConditionCode_C1) <> 0;
  sfConditionCodeC2:  Result := (GetX87StatusWord and X87SW_ConditionCode_C2) <> 0;
  sfConditionCodeC3:  Result := (GetX87StatusWord and X87SW_ConditionCode_C3) <> 0;
else
  raise EF80InvalidFlag.CreateFmt('GetX87StatusFlag: Invalid flag (%d).',[Ord(Flag)]);
end;
end;

//------------------------------------------------------------------------------

Function SetX87StatusFlag(Flag: TX87StatusFlag; NewValue: Boolean): Boolean;
{$IFDEF PurePascal}

  procedure SetFlag(FlagMask: UInt16);
  begin
    If NewValue then
      Pas_X87SW:= Pas_X87SW or FlagMask
    else
      Pas_X87SW:= Pas_X87SW and not FlagMask;
  end;

begin
Result := GetX87StatusFlag(Flag);
case Flag of
  sfStackFault:       SetFlag(X87SW_StackFault);
  sfExceptionSummary: SetFlag(X87SW_ExceptionSummary);
  sfFPUBusy:          SetFlag(X87SW_FPUBusy);
  sfConditionCodeC0:  SetFlag(X87SW_ConditionCode_C0);
  sfConditionCodeC1:  SetFlag(X87SW_ConditionCode_C1);
  sfConditionCodeC2:  SetFlag(X87SW_ConditionCode_C2);
  sfConditionCodeC3:  SetFlag(X87SW_ConditionCode_C3);
else
  raise EF80InvalidFlag.CreateFmt('SetX87StatusFlag: Invalid flag (%d).',[Ord(Flag)]);
end;
end;
{$ELSE}
begin
Result := GetX87StatusFlag(Flag);
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function GetX87StatusFlags: TX87StatusFlags;
var
  SW: UInt16;
  i:  TX87StatusFlag;
begin
Result := [];
SW := GetX87StatusWord;
For i := Low(TX87StatusFlag) to High(TX87StatusFlag) do
  case i of
    sfStackFault:       If (SW and X87SW_StackFault) <> 0 then Include(Result,i);
    sfExceptionSummary: If (SW and X87SW_ExceptionSummary) <> 0 then Include(Result,i);
    sfFPUBusy:          If (SW and X87SW_FPUBusy) <> 0 then Include(Result,i);
    sfConditionCodeC0:  If (SW and X87SW_ConditionCode_C0) <> 0 then Include(Result,i);
    sfConditionCodeC1:  If (SW and X87SW_ConditionCode_C1) <> 0 then Include(Result,i);
    sfConditionCodeC2:  If (SW and X87SW_ConditionCode_C2) <> 0 then Include(Result,i);
    sfConditionCodeC3:  If (SW and X87SW_ConditionCode_C3) <> 0 then Include(Result,i);
  else
    raise EF80InvalidFlag.CreateFmt('GetX87StatusFlags: Invalid flag (%d).',[Ord(i)]);
  end;
end;

//------------------------------------------------------------------------------

Function SetX87StatusFlags(NewValue: TX87StatusFlags): TX87StatusFlags;
{$IFDEF PurePascal}

  procedure SetFlag(FlagMask: UInt16; NewState: Boolean);
  begin
    If NewState then
      Pas_X87SW := Pas_X87SW or FlagMask
    else
      Pas_X87SW := Pas_X87SW and not FlagMask;
  end;

begin
Result := GetX87StatusFlags;
SetFlag(X87SW_StackFault,sfStackFault in NewValue);
SetFlag(X87SW_ExceptionSummary,sfExceptionSummary in NewValue);
SetFlag(X87SW_FPUBusy,sfFPUBusy in NewValue);
SetFlag(X87SW_ConditionCode_C0,sfConditionCodeC0 in NewValue);
SetFlag(X87SW_ConditionCode_C1,sfConditionCodeC1 in NewValue);
SetFlag(X87SW_ConditionCode_C2,sfConditionCodeC2 in NewValue);
SetFlag(X87SW_ConditionCode_C3,sfConditionCodeC3 in NewValue);
end;
{$ELSE}
begin
Result := GetX87StatusFlags;
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function GetX87ControlFlag(Flag: TX87ControlFlag): Boolean;
begin
case Flag of
  cfInfinityControl:  Result := (GetX87ControlWord and X87CW_InfinityControl) <> 0;
else
  raise EF80InvalidFlag.CreateFmt('GetX87ControlFlag: Invalid flag (%d).',[Ord(Flag)]);
end;
end;

//------------------------------------------------------------------------------

Function SetX87ControlFlag(Flag: TX87ControlFlag; NewValue: Boolean): Boolean;

  procedure SetFlag(FlagMask: UInt16);
  begin
    If NewValue then
      SetX87ControlWord(GetX87ControlWord or FlagMask)
    else
      SetX87ControlWord(GetX87ControlWord and not FlagMask);
  end;

begin
Result := GetX87ControlFlag(Flag);
case Flag of
  cfInfinityControl:  SetFlag(X87CW_InfinityControl);
else
  raise EF80InvalidFlag.CreateFmt('SetX87ControlFlag: Invalid flag (%d).',[Ord(Flag)]);
end;
end;

//------------------------------------------------------------------------------

Function GetX87ControlFlags: TX87ControlFlags;
var
  CW: UInt16;
  i:  TX87ControlFlag;
begin
Result := [];
CW := GetX87ControlWord;
For i := Low(TX87ControlFlag) to High(TX87ControlFlag) do
  case i of
    cfInfinityControl:  If (CW and X87CW_InfinityControl) <> 0 then Include(Result,i);
  else
    raise EF80InvalidFlag.CreateFmt('GetX87ControlFlags: Invalid flag (%d).',[Ord(i)]);
  end;
end;

//------------------------------------------------------------------------------

Function SetX87ControlFlags(NewValue: TX87ControlFlags): TX87ControlFlags;
var
  CW: UInt16;

  procedure SetFlag(FlagMask: UInt16; NewState: Boolean);
  begin
    If NewState then
      CW := CW or FlagMask
    else
      CW := CW and not FlagMask;
  end;

begin
Result := GetX87ControlFlags;
CW := GetX87ControlWord;
SetFlag(X87CW_InfinityControl,cfInfinityControl in NewValue);
SetX87ControlWord(CW);
end;

{-------------------------------------------------------------------------------
    Auxiliary routines - abstracted x87 exception flags access
-------------------------------------------------------------------------------}

Function GetX87ExceptionMask(Exception: TX87Exception): Boolean;
begin
case Exception of
  excInvalidOp: Result := (GetX87ControlWord and X87CW_EMASK_InvalidOP) <> 0;
  excDenormal:  Result := (GetX87ControlWord and X87CW_EMASK_Denormal) <> 0;
  excDivByZero: Result := (GetX87ControlWord and X87CW_EMASK_DivByZero) <> 0;
  excOverflow:  Result := (GetX87ControlWord and X87CW_EMASK_Overflow) <> 0;
  excUnderflow: Result := (GetX87ControlWord and X87CW_EMASK_Underflow) <> 0;
  excPrecision: Result := (GetX87ControlWord and X87CW_EMASK_Precision) <> 0;
else
  raise EF80InvalidFlag.CreateFmt('GetX87ExceptionMask: Invalid X87 exception (%d).',[Ord(Exception)]);
end;
end;

//------------------------------------------------------------------------------

Function SetX87ExceptionMask(Exception: TX87Exception; NewValue: Boolean): Boolean;

  procedure SetFlag(ExceptionMask: UInt16);
  begin
    If NewValue then
      SetX87ControlWord(GetX87ControlWord or ExceptionMask)
    else
      SetX87ControlWord(GetX87ControlWord and not ExceptionMask);
  end;

begin
Result := GetX87ExceptionMask(Exception);
case Exception of
  excInvalidOp: SetFlag(X87CW_EMASK_InvalidOP);
  excDenormal:  SetFlag(X87CW_EMASK_Denormal);
  excDivByZero: SetFlag(X87CW_EMASK_DivByZero);
  excOverflow:  SetFlag(X87CW_EMASK_Overflow);
  excUnderflow: SetFlag(X87CW_EMASK_Underflow);
  excPrecision: SetFlag(X87CW_EMASK_Precision);
else
  raise EF80InvalidFlag.CreateFmt('SetX87ExceptionMask: Invalid X87 exception (%d).',[Ord(Exception)]);
end;
end;

//------------------------------------------------------------------------------

Function GetX87ExceptionMasks: TX87Exceptions;
var
  CW: UInt16;
  i:  TX87Exception;
begin
Result := [];
CW := GetX87ControlWord;
For i := Low(TX87Exception) to High(TX87Exception) do
  case i of
    excInvalidOp: If (CW and X87CW_EMASK_InvalidOP) <> 0 then Include(Result,i);
    excDenormal:  If (CW and X87CW_EMASK_Denormal) <> 0 then Include(Result,i);
    excDivByZero: If (CW and X87CW_EMASK_DivByZero) <> 0 then Include(Result,i);
    excOverflow:  If (CW and X87CW_EMASK_Overflow) <> 0 then Include(Result,i);
    excUnderflow: If (CW and X87CW_EMASK_Underflow) <> 0 then Include(Result,i);
    excPrecision: If (CW and X87CW_EMASK_Precision) <> 0 then Include(Result,i);
  else
    raise EF80InvalidFlag.CreateFmt('GetX87ExceptionMasks: Invalid X87 exception (%d).',[Ord(i)]);
  end;
end;

//------------------------------------------------------------------------------

Function SetX87ExceptionMasks(NewValue: TX87Exceptions): TX87Exceptions;
var
  CW: UInt16;

  procedure SetFlag(ExceptionMask: UInt16; NewState: Boolean);
  begin
    If NewState then
      CW := CW or ExceptionMask
    else
      CW := CW and not ExceptionMask;
  end;

begin
Result := GetX87ExceptionMasks;
CW := GetX87ControlWord;
SetFlag(X87CW_EMASK_InvalidOP,excInvalidOp in NewValue);
SetFlag(X87CW_EMASK_Denormal,excDenormal in NewValue);
SetFlag(X87CW_EMASK_DivByZero,excDivByZero in NewValue);
SetFlag(X87CW_EMASK_Overflow,excOverflow in NewValue);
SetFlag(X87CW_EMASK_Underflow,excUnderflow in NewValue);
SetFlag(X87CW_EMASK_Precision,excPrecision in NewValue);
SetX87ControlWord(CW);
end;

//------------------------------------------------------------------------------

Function GetX87ExceptionState(Exception: TX87Exception): Boolean;
begin
case Exception of
  excInvalidOp: Result := (GetX87StatusWord and X87SW_EX_InvalidOP) <> 0;
  excDenormal:  Result := (GetX87StatusWord and X87SW_EX_Denormal) <> 0;
  excDivByZero: Result := (GetX87StatusWord and X87SW_EX_DivByZero) <> 0;
  excOverflow:  Result := (GetX87StatusWord and X87SW_EX_Overflow) <> 0;
  excUnderflow: Result := (GetX87StatusWord and X87SW_EX_Underflow) <> 0;
  excPrecision: Result := (GetX87StatusWord and X87SW_EX_Precision) <> 0;
else
  raise EF80InvalidFlag.CreateFmt('GetX87ExceptionState: Invalid X87 exception (%d).',[Ord(Exception)]);
end;
end;

//------------------------------------------------------------------------------

Function SetX87ExceptionState(Exception: TX87Exception; NewValue: Boolean): Boolean;
{$IFDEF PurePascal}

  procedure SetFlag(ExceptionMask: UInt16);
  begin
    If NewValue then
      Pas_X87SW := Pas_X87SW or ExceptionMask
    else
      Pas_X87SW := Pas_X87SW and not ExceptionMask
  end;

begin
Result := GetX87ExceptionState(Exception);
case Exception of
  excInvalidOp: SetFlag(X87SW_EX_InvalidOP);
  excDenormal:  SetFlag(X87SW_EX_Denormal);
  excDivByZero: SetFlag(X87SW_EX_DivByZero);
  excOverflow:  SetFlag(X87SW_EX_Overflow);
  excUnderflow: SetFlag(X87SW_EX_Underflow);
  excPrecision: SetFlag(X87SW_EX_Precision);
else
  raise EF80InvalidFlag.CreateFmt('SetX87ExceptionState: Invalid X87 exception (%d).',[Ord(Exception)]);
end;
end;
{$ELSE}
begin
Result := GetX87ExceptionState(Exception);
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function GetX87ExceptionStates: TX87Exceptions;
var
  SW: UInt16;
  i:  TX87Exception;
begin
Result := [];
SW := GetX87StatusWord;
For i := Low(TX87Exception) to High(TX87Exception) do
  case i of
    excInvalidOp: If (SW and X87SW_EX_InvalidOP) <> 0 then Include(Result,i);
    excDenormal:  If (SW and X87SW_EX_Denormal) <> 0 then Include(Result,i);
    excDivByZero: If (SW and X87SW_EX_DivByZero) <> 0 then Include(Result,i);
    excOverflow:  If (SW and X87SW_EX_Overflow) <> 0 then Include(Result,i);
    excUnderflow: If (SW and X87SW_EX_Underflow) <> 0 then Include(Result,i);
    excPrecision: If (SW and X87SW_EX_Precision) <> 0 then Include(Result,i);
  else
    raise EF80InvalidFlag.CreateFmt('GetX87ExceptionStates: Invalid X87 exception (%d).',[Ord(i)]);
  end;
end;

//------------------------------------------------------------------------------

Function SetX87ExceptionStates(NewValue: TX87Exceptions): TX87Exceptions;
{$IFDEF PurePascal}

  procedure SetFlag(ExceptionMask: UInt16; NewState: Boolean);
  begin
    If NewState then
      Pas_X87SW := Pas_X87SW or ExceptionMask
    else
      Pas_X87SW := Pas_X87SW and not ExceptionMask;
  end;

begin
Result := GetX87ExceptionStates;
SetFlag(X87SW_EX_InvalidOP,excInvalidOp in NewValue);
SetFlag(X87SW_EX_Denormal,excDenormal in NewValue);
SetFlag(X87SW_EX_DivByZero,excDivByZero in NewValue);
SetFlag(X87SW_EX_Overflow,excOverflow in NewValue);
SetFlag(X87SW_EX_Underflow,excUnderflow in NewValue);
SetFlag(X87SW_EX_Precision,excPrecision in NewValue);
end;
{$ELSE}
begin
Result := GetX87ExceptionStates;
end;
{$ENDIF}

//------------------------------------------------------------------------------

procedure ClearX87Exceptions;
{$IFNDEF PurePascal}
asm
    FCLEX
end;
{$ELSE}
begin
Pas_X87SW := 0;
end;
{$ENDIF}

//------------------------------------------------------------------------------

procedure RaiseX87Exceptions;
var
  SW: UInt16;
begin
SW := GetX87StatusWord;
If (SW and X87SW_EX_InvalidOP) <> 0 then
  begin
    If (SW and X87SW_StackFault) <> 0 then
      begin
        If (SW and X87SW_ConditionCode_C1) <> 0 then
          raise EF80StackOverflow.CreateDefMsg
        else
          raise EF80StackUnderflow.CreateDefMsg;
      end
    else raise EF80InvalidOp.CreateDefMsg;
  end;
If (SW and X87SW_EX_Denormal) <> 0 then
  raise EF80Denormal.CreateDefMsg;
If (SW and X87SW_EX_DivByZero) <> 0 then
  raise EF80DivByZero.CreateDefMsg;
If (SW and X87SW_EX_Overflow) <> 0 then
  raise EF80Overflow.CreateDefMsg;
If (SW and X87SW_EX_Underflow) <> 0 then
  raise EF80Underflow.CreateDefMsg;
If (SW and X87SW_EX_Precision) <> 0 then
  raise EF80Precision.CreateDefMsg;
end;


{===============================================================================
--------------------------------------------------------------------------------
                         Float80 <-> Float64 conversions
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    Conversion routines - implementation
===============================================================================}
{-------------------------------------------------------------------------------
    Conversion routines - axiliary routines
-------------------------------------------------------------------------------}
{
  There is a need for calculation with higher width than 64bits in conversion
  from Float80 to Float64 (in mantissa denormalization).
  Following routines and types implement bare minimum required for calculations
  on 65bit wide integer.
}
type
  UInt65 = record
    Low64:  UInt64;
    Bit65:  Byte;
  end;

const
  UInt65_ZERO: UInt65 = (Low64: 0; Bit65: 0);

  UI65_CMP_SMALLER = -1;
  UI65_CMP_LARGER  = +1;

//------------------------------------------------------------------------------

Function UInt65Get(Low64: UInt64; Bit65: Byte): UInt65;
begin
Result.Low64 := Low64;
Result.Bit65 := Bit65 and 1;
end;

//------------------------------------------------------------------------------

Function UInt65Not(Value: UInt65): UInt65;
begin
Result.Low64 := not Value.Low64;
Result.Bit65 := (not Value.Bit65) and 1;
end;

//------------------------------------------------------------------------------

Function UInt65And(A,B: UInt65): UInt65;
begin
Result.Low64 := A.Low64 and B.Low64;
Result.Bit65 := (A.Bit65 and B.Bit65) and 1;
end;

//------------------------------------------------------------------------------

Function UInt65Add(Value: UInt65; Add: UInt65): UInt65;
var
  Temp:   Int64;
  Carry:  Boolean;
begin
Temp := Int64(Int64Rec(Value.Low64).Lo) + Int64(Int64Rec(Add.Low64).Lo);
Carry := Temp > $FFFFFFFF;
Int64Rec(Result.Low64).Lo := Temp and $FFFFFFFF;  
If Carry then
  Temp := Int64(Int64Rec(Value.Low64).Hi) + Int64(Int64Rec(Add.Low64).Hi) + 1
else
  Temp := Int64(Int64Rec(Value.Low64).Hi) + Int64(Int64Rec(Add.Low64).Hi);
Carry := Temp > $FFFFFFFF;
Int64Rec(Result.Low64).Hi := Temp and $FFFFFFFF;
If Carry then
  Temp := Int64(Value.Bit65 and 1) + Int64(Add.Bit65 and 1) + 1
else
  Temp := Int64(Value.Bit65 and 1) + Int64(Add.Bit65 and 1);
Result.Bit65 := Byte(Temp and 1);
end;

//------------------------------------------------------------------------------

Function UInt65Subtract(Value: UInt65; Sub: UInt65): UInt65;
var
  Temp:   Int64;
  Borrow: Boolean;
begin
Temp := Int64(Int64Rec(Value.Low64).Lo) - Int64(Int64Rec(Sub.Low64).Lo);
Borrow := Temp < 0;
Int64Rec(Result.Low64).Lo := Temp and $FFFFFFFF;  
If Borrow then
  Temp := Int64(Int64Rec(Value.Low64).Hi) - Int64(Int64Rec(Sub.Low64).Hi) - 1
else
  Temp := Int64(Int64Rec(Value.Low64).Hi) - Int64(Int64Rec(Sub.Low64).Hi);
Borrow := Temp < 0;
Int64Rec(Result.Low64).Hi := Temp and $FFFFFFFF;
If Borrow then
  Temp := Int64(Value.Bit65 and 1) - Int64(Sub.Bit65 and 1) - 1
else
  Temp := Int64(Value.Bit65 and 1) - Int64(Sub.Bit65 and 1);
Result.Bit65 := Byte(Temp and 1);
end;

//------------------------------------------------------------------------------

Function UInt65RShift(Value: UInt65; Shift: Byte): UInt65;
begin
If Shift <= 0 then
  Result := Value
else If (Shift > 0) and (Shift < 64) then
  begin
    Result.Low64 := Value.Low64 shr Shift or (UInt64(Value.Bit65 and 1) shl (64 - Shift));
    Result.Bit65 := 0;
  end
else If Shift = 64 then
  Result := UInt65Get(Value.Bit65 and 1,0)
else
  Result := UInt65_ZERO
end;

//------------------------------------------------------------------------------

Function UInt65Compare(A,B: UInt65): Integer;
begin
If (A.Bit65 and 1) > (B.Bit65 and 1) then
  Result := UI65_CMP_LARGER
else If (A.Bit65 and 1) < (B.Bit65 and 1) then
  Result := UI65_CMP_SMALLER
else
  begin
    If Int64Rec(A.Low64).Hi > Int64Rec(B.Low64).Hi then
      Result := UI65_CMP_LARGER
    else If Int64Rec(A.Low64).Hi < Int64Rec(B.Low64).Hi then
      Result := UI65_CMP_SMALLER
    else
      begin
        If Int64Rec(A.Low64).Lo > Int64Rec(B.Low64).Lo then
          Result := UI65_CMP_LARGER
        else If Int64Rec(A.Low64).Lo < Int64Rec(B.Low64).Lo then
          Result := UI65_CMP_SMALLER
        else
          Result := 0;
      end;
  end;
end;

//------------------------------------------------------------------------------

Function UInt65IsZero(Value: UInt65): Boolean;
begin
Result := (Value.Low64 = 0) and ((Value.Bit65 and 1) = 0);
end;

{-------------------------------------------------------------------------------
    Conversion routines - Float64 -> Float80 conversion
-------------------------------------------------------------------------------}

procedure Float64ToFloat80(Float64Ptr,Float80Ptr: Pointer);{$IFNDEF PurePascal} register; assembler;
asm
    FLD     qword ptr [Float64Ptr]
    FSTP    tbyte ptr [Float80Ptr]
    FWAIT
end;
{$ELSE}
var
  Sign:           UInt64;
  Exponent:       Int32;  // biased exponent (bias 1023)
  Mantissa:       UInt64;
  MantissaShift:  Integer;

  procedure BuildExtendedResult(Upper: UInt16; Lower: UInt64);
  begin
  {$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
    PUInt16(PtrUInt(Float80Ptr) + 8)^ := Upper;
  {$IFDEF FPCDWM}{$POP}{$ENDIF}
    UInt64(Float80Ptr^) := Lower;
  end;

  Function HighZeroCount(Value: UInt64): Integer;
  begin
    If Value <> 0 then
      begin
        Result := 0;
        while (Value and UInt64($8000000000000000)) = 0  do
          begin
            Value := UInt64(Value shl 1);
            Inc(Result);
          end;
      end
    else Result := 64;
  end;

begin
Sign := UInt64(Float64Ptr^) and F64_MASK_SIGN;
Exponent := Int32((UInt64(Float64Ptr^) and F64_MASK_EXP) shr 52);
Mantissa := UInt64(Float64Ptr^) and F64_MASK_FRAC;
case Exponent of

        // zero exponent - zero or denormal
  0:    If Mantissa <> 0 then
          begin
            // denormal
            If GetX87ExceptionMask(excDenormal) then
              begin
                SetX87ExceptionState(excDenormal,True);
              {
                normalize...

                ...shift mantissa left so that its highest set bit will be shifted
                to integer bit (bit 63), also correct exponent to reflect this
                change
              }
                MantissaShift := HighZeroCount(Mantissa);
                BuildExtendedResult(UInt16(Sign shr 48) or UInt16(Exponent - MantissaShift + 15372),
                                    UInt64(Mantissa shl MantissaShift));
              end
            else raise EF80Denormal.CreateDefMsg;
          end
        // return signed zero
        else BuildExtendedResult(UInt16(Sign shr 48),0);

        // max exponent - infinity or NaN
  $7FF: If Mantissa <> 0 then
          begin
            // not a number
            If (Mantissa and F64_MASK_FHB) = 0 then
              begin
                // signaled NaN
                If GetX87ExceptionMask(excInvalidOp) then
                  begin
                    SetX87ExceptionState(excInvalidOp,True);
                    // quiet signed NaN with mantissa
                    BuildExtendedResult(UInt16(Sign shr 48) or F80_MASK16_EXP,
                                        UInt64(Mantissa shl 11) or F80_MASK64_FHB or F80_MASK64_INTB)
                  end
                // signaling NaN
                else raise EF80InvalidOp.CreateDefMsg;
              end
            // quiet signed NaN with mantissa
            else BuildExtendedResult(UInt16(Sign shr 48) or F80_MASK16_EXP,
                                     UInt64(Mantissa shl 11) or F80_MASK64_INTB);
          end  
        // signed infinity
        else BuildExtendedResult(UInt16(Sign shr 48) or F80_MASK16_EXP,F80_MASK64_INTB);

else
  // normal number
  BuildExtendedResult(UInt16(Sign shr 48) or UInt16(Exponent + 15360{16383 - 1023}),
                      UInt64(Mantissa shl 11) or F80_MASK64_INTB);
end;
end;
{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Float64ToFloat80(Value: Float64): AuxTypes.Float80;
begin
Float64ToFloat80(@Value,@Result);
end;

{-------------------------------------------------------------------------------
    Conversion routines - Float80 -> Float64 conversion
-------------------------------------------------------------------------------}

procedure Float80ToFloat64(Float80Ptr,Float64Ptr: Pointer); register;{$IFNDEF PurePascal} assembler;
asm
    FLD     tbyte ptr [Float80Ptr]       
    FSTP    qword ptr [Float64Ptr]
    FWAIT
end;
{$ELSE}
var
  Sign:         UInt64;
  Exponent:     Int32;    // biased exponent (bias 16383)
  Mantissa:     UInt64;   // including integer bit
  RoundMode:    TX87RoundingMode;
  BitsLost:     Boolean;
  ManOverflow:  Boolean;

  Function ShiftMantissaDenorm(Value: UInt64; Shift: Byte; out DataLoss: Boolean): UInt64;
  var
    Value65:  UInt65;
    Mask:     UInt65;
    Low:      UInt65;
    High:     UInt65;
    DiffLow:  UInt65;
    DiffHigh: UInt65;
  begin
    // min possible shift 12, max possible shift 64
    // everything must be calculated in at least 65bits
    DataLoss := False;
    If (Shift > 0) and (Shift <= 64) then
      begin
        Value65 := UInt65Get(Value,0);
        Mask := UInt65RShift(UInt65Get(UInt64($FFFFFFFFFFFFFFFF),0),64 - Shift);
        If not UInt65IsZero(UInt65And(UInt65Get(Value,0),Mask)) then
          begin
            DataLoss := not UInt65IsZero(UInt65And(Value65,Mask));          
            Low := UInt65And(Value65,UInt65Not(Mask));
            High := UInt65Add(Low,UInt65Add(Mask,UInt65Get(1,0))); 
            case RoundMode of
              rmDown:     If Sign <> 0 then
                            Result := UInt65RShift(High,Shift).Low64
                          else
                            Result := UInt65RShift(Low,Shift).Low64;
              rmUp:       If Sign <> 0 then
                            Result := UInt65RShift(Low,Shift).Low64
                          else
                            Result := UInt65RShift(High,Shift).Low64;
              rmTruncate: Result := UInt65RShift(Low,Shift).Low64;
            else
             {rmNearest}
              DiffLow := UInt65Subtract(Value65,Low);
              DiffHigh := UInt65Subtract(High,Value65);
              case UInt65Compare(DiffLow,DiffHigh) of
                UI65_CMP_SMALLER: Result := UInt65RShift(Low,Shift).Low64;
                UI65_CMP_LARGER:  Result := UInt65RShift(High,Shift).Low64;
              else
                // select the one with clear lowest bit
                If UInt65IsZero(UInt65And(Low,UInt65Add(Mask,UInt65Get(1,0)))) then
                  Result := UInt65RShift(Low,Shift).Low64
                else
                  Result := UInt65RShift(High,Shift).Low64;
              end;
            end;

          end
        else Result := Value shr Shift;
      end
    // following cases should never happen, but...
    else If Shift > 64 then
      Result := 0
    else
      Result := Value;
  end;

  Function ShiftMantissa(Value: UInt64; out DataLoss: Boolean): UInt64;
  const
    SM_MASK = UInt64($7FF);
  var
    Low:      UInt64;
    High:     UInt64;
    DiffLow:  UInt64;
    DiffHigh: UInt64;
  begin
    // implicit shift 11, static mask $7FF
    DataLoss := False;
    If (Value and SM_MASK) <> 0 then
      begin
        DataLoss := (Value and SM_MASK) <> 0;
        Low := Value and not SM_MASK;
        High := Low + (SM_MASK + 1);
        case RoundMode of
          rmDown:     If Sign <> 0 then
                        Result := High shr 11
                      else
                        Result := Low shr 11;
          rmUp:       If Sign <> 0 then
                        Result := Low shr 11
                      else
                        Result := High shr 11;
          rmTruncate: Result := Low shr 11;
        else
         {rmNearest}
          DiffLow := Value - Low;
          DiffHigh := High - Value;
          If DiffLow < DiffHigh then
            Result := Low shr 11
          else If DiffLow > DiffHigh then
            Result := High shr 11
          else
            begin
              // select the one with clear lowest bit
              If (Low and (SM_MASK + 1)) = 0 then
                Result := Low shr 11
              else
                Result := High shr 11;
            end;
        end;
      end
    else Result := Value shr 11;
  end;

begin
RoundMode := GetX87RoundingMode;
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
Sign := UInt64(PUInt8(PtrUInt(Float80Ptr) + 9)^ and $80) shl 56;
Exponent := Int32(PUInt16(PtrUInt(Float80Ptr) + 8)^) and F80_MASK16_EXP;
{$IFDEF FPCDWM}{$POP}{$ENDIF}
Mantissa := UInt64(Float80Ptr^);
// check unsupported encodings...
If ((Exponent > 0) and (Exponent <= F80_MASK16_EXP)) and ((Mantissa and F80_MASK64_INTB) = 0) then
  begin
  {
    unnormals (seemingly normalized numbers, but with integer bit of 0),
    pseudo-infinities and pseudo-NaNs (both with integer bit 0)
  }
    If GetX87ExceptionMask(excInvalidOP) then
      begin
        // return negative QNaN (QNaN floating point indefinite)
        PUInt64(Float64Ptr)^ := F64_MASK_SIGN or F64_MASK_EXP or F64_MASK_FHB;
        SetX87ExceptionState(excInvalidOP,True)
      end
    else raise EF80InvalidOP.CreateDefMsg;
  end
else
  case Exponent of
            // exponent of zero - zero or denormal
    0:      If Mantissa <> 0 then
              begin
              {
                non-zero mantissa - denormals

                Note that psedo-denormals (denormals with integer bit 1) are
                treated as usual denormals (with integer bit 0).

                Also note that X87 is not signaling denormal exception when
                converting from extended precision numbers.
              }
                If ((RoundMode = rmUp) and (Sign = 0)) or
                   ((RoundMode = rmDown) and (Sign <> 0)) then
                  // return signed smallest representable number
                  PUInt64(Float64Ptr)^ := Sign or UInt64(1)
                else
                  // convert to signed zero
                  PUInt64(Float64Ptr)^ := Sign;
                // post-computation exceptions
                If GetX87ExceptionMask(excUnderflow) then
                  SetX87ExceptionState(excUnderflow,True)
                else
                  raise EF80Underflow.CreateDefMsg;
                If GetX87ExceptionMask(excPrecision) then
                  SetX87ExceptionState(excPrecision,True)
                else
                  raise EF80Precision.CreateDefMsg;
              end
            // mantissa of 0 - return signed zero
            else PUInt64(Float64Ptr)^ := Sign;

          {
            exponent 1..15307 (-16382..-1076 unbiased) - exponent too small
            to be represented in double even as denormal
          }
    $1..
    $3BCB:  begin
              If ((RoundMode = rmUp) and (Sign = 0)) or
                 ((RoundMode = rmDown) and (Sign <> 0)) then
                // return signed smallest representable number
                PUInt64(Float64Ptr)^ := Sign or UInt64(1)
              else
                // convert to signed zero
                PUInt64(Float64Ptr)^ := Sign;
              // post-computation exceptions
              If GetX87ExceptionMask(excUnderflow) then
                SetX87ExceptionState(excUnderflow,True)
              else
                raise EF80Underflow.CreateDefMsg;
              If GetX87ExceptionMask(excPrecision) then
                SetX87ExceptionState(excPrecision,True)
              else
                raise EF80Precision.CreateDefMsg;
            end;

          {
            exponent 15308..15360 (-1075..-1023 unbiased) - exponent still too
            small to be represented in double, but can be denormalized to fit
            (result will have implicit exponent of -1022, explicit 0)
          }
    $3BCC..
    $3C00:  begin
            {
              denormalize

              Note that ShiftMantissaEx can return mantissa with integer bit
              (bit 52) set.
              This, when not masked, will result in biased exponent of 1.
              It is correct and expected behaviour, it just has to be observed
              when raising underflow exceptions.
            }
              PUInt64(Float64Ptr)^ := Sign or ShiftMantissaDenorm(Mantissa,$3C0C - Exponent,BitsLost);
              If BitsLost then
                begin
                  // post-computation exceptions
                  If (PUInt64(Float64Ptr)^ and F64_MASK_EXP) = 0 then
                    begin
                      // number was NOT converted to normalized encoding
                      If GetX87ExceptionMask(excUnderflow) then
                        SetX87ExceptionState(excUnderflow,True)
                      else
                        raise EF80Underflow.CreateDefMsg;
                    end;
                  If GetX87ExceptionMask(excPrecision) then
                    SetX87ExceptionState(excPrecision,True)
                  else
                    raise EF80Precision.CreateDefMsg;
                end;
            end;

          {
            exponent 17407..32766 (1024..16383 unbiased) - exponent too large
            to be represented in double (max. valid exponent 1023)
          }
    $43FF..
    $7FFE:  begin
              If (RoundMode = rmTruncate) or
                 ((RoundMode = rmUp) and (Sign <> 0)) or
                 ((RoundMode = rmDown) and (Sign = 0)) then
                // return signed largest representable number
                PUInt64(Float64Ptr)^ := Sign or UInt64($7FEFFFFFFFFFFFFF)
              else
                // convert to signed infinity
                PUInt64(Float64Ptr)^ := Sign or F64_MASK_EXP;
              // post-computation exceptions
              If GetX87ExceptionMask(excOverflow) then
                SetX87ExceptionState(excOverflow,True)
              else
                raise EF80Overflow.CreateDefMsg;
              If GetX87ExceptionMask(excPrecision) then
                SetX87ExceptionState(excPrecision,True)
              else
                raise EF80Precision.CreateDefMsg;                
            end;

          {
            maximum exponent - NaN or infinity (note that pseudo-NaN and
            pseudo-infinities are managed separately along with unnormals)
          }
    $7FFF:  If (Mantissa and F80_MASK64_FRAC) <> 0 then
              begin
                // not a number (NaN)
                If (Mantissa and F80_MASK64_FHB) = 0 then
                  begin
                    // signaling NaN
                    If GetX87ExceptionMask(excInvalidOP) then
                      begin
                        // return quiet signed NaN with truncated mantissa
                        SetX87ExceptionState(excInvalidOP,True);
                        PUInt64(Float64Ptr)^ := Sign or F64_MASK_EXP or F64_MASK_FHB or (Mantissa shr 11);
                      end
                    // singal NaN
                    else raise EF80InvalidOP.CreateDefMsg;
                  end
                // quiet signed NaN with truncated mantissa
                else PUInt64(Float64Ptr)^ := Sign or F64_MASK_EXP or F64_MASK_FHB or (Mantissa shr 11);
              end
            // renturn signed infinity
            else PUInt64(Float64Ptr)^ := Sign or F64_MASK_EXP;

  else
    // exponent 15361..17406 (-1022..1023 unbiased) - representable normalized value
    Mantissa := ShiftMantissa(Mantissa and not F80_MASK64_INTB,BitsLost);
    // check if mantisa overflowed - if so, increase exponent to compensate (mantissa will be zero)
    If Mantissa > F64_MASK_FRAC then
      begin
        Inc(Exponent);
        ManOverflow := True;
      end
    else ManOverflow := False;
    PUInt64(Float64Ptr)^ := Sign or
                            ((UInt64(Exponent - 15360) shl 52) and F64_MASK_EXP) or
                            (Mantissa and F64_MASK_FRAC);
    // post-computation exceptions
    If ManOverflow and (Exponent > 17406) then
      begin
        // number was converted to infinity
        If GetX87ExceptionMask(excOverflow) then
          SetX87ExceptionState(excOverflow,True)
        else
          raise EF80Overflow.CreateDefMsg;
      end;
    If BitsLost then
      begin
        If GetX87ExceptionMask(excPrecision) then
          SetX87ExceptionState(excPrecision,True)
        else
          raise EF80Precision.CreateDefMsg;
      end;
  end;
end;
{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Float80ToFloat64(Value: AuxTypes.Float80): Float64;
begin
Float80ToFloat64(@Value,@Result);
end;

end.
