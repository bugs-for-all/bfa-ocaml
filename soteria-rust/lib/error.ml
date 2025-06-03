type t =
  [ (* Memory safety *)
    `DoubleFree
  | `InvalidFree
  | `MemoryLeak
  | `MisalignedPointer
  | `NullDereference
  | `OutOfBounds
  | `UninitializedMemoryAccess
  | `UseAfterFree
  | `MisalignedFnPointer
  | (* Arithmetic *)
    `DivisionByZero
  | `InvalidShift
  | `Overflow
  | (* Undefined behaviour *)
    `RefToUninhabited
  | `StdErr of string
  | `UBAbort
  | `UBDanglingPointer
  | `UBPointerArithmetic
  | `UBPointerComparison
  | `UBTransmute
  | (* Tree borrows *)
    `AliasingError
  | (* Explicit errors *)
    `FailedAssert of string option
  | `Panic of string option
  | `UnwindTerminate
  | (* Local was not initialised (impossible without `mir!`) *)
    `DeadVariable
  | (* Meta errors *)
    `MetaExpectedError
    (* Type is too large for memory *)
  | `InvalidLayout ]

let is_unwindable : [> t ] -> bool = function
  | `NullDereference | `OutOfBounds | `DivisionByZero | `FailedAssert _
  | `Panic _ | `Overflow ->
      true
  | _ -> false

let pp_err ft ((err : [< t ]), call_trace) =
  Format.open_vbox 0;
  let () =
    match err with
    | `AliasingError -> Fmt.string ft "Aliasing error"
    | `DeadVariable -> Fmt.string ft "Dead variable accessed"
    | `DivisionByZero -> Fmt.string ft "Division by zero"
    | `DoubleFree -> Fmt.string ft "Double free"
    | `FailedAssert (Some msg) -> Fmt.pf ft "Failed assertion: %s" msg
    | `FailedAssert None -> Fmt.string ft "Failed assertion"
    | `InvalidFree -> Fmt.string ft "Invalid free"
    | `InvalidLayout -> Fmt.string ft "Invalid layout"
    | `InvalidShift -> Fmt.string ft "Invalid binary shift"
    | `MemoryLeak -> Fmt.string ft "Memory leak"
    | `MetaExpectedError -> Fmt.string ft "Meta: expected an error"
    | `MisalignedFnPointer -> Fmt.string ft "Misaligned function pointer"
    | `MisalignedPointer -> Fmt.string ft "Misaligned pointer"
    | `NullDereference -> Fmt.string ft "Null dereference"
    | `OutOfBounds -> Fmt.string ft "Out of bounds"
    | `Overflow -> Fmt.string ft "Overflow"
    | `Panic (Some msg) -> Fmt.pf ft "Panic: %s" msg
    | `Panic None -> Fmt.pf ft "Panic"
    | `RefToUninhabited -> Fmt.string ft "Ref to uninhabited type"
    | `StdErr msg -> Fmt.pf ft "UB in std: %s" msg
    | `UninitializedMemoryAccess -> Fmt.string ft "Uninitialized memory access"
    | `UBAbort -> Fmt.string ft "UB: undefined behaviour trap reached"
    | `UBDanglingPointer -> Fmt.string ft "UB: dangling pointer"
    | `UBPointerArithmetic -> Fmt.string ft "UB: pointer arithmetic"
    | `UBPointerComparison -> Fmt.string ft "UB: pointer comparison"
    | `UBTransmute -> Fmt.string ft "UB: Transmute"
    | `UnwindTerminate -> Fmt.string ft "Terminated unwind"
    | `UseAfterFree -> Fmt.string ft "Use after free"
  in
  Fmt.pf ft "@,Trace:@,%a" Call_trace.pp call_trace;
  Format.close_box ()
