(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Role:
   Provide coefficient/batch checkpoint IO, progress and bounded diagnostics for both reduction algorithms.

   Inputs:
   Coefficient records, batch identities and explicit cache paths.

   Dependencies:
   Algebra/MixedProjection.wl supplies finite gauge validation; GaugeCoefficients.wl
   supplies coefficient/batch predicates at call time. No reduction algorithm is selected or redefined here.
*)

ClearAll[RGZGaugeBatchCacheRead, RGZGaugeBatchCacheWrite, RGZGaugeBatchSummary, RGZGaugeCacheDirectory, RGZGaugeCacheRead, RGZGaugeCacheWrite, RGZGaugeFixedCacheFailure, RGZGaugeFixedCacheInfo, RGZGaugeFixedCacheRead, RGZGaugeFixedCacheWrite, RGZGaugeFixedFinalInfo, RGZGaugeFixedSaveProgress, RGZGaugeFixedSetCache, RGZGaugeFixedSummary, RGZGaugeFixedValidPacketQ, RGZGaugeInspectBatch, RGZGaugeInspectStopped, RGZGaugeProgressSummary, RGZGaugeShow, RGZGaugeSparseSummary, RGZGaugeStep, RGZGaugeStoppedInfo, RGZGaugeSummary];

RGZGaugeStep[label_String] := (
  RGZMXProgressUpdate["Substep"->label,"OperationStartTime"->AbsoluteTime[]];
  If[AssociationQ[rgzGaugeProgress],AssociateTo[rgzGaugeProgress,
    {"CurrentOperation"->label,"OperationStartTime"->AbsoluteTime[]}]];Null);

(* Separate residual cache: the diagram/integral caches are never modified.
   A fingerprint contains BOTH coefficient branches, the physical master key,
   reference gauge, pole definitions and this exact algebra implementation.
   Timed-out/failed rows are not reused as a successful computation. *)
RGZGaugeCacheDirectory[root_] := Module[{r=root,dir},
  If[r===None,Return[None]];
  If[r===Automatic,r=Quiet[Check[RGZWorkflow`Private`cacheDirectory[],None]]];
  If[!StringQ[r],Return[None]];
  dir=FileNameJoin[{r,"RGZ_Gauge_coefficient_cache_v3"}];
  If[!DirectoryQ[dir],Quiet[Check[
    CreateDirectory[dir,CreateIntermediateDirectories->True],Return[None]]]];
  dir
];

RGZGaugeCacheRead[path_String,key_] := Module[{r},
  If[!FileExistsQ[path],Return[Missing["NoCache"]]];
  r=Quiet[Check[Get[path],$Failed]];
  If[AssociationQ[r] && Lookup[r,"Identity",None]===key &&
      Lookup[r,"ValidationPolicy",None]==="GaugeFiniteScalar/3" &&
      RGZGaugeRowQ[Lookup[r,"Result",None]],
    r["Result"],Missing["NoValidCache"]]
];

RGZGaugeCacheWrite[row_,path_,key_] := Module[{r},
  If[!RGZGaugeRowQ[row],Return[Failure["InvalidGaugeCacheRow",<||>]]];
  If[path===None,Return[Null]];
  r=CheckAbort[TimeConstrained[
    RGZMXWrite[<|"Identity"->key,"ValidationPolicy"->"GaugeFiniteScalar/3","Result"->row|>,path],10,
    Missing["CacheWriteTimeout"]],Missing["CacheWriteAborted"]];
  If[FailureQ[r] || Head[r]===Missing,
    AssociateTo[rgzGaugeProgress,"CacheWrite"->"Incomplete, memoryResultRetain"]];Null
];

RGZGaugeProgressSummary[] := If[AssociationQ[rgzGaugeProgress],
  Grid[({#,Lookup[rgzGaugeProgress,#,Missing["Unavailable"]]}& /@
    {"CompletedTermCount","MasterIntegralTermCount","Statistics","CurrentTerm","CurrentOperation","CurrentTermElapsedSeconds","CacheDirectory"}),
    Frame->All,Alignment->Left],"still NotStartedNewversionCoefficientCheck"];

(* Print only a bounded diagnostic, never the stored coefficient input. *)
RGZGaugeStoppedInfo[] := If[AssociationQ[rgzGaugeStoppedUnit],
  Grid[({#,Lookup[rgzGaugeStoppedUnit,#,Missing["Unavailable"]]}& /@
    {"ID","TotalTermCount","StoppedOperation","TimeLimit","ElapsedSeconds","CompletedTermCount"}),
    Frame->All,Alignment->Left],"No stopped timeout term was saved in this run"];

(* Structural inspection only: no Together, Cancel, Simplify or PV evaluation.
   Each column has a separate small time budget; never print raw coefficients. *)
RGZGaugeInspectStopped[seconds_:10] := Module[
  {labels,keys,columns,metrics,x,z,key},
  If[!AssociationQ[rgzGaugeStoppedUnit],
    Return[Failure["NoStoppedCoefficient",<||>]]];
  If[!NumericQ[seconds] || !TrueQ[seconds>0],
    Return[Failure["DiagnosticTimeLimit",<||>]]];
  labels={"Structure","ExpressionLeafCount","Size (MiB)","TopLevelAdditiveTermCount",
    "PoleTagOccurrences","SquaredrootOccurrences"};
  keys={"AlphaShortCoefficient","ReferenceShortCoefficient"};
  metrics[e_] := {Head[e],LeafCount[e],N[ByteCount[e]/2^20,4],
    If[Head[e]===Plus,Length[e],1],
    Count[e,rgzLoopM1|rgzLoopM2|rgzLoopM3|rgzLoopM4,{0,Infinity}],
    Count[e,HoldPattern[Power[_,n_Rational]] /; Denominator[n]===2,
      {0,Infinity}]};
  columns=Table[
    x=Lookup[rgzGaugeStoppedUnit,key,Missing["NoInput"]];
    If[Head[x]===Missing,ConstantArray[x,Length[labels]],
      z=CheckAbort[TimeConstrained[metrics[x],seconds,
        Missing["DiagnosticTimeout"]],Missing["DiagnosticAborted"]];
      If[ListQ[z],z,ConstantArray[z,Length[labels]]]],{key,keys}];
  Grid[Transpose[{
    Prepend[labels,"CheckParts"],
    Prepend[columns[[1]],"αShortLabelCoefficient"],
    Prepend[columns[[2]],"ReferenceShortLabelCoefficient"]
  }],Frame->All,Alignment->Left]
];

(* Bounded summary; do not format a Failure's full amplitude or data. *)
RGZGaugeSummary[report_] := Which[
  AssociationQ[report],Print[Grid[{
    {"ReferenceGauge",report["ReferenceGauge"]},{"MasterIntegralTermCount",report["MasterIntegralTermCount"]},
    {"Statistics",report["Statistics"]},{"Status",report["Status"]}},Frame->All,Alignment->Left]],
  FailureQ[report],Print["CheckIncomplete; FailedTag: ",report[[1]]];
    Module[{data=report[[2]],keys},
      keys=Select[{"ID","TotalTermCount","Stage","StoppedOperation","InputParts",
        "TimeLimit","ElapsedSeconds","CompletedTermCount","Description"},KeyExistsQ[data,#]&];
      If[keys=!={},Print[Grid[({#,data[#]}& /@ keys),
        Frame->All,Alignment->Left]]]],
  Head[report]===Missing,Print["CheckIncomplete: ",report];
    If[report[[1]]==="PreparationTimeout",
      Print["Legacy preprocessing timed out before master-integral coefficient checks."]],
  True,Print["CheckIncomplete; head: ",Head[report]]
];

RGZGaugeShow[report_Association,n_Integer] := Module[{rows,row},
  rows=Lookup[report,"ResidualParts",{}];
  If[n<1 || n>Length[rows],Return[Failure["ResidualRowNumber",<||>]]];
  row=rows[[n]];
  Grid[{{"ID",n},{"MasterIntegral",TraditionalForm[row["MasterIntegral"]]},
    {"ResidualCoefficient",TraditionalForm[row["ResidualCoefficient"]]},
    {"Status",row["Status"]}},Frame->All,Alignment->Left]
];

RGZGaugeBatchCacheRead[path_,identity_] := Module[{r},
  If[!StringQ[path],Return[Missing["NoBatchCache"]]];
  If[FileExistsQ[path],
    r=Quiet[Check[Get[path],$Failed]];
    If[AssociationQ[r] && Lookup[r,"Identity",None]===identity &&
        Lookup[r,"ValidationPolicy",None]==="GaugeFiniteScalar/3" &&
        KeyExistsQ[r,"Result"] && TrueQ[RGZGaugeValidate[r["Result"]]===True] &&
        If[AssociationQ[r["Result"]],RGZGaugeBatchPreparedQ[r["Result"]],!ListQ[r["Result"]]],
      Return[r["Result"]]]];
  (* Ordinary batches formerly used the fixed WXF writer after that module
     was loaded. Read those records without changing the current write route. *)
  If[FileExistsQ[path<>".wxf"],
    r=RGZGaugeFixedCacheRead[path,identity];
    If[!MissingQ[r] && !FailureQ[r] && RGZGaugeValidate[r]===True &&
        If[AssociationQ[r],RGZGaugeBatchPreparedQ[r],!ListQ[r]],Return[r]]];
  Missing[If[FileExistsQ[path],"NoValidBatchCache","NoBatchCache"]]
];

RGZGaugeBatchCacheWrite[x_,path_,identity_] := Module[{r},
  With[{check=RGZGaugeValidate[x]},If[FailureQ[check],Return[check]]];
  If[If[AssociationQ[x],!RGZGaugeBatchPreparedQ[x],ListQ[x]],
    Return[Failure["InvalidGaugeBatchCacheValue",<||>]]];
  If[path===None,Return[True]];
  r=CheckAbort[TimeConstrained[
    RGZMXWrite[<|"Identity"->identity,"ValidationPolicy"->"GaugeFiniteScalar/3","Result"->x|>,path],10,
    Missing["BatchCacheWriteTimeout"]],Missing["BatchCacheWriteAborted"]];
  If[FailureQ[r] || Head[r]===Missing || r===$Failed || !FileExistsQ[path],
    Failure["BatchCacheWriteFailed",<|
      "Description"->"The batch was computed but not cached; combination has stopped."|>],True]
];

RGZGaugeBatchSummary[report_] := Module[{data,keys},
  data=Which[AssociationQ[report],report,FailureQ[report],report[[2]],
    True,Return[Print["BatchResulthead: ",Head[report]]]];
  If[FailureQ[report],Print["BatchStoppedTag: ",report[[1]]]];
  keys=Select[{"Status","ID","CoefficientStatus","TermsCount","initialBatchCount","RadicalTypes",
    "LevelID","BatchID","this levelBatchCount","StoppedOperation","TimeLimit","ElapsedSeconds",
    "NewComputeBatchCount","CacheRestoredBatchCount","Saveto OriginalCoefficientCache","Description"},
    KeyExistsQ[data,#]&];
  Print[Grid[({#,data[#]}& /@ keys),Frame->All,Alignment->Left]]
];

(* The timed-out node is retained in memory without printing its expressions. *)
RGZGaugeInspectBatch[seconds_:10] := Module[{node,inputs,vars,cols,measure,z},
  If[!AssociationQ[rgzGaugeBatchStoppedNode],
    Return[Failure["NoStoppedBatchNode",<||>]]];
  If[!NumericQ[seconds] || !TrueQ[seconds>0],
    Return[Failure["DiagnosticTimeLimit",<||>]]];
  node=rgzGaugeBatchStoppedNode;inputs=node["Input"];vars=node["RootSymbol"];
  measure[e_] := {LeafCount[e],LeafCount[Numerator[e]],
    LeafCount[Denominator[e]],If[vars==={},0,
      Max[Exponent[Numerator[e],#]& /@ vars]],
    If[vars==={},0,Max[Exponent[Denominator[e],#]& /@ vars]]};
  cols=Table[z=CheckAbort[TimeConstrained[measure[inputs[[j]]],seconds,
    Missing["DiagnosticTimeout"]],Missing["DiagnosticAborted"]];
    If[ListQ[z],z,ConstantArray[z,5]],{j,Length[inputs]}];
  Grid[Prepend[Transpose[Prepend[cols,
      {"ExpressionLeafCount","NumeratorLeafCount","DenominatorLeafCount","NumeratorMaxRootPower","DenominatorMaxRootPower"}]],
    Prepend[Table["Input "<>ToString[j],{j,Length[inputs]}],"CheckParts"]],
    Frame->All,Alignment->Left]
];

RGZGaugeSparseSummary[report_] := Module[{data},
  data=Which[AssociationQ[report],report,FailureQ[report],
    Print["SingleNodeStoppedTag: ",report[[1]]];KeyDrop[report[[2]],{"Cause"}],
    True,Return[Print["Resulthead: ",Head[report]]]];
  Print[Grid[({#,data[#]}& /@ Keys[data]),Frame->All,Alignment->Left]]
];

RGZGaugeFixedSummary[report_] := Module[{data,keys},
  data=Which[AssociationQ[report],report,FailureQ[report],
    Print["FixedDenominatorStoppedTag: ",report[[1]]];report[[2]],
    True,Return[Print["Resulthead: ",Head[report]]]];
  keys=Select[{"Status","ID","CoefficientStatus","ResultLeafCount","Source",
    "OriginalTermsCount","CommonDenominatorFactorCount","PoleSquaredStructureCount","InputFileVerify",
    "CompletedgroupCount","GroupID","TimeLimit","ElapsedSeconds","Saveto OriginalCoefficientCache","Description"},
    KeyExistsQ[data,#]&];
  Print[Grid[({#,data[#]}& /@ keys),Frame->All,Alignment->Left]]
];

If[!AssociationQ[rgzGaugeFixedCacheSettings],
  rgzGaugeFixedCacheSettings=<|"WriteTimeLimit"->120,"ReadTimeLimit"->60|>];

Options[RGZGaugeFixedSetCache]={"WriteTimeLimit"->120,"ReadTimeLimit"->60};

RGZGaugeFixedSetCache[OptionsPattern[]] := Module[{w,r},
  w=OptionValue["WriteTimeLimit"];r=OptionValue["ReadTimeLimit"];
  If[!And@@(NumericQ[#] && TrueQ[#>0]& /@ {w,r}),
    Return[Failure["CacheTimeLimitArguments",<||>]]];
  rgzGaugeFixedCacheSettings=<|"WriteTimeLimit"->w,"ReadTimeLimit"->r|>;
  <|"Status"->"Configured","CacheFormat"->"WXF","WriteTimeLimit"->w,"ReadTimeLimit"->r|>
];

RGZGaugeFixedValidPacketQ[r_,identity_] := AssociationQ[r] &&
  Lookup[r,"Identity",None]===identity && KeyExistsQ[r,"Result"] &&
  Lookup[r,"ValidationPolicy",None]==="FixedFiniteScalar/2" &&
  Lookup[r,"ContentHash",None]===Hash[r["Result"],"SHA256"] &&
  RGZGaugeValidate[r["Result"]]===True;

RGZGaugeFixedCacheRead[path_,identity_] := Module[{r,binary,limit},
  If[!StringQ[path],Return[Missing["NoBatchCache"]]];
  limit=rgzGaugeFixedCacheSettings["ReadTimeLimit"];
  binary=path<>".wxf";
  If[FileExistsQ[binary],
    r=CheckAbort[TimeConstrained[
      Quiet[Check[Import[binary,"WXF"],$Failed]],limit,
      Missing["BinaryCacheReadTimeout"]],Missing["BinaryCacheReadAborted"]];
    If[RGZGaugeFixedValidPacketQ[r,identity],Return[r["Result"]]]];
  (* Text checkpoints from v1.1 and older remain valid; never delete them. *)
  If[!FileExistsQ[path],Return[Missing["NoBatchCache"]]];
  r=CheckAbort[TimeConstrained[Quiet[Check[Get[path],$Failed]],limit,
    Missing["LegacyCacheReadTimeout"]],Missing["LegacyCacheReadAborted"]];
  If[RGZGaugeFixedValidPacketQ[r,identity],r["Result"],
    Missing["NoValidBatchCache"]]
];

RGZGaugeFixedCacheFailure[reason_,path_,operation_,limit_,elapsed_,messages_:{}] := Module[{data},
  data=<|"Status"->"SaveFailed","CacheFormat"->"WXF","CauseTag"->reason,
    "StoppedOperation"->operation,"File"->path,"TimeLimit"->limit,"ElapsedSeconds"->elapsed,
    "underlyingMessages"->If[ListQ[messages],
      (ToString[#,InputForm]& /@ Take[messages,Min[8,Length[messages]]]),{}],
    "Description"->"Completed groups remain in memory. Keep the kernel running, fix the save failure and resume from this group."|>;
  rgzGaugeFixedCacheLast=data;
  Failure["BatchCacheWriteFailed",data]
];

RGZGaugeFixedCacheWrite[x_,path_,identity_] := Module[
  {binary,tmp,limit,start=AbsoluteTime[],operation="create CacheDirectory",r,bytes,reason,
    messages={},ioResult},
  With[{check=RGZGaugeValidate[x]},If[FailureQ[check],Return[check]]];
  If[path===None,Return[True]];
  limit=rgzGaugeFixedCacheSettings["WriteTimeLimit"];
  If[!StringQ[path],Return[RGZGaugeFixedCacheFailure[
    "InvalidCachePath",ToString[Head[path]],operation,limit,0]]];
  binary=path<>".wxf";
  tmp=binary<>".tmp-"<>CreateUUID[];
  r=Block[{$MessageList={}},ioResult=CheckAbort[TimeConstrained[
    Quiet[Check[
      If[!DirectoryQ[DirectoryName[binary]],
        CreateDirectory[DirectoryName[binary],CreateIntermediateDirectories->True]];
      operation="Save WXF checkpoint";
      RGZGaugeStep[operation];
      rgzGaugeFixedCacheLast=<|"Status"->"SaveIn","CacheFormat"->"WXF",
        "StoppedOperation"->operation,"File"->binary,"TimeLimit"->limit|>;
      If[!StringQ[Export[tmp,<|"Format"->"RGZFixedBatchWXF-v1",
          "Identity"->identity,"ValidationPolicy"->"FixedFiniteScalar/2",
          "ContentHash"->Hash[x,"SHA256"],"Result"->x|>,"WXF"]],
        $Failed,
        operation="Commit WXF checkpoint";
        bytes=FileByteCount[tmp];
        If[!IntegerQ[bytes] || bytes<=0,$Failed,
          RenameFile[tmp,binary,OverwriteTarget->True]]],
      $Failed]],limit,Missing["BinaryCacheWriteTimeout"]],
    Missing["BinaryCacheWriteAborted"]];messages=$MessageList;ioResult];
  If[!StringQ[r] || !FileExistsQ[binary],
    If[FileExistsQ[tmp],Quiet[Check[DeleteFile[tmp],Null]]];
    reason=If[Head[r]===Missing,r[[1]],"BinaryCacheWriteIOFailed"];
    Return[RGZGaugeFixedCacheFailure[reason,binary,operation,limit,
      AbsoluteTime[]-start,messages]]];
  rgzGaugeFixedCacheLast=<|"Status"->"SaveComplete","CacheFormat"->"WXF",
    "File"->binary,"Size (MiB)"->N[bytes/2^20],
    "ElapsedSeconds"->AbsoluteTime[]-start,"TimeLimit"->limit|>;
  True
];

Options[RGZGaugeFixedSaveProgress]={"CacheDirectory"->Automatic};

RGZGaugeFixedSaveProgress[OptionsPattern[]] := Module[{root,id,path,r},
  If[!AssociationQ[rgzGaugeFixedWork] ||
      !And@@(KeyExistsQ[rgzGaugeFixedWork,#]& /@
        {"Identity","Prepare","Numerator","CompletegroupCount"}),
    Return[Failure["NoInMemoryFixedCheckpoint",<|
      "Description"->"No pending group checkpoints in memory; resume using disk caches."|>]]];
  root=RGZGaugeCacheDirectory[OptionValue["CacheDirectory"]];
  If[!StringQ[root],Return[Failure["CacheDirectoryUnavailable",<||>]]];
  id=rgzGaugeFixedWork["Identity"];
  path=FileNameJoin[{root,"fixed_"<>IntegerString[id,16],"checkpoint.wl"}];
  r=RGZGaugeBatchCacheWrite[rgzGaugeFixedWork,path,{id,"fixed"}];
  If[FailureQ[r],r,<|"Status"->"in-memory progressSave",
    "CompletedgroupCount"->rgzGaugeFixedWork["CompletegroupCount"],"CacheFormat"->"WXF"|>]
];

RGZGaugeFixedCacheInfo[] := Module[{keys,data},
  If[!AssociationQ[rgzGaugeFixedCacheLast],
    Return[Print["No current-format cache writes are recorded in this kernel."]]];
  data=rgzGaugeFixedCacheLast;
  keys=Select[{"Status","CacheFormat","CauseTag","StoppedOperation","File",
    "Size (MiB)","TimeLimit","ElapsedSeconds","underlyingMessages","Description"},KeyExistsQ[data,#]&];
  Grid[({#,data[#]}& /@ keys),Frame->All,Alignment->Left]
];

RGZGaugeFixedFinalInfo[] := Module[{d,keys},
  If[!AssociationQ[rgzGaugeFixedCancelWork],
    Return[Print["No factor cancellation records exist in this kernel."]]];
  d=rgzGaugeFixedCancelWork;
  keys={"FactorID","TotalFactorCount","CancelledPower","CheckCount"};
  Grid[Prepend[({#,d[#]}& /@ keys),{"Finalcancellation progress","Value"}],
    Frame->All,Alignment->Left]
];
