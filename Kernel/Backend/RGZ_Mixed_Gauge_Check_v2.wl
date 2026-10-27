(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* RGZ mixed one-loop insertions: external A-A assembly and gauge residual.
   Prerequisites: FeynCalc loaded; RGZ_Mixed_OneLoop_Overall.wl loaded;
   rgzMixed successfully restored/assembled. Loading defines functions only.
   Tree kernels are the Minkowski kernels already checked for this model:
   DT=i/KT, DL=i/KL; mG^4=2 Nc gs^2 gamma^4.
   Self-energy insertions retain the original diagram i factor. No extra
   i or symmetry factor is introduced. The result contains NO new pinch
   contributions; a complete S-matrix/PT proof needs those separately.
   Keep D and all RGZ masses. Remove M only after summing the full target.
*)
ClearAll[RGZGaugeStart,RGZGaugePVFreeQ,RGZGaugeCanonicalPV,
  RGZGaugePVPairs,RGZGaugeCompare,RGZGaugeSummary,RGZGaugeShow,
  RGZGaugeStep,RGZGaugeState,RGZGaugePrepare,RGZGaugePoleSquares,
  RGZGaugeCoefficient,RGZGaugeCacheRead,RGZGaugeCacheWrite,
  RGZGaugeCacheDirectory,RGZGaugeProgressSummary,RGZGaugeStoppedInfo,
  RGZGaugeInspectStopped,RGZGaugeBatchPrepare,RGZGaugeBatchReduce,
  RGZGaugeBatchCacheRead,RGZGaugeBatchCacheWrite,RGZGaugeResumeStopped,
  RGZGaugeBatchSummary,RGZGaugeBatchChunks,RGZGaugeBatchRootReduce,
  RGZGaugeBatchMerge,RGZGaugeInspectBatch,RGZGaugeSparseGroup,
  RGZGaugeSparseFrom,RGZGaugeSparseTimes,RGZGaugeSparsePower,
  RGZGaugeSparseReduce,RGZGaugeSparseTo,RGZGaugeSparseTidy,
  RGZGaugeMergeContext,RGZGaugeSparseMergeCore,RGZGaugeTestStoppedMerge,
  RGZGaugeSparseSummary];

RGZGaugeStep[label_String] := (
  RGZMXProgressUpdate["Substep"->label,"OperationStartTime"->AbsoluteTime[]];
  If[AssociationQ[rgzGaugeProgress],AssociateTo[rgzGaugeProgress,
    {"CurrentOperation"->label,"OperationStartTime"->AbsoluteTime[]}]];Null);

(* The external A row/column of the tree propagators. In the T basis,
   DT[A,i]=dAA u_i. In the L basis the left/right signs differ: no
   transpose/Hermitian-conjugation shortcut is used. *)
RGZGaugeStart[data_Association] := Module[
  {piT,piL,s0,k,h,delta,den,u,dAA,rowL,colL,termsT,termsL},
  If[!And@@(KeyExistsQ[data,#]&/@{"T","L","TFieldOrder","LFieldOrder"}),
    Return[Failure["MissingMixedMatrices",<||>]]];
  If[data["TFieldOrder"]=!=RGZMixBasis["T"] ||
      data["LFieldOrder"]=!=RGZMixBasis["L"],
    Return[Failure["MixedBasisMismatch",<|
      "Description"->"Use the checked kernel field order and normalized adjoint color basis."|>]]];
  piT=data["T"];piL=data["L"];
  If[Dimensions[piT]=!={5,5} || Dimensions[piL]=!={8,8} ||
      !FreeQ[{piT,piL},_Failure|_Missing|$Failed|$Aborted],
    Return[Failure["IncompleteMixedMatrices",<||>]]];
  If[OwnValues[\[Alpha]]=!={},Return[Failure["AssignedGaugeParameter",
    <|"Description"->"Keep Alpha symbolic; a fixed-gauge result cannot establish gauge independence."|>]]];
  s0=SPD[p,p];k=s0-\[Mu]^2;h=mG^2/2;
  delta=(s0-m^2)(s0-\[Mu]^2)+mG^4;
  u={1,-h/k,-h/k,h/k,h/k};dAA=-I k/delta;
  den=s0^2+\[Alpha] M^4;
  rowL={-I \[Alpha] s0/den,-s0 Sqrt[s0]/den,
    -\[Alpha] M^4/(Sqrt[s0] den),-\[Alpha] Sqrt[s0]/den,0,0,0,0};
  colL={-I \[Alpha] s0/den,s0 Sqrt[s0]/den,
    \[Alpha] M^4/(Sqrt[s0] den),\[Alpha] Sqrt[s0]/den,0,0,0,0};
  (* Use every ordered pair once; graph symmetry factors are already in Pi. *)
  termsT=Flatten[Table[u[[i]] piT[[i,j]] u[[j]],{i,5},{j,5}]];
  termsL=Flatten[Table[rowL[[i]] piL[[i,j]] colL[[j]],{i,4},{j,4}]];
  <|"TFieldOrder"->data["TFieldOrder"],"LFieldOrder"->data["LFieldOrder"],
    "TreeTransverseAA"->dAA,"TransverseExternalLegsweight"->u,
    "LongitudinalLeftExternalLegs"->rowL,"LongitudinalRightExternalLegs"->colL,
    "SelfEnergyTParts"->termsT,"SelfEnergyT"->Total[termsT],
    "AttachedT"->dAA^2 Total[termsT],"AttachedL"->Total[termsL],
    "Description"->"SelfEnergyT=u.PiT.u; attached T/L=(D0.Pi.D0)AA; pinch terms are excluded."|>
];
RGZGaugeStart[x_] := Failure["RestoreMixedResultFirst",<||>];

(* Scalar PV objects are opaque while coefficients are manipulated.
   Bubble symmetry is exact; general C0/D0 permutations are NOT guessed. *)
RGZGaugePVFreeQ[x_] := FreeQ[x,
  _PaVe|_GenPaVe|_A0|_A00|_B0|_B1|_B00|_B11|_C0|_D0];
RGZGaugeCanonicalPV[x_] := (x /. {
  HoldPattern[PaVe[0,{}, {a_},opts___Rule]] :> A0[a],
  HoldPattern[PaVe[0,{v_},{a_,b_},opts___Rule]] :> B0[v,a,b]
}) /. {
  HoldPattern[B0[v_,a_,b_]] :> With[{ab=Sort[{a,b}]},B0[v,ab[[1]],ab[[2]]]]
};

RGZGaugePVPairs[expr_] := Module[{walk,group,multiply,pairs,stop},Catch[
  group[ps_List] := Select[
    ({#[[1,1]],Total[#[[All,2]]]}& /@ GatherBy[ps,First]),
    !TrueQ[#[[2]]===0]&];
  multiply[left_List,right_List] := Flatten[Table[
    If[a[[1]]=!=1 && b[[1]]=!=1,
      Throw[Failure["NonlinearOneLoopPV",<||>],stop]];
    {a[[1]] b[[1]],a[[2]] b[[2]]},{a,left},{b,right}],1];
  walk[x_] := Which[
    RGZGaugePVFreeQ[x],{{1,x}},
    RGZMXPVObjectQ[x],
      If[!RGZMXPVScalarQ[x],Throw[Failure["TensorPVRemaining",<||>],stop]];
      {{RGZGaugeCanonicalPV[x],1}},
    Head[x]===Plus,Flatten[walk/@(List@@x),1],
    Head[x]===Times,Fold[multiply,{{1,1}},walk/@(List@@x)],
    True,Throw[Failure["UnsupportedPVStructure",
      <|"head"->RGZLoopName[Head[x]]|>],stop]];
  pairs=walk[expr];group[pairs],stop]
];

(* First split PV functions with SHORT pole labels. Do not substitute
   square-root pole masses into the entire large amplitude. A list input
   is a SUM of contributions, never a list of separate zero tests.
   Residuals are grouped across all inputs before testing cancellation.
   Each prepared row is {physical PV key, alpha coefficient in short
   labels, reference coefficient in short labels}. Only the small PV
   arguments are converted here; coefficients are converted later. *)
RGZGaugePrepare[expr_,reference_,seconds_] := Module[
  {inputs,batches={},raw,z,groups,out={},key,refkey,j,stop},Catch[
  inputs=If[ListQ[expr],expr,{expr}];
  RGZMXProgressUpdate["TermID"->0,"TotalTermCount"->Length[inputs],"CompleteTermCount"->0];
  Do[
    RGZMXProgressUpdate["TermID"->j,"CompleteTermCount"->j-1];
    RGZGaugeStep["separate MasterIntegral: Input "<>ToString[j]<>" / "<>ToString[Length[inputs]]];
    z=CheckAbort[TimeConstrained[RGZGaugePVPairs[inputs[[j]]],seconds,
      Missing["PreparationTimeout"]],$Aborted];
    If[Head[z]===Missing,Throw[Failure["GaugePreparationTimeout",<|
      "Stage"->"Retain pole labels and separate master integrals","InputParts"->j,
      "TimeLimit"->seconds,"Description"->"No gauge residual was obtained; gauge independence cannot be assessed."|>],stop]];
    If[FailureQ[z],Throw[z,stop]];
    If[z===$Aborted,Throw[Failure["GaugeCheckAborted",<|
      "Stage"->"separate MasterIntegral","InputParts"->j|>],stop]];
    AppendTo[batches,z];
    RGZMXProgressUpdate["CompleteTermCount"->j],{j,Length[inputs]}];
  RGZGaugeStep["Combine matching master integrals across inputs"];
  raw=({#[[1,1]],Total[#[[All,2]]]}& /@
    GatherBy[Flatten[batches,1],First]);
  RGZMXProgressUpdate["TermID"->0,"TotalTermCount"->Length[raw],"CompleteTermCount"->0];
  Do[
    RGZMXProgressUpdate["TermID"->j,"CompleteTermCount"->j-1];
    RGZGaugeStep["Gaugesubstitution: MasterIntegralParameter "<>ToString[j]<>" / "<>ToString[Length[raw]]];
    key=RGZGaugeCanonicalPV[RGZLoopPoleMasses[raw[[j,1]]]];
    refkey=RGZGaugeCanonicalPV[key/.\[Alpha]->reference];
    AppendTo[out,{key,raw[[j,2]],0}];
    AppendTo[out,{refkey,0,raw[[j,2]]}];
    RGZMXProgressUpdate["CompleteTermCount"->j],{j,Length[raw]}];
  groups=GatherBy[out,First];
  ({#[[1,1]],Total[#[[All,2]]],Total[#[[All,3]]]}& /@ groups),stop]
];

RGZGaugeState[x_] := Which[
  Head[x]===Missing,"Timeout",FailureQ[x] || x===$Failed || x===$Aborted,"Failed",
  TrueQ[x===0],"Zero",True,"Nonzero"];

(* v2: exact algebra with compact square-root symbols. No PowerExpand,
   numeric mass substitution, D->4, M->0 or gauge-independent pole assumption.
   Even powers of a pole label are replaced using its squared mass FIRST.
   Every remaining square root keeps its original principal-root definition.
   Polynomial remainders use only r^2 = radicand. This is a sufficient exact
   zero test; no converse/linear independence of all master functions assumed. *)
RGZGaugePoleSquares[x_] := Module[{labels,squares,rules},
  labels={rgzLoopM1,rgzLoopM2,rgzLoopM3,rgzLoopM4};
  squares=RGZLoopPoleMasses[#^2]& /@ labels;
  rules=MapThread[Function[{label,square},With[{l=label,v=square},
    HoldPattern[Power[l,n_Integer]] /; EvenQ[n] :> v^(n/2)]],{labels,squares}];
  RGZLoopPoleMasses[x/.rules]
];

RGZGaugeCoefficient[a_,b_,reference_] := Module[{rgzFunctionReturnTag=Unique["RGZWorkflow`Private`functionReturn"]},Catch[
Module[
  {ca,cb,bases,vars,rules,definitions,radicands,z,num,den,j,physical},
  RGZGaugeStep["PoleSquaredsubstitution"];
  ca=RGZGaugePoleSquares[a];cb=RGZGaugePoleSquares[b]/.\[Alpha]->reference;
  If[ca===cb,Return[<|"ResidualCoefficient"->0,"RadicalSymbolCoefficient"->0,"RadicalDefinition"->{}|>]];
  RGZGaugeStep["Radicaluse instead ShortSymbol"];
  bases=DeleteDuplicates[Cases[{ca,cb},
    HoldPattern[Power[x_,n_Rational]] /; Denominator[n]===2 :> x,Infinity]];
  If[bases==={},z=Cancel[Together[ca-cb]];
    Return[<|"ResidualCoefficient"->z,"RadicalSymbolCoefficient"->z,"RadicalDefinition"->{}|>]];
  (* Outer radicals precede the radicals in their radicands. *)
  bases=SortBy[bases,-Depth[#]&];
  vars=Table[Unique["rgzGaugeRoot$"],{Length[bases]}];
  rules=MapThread[Function[{base,var},With[{bb=base,vv=var},
    HoldPattern[Power[bb,n_Rational]] /; Denominator[n]===2 :>
      vv^Numerator[n]]],{bases,vars}];
  definitions=MapThread[Rule,{vars,Sqrt /@ bases}];
  radicands=bases/.rules;
  RGZGaugeStep["ShortSymbolRationalExpressionTogether"];
  z=Cancel[Together[(ca/.rules)-(cb/.rules)]];
  If[TrueQ[z===0],Return[<|"ResidualCoefficient"->0,"RadicalSymbolCoefficient"->0,
    "RadicalDefinition"->definitions|>]];
  num=Numerator[z];den=Denominator[z];
  RGZGaugeStep["VerifyRadicalSquaredrelation"];
  Do[
    If[!FreeQ[radicands[[j]],vars[[j]]],
      Throw[Failure["CyclicRootDefinition",<||>],rgzFunctionReturnTag]];
    If[PolynomialQ[num,vars[[j]]] && PolynomialQ[den,vars[[j]]],
      If[Exponent[num,vars[[j]]]>=2,
        num=PolynomialRemainder[num,vars[[j]]^2-radicands[[j]],vars[[j]]]];
      If[Exponent[den,vars[[j]]]>=2,
        den=PolynomialRemainder[den,vars[[j]]^2-radicands[[j]],vars[[j]]]]],
    {j,Length[vars]}];
  If[TrueQ[den===0],Return[Failure["PoleRelationSingularity",<|
    "Description"->"The denominator vanishes under the pole relations; this term does not pass."|>]]];
  z=Cancel[Together[num/den]];
  RGZGaugeStep["RestoreRadicalDefinition"];
  physical=z/.definitions;
  <|"ResidualCoefficient"->physical,"RadicalSymbolCoefficient"->z,"RadicalDefinition"->definitions|>
],rgzFunctionReturnTag]];

(* Separate residual cache: the diagram/integral caches are never modified.
   A fingerprint contains BOTH coefficient branches, the physical master key,
   reference gauge, pole definitions and this exact algebra implementation.
   Timed-out/failed rows are not reused as a successful computation. *)
RGZGaugeCacheDirectory[root_] := Module[{r=root,dir},
  If[r===None,Return[None]];
  If[r===Automatic,r=Quiet[Check[RGZWorkflow`Private`cacheDirectory[],None]]];
  If[!StringQ[r],Return[None]];
  dir=FileNameJoin[{r,"RGZ_Gauge_coefficient_cache_v2"}];
  If[!DirectoryQ[dir],Quiet[Check[
    CreateDirectory[dir,CreateIntermediateDirectories->True],Return[None]]]];
  dir
];
RGZGaugeCacheRead[path_String,key_] := Module[{r},
  If[!FileExistsQ[path],Return[Missing["NoCache"]]];
  r=Quiet[Check[Get[path],$Failed]];
  If[AssociationQ[r] && Lookup[r,"Identity",None]===key &&
      AssociationQ[Lookup[r,"Result",None]] &&
      MemberQ[{"Zero","Nonzero"},Lookup[r["Result"],"Status",None]],
    r["Result"],Missing["NoValidCache"]]
];
RGZGaugeCacheWrite[row_,path_,key_] := Module[{r},
  If[path===None,Return[Null]];
  r=CheckAbort[TimeConstrained[
    RGZMXWrite[<|"Identity"->key,"Result"->row|>,path],10,
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

(* Compare a SYMBOLIC alpha with a fixed reference (default 1). Pole masses
   are inserted BEFORE alpha replacement, both in coefficients AND PV
   arguments. This prevents a false pass from treating M1/M2 as constants.
   Only coefficients of identical PV objects are simplified; no large
   FullSimplify. A zero result is a sufficient exact check of the supplied
   expression. Nonzero formal PV coefficients can still require integral
   identities or the final IR-regulator limit; they are not a theorem of
   physical gauge dependence. *)
Options[RGZGaugeCompare]={"PreprocessTimeLimit"->300,"CacheDirectory"->Automatic,
  "Timeoutstop immediately"->True};
RGZGaugeCompare[expr_,reference_:1,seconds_:30,OptionsPattern[]] := Module[
  {prep,pairs,rows,c,stats,state,n,unresolved,abort,prepareSeconds,
    result,row,cacheDir,path,key,context,time,details,stopTimeout,operation},
  prepareSeconds=OptionValue["PreprocessTimeLimit"];
  stopTimeout=OptionValue["Timeoutstop immediately"];
  If[!FreeQ[reference,\[Alpha]] || !NumericQ[reference] ||
      !TrueQ[reference>0] || !NumericQ[seconds] || !TrueQ[seconds>0] ||
      !NumericQ[prepareSeconds] || !TrueQ[prepareSeconds>0] ||
      !MemberQ[{True,False},stopTimeout],
    Return[Failure["GaugeComparisonArguments",<|
      "Description"->"Use a positive reference gauge, such as 1, and positive simplification and preprocessing time limits."|>]]];
  If[OwnValues[\[Alpha]]=!={},Return[Failure["AssignedGaugeParameter",<||>]]];
  If[!FreeQ[expr,_Failure|_Missing|$Failed|$Aborted|_TID|_PaVeReduce|
      _FeynAmpDenominator|_LorentzIndex|_ExplicitLorentzIndex|_DiracGamma|
      _SUNIndex|_SUNF|_SUND|_SUNDelta],
    Return[Failure["NeedIntegratedColourProjectedScalar",<||>]]];
  unresolved=Cases[expr,x_ /; RGZMXPVObjectQ[x] && !RGZMXPVScalarQ[x],{0,Infinity}];
  If[unresolved=!={},Return[Failure["TensorPVRemaining",
    <|"UnreducedFunctionCount"->Length[DeleteDuplicates[unresolved]]|>]]];
  RGZMXProgressUpdate["Sector"->"RGZ GaugeResidualCheck","Stage"->"GaugeResidual",
    "DiagramID"->0,"DiagramCount"->0,"TermID"->0,"TotalTermCount"->0,"CompleteTermCount"->0,
    "Status"->"ComputeIn","StageStartTime"->AbsoluteTime[]];
  RGZGaugeStep["Retain pole labels and separate master integrals"];
  prep=CheckAbort[TimeConstrained[
    RGZGaugePrepare[expr,reference,prepareSeconds],prepareSeconds,
    Failure["GaugePreparationTimeout",<|
      "Stage"->"Separate and combine master integrals, then substitute gauge parameters","TimeLimit"->prepareSeconds,
      "Description"->"Preprocessing reached the total time limit; provide SelfEnergyTParts and inspect the current substep."|>]],
    Failure["GaugeCheckAborted",<|"Stage"->"Preprocess"|>]];
  If[FailureQ[prep] || Head[prep]===Missing || prep===$Aborted,
    RGZMXProgressUpdate["Status"->"Incomplete"];Return[prep]];
  pairs=prep;
  cacheDir=RGZGaugeCacheDirectory[OptionValue["CacheDirectory"]];
  context=Hash[{"RGZGaugeCoefficient-v2",DownValues[RGZGaugeCoefficient],
    DownValues[RGZGaugePoleSquares],DownValues[RGZLoopPoleMasses]},"SHA256"];
  rgzGaugeProgress=<|"CompletedTermCount"->0,"MasterIntegralTermCount"->Length[pairs],
    "Statistics"-><||>,"CurrentTerm"->0,"CacheDirectory"->cacheDir|>;
  rgzGaugeCompletedRows={};rgzGaugeStoppedUnit=Missing["NoStoppedCoefficient"];
  RGZMXProgressUpdate["TotalTermCount"->Length[pairs],"CompleteTermCount"->0];
  rows=Catch[Table[
    RGZMXProgressUpdate["TermID"->n,"CompleteTermCount"->n-1];
    AssociateTo[rgzGaugeProgress,"CurrentTerm"->n];
    path=None;key=None;
    {time,row}=AbsoluteTiming[CheckAbort[TimeConstrained[
      RGZGaugeStep["CheckCoefficientCache"];
      key=Hash[{context,reference,pairs[[n]]},"SHA256"];
      If[StringQ[cacheDir],path=FileNameJoin[{cacheDir,
        "coefficient_"<>IntegerString[key,16]<>".wl"}]];
      result=If[path===None,Missing["NoCache"],RGZGaugeCacheRead[path,key]];
      If[AssociationQ[result],
        Join[result,<|"ID"->n,"Source"->"CacheRestored"|>],
        result=RGZGaugeCoefficient[pairs[[n,2]],pairs[[n,3]],reference];
        If[FailureQ[result],
          <|"ID"->n,"MasterIntegral"->pairs[[n,1]],"OriginalCoefficient"->Missing["NotStored"],
            "ResidualCoefficient"->result,"Status"->"Failed","Source"->"current run Compute"|>,
          Join[<|"ID"->n,"MasterIntegral"->pairs[[n,1]],"OriginalCoefficient"->Missing["UseShortCoefficients"],
            "Status"->RGZGaugeState[result["ResidualCoefficient"]],"Source"->"current run Compute"|>,result]]],
      seconds,<|"ID"->n,"MasterIntegral"->pairs[[n,1]],"OriginalCoefficient"->Missing["NotPrepared"],
        "ResidualCoefficient"->Missing["CoefficientTimeout"],"Status"->"Timeout",
        "Source"->"current run Compute","StoppedOperation"->Lookup[rgzGaugeProgress,"CurrentOperation",""]|>],$Aborted]];
    If[row===$Aborted,Throw[Failure["GaugeCheckAborted",
      <|"CompletedTermCount"->n-1,"Description"->"Completed coefficients saved; comparison with the same input restores the cache."|>],abort]];
    row=Join[row,<|"ElapsedSeconds"->time|>];
    If[row["Status"]==="Timeout" && TrueQ[stopTimeout],
      operation=Lookup[row,"StoppedOperation",Lookup[rgzGaugeProgress,"CurrentOperation",""]];
      (* Keep both short-label branches in memory for one-unit diagnostics.
         Do not spend more time serializing a huge failed input before stopping.
         Successful earlier coefficients are already in the unchanged cache. *)
      rgzGaugeStoppedUnit=<|"ID"->n,"TotalTermCount"->Length[pairs],
        "MasterIntegral"->pairs[[n,1]],"AlphaShortCoefficient"->pairs[[n,2]],
        "ReferenceShortCoefficient"->pairs[[n,3]],"ReferenceGauge"->reference,
        "TimeLimit"->seconds,"ElapsedSeconds"->time,"StoppedOperation"->operation,
        "CompletedTermCount"->n-1,"CachePath"->path,"CacheIdentity"->key|>;
      AssociateTo[rgzGaugeProgress,{"CompletedTermCount"->n-1,
        "CurrentTermElapsedSeconds"->time,"CurrentOperation"->operation,
        "Statistics"->Counts[(#["Status"]& /@ rgzGaugeCompletedRows)]}];
      RGZMXProgressUpdate["Status"->"TimeoutStopped","CompleteTermCount"->n-1];
      Throw[Failure["GaugeCoefficientTimeout",<|
        "ID"->n,"TotalTermCount"->Length[pairs],"TimeLimit"->seconds,
        "ElapsedSeconds"->time,"StoppedOperation"->operation,"CompletedTermCount"->n-1,
        "Description"->"Stopped at the first coefficient timeout. Later terms are unchecked; completed coefficients are cached and the stopped input is in rgzGaugeStoppedUnit."|>],abort]];
    AppendTo[rgzGaugeCompletedRows,row];
    AssociateTo[rgzGaugeProgress,{"CompletedTermCount"->n,
      "CurrentTermElapsedSeconds"->time,"Statistics"->Counts[(#["Status"]& /@ rgzGaugeCompletedRows)]}];
    If[MemberQ[{"Zero","Nonzero"},row["Status"]] && row["Source"]=!="CacheRestored",
      RGZGaugeStep["SaveCompletedCoefficient"];RGZGaugeCacheWrite[row,path,key]];
    RGZMXProgressUpdate["CompleteTermCount"->n];row,
    {n,Length[pairs]}],abort];
  If[FailureQ[rows],RGZMXProgressUpdate["Status"->
    If[rows[[1]]==="GaugeCoefficientTimeout","TimeoutStopped","Aborted"]];Return[rows]];
  stats=Counts[(#["Status"]& /@ rows)];
  state=If[AllTrue[rows,#["Status"]==="Zero"&],
    "The checked expression has zero gauge residual","NotYetproofGaugeIndependent"];
  RGZMXProgressUpdate["Status"->If[state==="The checked expression has zero gauge residual","Complete","CheckComplete, pending Term"]];
  <|"ReferenceGauge"->reference,"MasterIntegralTermCount"->Length[rows],"Statistics"->stats,
    "Status"->state,"ResidualParts"->rows,
    "Description"->"Compare alpha with the reference gauge, including alpha-dependent pole masses, at finite M. Pinch terms are not added and M->0 is not taken automatically."|>
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

(* v2.4: resumable exact reduction of ONE stopped coefficient.
   No separate-batch zero tests: all nonzero batches are merged pairwise.
   Per-batch and whole-call budgets are separate. Existing coefficient,
   graph and integration cache formats/fingerprints stay unchanged. *)
RGZGaugeBatchChunks[x_List,n_Integer] :=
  Table[Take[x,{j,Min[j+n-1,Length[x]]}],{j,1,Length[x],n}];

RGZGaugeBatchPrepare[a_,b_,reference_,size_Integer] := Module[
  {ca,cb,bases,vars,rules,definitions,radicands,terms,grouped,chunks},
  RGZGaugeStep["Batch preparation: squared poles and common radical basis"];
  ca=RGZGaugePoleSquares[a];cb=RGZGaugePoleSquares[b]/.\[Alpha]->reference;
  bases=DeleteDuplicates[Cases[{ca,cb},
    HoldPattern[Power[x_,n_Rational]] /; Denominator[n]===2 :> x,Infinity]];
  bases=SortBy[bases,-Depth[#]&];
  (* Stable private symbols allow intermediate caches to survive Get/restarts.
     Reject assigned symbols rather than silently changing their meaning. *)
  vars=Table[Symbol["RGZGaugeBatchAlgebra`r"<>ToString[j]],{j,Length[bases]}];
  If[AnyTrue[vars,Function[v,OwnValues[v]=!={},HoldAll]],
    Return[Failure["AssignedBatchRootSymbol",<||>]]];
  rules=MapThread[Function[{base,var},With[{bb=base,vv=var},
    HoldPattern[Power[bb,n_Rational]] /; Denominator[n]===2 :> vv^Numerator[n]]],
    {bases,vars}];
  definitions=Table[vars[[j]]->Sqrt[bases[[j]]],{j,Length[vars]}];
  radicands=bases/.rules;
  If[AnyTrue[Range[Length[vars]],!FreeQ[radicands[[#]],vars[[#]]]&],
    Return[Failure["CyclicRootDefinition",<||>]]];
  terms=Join[If[Head[ca]===Plus,List@@ca,{ca}],
    (-#& /@ If[Head[cb]===Plus,List@@cb,{cb}])]/.rules;
  (* Denominator inspects individual summands; no full-amplitude Together.
     Identical syntactic denominators are neighbours before bounded chunks. *)
  terms=DeleteCases[terms,0];grouped=GatherBy[terms,Denominator];
  chunks=Flatten[(RGZGaugeBatchChunks[#,size]& /@ grouped),1];
  <|"Batch"->chunks,"RootSymbol"->vars,"RadicalDefinition"->definitions,
    "Radicand"->radicands,"TermsCount"->Length[terms],
    "same DenominatorgroupCount"->Length[grouped],"initialBatchCount"->Length[chunks]|>
];

RGZGaugeBatchReduce[x_,vars_List,radicands_List] := Module[{z,num,den,j},
  z=Cancel[Together[x]];
  If[TrueQ[z===0],Return[0]];
  num=Numerator[z];den=Denominator[z];
  Do[
    If[PolynomialQ[num,vars[[j]]] && PolynomialQ[den,vars[[j]]],
      If[Exponent[num,vars[[j]]]>=2,
        num=PolynomialRemainder[num,vars[[j]]^2-radicands[[j]],vars[[j]]]];
      If[Exponent[den,vars[[j]]]>=2,
        den=PolynomialRemainder[den,vars[[j]]^2-radicands[[j]],vars[[j]]]]],
    {j,Length[vars]}];
  If[TrueQ[den===0],Return[Failure["PoleRelationSingularity",<|
    "Description"->"Radical square relations make the denominator zero; this batch is not a valid cancellation result."|>]]];
  Cancel[Together[num/den]]
];

RGZGaugeBatchCacheRead[path_,identity_] := Module[{r},
  If[!StringQ[path] || !FileExistsQ[path],Return[Missing["NoBatchCache"]]];
  r=Quiet[Check[Get[path],$Failed]];
  If[AssociationQ[r] && Lookup[r,"Identity",None]===identity &&
      KeyExistsQ[r,"Result"] && FreeQ[r["Result"],_Failure|_Missing|$Failed|$Aborted],
    r["Result"],Missing["NoValidBatchCache"]]
];
RGZGaugeBatchCacheWrite[x_,path_,identity_] := Module[{r},
  If[path===None,Return[True]];
  r=CheckAbort[TimeConstrained[
    RGZMXWrite[<|"Identity"->identity,"Result"->x|>,path],10,
    Missing["BatchCacheWriteTimeout"]],Missing["BatchCacheWriteAborted"]];
  If[FailureQ[r] || Head[r]===Missing || r===$Failed || !FileExistsQ[path],
    Failure["BatchCacheWriteFailed",<|
      "Description"->"The batch was computed but not cached; combination has stopped."|>],True]
];

Options[RGZGaugeResumeStopped]={"GroupSize"->16,"PrepareTimeLimit"->60,
  "TotalTimeLimit"->1200,"CacheDirectory"->Automatic};
RGZGaugeResumeStopped[seconds_:60,OptionsPattern[]] := Module[
  {unit,size,prepLimit,totalLimit,root,identity,dir=None,prep,prepPath,
    vars,radicands,definitions,chunks,current,next,level=0,groups,
    run,worker,result,z,physical,row,path,key,cacheSaved=False,stop,
    start=AbsoluteTime[],resumeCount=0,computedCount=0,operation,n,
    mergeContext,mergeDir=None,legacyPrefix=True},
  If[!AssociationQ[rgzGaugeStoppedUnit],
    Return[Failure["NoStoppedCoefficient",<||>]]];
  unit=rgzGaugeStoppedUnit;size=OptionValue["GroupSize"];
  rgzGaugeBatchStoppedNode=Missing["NoStoppedBatchNode"];
  rgzGaugeBatchCurrentNode=Missing["NoCurrentBatchNode"];
  prepLimit=OptionValue["PrepareTimeLimit"];totalLimit=OptionValue["TotalTimeLimit"];
  If[!IntegerQ[size] || size<1 ||
      !And@@(NumericQ[#] && TrueQ[#>0]& /@ {seconds,prepLimit,totalLimit}),
    Return[Failure["GaugeBatchArguments",<||>]]];
  If[!And@@(KeyExistsQ[unit,#]& /@
      {"AlphaShortCoefficient","ReferenceShortCoefficient","ReferenceGauge","ID","MasterIntegral"}),
    Return[Failure["IncompleteStoppedInput",<||>]]];
  If[OwnValues[\[Alpha]]=!={},
    Return[Failure["AssignedGaugeParameter",<||>]]];
  rgzGaugeBatchProgress=<|"ID"->unit["ID"],"Status"->"ComputeIn",
    "LevelID"->0,"BatchID"->0,"this levelBatchCount"->0,"CacheRestoredBatchCount"->0,
    "NewComputeBatchCount"->0,"SingleBatchTimeLimit"->seconds,"TotalTimeLimit"->totalLimit|>;
  RGZMXProgressUpdate["Sector"->"RGZ GaugeCoefficientBatchCheck","Stage"->"BatchReduce",
    "StageStartTime"->start,"Status"->"ComputeIn","DiagramID"->0,"DiagramCount"->0,
    "TermID"->0,"TotalTermCount"->0,"CompleteTermCount"->0];
  worker[] := Catch[
    RGZGaugeStep["Batch preparation: check input identity and cache"];
    root=RGZGaugeCacheDirectory[OptionValue["CacheDirectory"]];
    identity=Hash[{"RGZGaugeBatch-v2.4",size,
      unit["AlphaShortCoefficient"],unit["ReferenceShortCoefficient"],unit["ReferenceGauge"],
      DownValues[RGZGaugeBatchPrepare],DownValues[RGZGaugeBatchReduce],
      DownValues[RGZGaugeBatchChunks],
      DownValues[RGZGaugePoleSquares],DownValues[RGZLoopPoleMasses]},"SHA256"];
    If[StringQ[root],dir=FileNameJoin[{root,"batch_"<>IntegerString[identity,16]}];
      If[!DirectoryQ[dir],Quiet[Check[
        CreateDirectory[dir,CreateIntermediateDirectories->True],
        Throw[Failure["BatchCacheDirectory",<||>],stop]]]]];
    prepPath=If[StringQ[dir],FileNameJoin[{dir,"prepare.wl"}],None];
    prep=RGZGaugeBatchCacheRead[prepPath,{identity,"prepare"}];
    If[!AssociationQ[prep],
      prep=TimeConstrained[
        RGZGaugeBatchPrepare[unit["AlphaShortCoefficient"],unit["ReferenceShortCoefficient"],
          unit["ReferenceGauge"],size],prepLimit,Missing["BatchPreparationTimeout"]];
      If[Head[prep]===Missing,Throw[Failure["GaugeBatchPreparationTimeout",<|
        "ID"->unit["ID"],"TimeLimit"->prepLimit,
        "StoppedOperation"->"BatchPrepare","Description"->"NotStartedbatchTogether. "|>],stop]];
      If[FailureQ[prep],Throw[prep,stop]];
      result=RGZGaugeBatchCacheWrite[prep,prepPath,{identity,"prepare"}];
      If[FailureQ[result],Throw[result,stop]]];
    mergeContext=RGZGaugeMergeContext[];
    If[StringQ[dir],mergeDir=FileNameJoin[{dir,
      "v26_"<>IntegerString[mergeContext,16]}];
      If[!DirectoryQ[mergeDir],Quiet[Check[
        CreateDirectory[mergeDir,CreateIntermediateDirectories->True],
        Throw[Failure["BatchCacheDirectory",<||>],stop]]]]];
    vars=prep["RootSymbol"];radicands=prep["Radicand"];
    definitions=prep["RadicalDefinition"];chunks=prep["Batch"];
    AssociateTo[rgzGaugeBatchProgress,{
      "TermsCount"->prep["TermsCount"],"initialBatchCount"->prep["initialBatchCount"],
      "same DenominatorgroupCount"->prep["same DenominatorgroupCount"],"RadicalTypes"->Length[vars],"CacheDirectory"->dir}];
    run[part_List,lev_Integer,index_Integer,total_Integer] := Module[
      {nodePath,nodeKey,v,elapsed,saved,fromCache=False,oldPath,oldKey,inputHash},
      operation=If[lev===0,"SmallBatchTogether","PairwiseCombine"];
      RGZMXProgressUpdate["TermID"->index,"TotalTermCount"->total,"CompleteTermCount"->index-1];
      RGZGaugeStep[operation<>": level "<>ToString[lev]<>", batch "<>
        ToString[index]<>" / "<>ToString[total]];
      AssociateTo[rgzGaugeBatchProgress,{
        "LevelID"->lev,"BatchID"->index,"this levelBatchCount"->total,"StoppedOperation"->operation}];
      rgzGaugeBatchCurrentNode=<|"ID"->unit["ID"],"LevelID"->lev,
        "BatchID"->index,"this levelBatchCount"->total,"Input"->part,"RootSymbol"->vars,
        "Radicand"->radicands,"RadicalDefinition"->definitions|>;
      oldKey={identity,lev,index};
      oldPath=If[StringQ[dir],FileNameJoin[{dir,
        "level_"<>ToString[lev]<>"_unit_"<>ToString[index]<>".wl"}],None];
      {elapsed,v}=AbsoluteTiming[TimeConstrained[
        inputHash=Hash[part,"SHA256"];
        nodeKey={identity,mergeContext,lev,index,inputHash};
        nodePath=If[StringQ[mergeDir],FileNameJoin[{mergeDir,
          "level_"<>ToString[lev]<>"_unit_"<>ToString[index]<>
          "_"<>IntegerString[inputHash,16]<>".wl"}],None];
        v=RGZGaugeBatchCacheRead[nodePath,nodeKey];
        If[Head[v]=!=Missing,
          legacyPrefix=False;fromCache=True;resumeCount++;v,
          (* Old indexed nodes are safe only along the unchanged traversal
             prefix. Once a new result is used, only input-keyed v2.5 nodes
             may be restored: changed zero removal cannot misalign caches. *)
          If[TrueQ[legacyPrefix],v=RGZGaugeBatchCacheRead[oldPath,oldKey]];
          If[Head[v]=!=Missing,fromCache=True;resumeCount++;v,
            legacyPrefix=False;
            If[lev===0,RGZGaugeBatchReduce[Total[part],vars,radicands],
              RGZGaugeBatchMerge[part,vars,radicands]]]],
        seconds,Missing["BatchTimeout"]]];
      If[Head[v]===Missing,
        rgzGaugeBatchStoppedNode=<|"ID"->unit["ID"],"LevelID"->lev,
          "BatchID"->index,"this levelBatchCount"->total,"Input"->part,"RootSymbol"->vars,
          "Radicand"->radicands,"RadicalDefinition"->definitions,
          "StoppedOperation"->Lookup[rgzGaugeProgress,"CurrentOperation",operation]|>;
        Throw[Failure["GaugeBatchTimeout",<|
        "ID"->unit["ID"],"LevelID"->lev,"BatchID"->index,
        "this levelBatchCount"->total,"StoppedOperation"->
          Lookup[rgzGaugeProgress,"CurrentOperation",operation],"TimeLimit"->seconds,
        "ElapsedSeconds"->elapsed,"NewComputeBatchCount"->computedCount,
        "CacheRestoredBatchCount"->resumeCount,
        "Description"->"Stopped at a batch timeout; rerun with the same input to reuse completed batches."|>],stop]];
      If[FailureQ[v] || v===$Failed || v===$Aborted,
        Throw[Failure["GaugeBatchReductionFailed",<|
          "ID"->unit["ID"],"LevelID"->lev,"BatchID"->index,
          "Cause"->v|>],stop]];
      (* Cached zero is a valid completed node, not a missing result. *)
      If[!TrueQ[fromCache],
        saved=RGZGaugeBatchCacheWrite[v,nodePath,nodeKey];
        If[FailureQ[saved],Throw[saved,stop]];computedCount++];
      AssociateTo[rgzGaugeBatchProgress,{
        "NewComputeBatchCount"->computedCount,"CacheRestoredBatchCount"->resumeCount,
        "CurrentbatchElapsedSeconds"->elapsed}];
      RGZMXProgressUpdate["CompleteTermCount"->index];v
    ];
    current=Table[run[chunks[[n]],0,n,Length[chunks]],{n,Length[chunks]}];
    current=DeleteCases[current,0];
    While[Length[current]>1,
      level++;groups=RGZGaugeBatchChunks[current,2];
      next=Table[run[groups[[n]],level,n,Length[groups]],{n,Length[groups]}];
      current=DeleteCases[next,0]];
    z=If[current==={},0,First[current]];
    RGZGaugeStep["Batch complete: restore radicals and save the term"];
    physical=z/.definitions;
    row=<|"ID"->unit["ID"],"MasterIntegral"->unit["MasterIntegral"],
      "OriginalCoefficient"->Missing["UseShortCoefficients"],"ResidualCoefficient"->physical,
      "RadicalSymbolCoefficient"->z,"RadicalDefinition"->definitions,
      "Status"->RGZGaugeState[physical],"Source"->"BatchExactReduce",
      "ElapsedSeconds"->AbsoluteTime[]-start|>;
    path=Lookup[unit,"CachePath",None];key=Lookup[unit,"CacheIdentity",None];
    rgzGaugeBatchLastCoefficient=row;
    If[!TrueQ[physical===0] && LeafCount[z]>5000,
      Throw[Failure["GaugeScalarCoefficientPending",<|
        "ID"->unit["ID"],"StoppedOperation"->"FinalParameterCoefficientSimplifystill Incomplete",
        "Description"->"The combined radical result is retained, but large parameter coefficients remain unnormalized. No completed-coefficient cache was written; neither a nonzero result nor gauge independence is established."|>],stop]];
    (* Verify the original v2 coefficient cache identity before writing it.
       RGZGaugeCoefficient itself has not changed in this release. *)
    If[StringQ[path] && key===Hash[{
        Hash[{"RGZGaugeCoefficient-v2",DownValues[RGZGaugeCoefficient],
          DownValues[RGZGaugePoleSquares],DownValues[RGZLoopPoleMasses]},"SHA256"],
        unit["ReferenceGauge"],{unit["MasterIntegral"],unit["AlphaShortCoefficient"],unit["ReferenceShortCoefficient"]}},"SHA256"],
      RGZGaugeCacheWrite[row,path,key];
      cacheSaved=TimeConstrained[
        AssociationQ[RGZGaugeCacheRead[path,key]],10,False]];
    rgzGaugeBatchLastCoefficient=row;
    <|"Status"->"SingleTermComplete","ID"->unit["ID"],
      "CoefficientStatus"->row["Status"],"TermsCount"->prep["TermsCount"],
      "initialBatchCount"->prep["initialBatchCount"],"RadicalTypes"->Length[vars],
      "NewComputeBatchCount"->computedCount,"CacheRestoredBatchCount"->resumeCount,
      "ElapsedSeconds"->AbsoluteTime[]-start,"Saveto OriginalCoefficientCache"->cacheSaved,
      "Description"->"Only the stopped term is complete; batches have been summed. Other master-integral terms still require checks."|>,stop];
  result=CheckAbort[TimeConstrained[worker[],totalLimit,
    Failure["GaugeBatchTotalTimeout",<|
      "ID"->unit["ID"],"TimeLimit"->totalLimit,
      "LevelID"->Lookup[rgzGaugeBatchProgress,"LevelID",0],
      "BatchID"->Lookup[rgzGaugeBatchProgress,"BatchID",0],
      "this levelBatchCount"->Lookup[rgzGaugeBatchProgress,"this levelBatchCount",0],
      "Description"->"The total time limit was reached; completed batch caches are retained."|>]],
    Failure["GaugeBatchAborted",<|"ID"->unit["ID"],
      "Description"->"Aborted; completed batch caches are retained."|>]];
  If[FailureQ[result] && !AssociationQ[rgzGaugeBatchStoppedNode] &&
      AssociationQ[rgzGaugeBatchCurrentNode],
    rgzGaugeBatchStoppedNode=Join[rgzGaugeBatchCurrentNode,<|
      "StoppedOperation"->Lookup[rgzGaugeProgress,"CurrentOperation",""]|>]];
  AssociateTo[rgzGaugeBatchProgress,{
    "Status"->If[FailureQ[result],"Stopped","SingleTermComplete"],
    "ElapsedSeconds"->AbsoluteTime[]-start}];
  RGZMXProgressUpdate["Status"->If[FailureQ[result],"Stopped","SingleTermComplete"]];
  result
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

(* v2.5: merge two already rational batches without factoring/cancelling
   the full new numerator. The denominator GCD has its own short budget;
   GCD timeout falls back to the exact product denominator, never to zero.
   No mass/gauge values or root branches are changed. *)
RGZGaugeBatchRootReduce[poly_,vars_List,radicands_List] := Module[{z=poly,j},
  Do[If[PolynomialQ[z,vars[[j]]] && Exponent[z,vars[[j]]]>=2,
    z=PolynomialRemainder[z,vars[[j]]^2-radicands[[j]],vars[[j]]]],
    {j,Length[vars]}];z
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

(* v2.6: sparse polynomials in root variables ONLY. Parameter coefficients
   remain exact factored expressions. No full expansion of mass parameters. *)
RGZGaugeSparseGroup[ps_List] := Select[
  ({#[[1,1]],Total[#[[All,2]]]}& /@ GatherBy[ps,First]),
  !TrueQ[#[[2]]===0]&];
RGZGaugeSparseFrom[x_,vars_List] := Module[{rootRules,rootPattern},
  If[TrueQ[x===0],Return[{}]];
  If[vars==={},Return[{{{},x}}]];
  rootPattern=Alternatives@@vars;
  If[!TrueQ[PolynomialQ[x,vars]],Return[Failure["NeedPolynomialInRoots",<|
    "Description"->"The input is not polynomial in radical variables; failure is not treated as zero."|>]]];
  rootRules=Quiet[Check[CoefficientRules[x,vars],$Failed]];
  If[!ListQ[rootRules] || !AllTrue[rootRules,
      MatchQ[#,_Rule] && ListQ[First[#]] && Length[First[#]]===Length[vars] &&
      VectorQ[First[#],IntegerQ[#] && #>=0&] && FreeQ[Last[#],rootPattern]&],
    Return[Failure["NeedPolynomialInRoots",<|
      "Description"->"Cannot extract a polynomial in the radical variables; failure is not treated as zero."|>]]];
  ({First[#],Last[#]}& /@ rootRules)
];
RGZGaugeSparseTimes[left_List,right_List] :=
  RGZGaugeSparseGroup[Flatten[Table[
    {leftSparseTerm[[1]]+rightSparseTerm[[1]],leftSparseTerm[[2]] rightSparseTerm[[2]]},{leftSparseTerm,left},{rightSparseTerm,right}],1]];
RGZGaugeSparsePower[ps_List,k_Integer,dim_Integer] := Module[
  {sparsePowerResult={{ConstantArray[0,dim],1}},sparsePowerBase=ps,sparsePowerExponent=k},
  While[sparsePowerExponent>0,If[OddQ[sparsePowerExponent],sparsePowerResult=RGZGaugeSparseTimes[sparsePowerResult,sparsePowerBase]];
    sparsePowerExponent=Quotient[sparsePowerExponent,2];If[sparsePowerExponent>0,sparsePowerBase=RGZGaugeSparseTimes[sparsePowerBase,sparsePowerBase]]];sparsePowerResult
];
RGZGaugeSparseReduce[ps_List,relations_List,dim_Integer] := Module[
  {reducedSparsePairs=ps,sparsePieces,sparseTerm,sparseRootPower,sparseExponents,sparseRootPosition},
  Do[
    sparsePieces=Table[sparseRootPower=Quotient[sparseTerm[[1,sparseRootPosition]],2];
      If[sparseRootPower===0,{sparseTerm},
        sparseExponents=ReplacePart[sparseTerm[[1]],sparseRootPosition->Mod[sparseTerm[[1,sparseRootPosition]],2]];
        ({sparseExponents+#[[1]],sparseTerm[[2]] #[[2]]}& /@
          RGZGaugeSparsePower[relations[[sparseRootPosition]],sparseRootPower,dim])],{sparseTerm,reducedSparsePairs}];
    reducedSparsePairs=RGZGaugeSparseGroup[Flatten[sparsePieces,1]],{sparseRootPosition,dim}];reducedSparsePairs
];
RGZGaugeSparseTo[ps_List,vars_List] := Total[
  (#[[2]] Times@@MapThread[Power,{vars,#[[1]]}]& /@ ps)];

(* Small scalar coefficients get a bounded exact zero test. Large/unfinished
   coefficients are retained verbatim, never discarded or labelled proven zero. *)
RGZGaugeSparseTidy[ps_List] := Module[
  {pending=0,tidiedPairs,sparseCoefficient,sparseSimplified,j},
  tidiedPairs=Table[sparseCoefficient=ps[[j,2]];
    If[LeafCount[sparseCoefficient]<=1000,
      sparseSimplified=TimeConstrained[Cancel[Together[sparseCoefficient]],.25,
        Missing["ScalarTidyTimeout"]];
      If[Head[sparseSimplified]===Missing,
        pending++;sparseSimplified=sparseCoefficient],
      pending++;sparseSimplified=sparseCoefficient];
    {ps[[j,1]],sparseSimplified},{j,Length[ps]}];
  <|"Polynomial"->RGZGaugeSparseGroup[tidiedPairs],"pending ParameterCoefficientCount"->pending|>
];
RGZGaugeMergeContext[] := Hash[{"RGZGaugeMerge-v2.6",
  DownValues[RGZGaugeSparseMergeCore],DownValues[RGZGaugeSparseFrom],
  DownValues[RGZGaugeSparseGroup],DownValues[RGZGaugeSparseTimes],
  DownValues[RGZGaugeSparsePower],DownValues[RGZGaugeSparseReduce],
  DownValues[RGZGaugeSparseTo],DownValues[RGZGaugeSparseTidy]},"SHA256"];

RGZGaugeSparseMergeCore[part_List,vars_List,radicands_List] := Module[
  {leftNumerator,rightNumerator,leftDenominator,rightDenominator,denominatorGCD,leftDenominatorMultiplier,rightDenominatorMultiplier,inputs,ps,relations,num,den,tidyN,tidyD,j,dim=Length[vars]},
  If[part==={},Return[0]];
  If[Length[part]===1,Return[First[part]]];
  If[Length[part]=!=2,Return[Failure["NeedTwoMergeInputs",<||>]]];
  If[Length[radicands]=!=dim,Return[Failure["RootBasisMismatch",<||>]]];
  If[AnyTrue[Range[dim],
      !FreeQ[radicands[[#]],Alternatives@@Take[vars,#]]&],
    Return[Failure["RootDependencyOrder",<||>]]];
  rgzGaugeSparseProgress=<|"RadicalVariableCount"->dim|>;
  RGZGaugeStep["Sparse combination: read numerator and denominator"];
  {leftNumerator,rightNumerator}=Numerator /@ part;{leftDenominator,rightDenominator}=Denominator /@ part;
  If[leftDenominator===rightDenominator,leftDenominatorMultiplier=1;rightDenominatorMultiplier=1,
    RGZGaugeStep["Sparse combination: find common denominator (up to 3 seconds)"];
    denominatorGCD=Quiet[Check[TimeConstrained[PolynomialGCD[leftDenominator,rightDenominator],3,
      Missing["DenominatorGCDTimeout"]],$Failed]];
    If[Head[denominatorGCD]===Missing || denominatorGCD===$Failed || TrueQ[denominatorGCD===0] ||
        !FreeQ[denominatorGCD,_PolynomialGCD],denominatorGCD=1];
    RGZGaugeStep["Sparse combination: construct denominator multiples"];
    If[denominatorGCD===1,leftDenominatorMultiplier=leftDenominator;rightDenominatorMultiplier=rightDenominator,leftDenominatorMultiplier=Cancel[leftDenominator/denominatorGCD];rightDenominatorMultiplier=Cancel[rightDenominator/denominatorGCD]]];
  inputs={leftNumerator,rightNumerator,leftDenominatorMultiplier,rightDenominatorMultiplier,leftDenominator};
  ps=Table[RGZGaugeStep["Sparse combination: extract radical monomials "<>ToString[j]<>" / 5"];
    RGZGaugeSparseFrom[inputs[[j]],vars],{j,5}];
  If[AnyTrue[ps,FailureQ],Return[First[Select[ps,FailureQ]]]];
  AssociateTo[rgzGaugeSparseProgress,{
    "Input1NumeratorSingleTermexpressionCount"->Length[ps[[1]]],"Input2NumeratorSingleTermexpressionCount"->Length[ps[[2]]]}];
  relations=RGZGaugeSparseFrom[#,vars]& /@ radicands;
  If[AnyTrue[relations,FailureQ],Return[First[Select[relations,FailureQ]]]];
  RGZGaugeStep["Sparse combination: reduce denominator radical squares"];
  den=RGZGaugeSparseReduce[RGZGaugeSparseTimes[ps[[5]],ps[[4]]],relations,dim];
  tidyD=RGZGaugeSparseTidy[den];den=tidyD["Polynomial"];
  If[den==={},Return[Failure["PoleRelationSingularity",<||>]]];
  RGZGaugeStep["Sparse combination: multiply numerator monomials and reduce radical squares"];
  num=RGZGaugeSparseGroup[Join[
    RGZGaugeSparseTimes[ps[[1]],ps[[4]]],
    RGZGaugeSparseTimes[ps[[2]],ps[[3]]]]];
  num=RGZGaugeSparseReduce[num,relations,dim];
  RGZGaugeStep["Sparse combination: check small parameter coefficients and retain large ones"];
  tidyN=RGZGaugeSparseTidy[num];num=tidyN["Polynomial"];
  AssociateTo[rgzGaugeSparseProgress,{
    "ResultNumeratorSingleTermexpressionCount"->Length[num],"ResultDenominatorSingleTermexpressionCount"->Length[den],
    "pending ParameterCoefficientCount"->tidyN["pending ParameterCoefficientCount"]+tidyD["pending ParameterCoefficientCount"]}];
  If[num==={},
    If[tidyD["pending ParameterCoefficientCount"]>0,
      Return[Failure["UnverifiedMergeDenominator",<||>]],Return[0]]];
  RGZGaugeSparseTo[num,vars]/RGZGaugeSparseTo[den,vars]
];

RGZGaugeBatchMerge[part_List,vars_List,radicands_List] := Module[{key},
  key=Hash[{RGZGaugeMergeContext[],part,vars,radicands},"SHA256"];
  If[AssociationQ[rgzGaugeSparseTestMemo] &&
      Lookup[rgzGaugeSparseTestMemo,"Identity",None]===key,
    rgzGaugeSparseProgress=rgzGaugeSparseTestMemo["Diagnostic"];
    Return[rgzGaugeSparseTestMemo["Result"]]];
  RGZGaugeSparseMergeCore[part,vars,radicands]
];

(* Test the already retained stopped node ONLY. Return a short report, not
   its huge expression. A successful result is memoized for the full resume. *)
RGZGaugeTestStoppedMerge[seconds_:60] := Module[{node,result,time,key},
  If[!AssociationQ[rgzGaugeBatchStoppedNode],
    Return[Failure["NoStoppedBatchNode",<||>]]];
  If[!NumericQ[seconds] || !TrueQ[seconds>0],
    Return[Failure["DiagnosticTimeLimit",<||>]]];
  node=rgzGaugeBatchStoppedNode;
  RGZMXProgressUpdate["Sector"->"RGZ isolated stopped-node test","Stage"->"SparseCombine",
    "StageStartTime"->AbsoluteTime[],"Status"->"ComputeIn", "DiagramID"->0,"DiagramCount"->0,
    "TermID"->1,"TotalTermCount"->1,"CompleteTermCount"->0];
  {time,result}=AbsoluteTiming[CheckAbort[TimeConstrained[
    key=Hash[{RGZGaugeMergeContext[],node["Input"],node["RootSymbol"],
      node["Radicand"]},"SHA256"];
    RGZGaugeSparseMergeCore[node["Input"],node["RootSymbol"],node["Radicand"]],
    seconds,Missing["SparseMergeTimeout"]],$Aborted]];
  If[Head[result]===Missing || result===$Aborted || FailureQ[result],
    RGZMXProgressUpdate["Status"->"Stopped"];
    Return[Failure["GaugeSparseMergeStopped",<|
      "ID"->node["ID"],"LevelID"->node["LevelID"],"BatchID"->node["BatchID"],
      "StoppedOperation"->Lookup[rgzGaugeProgress,"CurrentOperation","SparseCombine"],
      "TimeLimit"->seconds,"ElapsedSeconds"->time,
      "CauseTag"->Which[FailureQ[result],result[[1]],
        Head[result]===Missing,result[[1]],True,"Aborted"],"Cause"->result|>]]];
  rgzGaugeSparseTestMemo=<|"Identity"->key,"Result"->result,"Diagnostic"->rgzGaugeSparseProgress|>;
  RGZMXProgressUpdate["Status"->"SingleNodeComplete","CompleteTermCount"->1];
  Join[<|"Status"->"SingleNodeComplete","ID"->node["ID"],"LevelID"->node["LevelID"],
    "BatchID"->node["BatchID"],"ElapsedSeconds"->time,"ResultLeafCount"->LeafCount[result],
    "Isliteral Zero"->TrueQ[result===0]|>,rgzGaugeSparseProgress]
];
RGZGaugeSparseSummary[report_] := Module[{data},
  data=Which[AssociationQ[report],report,FailureQ[report],
    Print["SingleNodeStoppedTag: ",report[[1]]];KeyDrop[report[[2]],{"Cause"}],
    True,Return[Print["Resulthead: ",Head[report]]]];
  Print[Grid[({#,data[#]}& /@ Keys[data]),Frame->All,Alignment->Left]]
];

Print["Gauge coefficient checker v2.6 loaded; only radical monomials are expanded. First test the stopped node with RGZGaugeTestStoppedMerge[60]."] ;
