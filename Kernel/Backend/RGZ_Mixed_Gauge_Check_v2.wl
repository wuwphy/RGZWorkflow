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
  RGZMXProgressUpdate["\:5b50\:6b65\:9aa4"->label,"\:64cd\:4f5c\:5f00\:59cb\:65f6\:95f4"->AbsoluteTime[]];
  If[AssociationQ[rgzGaugeProgress],AssociateTo[rgzGaugeProgress,
    {"\:5f53\:524d\:64cd\:4f5c"->label,"\:64cd\:4f5c\:5f00\:59cb\:65f6\:95f4"->AbsoluteTime[]}]];Null);

(* The external A row/column of the tree propagators. In the T basis,
   DT[A,i]=dAA u_i. In the L basis the left/right signs differ: no
   transpose/Hermitian-conjugation shortcut is used. *)
RGZGaugeStart[data_Association] := Module[
  {piT,piL,s0,k,h,delta,den,u,dAA,rowL,colL,termsT,termsL},
  If[!And@@(KeyExistsQ[data,#]&/@{"T","L","T\:573a\:987a\:5e8f","L\:573a\:987a\:5e8f"}),
    Return[Failure["MissingMixedMatrices",<||>]]];
  If[data["T\:573a\:987a\:5e8f"]=!=RGZMixBasis["T"] ||
      data["L\:573a\:987a\:5e8f"]=!=RGZMixBasis["L"],
    Return[Failure["MixedBasisMismatch",<|
      "\:8bf4\:660e"->"\:5fc5\:987b\:4f7f\:7528\:5df2\:68c0\:67e5\:6838\:7684\:573a\:987a\:5e8f\:4e0e\:5f52\:4e00\:5316\:4f34\:968f\:989c\:8272\:57fa\:5e95\:3002"|>]]];
  piT=data["T"];piL=data["L"];
  If[Dimensions[piT]=!={5,5} || Dimensions[piL]=!={8,8} ||
      !FreeQ[{piT,piL},_Failure|_Missing|$Failed|$Aborted],
    Return[Failure["IncompleteMixedMatrices",<||>]]];
  If[OwnValues[\[Alpha]]=!={},Return[Failure["AssignedGaugeParameter",
    <|"\:8bf4\:660e"->"\:5148\:4fdd\:6301 Alpha \:4e3a\:7b26\:53f7\:ff0c\:4e0d\:80fd\:7528\:5df2\:7ecf\:56fa\:5b9a\:89c4\:8303\:7684\:7ed3\:679c\:8bc1\:660e\:89c4\:8303\:65e0\:5173\:3002"|>]]];
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
  <|"T\:573a\:987a\:5e8f"->data["T\:573a\:987a\:5e8f"],"L\:573a\:987a\:5e8f"->data["L\:573a\:987a\:5e8f"],
    "\:6811\:7ea7\:6a2a\:5411AA"->dAA,"\:6a2a\:5411\:5916\:817f\:6743\:91cd"->u,
    "\:7eb5\:5411\:5de6\:5916\:817f"->rowL,"\:7eb5\:5411\:53f3\:5916\:817f"->colL,
    "\:81ea\:80fdT\:5206\:9879"->termsT,"\:81ea\:80fdT"->Total[termsT],
    "\:8fde\:63a5\:540eT"->dAA^2 Total[termsT],"\:8fde\:63a5\:540eL"->Total[termsL],
    "\:8bf4\:660e"->"\:81ea\:80fdT=u.PiT.u\:ff1b\:8fde\:63a5\:540eT/L=(D0.Pi.D0)AA\:ff1b\:5c1a\:672a\:52a0\:5165 pinch \:9879\:3002"|>
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
      <|"\:5934\:90e8"->RGZLoopName[Head[x]]|>],stop]];
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
  RGZMXProgressUpdate["\:9879\:53f7"->0,"\:603b\:9879\:6570"->Length[inputs],"\:5b8c\:6210\:9879\:6570"->0];
  Do[
    RGZMXProgressUpdate["\:9879\:53f7"->j,"\:5b8c\:6210\:9879\:6570"->j-1];
    RGZGaugeStep["\:5206\:79bb\:4e3b\:79ef\:5206\:ff1a\:8f93\:5165 "<>ToString[j]<>" / "<>ToString[Length[inputs]]];
    z=CheckAbort[TimeConstrained[RGZGaugePVPairs[inputs[[j]]],seconds,
      Missing["PreparationTimeout"]],$Aborted];
    If[Head[z]===Missing,Throw[Failure["GaugePreparationTimeout",<|
      "\:9636\:6bb5"->"\:4fdd\:7559\:6781\:70b9\:6807\:7b7e\:ff0c\:5206\:79bb\:4e3b\:79ef\:5206","\:8f93\:5165\:5206\:9879"->j,
      "\:65f6\:9650"->seconds,"\:8bf4\:660e"->"\:6ca1\:6709\:5f97\:5230\:89c4\:8303\:6b8b\:5dee\:ff1b\:4e0d\:80fd\:636e\:6b64\:5224\:65ad\:662f\:5426\:89c4\:8303\:65e0\:5173\:3002"|>],stop]];
    If[FailureQ[z],Throw[z,stop]];
    If[z===$Aborted,Throw[Failure["GaugeCheckAborted",<|
      "\:9636\:6bb5"->"\:5206\:79bb\:4e3b\:79ef\:5206","\:8f93\:5165\:5206\:9879"->j|>],stop]];
    AppendTo[batches,z];
    RGZMXProgressUpdate["\:5b8c\:6210\:9879\:6570"->j],{j,Length[inputs]}];
  RGZGaugeStep["\:5408\:5e76\:5404\:8f93\:5165\:7684\:540c\:7c7b\:4e3b\:79ef\:5206"];
  raw=({#[[1,1]],Total[#[[All,2]]]}& /@
    GatherBy[Flatten[batches,1],First]);
  RGZMXProgressUpdate["\:9879\:53f7"->0,"\:603b\:9879\:6570"->Length[raw],"\:5b8c\:6210\:9879\:6570"->0];
  Do[
    RGZMXProgressUpdate["\:9879\:53f7"->j,"\:5b8c\:6210\:9879\:6570"->j-1];
    RGZGaugeStep["\:89c4\:8303\:66ff\:6362\:ff1a\:4e3b\:79ef\:5206\:53c2\:6570 "<>ToString[j]<>" / "<>ToString[Length[raw]]];
    key=RGZGaugeCanonicalPV[RGZLoopPoleMasses[raw[[j,1]]]];
    refkey=RGZGaugeCanonicalPV[key/.\[Alpha]->reference];
    AppendTo[out,{key,raw[[j,2]],0}];
    AppendTo[out,{refkey,0,raw[[j,2]]}];
    RGZMXProgressUpdate["\:5b8c\:6210\:9879\:6570"->j],{j,Length[raw]}];
  groups=GatherBy[out,First];
  ({#[[1,1]],Total[#[[All,2]]],Total[#[[All,3]]]}& /@ groups),stop]
];

RGZGaugeState[x_] := Which[
  Head[x]===Missing,"\:8d85\:65f6",FailureQ[x] || x===$Failed || x===$Aborted,"\:5931\:8d25",
  TrueQ[x===0],"\:5f52\:96f6",True,"\:672a\:5f52\:96f6"];

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

RGZGaugeCoefficient[a_,b_,reference_] := Module[
  {ca,cb,bases,vars,rules,definitions,radicands,z,num,den,j,physical},
  RGZGaugeStep["\:6781\:70b9\:5e73\:65b9\:66ff\:6362"];
  ca=RGZGaugePoleSquares[a];cb=RGZGaugePoleSquares[b]/.\[Alpha]->reference;
  If[ca===cb,Return[<|"\:6b8b\:5dee\:7cfb\:6570"->0,"\:6839\:5f0f\:7b26\:53f7\:7cfb\:6570"->0,"\:6839\:5f0f\:5b9a\:4e49"->{}|>]];
  RGZGaugeStep["\:6839\:5f0f\:6539\:7528\:77ed\:7b26\:53f7"];
  bases=DeleteDuplicates[Cases[{ca,cb},
    HoldPattern[Power[x_,n_Rational]] /; Denominator[n]===2 :> x,Infinity]];
  If[bases==={},z=Cancel[Together[ca-cb]];
    Return[<|"\:6b8b\:5dee\:7cfb\:6570"->z,"\:6839\:5f0f\:7b26\:53f7\:7cfb\:6570"->z,"\:6839\:5f0f\:5b9a\:4e49"->{}|>]];
  (* Outer radicals precede the radicals in their radicands. *)
  bases=SortBy[bases,-Depth[#]&];
  vars=Table[Unique["rgzGaugeRoot$"],{Length[bases]}];
  rules=MapThread[Function[{base,var},With[{bb=base,vv=var},
    HoldPattern[Power[bb,n_Rational]] /; Denominator[n]===2 :>
      vv^Numerator[n]]],{bases,vars}];
  definitions=MapThread[Rule,{vars,Sqrt /@ bases}];
  radicands=bases/.rules;
  RGZGaugeStep["\:77ed\:7b26\:53f7\:6709\:7406\:5f0f\:901a\:5206"];
  z=Cancel[Together[(ca/.rules)-(cb/.rules)]];
  If[TrueQ[z===0],Return[<|"\:6b8b\:5dee\:7cfb\:6570"->0,"\:6839\:5f0f\:7b26\:53f7\:7cfb\:6570"->0,
    "\:6839\:5f0f\:5b9a\:4e49"->definitions|>]];
  num=Numerator[z];den=Denominator[z];
  RGZGaugeStep["\:6838\:5bf9\:6839\:5f0f\:5e73\:65b9\:5173\:7cfb"];
  Do[
    If[!FreeQ[radicands[[j]],vars[[j]]],
      Return[Failure["CyclicRootDefinition",<||>]]];
    If[PolynomialQ[num,vars[[j]]] && PolynomialQ[den,vars[[j]]],
      If[Exponent[num,vars[[j]]]>=2,
        num=PolynomialRemainder[num,vars[[j]]^2-radicands[[j]],vars[[j]]]];
      If[Exponent[den,vars[[j]]]>=2,
        den=PolynomialRemainder[den,vars[[j]]^2-radicands[[j]],vars[[j]]]]],
    {j,Length[vars]}];
  If[TrueQ[den===0],Return[Failure["PoleRelationSingularity",<|
    "\:8bf4\:660e"->"\:6781\:70b9\:5173\:7cfb\:4e0b\:5206\:6bcd\:4e3a\:96f6\:ff1b\:4e0d\:5c06\:6b64\:9879\:6807\:8bb0\:4e3a\:901a\:8fc7\:3002"|>]]];
  z=Cancel[Together[num/den]];
  RGZGaugeStep["\:6062\:590d\:6839\:5f0f\:5b9a\:4e49"];
  physical=z/.definitions;
  <|"\:6b8b\:5dee\:7cfb\:6570"->physical,"\:6839\:5f0f\:7b26\:53f7\:7cfb\:6570"->z,"\:6839\:5f0f\:5b9a\:4e49"->definitions|>
];

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
  If[AssociationQ[r] && Lookup[r,"\:8eab\:4efd",None]===key &&
      AssociationQ[Lookup[r,"\:7ed3\:679c",None]] &&
      MemberQ[{"\:5f52\:96f6","\:672a\:5f52\:96f6"},Lookup[r["\:7ed3\:679c"],"\:72b6\:6001",None]],
    r["\:7ed3\:679c"],Missing["NoValidCache"]]
];
RGZGaugeCacheWrite[row_,path_,key_] := Module[{r},
  If[path===None,Return[Null]];
  r=CheckAbort[TimeConstrained[
    RGZMXWrite[<|"\:8eab\:4efd"->key,"\:7ed3\:679c"->row|>,path],10,
    Missing["CacheWriteTimeout"]],Missing["CacheWriteAborted"]];
  If[FailureQ[r] || Head[r]===Missing,
    AssociateTo[rgzGaugeProgress,"\:7f13\:5b58\:5199\:5165"->"\:672a\:5b8c\:6210\:ff0c\:5185\:5b58\:7ed3\:679c\:4fdd\:7559"]];Null
];
RGZGaugeProgressSummary[] := If[AssociationQ[rgzGaugeProgress],
  Grid[({#,Lookup[rgzGaugeProgress,#,Missing["Unavailable"]]}& /@
    {"\:5df2\:5b8c\:6210\:9879\:6570","\:4e3b\:79ef\:5206\:9879\:6570","\:7edf\:8ba1","\:5f53\:524d\:9879","\:5f53\:524d\:64cd\:4f5c","\:5f53\:524d\:9879\:8017\:65f6","\:7f13\:5b58\:76ee\:5f55"}),
    Frame->All,Alignment->Left],"\:5c1a\:672a\:5f00\:59cb\:65b0\:7248\:7cfb\:6570\:68c0\:67e5"];

(* Print only a bounded diagnostic, never the stored coefficient input. *)
RGZGaugeStoppedInfo[] := If[AssociationQ[rgzGaugeStoppedUnit],
  Grid[({#,Lookup[rgzGaugeStoppedUnit,#,Missing["Unavailable"]]}& /@
    {"\:7f16\:53f7","\:603b\:9879\:6570","\:505c\:6b62\:64cd\:4f5c","\:65f6\:9650","\:8017\:65f6","\:5df2\:5b8c\:6210\:9879\:6570"}),
    Frame->All,Alignment->Left],"\:672c\:8f6e\:6ca1\:6709\:4fdd\:5b58\:8d85\:65f6\:505c\:6b62\:9879"];

(* Structural inspection only: no Together, Cancel, Simplify or PV evaluation.
   Each column has a separate small time budget; never print raw coefficients. *)
RGZGaugeInspectStopped[seconds_:10] := Module[
  {labels,keys,columns,metrics,x,z,key},
  If[!AssociationQ[rgzGaugeStoppedUnit],
    Return[Failure["NoStoppedCoefficient",<||>]]];
  If[!NumericQ[seconds] || !TrueQ[seconds>0],
    Return[Failure["DiagnosticTimeLimit",<||>]]];
  labels={"\:7ed3\:6784","\:8868\:8fbe\:5f0f\:8282\:70b9\:6570","\:5927\:5c0f\:ff08MiB\:ff09","\:9876\:5c42\:52a0\:6cd5\:9879\:6570",
    "\:6781\:70b9\:6807\:7b7e\:51fa\:73b0\:6b21\:6570","\:5e73\:65b9\:6839\:51fa\:73b0\:6b21\:6570"};
  keys={"Alpha\:77ed\:7cfb\:6570","\:53c2\:8003\:77ed\:7cfb\:6570"};
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
    Prepend[labels,"\:68c0\:67e5\:9879\:76ee"],
    Prepend[columns[[1]],"\:03b1\:77ed\:6807\:7b7e\:7cfb\:6570"],
    Prepend[columns[[2]],"\:53c2\:8003\:77ed\:6807\:7b7e\:7cfb\:6570"]
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
Options[RGZGaugeCompare]={"\:9884\:5904\:7406\:65f6\:9650"->300,"\:7f13\:5b58\:76ee\:5f55"->Automatic,
  "\:8d85\:65f6\:5373\:505c"->True};
RGZGaugeCompare[expr_,reference_:1,seconds_:30,OptionsPattern[]] := Module[
  {prep,pairs,rows,c,stats,state,n,unresolved,abort,prepareSeconds,
    result,row,cacheDir,path,key,context,time,details,stopTimeout,operation},
  prepareSeconds=OptionValue["\:9884\:5904\:7406\:65f6\:9650"];
  stopTimeout=OptionValue["\:8d85\:65f6\:5373\:505c"];
  If[!FreeQ[reference,\[Alpha]] || !NumericQ[reference] ||
      !TrueQ[reference>0] || !NumericQ[seconds] || !TrueQ[seconds>0] ||
      !NumericQ[prepareSeconds] || !TrueQ[prepareSeconds>0] ||
      !MemberQ[{True,False},stopTimeout],
    Return[Failure["GaugeComparisonArguments",<|
      "\:8bf4\:660e"->"\:53c2\:8003\:89c4\:8303\:53d6\:6b63\:6570\:ff0c\:4f8b\:5982 1\:ff1b\:5316\:7b80\:548c\:9884\:5904\:7406\:65f6\:9650\:4e5f\:5fc5\:987b\:4e3a\:6b63\:6570\:3002"|>]]];
  If[OwnValues[\[Alpha]]=!={},Return[Failure["AssignedGaugeParameter",<||>]]];
  If[!FreeQ[expr,_Failure|_Missing|$Failed|$Aborted|_TID|_PaVeReduce|
      _FeynAmpDenominator|_LorentzIndex|_ExplicitLorentzIndex|_DiracGamma|
      _SUNIndex|_SUNF|_SUND|_SUNDelta],
    Return[Failure["NeedIntegratedColourProjectedScalar",<||>]]];
  unresolved=Cases[expr,x_ /; RGZMXPVObjectQ[x] && !RGZMXPVScalarQ[x],{0,Infinity}];
  If[unresolved=!={},Return[Failure["TensorPVRemaining",
    <|"\:672a\:7ea6\:5316\:51fd\:6570\:6570"->Length[DeleteDuplicates[unresolved]]|>]]];
  RGZMXProgressUpdate["\:90e8\:95e8"->"RGZ \:89c4\:8303\:6b8b\:5dee\:68c0\:67e5","\:9636\:6bb5"->"\:89c4\:8303\:6b8b\:5dee",
    "\:56fe\:53f7"->0,"\:56fe\:6570"->0,"\:9879\:53f7"->0,"\:603b\:9879\:6570"->0,"\:5b8c\:6210\:9879\:6570"->0,
    "\:72b6\:6001"->"\:8ba1\:7b97\:4e2d","\:9636\:6bb5\:5f00\:59cb\:65f6\:95f4"->AbsoluteTime[]];
  RGZGaugeStep["\:4fdd\:7559\:6781\:70b9\:6807\:7b7e\:ff0c\:5206\:79bb\:4e3b\:79ef\:5206"];
  prep=CheckAbort[TimeConstrained[
    RGZGaugePrepare[expr,reference,prepareSeconds],prepareSeconds,
    Failure["GaugePreparationTimeout",<|
      "\:9636\:6bb5"->"\:5206\:79bb\:3001\:5408\:5e76\:4e3b\:79ef\:5206\:53ca\:89c4\:8303\:53c2\:6570\:66ff\:6362","\:65f6\:9650"->prepareSeconds,
      "\:8bf4\:660e"->"\:9884\:5904\:7406\:8fbe\:5230\:603b\:65f6\:9650\:ff1b\:53ef\:4f20\:5165\:81ea\:80fdT\:5206\:9879\:5e76\:67e5\:770b\:5f53\:524d\:5b50\:6b65\:9aa4\:3002"|>]],
    Failure["GaugeCheckAborted",<|"\:9636\:6bb5"->"\:9884\:5904\:7406"|>]];
  If[FailureQ[prep] || Head[prep]===Missing || prep===$Aborted,
    RGZMXProgressUpdate["\:72b6\:6001"->"\:672a\:5b8c\:6210"];Return[prep]];
  pairs=prep;
  cacheDir=RGZGaugeCacheDirectory[OptionValue["\:7f13\:5b58\:76ee\:5f55"]];
  context=Hash[{"RGZGaugeCoefficient-v2",DownValues[RGZGaugeCoefficient],
    DownValues[RGZGaugePoleSquares],DownValues[RGZLoopPoleMasses]},"SHA256"];
  rgzGaugeProgress=<|"\:5df2\:5b8c\:6210\:9879\:6570"->0,"\:4e3b\:79ef\:5206\:9879\:6570"->Length[pairs],
    "\:7edf\:8ba1"-><||>,"\:5f53\:524d\:9879"->0,"\:7f13\:5b58\:76ee\:5f55"->cacheDir|>;
  rgzGaugeCompletedRows={};rgzGaugeStoppedUnit=Missing["NoStoppedCoefficient"];
  RGZMXProgressUpdate["\:603b\:9879\:6570"->Length[pairs],"\:5b8c\:6210\:9879\:6570"->0];
  rows=Catch[Table[
    RGZMXProgressUpdate["\:9879\:53f7"->n,"\:5b8c\:6210\:9879\:6570"->n-1];
    AssociateTo[rgzGaugeProgress,"\:5f53\:524d\:9879"->n];
    path=None;key=None;
    {time,row}=AbsoluteTiming[CheckAbort[TimeConstrained[
      RGZGaugeStep["\:68c0\:67e5\:7cfb\:6570\:7f13\:5b58"];
      key=Hash[{context,reference,pairs[[n]]},"SHA256"];
      If[StringQ[cacheDir],path=FileNameJoin[{cacheDir,
        "coefficient_"<>IntegerString[key,16]<>".wl"}]];
      result=If[path===None,Missing["NoCache"],RGZGaugeCacheRead[path,key]];
      If[AssociationQ[result],
        Join[result,<|"\:7f16\:53f7"->n,"\:6765\:6e90"->"\:7f13\:5b58\:6062\:590d"|>],
        result=RGZGaugeCoefficient[pairs[[n,2]],pairs[[n,3]],reference];
        If[FailureQ[result],
          <|"\:7f16\:53f7"->n,"\:4e3b\:79ef\:5206"->pairs[[n,1]],"\:539f\:7cfb\:6570"->Missing["NotStored"],
            "\:6b8b\:5dee\:7cfb\:6570"->result,"\:72b6\:6001"->"\:5931\:8d25","\:6765\:6e90"->"\:672c\:6b21\:8ba1\:7b97"|>,
          Join[<|"\:7f16\:53f7"->n,"\:4e3b\:79ef\:5206"->pairs[[n,1]],"\:539f\:7cfb\:6570"->Missing["UseShortCoefficients"],
            "\:72b6\:6001"->RGZGaugeState[result["\:6b8b\:5dee\:7cfb\:6570"]],"\:6765\:6e90"->"\:672c\:6b21\:8ba1\:7b97"|>,result]]],
      seconds,<|"\:7f16\:53f7"->n,"\:4e3b\:79ef\:5206"->pairs[[n,1]],"\:539f\:7cfb\:6570"->Missing["NotPrepared"],
        "\:6b8b\:5dee\:7cfb\:6570"->Missing["CoefficientTimeout"],"\:72b6\:6001"->"\:8d85\:65f6",
        "\:6765\:6e90"->"\:672c\:6b21\:8ba1\:7b97","\:505c\:6b62\:64cd\:4f5c"->Lookup[rgzGaugeProgress,"\:5f53\:524d\:64cd\:4f5c",""]|>],$Aborted]];
    If[row===$Aborted,Throw[Failure["GaugeCheckAborted",
      <|"\:5df2\:5b8c\:6210\:9879\:6570"->n-1,"\:8bf4\:660e"->"\:5df2\:5b8c\:6210\:7cfb\:6570\:5df2\:4fdd\:5b58\:ff1b\:540c\:4e00\:8f93\:5165\:91cd\:65b0\:6bd4\:8f83\:4f1a\:6062\:590d\:7f13\:5b58\:3002"|>],abort]];
    row=Join[row,<|"\:8017\:65f6"->time|>];
    If[row["\:72b6\:6001"]==="\:8d85\:65f6" && TrueQ[stopTimeout],
      operation=Lookup[row,"\:505c\:6b62\:64cd\:4f5c",Lookup[rgzGaugeProgress,"\:5f53\:524d\:64cd\:4f5c",""]];
      (* Keep both short-label branches in memory for one-unit diagnostics.
         Do not spend more time serializing a huge failed input before stopping.
         Successful earlier coefficients are already in the unchanged cache. *)
      rgzGaugeStoppedUnit=<|"\:7f16\:53f7"->n,"\:603b\:9879\:6570"->Length[pairs],
        "\:4e3b\:79ef\:5206"->pairs[[n,1]],"Alpha\:77ed\:7cfb\:6570"->pairs[[n,2]],
        "\:53c2\:8003\:77ed\:7cfb\:6570"->pairs[[n,3]],"\:53c2\:8003\:89c4\:8303"->reference,
        "\:65f6\:9650"->seconds,"\:8017\:65f6"->time,"\:505c\:6b62\:64cd\:4f5c"->operation,
        "\:5df2\:5b8c\:6210\:9879\:6570"->n-1,"\:7f13\:5b58\:8def\:5f84"->path,"\:7f13\:5b58\:8eab\:4efd"->key|>;
      AssociateTo[rgzGaugeProgress,{"\:5df2\:5b8c\:6210\:9879\:6570"->n-1,
        "\:5f53\:524d\:9879\:8017\:65f6"->time,"\:5f53\:524d\:64cd\:4f5c"->operation,
        "\:7edf\:8ba1"->Counts[(#["\:72b6\:6001"]& /@ rgzGaugeCompletedRows)]}];
      RGZMXProgressUpdate["\:72b6\:6001"->"\:8d85\:65f6\:505c\:6b62","\:5b8c\:6210\:9879\:6570"->n-1];
      Throw[Failure["GaugeCoefficientTimeout",<|
        "\:7f16\:53f7"->n,"\:603b\:9879\:6570"->Length[pairs],"\:65f6\:9650"->seconds,
        "\:8017\:65f6"->time,"\:505c\:6b62\:64cd\:4f5c"->operation,"\:5df2\:5b8c\:6210\:9879\:6570"->n-1,
        "\:8bf4\:660e"->"\:5df2\:5728\:7b2c\:4e00\:4e2a\:7cfb\:6570\:8d85\:65f6\:5904\:505c\:6b62\:ff1b\:540e\:7eed\:9879\:5c1a\:672a\:68c0\:67e5\:3002\:5df2\:5b8c\:6210\:7cfb\:6570\:7f13\:5b58\:4fdd\:7559\:ff0c\:505c\:6b62\:9879\:8f93\:5165\:4fdd\:5b58\:5728 rgzGaugeStoppedUnit\:3002"|>],abort]];
    AppendTo[rgzGaugeCompletedRows,row];
    AssociateTo[rgzGaugeProgress,{"\:5df2\:5b8c\:6210\:9879\:6570"->n,
      "\:5f53\:524d\:9879\:8017\:65f6"->time,"\:7edf\:8ba1"->Counts[(#["\:72b6\:6001"]& /@ rgzGaugeCompletedRows)]}];
    If[MemberQ[{"\:5f52\:96f6","\:672a\:5f52\:96f6"},row["\:72b6\:6001"]] && row["\:6765\:6e90"]=!="\:7f13\:5b58\:6062\:590d",
      RGZGaugeStep["\:4fdd\:5b58\:5df2\:5b8c\:6210\:7cfb\:6570"];RGZGaugeCacheWrite[row,path,key]];
    RGZMXProgressUpdate["\:5b8c\:6210\:9879\:6570"->n];row,
    {n,Length[pairs]}],abort];
  If[FailureQ[rows],RGZMXProgressUpdate["\:72b6\:6001"->
    If[rows[[1]]==="GaugeCoefficientTimeout","\:8d85\:65f6\:505c\:6b62","\:5df2\:4e2d\:6b62"]];Return[rows]];
  stats=Counts[(#["\:72b6\:6001"]& /@ rows)];
  state=If[AllTrue[rows,#["\:72b6\:6001"]==="\:5f52\:96f6"&],
    "\:6240\:68c0\:67e5\:8868\:8fbe\:5f0f\:7684\:89c4\:8303\:6b8b\:5dee\:4e3a\:96f6","\:5c1a\:672a\:8bc1\:660e\:89c4\:8303\:65e0\:5173"];
  RGZMXProgressUpdate["\:72b6\:6001"->If[state==="\:6240\:68c0\:67e5\:8868\:8fbe\:5f0f\:7684\:89c4\:8303\:6b8b\:5dee\:4e3a\:96f6","\:5b8c\:6210","\:68c0\:67e5\:5b8c\:6210\:ff0c\:6709\:5f85\:5904\:7406\:9879"]];
  <|"\:53c2\:8003\:89c4\:8303"->reference,"\:4e3b\:79ef\:5206\:9879\:6570"->Length[rows],"\:7edf\:8ba1"->stats,
    "\:72b6\:6001"->state,"\:6b8b\:5dee\:5206\:9879"->rows,
    "\:8bf4\:660e"->"\:6bd4\:8f83 alpha \:4e0e\:53c2\:8003\:89c4\:8303\:ff0c\:5df2\:5305\:542b\:6781\:70b9\:8d28\:91cf\:7684 alpha \:4f9d\:8d56\:ff1b\:4fdd\:7559\:6709\:9650 M\:ff0c\:672a\:81ea\:52a8\:8865\:5145 pinch \:9879\:6216\:53d6 M->0\:3002"|>
];

(* Bounded summary; do not format a Failure's full amplitude or data. *)
RGZGaugeSummary[report_] := Which[
  AssociationQ[report],Print[Grid[{
    {"\:53c2\:8003\:89c4\:8303",report["\:53c2\:8003\:89c4\:8303"]},{"\:4e3b\:79ef\:5206\:9879\:6570",report["\:4e3b\:79ef\:5206\:9879\:6570"]},
    {"\:7edf\:8ba1",report["\:7edf\:8ba1"]},{"\:72b6\:6001",report["\:72b6\:6001"]}},Frame->All,Alignment->Left]],
  FailureQ[report],Print["\:68c0\:67e5\:672a\:5b8c\:6210\:ff1b\:5931\:8d25\:6807\:7b7e\:ff1a",report[[1]]];
    Module[{data=report[[2]],keys},
      keys=Select[{"\:7f16\:53f7","\:603b\:9879\:6570","\:9636\:6bb5","\:505c\:6b62\:64cd\:4f5c","\:8f93\:5165\:5206\:9879",
        "\:65f6\:9650","\:8017\:65f6","\:5df2\:5b8c\:6210\:9879\:6570","\:8bf4\:660e"},KeyExistsQ[data,#]&];
      If[keys=!={},Print[Grid[({#,data[#]}& /@ keys),
        Frame->All,Alignment->Left]]]],
  Head[report]===Missing,Print["\:68c0\:67e5\:672a\:5b8c\:6210\:ff1a",report];
    If[report[[1]]==="PreparationTimeout",
      Print["\:65e7\:7248\:9884\:5904\:7406\:8d85\:65f6\:ff0c\:5c1a\:672a\:8fdb\:5165\:4e3b\:79ef\:5206\:7cfb\:6570\:68c0\:67e5\:3002"]],
  True,Print["\:68c0\:67e5\:672a\:5b8c\:6210\:ff1b\:5934\:90e8\:ff1a",Head[report]]
];
RGZGaugeShow[report_Association,n_Integer] := Module[{rows,row},
  rows=Lookup[report,"\:6b8b\:5dee\:5206\:9879",{}];
  If[n<1 || n>Length[rows],Return[Failure["ResidualRowNumber",<||>]]];
  row=rows[[n]];
  Grid[{{"\:7f16\:53f7",n},{"\:4e3b\:79ef\:5206",TraditionalForm[row["\:4e3b\:79ef\:5206"]]},
    {"\:6b8b\:5dee\:7cfb\:6570",TraditionalForm[row["\:6b8b\:5dee\:7cfb\:6570"]]},
    {"\:72b6\:6001",row["\:72b6\:6001"]}},Frame->All,Alignment->Left]
];

(* v2.4: resumable exact reduction of ONE stopped coefficient.
   No separate-batch zero tests: all nonzero batches are merged pairwise.
   Per-batch and whole-call budgets are separate. Existing coefficient,
   graph and integration cache formats/fingerprints stay unchanged. *)
RGZGaugeBatchChunks[x_List,n_Integer] :=
  Table[Take[x,{j,Min[j+n-1,Length[x]]}],{j,1,Length[x],n}];

RGZGaugeBatchPrepare[a_,b_,reference_,size_Integer] := Module[
  {ca,cb,bases,vars,rules,definitions,radicands,terms,grouped,chunks},
  RGZGaugeStep["\:5206\:6279\:51c6\:5907\:ff1a\:6781\:70b9\:5e73\:65b9\:4e0e\:5171\:540c\:6839\:5f0f\:57fa\:5e95"];
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
  <|"\:5206\:6279"->chunks,"\:6839\:7b26\:53f7"->vars,"\:6839\:5f0f\:5b9a\:4e49"->definitions,
    "\:88ab\:5f00\:65b9\:5f0f"->radicands,"\:52a0\:9879\:6570"->Length[terms],
    "\:540c\:5206\:6bcd\:7ec4\:6570"->Length[grouped],"\:521d\:59cb\:6279\:6570"->Length[chunks]|>
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
    "\:8bf4\:660e"->"\:6839\:5f0f\:5e73\:65b9\:5173\:7cfb\:4f7f\:5206\:6bcd\:4e3a\:96f6\:ff1b\:6b64\:6279\:4e0d\:80fd\:4f5c\:4e3a\:6709\:6548\:62b5\:6d88\:7ed3\:679c\:3002"|>]]];
  Cancel[Together[num/den]]
];

RGZGaugeBatchCacheRead[path_,identity_] := Module[{r},
  If[!StringQ[path] || !FileExistsQ[path],Return[Missing["NoBatchCache"]]];
  r=Quiet[Check[Get[path],$Failed]];
  If[AssociationQ[r] && Lookup[r,"\:8eab\:4efd",None]===identity &&
      KeyExistsQ[r,"\:7ed3\:679c"] && FreeQ[r["\:7ed3\:679c"],_Failure|_Missing|$Failed|$Aborted],
    r["\:7ed3\:679c"],Missing["NoValidBatchCache"]]
];
RGZGaugeBatchCacheWrite[x_,path_,identity_] := Module[{r},
  If[path===None,Return[True]];
  r=CheckAbort[TimeConstrained[
    RGZMXWrite[<|"\:8eab\:4efd"->identity,"\:7ed3\:679c"->x|>,path],10,
    Missing["BatchCacheWriteTimeout"]],Missing["BatchCacheWriteAborted"]];
  If[FailureQ[r] || Head[r]===Missing || r===$Failed || !FileExistsQ[path],
    Failure["BatchCacheWriteFailed",<|
      "\:8bf4\:660e"->"\:5f53\:524d\:6279\:5df2\:8ba1\:7b97\:ff0c\:4f46\:7f13\:5b58\:672a\:4fdd\:5b58\:ff1b\:505c\:6b62\:7ee7\:7eed\:5408\:5e76\:3002"|>],True]
];

Options[RGZGaugeResumeStopped]={"\:5206\:7ec4\:5927\:5c0f"->16,"\:51c6\:5907\:65f6\:9650"->60,
  "\:603b\:65f6\:9650"->1200,"\:7f13\:5b58\:76ee\:5f55"->Automatic};
RGZGaugeResumeStopped[seconds_:60,OptionsPattern[]] := Module[
  {unit,size,prepLimit,totalLimit,root,identity,dir=None,prep,prepPath,
    vars,radicands,definitions,chunks,current,next,level=0,groups,
    run,worker,result,z,physical,row,path,key,cacheSaved=False,stop,
    start=AbsoluteTime[],resumeCount=0,computedCount=0,operation,n,
    mergeContext,mergeDir=None,legacyPrefix=True},
  If[!AssociationQ[rgzGaugeStoppedUnit],
    Return[Failure["NoStoppedCoefficient",<||>]]];
  unit=rgzGaugeStoppedUnit;size=OptionValue["\:5206\:7ec4\:5927\:5c0f"];
  rgzGaugeBatchStoppedNode=Missing["NoStoppedBatchNode"];
  rgzGaugeBatchCurrentNode=Missing["NoCurrentBatchNode"];
  prepLimit=OptionValue["\:51c6\:5907\:65f6\:9650"];totalLimit=OptionValue["\:603b\:65f6\:9650"];
  If[!IntegerQ[size] || size<1 ||
      !And@@(NumericQ[#] && TrueQ[#>0]& /@ {seconds,prepLimit,totalLimit}),
    Return[Failure["GaugeBatchArguments",<||>]]];
  If[!And@@(KeyExistsQ[unit,#]& /@
      {"Alpha\:77ed\:7cfb\:6570","\:53c2\:8003\:77ed\:7cfb\:6570","\:53c2\:8003\:89c4\:8303","\:7f16\:53f7","\:4e3b\:79ef\:5206"}),
    Return[Failure["IncompleteStoppedInput",<||>]]];
  If[OwnValues[\[Alpha]]=!={},
    Return[Failure["AssignedGaugeParameter",<||>]]];
  rgzGaugeBatchProgress=<|"\:7f16\:53f7"->unit["\:7f16\:53f7"],"\:72b6\:6001"->"\:8ba1\:7b97\:4e2d",
    "\:5c42\:53f7"->0,"\:6279\:53f7"->0,"\:8be5\:5c42\:6279\:6570"->0,"\:7f13\:5b58\:6062\:590d\:6279\:6570"->0,
    "\:65b0\:8ba1\:7b97\:6279\:6570"->0,"\:5355\:6279\:65f6\:9650"->seconds,"\:603b\:65f6\:9650"->totalLimit|>;
  RGZMXProgressUpdate["\:90e8\:95e8"->"RGZ \:89c4\:8303\:7cfb\:6570\:5206\:6279\:68c0\:67e5","\:9636\:6bb5"->"\:5206\:6279\:5f52\:7ea6",
    "\:9636\:6bb5\:5f00\:59cb\:65f6\:95f4"->start,"\:72b6\:6001"->"\:8ba1\:7b97\:4e2d","\:56fe\:53f7"->0,"\:56fe\:6570"->0,
    "\:9879\:53f7"->0,"\:603b\:9879\:6570"->0,"\:5b8c\:6210\:9879\:6570"->0];
  worker[] := Catch[
    RGZGaugeStep["\:5206\:6279\:51c6\:5907\:ff1a\:68c0\:67e5\:8f93\:5165\:8eab\:4efd\:4e0e\:7f13\:5b58"];
    root=RGZGaugeCacheDirectory[OptionValue["\:7f13\:5b58\:76ee\:5f55"]];
    identity=Hash[{"RGZGaugeBatch-v2.4",size,
      unit["Alpha\:77ed\:7cfb\:6570"],unit["\:53c2\:8003\:77ed\:7cfb\:6570"],unit["\:53c2\:8003\:89c4\:8303"],
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
        RGZGaugeBatchPrepare[unit["Alpha\:77ed\:7cfb\:6570"],unit["\:53c2\:8003\:77ed\:7cfb\:6570"],
          unit["\:53c2\:8003\:89c4\:8303"],size],prepLimit,Missing["BatchPreparationTimeout"]];
      If[Head[prep]===Missing,Throw[Failure["GaugeBatchPreparationTimeout",<|
        "\:7f16\:53f7"->unit["\:7f16\:53f7"],"\:65f6\:9650"->prepLimit,
        "\:505c\:6b62\:64cd\:4f5c"->"\:5206\:6279\:51c6\:5907","\:8bf4\:660e"->"\:672a\:5f00\:59cb\:6279\:6b21\:901a\:5206\:3002"|>],stop]];
      If[FailureQ[prep],Throw[prep,stop]];
      result=RGZGaugeBatchCacheWrite[prep,prepPath,{identity,"prepare"}];
      If[FailureQ[result],Throw[result,stop]]];
    mergeContext=RGZGaugeMergeContext[];
    If[StringQ[dir],mergeDir=FileNameJoin[{dir,
      "v26_"<>IntegerString[mergeContext,16]}];
      If[!DirectoryQ[mergeDir],Quiet[Check[
        CreateDirectory[mergeDir,CreateIntermediateDirectories->True],
        Throw[Failure["BatchCacheDirectory",<||>],stop]]]]];
    vars=prep["\:6839\:7b26\:53f7"];radicands=prep["\:88ab\:5f00\:65b9\:5f0f"];
    definitions=prep["\:6839\:5f0f\:5b9a\:4e49"];chunks=prep["\:5206\:6279"];
    AssociateTo[rgzGaugeBatchProgress,{
      "\:52a0\:9879\:6570"->prep["\:52a0\:9879\:6570"],"\:521d\:59cb\:6279\:6570"->prep["\:521d\:59cb\:6279\:6570"],
      "\:540c\:5206\:6bcd\:7ec4\:6570"->prep["\:540c\:5206\:6bcd\:7ec4\:6570"],"\:6839\:5f0f\:79cd\:7c7b"->Length[vars],"\:7f13\:5b58\:76ee\:5f55"->dir}];
    run[part_List,lev_Integer,index_Integer,total_Integer] := Module[
      {nodePath,nodeKey,v,elapsed,saved,fromCache=False,oldPath,oldKey,inputHash},
      operation=If[lev===0,"\:5c0f\:6279\:901a\:5206","\:4e24\:4e24\:5408\:5e76"];
      RGZMXProgressUpdate["\:9879\:53f7"->index,"\:603b\:9879\:6570"->total,"\:5b8c\:6210\:9879\:6570"->index-1];
      RGZGaugeStep[operation<>"\:ff1a\:5c42 "<>ToString[lev]<>"\:ff0c\:6279 "<>
        ToString[index]<>" / "<>ToString[total]];
      AssociateTo[rgzGaugeBatchProgress,{
        "\:5c42\:53f7"->lev,"\:6279\:53f7"->index,"\:8be5\:5c42\:6279\:6570"->total,"\:505c\:6b62\:64cd\:4f5c"->operation}];
      rgzGaugeBatchCurrentNode=<|"\:7f16\:53f7"->unit["\:7f16\:53f7"],"\:5c42\:53f7"->lev,
        "\:6279\:53f7"->index,"\:8be5\:5c42\:6279\:6570"->total,"\:8f93\:5165"->part,"\:6839\:7b26\:53f7"->vars,
        "\:88ab\:5f00\:65b9\:5f0f"->radicands,"\:6839\:5f0f\:5b9a\:4e49"->definitions|>;
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
        rgzGaugeBatchStoppedNode=<|"\:7f16\:53f7"->unit["\:7f16\:53f7"],"\:5c42\:53f7"->lev,
          "\:6279\:53f7"->index,"\:8be5\:5c42\:6279\:6570"->total,"\:8f93\:5165"->part,"\:6839\:7b26\:53f7"->vars,
          "\:88ab\:5f00\:65b9\:5f0f"->radicands,"\:6839\:5f0f\:5b9a\:4e49"->definitions,
          "\:505c\:6b62\:64cd\:4f5c"->Lookup[rgzGaugeProgress,"\:5f53\:524d\:64cd\:4f5c",operation]|>;
        Throw[Failure["GaugeBatchTimeout",<|
        "\:7f16\:53f7"->unit["\:7f16\:53f7"],"\:5c42\:53f7"->lev,"\:6279\:53f7"->index,
        "\:8be5\:5c42\:6279\:6570"->total,"\:505c\:6b62\:64cd\:4f5c"->
          Lookup[rgzGaugeProgress,"\:5f53\:524d\:64cd\:4f5c",operation],"\:65f6\:9650"->seconds,
        "\:8017\:65f6"->elapsed,"\:65b0\:8ba1\:7b97\:6279\:6570"->computedCount,
        "\:7f13\:5b58\:6062\:590d\:6279\:6570"->resumeCount,
        "\:8bf4\:660e"->"\:5728\:5f53\:524d\:6279\:8d85\:65f6\:5904\:505c\:6b62\:ff1b\:540c\:4e00\:8f93\:5165\:91cd\:8dd1\:53ef\:590d\:7528\:5df2\:5b8c\:6210\:6279\:6b21\:3002"|>],stop]];
      If[FailureQ[v] || v===$Failed || v===$Aborted,
        Throw[Failure["GaugeBatchReductionFailed",<|
          "\:7f16\:53f7"->unit["\:7f16\:53f7"],"\:5c42\:53f7"->lev,"\:6279\:53f7"->index,
          "\:539f\:56e0"->v|>],stop]];
      (* Cached zero is a valid completed node, not a missing result. *)
      If[!TrueQ[fromCache],
        saved=RGZGaugeBatchCacheWrite[v,nodePath,nodeKey];
        If[FailureQ[saved],Throw[saved,stop]];computedCount++];
      AssociateTo[rgzGaugeBatchProgress,{
        "\:65b0\:8ba1\:7b97\:6279\:6570"->computedCount,"\:7f13\:5b58\:6062\:590d\:6279\:6570"->resumeCount,
        "\:5f53\:524d\:6279\:8017\:65f6"->elapsed}];
      RGZMXProgressUpdate["\:5b8c\:6210\:9879\:6570"->index];v
    ];
    current=Table[run[chunks[[n]],0,n,Length[chunks]],{n,Length[chunks]}];
    current=DeleteCases[current,0];
    While[Length[current]>1,
      level++;groups=RGZGaugeBatchChunks[current,2];
      next=Table[run[groups[[n]],level,n,Length[groups]],{n,Length[groups]}];
      current=DeleteCases[next,0]];
    z=If[current==={},0,First[current]];
    RGZGaugeStep["\:5206\:6279\:5b8c\:6210\:ff1a\:6062\:590d\:539f\:6839\:5f0f\:5e76\:4fdd\:5b58\:5355\:9879\:7ed3\:679c"];
    physical=z/.definitions;
    row=<|"\:7f16\:53f7"->unit["\:7f16\:53f7"],"\:4e3b\:79ef\:5206"->unit["\:4e3b\:79ef\:5206"],
      "\:539f\:7cfb\:6570"->Missing["UseShortCoefficients"],"\:6b8b\:5dee\:7cfb\:6570"->physical,
      "\:6839\:5f0f\:7b26\:53f7\:7cfb\:6570"->z,"\:6839\:5f0f\:5b9a\:4e49"->definitions,
      "\:72b6\:6001"->RGZGaugeState[physical],"\:6765\:6e90"->"\:5206\:6279\:7cbe\:786e\:5f52\:7ea6",
      "\:8017\:65f6"->AbsoluteTime[]-start|>;
    path=Lookup[unit,"\:7f13\:5b58\:8def\:5f84",None];key=Lookup[unit,"\:7f13\:5b58\:8eab\:4efd",None];
    rgzGaugeBatchLastCoefficient=row;
    If[!TrueQ[physical===0] && LeafCount[z]>5000,
      Throw[Failure["GaugeScalarCoefficientPending",<|
        "\:7f16\:53f7"->unit["\:7f16\:53f7"],"\:505c\:6b62\:64cd\:4f5c"->"\:6700\:7ec8\:53c2\:6570\:7cfb\:6570\:5316\:7b80\:5c1a\:672a\:5b8c\:6210",
        "\:8bf4\:660e"->"\:6839\:5f0f\:5408\:5e76\:7ed3\:679c\:5df2\:4fdd\:7559\:ff0c\:4f46\:5927\:578b\:53c2\:6570\:7cfb\:6570\:672a\:5b8c\:6210\:89c4\:8303\:5316\:ff1b\:672a\:5199\:5165\:5df2\:5b8c\:6210\:7cfb\:6570\:7f13\:5b58\:ff0c\:4e0d\:80fd\:636e\:6b64\:5ba3\:79f0\:975e\:96f6\:6216\:89c4\:8303\:65e0\:5173\:3002"|>],stop]];
    (* Verify the original v2 coefficient cache identity before writing it.
       RGZGaugeCoefficient itself has not changed in this release. *)
    If[StringQ[path] && key===Hash[{
        Hash[{"RGZGaugeCoefficient-v2",DownValues[RGZGaugeCoefficient],
          DownValues[RGZGaugePoleSquares],DownValues[RGZLoopPoleMasses]},"SHA256"],
        unit["\:53c2\:8003\:89c4\:8303"],{unit["\:4e3b\:79ef\:5206"],unit["Alpha\:77ed\:7cfb\:6570"],unit["\:53c2\:8003\:77ed\:7cfb\:6570"]}},"SHA256"],
      RGZGaugeCacheWrite[row,path,key];
      cacheSaved=TimeConstrained[
        AssociationQ[RGZGaugeCacheRead[path,key]],10,False]];
    rgzGaugeBatchLastCoefficient=row;
    <|"\:72b6\:6001"->"\:5355\:9879\:5b8c\:6210","\:7f16\:53f7"->unit["\:7f16\:53f7"],
      "\:7cfb\:6570\:72b6\:6001"->row["\:72b6\:6001"],"\:52a0\:9879\:6570"->prep["\:52a0\:9879\:6570"],
      "\:521d\:59cb\:6279\:6570"->prep["\:521d\:59cb\:6279\:6570"],"\:6839\:5f0f\:79cd\:7c7b"->Length[vars],
      "\:65b0\:8ba1\:7b97\:6279\:6570"->computedCount,"\:7f13\:5b58\:6062\:590d\:6279\:6570"->resumeCount,
      "\:8017\:65f6"->AbsoluteTime[]-start,"\:5df2\:4fdd\:5b58\:5230\:539f\:7cfb\:6570\:7f13\:5b58"->cacheSaved,
      "\:8bf4\:660e"->"\:53ea\:5b8c\:6210\:5f53\:524d\:505c\:6b62\:9879\:ff1b\:4e0d\:540c\:6279\:6b21\:5df2\:76f8\:52a0\:3002\:5176\:4f59\:4e3b\:79ef\:5206\:9879\:5c1a\:9700\:68c0\:67e5\:3002"|>,stop];
  result=CheckAbort[TimeConstrained[worker[],totalLimit,
    Failure["GaugeBatchTotalTimeout",<|
      "\:7f16\:53f7"->unit["\:7f16\:53f7"],"\:65f6\:9650"->totalLimit,
      "\:5c42\:53f7"->Lookup[rgzGaugeBatchProgress,"\:5c42\:53f7",0],
      "\:6279\:53f7"->Lookup[rgzGaugeBatchProgress,"\:6279\:53f7",0],
      "\:8be5\:5c42\:6279\:6570"->Lookup[rgzGaugeBatchProgress,"\:8be5\:5c42\:6279\:6570",0],
      "\:8bf4\:660e"->"\:8fbe\:5230\:672c\:6b21\:603b\:65f6\:9650\:ff1b\:5df2\:5b8c\:6210\:6279\:6b21\:7f13\:5b58\:4fdd\:7559\:3002"|>]],
    Failure["GaugeBatchAborted",<|"\:7f16\:53f7"->unit["\:7f16\:53f7"],
      "\:8bf4\:660e"->"\:5df2\:4e2d\:6b62\:ff1b\:5df2\:5b8c\:6210\:6279\:6b21\:7f13\:5b58\:4fdd\:7559\:3002"|>]];
  If[FailureQ[result] && !AssociationQ[rgzGaugeBatchStoppedNode] &&
      AssociationQ[rgzGaugeBatchCurrentNode],
    rgzGaugeBatchStoppedNode=Join[rgzGaugeBatchCurrentNode,<|
      "\:505c\:6b62\:64cd\:4f5c"->Lookup[rgzGaugeProgress,"\:5f53\:524d\:64cd\:4f5c",""]|>]];
  AssociateTo[rgzGaugeBatchProgress,{
    "\:72b6\:6001"->If[FailureQ[result],"\:5df2\:505c\:6b62","\:5355\:9879\:5b8c\:6210"],
    "\:8017\:65f6"->AbsoluteTime[]-start}];
  RGZMXProgressUpdate["\:72b6\:6001"->If[FailureQ[result],"\:5df2\:505c\:6b62","\:5355\:9879\:5b8c\:6210"]];
  result
];

RGZGaugeBatchSummary[report_] := Module[{data,keys},
  data=Which[AssociationQ[report],report,FailureQ[report],report[[2]],
    True,Return[Print["\:5206\:6279\:7ed3\:679c\:5934\:90e8\:ff1a",Head[report]]]];
  If[FailureQ[report],Print["\:5206\:6279\:505c\:6b62\:6807\:7b7e\:ff1a",report[[1]]]];
  keys=Select[{"\:72b6\:6001","\:7f16\:53f7","\:7cfb\:6570\:72b6\:6001","\:52a0\:9879\:6570","\:521d\:59cb\:6279\:6570","\:6839\:5f0f\:79cd\:7c7b",
    "\:5c42\:53f7","\:6279\:53f7","\:8be5\:5c42\:6279\:6570","\:505c\:6b62\:64cd\:4f5c","\:65f6\:9650","\:8017\:65f6",
    "\:65b0\:8ba1\:7b97\:6279\:6570","\:7f13\:5b58\:6062\:590d\:6279\:6570","\:5df2\:4fdd\:5b58\:5230\:539f\:7cfb\:6570\:7f13\:5b58","\:8bf4\:660e"},
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
  node=rgzGaugeBatchStoppedNode;inputs=node["\:8f93\:5165"];vars=node["\:6839\:7b26\:53f7"];
  measure[e_] := {LeafCount[e],LeafCount[Numerator[e]],
    LeafCount[Denominator[e]],If[vars==={},0,
      Max[Exponent[Numerator[e],#]& /@ vars]],
    If[vars==={},0,Max[Exponent[Denominator[e],#]& /@ vars]]};
  cols=Table[z=CheckAbort[TimeConstrained[measure[inputs[[j]]],seconds,
    Missing["DiagnosticTimeout"]],Missing["DiagnosticAborted"]];
    If[ListQ[z],z,ConstantArray[z,5]],{j,Length[inputs]}];
  Grid[Prepend[Transpose[Prepend[cols,
      {"\:8868\:8fbe\:5f0f\:8282\:70b9\:6570","\:5206\:5b50\:8282\:70b9\:6570","\:5206\:6bcd\:8282\:70b9\:6570","\:5206\:5b50\:6700\:9ad8\:6839\:5e42","\:5206\:6bcd\:6700\:9ad8\:6839\:5e42"}]],
    Prepend[Table["\:8f93\:5165 "<>ToString[j],{j,Length[inputs]}],"\:68c0\:67e5\:9879\:76ee"]],
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
    "\:8bf4\:660e"->"\:8f93\:5165\:542b\:6839\:5f0f\:53d8\:91cf\:7684\:975e\:591a\:9879\:5f0f\:4f9d\:8d56\:ff1b\:4e0d\:628a\:5931\:8d25\:5f53\:4f5c\:96f6\:3002"|>]]];
  rootRules=Quiet[Check[CoefficientRules[x,vars],$Failed]];
  If[!ListQ[rootRules] || !AllTrue[rootRules,
      MatchQ[#,_Rule] && ListQ[First[#]] && Length[First[#]]===Length[vars] &&
      VectorQ[First[#],IntegerQ[#] && #>=0&] && FreeQ[Last[#],rootPattern]&],
    Return[Failure["NeedPolynomialInRoots",<|
      "\:8bf4\:660e"->"\:65e0\:6cd5\:6309\:6839\:5f0f\:53d8\:91cf\:63d0\:53d6\:591a\:9879\:5f0f\:ff1b\:4e0d\:628a\:5931\:8d25\:5f53\:4f5c\:96f6\:3002"|>]]];
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
  <|"\:591a\:9879\:5f0f"->RGZGaugeSparseGroup[tidiedPairs],"\:5f85\:53c2\:6570\:7cfb\:6570\:6570"->pending|>
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
  rgzGaugeSparseProgress=<|"\:6839\:5f0f\:53d8\:91cf\:6570"->dim|>;
  RGZGaugeStep["\:7a00\:758f\:5408\:5e76\:ff1a\:8bfb\:53d6\:5206\:5b50\:548c\:5206\:6bcd"];
  {leftNumerator,rightNumerator}=Numerator /@ part;{leftDenominator,rightDenominator}=Denominator /@ part;
  If[leftDenominator===rightDenominator,leftDenominatorMultiplier=1;rightDenominatorMultiplier=1,
    RGZGaugeStep["\:7a00\:758f\:5408\:5e76\:ff1a\:516c\:5171\:5206\:6bcd\:68c0\:6d4b\:ff08\:6700\:591a3\:79d2\:ff09"];
    denominatorGCD=Quiet[Check[TimeConstrained[PolynomialGCD[leftDenominator,rightDenominator],3,
      Missing["DenominatorGCDTimeout"]],$Failed]];
    If[Head[denominatorGCD]===Missing || denominatorGCD===$Failed || TrueQ[denominatorGCD===0] ||
        !FreeQ[denominatorGCD,_PolynomialGCD],denominatorGCD=1];
    RGZGaugeStep["\:7a00\:758f\:5408\:5e76\:ff1a\:6784\:9020\:5206\:6bcd\:500d\:6570"];
    If[denominatorGCD===1,leftDenominatorMultiplier=leftDenominator;rightDenominatorMultiplier=rightDenominator,leftDenominatorMultiplier=Cancel[leftDenominator/denominatorGCD];rightDenominatorMultiplier=Cancel[rightDenominator/denominatorGCD]]];
  inputs={leftNumerator,rightNumerator,leftDenominatorMultiplier,rightDenominatorMultiplier,leftDenominator};
  ps=Table[RGZGaugeStep["\:7a00\:758f\:5408\:5e76\:ff1a\:63d0\:53d6\:6839\:5f0f\:5355\:9879\:5f0f "<>ToString[j]<>" / 5"];
    RGZGaugeSparseFrom[inputs[[j]],vars],{j,5}];
  If[AnyTrue[ps,FailureQ],Return[First[Select[ps,FailureQ]]]];
  AssociateTo[rgzGaugeSparseProgress,{
    "\:8f93\:51651\:5206\:5b50\:5355\:9879\:5f0f\:6570"->Length[ps[[1]]],"\:8f93\:51652\:5206\:5b50\:5355\:9879\:5f0f\:6570"->Length[ps[[2]]]}];
  relations=RGZGaugeSparseFrom[#,vars]& /@ radicands;
  If[AnyTrue[relations,FailureQ],Return[First[Select[relations,FailureQ]]]];
  RGZGaugeStep["\:7a00\:758f\:5408\:5e76\:ff1a\:5206\:6bcd\:6839\:5f0f\:5e73\:65b9\:7ea6\:5316"];
  den=RGZGaugeSparseReduce[RGZGaugeSparseTimes[ps[[5]],ps[[4]]],relations,dim];
  tidyD=RGZGaugeSparseTidy[den];den=tidyD["\:591a\:9879\:5f0f"];
  If[den==={},Return[Failure["PoleRelationSingularity",<||>]]];
  RGZGaugeStep["\:7a00\:758f\:5408\:5e76\:ff1a\:5206\:5b50\:5355\:9879\:5f0f\:76f8\:4e58\:4e0e\:6839\:5f0f\:5e73\:65b9\:7ea6\:5316"];
  num=RGZGaugeSparseGroup[Join[
    RGZGaugeSparseTimes[ps[[1]],ps[[4]]],
    RGZGaugeSparseTimes[ps[[2]],ps[[3]]]]];
  num=RGZGaugeSparseReduce[num,relations,dim];
  RGZGaugeStep["\:7a00\:758f\:5408\:5e76\:ff1a\:68c0\:67e5\:5c0f\:578b\:53c2\:6570\:7cfb\:6570\:ff0c\:4fdd\:7559\:5927\:578b\:7cfb\:6570"];
  tidyN=RGZGaugeSparseTidy[num];num=tidyN["\:591a\:9879\:5f0f"];
  AssociateTo[rgzGaugeSparseProgress,{
    "\:7ed3\:679c\:5206\:5b50\:5355\:9879\:5f0f\:6570"->Length[num],"\:7ed3\:679c\:5206\:6bcd\:5355\:9879\:5f0f\:6570"->Length[den],
    "\:5f85\:53c2\:6570\:7cfb\:6570\:6570"->tidyN["\:5f85\:53c2\:6570\:7cfb\:6570\:6570"]+tidyD["\:5f85\:53c2\:6570\:7cfb\:6570\:6570"]}];
  If[num==={},
    If[tidyD["\:5f85\:53c2\:6570\:7cfb\:6570\:6570"]>0,
      Return[Failure["UnverifiedMergeDenominator",<||>]],Return[0]]];
  RGZGaugeSparseTo[num,vars]/RGZGaugeSparseTo[den,vars]
];

RGZGaugeBatchMerge[part_List,vars_List,radicands_List] := Module[{key},
  key=Hash[{RGZGaugeMergeContext[],part,vars,radicands},"SHA256"];
  If[AssociationQ[rgzGaugeSparseTestMemo] &&
      Lookup[rgzGaugeSparseTestMemo,"\:8eab\:4efd",None]===key,
    rgzGaugeSparseProgress=rgzGaugeSparseTestMemo["\:8bca\:65ad"];
    Return[rgzGaugeSparseTestMemo["\:7ed3\:679c"]]];
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
  RGZMXProgressUpdate["\:90e8\:95e8"->"RGZ \:505c\:6b62\:8282\:70b9\:5355\:72ec\:6d4b\:8bd5","\:9636\:6bb5"->"\:7a00\:758f\:5408\:5e76",
    "\:9636\:6bb5\:5f00\:59cb\:65f6\:95f4"->AbsoluteTime[],"\:72b6\:6001"->"\:8ba1\:7b97\:4e2d", "\:56fe\:53f7"->0,"\:56fe\:6570"->0,
    "\:9879\:53f7"->1,"\:603b\:9879\:6570"->1,"\:5b8c\:6210\:9879\:6570"->0];
  {time,result}=AbsoluteTiming[CheckAbort[TimeConstrained[
    key=Hash[{RGZGaugeMergeContext[],node["\:8f93\:5165"],node["\:6839\:7b26\:53f7"],
      node["\:88ab\:5f00\:65b9\:5f0f"]},"SHA256"];
    RGZGaugeSparseMergeCore[node["\:8f93\:5165"],node["\:6839\:7b26\:53f7"],node["\:88ab\:5f00\:65b9\:5f0f"]],
    seconds,Missing["SparseMergeTimeout"]],$Aborted]];
  If[Head[result]===Missing || result===$Aborted || FailureQ[result],
    RGZMXProgressUpdate["\:72b6\:6001"->"\:5df2\:505c\:6b62"];
    Return[Failure["GaugeSparseMergeStopped",<|
      "\:7f16\:53f7"->node["\:7f16\:53f7"],"\:5c42\:53f7"->node["\:5c42\:53f7"],"\:6279\:53f7"->node["\:6279\:53f7"],
      "\:505c\:6b62\:64cd\:4f5c"->Lookup[rgzGaugeProgress,"\:5f53\:524d\:64cd\:4f5c","\:7a00\:758f\:5408\:5e76"],
      "\:65f6\:9650"->seconds,"\:8017\:65f6"->time,
      "\:539f\:56e0\:6807\:7b7e"->Which[FailureQ[result],result[[1]],
        Head[result]===Missing,result[[1]],True,"\:5df2\:4e2d\:6b62"],"\:539f\:56e0"->result|>]]];
  rgzGaugeSparseTestMemo=<|"\:8eab\:4efd"->key,"\:7ed3\:679c"->result,"\:8bca\:65ad"->rgzGaugeSparseProgress|>;
  RGZMXProgressUpdate["\:72b6\:6001"->"\:5355\:8282\:70b9\:5b8c\:6210","\:5b8c\:6210\:9879\:6570"->1];
  Join[<|"\:72b6\:6001"->"\:5355\:8282\:70b9\:5b8c\:6210","\:7f16\:53f7"->node["\:7f16\:53f7"],"\:5c42\:53f7"->node["\:5c42\:53f7"],
    "\:6279\:53f7"->node["\:6279\:53f7"],"\:8017\:65f6"->time,"\:7ed3\:679c\:8282\:70b9\:6570"->LeafCount[result],
    "\:662f\:5426\:5b57\:9762\:5f52\:96f6"->TrueQ[result===0]|>,rgzGaugeSparseProgress]
];
RGZGaugeSparseSummary[report_] := Module[{data},
  data=Which[AssociationQ[report],report,FailureQ[report],
    Print["\:5355\:8282\:70b9\:505c\:6b62\:6807\:7b7e\:ff1a",report[[1]]];KeyDrop[report[[2]],{"\:539f\:56e0"}],
    True,Return[Print["\:7ed3\:679c\:5934\:90e8\:ff1a",Head[report]]]];
  Print[Grid[({#,data[#]}& /@ Keys[data]),Frame->All,Alignment->Left]]
];

Print["\:5df2\:52a0\:8f7d\:89c4\:8303\:7cfb\:6570\:68c0\:67e5 v2.6\:ff1a\:53ea\:5c55\:5f00\:6839\:5f0f\:5355\:9879\:5f0f\:ff1b\:5148 RGZGaugeTestStoppedMerge[60] \:5355\:72ec\:6d4b\:8bd5\:505c\:6b62\:8282\:70b9\:3002"] ;
