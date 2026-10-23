(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* RGZ stopped-coefficient exact reduction, v1.3.
   Requires RGZ_Mixed_Gauge_Check_v2.wl and the mixed workflow already loaded.
   Preserves ALL graph, integration and old batch caches.
   Fixed denominator: start from original summands, factor denominators once,
   take MAXIMUM powers, and add polynomial numerators over that one denominator.
   The included fast result applies ONLY to the byte-verified uploaded WXF
   and a matching current stopped coefficient. It is an independently derived
   exact result, not a fitted subtraction or a numeric zero test.
   Base reducer: one gauge branch. The v1.1 continuation below checks both branches and all prepared coefficients.
   A nonzero coefficient is not, by itself, a proof about the full S-matrix.
*)
ClearAll[RGZGaugeFixedDenResume,RGZGaugeFixedSummary,
  RGZGaugeFixedCoordinates,RGZGaugeFixedPrepare,RGZGaugeFixedFinish,
  RGZGaugeFixedVerified,RGZGaugeFixedUnit1Formula];

(* No personal uploaded-input shortcut is distributed in this package. *)
RGZGaugeFixedVerified[___] := Missing["DisabledInCleanProject"];

RGZGaugeFixedCoordinates[raw_] := Module[
  {v,ss,xx,yy,ww,zz,tt,aa,bb,dd,bases,rootRules,terms,common,restore},
  v=Table[Symbol["RGZGaugeFixedAlgebra`"<>name],
    {name,{"s","x","y","w","z1","z3","a","b","d"}}];
  If[AnyTrue[v,Function[q,OwnValues[q]=!={},HoldAll]],
    Return[Failure["AssignedFixedCoordinate",<||>]]];
  {ss,xx,yy,ww,zz,tt,aa,bb,dd}=v;
  bases={4 mG^4-(m^2-\:03bc^2)^2,
    mG^4-(m^2-\:03bc^2)^2/4,\:03b1 M^4};
  rootRules=MapThread[Function[{base,root},With[{bb0=base,rr0=root},
    HoldPattern[Power[bb0,k_Rational]] /; Denominator[k]===2 :>
      rr0^Numerator[k]]],
    {bases,{-2 I (tt-(xx+yy)/2),-I (tt-(xx+yy)/2),I zz}}];
  (* Cancel/partial fractions in full NAV expand the discriminant.
     Expansion changes only polynomial spelling, never a root branch. *)
  rootRules=Join[rootRules,MapThread[Function[{base,root},With[{bb0=Expand[base],rr0=root},
    HoldPattern[Power[bb0,k_Rational]] /; Denominator[k]===2 :>
      rr0^Numerator[k]]],
    {bases,{-2 I (tt-(xx+yy)/2),-I (tt-(xx+yy)/2),I zz}}]];
  common=Pi^2 SMP["g_s"]^2;
  terms=If[Head[raw]===Plus,List@@raw,{raw}];
  terms=(#/common& /@ terms)/.rootRules;
  terms=terms/.{
    HoldPattern[rgzLoopM1^k_Integer] /; EvenQ[k] :> zz^(k/2),
    HoldPattern[rgzLoopM2^k_Integer] /; EvenQ[k] :> (-zz)^(k/2),
    HoldPattern[rgzLoopM3^k_Integer] /; EvenQ[k] :> tt^(k/2),
    HoldPattern[rgzLoopM4^k_Integer] /; EvenQ[k] :> (xx+yy-tt)^(k/2),
    HoldPattern[m^k_Integer] /; EvenQ[k] :> xx^(k/2),
    HoldPattern[\:03bc^k_Integer] /; EvenQ[k] :> yy^(k/2),
    HoldPattern[M^k_Integer] /; Mod[k,4]===0 :> ww^(k/4),
    HoldPattern[mG^k_Integer] /; Mod[k,4]===0 :>
      (-(tt-xx)(tt-yy))^(k/4),
    HoldPattern[System`N^k_] /; IntegerQ[2 k] :> aa^(2 k),
    HoldPattern[SUNN^k_] /; IntegerQ[2 k] :> bb^(2 k),
    HoldPattern[SPD[p,p]] :> ss,
    HoldPattern[Pair[Momentum[p,D],Momentum[p,D]]] :> ss,
    HoldPattern[Pair[Momentum[p],Momentum[p]]] :> ss,
    \:03b1 :> -zz^2/ww,System`N :> aa^2,SUNN :> bb^2,D :> dd};
  If[!FreeQ[terms,rgzLoopM1|rgzLoopM2|rgzLoopM3|rgzLoopM4] ||
      Cases[terms,HoldPattern[Power[_,k_Rational]] /;
        Denominator[k]=!=1,Infinity]=!={},
    Return[Failure["UnsupportedFixedCoordinateRoot",<|
      "\:8bf4\:660e"->"\:8f93\:5165\:542b\:672a\:652f\:6301\:7684\:6839\:5f0f\:6216\:5947\:6b21\:6781\:70b9\:8d28\:91cf\:ff1b\:672a\:7ee7\:7eed\:5f52\:7ea6\:3002"|>]]];
  restore={ss->SPD[p,p],xx->m^2,yy->\:03bc^2,ww->M^4,
    zz->-I Sqrt[\:03b1 M^4],
    tt->(m^2+\:03bc^2)/2+I Sqrt[mG^4-(m^2-\:03bc^2)^2/4],
    aa->Sqrt[System`N],bb->Sqrt[SUNN],dd->D};
  <|"\:52a0\:9879"->terms,"\:53d8\:91cf"->v,"\:8fd8\:539f\:89c4\:5219"->restore,"\:516c\:5171\:56e0\:5b50"->common|>
];

RGZGaugeFixedPrepare[coords_Association] := Module[
  {terms=coords["\:52a0\:9879"],vars=coords["\:53d8\:91cf"],factor,packets,groups,
    groups2,factors,maxima,lcd,q,fl,c,num,j,bag},
  factor[e_] := factor[e]=FactorList[e];
  packets=Table[
    If[Mod[j,100]===1,RGZMXProgressUpdate[
      "\:9879\:53f7"->j,"\:603b\:9879\:6570"->Length[terms],"\:5b8c\:6210\:9879\:6570"->j-1];
      RGZGaugeStep["\:56fa\:5b9a\:5206\:6bcd\:ff1a\:6574\:7406\:539f\:59cb\:52a0\:9879 "<>ToString[j]<>" / "<>
        ToString[Length[terms]]]];
    q=Together[terms[[j]]];num=Numerator[q];
    If[!PolynomialQ[num,vars] || !PolynomialQ[Denominator[q],vars],
      Return[Failure["NonPolynomialFixedCoordinate",<|"\:52a0\:9879\:53f7"->j|>]]];
    fl=factor[Denominator[q]];c=fl[[1,1]]^fl[[1,2]];
    bag=SortBy[Rest[fl],First];{num/c,bag},
    {j,Length[terms]}];
  groups=GatherBy[packets,Last];
  groups2=({Expand[Total[#[[All,1]]]],#[[1,2]]}& /@ groups);
  groups2=Select[groups2,!TrueQ[First[#]===0]&];
  factors=DeleteDuplicates[Flatten[(First /@ #[[2]]& /@ groups2),1]];
  maxima=Table[Max[Prepend[Flatten[
      Cases[#[[2]],{f_,k_Integer} /; SameQ[f,factors[[j]]] :> k]& /@
        groups2],0]],{j,Length[factors]}];
  lcd=Times@@MapThread[Power,{factors,maxima}];
  Join[coords,<|"\:5206\:7ec4"->groups2,"\:5206\:6bcd\:56e0\:5b50"->factors,
    "\:6700\:9ad8\:5e42"->maxima,"\:516c\:5171\:5206\:6bcd"->lcd,"\:539f\:59cb\:52a0\:9879\:6570"->Length[terms]|>]
];

RGZGaugeFixedFinish[unit_Association,phi_,details_Association] := Module[
  {row,path,key,expected,saved=False},
  If[!FreeQ[phi,Indeterminate|ComplexInfinity|_DirectedInfinity|
      $Failed|$Aborted|_Failure|_Missing],
    Return[Failure["InvalidFixedResult",<||>]]];
  row=<|"\:7f16\:53f7"->unit["\:7f16\:53f7"],"\:4e3b\:79ef\:5206"->unit["\:4e3b\:79ef\:5206"],
    "\:539f\:7cfb\:6570"->Missing["UseShortCoefficients"],"\:6b8b\:5dee\:7cfb\:6570"->phi,
    "\:72b6\:6001"->RGZGaugeState[phi],"\:6765\:6e90"->details["\:6765\:6e90"],
    "\:8017\:65f6"->Lookup[details,"\:8017\:65f6",Missing["PreviouslyVerifiedResult"]]|>;
  rgzGaugeFixedCoefficient=row;rgzGaugeBatchLastCoefficient=row;
  path=Lookup[unit,"\:7f13\:5b58\:8def\:5f84",None];key=Lookup[unit,"\:7f13\:5b58\:8eab\:4efd",None];
  expected=Hash[{
    Hash[{"RGZGaugeCoefficient-v2",DownValues[RGZGaugeCoefficient],
      DownValues[RGZGaugePoleSquares],DownValues[RGZLoopPoleMasses]},"SHA256"],
    unit["\:53c2\:8003\:89c4\:8303"],{unit["\:4e3b\:79ef\:5206"],unit["Alpha\:77ed\:7cfb\:6570"],
      unit["\:53c2\:8003\:77ed\:7cfb\:6570"]}},"SHA256"];
  If[StringQ[path] && key===expected,
    RGZGaugeCacheWrite[row,path,key];
    saved=TimeConstrained[AssociationQ[RGZGaugeCacheRead[path,key]],10,False]];
  RGZMXProgressUpdate["\:72b6\:6001"->"\:5355\:9879\:5b8c\:6210"];
  Join[<|"\:72b6\:6001"->"\:5355\:9879\:5b8c\:6210","\:7f16\:53f7"->unit["\:7f16\:53f7"],
    "\:7cfb\:6570\:72b6\:6001"->row["\:72b6\:6001"],"\:7ed3\:679c\:8282\:70b9\:6570"->LeafCount[phi],
    "\:5df2\:4fdd\:5b58\:5230\:539f\:7cfb\:6570\:7f13\:5b58"->saved,
    "\:8bf4\:660e"->"\:53ea\:5b8c\:6210\:5f53\:524d\:7cfb\:6570\:ff1b\:672a\:5f52\:96f6\:4e0d\:7b49\:540c\:4e8e\:5b8c\:6574 S \:77e9\:9635\:89c4\:8303\:4f9d\:8d56\:3002"|>,
    KeyDrop[details,{"\:7ed3\:679c"}]]
];

Options[RGZGaugeFixedDenResume]={"\:5df2\:9a8c\:8bc1\:8f93\:5165"->Automatic,
  "\:51c6\:5907\:65f6\:9650"->600,"\:603b\:65f6\:9650"->1200,"\:7f13\:5b58\:76ee\:5f55"->Automatic,
  "\:4fdd\:5b58\:95f4\:9694"->25};
RGZGaugeFixedDenResume[seconds_:60,OptionsPattern[]] := Module[
  {unit,file,prepLimit,totalLimit,every,root,dir=None,path=None,id,work,
    prep,num,index=0,group,exponents,multiplier,contribution,z,physical,
    details,vars,xx,yy,zz,tt,ww,alphaVar,tVar,restoreRules,worker,stop,
    answer,verified,coords,start=AbsoluteTime[],write,checkpoint,reduced},
  If[!AssociationQ[rgzGaugeStoppedUnit],
    Return[Failure["NoStoppedCoefficient",<||>]]];
  unit=rgzGaugeStoppedUnit;file=OptionValue["\:5df2\:9a8c\:8bc1\:8f93\:5165"];
  prepLimit=OptionValue["\:51c6\:5907\:65f6\:9650"];totalLimit=OptionValue["\:603b\:65f6\:9650"];
  every=OptionValue["\:4fdd\:5b58\:95f4\:9694"];
  If[!And@@(NumericQ[#] && TrueQ[#>0]& /@ {seconds,prepLimit,totalLimit}) ||
      !IntegerQ[every] || every<1,Return[Failure["FixedDenArguments",<||>]]];
  If[OwnValues[\:03b1]=!={},Return[Failure["AssignedGaugeParameter",<||>]]];
  If[Lookup[unit,"\:53c2\:8003\:77ed\:7cfb\:6570",Missing["Absent"]]=!=0,
    Return[Failure["FixedDenReferenceNotZero",<|
      "\:8bf4\:660e"->"\:672c\:65b9\:6cd5\:76ee\:524d\:53ea\:5904\:7406\:53c2\:8003\:77ed\:7cfb\:6570\:4e3a\:5b57\:9762\:96f6\:7684\:505c\:6b62\:9879\:ff1b\:672a\:6539\:53d8\:53c2\:8003\:89c4\:8303\:3002"|>]]];
  RGZMXProgressUpdate["\:90e8\:95e8"->"RGZ \:56fa\:5b9a\:5206\:6bcd\:7cfb\:6570\:68c0\:67e5","\:9636\:6bb5"->"\:56fa\:5b9a\:5206\:6bcd\:5f52\:7ea6",
    "\:9636\:6bb5\:5f00\:59cb\:65f6\:95f4"->start,"\:72b6\:6001"->"\:8ba1\:7b97\:4e2d","\:56fe\:53f7"->0,"\:56fe\:6570"->0];
  worker[] := Catch[
    If[file===Automatic,file=Quiet[Check[With[{candidateFiles=
        FileNameJoin[{RGZWorkflow`Private`cacheDirectory[],#}]& /@
          {"RGZ_Gauge_Stopped_Input(2).wxf","RGZ_Gauge_Stopped_Input(1).wxf","RGZ_Gauge_Stopped_Input.wxf"}},
        SelectFirst[candidateFiles,FileExistsQ,None]],None]]];
    verified=If[StringQ[file],TimeConstrained[
      RGZGaugeFixedVerified[unit,file],seconds,Missing["VerifiedReadTimeout"]],
      Missing["NoVerifiedInputFile"]];
    If[AssociationQ[verified],Throw[
      RGZGaugeFixedFinish[unit,verified["\:7ed3\:679c"],verified],stop]];
    root=RGZGaugeCacheDirectory[OptionValue["\:7f13\:5b58\:76ee\:5f55"]];
    id=Hash[{"RGZFixedDen-v1",unit["\:4e3b\:79ef\:5206"],unit["Alpha\:77ed\:7cfb\:6570"],
      unit["\:53c2\:8003\:89c4\:8303"],DownValues[RGZGaugeFixedCoordinates],
      DownValues[RGZGaugeFixedPrepare]},"SHA256"];
    If[StringQ[root],dir=FileNameJoin[{root,"fixed_"<>IntegerString[id,16]}];
      If[!DirectoryQ[dir],CreateDirectory[dir,CreateIntermediateDirectories->True]];
      path=FileNameJoin[{dir,"checkpoint.wl"}]];
    work=If[AssociationQ[rgzGaugeFixedWork] &&
        Lookup[rgzGaugeFixedWork,"\:8eab\:4efd",None]===id,
      rgzGaugeFixedWork,RGZGaugeBatchCacheRead[path,{id,"fixed"}]];
    If[AssociationQ[work],prep=work["\:51c6\:5907"];num=work["\:5206\:5b50"];index=work["\:5b8c\:6210\:7ec4\:6570"],
      RGZGaugeStep["\:56fa\:5b9a\:5206\:6bcd\:ff1a\:5efa\:7acb\:6781\:70b9\:5e73\:65b9\:5750\:6807\:5e76\:6574\:7406\:5206\:6bcd\:56e0\:5b50"];
      prep=TimeConstrained[
        coords=RGZGaugeFixedCoordinates[unit["Alpha\:77ed\:7cfb\:6570"]];
        If[FailureQ[coords],coords,RGZGaugeFixedPrepare[coords]],prepLimit,
        Missing["FixedPreparationTimeout"]];
      If[Head[prep]===Missing || FailureQ[prep],Throw[
        Failure["FixedDenPreparationStopped",<|"\:539f\:56e0"->prep|>],stop]];
      num=0;index=0];
    checkpoint[] := (rgzGaugeFixedWork=<|"\:8eab\:4efd"->id,"\:51c6\:5907"->prep,
      "\:5206\:5b50"->num,"\:5b8c\:6210\:7ec4\:6570"->index|>;
      If[StringQ[path],write=RGZGaugeBatchCacheWrite[
        rgzGaugeFixedWork,path,{id,"fixed"}];
        If[FailureQ[write],Throw[write,stop]]]);
    vars=prep["\:53d8\:91cf"];
    While[index<Length[prep["\:5206\:7ec4"]],
      group=prep["\:5206\:7ec4"][[index+1]];
      RGZMXProgressUpdate["\:9879\:53f7"->index+1,"\:603b\:9879\:6570"->Length[prep["\:5206\:7ec4"]],
        "\:5b8c\:6210\:9879\:6570"->index];
      RGZGaugeStep["\:56fa\:5b9a\:5206\:6bcd\:ff1a\:7d2f\:8ba1\:591a\:9879\:5f0f\:7ec4 "<>ToString[index+1]<>" / "<>
        ToString[Length[prep["\:5206\:7ec4"]]]];
      contribution=TimeConstrained[
        exponents=Table[Total[Cases[group[[2]],
          {f_,k_Integer} /; SameQ[f,prep["\:5206\:6bcd\:56e0\:5b50"][[j]]] :> k]],
          {j,Length[prep["\:5206\:6bcd\:56e0\:5b50"]]}];
        multiplier=Times@@MapThread[Power,{prep["\:5206\:6bcd\:56e0\:5b50"],
          prep["\:6700\:9ad8\:5e42"]-exponents}];
        Expand[group[[1]] multiplier],seconds,Missing["FixedGroupTimeout"]];
      If[Head[contribution]===Missing,checkpoint[];Throw[
        Failure["FixedDenGroupTimeout",<|"\:7f16\:53f7"->unit["\:7f16\:53f7"],
          "\:7ec4\:53f7"->index+1,"\:65f6\:9650"->seconds|>],stop]];
      z=TimeConstrained[Expand[num+contribution],seconds,
        Missing["FixedAddTimeout"]];
      If[Head[z]===Missing,checkpoint[];Throw[
        Failure["FixedDenAddTimeout",<|"\:7ec4\:53f7"->index+1,"\:65f6\:9650"->seconds|>],stop]];
      num=z;index++;rgzGaugeFixedWork=<|"\:8eab\:4efd"->id,"\:51c6\:5907"->prep,
        "\:5206\:5b50"->num,"\:5b8c\:6210\:7ec4\:6570"->index|>;
      If[Mod[index,every]===0,checkpoint[]]];
    checkpoint[];
    reduced=RGZGaugeFixedFactorReduce[num,prep,id,root,seconds];
    If[FailureQ[reduced],Throw[reduced,stop]];
    z=reduced["\:5206\:5b50"]/reduced["\:5206\:6bcd"];
    {xx,yy,ww,zz,tt}=vars[[{2,3,4,5,6}]];
    alphaVar=Symbol["RGZGaugeFixedAlgebra`alpha"];
    tVar=Symbol["RGZGaugeFixedAlgebra`t"];
    If[AnyTrue[{alphaVar,tVar},Function[q,OwnValues[q]=!={},HoldAll]],
      Throw[Failure["AssignedFixedCoordinate",<||>],stop]];
    RGZGaugeStep["\:56fa\:5b9a\:5206\:6bcd\:ff1a\:8fd8\:539f\:7269\:7406\:8d28\:91cf\:5e73\:65b9\:5173\:7cfb"];
    z=TimeConstrained[Module[{n0=Numerator[z],d0=Denominator[z]},
      n0=PolynomialRemainder[n0,tt^2-(xx+yy)tt+xx yy+tVar,tt];
      d0=PolynomialRemainder[d0,tt^2-(xx+yy)tt+xx yy+tVar,tt];
      n0=PolynomialRemainder[n0,zz^2+alphaVar ww,zz];
      d0=PolynomialRemainder[d0,zz^2+alphaVar ww,zz];
      If[TrueQ[d0===0],Failure["PoleRelationSingularity",<||>],Cancel[n0/d0]]],
      seconds,Missing["FixedPhysicalReductionTimeout"]];
    If[FailureQ[z] || Head[z]===Missing,Throw[
      Failure["FixedDenPhysicalReductionStopped",<|"\:539f\:56e0"->z|>],stop]];
    restoreRules=Join[prep["\:8fd8\:539f\:89c4\:5219"],{alphaVar->\:03b1,tVar->mG^4}];
    physical=prep["\:516c\:5171\:56e0\:5b50"] (z/.restoreRules);
    details=<|"\:6765\:6e90"->"\:539f\:59cb\:52a0\:9879\:7684\:56fa\:5b9a\:516c\:5171\:5206\:6bcd\:7cbe\:786e\:5f52\:7ea6",
      "\:539f\:59cb\:52a0\:9879\:6570"->prep["\:539f\:59cb\:52a0\:9879\:6570"],
      "\:516c\:5171\:5206\:6bcd\:56e0\:5b50\:6570"->Length[prep["\:5206\:6bcd\:56e0\:5b50"]],
      "\:5df2\:5b8c\:6210\:7ec4\:6570"->index,"\:8017\:65f6"->AbsoluteTime[]-start|>;
    RGZGaugeFixedFinish[unit,physical,details],stop];
  answer=CheckAbort[TimeConstrained[worker[],totalLimit,
    Failure["FixedDenTotalTimeout",<|"\:65f6\:9650"->totalLimit,
      "\:8bf4\:660e"->"\:5df2\:5b8c\:6210\:7ec4\:4fdd\:7559\:5728\:5185\:5b58\:53ca\:6700\:8fd1\:68c0\:67e5\:70b9\:ff1b\:91cd\:8dd1\:4f1a\:7eed\:7b97\:3002"|>]],
    Failure["FixedDenAborted",<|"\:8bf4\:660e"->"\:5df2\:5b8c\:6210\:7ec4\:53ca\:65e7\:56fe\:7f13\:5b58\:4fdd\:7559\:3002"|>]];
  If[FailureQ[answer],RGZMXProgressUpdate["\:72b6\:6001"->"\:5df2\:505c\:6b62"]];answer
];

RGZGaugeFixedSummary[report_] := Module[{data,keys},
  data=Which[AssociationQ[report],report,FailureQ[report],
    Print["\:56fa\:5b9a\:5206\:6bcd\:505c\:6b62\:6807\:7b7e\:ff1a",report[[1]]];report[[2]],
    True,Return[Print["\:7ed3\:679c\:5934\:90e8\:ff1a",Head[report]]]];
  keys=Select[{"\:72b6\:6001","\:7f16\:53f7","\:7cfb\:6570\:72b6\:6001","\:7ed3\:679c\:8282\:70b9\:6570","\:6765\:6e90",
    "\:539f\:59cb\:52a0\:9879\:6570","\:516c\:5171\:5206\:6bcd\:56e0\:5b50\:6570","\:6781\:70b9\:5e73\:65b9\:7ed3\:6784\:6570","\:8f93\:5165\:6587\:4ef6\:6838\:5bf9",
    "\:5df2\:5b8c\:6210\:7ec4\:6570","\:7ec4\:53f7","\:65f6\:9650","\:8017\:65f6","\:5df2\:4fdd\:5b58\:5230\:539f\:7cfb\:6570\:7f13\:5b58","\:8bf4\:660e"},
    KeyExistsQ[data,#]&];
  Print[Grid[({#,data[#]}& /@ keys),Frame->All,Alignment->Left]]
];


(* v1.1: all prepared master coefficients. The two gauge branches are
   reduced separately BEFORE substituting the reference gauge. This keeps
   the gauge dependence of pole masses. Existing core cache keys are kept. *)
ClearAll[RGZGaugeFixedBranch,RGZGaugeFixedPair,RGZGaugeFixedCompare,
  RGZPTFixedCompare,RGZGaugeFixedExportPrepared,RGZGaugeFixedReport];

Options[RGZGaugeFixedBranch]=Options[RGZGaugeFixedDenResume];
RGZGaugeFixedBranch[unit_Association,raw_,seconds_,OptionsPattern[]] := Module[
  {root,path=None,key,old,branch,result,row,saved},
  If[TrueQ[raw===0],Return[<|"\:6b8b\:5dee\:7cfb\:6570"->0,"\:6765\:6e90"->"\:96f6\:5206\:652f"|>]];
  key=Hash[{"RGZFixedBranch-v1.1",raw,
    DownValues[RGZGaugeFixedCoordinates],DownValues[RGZGaugeFixedPrepare],
    DownValues[RGZGaugeFixedDenResume],DownValues[RGZGaugeFixedUnit1Formula]},
    "SHA256"];
  root=RGZGaugeCacheDirectory[OptionValue["\:7f13\:5b58\:76ee\:5f55"]];
  If[StringQ[root],path=FileNameJoin[{root,
    "fixed_branch_"<>IntegerString[key,16]<>".wl"}]];
  old=RGZGaugeBatchCacheRead[path,{key,"branch"}];
  If[AssociationQ[old] && KeyExistsQ[old,"\:6b8b\:5dee\:7cfb\:6570"],
    Return[Join[old,<|"\:6765\:6e90"->"\:5206\:652f\:7f13\:5b58\:6062\:590d"|>]]];
  branch=Join[unit,<|"Alpha\:77ed\:7cfb\:6570"->raw,"\:53c2\:8003\:77ed\:7cfb\:6570"->0,
    "\:7f13\:5b58\:8def\:5f84"->None,"\:7f13\:5b58\:8eab\:4efd"->None|>];
  (* Do not put a one-sided intermediate result in the original difference cache. *)
  result=Block[{rgzGaugeStoppedUnit=branch,rgzGaugeFixedCoefficient,
      rgzGaugeBatchLastCoefficient},
    row=RGZGaugeFixedDenResume[seconds,
      "\:5df2\:9a8c\:8bc1\:8f93\:5165"->OptionValue["\:5df2\:9a8c\:8bc1\:8f93\:5165"],
      "\:51c6\:5907\:65f6\:9650"->OptionValue["\:51c6\:5907\:65f6\:9650"],
      "\:603b\:65f6\:9650"->OptionValue["\:603b\:65f6\:9650"],
      "\:7f13\:5b58\:76ee\:5f55"->OptionValue["\:7f13\:5b58\:76ee\:5f55"],
      "\:4fdd\:5b58\:95f4\:9694"->OptionValue["\:4fdd\:5b58\:95f4\:9694"]];
    If[AssociationQ[row] && AssociationQ[rgzGaugeFixedCoefficient],
      KeyTake[rgzGaugeFixedCoefficient,{"\:6b8b\:5dee\:7cfb\:6570","\:6765\:6e90"}],row]];
  If[!AssociationQ[result],Return[result]];
  saved=RGZGaugeBatchCacheWrite[result,path,{key,"branch"}];
  If[FailureQ[saved],Return[saved]];result
];

Options[RGZGaugeFixedPair]=Join[Options[RGZGaugeFixedDenResume],
  {"\:76f4\:63a5\:9608\:503c"->2000}];
RGZGaugeFixedPair[unit_Association,seconds_:60,OptionsPattern[]] := Module[
  {a,b,ref,small,worker,answer,ca,cb,reduced,report,start=AbsoluteTime[],stop},
  If[!And@@(KeyExistsQ[unit,#]& /@
      {"\:7f16\:53f7","\:4e3b\:79ef\:5206","Alpha\:77ed\:7cfb\:6570","\:53c2\:8003\:77ed\:7cfb\:6570","\:53c2\:8003\:89c4\:8303"}),
    Return[Failure["IncompleteStoppedInput",<||>]]];
  {a,b,ref}=Lookup[unit,{"Alpha\:77ed\:7cfb\:6570","\:53c2\:8003\:77ed\:7cfb\:6570","\:53c2\:8003\:89c4\:8303"}];
  If[OwnValues[\:03b1]=!={},Return[Failure["AssignedGaugeParameter",<||>]]];
  If[!NumericQ[ref] || !TrueQ[ref>0],
    Return[Failure["GaugeComparisonArguments",<||>]]];
  small=OptionValue["\:76f4\:63a5\:9608\:503c"];
  If[!IntegerQ[small] || small<0,
    Return[Failure["FixedDenArguments",<||>]]];
  worker[] := Catch[
    If[LeafCount[a]+LeafCount[b]<=small,
      RGZGaugeStep["\:56fa\:5b9a\:5206\:6bcd\:ff1a\:68c0\:67e5\:5c0f\:578b\:7cfb\:6570"];
      reduced=TimeConstrained[RGZGaugeCoefficient[a,b,ref],seconds,
        Missing["SmallCoefficientTimeout"]];
      If[AssociationQ[reduced],
        Throw[RGZGaugeFixedFinish[unit,reduced["\:6b8b\:5dee\:7cfb\:6570"],
          <|"\:6765\:6e90"->"\:5c0f\:578b\:7cfb\:6570\:7684\:7cbe\:786e\:6839\:5f0f\:68c0\:67e5",
            "\:8017\:65f6"->AbsoluteTime[]-start|>],stop]];
      (* A timeout of the direct small path switches to the fixed denominator. *)
      If[FailureQ[reduced],Throw[reduced,stop]]];
    RGZGaugeStep["\:56fa\:5b9a\:5206\:6bcd\:ff1a\:5f52\:7ea6\:5f53\:524d\:89c4\:8303\:5206\:652f"];
    ca=RGZGaugeFixedBranch[unit,a,seconds,
      "\:5df2\:9a8c\:8bc1\:8f93\:5165"->OptionValue["\:5df2\:9a8c\:8bc1\:8f93\:5165"],
      "\:51c6\:5907\:65f6\:9650"->OptionValue["\:51c6\:5907\:65f6\:9650"],
      "\:603b\:65f6\:9650"->OptionValue["\:603b\:65f6\:9650"],
      "\:7f13\:5b58\:76ee\:5f55"->OptionValue["\:7f13\:5b58\:76ee\:5f55"],
      "\:4fdd\:5b58\:95f4\:9694"->OptionValue["\:4fdd\:5b58\:95f4\:9694"]];
    If[!AssociationQ[ca],Throw[ca,stop]];
    RGZGaugeStep["\:56fa\:5b9a\:5206\:6bcd\:ff1a\:5f52\:7ea6\:53c2\:8003\:89c4\:8303\:5206\:652f"];
    cb=RGZGaugeFixedBranch[unit,b,seconds,
      "\:5df2\:9a8c\:8bc1\:8f93\:5165"->None,"\:51c6\:5907\:65f6\:9650"->OptionValue["\:51c6\:5907\:65f6\:9650"],
      "\:603b\:65f6\:9650"->OptionValue["\:603b\:65f6\:9650"],
      "\:7f13\:5b58\:76ee\:5f55"->OptionValue["\:7f13\:5b58\:76ee\:5f55"],
      "\:4fdd\:5b58\:95f4\:9694"->OptionValue["\:4fdd\:5b58\:95f4\:9694"]];
    If[!AssociationQ[cb],Throw[cb,stop]];
    (* GaugeCoefficient inserts the physical pole definitions in BOTH branches,
       then sets alpha -> ref in the reference branch. Never replace the
       reference poles by the still-variable current-gauge poles. *)
    RGZGaugeStep["\:56fa\:5b9a\:5206\:6bcd\:ff1a\:76f8\:51cf\:5df2\:7d27\:51d1\:5316\:7684\:4e24\:4e2a\:89c4\:8303\:5206\:652f"];
    reduced=TimeConstrained[RGZGaugeCoefficient[
      ca["\:6b8b\:5dee\:7cfb\:6570"],cb["\:6b8b\:5dee\:7cfb\:6570"],ref],seconds,
      Missing["CompactDifferenceTimeout"]];
    If[Head[reduced]===Missing,Throw[Failure["FixedDenDifferenceTimeout",<|
      "\:7f16\:53f7"->unit["\:7f16\:53f7"],"\:65f6\:9650"->seconds,
      "\:505c\:6b62\:64cd\:4f5c"->"\:4e24\:4e2a\:7d27\:51d1\:89c4\:8303\:5206\:652f\:76f8\:51cf",
      "\:8bf4\:660e"->"\:5df2\:5b8c\:6210\:5206\:652f\:7684\:7f13\:5b58\:4fdd\:7559\:ff1b\:6b64\:5dee\:503c\:5c1a\:672a\:68c0\:67e5\:5b8c\:6210\:3002"|>],stop]];
    If[FailureQ[reduced],Throw[reduced,stop]];
    If[!AssociationQ[reduced],Throw[Failure["InvalidFixedResult",<||>],stop]];
    RGZGaugeFixedFinish[unit,reduced["\:6b8b\:5dee\:7cfb\:6570"],
      <|"\:6765\:6e90"->"\:5206\:522b\:56fa\:5b9a\:5206\:6bcd\:5f52\:7ea6\:540e\:6bd4\:8f83\:4e24\:4e2a\:89c4\:8303\:5206\:652f",
        "\:8017\:65f6"->AbsoluteTime[]-start|>],stop];
  answer=CheckAbort[TimeConstrained[worker[],OptionValue["\:603b\:65f6\:9650"],
      Failure["FixedDenPairTimeout",<|"\:7f16\:53f7"->unit["\:7f16\:53f7"],
        "\:65f6\:9650"->OptionValue["\:603b\:65f6\:9650"],
        "\:8bf4\:660e"->"\:5f53\:524d\:7cfb\:6570\:8fbe\:5230\:603b\:65f6\:9650\:ff1b\:5b8c\:6210\:7684\:7ec4\:548c\:5206\:652f\:7f13\:5b58\:53ef\:7eed\:7528\:3002"|>]],
    Failure["FixedDenAborted",<|"\:7f16\:53f7"->unit["\:7f16\:53f7"]|>]];
  If[FailureQ[answer],rgzGaugeStoppedUnit=unit;
    RGZMXProgressUpdate["\:72b6\:6001"->"\:5df2\:505c\:6b62"]];answer
];

Options[RGZGaugeFixedCompare]=Join[Options[RGZGaugeFixedPair],
  {"\:9884\:5904\:7406\:65f6\:9650"->600,"\:7cfb\:6570\:7f16\:53f7"->All}];
RGZGaugeFixedCompare[expr_,reference_:1,seconds_:60,OptionsPattern[]] := Module[
  {prepLimit,root,prepKey,prepPath=None,prepared,pairs,indices,context,
    unit,path,key,row,report,n,total,rows={},stats,answer,start=AbsoluteTime[],
    selected,stop,run,part,pathOption},
  prepLimit=OptionValue["\:9884\:5904\:7406\:65f6\:9650"];selected=OptionValue["\:7cfb\:6570\:7f16\:53f7"];
  If[OwnValues[\:03b1]=!={},Return[Failure["AssignedGaugeParameter",<||>]]];
  If[!NumericQ[reference] || !TrueQ[reference>0] ||
      !And@@(NumericQ[#] && TrueQ[#>0]& /@
        {seconds,prepLimit,OptionValue["\:51c6\:5907\:65f6\:9650"],OptionValue["\:603b\:65f6\:9650"]}),
    Return[Failure["GaugeComparisonArguments",<||>]]];
  If[!FreeQ[expr,_Failure|_Missing|$Failed|$Aborted|_TID|_PaVeReduce|
      _FeynAmpDenominator|_LorentzIndex|_ExplicitLorentzIndex|_DiracGamma|
      _SUNIndex|_SUNF|_SUND|_SUNDelta],
    Return[Failure["NeedIntegratedColourProjectedScalar",<||>]]];
  RGZMXProgressUpdate["\:90e8\:95e8"->"RGZ \:56fa\:5b9a\:5206\:6bcd\:89c4\:8303\:68c0\:67e5",
    "\:9636\:6bb5"->"\:51c6\:5907\:4e3b\:79ef\:5206\:7cfb\:6570","\:9636\:6bb5\:5f00\:59cb\:65f6\:95f4"->start,
    "\:56fe\:53f7"->0,"\:56fe\:6570"->0,"\:9879\:53f7"->0,"\:603b\:9879\:6570"->0,
    "\:5b8c\:6210\:9879\:6570"->0,"\:72b6\:6001"->"\:8ba1\:7b97\:4e2d"];
  root=RGZGaugeCacheDirectory[OptionValue["\:7f13\:5b58\:76ee\:5f55"]];
  prepKey=Hash[{"RGZFixedPrepared-v1.1",expr,reference,
    DownValues[RGZGaugePrepare],DownValues[RGZGaugePVPairs],
    DownValues[RGZGaugeCanonicalPV],DownValues[RGZLoopPoleMasses]},"SHA256"];
  If[StringQ[root],prepPath=FileNameJoin[{root,
    "fixed_prepared_"<>IntegerString[prepKey,16]<>".wl"}]];
  answer=CheckAbort[TimeConstrained[
    If[AssociationQ[rgzGaugeFixedPrepared] &&
        Lookup[rgzGaugeFixedPrepared,"\:8eab\:4efd",None]===prepKey,
      prepared=rgzGaugeFixedPrepared,
      prepared=RGZGaugeBatchCacheRead[prepPath,{prepKey,"prepared"}];
      If[!AssociationQ[prepared],
        RGZGaugeStep["\:56fa\:5b9a\:5206\:6bcd\:ff1a\:5408\:5e76\:603b\:548c\:7684\:540c\:7c7b\:4e3b\:79ef\:5206\:ff08\:53ea\:505a\:7cfb\:6570\:9884\:5904\:7406\:ff09"];
        pairs=RGZGaugePrepare[expr,reference,prepLimit];
        If[FailureQ[pairs] || !ListQ[pairs],Return[pairs]];
        prepared=<|"\:8eab\:4efd"->prepKey,"\:53c2\:8003\:89c4\:8303"->reference,"\:7cfb\:6570\:5217\:8868"->pairs|>;
        rgzGaugeFixedPrepared=prepared;
        report=RGZGaugeBatchCacheWrite[prepared,prepPath,{prepKey,"prepared"}];
        If[FailureQ[report],Return[report]]]],prepLimit,
    Failure["FixedDenPreparationTimeout",<|"\:9636\:6bb5"->"\:4e3b\:79ef\:5206\:5206\:79bb\:53ca\:7f13\:5b58\:8bfb\:53d6",
      "\:65f6\:9650"->prepLimit|>]],Failure["FixedDenAborted",<|"\:9636\:6bb5"->"\:9884\:5904\:7406"|>]];
  If[FailureQ[answer] || !AssociationQ[prepared],
    RGZMXProgressUpdate["\:72b6\:6001"->"\:5df2\:505c\:6b62"];Return[answer]];
  rgzGaugeFixedPrepared=prepared;pairs=prepared["\:7cfb\:6570\:5217\:8868"];total=Length[pairs];
  indices=Which[selected===All,Range[total],IntegerQ[selected],{selected},
    ListQ[selected],DeleteDuplicates[selected],True,{}];
  If[selected=!=All && (indices==={} ||
      !AllTrue[indices,IntegerQ[#] && 1<=#<=total&]),
    Return[Failure["ResidualRowNumber",<|"\:603b\:9879\:6570"->total|>]]];
  context=Hash[{"RGZGaugeCoefficient-v2",DownValues[RGZGaugeCoefficient],
    DownValues[RGZGaugePoleSquares],DownValues[RGZLoopPoleMasses]},"SHA256"];
  rgzGaugeCompletedRows={};
  rgzGaugeProgress=<|"\:5df2\:5b8c\:6210\:9879\:6570"->0,"\:4e3b\:79ef\:5206\:9879\:6570"->total,
    "\:7edf\:8ba1"-><||>,"\:5f53\:524d\:9879"->0,"\:7f13\:5b58\:76ee\:5f55"->root|>;
  run[] := Catch[Do[
    RGZMXProgressUpdate["\:9636\:6bb5"->"\:7cfb\:6570 "<>ToString[n]<>" / "<>ToString[total],
      "\:9879\:53f7"->n,"\:603b\:9879\:6570"->total,"\:5b8c\:6210\:9879\:6570"->Length[rows]];
    AssociateTo[rgzGaugeProgress,"\:5f53\:524d\:9879"->n];
    key=Hash[{context,reference,pairs[[n]]},"SHA256"];
    path=If[StringQ[root],FileNameJoin[{root,
      "coefficient_"<>IntegerString[key,16]<>".wl"}],None];
    unit=<|"\:7f16\:53f7"->n,"\:603b\:9879\:6570"->total,"\:4e3b\:79ef\:5206"->pairs[[n,1]],
      "Alpha\:77ed\:7cfb\:6570"->pairs[[n,2]],"\:53c2\:8003\:77ed\:7cfb\:6570"->pairs[[n,3]],
      "\:53c2\:8003\:89c4\:8303"->reference,"\:65f6\:9650"->seconds,
      "\:7f13\:5b58\:8def\:5f84"->path,"\:7f13\:5b58\:8eab\:4efd"->key,"\:5df2\:5b8c\:6210\:9879\:6570"->Length[rows]|>;
    rgzGaugeStoppedUnit=unit;
    row=RGZGaugeCacheRead[path,key];
    If[!AssociationQ[row],
      report=RGZGaugeFixedPair[unit,seconds,
        "\:5df2\:9a8c\:8bc1\:8f93\:5165"->OptionValue["\:5df2\:9a8c\:8bc1\:8f93\:5165"],
        "\:51c6\:5907\:65f6\:9650"->OptionValue["\:51c6\:5907\:65f6\:9650"],
        "\:603b\:65f6\:9650"->OptionValue["\:603b\:65f6\:9650"],
        "\:7f13\:5b58\:76ee\:5f55"->OptionValue["\:7f13\:5b58\:76ee\:5f55"],
        "\:4fdd\:5b58\:95f4\:9694"->OptionValue["\:4fdd\:5b58\:95f4\:9694"],
        "\:76f4\:63a5\:9608\:503c"->OptionValue["\:76f4\:63a5\:9608\:503c"]];
      If[!AssociationQ[report],Throw[Failure["FixedDenCoefficientStopped",<|
        "\:7f16\:53f7"->n,"\:603b\:9879\:6570"->total,"\:5df2\:5b8c\:6210\:9879\:6570"->Length[rows],
        "\:505c\:6b62\:64cd\:4f5c"->Lookup[rgzGaugeProgress,"\:5f53\:524d\:64cd\:4f5c",""],
        "\:539f\:56e0"->report,"\:8bf4\:660e"->"\:5728\:5f53\:524d\:7cfb\:6570\:505c\:6b62\:ff1b\:6b64\:524d\:56fe\:3001\:79ef\:5206\:53ca\:5b8c\:6210\:7cfb\:6570\:7f13\:5b58\:4fdd\:7559\:3002"|>],stop]];
      row=rgzGaugeFixedCoefficient,
      row=Join[row,<|"\:7f16\:53f7"->n,"\:6765\:6e90"->"\:539f\:7cfb\:6570\:7f13\:5b58\:6062\:590d"|>]];
    AppendTo[rows,row];rgzGaugeCompletedRows=rows;
    AssociateTo[rgzGaugeProgress,{"\:5df2\:5b8c\:6210\:9879\:6570"->Length[rows],
      "\:7edf\:8ba1"->Counts[Lookup[rows,"\:72b6\:6001"]]}],{n,indices}];
    stats=Counts[Lookup[rows,"\:72b6\:6001"]];
    <|"\:53c2\:8003\:89c4\:8303"->reference,"\:4e3b\:79ef\:5206\:9879\:6570"->total,
      "\:672c\:6b21\:68c0\:67e5\:9879\:6570"->Length[rows],"\:7edf\:8ba1"->stats,
      "\:72b6\:6001"->If[Length[rows]=!=total,"\:53ea\:68c0\:67e5\:4e86\:9009\:5b9a\:7cfb\:6570",
        If[AllTrue[rows,#["\:72b6\:6001"]==="\:5f52\:96f6"&],
          "\:6240\:68c0\:67e5\:8868\:8fbe\:5f0f\:7684\:89c4\:8303\:6b8b\:5dee\:4e3a\:96f6","\:5c1a\:672a\:8bc1\:660e\:89c4\:8303\:65e0\:5173"]],
      "\:6b8b\:5dee\:5206\:9879"->rows,
      "\:8bf4\:660e"->"\:68c0\:67e5\:5f53\:524d\:5019\:9009\:7ec4\:5408\:ff1b\:4fdd\:7559\:6709\:9650 M\:ff0c\:4e0d\:81ea\:52a8\:5047\:5b9a\:4e0d\:540c\:4e3b\:79ef\:5206\:72ec\:7acb\:6216 pinch \:5b8c\:5907\:3002"|>,stop];
  answer=CheckAbort[run[],Failure["FixedDenAborted",<|
    "\:5df2\:5b8c\:6210\:9879\:6570"->Length[rows],"\:8bf4\:660e"->"\:91cd\:8dd1\:4f1a\:590d\:7528\:5b8c\:6210\:7684\:7cfb\:6570\:4e0e\:5206\:652f\:7f13\:5b58\:3002"|>]];
  RGZMXProgressUpdate["\:72b6\:6001"->If[FailureQ[answer],"\:5df2\:505c\:6b62","\:68c0\:67e5\:5b8c\:6210"]];
  rgzGaugeFixedReport=answer;answer
];

Options[RGZPTFixedCompare]=Options[RGZGaugeFixedCompare];
RGZPTFixedCompare[reference_:1,seconds_:60,OptionsPattern[]] := Module[
  {names,missing,parts,answer},
  If[!AssociationQ[rgzPTJob],Return[Failure["RestorePTJobFirst",<|
    "\:8bf4\:660e"->"\:5148\:6062\:590d\:5df2\:5b8c\:6210\:7684 rgzPTJob\:ff1b\:672c\:547d\:4ee4\:4e0d\:4f1a\:91cd\:7b97\:56db\:7c7b\:56fe\:3002"|>]]];
  With[{check=RGZPTJobCheck[]},If[FailureQ[check],Return[check]]];
  If[rgzPTJob["Mode"]=!="FourPinch",Return[Failure["FullPinchRequired",<|"Action"->"RGZPTStart[\"Mode\" -> \"FourPinch\"]"|>]]];
  names=rgzPTJob["\:9879\:76ee\:987a\:5e8f"];
  missing=Select[names,
    !KeyExistsQ[rgzPTJob["\:9879\:76ee"][#]["\:7ed3\:679c"],"\:6807\:91cf"]&];
  If[missing=!={},Return[Failure["PinchPartsIncomplete",<|"\:672a\:5b8c\:6210\:9879\:76ee"->missing|>]]];
  (* Read existing ordered contributions directly; avoid forming/formatting
     RGZPTResult's large redundant totals just to obtain its parts list. *)
  parts=Join[rgzPTJob["\:81ea\:80fd\:5206\:9879"],
    (rgzPTJob["\:9879\:76ee"][#]["\:7ed3\:679c"]["\:6807\:91cf"]& /@ names)];
  (* Also normalize restored jobs, whose mixed contributions can still
     contain the model's bare N. Changed inputs receive fresh cache keys. *)
  parts=RGZPTColour /@ parts;
  answer=RGZGaugeFixedCompare[parts,reference,seconds,
    "\:5df2\:9a8c\:8bc1\:8f93\:5165"->OptionValue["\:5df2\:9a8c\:8bc1\:8f93\:5165"],
    "\:51c6\:5907\:65f6\:9650"->OptionValue["\:51c6\:5907\:65f6\:9650"],
    "\:603b\:65f6\:9650"->OptionValue["\:603b\:65f6\:9650"],
    "\:7f13\:5b58\:76ee\:5f55"->OptionValue["\:7f13\:5b58\:76ee\:5f55"],
    "\:4fdd\:5b58\:95f4\:9694"->OptionValue["\:4fdd\:5b58\:95f4\:9694"],
    "\:76f4\:63a5\:9608\:503c"->OptionValue["\:76f4\:63a5\:9608\:503c"],
    "\:9884\:5904\:7406\:65f6\:9650"->OptionValue["\:9884\:5904\:7406\:65f6\:9650"],
    "\:7cfb\:6570\:7f16\:53f7"->OptionValue["\:7cfb\:6570\:7f16\:53f7"]];
  AssociateTo[rgzPTJob,"\:6bd4\:8f83"->answer];answer
];

RGZGaugeFixedExportPrepared[file_String] := Module[{data,saved},
  If[!AssociationQ[rgzGaugeFixedPrepared],
    Return[Failure["PrepareCoefficientsFirst",<||>]]];
  data=KeyTake[rgzGaugeFixedPrepared,{"\:53c2\:8003\:89c4\:8303","\:7cfb\:6570\:5217\:8868"}];
  saved=Check[Export[file,data,"WXF"],$Failed];
  If[saved===$Failed,Failure["PreparedExportFailed",<||>],
    <|"\:72b6\:6001"->"\:5bfc\:51fa\:5b8c\:6210","\:7cfb\:6570\:6570"->Length[data["\:7cfb\:6570\:5217\:8868"]],"\:6587\:4ef6"->saved|>]
];

RGZGaugeFixedReport[report_] := Module[{data,keys,reason},
  data=Which[AssociationQ[report],report,FailureQ[report],
    Print["\:505c\:6b62\:6807\:7b7e\:ff1a",report[[1]]];report[[2]],
    True,Return[Print["\:7ed3\:679c\:5934\:90e8\:ff1a",Head[report]]]];
  keys=Select[{"\:53c2\:8003\:89c4\:8303","\:4e3b\:79ef\:5206\:9879\:6570","\:672c\:6b21\:68c0\:67e5\:9879\:6570","\:7edf\:8ba1","\:72b6\:6001",
    "\:7f16\:53f7","\:603b\:9879\:6570","\:5df2\:5b8c\:6210\:9879\:6570","\:9636\:6bb5","\:505c\:6b62\:64cd\:4f5c","\:65f6\:9650","\:8bf4\:660e"},
    KeyExistsQ[data,#]&];
  Print[Grid[({#,data[#]}& /@ keys),Frame->All,Alignment->Left]];
  reason=Lookup[data,"\:539f\:56e0",None];
  If[FailureQ[reason],Print["\:5f53\:524d\:7cfb\:6570\:505c\:6b62\:6807\:7b7e\:ff1a",reason[[1]]];
    keys=Select[{"\:7ec4\:53f7","\:56e0\:5b50\:53f7","\:603b\:56e0\:5b50\:6570","\:5269\:4f59\:5e42","\:5df2\:7ea6\:53bb\:6b21\:6570","\:65f6\:9650","\:8017\:65f6","\:539f\:56e0\:6807\:7b7e","\:505c\:6b62\:64cd\:4f5c","\:6587\:4ef6","\:8bf4\:660e"},
      KeyExistsQ[reason[[2]],#]&];
    If[keys=!={},Print[Grid[({#,reason[[2]][#]}& /@ keys),
      Frame->All,Alignment->Left]]]]
];

(* v1.2: binary, atomic batch checkpoints. Algebra and cache identities
   above are unchanged. Keep rgzGaugeFixedWork and all prepared data alive. *)
ClearAll[RGZGaugeFixedSetCache,RGZGaugeFixedCacheInfo,
  RGZGaugeFixedSaveProgress,
  RGZGaugeFixedCacheRead,RGZGaugeFixedCacheWrite,
  RGZGaugeFixedValidPacketQ,RGZGaugeFixedCacheFailure];

If[!AssociationQ[rgzGaugeFixedCacheSettings],
  rgzGaugeFixedCacheSettings=<|"\:5199\:5165\:65f6\:9650"->120,"\:8bfb\:53d6\:65f6\:9650"->60|>];
Options[RGZGaugeFixedSetCache]={"\:5199\:5165\:65f6\:9650"->120,"\:8bfb\:53d6\:65f6\:9650"->60};
RGZGaugeFixedSetCache[OptionsPattern[]] := Module[{w,r},
  w=OptionValue["\:5199\:5165\:65f6\:9650"];r=OptionValue["\:8bfb\:53d6\:65f6\:9650"];
  If[!And@@(NumericQ[#] && TrueQ[#>0]& /@ {w,r}),
    Return[Failure["CacheTimeLimitArguments",<||>]]];
  rgzGaugeFixedCacheSettings=<|"\:5199\:5165\:65f6\:9650"->w,"\:8bfb\:53d6\:65f6\:9650"->r|>;
  <|"\:72b6\:6001"->"\:5df2\:8bbe\:7f6e","\:7f13\:5b58\:683c\:5f0f"->"WXF","\:5199\:5165\:65f6\:9650"->w,"\:8bfb\:53d6\:65f6\:9650"->r|>
];

RGZGaugeFixedValidPacketQ[r_,identity_] := AssociationQ[r] &&
  Lookup[r,"\:8eab\:4efd",None]===identity && KeyExistsQ[r,"\:7ed3\:679c"] &&
  FreeQ[r["\:7ed3\:679c"],_Failure|_Missing|$Failed|$Aborted];

RGZGaugeFixedCacheRead[path_,identity_] := Module[{r,binary,limit},
  If[!StringQ[path],Return[Missing["NoBatchCache"]]];
  limit=rgzGaugeFixedCacheSettings["\:8bfb\:53d6\:65f6\:9650"];
  binary=path<>".wxf";
  If[FileExistsQ[binary],
    r=CheckAbort[TimeConstrained[
      Quiet[Check[Import[binary,"WXF"],$Failed]],limit,
      Missing["BinaryCacheReadTimeout"]],Missing["BinaryCacheReadAborted"]];
    If[RGZGaugeFixedValidPacketQ[r,identity],Return[r["\:7ed3\:679c"]]]];
  (* Text checkpoints from v1.1 and older remain valid; never delete them. *)
  If[!FileExistsQ[path],Return[Missing["NoBatchCache"]]];
  r=CheckAbort[TimeConstrained[Quiet[Check[Get[path],$Failed]],limit,
    Missing["LegacyCacheReadTimeout"]],Missing["LegacyCacheReadAborted"]];
  If[RGZGaugeFixedValidPacketQ[r,identity],r["\:7ed3\:679c"],
    Missing["NoValidBatchCache"]]
];

RGZGaugeFixedCacheFailure[reason_,path_,operation_,limit_,elapsed_,messages_:{}] := Module[{data},
  data=<|"\:72b6\:6001"->"\:4fdd\:5b58\:5931\:8d25","\:7f13\:5b58\:683c\:5f0f"->"WXF","\:539f\:56e0\:6807\:7b7e"->reason,
    "\:505c\:6b62\:64cd\:4f5c"->operation,"\:6587\:4ef6"->path,"\:65f6\:9650"->limit,"\:8017\:65f6"->elapsed,
    "\:5e95\:5c42\:6d88\:606f"->If[ListQ[messages],
      (ToString[#,InputForm]& /@ Take[messages,Min[8,Length[messages]]]),{}],
    "\:8bf4\:660e"->"\:5185\:5b58\:4e2d\:7684\:5df2\:5b8c\:6210\:7ec4\:4fdd\:7559\:ff1b\:4e0d\:8981\:9000\:51fa\:5185\:6838\:3002\:4fee\:590d\:4fdd\:5b58\:95ee\:9898\:540e\:53ef\:4ece\:8be5\:7ec4\:7ee7\:7eed\:3002"|>;
  rgzGaugeFixedCacheLast=data;
  Failure["BatchCacheWriteFailed",data]
];

RGZGaugeFixedCacheWrite[x_,path_,identity_] := Module[
  {binary,tmp,limit,start=AbsoluteTime[],operation="\:5efa\:7acb\:7f13\:5b58\:76ee\:5f55",r,bytes,reason,
    messages={},ioResult},
  If[path===None,Return[True]];
  limit=rgzGaugeFixedCacheSettings["\:5199\:5165\:65f6\:9650"];
  If[!StringQ[path],Return[RGZGaugeFixedCacheFailure[
    "InvalidCachePath",ToString[Head[path]],operation,limit,0]]];
  binary=path<>".wxf";
  tmp=binary<>".tmp-"<>CreateUUID[];
  r=Block[{$MessageList={}},ioResult=CheckAbort[TimeConstrained[
    Quiet[Check[
      If[!DirectoryQ[DirectoryName[binary]],
        CreateDirectory[DirectoryName[binary],CreateIntermediateDirectories->True]];
      operation="\:4fdd\:5b58 WXF \:68c0\:67e5\:70b9";
      RGZGaugeStep[operation];
      rgzGaugeFixedCacheLast=<|"\:72b6\:6001"->"\:4fdd\:5b58\:4e2d","\:7f13\:5b58\:683c\:5f0f"->"WXF",
        "\:505c\:6b62\:64cd\:4f5c"->operation,"\:6587\:4ef6"->binary,"\:65f6\:9650"->limit|>;
      If[!StringQ[Export[tmp,<|"\:683c\:5f0f"->"RGZFixedBatchWXF-v1",
          "\:8eab\:4efd"->identity,"\:7ed3\:679c"->x|>,"WXF"]],
        $Failed,
        operation="\:63d0\:4ea4 WXF \:68c0\:67e5\:70b9";
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
  rgzGaugeFixedCacheLast=<|"\:72b6\:6001"->"\:4fdd\:5b58\:5b8c\:6210","\:7f13\:5b58\:683c\:5f0f"->"WXF",
    "\:6587\:4ef6"->binary,"\:5927\:5c0f\:ff08MiB\:ff09"->N[bytes/2^20],
    "\:8017\:65f6"->AbsoluteTime[]-start,"\:65f6\:9650"->limit|>;
  True
];

(* Do not change reducer DownValues: legacy fixed/branch identities stay valid. *)
Clear[RGZGaugeBatchCacheRead,RGZGaugeBatchCacheWrite];
RGZGaugeBatchCacheRead[path_,identity_] := RGZGaugeFixedCacheRead[path,identity];
RGZGaugeBatchCacheWrite[x_,path_,identity_] := RGZGaugeFixedCacheWrite[x,path,identity];

Options[RGZGaugeFixedSaveProgress]={"\:7f13\:5b58\:76ee\:5f55"->Automatic};
RGZGaugeFixedSaveProgress[OptionsPattern[]] := Module[{root,id,path,r},
  If[!AssociationQ[rgzGaugeFixedWork] ||
      !And@@(KeyExistsQ[rgzGaugeFixedWork,#]& /@
        {"\:8eab\:4efd","\:51c6\:5907","\:5206\:5b50","\:5b8c\:6210\:7ec4\:6570"}),
    Return[Failure["NoInMemoryFixedCheckpoint",<|
      "\:8bf4\:660e"->"\:5f53\:524d\:5185\:6838\:6ca1\:6709\:5f85\:4fdd\:5b58\:7684\:7ec4\:68c0\:67e5\:70b9\:ff1b\:53ef\:4f7f\:7528\:78c1\:76d8\:65e7\:7f13\:5b58\:7eed\:7b97\:3002"|>]]];
  root=RGZGaugeCacheDirectory[OptionValue["\:7f13\:5b58\:76ee\:5f55"]];
  If[!StringQ[root],Return[Failure["CacheDirectoryUnavailable",<||>]]];
  id=rgzGaugeFixedWork["\:8eab\:4efd"];
  path=FileNameJoin[{root,"fixed_"<>IntegerString[id,16],"checkpoint.wl"}];
  r=RGZGaugeBatchCacheWrite[rgzGaugeFixedWork,path,{id,"fixed"}];
  If[FailureQ[r],r,<|"\:72b6\:6001"->"\:5185\:5b58\:8fdb\:5ea6\:5df2\:4fdd\:5b58",
    "\:5df2\:5b8c\:6210\:7ec4\:6570"->rgzGaugeFixedWork["\:5b8c\:6210\:7ec4\:6570"],"\:7f13\:5b58\:683c\:5f0f"->"WXF"|>]
];

RGZGaugeFixedCacheInfo[] := Module[{keys,data},
  If[!AssociationQ[rgzGaugeFixedCacheLast],
    Return[Print["\:5f53\:524d\:5185\:6838\:5c1a\:65e0\:65b0\:7248\:7f13\:5b58\:5199\:5165\:8bb0\:5f55\:3002"]]];
  data=rgzGaugeFixedCacheLast;
  keys=Select[{"\:72b6\:6001","\:7f13\:5b58\:683c\:5f0f","\:539f\:56e0\:6807\:7b7e","\:505c\:6b62\:64cd\:4f5c","\:6587\:4ef6",
    "\:5927\:5c0f\:ff08MiB\:ff09","\:65f6\:9650","\:8017\:65f6","\:5e95\:5c42\:6d88\:606f","\:8bf4\:660e"},KeyExistsQ[data,#]&];
  Grid[({#,data[#]}& /@ keys),Frame->All,Alignment->Left]
];



(* v1.3: divide by known denominator factors, not a global polynomial GCD.
   FactorList in the preparation supplies irreducible rational factors.
   Exact zero remainders only; no numerical divisibility decisions. *)
ClearAll[RGZGaugeFixedTryFactor,RGZGaugeFixedFactorReduce,
  RGZGaugeFixedFinalInfo];

RGZGaugeFixedTryFactor[n_,f_,vars_List] := Module[{v,candidates,degree,qr},
  If[TrueQ[n===0],Return[0]];
  If[MemberQ[vars,f],
    Return[If[Exponent[n,f,Min]>0,Expand[n/f],Missing["NotDivisible"]]]];
  candidates=Select[vars,Function[t,
    degree=Exponent[f,t];IntegerQ[degree] && degree>0 &&
      NumberQ[Coefficient[f,t,degree]] &&
      !TrueQ[Coefficient[f,t,degree]===0]]];
  If[candidates=!={},
    v=First[SortBy[candidates,Exponent[f,#]&]];
    qr=PolynomialQuotientRemainder[n,f,v];
    If[!MatchQ[qr,{_,_}],Return[Failure["InvalidPolynomialDivision",<||>]]];
    Return[If[TrueQ[Expand[qr[[2]]]===0],qr[[1]],Missing["NotDivisible"]]]];
  (* Rare factors without a numeric leading coefficient: cancel only ONE factor. *)
  qr=Cancel[n/f];
  If[PolynomialQ[qr,vars],qr,Missing["NotDivisible"]]
];

RGZGaugeFixedFactorReduce[num_,prep_Association,id_,root_,seconds_] := Module[
  {factors=prep["\:5206\:6bcd\:56e0\:5b50"],powers=prep["\:6700\:9ad8\:5e42"],vars=prep["\:53d8\:91cf"],
    identity,path=None,state,n=num,j=1,remaining,attempts=0,divided=0,
    q,save,write,stop,answer},
  identity=Hash[{"RGZFixedFactor-v1.3",id,factors,powers,
    DownValues[RGZGaugeFixedTryFactor]},"SHA256"];
  If[StringQ[root],path=FileNameJoin[{root,"fixed_"<>IntegerString[id,16],
    "factor_cancel_"<>IntegerString[identity,16]<>".wl"}]];
  state=If[AssociationQ[rgzGaugeFixedCancelWork] &&
      Lookup[rgzGaugeFixedCancelWork,"\:8eab\:4efd",None]===identity,
    rgzGaugeFixedCancelWork,RGZGaugeBatchCacheRead[path,{identity,"factor"}]];
  remaining=powers;
  If[AssociationQ[state],
    n=state["\:5206\:5b50"];remaining=state["\:5269\:4f59\:5e42"];j=state["\:56e0\:5b50\:53f7"];
    attempts=state["\:5df2\:68c0\:67e5\:6b21\:6570"];divided=state["\:7ea6\:53bb\:6b21\:6570"]];
  save[] := (
    rgzGaugeFixedCancelWork=<|"\:8eab\:4efd"->identity,"\:5206\:5b50"->n,
      "\:5269\:4f59\:5e42"->remaining,"\:56e0\:5b50\:53f7"->j,"\:5df2\:68c0\:67e5\:6b21\:6570"->attempts,
      "\:7ea6\:53bb\:6b21\:6570"->divided,"\:603b\:56e0\:5b50\:6570"->Length[factors]|>;
    write=RGZGaugeBatchCacheWrite[rgzGaugeFixedCancelWork,path,{identity,"factor"}];
    If[FailureQ[write],Throw[write,stop]]);
  answer=Catch[
    While[j<=Length[factors],
      If[TrueQ[n===0],remaining=ConstantArray[0,Length[factors]];
        j=Length[factors]+1;Break[]];
      If[remaining[[j]]===0,j++;Continue[]];
      RGZMXProgressUpdate["\:9879\:53f7"->j,"\:603b\:9879\:6570"->Length[factors],
        "\:5b8c\:6210\:9879\:6570"->j-1];
      RGZGaugeStep["\:56fa\:5b9a\:5206\:6bcd\:ff1a\:9010\:56e0\:5b50\:6574\:9664 "<>ToString[j]<>" / "<>
        ToString[Length[factors]]<>"\:ff1b\:5269\:4f59\:5e42 "<>ToString[remaining[[j]]]];
      q=TimeConstrained[RGZGaugeFixedTryFactor[n,factors[[j]],vars],seconds,
        Missing["FactorDivisionTimeout"]];
      If[q===Missing["FactorDivisionTimeout"],save[];
        Throw[Failure["FixedDenFactorCancelTimeout",<|
          "\:56e0\:5b50\:53f7"->j,"\:603b\:56e0\:5b50\:6570"->Length[factors],"\:5269\:4f59\:5e42"->remaining[[j]],
          "\:65f6\:9650"->seconds,"\:5df2\:7ea6\:53bb\:6b21\:6570"->divided,
          "\:8bf4\:660e"->"\:5df2\:4fdd\:5b58\:9010\:56e0\:5b50\:7ea6\:5206\:8fdb\:5ea6\:ff1b\:91cd\:8dd1\:4ece\:8fd9\:4e2a\:56e0\:5b50\:7ee7\:7eed\:ff0c\:4e0d\:91cd\:7b97\:591a\:9879\:5f0f\:7ec4\:3002"|>],stop]];
      If[FailureQ[q],save[];Throw[q,stop]];
      If[q===$Failed || q===$Aborted ||
          !FreeQ[q,Indeterminate|ComplexInfinity|_DirectedInfinity],
        save[];Throw[Failure["InvalidFactorDivisionResult",<|"\:56e0\:5b50\:53f7"->j|>],stop]];
      attempts++;
      If[q===Missing["NotDivisible"],j++,n=q;remaining[[j]]--;divided++];
      (* Commit before IO, so even an interrupted save retains exact memory progress. *)
      save[]];
    save[];
    <|"\:5206\:5b50"->n,"\:5206\:6bcd"->Times@@MapThread[Power,{factors,remaining}],
      "\:5df2\:7ea6\:53bb\:6b21\:6570"->divided,"\:5df2\:68c0\:67e5\:6b21\:6570"->attempts|>,stop];
  answer
];

RGZGaugeFixedFinalInfo[] := Module[{d,keys},
  If[!AssociationQ[rgzGaugeFixedCancelWork],
    Return[Print["\:5f53\:524d\:5185\:6838\:5c1a\:65e0\:9010\:56e0\:5b50\:7ea6\:5206\:8bb0\:5f55\:3002"]]];
  d=rgzGaugeFixedCancelWork;
  keys={"\:56e0\:5b50\:53f7","\:603b\:56e0\:5b50\:6570","\:7ea6\:53bb\:6b21\:6570","\:5df2\:68c0\:67e5\:6b21\:6570"};
  Grid[Prepend[({#,d[#]}& /@ keys),{"\:6700\:7ec8\:7ea6\:5206\:8fdb\:5ea6","\:503c"}],
    Frame->All,Alignment->Left]
];

Print["\:5df2\:52a0\:8f7d\:56fa\:5b9a\:5206\:6bcd\:68c0\:67e5 v1.3\:ff1a\:9010\:56e0\:5b50\:7cbe\:786e\:6574\:9664\:ff1b\:590d\:7528\:5168\:90e8\:591a\:9879\:5f0f\:7ec4\:ff1bRGZGaugeFixedFinalInfo[] \:67e5\:770b\:7ea6\:5206\:8fdb\:5ea6\:3002"];
