(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Role:
   Reduce both gauge branches over fixed factored denominators, then compare their coefficients.

   Inputs:
   Prepared master coefficients or a stopped coefficient record.

   Dependencies:
   GaugeCoefficients.wl supplies PV preparation and exact small-coefficient checks;
   Internal/GaugeRuntime.wl supplies explicitly named fixed WXF checkpoint IO.
   Loading this file does not replace any ordinary coefficient or cache function.
*)

ClearAll[RGZGaugeFixedBranch, RGZGaugeFixedCompare, RGZGaugeFixedCoordinates, RGZGaugeFixedDenResume, RGZGaugeFixedFactorReduce, RGZGaugeFixedFinish, RGZGaugeFixedPair, RGZGaugeFixedPrepare, RGZGaugeFixedPreparedQ, RGZGaugeFixedTryFactor];

(* 1. Introduce exact squared-pole coordinates. *)
RGZGaugeFixedCoordinates[raw_] := Module[
  {v,ss,xx,yy,ww,zz,tt,aa,bb,dd,bases,rootRules,terms,common,restore},
  With[{check=RGZGaugeValidate[raw]},If[FailureQ[check],Return[check]]];
  v=Table[Symbol["RGZGaugeFixedAlgebra`"<>name],
    {name,{"s","x","y","w","z1","z3","a","b","d"}}];
  If[AnyTrue[v,Function[q,OwnValues[q]=!={},HoldAll]],
    Return[Failure["AssignedFixedCoordinate",<||>]]];
  {ss,xx,yy,ww,zz,tt,aa,bb,dd}=v;
  bases={4 mG^4-(m^2-μ^2)^2,
    mG^4-(m^2-μ^2)^2/4,α M^4};
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
    HoldPattern[μ^k_Integer] /; EvenQ[k] :> yy^(k/2),
    HoldPattern[M^k_Integer] /; Mod[k,4]===0 :> ww^(k/4),
    HoldPattern[mG^k_Integer] /; Mod[k,4]===0 :>
      (-(tt-xx)(tt-yy))^(k/4),
    HoldPattern[System`N^k_] /; IntegerQ[2 k] :> aa^(2 k),
    HoldPattern[SUNN^k_] /; IntegerQ[2 k] :> bb^(2 k),
    HoldPattern[SPD[p,p]] :> ss,
    HoldPattern[Pair[Momentum[p,D],Momentum[p,D]]] :> ss,
    HoldPattern[Pair[Momentum[p],Momentum[p]]] :> ss,
    α :> -zz^2/ww,System`N :> aa^2,SUNN :> bb^2,D :> dd};
  If[!FreeQ[terms,rgzLoopM1|rgzLoopM2|rgzLoopM3|rgzLoopM4] ||
      Cases[terms,HoldPattern[Power[_,k_Rational]] /;
        Denominator[k]=!=1,Infinity]=!={},
    Return[Failure["UnsupportedFixedCoordinateRoot",<|
      "Description"->"The input contains unsupported radicals or odd pole-mass powers; reduction stopped."|>]]];
  restore={ss->SPD[p,p],xx->m^2,yy->μ^2,ww->M^4,
    zz->-I Sqrt[α M^4],
    tt->(m^2+μ^2)/2+I Sqrt[mG^4-(m^2-μ^2)^2/4],
    aa->Sqrt[System`N],bb->Sqrt[SUNN],dd->D};
  <|"Terms"->terms,"Variable"->v,"RestorationRules"->restore,"CommonFactor"->common|>
];

(* 2. Prepare one factored common denominator. *)
RGZGaugeFixedPrepare[coords_Association] := Module[{rgzFunctionReturnTag=Unique["RGZWorkflow`Private`functionReturn"]},Catch[
Module[
  {terms=coords["Terms"],vars=coords["Variable"],factor,packets,groups,
    groups2,factors,maxima,lcd,q,fl,c,num,j,bag},
  factor[e_] := factor[e]=FactorList[e];
  packets=Table[
    If[Mod[j,100]===1,RGZMXProgressUpdate[
      "TermID"->j,"TotalTermCount"->Length[terms],"CompleteTermCount"->j-1];
      RGZGaugeStep["Fixed denominator: organize original terms "<>ToString[j]<>" / "<>
        ToString[Length[terms]]]];
    q=Together[terms[[j]]];num=Numerator[q];
    If[!PolynomialQ[num,vars] || !PolynomialQ[Denominator[q],vars],
      Throw[Failure["NonPolynomialFixedCoordinate",<|"TermsID"->j|>],rgzFunctionReturnTag]];
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
  Join[coords,<|"Group"->groups2,"DenominatorFactor"->factors,
    "MaxPower"->maxima,"CommonDenominator"->lcd,"OriginalTermsCount"->Length[terms]|>]
],rgzFunctionReturnTag]];

RGZGaugeFixedFinish[unit_Association,phi_,details_Association] := Module[
  {row,path,key,expected,saved=False},
  With[{check=RGZGaugeValidate[phi]},If[FailureQ[check],Return[check]]];
  row=<|"ID"->unit["ID"],"MasterIntegral"->unit["MasterIntegral"],
    "OriginalCoefficient"->Missing["UseShortCoefficients"],"ResidualCoefficient"->phi,
    "RadicalSymbolCoefficient"->phi,"RadicalDefinition"->{},
    "Status"->RGZGaugeState[phi],"Source"->details["Source"],
    "ElapsedSeconds"->Lookup[details,"ElapsedSeconds",Missing["PreviouslyVerifiedResult"]]|>;
  rgzGaugeFixedCoefficient=row;rgzGaugeBatchLastCoefficient=row;
  path=Lookup[unit,"CachePath",None];key=Lookup[unit,"CacheIdentity",None];
  expected=Hash[{RGZGaugeCoefficientContext[],unit["ReferenceGauge"],
    {unit["MasterIntegral"],unit["AlphaShortCoefficient"],unit["ReferenceShortCoefficient"]}},"SHA256"];
  If[StringQ[path] && key===expected,
    RGZGaugeCacheWrite[row,path,key];
    saved=TimeConstrained[AssociationQ[RGZGaugeCacheRead[path,key]],10,False]];
  RGZMXProgressUpdate["Status"->"SingleTermComplete"];
  Join[<|"Status"->"SingleTermComplete","ID"->unit["ID"],
    "CoefficientStatus"->row["Status"],"ResultLeafCount"->LeafCount[phi],
    "Saveto OriginalCoefficientCache"->saved,
    "Description"->"Only the current coefficient is complete. A nonzero value does not establish gauge dependence of the full S matrix."|>,
    KeyDrop[details,{"Result"}]]
];

(* 3. Accumulate the numerator and cancel exact denominator factors. *)
Options[RGZGaugeFixedDenResume]={
  "PrepareTimeLimit"->600,"TotalTimeLimit"->1200,"CacheDirectory"->Automatic,
  "Saveinterval"->25};

RGZGaugeFixedDenResume[seconds_:60,OptionsPattern[]] := Module[
  {unit,prepLimit,totalLimit,every,root,dir=None,path=None,id,work,
    prep,num,index=0,group,exponents,multiplier,contribution,z,physical,
    details,vars,xx,yy,zz,tt,ww,alphaVar,tVar,restoreRules,worker,stop,
    answer,coords,start=AbsoluteTime[],write,checkpoint,reduced},
  If[!AssociationQ[rgzGaugeStoppedUnit],
    Return[Failure["NoStoppedCoefficient",<||>]]];
  unit=rgzGaugeStoppedUnit;
  With[{check=RGZGaugeValidate[Lookup[unit,{"MasterIntegral","AlphaShortCoefficient","ReferenceShortCoefficient","ReferenceGauge"}]]},If[FailureQ[check],Return[check]]];
  prepLimit=OptionValue["PrepareTimeLimit"];totalLimit=OptionValue["TotalTimeLimit"];
  every=OptionValue["Saveinterval"];
  If[!And@@(NumericQ[#] && TrueQ[#>0]& /@ {seconds,prepLimit,totalLimit}) ||
      !IntegerQ[every] || every<1,Return[Failure["FixedDenArguments",<||>]]];
  If[OwnValues[α]=!={},Return[Failure["AssignedGaugeParameter",<||>]]];
  If[Lookup[unit,"ReferenceShortCoefficient",Missing["Absent"]]=!=0,
    Return[Failure["FixedDenReferenceNotZero",<|
      "Description"->"This method handles only stopped terms with a literally zero reference short coefficient; the reference gauge is retained."|>]]];
  RGZMXProgressUpdate["Sector"->"RGZ FixedDenominatorCoefficientCheck","Stage"->"FixedDenominatorReduce",
    "StageStartTime"->start,"Status"->"ComputeIn","DiagramID"->0,"DiagramCount"->0];
  worker[] := Catch[
    root=RGZGaugeCacheDirectory[OptionValue["CacheDirectory"]];
    id=Hash[{"RGZFixedDen-v2",unit["MasterIntegral"],unit["AlphaShortCoefficient"],
      unit["ReferenceGauge"],DownValues[RGZGaugeFixedCoordinates],
      DownValues[RGZGaugeFixedPrepare]},"SHA256"];
    If[StringQ[root],dir=FileNameJoin[{root,"fixed_"<>IntegerString[id,16]}];
      If[!DirectoryQ[dir],CreateDirectory[dir,CreateIntermediateDirectories->True]];
      path=FileNameJoin[{dir,"checkpoint.wl"}]];
    work=If[AssociationQ[rgzGaugeFixedWork] &&
        Lookup[rgzGaugeFixedWork,"Identity",None]===id,
      rgzGaugeFixedWork,RGZGaugeFixedCacheRead[path,{id,"fixed"}]];
    If[AssociationQ[work] && RGZGaugeValidate[work]=!=True,Throw[Failure["InvalidFixedCheckpoint",<||>],stop]];
    If[AssociationQ[work],prep=work["Prepare"];num=work["Numerator"];index=work["CompletegroupCount"],
      RGZGaugeStep["Fixed denominator: introduce squared-pole coordinates and organize denominator factors"];
      prep=TimeConstrained[
        coords=RGZGaugeFixedCoordinates[unit["AlphaShortCoefficient"]];
        If[FailureQ[coords],coords,RGZGaugeFixedPrepare[coords]],prepLimit,
        Missing["FixedPreparationTimeout"]];
      If[Head[prep]===Missing || FailureQ[prep],Throw[
        Failure["FixedDenPreparationStopped",<|"Cause"->prep|>],stop]];
      num=0;index=0];
    checkpoint[] := (rgzGaugeFixedWork=<|"Identity"->id,"Prepare"->prep,
      "Numerator"->num,"CompletegroupCount"->index|>;
      If[StringQ[path],write=RGZGaugeFixedCacheWrite[
        rgzGaugeFixedWork,path,{id,"fixed"}];
        If[FailureQ[write],Throw[write,stop]]]);
    vars=prep["Variable"];
    While[index<Length[prep["Group"]],
      group=prep["Group"][[index+1]];
      RGZMXProgressUpdate["TermID"->index+1,"TotalTermCount"->Length[prep["Group"]],
        "CompleteTermCount"->index];
      RGZGaugeStep["Fixed denominator: accumulate polynomial groups "<>ToString[index+1]<>" / "<>
        ToString[Length[prep["Group"]]]];
      contribution=TimeConstrained[
        exponents=Table[Total[Cases[group[[2]],
          {f_,k_Integer} /; SameQ[f,prep["DenominatorFactor"][[j]]] :> k]],
          {j,Length[prep["DenominatorFactor"]]}];
        multiplier=Times@@MapThread[Power,{prep["DenominatorFactor"],
          prep["MaxPower"]-exponents}];
        Expand[group[[1]] multiplier],seconds,Missing["FixedGroupTimeout"]];
      If[Head[contribution]===Missing,checkpoint[];Throw[
        Failure["FixedDenGroupTimeout",<|"ID"->unit["ID"],
          "GroupID"->index+1,"TimeLimit"->seconds|>],stop]];
      z=TimeConstrained[Expand[num+contribution],seconds,
        Missing["FixedAddTimeout"]];
      If[Head[z]===Missing,checkpoint[];Throw[
        Failure["FixedDenAddTimeout",<|"GroupID"->index+1,"TimeLimit"->seconds|>],stop]];
      If[RGZGaugeValidate[z]=!=True,Throw[Failure["InvalidFixedAccumulation",<||>],stop]];
      num=z;index++;rgzGaugeFixedWork=<|"Identity"->id,"Prepare"->prep,
        "Numerator"->num,"CompletegroupCount"->index|>;
      If[Mod[index,every]===0,checkpoint[]]];
    checkpoint[];
    reduced=RGZGaugeFixedFactorReduce[num,prep,id,root,seconds];
    If[FailureQ[reduced],Throw[reduced,stop]];
    z=reduced["Numerator"]/reduced["Denominator"];
    {xx,yy,ww,zz,tt}=vars[[{2,3,4,5,6}]];
    alphaVar=Symbol["RGZGaugeFixedAlgebra`alpha"];
    tVar=Symbol["RGZGaugeFixedAlgebra`t"];
    If[AnyTrue[{alphaVar,tVar},Function[q,OwnValues[q]=!={},HoldAll]],
      Throw[Failure["AssignedFixedCoordinate",<||>],stop]];
    RGZGaugeStep["Fixed denominator: restore physical squared-mass relations"];
    z=TimeConstrained[Module[{n0=Numerator[z],d0=Denominator[z]},
      n0=PolynomialRemainder[n0,tt^2-(xx+yy)tt+xx yy+tVar,tt];
      d0=PolynomialRemainder[d0,tt^2-(xx+yy)tt+xx yy+tVar,tt];
      n0=PolynomialRemainder[n0,zz^2+alphaVar ww,zz];
      d0=PolynomialRemainder[d0,zz^2+alphaVar ww,zz];
      If[TrueQ[d0===0],Failure["PoleRelationSingularity",<||>],Cancel[n0/d0]]],
      seconds,Missing["FixedPhysicalReductionTimeout"]];
    If[FailureQ[z] || Head[z]===Missing,Throw[
      Failure["FixedDenPhysicalReductionStopped",<|"Cause"->z|>],stop]];
    restoreRules=Join[prep["RestorationRules"],{alphaVar->α,tVar->mG^4}];
    physical=prep["CommonFactor"] (z/.restoreRules);
    details=<|"Source"->"Exact reduction of original terms over a fixed common denominator",
      "OriginalTermsCount"->prep["OriginalTermsCount"],
      "CommonDenominatorFactorCount"->Length[prep["DenominatorFactor"]],
      "CompletedgroupCount"->index,"ElapsedSeconds"->AbsoluteTime[]-start|>;
    RGZGaugeFixedFinish[unit,physical,details],stop];
  answer=CheckAbort[TimeConstrained[worker[],totalLimit,
    Failure["FixedDenTotalTimeout",<|"TimeLimit"->totalLimit,
      "Description"->"Completed groups remain in memory and the latest checkpoint; rerun to resume."|>]],
    Failure["FixedDenAborted",<|"Description"->"Completed groups and previous diagram caches are retained."|>]];
  If[FailureQ[answer],RGZMXProgressUpdate["Status"->"Stopped"]];answer
];

(* 4. Reduce both gauge branches before reference-gauge substitution. *)
Options[RGZGaugeFixedBranch]=Options[RGZGaugeFixedDenResume];

RGZGaugeFixedBranch[unit_Association,raw_,seconds_,OptionsPattern[]] := Module[
  {root,path=None,key,old,branch,result,row,saved},
  With[{check=RGZGaugeValidate[raw]},If[FailureQ[check],Return[check]]];
  If[TrueQ[raw===0],Return[<|"ResidualCoefficient"->0,"Source"->"ZeroBranch"|>]];
  key=Hash[{"RGZFixedBranch-v2",raw,
    DownValues[RGZGaugeFixedCoordinates],DownValues[RGZGaugeFixedPrepare],
    DownValues[RGZGaugeFixedDenResume]},
    "SHA256"];
  root=RGZGaugeCacheDirectory[OptionValue["CacheDirectory"]];
  If[StringQ[root],path=FileNameJoin[{root,
    "fixed_branch_"<>IntegerString[key,16]<>".wl"}]];
  old=RGZGaugeFixedCacheRead[path,{key,"branch"}];
  If[AssociationQ[old] && KeyExistsQ[old,"ResidualCoefficient"] && RGZGaugeValidate[old]===True,
    Return[Join[old,<|"Source"->"BranchCacheRestored"|>]]];
  branch=Join[unit,<|"AlphaShortCoefficient"->raw,"ReferenceShortCoefficient"->0,
    "CachePath"->None,"CacheIdentity"->None|>];
  (* Do not put a one-sided intermediate result in the original difference cache. *)
  result=Block[{rgzGaugeStoppedUnit=branch,rgzGaugeFixedCoefficient,
      rgzGaugeBatchLastCoefficient},
    row=RGZGaugeFixedDenResume[seconds,
      "PrepareTimeLimit"->OptionValue["PrepareTimeLimit"],
      "TotalTimeLimit"->OptionValue["TotalTimeLimit"],
      "CacheDirectory"->OptionValue["CacheDirectory"],
      "Saveinterval"->OptionValue["Saveinterval"]];
    If[AssociationQ[row] && AssociationQ[rgzGaugeFixedCoefficient],
      KeyTake[rgzGaugeFixedCoefficient,{"ResidualCoefficient","Source"}],row]];
  If[!AssociationQ[result],Return[result]];
  saved=RGZGaugeFixedCacheWrite[result,path,{key,"branch"}];
  If[FailureQ[saved],Return[saved]];result
];

Options[RGZGaugeFixedPair]=Join[Options[RGZGaugeFixedDenResume],
  {"DirectThreshold"->2000}];

RGZGaugeFixedPair[unit_Association,seconds_:60,OptionsPattern[]] := Module[
  {a,b,ref,small,worker,answer,ca,cb,reduced,report,start=AbsoluteTime[],stop},
  If[!And@@(KeyExistsQ[unit,#]& /@
      {"ID","MasterIntegral","AlphaShortCoefficient","ReferenceShortCoefficient","ReferenceGauge"}),
    Return[Failure["IncompleteStoppedInput",<||>]]];
  With[{check=RGZGaugeValidate[Lookup[unit,{"MasterIntegral","AlphaShortCoefficient","ReferenceShortCoefficient","ReferenceGauge"}]]},If[FailureQ[check],Return[check]]];
  {a,b,ref}=Lookup[unit,{"AlphaShortCoefficient","ReferenceShortCoefficient","ReferenceGauge"}];
  If[OwnValues[α]=!={},Return[Failure["AssignedGaugeParameter",<||>]]];
  If[!NumericQ[ref] || !TrueQ[ref>0],
    Return[Failure["GaugeComparisonArguments",<||>]]];
  small=OptionValue["DirectThreshold"];
  If[!IntegerQ[small] || small<0,
    Return[Failure["FixedDenArguments",<||>]]];
  worker[] := Catch[
    If[LeafCount[a]+LeafCount[b]<=small,
      RGZGaugeStep["Fixed denominator: check small coefficients"];
      reduced=TimeConstrained[RGZGaugeCoefficient[a,b,ref],seconds,
        Missing["SmallCoefficientTimeout"]];
      If[AssociationQ[reduced],
        Throw[RGZGaugeFixedFinish[unit,reduced["ResidualCoefficient"],
          <|"Source"->"Exact radical check for small coefficients",
            "ElapsedSeconds"->AbsoluteTime[]-start|>],stop]];
      (* A timeout of the direct small path switches to the fixed denominator. *)
      If[FailureQ[reduced],Throw[reduced,stop]]];
    RGZGaugeStep["Fixed denominator: reduce the current-gauge branch"];
    ca=RGZGaugeFixedBranch[unit,a,seconds,
      "PrepareTimeLimit"->OptionValue["PrepareTimeLimit"],
      "TotalTimeLimit"->OptionValue["TotalTimeLimit"],
      "CacheDirectory"->OptionValue["CacheDirectory"],
      "Saveinterval"->OptionValue["Saveinterval"]];
    If[!AssociationQ[ca],Throw[ca,stop]];
    RGZGaugeStep["Fixed denominator: reduce the reference-gauge branch"];
    cb=RGZGaugeFixedBranch[unit,b,seconds,
      "PrepareTimeLimit"->OptionValue["PrepareTimeLimit"],
      "TotalTimeLimit"->OptionValue["TotalTimeLimit"],
      "CacheDirectory"->OptionValue["CacheDirectory"],
      "Saveinterval"->OptionValue["Saveinterval"]];
    If[!AssociationQ[cb],Throw[cb,stop]];
    (* GaugeCoefficient inserts the physical pole definitions in BOTH branches,
       then sets alpha -> ref in the reference branch. Never replace the
       reference poles by the still-variable current-gauge poles. *)
    RGZGaugeStep["Fixed denominator: subtract compacted gauge branches"];
    reduced=TimeConstrained[RGZGaugeCoefficient[
      ca["ResidualCoefficient"],cb["ResidualCoefficient"],ref],seconds,
      Missing["CompactDifferenceTimeout"]];
    If[Head[reduced]===Missing,Throw[Failure["FixedDenDifferenceTimeout",<|
      "ID"->unit["ID"],"TimeLimit"->seconds,
      "StoppedOperation"->"twoItemscompactGaugeBranchSubtract",
      "Description"->"Completed branch caches are retained; this difference remains unchecked."|>],stop]];
    If[FailureQ[reduced],Throw[reduced,stop]];
    If[!AssociationQ[reduced],Throw[Failure["InvalidFixedResult",<||>],stop]];
    RGZGaugeFixedFinish[unit,reduced["ResidualCoefficient"],
      <|"Source"->"Reduce each gauge branch with fixed denominators, then compare",
        "ElapsedSeconds"->AbsoluteTime[]-start|>],stop];
  answer=CheckAbort[TimeConstrained[worker[],OptionValue["TotalTimeLimit"],
      Failure["FixedDenPairTimeout",<|"ID"->unit["ID"],
        "TimeLimit"->OptionValue["TotalTimeLimit"],
        "Description"->"The current coefficient reached the total time limit; completed groups and branch caches can be reused."|>]],
    Failure["FixedDenAborted",<|"ID"->unit["ID"]|>]];
  If[FailureQ[answer],rgzGaugeStoppedUnit=unit;
    RGZMXProgressUpdate["Status"->"Stopped"]];answer
];

(* 5. Compare the complete prepared master-coefficient list. *)
Options[RGZGaugeFixedCompare]=Join[Options[RGZGaugeFixedPair],
  {"PreprocessTimeLimit"->600,"CoefficientID"->All}];

RGZGaugeFixedCompare[expr_,reference_:1,seconds_:60,OptionsPattern[]] := Module[
  {prepLimit,root,prepKey,prepPath=None,prepared,pairs,indices,context,
    unit,path,key,row,report,n,total,rows={},stats,answer,start=AbsoluteTime[],
    selected,stop,run,part,pathOption},
  With[{check=RGZGaugeValidate[expr]},If[FailureQ[check],Return[check]]];
  prepLimit=OptionValue["PreprocessTimeLimit"];selected=OptionValue["CoefficientID"];
  If[OwnValues[α]=!={},Return[Failure["AssignedGaugeParameter",<||>]]];
  If[!NumericQ[reference] || !TrueQ[reference>0] ||
      !And@@(NumericQ[#] && TrueQ[#>0]& /@
        {seconds,prepLimit,OptionValue["PrepareTimeLimit"],OptionValue["TotalTimeLimit"]}),
    Return[Failure["GaugeComparisonArguments",<||>]]];
  If[!FreeQ[expr,_Failure|_Missing|$Failed|$Aborted|_TID|_PaVeReduce|
      _FeynAmpDenominator|_LorentzIndex|_ExplicitLorentzIndex|_DiracGamma|
      _SUNIndex|_SUNF|_SUND|_SUNDelta],
    Return[Failure["NeedIntegratedColourProjectedScalar",<||>]]];
  RGZMXProgressUpdate["Sector"->"RGZ FixedDenominatorGaugeCheck",
    "Stage"->"PrepareMasterIntegralCoefficient","StageStartTime"->start,
    "DiagramID"->0,"DiagramCount"->0,"TermID"->0,"TotalTermCount"->0,
    "CompleteTermCount"->0,"Status"->"ComputeIn"];
  root=RGZGaugeCacheDirectory[OptionValue["CacheDirectory"]];
  prepKey=Hash[{"RGZFixedPrepared-v2",expr,reference,
    DownValues[RGZGaugePrepare],DownValues[RGZGaugePVPairs],
    DownValues[RGZGaugeCanonicalPV],DownValues[RGZLoopPoleMasses]},"SHA256"];
  If[StringQ[root],prepPath=FileNameJoin[{root,
    "fixed_prepared_"<>IntegerString[prepKey,16]<>".wl"}]];
  answer=CheckAbort[TimeConstrained[
    If[AssociationQ[rgzGaugeFixedPrepared] &&
        Lookup[rgzGaugeFixedPrepared,"Identity",None]===prepKey && RGZGaugeFixedPreparedQ[rgzGaugeFixedPrepared],
      prepared=rgzGaugeFixedPrepared,
      prepared=RGZGaugeFixedCacheRead[prepPath,{prepKey,"prepared"}];
      If[!AssociationQ[prepared] || !RGZGaugeFixedPreparedQ[prepared],
        RGZGaugeStep["Fixed denominator: combine matching master integrals (coefficient preprocessing only)"];
        pairs=RGZGaugePrepare[expr,reference,prepLimit];
        If[FailureQ[pairs] || !ListQ[pairs],Return[pairs]];
        prepared=<|"Identity"->prepKey,"ReferenceGauge"->reference,"CoefficientList"->pairs,"ContentHash"->Hash[pairs,"SHA256"]|>;
        rgzGaugeFixedPrepared=prepared;
        report=RGZGaugeFixedCacheWrite[prepared,prepPath,{prepKey,"prepared"}];
        If[FailureQ[report],Return[report]]]],prepLimit,
    Failure["FixedDenPreparationTimeout",<|"Stage"->"MasterIntegralseparate and CacheRead",
      "TimeLimit"->prepLimit|>]],Failure["FixedDenAborted",<|"Stage"->"Preprocess"|>]];
  If[FailureQ[answer] || !AssociationQ[prepared],
    RGZMXProgressUpdate["Status"->"Stopped"];Return[answer]];
  rgzGaugeFixedPrepared=prepared;pairs=prepared["CoefficientList"];total=Length[pairs];
  indices=Which[selected===All,Range[total],IntegerQ[selected],{selected},
    ListQ[selected],DeleteDuplicates[selected],True,{}];
  If[selected=!=All && (indices==={} ||
      !AllTrue[indices,IntegerQ[#] && 1<=#<=total&]),
    Return[Failure["ResidualRowNumber",<|"TotalTermCount"->total|>]]];
  context=RGZGaugeCoefficientContext[];
  rgzGaugeCompletedRows={};
  rgzGaugeProgress=<|"CompletedTermCount"->0,"MasterIntegralTermCount"->total,
    "Statistics"-><||>,"CurrentTerm"->0,"CacheDirectory"->root|>;
  run[] := Catch[Do[
    RGZMXProgressUpdate["Stage"->"Coefficient "<>ToString[n]<>" / "<>ToString[total],
      "TermID"->n,"TotalTermCount"->total,"CompleteTermCount"->Length[rows]];
    AssociateTo[rgzGaugeProgress,"CurrentTerm"->n];
    key=Hash[{context,reference,pairs[[n]]},"SHA256"];
    path=If[StringQ[root],FileNameJoin[{root,
      "coefficient_"<>IntegerString[key,16]<>".wl"}],None];
    unit=<|"ID"->n,"TotalTermCount"->total,"MasterIntegral"->pairs[[n,1]],
      "AlphaShortCoefficient"->pairs[[n,2]],"ReferenceShortCoefficient"->pairs[[n,3]],
      "ReferenceGauge"->reference,"TimeLimit"->seconds,
      "CachePath"->path,"CacheIdentity"->key,"CompletedTermCount"->Length[rows]|>;
    rgzGaugeStoppedUnit=unit;
    row=RGZGaugeCacheRead[path,key];
    If[!AssociationQ[row],
      report=RGZGaugeFixedPair[unit,seconds,
        "PrepareTimeLimit"->OptionValue["PrepareTimeLimit"],
        "TotalTimeLimit"->OptionValue["TotalTimeLimit"],
        "CacheDirectory"->OptionValue["CacheDirectory"],
        "Saveinterval"->OptionValue["Saveinterval"],
        "DirectThreshold"->OptionValue["DirectThreshold"]];
      If[!AssociationQ[report],Throw[Failure["FixedDenCoefficientStopped",<|
        "ID"->n,"TotalTermCount"->total,"CompletedTermCount"->Length[rows],
        "StoppedOperation"->Lookup[rgzGaugeProgress,"CurrentOperation",""],
        "Cause"->report,"Description"->"Stopped at the current coefficient; previous diagram, integral and completed-coefficient caches are retained."|>],stop]];
      row=rgzGaugeFixedCoefficient,
      row=Join[row,<|"ID"->n,"Source"->"OriginalCoefficientCacheRestored"|>]];
    AppendTo[rows,row];rgzGaugeCompletedRows=rows;
    AssociateTo[rgzGaugeProgress,{"CompletedTermCount"->Length[rows],
      "Statistics"->Counts[(#["Status"]& /@ rows)]}],{n,indices}];
    stats=Counts[(#["Status"]& /@ rows)];
    <|"ReferenceGauge"->reference,"MasterIntegralTermCount"->total,
      "CheckedCount"->Length[rows],"Statistics"->stats,
      "Status"->If[Length[rows]=!=total,"Selected coefficients checked",
        If[AllTrue[rows,#["Status"]==="Zero"&],
          "The checked expression has zero gauge residual","NotYetproofGaugeIndependent"]],
      "ResidualParts"->rows,
      "Description"->"Check the current candidate at finite M without assuming master-integral independence or pinch completeness."|>,stop];
  answer=CheckAbort[run[],Failure["FixedDenAborted",<|
    "CompletedTermCount"->Length[rows],"Description"->"Rerunning reuses completed coefficients and branch caches."|>]];
  RGZMXProgressUpdate["Status"->If[FailureQ[answer],"Stopped","CheckComplete"]];
  rgzGaugeFixedReport=answer;answer
];

RGZGaugeFixedPreparedQ[x_] := AssociationQ[x] && ListQ[Lookup[x,"CoefficientList",None]] &&
  Lookup[x,"ContentHash",None]===Hash[x["CoefficientList"],"SHA256"] &&
  AllTrue[x["CoefficientList"],MatchQ[#,{_,_,_}]&] && RGZGaugeValidate[x]===True;

(* 6. Exact factor division used by numerator completion. *)
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
  {factors=prep["DenominatorFactor"],powers=prep["MaxPower"],vars=prep["Variable"],
    identity,path=None,state,n=num,j=1,remaining,attempts=0,divided=0,
    q,save,write,stop,answer},
  identity=Hash[{"RGZFixedFactor-v2",id,factors,powers,
    DownValues[RGZGaugeFixedTryFactor]},"SHA256"];
  If[StringQ[root],path=FileNameJoin[{root,"fixed_"<>IntegerString[id,16],
    "factor_cancel_"<>IntegerString[identity,16]<>".wl"}]];
  state=If[AssociationQ[rgzGaugeFixedCancelWork] &&
      Lookup[rgzGaugeFixedCancelWork,"Identity",None]===identity,
    rgzGaugeFixedCancelWork,RGZGaugeFixedCacheRead[path,{identity,"factor"}]];
  If[AssociationQ[state] && RGZGaugeValidate[state]=!=True,Return[Failure["InvalidFixedCheckpoint",<||>]]];
  remaining=powers;
  If[AssociationQ[state],
    n=state["Numerator"];remaining=state["RemainingPower"];j=state["FactorID"];
    attempts=state["CheckCount"];divided=state["CancelledPower"]];
  save[] := (
    rgzGaugeFixedCancelWork=<|"Identity"->identity,"Numerator"->n,
      "RemainingPower"->remaining,"FactorID"->j,"CheckCount"->attempts,
      "CancelledPower"->divided,"TotalFactorCount"->Length[factors]|>;
    write=RGZGaugeFixedCacheWrite[rgzGaugeFixedCancelWork,path,{identity,"factor"}];
    If[FailureQ[write],Throw[write,stop]]);
  answer=Catch[
    While[j<=Length[factors],
      If[TrueQ[n===0],remaining=ConstantArray[0,Length[factors]];
        j=Length[factors]+1;Break[]];
      If[remaining[[j]]===0,j++;Continue[]];
      RGZMXProgressUpdate["TermID"->j,"TotalTermCount"->Length[factors],
        "CompleteTermCount"->j-1];
      RGZGaugeStep["Fixed denominator: exact factor division "<>ToString[j]<>" / "<>
        ToString[Length[factors]]<>"; RemainingPower "<>ToString[remaining[[j]]]];
      q=TimeConstrained[RGZGaugeFixedTryFactor[n,factors[[j]],vars],seconds,
        Missing["FactorDivisionTimeout"]];
      If[q===Missing["FactorDivisionTimeout"],save[];
        Throw[Failure["FixedDenFactorCancelTimeout",<|
          "FactorID"->j,"TotalFactorCount"->Length[factors],"RemainingPower"->remaining[[j]],
          "TimeLimit"->seconds,"CancelledPower"->divided,
          "Description"->"Factor cancellation progress saved; rerun to resume from this factor without recomputing polynomial groups."|>],stop]];
      If[FailureQ[q],save[];Throw[q,stop]];
      If[q=!=Missing["NotDivisible"] && RGZGaugeValidate[q]=!=True,
        save[];Throw[Failure["InvalidFactorDivisionResult",<|"FactorID"->j|>],stop]];
      attempts++;
      If[q===Missing["NotDivisible"],j++,n=q;remaining[[j]]--;divided++];
      (* Commit before IO, so even an interrupted save retains exact memory progress. *)
      save[]];
    save[];
    <|"Numerator"->n,"Denominator"->Times@@MapThread[Power,{factors,remaining}],
      "CancelledPower"->divided,"CheckCount"->attempts|>,stop];
  answer
];
