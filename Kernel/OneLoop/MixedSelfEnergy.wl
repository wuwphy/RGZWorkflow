(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Role:
   Generate mixed-field graphs, run each sector and assemble the mixed matrices.

   Inputs:
   The selected model, ordered external-field pair and completed per-graph contributions.

   Dependencies:
   loadLoopEngine[] loads common AA conventions, algebra and runtime definitions first.
   This file does not load other source files; IO and stage execution are in Internal/MixedRuntime.wl.
*)

ClearAll[RGZMXAntiField, RGZMXAuxQ, RGZMXConvert, RGZMXConvertOne, RGZMXFCOptions, RGZMXField, RGZMXGenerateGraphs, RGZMXLabel, RGZMXLorentzNames, RGZMXPairID, RGZMXPrepareFA, RGZMXTotal, RGZMXUnfixedKinematicsQ, RGZMXVectorQ, RGZMixAssembleInsertion, RGZMixBasis, RGZMixComputeAll, RGZMixComputePair, RGZMixMatrix, RGZMixReadPair, RGZMixResult, RGZMixSectorNames, RGZMixStartPair, RGZMixTensor, RGZMixValidatePair, rgzMixTopGhostTadpole];

(* Resolve option SYMBOLS from the active FCFAConvert definition.
   This avoids RGZWorkflow`Engine`/FeynCalc` shadowing and omits optional settings
   absent from older versions. Keep DropSumOver->False mandatory. *)
RGZMXFCOptions[] := Module[{available,wanted,out={},match,name},
  available=First/@Options[FCFAConvert];
  If[!AnyTrue[available,RGZLoopName[#]==="DropSumOver"&],
    Return[Failure["UnsupportedConverter",<|
      "Description"->"This FCFAConvert has no DropSumOver option; sum labels cannot be retained.",
      "ActualselectedTerm"->Options[FCFAConvert]|>]]];
  wanted={"IncomingMomenta"->{p},"OutgoingMomenta"->{p},
    "LoopMomenta"->{q},"UndoChiralSplittings"->False,
    "LorentzIndexNames"->(RGZMXLorentzNames@@rgzMXJob["FieldPair"]),"ChangeDimension"->D,
    "List"->True,"Contract"->False,"DropSumOver"->False,
    "FeynAmpDenominatorCombine"->False,"FCFADiracChainJoin"->False,
    "DropIndexSum"->False};
  Do[name=First[rule];
    match=Select[available,RGZLoopName[#]===name&,1];
    If[match=!={},AppendTo[out,First[match]->Last[rule]]],{rule,wanted}];
  out
];

(* Preserve the FeynAmpList metadata and original symmetry factors.
   No FCFAConvert here: all raw graph checkpoints exist before conversion. *)
RGZMXPrepareFA[diagrams_,scalarC_:False,seconds_:60] := Module[
  {fa,stop,audits={},prepared},Catch[
  If[Length[diagrams]===0,Throw[<|"FAAmplitudeList"->{},"OriginalSumAudit"->{}|>,stop]];
  fa=RGZMXInitStep["CreateFeynAmp",
    CreateFeynAmp[diagrams,Truncated->True,GaugeRules->{},PreFactor->1],seconds];
  If[FailureQ[fa],Throw[fa,stop]];
  (* FeynArts attaches particle indices to a symbolic class mass. Both
     quark masses are color independent; strip those arguments first. *)
  fa=fa /. {HoldPattern[RGZWorkflow`Engine`mQ1[___]] :> RGZWorkflow`Engine`mQ1,
    HoldPattern[RGZWorkflow`Engine`mQ2[___]] :> RGZWorkflow`Engine`mQ2};
  fa=fa /. RGZWorkflow`Private`quarkMassRules[];
  prepared=RGZMXInitStep["Original FeynArts SumAudit",
    RGZMXPrepareIndices[fa] /. (HoldPattern[h_[id_,lm_,integrand_,rest___]] /;
      MemberQ[{"FeynAmp","FAFeynAmp"},RGZLoopName[h]] :> Module[{record},
        record=RGZMXAuditSums[integrand,scalarC];
        If[FailureQ[record],Throw[record,stop]];
        AppendTo[audits,record["SumAudit"]];
        h[id,lm,record["Expression"],rest]]),seconds];
  If[FailureQ[prepared],Throw[prepared,stop]];
  If[Length[prepared]=!=Length[audits],
    Throw[Failure["RawAmplitudeCount",<|"AmplitudeCount"->Length[prepared],
      "AuditCount"->Length[audits]|>],stop]];
  <|"FAAmplitudeList"->prepared,"OriginalSumAudit"->audits|>,stop]
];

(* Input is a ONE-amplitude FeynAmpList with its metadata intact.
   Closed-quark Dirac traces are evaluated in symbolic D; remaining
   tensor/colour contractions are deferred to the projection stage. *)
RGZMXConvertOne[prepared_,scalarC_:False,rawAudit_List:{}] := Module[
  {opts,fc,a,bad},
  opts=RGZMXFCOptions[];If[FailureQ[opts],Return[opts]];
  RGZMXProgressUpdate["Substep"->"FCFAConvert"];
  RGZMXReport["Verbose","Convert: FCFAConvert (SingleDiagram; NotcontractionTensor)"];
  fc=FCFAConvert[prepared,Sequence@@opts];
  If[FailureQ[fc],Return[fc]];
  If[!ListQ[fc] || Length[fc]=!=1,
    Return[Failure["ConversionFailed",<|"Output"->fc|>]]];
  bad=RGZLoopUnresolved[First[fc]];
  If[bad=!={},Return[Failure["UnresolvedCoupling",<|"Objects"->bad|>]]];
  RGZMXProgressUpdate["Substep"->"SumAudit"];
  RGZMXReport["Verbose","Convert: FeynCalc SumAudit"];
  fc=fc /. DiracTrace[x_,opts___] :> DiracTrace[x,DiracTraceEvaluate->True];
  a=RGZMXAuditSums[First[fc],scalarC];If[FailureQ[a],Return[a]];
  <|"Amplitude"->a["Expression"],"SumAudit"->Join[rawAudit,a["SumAudit"]]|>
];

(* Compatibility helper: still converts a whole list when explicitly
   called, but one graph at a time. RGZMXStart uses only PrepareFA. *)
RGZMXConvert[diagrams_,scalarC_:False,seconds_:60] := Module[
  {prepared,fa,out={},a,i,stop},Catch[
  prepared=RGZMXPrepareFA[diagrams,scalarC,seconds];
  If[FailureQ[prepared],Throw[prepared,stop]];
  fa=prepared["FAAmplitudeList"];
  Do[
    a=RGZMXInitStep["FCFAConvert："<>ToString[i]<>" / "<>ToString[Length[fa]],
      RGZMXConvertOne[Apply[Head[fa],{fa[[i]]}],scalarC,
        prepared["OriginalSumAudit"][[i]]],seconds];
    If[FailureQ[a],Throw[a,stop]];AppendTo[out,a],{i,Length[fa]}];
  out,stop]
];

RGZMXField[i_Integer] := {V[1],S[3],S[2],S[1],V[2],-V[2],V[3],-V[3]}[[i]];

RGZMXAntiField[i_Integer] := If[MemberQ[{5,6,7,8},i],-RGZMXField[i],RGZMXField[i]];

RGZMXVectorQ[i_] := MemberQ[{1,5,6,7,8},i];

RGZMXAuxQ[i_] := MemberQ[{5,6,7,8},i];

RGZMXLabel[i_Integer] := {"A","b","\[Tau]","\[Xi]","\[Phi]",
  "\[Phi]bar","\[Beta]","\[Beta]bar"}[[i]];

RGZMXPairID[x_Integer] /; 1<=x<=8 := x;

RGZMXPairID[x_String] := Module[{k},
  k=FirstPosition[RGZMXLabel/@Range[8],x,Missing["Field"]];
  If[Head[k]===Missing,Failure["FieldName",<|"Name"->x,
    "AvailableName"->(RGZMXLabel/@Range[8])|>],First[k]]];

RGZMXPairID[x_] := Failure["FieldName",<|"Name"->HoldForm[x]|>];

RGZMXLorentzNames[i_,j_] := If[RGZMXVectorQ[i] && RGZMXVectorQ[j],
  {\[Mu],\[Nu]},If[RGZMXVectorQ[i] || RGZMXVectorQ[j],{\[Mu]},{}]];

RGZMXUnfixedKinematicsQ[] := SameQ[
  ChangeDimension[FCI[{SPD[p,p],SPD[p,q],SPD[q,q]}],D],
  {Pair[Momentum[p,D],Momentum[p,D]],Pair[Momentum[p,D],Momentum[q,D]],
    Pair[Momentum[q,D],Momentum[q,D]]}];

(* Graph generation: each disjoint internal-field route is counted once. *)
RGZMXGenerateGraphs[pair_,tops_,process_,content_,seconds_] := Module[
 {ins=<||>,rows={},sector,routes,route,data,classes,sub,a,fa,n=0,i,j,stop},Catch[
    Do[
      routes=If[sector==="FourPointVertexLoop",
        {{tops[sector],False},{rgzMixTopGhostTadpole,True}},
        If[sector==="QuarkBubble",{{tops[sector],"F"}},{{tops[sector],sector==="GhostThreePointLoop"}}]];
      Do[
      data=RGZMXInitStep["InsertFields："<>sector,
        InsertFields[route[[1]],process,InsertionLevel->{Particles},
          Model->modelRGZ,GenericModel->modelRGZ],seconds];
      If[FailureQ[data],Throw[data,stop]];
      (* Disjoint routes: V/S tadpole (one insertion), pure U tadpole
         (none), V/S bubble (two), pure U and pure F bubbles (none). *)
      data=RGZMXInitStep["filter internal loop Field: "<>sector,
        If[route[[2]]==="F",
          DiagramSelect[data,With[{ff=LoopFields[##]},ff=!={}&&And@@(MatchQ[#,_F|-_F]&/@ff)]&],
        If[TrueQ[route[[2]]],
          DiagramSelect[data,With[{ff=LoopFields[##]},ff=!={} &&
            And@@(MatchQ[#,_U|-_U]&/@ff)]&],
          DiagramSelect[data,FreeQ[LoopFields[##],_U|_F]&]]],seconds];
      If[FailureQ[data],Throw[data,stop]];
      AssociateTo[ins,(sector<>Which[route[[2]]==="F",":F",TrueQ[route[[2]]],"：U",True,"：VS"])->data];
      classes=Which[route[[2]]==="F",content["QuarkClasses"],TrueQ[route[[2]]],content["GhostClasses"],True,{0}];
      Do[
        sub=If[j===0,data,With[{jj=j,fieldHead=If[route[[2]]==="F","F","U"]},DiagramSelect[data,
          With[{ids=DeleteDuplicates@Cases[LoopFields[##],
            h_[k_Integer,___] /; RGZLoopName[h]===fieldHead :> k,Infinity]},ids==={jj}]&]]];
        a=RGZMXPrepareFA[sub,TrueQ[route[[2]]]&&MemberQ[content["ScalarGhostClasses"],j],seconds];
        If[FailureQ[a],Throw[Failure["AuditStopped",<|"FieldPair"->pair,
          "Category"->sector,"UCategory"->j,"Cause"->a|>],stop]];
        fa=a["FAAmplitudeList"];
        Do[n++;AppendTo[rows,<|"ID"->n,"Category"->sector,"UCategory"->If[route[[2]]==="F",0,j],"FClass"->If[route[[2]]==="F",j,0],"ScalarGhost"->(TrueQ[route[[2]]]&&MemberQ[content["ScalarGhostClasses"],j]),
          "WithinClassID"->i,"OriginalFAAmplitude"->Apply[Head[fa],{fa[[i]]}],
          "OriginalSumAudit"->a["OriginalSumAudit"][[i]],
          "Amplitude"->Missing["NotConverted"],"SumAudit"->{},
          "Status"-><|"Convert"->"NotRun"|>,"FailedRecord"-><||>|>],{i,Length[fa]}],
        {j,classes}],{route,routes}],{sector,Keys[tops]}];
 <|"InsertionDiagram"->ins,"PerDiagram"->rows|>,stop]];

RGZMXTotal[] := Module[{rows,incomplete,groups,value},
  If[!AssociationQ[rgzMXJob],Return[Failure["StartFirst",<||>]]];
  rows=RGZMXMigrateRow/@rgzMXJob["PerDiagram"];
  incomplete=Select[rows,Lookup[#["Status"],"IntegrateT",""]=!="Complete" ||
    Lookup[#["Status"],"IntegrateL",""]=!="Complete"&];
  If[incomplete=!={},Return[Failure["IncompleteGraphs",<|
    "IncompleteID"->Lookup[incomplete,"ID"]|>]]];
  groups=GroupBy[rows,#["Category"]&];
  value[row_,component_] := If[
    Lookup[row["Status"],"Scalar"<>component,""]==="Complete",
    row["Scalar"<>component],row["Integrate"<>component]];
  <|"ComputeCategory"->rgzMXJob["ComputeCategory"],
    "include AllthreeItemsSector"->TrueQ[Length[rgzMXJob["ComputeCategory"]]===3],
    "ClassifiedResult"->Map[<|"DiagramCount"->Length[#],"T"->Total[value[#,"T"]&/@#],
      "L"->Total[value[#,"L"]&/@#]|>&,groups],
    "T"->Total[value[#,"T"]&/@rows],"L"->Total[value[#,"L"]&/@rows],
    "Description"->"Ordinary amputated mixed insertion retaining external colors, i and pole-mass labels."|>
];

rgzMixTopGhostTadpole=TopologyList[Topology[2][
  Propagator[Incoming][Vertex[1][1],Vertex[4][3]],
  Propagator[Outgoing][Vertex[1][2],Vertex[4][3]],
  Propagator[FALoop[1]][Vertex[4][3],Vertex[4][3]]]];

RGZMixSectorNames={"FourPointVertexLoop","BosonicThreePointLoop","GhostThreePointLoop","QuarkBubble"};

RGZMixBasis["L"] := RGZMXLabel/@Range[8];

RGZMixBasis["T"] := RGZMXLabel/@{1,5,6,7,8};

RGZMixBasis["Tensor"] := RGZMXLabel/@Range[8];

RGZMixValidatePair[a_,b_] := Module[{pair=RGZMXPairID/@{a,b}},
  If[AnyTrue[pair,FailureQ],First[Select[pair,FailureQ]],pair]];

Options[RGZMixStartPair]={"Category"->All,"SingleStepTimeLimit"->60,"reuseAACache"->True};

RGZMixStartPair[a_,b_,OptionsPattern[]] := Module[{pair,r,selected},
  pair=RGZMixValidatePair[a,b];If[FailureQ[pair],Return[pair]];
  r=RGZMixPrepare[];If[FailureQ[r],Return[r]];
  selected=OptionValue["Category"];
  If[StringQ[selected],selected={selected}];
  RGZMXStart["FieldPair"->pair,"Category"->selected,"SingleStepTimeLimit"->OptionValue["SingleStepTimeLimit"],
    "reuseAACache"->OptionValue["reuseAACache"]]
];

Options[RGZMixComputePair]={"Category"->All,"reuseAACache"->True};

RGZMixComputePair[a_,b_,seconds_:300,OptionsPattern[]] := Module[
  {pair,r,selected,sector,old,entry,rows,total,stop},Catch[
  pair=RGZMixValidatePair[a,b];If[FailureQ[pair],Throw[pair,stop]];
  If[!NumericQ[seconds] || !TrueQ[seconds>0],Throw[Failure["InvalidTimeLimit",<||>],stop]];
  r=RGZMixPrepare[];If[FailureQ[r],Throw[r,stop]];
  selected=OptionValue["Category"];
  If[selected===All,selected=RGZMixSectorNames];If[StringQ[selected],selected={selected}];
  If[!ListQ[selected] || selected==={} || !And@@(MemberQ[RGZMixSectorNames,#]&/@selected),
    Throw[Failure["SectorName",<|"AvailableCategory"->RGZMixSectorNames|>],stop]];
  Do[
    old=RGZMixRead[pair,sector];If[FailureQ[old],Throw[old,stop]];
    If[AssociationQ[old] && Lookup[old,"Status",""]==="Complete",Continue[]];
    r=RGZMXStart["FieldPair"->pair,"Category"->{sector},"SingleStepTimeLimit"->seconds,
      "reuseAACache"->OptionValue["reuseAACache"]];
    If[FailureQ[r],
      entry=<|"Status"->"InitializeFailed","DiagramCount"->Missing["NotGenerated"],"StoppedCause"->r|>;
      With[{w=RGZMixStore[pair,sector,entry]},If[FailureQ[w],Throw[w,stop]]];Throw[r,stop]];
    rows=rgzMXJob["PerDiagram"];
    entry=<|"Status"->"ComputeIn","DiagramCount"->Length[rows],"PerDiagramCache"->rgzMXJob["CacheDirectory"]|>;
    r=RGZMixStore[pair,sector,entry];If[FailureQ[r],Throw[r,stop]];
    (* Empty graphs give exact zero only after InsertFields and all audits
       returned normally. A timed-out graph is never zero. *)
    If[rows==={},total=<|"T"->0,"L"->0|>,
      r=RGZMXRunAll[{"Project","DecomposeT","IntegrateT","ScalarT",
        "DecomposeL","IntegrateL","ScalarL"},seconds];
      If[FailureQ[r],
        entry=Join[entry,<|"Status"->"Incomplete","StoppedCause"->r|>];
        With[{w=RGZMixStore[pair,sector,entry]},If[FailureQ[w],Throw[w,stop]]];
        RGZMXReport["Error","FieldPair ",RGZMXLabel/@pair," stopin FailedStage; this BeforeResultSave. "];
        Throw[r,stop]];
      total=RGZMXTotal[];If[FailureQ[total],Throw[total,stop]]];
    entry=Join[entry,<|"Status"->"Complete","T"->total["T"],"L"->total["L"]|>];
    r=RGZMixStore[pair,sector,entry];If[FailureQ[r],Throw[r,stop]];
    RGZMXReport["Summary",StringRiffle[RGZMXLabel/@pair," / "],"；",sector,
      "；",Length[rows]," Diagram; Complete. "],{sector,DeleteDuplicates[selected]}];
  <|"FieldPair"->(RGZMXLabel/@pair),"CompleteCategory"->DeleteDuplicates[selected],"Status"->"Complete"|>,stop]
];

Options[RGZMixComputeAll]={"FieldPair"->All,"Category"->All,"reuseAACache"->True};

RGZMixComputeAll[seconds_:300,OptionsPattern[]] := Module[{pairs,r,pair,stop},Catch[
  pairs=OptionValue["FieldPair"];
  If[pairs===All,pairs=Tuples[Range[8],2]];
  If[!ListQ[pairs] || !And@@(MatchQ[#,{_,_}]&/@pairs),
    Throw[Failure["FieldPairs",<||>],stop]];
  pairs=RGZMixValidatePair@@#&/@pairs;
  If[AnyTrue[pairs,FailureQ],Throw[First[Select[pairs,FailureQ]],stop]];
  Do[r=RGZMixComputePair[Sequence@@pair,seconds,"Category"->OptionValue["Category"],
      "reuseAACache"->OptionValue["reuseAACache"]];
    If[FailureQ[r],Throw[r,stop]],{pair,DeleteDuplicates[pairs]}];
  RGZMixStatus[],stop]
];

RGZMixReadPair[pair_List] := Module[{r,parts,bad},
  RGZMixResultStep["CheckModelAndSummaryCachePath",pair];
  r=RGZMixPrepare[];If[FailureQ[r],Return[r]];
  RGZMixResultStep["Read and sum all sectors",pair];
  parts=RGZMixRead[pair,#]&/@RGZMixSectorNames;
  bad=Select[parts,FailureQ];If[bad=!={},Return[First[bad]]];
  If[!And@@(AssociationQ[#] && Lookup[#,"Status",""]==="Complete"&/@parts),
    Return[Missing["IncompletePair",RGZMXLabel/@pair]]];
  <|"T"->Total[Lookup[parts,"T"]],"L"->Total[Lookup[parts,"L"]],
    "DiagramCount"->Total[Lookup[parts,"DiagramCount"]]|>
];

RGZMixTensor[data_Association,i_,j_] := Which[
  RGZMXVectorQ[i] && RGZMXVectorQ[j],FCI[
    RGZLoopPT[p,\[Mu],\[Nu]] data["T"]+
      FVD[p,\[Mu]] FVD[p,\[Nu]] data["L"]/SPD[p,p]],
  RGZMXVectorQ[i],FCI[FVD[p,\[Mu]] data["L"]],
  RGZMXVectorQ[j],FCI[FVD[p,\[Nu]] data["L"]],True,data["L"]];

RGZMixMatrix[component_String:"L",seconds_:60] := Module[{indices,rows,z,n=0,total},
  If[!MemberQ[{"T","L","Tensor"},component],Return[Failure["Component",<||>]]];
  indices=If[component==="T",{1,5,6,7,8},Range[8]];
  total=Length[indices]^2;
  RGZMXProgressUpdate["Sector"->"MixedMatrixSummary", "DiagramID"->0,"DiagramCount"->0,
    "Stage"->component<>"Matrix","TermID"->0,"TotalTermCount"->total,"CompleteTermCount"->0,
    "Status"->"ComputeIn","StageStartTime"->AbsoluteTime[]];
  rows=Table[
    n++;RGZMXProgressUpdate["TermID"->n,"CompleteTermCount"->n-1];
    RGZMixResultStep[If[component==="Tensor","SummaryFullColorTensor","ReadOrComputeAdjointProject"],{i,j}];
    z=If[component==="Tensor",RGZMixReadPair[{i,j}],RGZMixProjectedPair[{i,j},seconds]];
    RGZMixResultStep["Assemble the current matrix entry",{i,j}];
    With[{value=Which[Head[z]===Missing || FailureQ[z],z,
      component==="Tensor",RGZMixTensor[z,i,j],True,z[component]]},
      RGZMXProgressUpdate["CompleteTermCount"->n];value],
    {i,indices},{j,indices}];
  RGZMXProgressUpdate["Status"->"Complete"];rows
];

RGZMixResult[seconds_:60] := Module[{full,t,l,bad},
  RGZMXProgressUpdate["Sector"->"MixedMatrixSummary","DiagramID"->0,"DiagramCount"->0,
    "Stage"->"Aggregate","TermID"->0,"TotalTermCount"->0,"CompleteTermCount"->0,
    "Status"->"ComputeIn","StageStartTime"->AbsoluteTime[]];
  RGZMixResultStep["PrepareFullColorTensorMatrix"];
  full=RGZMixMatrix["Tensor",seconds];
  RGZMXProgressUpdate["Status"->"ComputeIn"];
  RGZMixResultStep["Check completeness of the full color-tensor matrix"];
  bad=Cases[full,_Missing|_Failure,Infinity];
  If[bad=!={},RGZMXProgressUpdate["Status"->"ComputeFailed"];
    Return[Failure["IncompleteMatrix",<|"IncompleteOrFailed"->DeleteDuplicates[bad]|>]]];
  t=RGZMixMatrix["T",seconds];l=RGZMixMatrix["L",seconds];
  RGZMXProgressUpdate["Status"->"ComputeIn"];
  RGZMixResultStep["Check T/L matrixand organize return Result"];
  bad=Cases[{t,l},_Missing|_Failure,Infinity];
  If[bad=!={},RGZMXProgressUpdate["Status"->"ComputeFailed"];
    Return[Failure["ProjectionIncomplete",<|"Cause"->DeleteDuplicates[bad],
    "FullColorTensorMatrixdimensions"->Dimensions[full],
    "FullColorTensorRead"->"RGZMixMatrix[\"Tensor\"]"|>]]];
  If[RGZWorkflow`Private`qqFFInvalidQ[{full,t,l}],RGZMXProgressUpdate["Status"->"ComputeFailed"];Return[Failure["InvalidMatrixValues",<||>]]];
  RGZMXProgressUpdate["Status"->"Complete"];
  <|"TFieldOrder"->RGZMixBasis["T"],"LFieldOrder"->RGZMixBasis["L"],
    "ModelFingerprint"->RGZWorkflow`Private`modelKey[],"ModelContent"->RGZWorkflow`Private`modelContent[],"T"->t,"L"->l,"FullColorTensorMatrix"->full,
    "Description"->"Amputated one-loop insertion retaining the original amplitude factor i. T/L are normalized adjoint color projections; pinch terms, the loop measure and renormalization are excluded."|>
];

(* Contract any supplied mixed insertion with tree propagator matrices.
   d0 and insertion must use the SAME basis, colours and i convention.
   If the action kernel is K+Gamma, give insertion=I Gamma and d0=I/K.
   d0.insertion.d0 then equals -I K^-1 Gamma K^-1. *)
RGZMixAssembleInsertion[d0_List,insertion_List] := Module[{n},
  n=Length[d0];
  If[!MatrixQ[d0] || !MatrixQ[insertion] || Dimensions[d0]=!={n,n} ||
    Dimensions[insertion]=!={n,n} ||
    !FreeQ[{d0,insertion},_Missing|_Failure],Return[Failure["MixedMatrixIncomplete",<||>]]];
  d0.insertion.d0
];
