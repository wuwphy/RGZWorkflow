(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* QCD benchmark: four candidate pinch contributions plus actual pure-gluon gse.
   Prerequisites: FeynCalc/FeynArts loaded; modelRGZ set, or valid rgzOneLoop.
   Keep m, mG, M, mu, alpha and D symbolic.
   Put this file and the four helper files in the notebook directory.
   This checks the transverse QCD gauge difference Pi(alpha)-Pi(0).
   It does not calculate the complete finite-mass RGZ self-energy. *)

rgzQCDReport = Module[
  {directory,files,missing,part,i,r,rows,summary,total,stop,require},

  require[value_,name_,step_] := If[
    FailureQ[value] || MemberQ[{$Failed,$Aborted},value],
    Throw[Failure["QCDCheckStopped",<|
      "Parts"->name,"Step"->step,"Cause"->value|>],stop],value];

  Catch[
    directory=RGZWorkflow`Private`backendDirectory[];
    If[!StringQ[directory],Throw[Failure["SaveNotebookFirst",<|
      "Description"->"Save the notebook and place the four helper files in its directory first."|>],stop]];
    files=FileNameJoin[{directory,#}]& /@ {
      "RGZ_OneLoop_AA_PT_Check.wl",
      "RGZ_QCD_PartComparison.wl",
      "RGZ_QCD_Stepwise_Check.wl",
      "RGZ_QCD_GSE_Stepwise_Check.wl"};
    missing=Select[files,!FileExistsQ[#]&];
    If[missing =!= {},Throw[Failure["MissingFiles",<|"MissingFile"->missing|>],stop]];
    Block[{$Output={}},Scan[Get,files]];

    (* Recompute all parts; do not depend on previously saved comparisons. *)
    rgzQCDStepResults=<||>;
    Do[
      Print["Check: ",part];
      r=Block[{$Output={}},RGZQCDStepStart[part]];
      require[r,part,0];
      Do[
        r=Block[{$Output={}},RGZQCDStepRun[i,60]];
        require[r,part,i],
        {i,1,7}],
      {part,{"box","navAll","avAll","qseAll"}}];

    (* gse is already amputated; its helper does not multiply by (p^2)^2. *)
    Print["Check: gse"];
    r=Block[{$Output={}},RGZQCDGSEStart[]];
    require[r,"gse",0];
    Do[
      r=Block[{$Output={}},RGZQCDGSERun[i,60]];
      require[r,"gse",i],
      {i,1,9}];

    rows=Lookup[rgzQCDStepResults,
      {"box","navAll","avAll","qseAll","gse"}];
    summary=Grid[Prepend[
      Map[{#["Parts"],#["Status"],#["IntegrateAfterDifferenceCoefficient"]}&,rows],
      {"Parts","Status","Difference / I11"}],
      Frame->All,Alignment->Left,Spacings->{2,1}];
    total=Block[{$Output={}},RGZQCDGSETotal[]];
    Print[summary];
    Print["SumCheck: ",total];
    <|"PerTermResults"->rows,"Summary"->summary,"SumCheck"->total|>,
    stop]
];

If[FailureQ[rgzQCDReport],Print[rgzQCDReport]];

(* Optional details, run separately:
   RGZShowQCDPart[rgzQCDReport,"box"];
   RGZShowQCDPart[rgzQCDReport,"gse"];
   RGZQCDGSEPaint[];
*)
