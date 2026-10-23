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
      "\:9879\:76ee"->name,"\:6b65\:9aa4"->step,"\:539f\:56e0"->value|>],stop],value];

  Catch[
    directory=RGZWorkflow`Private`backendDirectory[];
    If[!StringQ[directory],Throw[Failure["SaveNotebookFirst",<|
      "\:8bf4\:660e"->"\:5148\:4fdd\:5b58\:7b14\:8bb0\:672c\:ff0c\:5e76\:628a\:56db\:4e2a\:8f85\:52a9\:6587\:4ef6\:653e\:5728\:7b14\:8bb0\:672c\:6240\:5728\:76ee\:5f55\:3002"|>],stop]];
    files=FileNameJoin[{directory,#}]& /@ {
      "RGZ_OneLoop_AA_PT_Check.wl",
      "RGZ_QCD_PartComparison.wl",
      "RGZ_QCD_Stepwise_Check.wl",
      "RGZ_QCD_GSE_Stepwise_Check.wl"};
    missing=Select[files,!FileExistsQ[#]&];
    If[missing =!= {},Throw[Failure["MissingFiles",<|"\:7f3a\:5c11\:6587\:4ef6"->missing|>],stop]];
    Block[{$Output={}},Scan[Get,files]];

    (* Recompute all parts; do not depend on previously saved comparisons. *)
    rgzQCDStepResults=<||>;
    Do[
      Print["\:68c0\:67e5\:ff1a",part];
      r=Block[{$Output={}},RGZQCDStepStart[part]];
      require[r,part,0];
      Do[
        r=Block[{$Output={}},RGZQCDStepRun[i,60]];
        require[r,part,i],
        {i,1,7}],
      {part,{"box","nav\:5168\:90e8","av\:5168\:90e8","qse\:5168\:90e8"}}];

    (* gse is already amputated; its helper does not multiply by (p^2)^2. *)
    Print["\:68c0\:67e5\:ff1agse"];
    r=Block[{$Output={}},RGZQCDGSEStart[]];
    require[r,"gse",0];
    Do[
      r=Block[{$Output={}},RGZQCDGSERun[i,60]];
      require[r,"gse",i],
      {i,1,9}];

    rows=Lookup[rgzQCDStepResults,
      {"box","nav\:5168\:90e8","av\:5168\:90e8","qse\:5168\:90e8","gse"}];
    summary=Grid[Prepend[
      Map[{#["\:9879\:76ee"],#["\:72b6\:6001"],#["\:79ef\:5206\:540e\:5dee\:503c\:7cfb\:6570"]}&,rows],
      {"\:9879\:76ee","\:72b6\:6001","\:5dee\:503c / I11"}],
      Frame->All,Alignment->Left,Spacings->{2,1}];
    total=Block[{$Output={}},RGZQCDGSETotal[]];
    Print[summary];
    Print["\:603b\:548c\:68c0\:67e5\:ff1a",total];
    <|"\:9010\:9879\:7ed3\:679c"->rows,"\:6458\:8981"->summary,"\:603b\:548c\:68c0\:67e5"->total|>,
    stop]
];

If[FailureQ[rgzQCDReport],Print[rgzQCDReport]];

(* Optional details, run separately:
   RGZShowQCDPart[rgzQCDReport,"box"];
   RGZShowQCDPart[rgzQCDReport,"gse"];
   RGZQCDGSEPaint[];
*)
