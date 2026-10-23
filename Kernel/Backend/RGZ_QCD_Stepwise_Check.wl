(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Load the corrected RGZ_OneLoop_AA_PT_Check.wl and
   RGZ_QCD_PartComparison.wl first. This file checks the four candidate
   pinch contributions in the massless QCD limit, one stage at a time.
   Rational propagators are exactly the recombined pole forms.
   It does not compute the actual gse or establish finite-mass RGZ STIs. *)

ClearAll[RGZCandidatePinchesRational, RGZQCDRationalMassless,
  RGZQCDStepStart, RGZQCDStepRun, RGZQCDStepShow, RGZQCDStepSummary];

Options[RGZCandidatePinchesRational]=Options[RGZCandidatePinches];
RGZCandidatePinchesRational[OptionsPattern[]] := Module[{r=RGZBuildPinches[
  "Mode"->OptionValue["Mode"],"ReferenceGauge"->OptionValue["ReferenceGauge"],
  "Representation"->"Rational"]},If[FailureQ[r],r,KeyTake[r,{"box","nav全部","av全部","qse全部"}]]];

RGZQCDRationalMassless[expr_] := Module[{r,indices,tags},
  If[FailureQ[expr] || MemberQ[{$Failed,$Aborted},expr],
    Return[Failure["InvalidInput",<|"Input"->expr|>]]];
  r=ChangeDimension[FCI[expr],D];
  If[!FreeQ[r,_FeynAmpDenominator | _PropagatorDenominator],
    Return[Failure["RationalInputRequired",<|
      "Message"->"\:672c\:51fd\:6570\:53ea\:63a5\:53d7\:5df2\:5408\:5e76\:7684\:6709\:7406\:4f20\:64ad\:5b50\:3002"|>]]];
  indices=DeleteDuplicates[Cases[r,_LorentzIndex,Infinity]];
  tags=Table[Unique["rgzStepLor"],{Length[indices]}];
  r=r /. Thread[indices->tags];
  r=r /. {m->0,mG->0,M->0,\[Mu]->0};
  If[!FreeQ[r,Indeterminate | ComplexInfinity | DirectedInfinity[_]],
    Return[Failure["SingularMasslessLimit",<|"Expression"->r|>]]];
  r /. Thread[tags->indices]
];

If[!AssociationQ[rgzQCDStepResults],rgzQCDStepResults=Association[]];
If[AssociationQ[rgzBoxComparison] &&
    KeyExistsQ[rgzBoxComparison,"\:79ef\:5206\:540e\:5dee\:503c\:7cfb\:6570"],
  AssociateTo[rgzQCDStepResults,"box"->rgzBoxComparison]];

RGZQCDStepStart[name_String] := Module[{sources,paper,eqs},
  If[Length[DownValues[RGZLoopPT]] === 0 ||
      Length[DownValues[RGZQCDRow]] === 0,
    Return[Failure["LoadFilesFirst",<|
      "Message"->"\:5148\:52a0\:8f7d\:4fee\:6b63\:7248\:7684\:4e00\:5708\:68c0\:67e5\:6587\:4ef6\:548c QCD \:6bd4\:8f83\:6587\:4ef6\:3002"|>]]];
  If[Total[Length /@ {OwnValues[m],OwnValues[mG],OwnValues[M],
      OwnValues[\[Mu]],OwnValues[\[Alpha]]}] > 0,
    Return[Failure["SymbolicParametersRequired",<|
      "Message"->"m\:3001mG\:3001M\:3001\:03bc\:3001\:03b1 \:5e94\:4fdd\:7559\:4e3a\:672a\:8d4b\:503c\:7684\:7b26\:53f7\:3002"|>]]];
  eqs=<|"box"->"(2.45)","nav\:5168\:90e8"->"2 \:00d7 (2.47)",
    "av\:5168\:90e8"->"2 \:00d7 (2.47)","qse\:5168\:90e8"->"4 \:00d7 (2.52)"|>;
  If[!KeyExistsQ[eqs,name],Return[Failure["UnknownPart",<|"Name"->name|>]]];
  sources=RGZCandidatePinchesRational[];
  paper=RGZQCDPaperParts[][name];
  rgzQCDStepSession=<|"\:9879\:76ee"->name,"\:6587\:732e\:5f0f\:53f7"->eqs[name],
    "\:539f\:6709\:7406\:5f0f"->sources[name],
    "\:539f\:6587\:732e\:89c4\:8303\:5dee"->Expand[paper-(paper /. \[Alpha]->0)],
    "\:5df2\:5b8c\:6210\:6b65\:9aa4"->0|>;
  Print["\:5df2\:51c6\:5907\:ff1a",name,"\:ff1b\:9010\:4e2a\:8fd0\:884c RGZQCDStepRun[1] \:81f3 [7]\:3002"];
  <|"\:9879\:76ee"->name,"\:72b6\:6001"->"\:51c6\:5907\:5b8c\:6210"|>
];

RGZQCDStepRun[n_Integer,seconds_:60] := Module[
  {keys,labels,t,result,input,row},
  keys={"QCD\:6781\:9650","\:6295\:5f71","\:8ba1\:7b97\:503c","\:6587\:732e\:6709\:7406\:5f0f",
    "\:6587\:732e\:6295\:5f71","\:6587\:732e\:53c2\:8003","\:6bd4\:8f83\:7ed3\:679c"};
  labels={"\:53d6\:65e0\:8d28\:91cf QCD \:6781\:9650","\:622a\:53bb\:5916\:817f\:5e76\:4f5c\:6a2a\:5411\:6295\:5f71",
    "\:8ba1\:7b97\:503c\:8f6c\:6362\:4e3a s\:3001x\:3001y","\:6587\:732e\:53c2\:8003\:8f6c\:6362\:4e3a\:6709\:7406\:5f0f",
    "\:6587\:732e\:53c2\:8003\:6a2a\:5411\:6295\:5f71","\:6587\:732e\:53c2\:8003\:8f6c\:6362\:4e3a s\:3001x\:3001y",
    "\:6362\:5143\:68c0\:67e5\:548c\:4e3b\:79ef\:5206\:7cfb\:6570\:6bd4\:8f83"};
  If[!AssociationQ[rgzQCDStepSession],
    Return[Failure["StartFirst",<|"Message"->"\:5148\:8fd0\:884c RGZQCDStepStart\:3002"|>]]];
  If[n<1 || n>7,Return[Failure["InvalidStage",<|"Stage"->n|>]]];
  If[!NumericQ[seconds] || !TrueQ[seconds>0],
    Return[Failure["InvalidTimeout",<|"Seconds"->seconds|>]]];
  If[n =!= rgzQCDStepSession["\:5df2\:5b8c\:6210\:6b65\:9aa4"]+1,
    Return[Failure["StageOrder",<|
      "\:4e0b\:4e00\:6b65"->(rgzQCDStepSession["\:5df2\:5b8c\:6210\:6b65\:9aa4"]+1)|>]]];
  Print[rgzQCDStepSession["\:9879\:76ee"],"\:ff0c\:6b65\:9aa4 ",n,"\:ff1a",labels[[n]]];
  {t,result}=AbsoluteTiming[TimeConstrained[
    Switch[n,
      1,RGZQCDRationalMassless[rgzQCDStepSession["\:539f\:6709\:7406\:5f0f"]],
      2,RGZQCDProject[SPD[p,p]^2 rgzQCDStepSession["QCD\:6781\:9650"]],
      3,RGZQCDXY[rgzQCDStepSession["\:6295\:5f71"]],
      4,RGZQCDRational[rgzQCDStepSession["\:539f\:6587\:732e\:89c4\:8303\:5dee"]],
      5,RGZQCDProject[rgzQCDStepSession["\:6587\:732e\:6709\:7406\:5f0f"]],
      6,RGZQCDXY[rgzQCDStepSession["\:6587\:732e\:6295\:5f71"]],
      7,RGZQCDRow[rgzQCDStepSession["\:9879\:76ee"],
        rgzQCDStepSession["\:6587\:732e\:5f0f\:53f7"],rgzQCDStepSession["\:8ba1\:7b97\:503c"],
        rgzQCDStepSession["\:6587\:732e\:53c2\:8003"]]],seconds,$Aborted]];
  Print["\:8017\:65f6\:ff1a",NumberForm[t,{10,3}]," \:79d2"];
  If[result === $Aborted,Return[Failure["Timeout",<|
    "\:6b65\:9aa4"->n,"\:8bf4\:660e"->"\:672a\:63a8\:8fdb\:72b6\:6001\:ff1b\:53ef\:589e\:5927\:79d2\:6570\:540e\:91cd\:8bd5\:5f53\:524d\:6b65\:9aa4\:3002"|>]]];
  If[result === $Failed || FailureQ[result],
    Return[Failure["StageFailed",<|"\:6b65\:9aa4"->n,"\:539f\:56e0"->result|>]]];
  AssociateTo[rgzQCDStepSession,
    {keys[[n]]->result,"\:5df2\:5b8c\:6210\:6b65\:9aa4"->n}];
  If[rgzQCDStepSession["\:5df2\:5b8c\:6210\:6b65\:9aa4"] =!= n ||
      !KeyExistsQ[rgzQCDStepSession,keys[[n]]],
    Return[Failure["StateWriteFailed",<|"\:6b65\:9aa4"->n,
      "\:8bf4\:660e"->"\:8ba1\:7b97\:5df2\:8fd4\:56de\:ff0c\:4f46\:7ed3\:679c\:672a\:6210\:529f\:5199\:5165\:6b65\:9aa4\:72b6\:6001\:3002"|>]]];
  If[n === 7,
    AssociateTo[rgzQCDStepResults,rgzQCDStepSession["\:9879\:76ee"]->result];
    RGZQCDStepShow[]];
  <|"\:6b65\:9aa4"->n,"\:72b6\:6001"->"\:5b8c\:6210","\:8017\:65f6\:ff08\:79d2\:ff09"->t,
    "\:8868\:8fbe\:5f0f\:5927\:5c0f"->LeafCount[result]|>
];

RGZQCDStepShow[] := Module[{row,fields},
  If[!AssociationQ[rgzQCDStepSession] ||
      !KeyExistsQ[rgzQCDStepSession,"\:6bd4\:8f83\:7ed3\:679c"],
    Return[Failure["ComparisonNotReady",<||>]]];
  row=rgzQCDStepSession["\:6bd4\:8f83\:7ed3\:679c"];
  fields={"\:8ba1\:7b97\:503c","\:6587\:732e\:53c2\:8003","\:6362\:5143\:540e\:5dee\:503c","\:8ba1\:7b97\:7684I11\:7cfb\:6570",
    "\:6587\:732e\:7684I11\:7cfb\:6570","\:79ef\:5206\:540e\:5dee\:503c\:7cfb\:6570","\:72b6\:6001"};
  Print["\:9879\:76ee\:ff1a",row["\:9879\:76ee"],"\:ff1b\:6587\:732e\:ff1a",row["\:6587\:732e\:5f0f\:53f7"]];
  Print[Grid[Prepend[Map[{#,TraditionalForm[Lookup[row,#,
    Missing["NotComputed"]]]}&,fields],{"\:68c0\:67e5\:9879\:76ee","\:7ed3\:679c"}],
    Frame->All,Alignment->Left,Spacings->{2,1}]]
];

RGZQCDStepSummary[] := Grid[Prepend[
  Map[Function[name,If[KeyExistsQ[rgzQCDStepResults,name],
    With[{r=rgzQCDStepResults[name]},
      {name,r["\:72b6\:6001"],r["\:79ef\:5206\:540e\:5dee\:503c\:7cfb\:6570"]}],
    {name,"\:5c1a\:672a\:68c0\:67e5",Missing["NotComputed"]}]],
    {"box","nav\:5168\:90e8","av\:5168\:90e8","qse\:5168\:90e8"}],
  {"\:9879\:76ee","\:72b6\:6001","\:5dee\:503c / I11"}],Frame->All,Alignment->Left];

Print["\:9010\:6b65\:68c0\:67e5\:51fd\:6570\:5df2\:52a0\:8f7d\:3002\:8fd0\:884c RGZQCDStepStart[\"nav\:5168\:90e8\"]\:ff0c\:518d\:9010\:4e2a\:8fd0\:884c RGZQCDStepRun[1] \:81f3 [7]\:3002"];
