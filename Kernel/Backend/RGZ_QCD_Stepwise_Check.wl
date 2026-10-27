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
  "Representation"->"Rational"]},If[FailureQ[r],r,KeyTake[r,{"box","navAll","avAll","qseAll"}]]];

RGZQCDRationalMassless[expr_] := Module[{r,indices,tags},
  If[FailureQ[expr] || MemberQ[{$Failed,$Aborted},expr],
    Return[Failure["InvalidInput",<|"Input"->expr|>]]];
  r=ChangeDimension[FCI[expr],D];
  If[!FreeQ[r,_FeynAmpDenominator | _PropagatorDenominator],
    Return[Failure["RationalInputRequired",<|
      "Message"->"This function accepts only combined rational propagators."|>]]];
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
    KeyExistsQ[rgzBoxComparison,"IntegrateAfterDifferenceCoefficient"],
  AssociateTo[rgzQCDStepResults,"box"->rgzBoxComparison]];

RGZQCDStepStart[name_String] := Module[{sources,paper,eqs},
  If[Length[DownValues[RGZLoopPT]] === 0 ||
      Length[DownValues[RGZQCDRow]] === 0,
    Return[Failure["LoadFilesFirst",<|
      "Message"->"Load the corrected one-loop check and QCD comparison modules first."|>]]];
  If[Total[Length /@ {OwnValues[m],OwnValues[mG],OwnValues[M],
      OwnValues[\[Mu]],OwnValues[\[Alpha]]}] > 0,
    Return[Failure["SymbolicParametersRequired",<|
      "Message"->"Keep m, mG, M, mu and alpha unassigned."|>]]];
  eqs=<|"box"->"(2.45)","navAll"->"2 × (2.47)",
    "avAll"->"2 × (2.47)","qseAll"->"4 × (2.52)"|>;
  If[!KeyExistsQ[eqs,name],Return[Failure["UnknownPart",<|"Name"->name|>]]];
  sources=RGZCandidatePinchesRational[];
  paper=RGZQCDPaperParts[][name];
  rgzQCDStepSession=<|"Parts"->name,"LiteratureEquation"->eqs[name],
    "OriginalRationalExpression"->sources[name],
    "OriginalLiteratureGaugeDifference"->Expand[paper-(paper /. \[Alpha]->0)],
    "CompletedStep"->0|>;
  Print["Prepare: ",name,"; run RGZQCDStepRun[1] through [7] individually."];
  <|"Parts"->name,"Status"->"PrepareComplete"|>
];

RGZQCDStepRun[n_Integer,seconds_:60] := Module[
  {keys,labels,t,result,input,row},
  keys={"QCDlimit","Project","ComputedValue","LiteratureRationalExpression",
    "LiteratureProject","LiteratureReference","CompareResult"};
  labels={"take massless Mass QCD limit","Amputate external legs and project transversely",
    "ComputedValueConvertIs s, x, y","LiteratureReferenceConvertIsRationalExpression",
    "LiteratureReferenceTransverseProject","LiteratureReferenceConvertIs s, x, y",
    "Check substitutions and compare master-integral coefficients"};
  If[!AssociationQ[rgzQCDStepSession],
    Return[Failure["StartFirst",<|"Message"->"Run RGZQCDStepStart first."|>]]];
  If[n<1 || n>7,Return[Failure["InvalidStage",<|"Stage"->n|>]]];
  If[!NumericQ[seconds] || !TrueQ[seconds>0],
    Return[Failure["InvalidTimeout",<|"Seconds"->seconds|>]]];
  If[n =!= rgzQCDStepSession["CompletedStep"]+1,
    Return[Failure["StageOrder",<|
      "NextStep"->(rgzQCDStepSession["CompletedStep"]+1)|>]]];
  Print[rgzQCDStepSession["Parts"],", Step ",n,"：",labels[[n]]];
  {t,result}=AbsoluteTiming[TimeConstrained[
    Switch[n,
      1,RGZQCDRationalMassless[rgzQCDStepSession["OriginalRationalExpression"]],
      2,RGZQCDProject[SPD[p,p]^2 rgzQCDStepSession["QCDlimit"]],
      3,RGZQCDXY[rgzQCDStepSession["Project"]],
      4,RGZQCDRational[rgzQCDStepSession["OriginalLiteratureGaugeDifference"]],
      5,RGZQCDProject[rgzQCDStepSession["LiteratureRationalExpression"]],
      6,RGZQCDXY[rgzQCDStepSession["LiteratureProject"]],
      7,RGZQCDRow[rgzQCDStepSession["Parts"],
        rgzQCDStepSession["LiteratureEquation"],rgzQCDStepSession["ComputedValue"],
        rgzQCDStepSession["LiteratureReference"]]],seconds,$Aborted]];
  Print["ElapsedSeconds: ",NumberForm[t,{10,3}]," Seconds"];
  If[result === $Aborted,Return[Failure["Timeout",<|
    "Step"->n,"Description"->"No progress; increase the time limit and retry this step."|>]]];
  If[result === $Failed || FailureQ[result],
    Return[Failure["StageFailed",<|"Step"->n,"Cause"->result|>]]];
  AssociateTo[rgzQCDStepSession,
    {keys[[n]]->result,"CompletedStep"->n}];
  If[rgzQCDStepSession["CompletedStep"] =!= n ||
      !KeyExistsQ[rgzQCDStepSession,keys[[n]]],
    Return[Failure["StateWriteFailed",<|"Step"->n,
      "Description"->"Computation returned, but the result could not be stored in the step state."|>]]];
  If[n === 7,
    AssociateTo[rgzQCDStepResults,rgzQCDStepSession["Parts"]->result];
    RGZQCDStepShow[]];
  <|"Step"->n,"Status"->"Complete","ElapsedSeconds (Seconds)"->t,
    "ExpressionSize"->LeafCount[result]|>
];

RGZQCDStepShow[] := Module[{row,fields},
  If[!AssociationQ[rgzQCDStepSession] ||
      !KeyExistsQ[rgzQCDStepSession,"CompareResult"],
    Return[Failure["ComparisonNotReady",<||>]]];
  row=rgzQCDStepSession["CompareResult"];
  fields={"ComputedValue","LiteratureReference","AfterSubstitutionDifference","ComputedI11Coefficient",
    "LiteratureI11Coefficient","IntegrateAfterDifferenceCoefficient","Status"};
  Print["Parts: ",row["Parts"],"; Literature: ",row["LiteratureEquation"]];
  Print[Grid[Prepend[Map[{#,TraditionalForm[Lookup[row,#,
    Missing["NotComputed"]]]}&,fields],{"CheckParts","Result"}],
    Frame->All,Alignment->Left,Spacings->{2,1}]]
];

RGZQCDStepSummary[] := Grid[Prepend[
  Map[Function[name,If[KeyExistsQ[rgzQCDStepResults,name],
    With[{r=rgzQCDStepResults[name]},
      {name,r["Status"],r["IntegrateAfterDifferenceCoefficient"]}],
    {name,"NotYetCheck",Missing["NotComputed"]}]],
    {"box","navAll","avAll","qseAll"}],
  {"Parts","Status","Difference / I11"}],Frame->All,Alignment->Left];

Print["Stepwise checks loaded. Run RGZQCDStepStart[\"navAll\"], then RGZQCDStepRun[1] through [7] individually."];
