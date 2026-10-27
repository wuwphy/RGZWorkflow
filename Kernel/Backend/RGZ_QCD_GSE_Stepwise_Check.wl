(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Actual FeynArts gse versus arXiv:0909.2536 Eq.(2.56).
   Load the corrected main, comparison, and pinch-step files first.
   The model must encode full rational propagators in its two-leg vertices.
   Select only V[1] on both internal lines of the cubic-vertex bubble.
   Compare alpha minus alpha=0 after taking the massless QCD limit.
   This amplitude is ALREADY amputated: do not multiply it by (p^2)^2.
   The user-specified common conversion factor defaults to 1; no sign or
   phase is selected automatically by fitting the paper result. *)

ClearAll[RGZQCDGSEStart, RGZQCDGSERun, RGZQCDGSEShow,
  RGZQCDGSEPaint, RGZQCDGSETotal, RGZQCDGSEMap];

RGZQCDGSEMap[fun_,amps_] := Module[{out,bad},
  If[!ListQ[amps] || amps === {},
    Return[Failure["NoAmplitudes",<|"Input"->amps|>]]];
  out=fun /@ amps;
  bad=Select[out,FailureQ[#] || MemberQ[{$Failed,$Aborted},#]&];
  If[bad =!= {},First[bad],out]
];

Options[RGZQCDGSEStart]={"FeynArtsToPaperFactor"->1};
RGZQCDGSEStart[loops_:Automatic,OptionsPattern[]] := Module[
  {input=loops,sector="BosonicThreePointLoop",graphs,probe},
  If[Length[DownValues[RGZQCDRationalMassless]] === 0 ||
      Length[DownValues[RGZQCDRow]] === 0,
    Return[Failure["LoadHelpers",<|
      "Message"->"Load the one-loop check, QCD comparison and stepwise pinch modules first."|>]]];
  If[Total[Length /@ {OwnValues[m],OwnValues[mG],OwnValues[M],
      OwnValues[\[Mu]],OwnValues[\[Alpha]]}] > 0,
    Return[Failure["SymbolicParametersRequired",<|
      "Message"->"m, mG, M, mu and alpha must be unassigned symbols."|>]]];
  probe=FCI[FVD[q,\[Rho]]];
  If[RGZLoopNormalize[probe] =!= probe,
    Return[Failure["IndexNormalizationBug",<|
      "Message"->"The normalization function changed Lorentz indices; load the corrected main module first."|>]]];
  If[input === Automatic && AssociationQ[rgzOneLoop],input=rgzOneLoop];
  If[input === Automatic,
    If[!StringQ[modelRGZ],Return[Failure["SetModel",<|
      "Message"->"No reusable rgzOneLoop; set modelRGZ to the common gen/mod path."|>]]];
    graphs=Missing["GenerateAtStage1"],
    If[!AssociationQ[input] || !KeyExistsQ[input,"InsertionDiagram"] ||
        !AssociationQ[input["InsertionDiagram"]] ||
        !KeyExistsQ[input["InsertionDiagram"],sector],
      Return[Failure["MissingGraphs",<|"Sector"->sector|>]]];
    graphs=input["InsertionDiagram"][sector]];
  rgzQCDGSESession=<|"CompletedStep"->0,"candidate Diagram"->graphs,
    "OverallFactor"->OptionValue["FeynArtsToPaperFactor"]|>;
  Print["gse Prepare: ",If[Head[graphs] === Missing,
    "Step 1 direct Generaterequired topologies. ","Reuse existing inserted diagrams."]];
  Print["Run RGZQCDGSERun[1] through [9] individually; do not multiply this term by (p^2)^2."];
  <|"Status"->"PrepareComplete","OverallFactor"->rgzQCDGSESession["OverallFactor"]|>
];

RGZQCDGSERun[n_Integer,seconds_:60] := Module[
  {labels,keys,t,result,graphs,pure,fc,bad,r,parts,paper,a,ref,ratio},
  labels={"Select pure-gluon loops with two three-point vertices","Generate amplitudes with CreateFeynAmp",
    "Convert to FeynCalc with FCFAConvert","explicitrationalDenominatorand take  QCD limit",
    "Subtract the Landau-gauge amplitude","Transverse projection by diagram",
    "Normalize loop momenta and sum diagrams","construct Literature gse GaugeDifference",
    "IntegrateBeforeAfterCompareand display Result"};
  keys={"InsertionDiagram","FeynArtsAmplitude","FeynCalcAmplitude","QCDAmplitude",
    "GaugeDifferenceAmplitude","ProjectList","ComputedValue","LiteratureReference","CompareResult"};
  If[!AssociationQ[rgzQCDGSESession],
    Return[Failure["StartFirst",<|"Message"->"Run RGZQCDGSEStart[] first."|>]]];
  If[n<1 || n>9,Return[Failure["InvalidStage",<|"Stage"->n|>]]];
  If[!NumericQ[seconds] || !TrueQ[seconds>0],
    Return[Failure["InvalidTimeout",<|"Seconds"->seconds|>]]];
  If[n =!= rgzQCDGSESession["CompletedStep"]+1,
    Return[Failure["StageOrder",<|
      "NextStep"->(rgzQCDGSESession["CompletedStep"]+1)|>]]];
  Print["gse, Step ",n,"：",labels[[n]]];
  {t,result}=AbsoluteTiming[TimeConstrained[Switch[n,
    1,
      graphs=rgzQCDGSESession["candidate Diagram"];
      If[Head[graphs] === Missing,
        graphs=InsertFields[rgzLoopTopBosonBubble,{V[1]}->{V[1]},
          InsertionLevel->{Particles},Model->modelRGZ,
          GenericModel->modelRGZ]];
      pure=With[{fields=LoopFields[##]},fields =!= {} &&
        And@@(MatchQ[#,V[1,___]|-V[1,___]]& /@ fields)]&;
      DiagramSelect[graphs,pure],
    2,
      CreateFeynAmp[rgzQCDGSESession["InsertionDiagram"],Truncated->True,
        GaugeRules->{},PreFactor->1],
    3,
      fc=FCFAConvert[RGZLoopPrepareFA[rgzQCDGSESession["FeynArtsAmplitude"]],
        IncomingMomenta->{p},OutgoingMomenta->{p},LoopMomenta->{q},
        LorentzIndexNames->{\[Mu],\[Nu]},ChangeDimension->D,
        UndoChiralSplittings->True,List->True,DropSumOver->True,SMP->True];
      If[!ListQ[fc] || fc === {},Failure["NoConvertedAmplitudes",<|"Output"->fc|>],
        bad=RGZLoopUnresolved[fc];
        If[bad =!= {},Failure["UnresolvedCoupling",<|"Objects"->bad|>],fc]],
    4,
      RGZQCDGSEMap[Function[amp,
        r=RGZLoopNormalize[ChangeDimension[FCI[amp],D]];
        bad=Cases[r,x_ /; MemberQ[
          {"mG1","mG2","mG3","mA1","mA2","mB",
           "m1","m2","m3","m4","rgzLoopM1","rgzLoopM2",
           "rgzLoopM3","rgzLoopM4"},RGZLoopName[x]],Infinity];
        If[bad =!= {},Failure["RationalModelRequired",<|
          "Objects"->DeleteDuplicates[bad],
          "Message"->"Use the checked rational-propagator model; legacy placeholder-denominator rules are not applied."|>],
          RGZQCDRationalMassless[FeynAmpDenominatorExplicit[r]]]],
        rgzQCDGSESession["FeynCalcAmplitude"]],
    5,
      RGZQCDGSEMap[Function[amp,
        RGZLoopReduce[Expand[amp-(amp /. \[Alpha]->0)]]],
        rgzQCDGSESession["QCDAmplitude"]],
    6,
      RGZQCDGSEMap[RGZQCDProject,rgzQCDGSESession["GaugeDifferenceAmplitude"]],
    7,
      parts=RGZQCDGSEMap[RGZQCDXY,rgzQCDGSESession["ProjectList"]];
      If[FailureQ[parts],parts,
        Cancel[Together[rgzQCDGSESession["OverallFactor"] Total[parts]]]],
    8,
      paper=RGZQCDPaperParts[]["gse"];
      a=RGZQCDRational[Expand[paper-(paper /. \[Alpha]->0)]];
      If[FailureQ[a],a,a=RGZQCDProject[a];
        If[FailureQ[a],a,RGZQCDXY[a]]],
    9,
      RGZQCDRow["gse","(2.56)",rgzQCDGSESession["ComputedValue"],
        rgzQCDGSESession["LiteratureReference"]]],seconds,$Aborted]];
  Print["ElapsedSeconds: ",NumberForm[t,{10,3}]," Seconds"];
  If[result === $Aborted,Return[Failure["Timeout",<|
    "Step"->n,"Description"->"No progress; increase the time limit and retry this step."|>]]];
  If[result === $Failed || FailureQ[result],
    Return[Failure["StageFailed",<|"Step"->n,"Cause"->result|>]]];
  AssociateTo[rgzQCDGSESession,{keys[[n]]->result,"CompletedStep"->n}];
  If[rgzQCDGSESession["CompletedStep"] =!= n ||
      !KeyExistsQ[rgzQCDGSESession,keys[[n]]],
    Return[Failure["StateWriteFailed",<|"Step"->n|>]]];
  If[MemberQ[{3,4,5,6},n],Print["AmplitudeTermCount: ",Length[result]]];
  If[n === 9,
    a=result["ComputedI11Coefficient"];ref=result["LiteratureI11Coefficient"];
    ratio=If[FailureQ[a] || FailureQ[ref] || TrueQ[ref === 0],
      Missing["RatioUnavailable"],Cancel[Together[a/ref]]];
    AssociateTo[rgzQCDGSESession,"CoefficientRatioNotUsedForAdjustment"->ratio];
    If[!AssociationQ[rgzQCDStepResults],rgzQCDStepResults=Association[]];
    AssociateTo[rgzQCDStepResults,"gse"->result];
    RGZQCDGSEShow[]];
  <|"Step"->n,"Status"->"Complete","ElapsedSeconds (Seconds)"->t,
    "ExpressionSize"->LeafCount[result]|>
];

RGZQCDGSEPaint[] := If[AssociationQ[rgzQCDGSESession] &&
    KeyExistsQ[rgzQCDGSESession,"InsertionDiagram"],
  Paint[rgzQCDGSESession["InsertionDiagram"],Numbering->Simple,SheetHeader->None],
  Failure["SelectGraphsFirst",<|"Message"->"Complete step 1 first."|>]];

RGZQCDGSEShow[] := Module[{row,fields},
  If[!AssociationQ[rgzQCDGSESession] ||
      !KeyExistsQ[rgzQCDGSESession,"CompareResult"],
    Return[Failure["ComparisonNotReady",<||>]]];
  row=rgzQCDGSESession["CompareResult"];
  fields={"ComputedValue","LiteratureReference","AfterSubstitutionDifference","ComputedI11Coefficient",
    "LiteratureI11Coefficient","IntegrateAfterDifferenceCoefficient","Status"};
  Print[Grid[Prepend[Map[{#,TraditionalForm[Lookup[row,#,
    Missing["NotComputed"]]]}&,fields],{"gse CheckParts","Result"}],
    Frame->All,Alignment->Left,Spacings->{2,1}]];
  Print["CommonFactor: ",rgzQCDGSESession["OverallFactor"]];
  Print["Compute / Literature OfIntegrateCoefficientratio (Notused for adjustment): ",
    rgzQCDGSESession["CoefficientRatioNotUsedForAdjustment"]]
];

RGZQCDGSETotal[] := Module[{names,missing,rows,bad,coefficient},
  names={"box","navAll","avAll","qseAll","gse"};
  If[!AssociationQ[rgzQCDStepResults],
    Return[Failure["NoResults",<||>]]];
  missing=Select[names,!KeyExistsQ[rgzQCDStepResults,#]&];
  If[missing =!= {},Return[Failure["IncompleteParts",<|"MissingParts"->missing|>]]];
  rows=Lookup[rgzQCDStepResults,names];
  bad=Select[rows,!MemberQ[{"PassedBeforeAndAfterIntegration","Passed after integration; integrand forms differ"},
    Lookup[#,"Status",Missing["NotComputed"]]]&];
  If[bad =!= {},Return[Failure["ComparisonNotPassed",<|"NotPassedParts"->
    Lookup[bad,"Parts"]|>]]];
  coefficient=RGZQCDMasterCoefficient[Total[Lookup[rows,"ComputedValue"]]];
  If[FailureQ[coefficient],Return[coefficient]];
  Print[Grid[Prepend[Map[{#["Parts"],#["Status"],#["IntegrateAfterDifferenceCoefficient"]}&,
    rows],{"Parts","Status","Difference / I11"}],Frame->All,Alignment->Left]];
  <|"ComputedFiveSectorGaugeDifference / I11"->coefficient,
    "CancellationCheck"->If[TrueQ[coefficient === 0],"Passed","Sumstill Nonzero"]|>
];

Print["Actual gse checks loaded. Run RGZQCDGSEStart[], then RGZQCDGSERun[1] through [9] individually."];
