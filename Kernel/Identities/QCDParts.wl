(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Role:
   Specify and check the QCD reference contributions in a common integral convention.

   Inputs:
   Tree identities or computed amplitudes and the source-linked reports being checked.

   Dependencies:
   Tree/, OneLoop/, Algebra/ and Renormalization/ for the result being checked.
*)


(* Common massless master and reference coefficients *)
(* Load RGZ_OneLoop_AA_PT_Check.wl BEFORE this file.
   Comparison with 0909.2536, Eqs.(2.45),(2.47),(2.52),(2.56).
   Compare only the transverse, propagator-like gauge-dependent parts.
   The reference is converted from alpha=1 to alpha=0.
   Raw pinch kernels are amputated by s^2 in their massless QCD limit.
   Keep D symbolic. Every loop result is a coefficient of the SAME
   massless bubble I11(s)=Integral[1/(q^2 (q-p)^2)].
   Actual FeynArts rows are computed only from diagrams supplied by user;
   reference gse is NEVER substituted for a missing calculated gse.
   No statement about finite-mass RGZ gauge independence is made here. *)

ClearAll[RGZQCDPaperParts, RGZQCDRational, RGZQCDMassless,
  RGZQCDProject, RGZQCDXY, RGZQCDPoch, RGZQCDBubbleRatio,
  RGZQCDMasterCoefficient, RGZQCDRow, RGZQCDCalculatedLoops,
  RGZCompareWithQCD, RGZShowQCDPart,
  rgzQCDS, rgzQCDX, rgzQCDY];

RGZQCDPaperParts[] := Module[
  {l=1-\[Alpha],g2=SMP["g_s"]^2,s0=SPD[p,p],pt,h,jbox,jnav,k,t},
  pt=RGZLoopPT[p,\[Mu],\[Nu]];
  h=RGZLoopPT[p,\[Mu],\[Rho]] RGZLoopPT[p,\[Nu],\[Sigma]]
    FVD[q,\[Rho]] FVD[q,\[Sigma]] FAD[q,q,p-q,p-q];
  jbox=pt FAD[q,q,p-q];
  jnav=pt FAD[q,p-q,p-q];
  k=RGZLoopPT[p,\[Nu],\[Rho]] FVD[q,\[Mu]] FVD[q,\[Rho]]
    FAD[q,q,p-q];
  t=pt FAD[q,q];
  (* Keep Jbox and Jnav separate as printed in the paper.
     Their integrals agree by a loop-momentum change of variables. *)
  Map[ChangeDimension[FCI[#],D]&,
    <|"box"->g2 CA (l^2 s0^2 h/2-l s0^2 jbox),
      "navAll"->2g2 CA (-l^2 s0^2 h/2+
        l (s0^2 jnav+s0 k-s0 t)),
      "avAll"->2l g2 (CA/2-CF)s0 t,
      "qseAll"->4l g2 CF s0 t/2,
      "gse"->g2 CA (l^2 s0^2 h/2-
        l (s0^2 jnav+2s0 k-s0 t))|>]
];

RGZQCDRational[expr_] := Module[{r},
  If[FailureQ[expr],Return[expr]];
  If[MemberQ[{$Failed,$Aborted},expr],
    Return[Failure["InvalidInput",<|"Input"->expr|>]]];
  r=ChangeDimension[FCI[RGZLoopNormalize[expr]],D];
  r=FeynAmpDenominatorExplicit[r];
  r=RGZLoopPoleMasses[r];
  (* Recombine generic poles BEFORE taking a degenerate massless limit. *)
  FullSimplify[Cancel[Together[r]]]
];

RGZQCDMassless[expr_] := Module[{r,indices,tags,protected},
  r=RGZQCDRational[expr];
  If[FailureQ[r] || r === $Failed,Return[r]];
  (* mu is BOTH the mass symbol and an external Lorentz-index label.
     Protect every LorentzIndex object before setting the mass to zero. *)
  indices=DeleteDuplicates[Cases[r,_LorentzIndex,Infinity]];
  tags=Table[Unique["rgzProtectedLor"],{Length[indices]}];
  protected=r /. Thread[indices->tags];
  protected=FullSimplify[Cancel[Together[
    protected /. {m->0,mG->0,M->0,\[Mu]->0}]]];
  If[!FreeQ[protected,Indeterminate|ComplexInfinity|DirectedInfinity[_]],
    Return[Failure["SingularMasslessLimit",<|"Expression"->protected|>]]];
  protected /. Thread[tags->indices]
];

RGZQCDProject[expr_] := Module[{r,deltas,c},
  If[FailureQ[expr] || expr === $Failed,Return[expr]];
  r=Contract[FCI[RGZLoopPT[p,\[Mu],\[Nu]]] FCI[expr]]/(D-1);
  r=SUNSimplify[r,SUNNToCACF->True] /. SUNN->CA;
  r=FullSimplify[ExpandScalarProduct[r]];
  deltas=DeleteDuplicates[Cases[r,_SUNDelta,Infinity]];
  If[Length[deltas] > 1,
    Return[Failure["UnresolvedColor",<|"Expression"->r|>]]];
  If[Length[deltas] == 1,
    c=Coefficient[Expand[r],First[deltas],1];
    If[!TrueQ[FullSimplify[r-c First[deltas]] === 0],
      Return[Failure["NonlinearColor",<|"Expression"->r|>]]];
    r=c];
  If[!FreeQ[r,_LorentzIndex | _ExplicitLorentzIndex |
      _TemporalPair | _TemporalMomentum | _CartesianPair | _CartesianMomentum],
    Return[Failure["NonCovariantScalar",<|"Expression"->r,
      "Message"->"The scalar projection contains Lorentz indices or fixed spacetime components; check whether parameter substitution affected indices."|>]]];
  FullSimplify[r]
];

(* x=q^2, y=(q-p)^2, s=p^2. Normalize q+p routing independently for
   each calculated diagram by q -> -q BEFORE identifying y. *)
RGZQCDXY[expr_] := Module[{r,x,z,s0,den,factors,plus,minus,c},
  If[FailureQ[expr],Return[expr]];
  If[MemberQ[{$Failed,$Aborted},expr],
    Return[Failure["InvalidScalar",<|"Input"->expr|>]]];
  r=ExpandScalarProduct[ChangeDimension[FCI[expr],D]];
  r=r /. {
    HoldPattern[Pair[Momentum[q,D],Momentum[q,D]]] -> x,
    HoldPattern[Pair[Momentum[p,D],Momentum[q,D]]] -> z,
    HoldPattern[Pair[Momentum[q,D],Momentum[p,D]]] -> z,
    HoldPattern[Pair[Momentum[p,D],Momentum[p,D]]] -> s0};
  If[!FreeQ[r,_Pair | _LorentzIndex | _ExplicitLorentzIndex |
      _TemporalPair | _TemporalMomentum | _CartesianPair | _CartesianMomentum],
    Return[Failure["ResidualTensor",<|"Expression"->r,
      "Message"->"The s, x, y conversion still contains unresolved tensors or time components."|>]]];
  r=Cancel[Together[r]]; den=Denominator[r];
  If[!PolynomialQ[den,{x,z}],
    Return[Failure["UnknownScalarProduct",<|"Expression"->r|>]]];
  factors=First /@ Rest[FactorList[den]];
  plus=AnyTrue[factors,Function[f,c=Coefficient[f,x];
    !TrueQ[c === 0] && TrueQ[FullSimplify[f-c(x+2z+s0)] === 0]]];
  minus=AnyTrue[factors,Function[f,c=Coefficient[f,x];
    !TrueQ[c === 0] && TrueQ[FullSimplify[f-c(x-2z+s0)] === 0]]];
  If[plus && minus,
    Return[Failure["MixedRouting",<|"Expression"->r|>]]];
  If[plus,r=r /. z->-z];
  FullSimplify[Cancel[Together[r /. {
    z->(rgzQCDS+rgzQCDX-rgzQCDY)/2,
    x->rgzQCDX,s0->rgzQCDS}]]]
];

RGZQCDPoch[z_,n_Integer] /; n>=0 := Product[z+j,{j,0,n-1}];

(* Exact D-dimensional massless two-propagator ratios.
   I21/I11=-(D-3)/s; I22/I11=(D-6)(D-3)/s^2.
   Nonpositive powers leave one massless denominator and give scaleless
   polynomial/tadpole integrals, zero in dimensional regularization. *)
RGZQCDBubbleRatio[a_Integer,b_Integer] := If[a<=0 || b<=0,0,
  FullSimplify[
    rgzQCDS^(2-a-b) RGZQCDPoch[2-D/2,a+b-2]
      RGZQCDPoch[D-a-b,a+b-2]/
    (Factorial[a-1] Factorial[b-1]
      RGZQCDPoch[D/2-a,a-1] RGZQCDPoch[D/2-b,b-1])]];

RGZQCDMasterCoefficient[expr_] := Module[{r,num,den,dx,dy,c,rules},
  If[FailureQ[expr] || Head[expr] === Missing,Return[expr]];
  If[MemberQ[{$Failed,$Aborted},expr],
    Return[Failure["InvalidIntegrand",<|"Input"->expr|>]]];
  r=Cancel[Together[expr]]; If[TrueQ[r === 0],Return[0]];
  If[!FreeQ[r,_Pair | _LorentzIndex | _ExplicitLorentzIndex |
      _TemporalPair | _TemporalMomentum | _CartesianPair | _CartesianMomentum],
    Return[Failure["ResidualTensor",<|"Expression"->r|>]]];
  num=Numerator[r]; den=Denominator[r];
  If[!PolynomialQ[num,{rgzQCDX,rgzQCDY}] ||
      !PolynomialQ[den,{rgzQCDX,rgzQCDY}],
    Return[Failure["NonRationalIntegrand",<|"Expression"->r|>]]];
  dx=Exponent[den,rgzQCDX]; dy=Exponent[den,rgzQCDY];
  c=Cancel[den/(rgzQCDX^dx rgzQCDY^dy)];
  If[!FreeQ[c,rgzQCDX|rgzQCDY],
    Return[Failure["NotMasslessBubble",<|"Denominator"->den|>]]];
  rules=CoefficientRules[num,{rgzQCDX,rgzQCDY}];
  FullSimplify[Total[Map[Function[rule,
    Last[rule]/c RGZQCDBubbleRatio[
      dx-First[rule][[1]],dy-First[rule][[2]]]],rules]]]
];

RGZQCDRow[name_,eq_,actual_,reference_] := Module[
  {diff,shiftDiff,actualMaster,refMaster,masterDiff,state},
  If[Head[actual] === Missing || FailureQ[actual],
    Return[<|"Parts"->name,"LiteratureEquation"->eq,"ComputedValue"->actual,
      "LiteratureReference"->reference,"AfterSubstitutionDifference"->actual,
      "IntegrateAfterDifferenceCoefficient"->actual,"Status"->
        If[Head[actual] === Missing,"NotRunActualloopDiagram","ComputeFailed"]|>]];
  diff=Cancel[Together[actual-reference]];
  shiftDiff=FullSimplify[Cancel[Together[
    (diff+(diff /. {rgzQCDX->rgzQCDY,rgzQCDY->rgzQCDX}))/2]]];
  actualMaster=RGZQCDMasterCoefficient[actual];
  refMaster=RGZQCDMasterCoefficient[reference];
  masterDiff=If[FailureQ[actualMaster] || FailureQ[refMaster],
    Failure["MasterReduction",<|"Actual"->actualMaster,"Reference"->refMaster|>],
    FullSimplify[actualMaster-refMaster]];
  state=Which[FailureQ[masterDiff],"IntegrateReduceFailed",
    TrueQ[shiftDiff === 0] && TrueQ[masterDiff === 0],"PassedBeforeAndAfterIntegration",
    TrueQ[masterDiff === 0],"Passed after integration; integrand forms differ",
    True,"DifferenceNonzero"];
  <|"Parts"->name,"LiteratureEquation"->eq,"ComputedValue"->actual,
    "LiteratureReference"->reference,"AfterSubstitutionDifference"->shiftDiff,
    "ComputedI11Coefficient"->actualMaster,"LiteratureI11Coefficient"->refMaster,
    "IntegrateAfterDifferenceCoefficient"->masterDiff,"Status"->state|>
];

RGZQCDCalculatedLoops[loops_Association] := Module[
  {pureV,pureU,convert,selected,amps,parts,bad,out},
  pureV=With[{fields=LoopFields[##]},
    fields =!= {} && And@@(MatchQ[#,V[1,___]|-V[1,___]]& /@ fields)]&;
  pureU=With[{fields=LoopFields[##]},
    fields =!= {} && And@@(MatchQ[#,U[1,___]|-U[1,___]]& /@ fields)]&;
  convert[key_,filter_] := Module[{graphs},
    selected=DiagramSelect[loops["InsertionDiagram"][key],filter];
    amps=RGZLoopConvert[selected];
    If[FailureQ[amps],Return[amps]];
    parts=Map[Function[a,Module[{sc},
      sc=RGZQCDMassless[a]; If[FailureQ[sc],Return[sc]];
      sc=RGZQCDProject[sc]; If[FailureQ[sc],Return[sc]];
      RGZQCDXY[sc]]],amps];
    bad=Select[parts,FailureQ];
    If[bad =!= {},Return[First[bad]]];
    FullSimplify[Total[parts]]];
  out=<|"gse"->convert["BosonicThreePointLoop",pureV],
    "FourGluonloopDiagram"->convert["FourPointVertexLoop",pureV],
    "FPGhostLoopDiagram"->convert["GhostThreePointLoop",pureU]|>;
  out
];

Options[RGZCompareWithQCD]={"FeynArtsToPaperFactor"->1};
RGZCompareWithQCD[pinches_:Automatic,loops_:Automatic,OptionsPattern[]] := Module[
  {source,paper,paperDelta,refXY,calcXY=Association[],weights,keys,eqs,
    a,sc,rows,loopParts,factor,totalPaper,totalCalc,summary},
  If[Length[DownValues[RGZCandidatePinches]] === 0,
    Return[Failure["LoadMainFile",<|"Message"->"Load RGZ_OneLoop_AA_PT_Check.wl first."|>]]];
  If[Total[Length /@ {OwnValues[m],OwnValues[mG],OwnValues[M],
      OwnValues[\[Mu]],OwnValues[\[Alpha]]}] > 0,
    Return[Failure["SymbolicParametersRequired",<|
      "Message"->"Keep m, mG, M, mu and alpha unassigned before comparison; do not set masses or alpha to zero first."|>]]];
  source=If[pinches === Automatic,RGZCandidatePinches[],pinches];
  keys={"box","navAll","avAll","qseAll"};
  If[!AssociationQ[source] || !And@@(KeyExistsQ[source,#]& /@ keys),
    Return[Failure["MissingPinchParts",<|"RequiredKeys"->keys|>]]];
  If[loops =!= Automatic &&
      (!AssociationQ[loops] || !KeyExistsQ[loops,"InsertionDiagram"]),
    Return[Failure["MissingLoopGraphs",<|
      "Message"->"Loop comparison requires rgzOneLoop from a successful RGZRunAA[] call; omit the second argument to compare only the four handwritten sectors."|>]]];
  paper=RGZQCDPaperParts[];
  paperDelta=Map[FullSimplify[#-(# /. \[Alpha]->0)]&,paper];
  refXY=Map[RGZQCDXY[RGZQCDProject[RGZQCDRational[#]]]&,paperDelta];
  If[AnyTrue[Values[refXY],FailureQ],
    Return[Failure["ReferenceConstruction",<|"Parts"->refXY|>]]];
  Do[
    Print["Comparing the QCD limit: ",key];
    a=RGZQCDMassless[source[key]];
    If[FailureQ[a],AssociateTo[calcXY,key->a],
      (* d(s)^2=1/s^2: amputate the common external propagators. *)
      sc=RGZQCDProject[SPD[p,p]^2 a];
      AssociateTo[calcXY,key->If[FailureQ[sc],sc,RGZQCDXY[sc]]]],
    {key,keys}];
  eqs=<|"box"->"(2.45)","navAll"->"2 × (2.47)",
    "avAll"->"2 × (2.47)","qseAll"->"4 × (2.52)",
    "gse"->"(2.56)"|>;
  rows=Map[RGZQCDRow[#,eqs[#],calcXY[#],refXY[#]]&,keys];
  If[AssociationQ[loops] && KeyExistsQ[loops,"InsertionDiagram"],
    Print["Comparing generated QCD gluon, four-gluon and FP-ghost loop diagrams."];
    loopParts=RGZQCDCalculatedLoops[loops];
    factor=OptionValue["FeynArtsToPaperFactor"];
    Do[
      a=loopParts[key];
      If[!FailureQ[a],a=factor(a-(a /. \[Alpha]->0))];
      AssociateTo[calcXY,key->a],{key,{"gse","FPGhostLoopDiagram"}}];
    AssociateTo[calcXY,"FourGluonloopDiagram"->If[FailureQ[loopParts["FourGluonloopDiagram"]],
      loopParts["FourGluonloopDiagram"],factor loopParts["FourGluonloopDiagram"]]],
    AssociateTo[calcXY,{"gse"->Missing["ActualGraphsNotProvided"],
      "FourGluonloopDiagram"->Missing["ActualGraphsNotProvided"],
      "FPGhostLoopDiagram"->Missing["ActualGraphsNotProvided"]}]];
  AppendTo[rows,RGZQCDRow["gse",eqs["gse"],calcXY["gse"],refXY["gse"]]];
  AppendTo[rows,RGZQCDRow["FourGluonloopDiagram","without MasstadpoleIntegrate=0",
    calcXY["FourGluonloopDiagram"],0]];
  AppendTo[rows,RGZQCDRow["FPGhostLoopGaugeDifference","α Independent",
    calcXY["FPGhostLoopDiagram"],0]];
  totalPaper=RGZQCDMasterCoefficient[Total[Values[refXY]]];
  totalCalc=If[AnyTrue[Lookup[calcXY,Append[keys,"gse"]],
    FailureQ[#] || Head[#] === Missing&],Missing["ActualTotalNotAvailable"],
    RGZQCDMasterCoefficient[Total[Lookup[calcXY,Append[keys,"gse"]]]]];
  summary=Grid[Prepend[Map[
    {#["Parts"],#["LiteratureEquation"],#["Status"],#["IntegrateAfterDifferenceCoefficient"]}&,rows],
    {"CompareParts","LiteratureSource","Status","Difference / I11"}],
    Frame->All,Alignment->Left,Spacings->{2,1}];
  Print[summary];
  Print["Literaturefive sectorsGaugeDifferenceSum / I11: ",totalPaper];
  Print["ActualComputefive sectorsGaugeDifferenceSum / I11: ",totalCalc];
  <|"PerTermResults"->rows,"LiteratureOriginalformula (α=1Benchmark)"->paper,
    "LiteratureGaugeDifference (α=0Benchmark)"->paperDelta,
    "LiteratureTotalDifferenceCoefficient"->totalPaper,"ComputeTotalDifferenceCoefficient"->totalCalc,
    "Summary"->summary,
    "Description"->"s=p^2, x=q^2, y=(q-p)^2; multiply integrated results by the common I11(s). This compares transverse gauge-dependent terms in the QCD limit."|>
];

RGZShowQCDPart[report_Association,name_String] := Module[{row,fields},
  row=SelectFirst[report["PerTermResults"],#["Parts"] === name&,Missing["NoSuchPart"]];
  If[Head[row] === Missing,Return[row]];
  fields={"ComputedValue","LiteratureReference","AfterSubstitutionDifference","ComputedI11Coefficient",
    "LiteratureI11Coefficient","IntegrateAfterDifferenceCoefficient","Status"};
  Print["Parts: ",name,"; Literature: ",row["LiteratureEquation"]];
  Print[Grid[Map[{#,Pane[TraditionalForm[Lookup[row,#,Missing["NotComputed"]]],
    ImageSize->{850,Automatic},Scrollbars->{True,False}]}&,fields],
    Frame->All,Alignment->Left]]
];

Print["QCD term comparison loaded. Run RGZCompareWithQCD[rgzPinch,rgzOneLoop]."];
Print["RGZCompareWithQCD[] compares the four handwritten sectors only; the actual gse is marked NotRun."];

(* The four candidate pinch contributions *)
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

(* The calculated gluon self energy *)
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

(* ================================================================
   Exact QCD check: review 0909.2536, Eqs.(2.45),(2.47),(2.52),(2.56).
   lambda = 1-alpha; every term below is AMPUTATED and uses the SAME
   common integration convention. The loop routing is q and q+p.
   This is an independent benchmark, not an RGZ derivation.
   ================================================================ *)
RGZQCDPinchBenchmark[] := Module[
  {lambda=1-\[Alpha],g2=SMP["g_s"]^2,s0=SPD[p,p],pr,h,j,k,t,
    basis,cb,cn,ca,cq,cg,coeffSum,oldRGZResidual,
    box,nav,av,qse,gse,sum},
  pr=RGZLoopPT[p,\[Mu],\[Nu]];
  h=ChangeDimension[FCI[RGZLoopPT[p,\[Mu],\[Rho]] RGZLoopPT[p,\[Nu],\[Sigma]]
    FVD[q,\[Rho]] FVD[q,\[Sigma]] FAD[q,q,q+p,q+p]],D];
  j=ChangeDimension[FCI[pr FAD[q,q,q+p]],D];
  k=ChangeDimension[FCI[RGZLoopPT[p,\[Nu],\[Rho]] FVD[q,\[Mu]] FVD[q,\[Rho]]
    FAD[q,q,q+p]],D];
  t=ChangeDimension[FCI[pr FAD[q,q]],D];
  basis={h,j,k,t};
  cb=g2 CA {lambda^2 s0^2/2,-lambda s0^2,0,0};
  cn=g2 CA {-lambda^2 s0^2/2,lambda s0^2,lambda s0,-lambda s0};
  ca=g2 {0,0,0,lambda (CA/2-CF) s0};
  cq=g2 {0,0,0,lambda CF s0/2};
  cg=g2 CA {lambda^2 s0^2/2,-lambda s0^2,-2lambda s0,lambda s0};
  {box,nav,av,qse,gse}=(#.basis& /@ {cb,cn,ca,cq,cg});
  coeffSum=FullSimplify[Expand[cg+cb+2cn+2ca+4cq]];
  sum=coeffSum.basis;
  (* After repairing BT, leaving the OLD three negative signs in nav
     gives this residual in the massless QCD alpha-minus-Landau test. *)
  oldRGZResidual=g2 CA {2(\[Alpha]^2-2\[Alpha])s0^2,
    4\[Alpha]s0^2,4\[Alpha]s0,0};
  <|"box"->box,"navSingleDiagram"->nav,"avSingleDiagram"->av,
    "qseSingleLeg"->qse,"gseGaugeDependentPart"->gse,
    "cancellationCoefficient"->coeffSum,"CancellationDifference"->sum,"Passed"->TrueQ[sum === 0],
    "OriginalNAVSignQCDLimitResidualCoefficient"->oldRGZResidual|>
];

