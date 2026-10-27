(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
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
