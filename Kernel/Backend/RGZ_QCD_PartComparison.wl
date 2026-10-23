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
      "nav\:5168\:90e8"->2g2 CA (-l^2 s0^2 h/2+
        l (s0^2 jnav+s0 k-s0 t)),
      "av\:5168\:90e8"->2l g2 (CA/2-CF)s0 t,
      "qse\:5168\:90e8"->4l g2 CF s0 t/2,
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
      "Message"->"\:6807\:91cf\:6295\:5f71\:4ecd\:542b\:6d1b\:4f26\:5179\:6307\:6807\:6216\:56fa\:5b9a\:65f6\:7a7a\:5206\:91cf\:ff1b\:68c0\:67e5\:53c2\:6570\:66ff\:6362\:662f\:5426\:6c61\:67d3\:4e86\:6307\:6807\:3002"|>]]];
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
      "Message"->"s\:3001x\:3001y \:8f6c\:6362\:540e\:4ecd\:542b\:672a\:89e3\:6790\:5f20\:91cf\:6216\:65f6\:95f4\:5206\:91cf\:3002"|>]]];
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
    Return[<|"\:9879\:76ee"->name,"\:6587\:732e\:5f0f\:53f7"->eq,"\:8ba1\:7b97\:503c"->actual,
      "\:6587\:732e\:53c2\:8003"->reference,"\:6362\:5143\:540e\:5dee\:503c"->actual,
      "\:79ef\:5206\:540e\:5dee\:503c\:7cfb\:6570"->actual,"\:72b6\:6001"->
        If[Head[actual] === Missing,"\:672a\:8fd0\:884c\:5b9e\:9645\:5708\:56fe","\:8ba1\:7b97\:5931\:8d25"]|>]];
  diff=Cancel[Together[actual-reference]];
  shiftDiff=FullSimplify[Cancel[Together[
    (diff+(diff /. {rgzQCDX->rgzQCDY,rgzQCDY->rgzQCDX}))/2]]];
  actualMaster=RGZQCDMasterCoefficient[actual];
  refMaster=RGZQCDMasterCoefficient[reference];
  masterDiff=If[FailureQ[actualMaster] || FailureQ[refMaster],
    Failure["MasterReduction",<|"Actual"->actualMaster,"Reference"->refMaster|>],
    FullSimplify[actualMaster-refMaster]];
  state=Which[FailureQ[masterDiff],"\:79ef\:5206\:7ea6\:5316\:5931\:8d25",
    TrueQ[shiftDiff === 0] && TrueQ[masterDiff === 0],"\:79ef\:5206\:524d\:540e\:5747\:901a\:8fc7",
    TrueQ[masterDiff === 0],"\:79ef\:5206\:540e\:901a\:8fc7\:ff1b\:79ef\:5206\:524d\:5f62\:5f0f\:4e0d\:540c",
    True,"\:5dee\:503c\:672a\:5f52\:96f6"];
  <|"\:9879\:76ee"->name,"\:6587\:732e\:5f0f\:53f7"->eq,"\:8ba1\:7b97\:503c"->actual,
    "\:6587\:732e\:53c2\:8003"->reference,"\:6362\:5143\:540e\:5dee\:503c"->shiftDiff,
    "\:8ba1\:7b97\:7684I11\:7cfb\:6570"->actualMaster,"\:6587\:732e\:7684I11\:7cfb\:6570"->refMaster,
    "\:79ef\:5206\:540e\:5dee\:503c\:7cfb\:6570"->masterDiff,"\:72b6\:6001"->state|>
];

RGZQCDCalculatedLoops[loops_Association] := Module[
  {pureV,pureU,convert,selected,amps,parts,bad,out},
  pureV=With[{fields=LoopFields[##]},
    fields =!= {} && And@@(MatchQ[#,V[1,___]|-V[1,___]]& /@ fields)]&;
  pureU=With[{fields=LoopFields[##]},
    fields =!= {} && And@@(MatchQ[#,U[1,___]|-U[1,___]]& /@ fields)]&;
  convert[key_,filter_] := Module[{graphs},
    selected=DiagramSelect[loops["\:63d2\:5165\:56fe"][key],filter];
    amps=RGZLoopConvert[selected];
    If[FailureQ[amps],Return[amps]];
    parts=Map[Function[a,Module[{sc},
      sc=RGZQCDMassless[a]; If[FailureQ[sc],Return[sc]];
      sc=RGZQCDProject[sc]; If[FailureQ[sc],Return[sc]];
      RGZQCDXY[sc]]],amps];
    bad=Select[parts,FailureQ];
    If[bad =!= {},Return[First[bad]]];
    FullSimplify[Total[parts]]];
  out=<|"gse"->convert["\:73bb\:8272\:573a\:4e24\:4e09\:70b9\:9876\:70b9\:5708\:56fe",pureV],
    "\:56db\:80f6\:5b50\:5708\:56fe"->convert["\:56db\:70b9\:9876\:70b9\:5708\:56fe",pureV],
    "FP\:9b3c\:5708\:56fe"->convert["\:9b3c\:573a\:4e24\:4e09\:70b9\:9876\:70b9\:5708\:56fe",pureU]|>;
  out
];

Options[RGZCompareWithQCD]={"FeynArtsToPaperFactor"->1};
RGZCompareWithQCD[pinches_:Automatic,loops_:Automatic,OptionsPattern[]] := Module[
  {source,paper,paperDelta,refXY,calcXY=Association[],weights,keys,eqs,
    a,sc,rows,loopParts,factor,totalPaper,totalCalc,summary},
  If[Length[DownValues[RGZCandidatePinches]] === 0,
    Return[Failure["LoadMainFile",<|"Message"->"\:5148\:52a0\:8f7d RGZ_OneLoop_AA_PT_Check.wl\:3002"|>]]];
  If[Total[Length /@ {OwnValues[m],OwnValues[mG],OwnValues[M],
      OwnValues[\[Mu]],OwnValues[\[Alpha]]}] > 0,
    Return[Failure["SymbolicParametersRequired",<|
      "Message"->"\:6bd4\:8f83\:524d m\:3001mG\:3001M\:3001\:03bc\:3001\:03b1 \:9700\:4fdd\:7559\:4e3a\:672a\:8d4b\:503c\:7684\:7b26\:53f7\:ff1b\:4e0d\:8981\:5148\:4ee4\:8d28\:91cf\:6216 \:03b1 \:4e3a\:96f6\:3002"|>]]];
  source=If[pinches === Automatic,RGZCandidatePinches[],pinches];
  keys={"box","nav\:5168\:90e8","av\:5168\:90e8","qse\:5168\:90e8"};
  If[!AssociationQ[source] || !And@@(KeyExistsQ[source,#]& /@ keys),
    Return[Failure["MissingPinchParts",<|"RequiredKeys"->keys|>]]];
  If[loops =!= Automatic &&
      (!AssociationQ[loops] || !KeyExistsQ[loops,"\:63d2\:5165\:56fe"]),
    Return[Failure["MissingLoopGraphs",<|
      "Message"->"\:5b9e\:9645\:5708\:56fe\:6bd4\:8f83\:9700\:8981 RGZRunAA[] \:6210\:529f\:8fd4\:56de\:7684 rgzOneLoop\:ff1b\:53ea\:6bd4\:8f83\:56db\:7c7b\:624b\:5199\:9879\:65f6\:4e0d\:4f20\:7b2c\:4e8c\:4e2a\:53c2\:6570\:3002"|>]]];
  paper=RGZQCDPaperParts[];
  paperDelta=Map[FullSimplify[#-(# /. \[Alpha]->0)]&,paper];
  refXY=Map[RGZQCDXY[RGZQCDProject[RGZQCDRational[#]]]&,paperDelta];
  If[AnyTrue[Values[refXY],FailureQ],
    Return[Failure["ReferenceConstruction",<|"Parts"->refXY|>]]];
  Do[
    Print["\:6b63\:5728\:6bd4\:8f83 QCD \:6781\:9650\:ff1a",key];
    a=RGZQCDMassless[source[key]];
    If[FailureQ[a],AssociateTo[calcXY,key->a],
      (* d(s)^2=1/s^2: amputate the common external propagators. *)
      sc=RGZQCDProject[SPD[p,p]^2 a];
      AssociateTo[calcXY,key->If[FailureQ[sc],sc,RGZQCDXY[sc]]]],
    {key,keys}];
  eqs=<|"box"->"(2.45)","nav\:5168\:90e8"->"2 \:00d7 (2.47)",
    "av\:5168\:90e8"->"2 \:00d7 (2.47)","qse\:5168\:90e8"->"4 \:00d7 (2.52)",
    "gse"->"(2.56)"|>;
  rows=Map[RGZQCDRow[#,eqs[#],calcXY[#],refXY[#]]&,keys];
  If[AssociationQ[loops] && KeyExistsQ[loops,"\:63d2\:5165\:56fe"],
    Print["\:6b63\:5728\:6bd4\:8f83\:5b9e\:9645\:751f\:6210\:7684 QCD \:80f6\:5b50\:3001\:56db\:80f6\:5b50\:548c FP \:9b3c\:5708\:56fe\:3002"];
    loopParts=RGZQCDCalculatedLoops[loops];
    factor=OptionValue["FeynArtsToPaperFactor"];
    Do[
      a=loopParts[key];
      If[!FailureQ[a],a=factor(a-(a /. \[Alpha]->0))];
      AssociateTo[calcXY,key->a],{key,{"gse","FP\:9b3c\:5708\:56fe"}}];
    AssociateTo[calcXY,"\:56db\:80f6\:5b50\:5708\:56fe"->If[FailureQ[loopParts["\:56db\:80f6\:5b50\:5708\:56fe"]],
      loopParts["\:56db\:80f6\:5b50\:5708\:56fe"],factor loopParts["\:56db\:80f6\:5b50\:5708\:56fe"]]],
    AssociateTo[calcXY,{"gse"->Missing["ActualGraphsNotProvided"],
      "\:56db\:80f6\:5b50\:5708\:56fe"->Missing["ActualGraphsNotProvided"],
      "FP\:9b3c\:5708\:56fe"->Missing["ActualGraphsNotProvided"]}]];
  AppendTo[rows,RGZQCDRow["gse",eqs["gse"],calcXY["gse"],refXY["gse"]]];
  AppendTo[rows,RGZQCDRow["\:56db\:80f6\:5b50\:5708\:56fe","\:65e0\:8d28\:91cf\:874c\:86aa\:79ef\:5206=0",
    calcXY["\:56db\:80f6\:5b50\:5708\:56fe"],0]];
  AppendTo[rows,RGZQCDRow["FP\:9b3c\:5708\:56fe\:7684\:89c4\:8303\:5dee","\:03b1 \:65e0\:5173",
    calcXY["FP\:9b3c\:5708\:56fe"],0]];
  totalPaper=RGZQCDMasterCoefficient[Total[Values[refXY]]];
  totalCalc=If[AnyTrue[Lookup[calcXY,Append[keys,"gse"]],
    FailureQ[#] || Head[#] === Missing&],Missing["ActualTotalNotAvailable"],
    RGZQCDMasterCoefficient[Total[Lookup[calcXY,Append[keys,"gse"]]]]];
  summary=Grid[Prepend[Map[
    {#["\:9879\:76ee"],#["\:6587\:732e\:5f0f\:53f7"],#["\:72b6\:6001"],#["\:79ef\:5206\:540e\:5dee\:503c\:7cfb\:6570"]}&,rows],
    {"\:6bd4\:8f83\:9879\:76ee","\:6587\:732e\:4f9d\:636e","\:72b6\:6001","\:5dee\:503c / I11"}],
    Frame->All,Alignment->Left,Spacings->{2,1}];
  Print[summary];
  Print["\:6587\:732e\:4e94\:7c7b\:89c4\:8303\:5dee\:603b\:548c / I11\:ff1a",totalPaper];
  Print["\:5b9e\:9645\:8ba1\:7b97\:4e94\:7c7b\:89c4\:8303\:5dee\:603b\:548c / I11\:ff1a",totalCalc];
  <|"\:9010\:9879\:7ed3\:679c"->rows,"\:6587\:732e\:539f\:516c\:5f0f\:ff08\:03b1=1\:57fa\:51c6\:ff09"->paper,
    "\:6587\:732e\:89c4\:8303\:5dee\:ff08\:03b1=0\:57fa\:51c6\:ff09"->paperDelta,
    "\:6587\:732e\:603b\:5dee\:503c\:7cfb\:6570"->totalPaper,"\:8ba1\:7b97\:603b\:5dee\:503c\:7cfb\:6570"->totalCalc,
    "\:6458\:8981"->summary,
    "\:8bf4\:660e"->"s=p^2\:ff0cx=q^2\:ff0cy=(q-p)^2\:ff1b\:79ef\:5206\:7ed3\:679c\:4e58\:5171\:540c I11(s)\:3002\:8fd9\:662f QCD \:6781\:9650\:4e0b\:7684\:6a2a\:5411\:89c4\:8303\:76f8\:5173\:90e8\:5206\:6bd4\:8f83\:3002"|>
];

RGZShowQCDPart[report_Association,name_String] := Module[{row,fields},
  row=SelectFirst[report["\:9010\:9879\:7ed3\:679c"],#["\:9879\:76ee"] === name&,Missing["NoSuchPart"]];
  If[Head[row] === Missing,Return[row]];
  fields={"\:8ba1\:7b97\:503c","\:6587\:732e\:53c2\:8003","\:6362\:5143\:540e\:5dee\:503c","\:8ba1\:7b97\:7684I11\:7cfb\:6570",
    "\:6587\:732e\:7684I11\:7cfb\:6570","\:79ef\:5206\:540e\:5dee\:503c\:7cfb\:6570","\:72b6\:6001"};
  Print["\:9879\:76ee\:ff1a",name,"\:ff1b\:6587\:732e\:ff1a",row["\:6587\:732e\:5f0f\:53f7"]];
  Print[Grid[Map[{#,Pane[TraditionalForm[Lookup[row,#,Missing["NotComputed"]]],
    ImageSize->{850,Automatic},Scrollbars->{True,False}]}&,fields],
    Frame->All,Alignment->Left]]
];

Print["QCD \:9010\:9879\:6bd4\:8f83\:51fd\:6570\:5df2\:52a0\:8f7d\:3002\:8fd0\:884c RGZCompareWithQCD[rgzPinch,rgzOneLoop]\:3002"];
Print["\:4ec5\:6bd4\:8f83\:56db\:7c7b\:624b\:5199\:9879\:ff1aRGZCompareWithQCD[]\:ff1b\:5b9e\:9645 gse \:4f1a\:660e\:786e\:6807\:8bb0\:4e3a\:672a\:8fd0\:884c\:3002"];
