(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* ================================================================
   RGZ one-loop AA graphs and PT diagnostics.
   Load FeynCalc (with FeynArts) first. Set modelRGZ to the model basename.
   This file is for the custom representation whose V/S propagators are
   dummy tensors and whose order-1 two-leg vertices carry FULL propagators.
   U propagators must already be physical propagators. rho is set to zero.
   No claim of RGZ gauge cancellation is built into this file.
   PreFactor -> 1 throughout: no loop measure is included here.
   ================================================================ *)

ClearAll[RGZLoopName, RGZLoopNormalize, RGZLoopPrepareFA,
  RGZLoopReduce, RGZLoopTerms, RGZLoopPT, RGZLoopDelta, RGZLoopFreeLorentz,
  RGZLoopPoleMasses, RGZLoopSplitFactor, RGZLoopSplitTerm,
  RGZLoopSplit, RGZLoopTID, RGZLoopConvert, RGZLoopUnresolved,
  RGZRunAA, RGZShowLoopGraph, RGZReduceAllAA, RGZGaugeDifference,
  RGZQCDPinchBenchmark, RGZCandidatePinches,
  rgzLoopM1, rgzLoopM2, rgzLoopM3, rgzLoopM4];

RGZLoopName[x_Symbol] := SymbolName[Unevaluated[x]];
RGZLoopName[x_String] := x;
RGZLoopName[_] := "";

(* rho is the odd source; Greek Rho is a Lorentz label, not that source.
   Protect internal Lorentz-index objects from parameter replacements. *)
RGZLoopNormalize[expr_] := Module[{params,indices,tags,protected,result},
  params = <|"m" -> m, "mG" -> mG, "M" -> M,
    "\[Mu]" -> \[Mu], "\[Alpha]" -> \[Alpha],
    "m02" -> m^2, "mu2" -> \[Mu]^2, "mg2" -> mG^2,
    "mG2" -> mG^2, "M4" -> M^4, "rho" -> 0,
    "0" -> 0, "1" -> 1, "2" -> 2, "I" -> I|>;
  indices=DeleteDuplicates[Cases[expr,
    _LorentzIndex | _ExplicitLorentzIndex,Infinity]];
  tags=Table[Unique["rgzNormalizeLor"],{Length[indices]}];
  protected=expr /. Thread[indices->tags];
  result=FixedPoint[Function[z, z /. {
    HoldPattern[Superscript[a_, n_Integer]] :> a^n,
    HoldPattern[Subsuperscript[a_, b_, n_Integer]] :> Subscript[a,b]^n,
    HoldPattern[Subscript[a_,b_]] /;
      RGZLoopName[a] == "m" && RGZLoopName[b] == "G" :> mG,
    HoldPattern[FCGV[a_String]] /; KeyExistsQ[params,a] :> params[a],
    a_String /; KeyExistsQ[params,a] :> params[a],
    a_Symbol /; KeyExistsQ[params,RGZLoopName[a]] :>
      params[RGZLoopName[a]],
    HoldPattern[FCGV["N"]] :> SUNN,
    HoldPattern[FCGV["Nc"]] :> SUNN,
    rho -> 0}], protected, 8];
  result /. Thread[tags->indices]
];

(* Preserve the two distinct kinds of adjoint index. *)
RGZLoopPrepareFA[expr_] := expr /. {
  HoldPattern[h_[type_,label_]] /;
    RGZLoopName[h] == "Index" &&
      MemberQ[{"Gluon","Gribov"},RGZLoopName[type]] :>
    SUNIndex[If[IntegerQ[label],
      Symbol["RGZWorkflow`Engine`rgz" <> RGZLoopName[type] <> ToString[label]],
      label]]
};

RGZLoopFreeLorentz[expr_] := DeleteDuplicates[Flatten[
  Map[Function[t,Keys[Select[
    KeyDrop[Counts[Cases[t,HoldPattern[LorentzIndex[a_,___]] :> a,Infinity]],
      {\[Mu],\[Nu]}],# == 1&]]],RGZLoopTerms[Expand[expr]]]]];

RGZLoopReduce[expr_] := Module[{z,extra},
  z = RGZLoopNormalize[expr];
  z = z /. (HoldPattern[h_[a_,b_]] /;
    RGZLoopName[h] == "IndexDelta" :> SUNDelta[a,b]);
  z = Contract[ExpandScalarProduct[FCI[z]]];
  z = SUNSimplify[z, SUNNToCACF -> False];
  z = FCCanonicalizeDummyIndices[z];
  extra=RGZLoopFreeLorentz[z];
  If[extra =!= {},Return[Failure["UnexpectedFreeLorentzIndex",
    <|"Indices"->extra,"Amplitude"->z|>]]];
  Factor2[z]
];

RGZLoopTerms[expr_] := If[Head[expr] === Plus, List @@ expr, {expr}];
RGZLoopPT[k_,a_,b_] :=
  MTD[a,b] - FVD[k,a] FVD[k,b]/SPD[k,k];
RGZLoopDelta[z_] := (z-m^2)(z-\[Mu]^2)+mG^4;

(* These labels are independent of the user's m1,m2,m3,m4. *)
RGZLoopPoleMasses[expr_] := expr /. {
  rgzLoopM1 -> Sqrt[-I Sqrt[\[Alpha] M^4]],
  rgzLoopM2 -> Sqrt[I Sqrt[\[Alpha] M^4]],
  rgzLoopM3 -> Sqrt[(m^2+\[Mu]^2)/2+
    I Sqrt[mG^4-(m^2-\[Mu]^2)^2/4]],
  rgzLoopM4 -> Sqrt[(m^2+\[Mu]^2)/2-
    I Sqrt[mG^4-(m^2-\[Mu]^2)^2/4]]
};

(* Split a single polynomial denominator in k^2. Repeated poles allowed.
   Coefficients are verified by recombining BEFORE introducing FAD. *)
RGZLoopSplitFactor[den_, var_, k_] := Module[
  {roots, r, mass, order, quot, regular, coeff, out=0, back=0, degree},
  roots = {{0,0},{\[Mu]^2,\[Mu]},
    {-I Sqrt[\[Alpha] M^4],rgzLoopM1},
    {I Sqrt[\[Alpha] M^4],rgzLoopM2},
    {(m^2+\[Mu]^2)/2+I Sqrt[mG^4-(m^2-\[Mu]^2)^2/4],rgzLoopM3},
    {(m^2+\[Mu]^2)/2-I Sqrt[mG^4-(m^2-\[Mu]^2)^2/4],rgzLoopM4}};
  roots = DeleteDuplicates[roots,
    TrueQ[FullSimplify[#1[[1]]-#2[[1]]] === 0]&];
  If[!PolynomialQ[den,var],
    Return[Failure["NonPolynomial",<|"Denominator"->den|>]]];
  degree = Exponent[den,var];
  Do[
    r = pole[[1]]; mass = pole[[2]];
    If[TrueQ[FullSimplify[den /. var->r] === 0],
      order = 1;
      While[order < degree &&
        TrueQ[FullSimplify[D[den,{var,order}] /. var->r] === 0],order++];
      quot = FullSimplify[PolynomialQuotient[den,(var-r)^order,var]];
      regular = 1/quot;
      Do[
        coeff = FullSimplify[
          (D[regular,{var,order-j}] /. var->r)/Factorial[order-j]];
        back += coeff/(var-r)^j;
        out += coeff ChangeDimension[FCI[FAD @@ ConstantArray[{k,mass},j]],D],
        {j,1,order}]
    ], {pole,roots}];
  If[!TrueQ[FullSimplify[Together[1/den-back]] === 0],
    Return[Failure["UnknownPole",<|"Denominator"->den,
      "Remainder"->Together[1/den-back]|>]]];
  out
];

(* The graph may use q, q-p, or q+p. Inspect EACH denominator factor;
   never identify Pair[Momentum[p],Momentum[q]] with q^2.
   Multiply ALL factors with the SAME routing before partial fractions.
   This prevents artificial higher-point integrals from products of
   different simple propagators carrying identical loop momentum.
   Genuine repeated poles retain their powers. *)
RGZLoopSplitTerm[expr_, loop_:q, ext_:p] := Module[
  {x,z,u,v,r,num,den,factors,out,f,n,poly,k,split,backRules,route,
    groups={1,1,1},routing={loop,loop-ext,loop+ext},j,stop},Catch[
  r = ExpandScalarProduct[
    FeynAmpDenominatorExplicit[ChangeDimension[FCI[RGZLoopNormalize[expr]],D]]];
  r = r /. {
    HoldPattern[Pair[Momentum[loop,D],Momentum[loop,D]]] -> x,
    HoldPattern[Pair[Momentum[ext,D],Momentum[loop,D]]] -> z,
    HoldPattern[Pair[Momentum[loop,D],Momentum[ext,D]]] -> z,
    HoldPattern[Pair[Momentum[ext,D],Momentum[ext,D]]] -> u};
  r = Together[r]; num = Numerator[r]; den = Denominator[r];
  If[!PolynomialQ[den,{x,z}],
    Throw[Failure["NonRationalDenominator",<|"Expression"->r|>],stop]];
  factors = FactorList[den]; out = num/factors[[1,1]];
  Do[
    {f,n} = item;
    If[FreeQ[f,x|z],out /= f^n,
      route = Which[
        FreeQ[f,z], {f /. x->v,1},
        FreeQ[Expand[f /. x->v+2z-u],z],
          {Expand[f /. x->v+2z-u],2},
        FreeQ[Expand[f /. x->v-2z-u],z],
          {Expand[f /. x->v-2z-u],3},
        True, Throw[Failure["UnsupportedRouting",<|"Factor"->f|>],stop]];
      {poly,j} = route;
      groups[[j]] *= poly^n
    ], {item,Rest[factors]}];
  Do[If[groups[[j]]=!=1,
    k=routing[[j]];
    split=RGZLoopSplitFactor[Expand[groups[[j]]],v,k];
    If[FailureQ[split],Throw[split,stop]];
    out *= split],{j,3}];
  backRules = {x->SPD[loop,loop],z->SPD[ext,loop],u->SPD[ext,ext]};
  FCI[out /. backRules],stop]
];

RGZLoopSplit[expr_, loop_:q, ext_:p] := Module[{parts,bad},
  parts = RGZLoopSplitTerm[#,loop,ext]& /@
    RGZLoopTerms[Expand[FCI[expr]]];
  bad = Select[parts,FailureQ];
  If[bad =!= {},Return[First[bad]]];
  FeynAmpDenominatorCombine[Total[parts]]
];

RGZLoopTID[expr_, loop_:q, ext_:p] := Module[{split,parts,result},
  If[expr === $Failed || FailureQ[expr],Return[expr]];
  split = RGZLoopSplit[expr,loop,ext];
  If[FailureQ[split],Return[split]];
  parts = RGZLoopTerms[Expand[split]];
  result = Total[Map[
    Function[t,If[FreeQ[t,_FeynAmpDenominator],0,
      TID[t,loop,ToPaVe->True,UsePaVeBasis->True,ApartFF->False]]],
    parts]];
  If[!FreeQ[result,_TID|_FeynAmpDenominator],
    Return[Failure["IncompleteTID",<|"Output"->result|>]]];
  Factor2[Contract[result]]
];

(* Three sectors: every internal V/S line gets one FULL-propagator
   insertion; every internal U line gets NONE. Topology[2] gives 1/2,
   not two loops. No physical counterterms on the cubic/quartic vertices. *)
rgzLoopTopTadpole = TopologyList[Topology[2][
  Propagator[Incoming][Vertex[1][1],Vertex[4][3]],
  Propagator[Outgoing][Vertex[1][2],Vertex[4][3]],
  Propagator[FALoop[1]][Vertex[4][3],Vertex[2,1][4]],
  Propagator[FALoop[1]][Vertex[4][3],Vertex[2,1][4]]]];

rgzLoopTopBosonBubble = TopologyList[Topology[2][
  Propagator[Incoming][Vertex[1][1],Vertex[3][3]],
  Propagator[Outgoing][Vertex[1][2],Vertex[3][4]],
  Propagator[FALoop[1]][Vertex[3][3],Vertex[2,1][5]],
  Propagator[FALoop[1]][Vertex[3][4],Vertex[2,1][5]],
  Propagator[FALoop[1]][Vertex[3][3],Vertex[2,1][6]],
  Propagator[FALoop[1]][Vertex[3][4],Vertex[2,1][6]]]];

rgzLoopTopGhostBubble = TopologyList[Topology[2][
  Propagator[Incoming][Vertex[1][1],Vertex[3][3]],
  Propagator[Outgoing][Vertex[1][2],Vertex[3][4]],
  Propagator[FALoop[1]][Vertex[3][3],Vertex[3][4]],
  Propagator[FALoop[1]][Vertex[3][3],Vertex[3][4]]]];

RGZLoopUnresolved[expr_] := DeleteDuplicates[Cases[expr,
  a_ /; MemberQ[{"GenericCoupling","FeynAmp","FAScalarProduct",
    "FAMetricTensor","FAFourVector","FAPropagatorDenominator"},
      RGZLoopName[Head[a]]] || RGZLoopName[Head[Head[a]]] == "G",
  Infinity]];

RGZLoopConvert[diagrams_] := Module[{fa,fc,bad},
  If[Length[diagrams] === 0,Return[{}]];
  fa = CreateFeynAmp[diagrams,Truncated->True,GaugeRules->{},PreFactor->1];
  fa = RGZLoopPrepareFA[fa];
  fc = FCFAConvert[fa,IncomingMomenta->{p},OutgoingMomenta->{p},
    LoopMomenta->{q},UndoChiralSplittings->True,
    LorentzIndexNames->{\[Mu],\[Nu]},ChangeDimension->D,
    List->True,DropSumOver->True];
  If[!ListQ[fc],Return[Failure["ConversionFailed",<|"Output"->fc|>]]];
  bad=RGZLoopUnresolved[fc];
  If[bad =!= {},Return[Failure["UnresolvedCoupling",<|"Objects"->bad|>]]];
  If[!FreeQ[fc, a_ /; MemberQ[{"mG1","mG2","mG3","mA1","mA2","mB"},
      RGZLoopName[a]]],
    Print["The model contains legacy placeholder denominators. Use corrected gen/mod files that pass propagator checks; unchecked legacy substitution rules are not applied."]];
  fc=RGZLoopReduce /@ fc;
  bad=Select[fc,FailureQ];
  If[bad =!= {},First[bad],fc]
];

RGZRunAA[] := Module[{rgzFunctionReturnTag=Unique["RGZWorkflow`Private`functionReturn"]},Catch[
Module[{tops,inserted,amps,rows={},data,sector,a,i},
  If[!StringQ[modelRGZ],
    Return[Failure["SetModel",<|"Message"->"Set modelRGZ to the shared .gen/.mod path without an extension."|>]]];
  tops = <|"FourPointVertexLoop"->rgzLoopTopTadpole,
    "BosonicThreePointLoop"->rgzLoopTopBosonBubble,
    "GhostThreePointLoop"->rgzLoopTopGhostBubble|>;
  inserted = Association[]; amps = Association[];
  Do[
    Print["Generating: ",sector];
    data = InsertFields[tops[sector],{V[1]}->{V[1]},
      InsertionLevel->{Particles},Model->modelRGZ,GenericModel->modelRGZ];
    data = If[sector == "GhostThreePointLoop",
      DiagramSelect[data,With[{fields=LoopFields[##]},
        fields =!= {} && And@@(MatchQ[#,_U|-_U]& /@ fields)]&],
      DiagramSelect[data,FreeQ[LoopFields[##],_U]&]];
    AssociateTo[inserted,sector->data];
    a = RGZLoopConvert[data];
    If[FailureQ[a],Throw[a,rgzFunctionReturnTag]];
    AssociateTo[amps,sector->a];
    Do[AppendTo[rows,<|"Category"->sector,"WithinClassID"->i,
      "Amplitude"->a[[i]]|>],{i,Length[a]}];
    Print[sector,"：",Length[a]," TermAmplitude"], {sector,Keys[tops]}];
  If[rows === {},Return[Failure["NoGraphs",<||>]]];
  Print["for All ",Length[rows]," TermSum; Notexclude  φ, β and its conjugate Field. "];
  <|"InsertionDiagram"->inserted,"ClassifiedAmplitude"->amps,"PerDiagramResults"->rows,
    "AmputatedAATotalAmplitude"->Total[Lookup[rows,"Amplitude"]],
    "Description"->"Ordinary AA 1PI insertion; full mixed-propagator corrections and a proven PT self-energy are outside this result."|>
],rgzFunctionReturnTag]];

RGZShowLoopGraph[result_Association,n_Integer] := Module[{rows,row},
  rows = result["PerDiagramResults"];
  If[n<1 || n>Length[rows],Return[$Failed]];
  row=rows[[n]];
  Print["number  ",n," Term: ",row["Category"],", WithinClassID ",row["WithinClassID"]];
  Print[TraditionalForm[row["Amplitude"]]]
];

RGZReduceAllAA[result_Association] := Module[{rows,out={},row,z,i,bad},
  rows = result["PerDiagramResults"];
  Do[
    row=rows[[i]]; Print["Reducing ",i," / ",Length[rows]];
    z = RGZLoopTID[row["Amplitude"]];
    AppendTo[out,Join[row,<|"PaVeResult"->z|>]],{i,Length[rows]}];
  bad = Select[out,MemberQ[{$Failed,$Aborted},# ["PaVeResult"]] ||
    FailureQ[# ["PaVeResult"]]&];
  If[bad =!= {},Return[Failure["ReductionFailed",<|"FailedTerm"->bad|>]]];
  Join[result,<|"PerDiagramPaVe"->out,
    "AmputatedAATotalPaVe"->Factor2[Total[Lookup[out,"PaVeResult"]]]|>]
];

(* Gauge dependence is hidden in longitudinal pole masses as well.
   A nonzero un-simplified expression is NOT automatically a proof of
   dependence. Values a1,a2 should avoid a singular/degenerate pole limit. *)
RGZGaugeDifference[expr_,{a1_,a2_}] := Module[{physical,diff},
  physical = RGZLoopPoleMasses[expr];
  diff = FullSimplify[(physical /. \[Alpha]->a1)-
    (physical /. \[Alpha]->a2)];
  <|"ParameterValues"->{a1,a2},"Difference"->diff,
    "Status"->If[TrueQ[diff === 0],"The difference is zero at these two parameter points",
      "The difference is not zero; check missing terms, conventions and integral identities"]|>
];

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

(* Compatibility entry point for the historical alpha-minus-Landau check.
   All physics formulas now come from the shared reviewed implementation. *)
Get[FileNameJoin[{RGZWorkflow`Private`backendDirectory[],"RGZ_Pinch_Kernels.wl"}]];
Options[RGZCandidatePinches]={"Mode"->"GaugeDifference","ReferenceGauge"->0};
RGZCandidatePinches[OptionsPattern[]] := RGZBuildPinches[
  "Mode"->OptionValue["Mode"],"ReferenceGauge"->OptionValue["ReferenceGauge"],
  "Representation"->"FAD"];

Print["One-loop checks loaded. First run rgzOneLoop = RGZRunAA[];" ];
Print["Inspect one diagram with RGZShowLoopGraph[rgzOneLoop,1]; reduce all with rgzOneLoopPV = RGZReduceAllAA[rgzOneLoop];" ];
Print["Independent QCD benchmark: RGZQCDPinchBenchmark[][\"CancellationDifference\"] should be 0." ];
Print["This module makes no gauge-independence claim for candidate RGZ pinch terms." ];
