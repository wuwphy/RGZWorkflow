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
    Print["\:63d0\:793a\:ff1a\:5f53\:524d\:6a21\:578b\:4ecd\:542b\:65e7\:5360\:4f4d\:5206\:6bcd\:3002\:8bf7\:4f7f\:7528\:524d\:9762\:4f20\:64ad\:5b50\:68c0\:67e5\:5df2\:901a\:8fc7\:7684\:4fee\:6b63\:7248 gen/mod\:ff1b\:672c\:811a\:672c\:4e0d\:4f1a\:6cbf\:7528\:672a\:7ecf\:6838\:5bf9\:7684\:65e7\:66ff\:6362\:89c4\:5219\:3002"]];
  fc=RGZLoopReduce /@ fc;
  bad=Select[fc,FailureQ];
  If[bad =!= {},First[bad],fc]
];

RGZRunAA[] := Module[{tops,inserted,amps,rows={},data,sector,a,i},
  If[!StringQ[modelRGZ],
    Return[Failure["SetModel",<|"Message"->"\:5148\:8bbe\:7f6e modelRGZ \:4e3a .gen/.mod \:7684\:5171\:540c\:8def\:5f84\:ff0c\:4e0d\:5e26\:6269\:5c55\:540d\:3002"|>]]];
  tops = <|"\:56db\:70b9\:9876\:70b9\:5708\:56fe"->rgzLoopTopTadpole,
    "\:73bb\:8272\:573a\:4e24\:4e09\:70b9\:9876\:70b9\:5708\:56fe"->rgzLoopTopBosonBubble,
    "\:9b3c\:573a\:4e24\:4e09\:70b9\:9876\:70b9\:5708\:56fe"->rgzLoopTopGhostBubble|>;
  inserted = Association[]; amps = Association[];
  Do[
    Print["\:6b63\:5728\:751f\:6210\:ff1a",sector];
    data = InsertFields[tops[sector],{V[1]}->{V[1]},
      InsertionLevel->{Particles},Model->modelRGZ,GenericModel->modelRGZ];
    data = If[sector == "\:9b3c\:573a\:4e24\:4e09\:70b9\:9876\:70b9\:5708\:56fe",
      DiagramSelect[data,With[{fields=LoopFields[##]},
        fields =!= {} && And@@(MatchQ[#,_U|-_U]& /@ fields)]&],
      DiagramSelect[data,FreeQ[LoopFields[##],_U]&]];
    AssociateTo[inserted,sector->data];
    a = RGZLoopConvert[data];
    If[FailureQ[a],Return[a]];
    AssociateTo[amps,sector->a];
    Do[AppendTo[rows,<|"\:7c7b\:522b"->sector,"\:7c7b\:5185\:7f16\:53f7"->i,
      "\:632f\:5e45"->a[[i]]|>],{i,Length[a]}];
    Print[sector,"\:ff1a",Length[a]," \:9879\:632f\:5e45"], {sector,Keys[tops]}];
  If[rows === {},Return[Failure["NoGraphs",<||>]]];
  Print["\:5df2\:5bf9\:5168\:90e8 ",Length[rows]," \:9879\:6c42\:548c\:ff1b\:672a\:6392\:9664 \:03c6\:3001\:03b2 \:53ca\:5176\:53cd\:573a\:3002"];
  <|"\:63d2\:5165\:56fe"->inserted,"\:5206\:7c7b\:632f\:5e45"->amps,"\:9010\:56fe\:7ed3\:679c"->rows,
    "\:622a\:80a2AA\:603b\:632f\:5e45"->Total[Lookup[rows,"\:632f\:5e45"]],
    "\:8bf4\:660e"->"\:666e\:901a AA \:7684 1PI \:63d2\:5165\:ff1b\:4e0d\:662f\:5b8c\:6574\:6df7\:5408\:4f20\:64ad\:5b50\:7684\:4e00\:5708\:4fee\:6b63\:ff0c\:4e5f\:4e0d\:662f\:5df2\:8bc1\:660e\:7684 PT \:81ea\:80fd\:3002"|>
];

RGZShowLoopGraph[result_Association,n_Integer] := Module[{rows,row},
  rows = result["\:9010\:56fe\:7ed3\:679c"];
  If[n<1 || n>Length[rows],Return[$Failed]];
  row=rows[[n]];
  Print["\:7b2c ",n," \:9879\:ff1a",row["\:7c7b\:522b"],"\:ff0c\:7c7b\:5185\:7f16\:53f7 ",row["\:7c7b\:5185\:7f16\:53f7"]];
  Print[TraditionalForm[row["\:632f\:5e45"]]]
];

RGZReduceAllAA[result_Association] := Module[{rows,out={},row,z,i,bad},
  rows = result["\:9010\:56fe\:7ed3\:679c"];
  Do[
    row=rows[[i]]; Print["\:6b63\:5728\:7ea6\:5316 ",i," / ",Length[rows]];
    z = RGZLoopTID[row["\:632f\:5e45"]];
    AppendTo[out,Join[row,<|"PaVe\:7ed3\:679c"->z|>]],{i,Length[rows]}];
  bad = Select[out,MemberQ[{$Failed,$Aborted},# ["PaVe\:7ed3\:679c"]] ||
    FailureQ[# ["PaVe\:7ed3\:679c"]]&];
  If[bad =!= {},Return[Failure["ReductionFailed",<|"\:5931\:8d25\:9879"->bad|>]]];
  Join[result,<|"\:9010\:56fePaVe"->out,
    "\:622a\:80a2AA\:603bPaVe"->Factor2[Total[Lookup[out,"PaVe\:7ed3\:679c"]]]|>]
];

(* Gauge dependence is hidden in longitudinal pole masses as well.
   A nonzero un-simplified expression is NOT automatically a proof of
   dependence. Values a1,a2 should avoid a singular/degenerate pole limit. *)
RGZGaugeDifference[expr_,{a1_,a2_}] := Module[{physical,diff},
  physical = RGZLoopPoleMasses[expr];
  diff = FullSimplify[(physical /. \[Alpha]->a1)-
    (physical /. \[Alpha]->a2)];
  <|"\:53c2\:6570\:53d6\:503c"->{a1,a2},"\:5dee\:503c"->diff,
    "\:72b6\:6001"->If[TrueQ[diff === 0],"\:8fd9\:4e24\:4e2a\:53c2\:6570\:53d6\:503c\:4e0b\:5dee\:503c\:4e3a\:96f6",
      "\:5dee\:503c\:5c1a\:672a\:5316\:96f6\:ff1a\:9700\:68c0\:67e5\:9057\:6f0f\:9879\:3001\:7ea6\:5b9a\:6216\:79ef\:5206\:6052\:7b49\:5f0f"]|>
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
  <|"box"->box,"nav\:5355\:56fe"->nav,"av\:5355\:56fe"->av,
    "qse\:5355\:817f"->qse,"gse\:89c4\:8303\:76f8\:5173\:90e8\:5206"->gse,
    "\:62b5\:6d88\:7cfb\:6570"->coeffSum,"\:62b5\:6d88\:5dee\:503c"->sum,"\:901a\:8fc7"->TrueQ[sum === 0],
    "\:539fnav\:7b26\:53f7\:7684QCD\:6781\:9650\:6b8b\:4f59\:7cfb\:6570"->oldRGZResidual|>
];

(* Compatibility entry point for the historical alpha-minus-Landau check.
   All physics formulas now come from the shared reviewed implementation. *)
Get[FileNameJoin[{RGZWorkflow`Private`backendDirectory[],"RGZ_Pinch_Kernels.wl"}]];
Options[RGZCandidatePinches]={"Mode"->"GaugeDifference","ReferenceGauge"->0};
RGZCandidatePinches[OptionsPattern[]] := RGZBuildPinches[
  "Mode"->OptionValue["Mode"],"ReferenceGauge"->OptionValue["ReferenceGauge"],
  "Representation"->"FAD"];

Print["\:5df2\:8f7d\:5165 RGZ \:4e00\:5708\:68c0\:67e5\:51fd\:6570\:3002\:5148\:8fd0\:884c\:ff1argzOneLoop = RGZRunAA[];" ];
Print["\:9010\:56fe\:67e5\:770b\:ff1aRGZShowLoopGraph[rgzOneLoop,1]\:ff1b\:5168\:90e8\:7ea6\:5316\:ff1argzOneLoopPV = RGZReduceAllAA[rgzOneLoop];" ];
Print["\:72ec\:7acb QCD \:57fa\:51c6\:ff1aRGZQCDPinchBenchmark[][\"\:62b5\:6d88\:5dee\:503c\"]\:ff0c\:5e94\:4e3a 0\:3002" ];
Print["\:672c\:6587\:4ef6\:672a\:5ba3\:79f0 RGZ \:5019\:9009 pinch \:9879\:89c4\:8303\:65e0\:5173\:3002" ];
