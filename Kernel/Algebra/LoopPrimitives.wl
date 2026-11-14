(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Role:
   Provide shared two-point conventions, pole splitting and native TID primitives.

   Inputs:
   FeynCalc loop expressions, canonical model parameters and loop/external momenta.

   Dependencies:
   FeynCalc/FeynArts. Definitions only: no AA graph generation or task initialization.
   loadLoopEngine[] loads this file before the mixed and quark backends.
*)

ClearAll[RGZGaugeDifference, RGZLoopDelta, RGZLoopFreeLorentz, RGZLoopGaugeValidate, RGZLoopName, RGZLoopNormalize, RGZLoopPT, RGZLoopPoleMasses, RGZLoopPrepareFA, RGZLoopReduce, RGZLoopSplit, RGZLoopSplitFactor, RGZLoopSplitTerm, RGZLoopTID, RGZLoopTerms, RGZLoopValidValueQ, RGZLoopUnresolved, rgzLoopM1, rgzLoopM2, rgzLoopM3, rgzLoopM4];

RGZLoopName[x_Symbol] := SymbolName[Unevaluated[x]];
RGZLoopName[x_String] := x;
RGZLoopName[_] := "";

(* Validate physical values before algebra can cancel or discard errors. *)
RGZLoopValidValueQ[x_] := FreeQ[x,
 _Failure|_Missing|_TerminatedEvaluation|$Failed|$Aborted|$TimedOut|
 Indeterminate|ComplexInfinity|_DirectedInfinity];
RGZLoopGaugeValidate[x_] := Module[{foreign},
 If[OwnValues[\[Alpha]]=!={},Return[Failure["AssignedGaugeParameter",<||>]]];
 If[!RGZLoopValidValueQ[x],Return[Failure["InvalidGaugeExpression",<||>]]];
 foreign=Cases[x,a_Symbol /; MemberQ[{"\[Alpha]","alpha"},SymbolName[Unevaluated[a]]] &&
   !SameQ[Unevaluated[a],\[Alpha]],{0,Infinity},Heads->True];
 If[foreign=!={} || !FreeQ[x,FCGV["\[Alpha]"|"alpha"]],
  Return[Failure["GaugeSymbolContext",<|"ExpectedContext"->Context[\[Alpha]]|>]]];True];

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
  If[!RGZLoopValidValueQ[expr],Return[Failure["InvalidAAAmplitude",<||>]]];
  z = RGZLoopNormalize[expr];
  z = z /. (HoldPattern[h_[a_,b_]] /;
    RGZLoopName[h] == "IndexDelta" :> SUNDelta[a,b]);
  z = Contract[ExpandScalarProduct[FCI[z]]];
  z = SUNSimplify[z, SUNNToCACF -> False];
  z = FCCanonicalizeDummyIndices[z];
  extra=RGZLoopFreeLorentz[z];
  If[extra =!= {},Return[Failure["UnexpectedFreeLorentzIndex",
    <|"Indices"->extra,"Amplitude"->z|>]]];
  If[!RGZLoopValidValueQ[z],Return[Failure["InvalidAAAmplitude",<||>]]];
  z=Factor2[z];If[RGZLoopValidValueQ[z],z,Failure["InvalidAAAmplitude",<||>]]
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
RGZLoopSplitFactor[den_, var_, k_] := Block[{$Assumptions=True},Module[
  {roots, r, mass, order, quot, regular, coeff, out=0, back=0, degree,emitted,probe=Unique["rgzAAPoleProbe$"]},
  If[!RGZLoopValidValueQ[{den,var,k}],Return[Failure["InvalidAAPoleInput",<||>]]];
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
  With[{route=MomentumExpand[FCI[Momentum[k,D]]]},
    emitted=out /. HoldPattern[PropagatorDenominator[route,label_]] :>
      PropagatorDenominator[Momentum[probe,D],label]];
  emitted=FeynAmpDenominatorExplicit[RGZLoopPoleMasses[emitted]] /.
    Pair[Momentum[probe,D],Momentum[probe,D]]->var;
  If[!RGZLoopValidValueQ[{out,emitted}] || !TrueQ[FullSimplify[Together[1/den-emitted]]===0],
    Return[Failure["PoleLabelReconstruction",<|"Denominator"->den|>]]];
  out
]];

(* The graph may use q, q-p, or q+p. Inspect EACH denominator factor;
   never identify Pair[Momentum[p],Momentum[q]] with q^2.
   Multiply ALL factors with the SAME routing before partial fractions.
   This prevents artificial higher-point integrals from products of
   different simple propagators carrying identical loop momentum.
   Genuine repeated poles retain their powers. *)
RGZLoopSplitTerm[expr_, loop_:q, ext_:p] := Module[
  {x,z,u,v,r,num,den,factors,out,f,n,poly,k,split,backRules,route,
    groups={1,1,1},routing={loop,loop-ext,loop+ext},j,stop},Catch[
  If[!RGZLoopValidValueQ[expr],Throw[Failure["InvalidAAIntegralInput",<||>],stop]];
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
  If[!RGZLoopValidValueQ[expr],Return[Failure["InvalidAAIntegralInput",<||>]]];
  parts = RGZLoopSplitTerm[#,loop,ext]& /@
    RGZLoopTerms[Expand[FCI[expr]]];
  bad = Select[parts,FailureQ];
  If[bad =!= {},Return[First[bad]]];
  If[!RGZLoopValidValueQ[parts],Return[Failure["InvalidAASplitResult",<||>]]];
  FeynAmpDenominatorCombine[Total[parts]]
];

RGZLoopTID[expr_, loop_:q, ext_:p] := Module[{split,parts,result,values},
  If[!RGZLoopValidValueQ[expr],Return[Failure["InvalidAAIntegralInput",<||>]]];
  split = RGZLoopSplit[expr,loop,ext];
  If[!RGZLoopValidValueQ[split],Return[Failure["InvalidAASplitResult",<||>]]];
  parts = RGZLoopTerms[Expand[split]];
  values = Map[
    Function[t,If[FreeQ[t,_FeynAmpDenominator],0,
      TID[t,loop,ToPaVe->True,UsePaVeBasis->True,ApartFF->False]]],
    parts];
  If[!RGZLoopValidValueQ[values],Return[Failure["InvalidAAIntegralResult",<||>]]];
  result=Total[values];
  If[!RGZLoopValidValueQ[result] || !FreeQ[result,_TID|_FeynAmpDenominator],
    Return[Failure["IncompleteTID",<|"Output"->result|>]]];
  result=Factor2[Contract[result]];
  If[RGZLoopValidValueQ[result],result,Failure["InvalidAAIntegralResult",<||>]]
];

(* Gauge dependence is hidden in longitudinal pole masses as well.
   A nonzero un-simplified expression is NOT automatically a proof of
   dependence. Values a1,a2 should avoid a singular/degenerate pole limit. *)
RGZGaugeDifference[expr_,{a1_,a2_}] := Block[{$Assumptions=True},Module[{physical,diff},
  With[{check=RGZLoopGaugeValidate[{expr,a1,a2}]},If[FailureQ[check],Return[check]]];
  physical = RGZLoopPoleMasses[expr];
  If[!RGZLoopValidValueQ[physical],Return[Failure["InvalidGaugeExpression",<||>]]];
  diff = FullSimplify[(physical /.  \[Alpha]->a1)-
    (physical /. \[Alpha]->a2)];
  If[!RGZLoopValidValueQ[diff],Return[Failure["InvalidGaugeDifference",<||>]]];
  <|"ParameterValues"->{a1,a2},"Difference"->diff,
    "Status"->If[TrueQ[diff === 0],"The difference is zero at these two parameter points",
      "The difference is not zero; check missing terms, conventions and integral identities"]|>
]];


RGZLoopUnresolved[expr_] := DeleteDuplicates[Cases[expr,
  a_ /; MemberQ[{"GenericCoupling","FeynAmp","FAScalarProduct",
    "FAMetricTensor","FAFourVector","FAPropagatorDenominator"},
      RGZLoopName[Head[a]]] || RGZLoopName[Head[Head[a]]] == "G",
  Infinity]];

