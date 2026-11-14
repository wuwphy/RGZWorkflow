(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Role:
   Split routed quadratic propagators and verify exact pole reconstruction.

   Inputs:
   A rational loop expression and its loop/external momenta.

   Dependencies:
   Algebra/LoopPrimitives.wl provides physical pole conventions;
   Internal/MixedRuntime.wl provides progress callbacks. No source files are loaded here.
*)

ClearAll[RGZMXSplit, RGZMXSplitFactor, RGZMXSplitTerm];

(* A saved decomposition is a general algebraic identity. Never use the
   notebook's conditional mass assumptions to simplify its coefficients. *)
RGZMXSplitFactor[den_, var_, k_] := Block[{$Assumptions=True},Module[
  {roots, r, mass, order, quot, regular, coeff, out=0, back=0, degree, poleNo=0,
    emitted,probe=Unique["rgzPoleProbe$"]},
  roots = Join[({#^2,#}& /@ RGZWorkflow`Private`quarkMasses[]),{{0,0},{\[Mu]^2,\[Mu]},
    {-I Sqrt[\[Alpha] M^4],rgzLoopM1},
    {I Sqrt[\[Alpha] M^4],rgzLoopM2},
    {(m^2+\[Mu]^2)/2+I Sqrt[mG^4-(m^2-\[Mu]^2)^2/4],rgzLoopM3},
    {(m^2+\[Mu]^2)/2-I Sqrt[mG^4-(m^2-\[Mu]^2)^2/4],rgzLoopM4}}];
  RGZMXSplitStep["Deduplicate candidate poles (FullSimplify)",<|"Denominator"->den,"LoopMomentum"->k|>];
  roots = DeleteDuplicates[roots,
    TrueQ[FullSimplify[#1[[1]]-#2[[1]]] === 0]&];
  If[!PolynomialQ[den,var],
    Return[Failure["NonPolynomial",<|"Denominator"->den|>]]];
  RGZMXSplitStep["ReadDenominatorCount",<|"Denominator"->den,"LoopMomentum"->k|>];
  degree = Exponent[den,var];
  Do[
    r = pole[[1]]; mass = pole[[2]];poleNo++;
    RGZMXSplitStep["VerifyPole "<>ToString[poleNo]<>" / "<>ToString[Length[roots]]<>"（FullSimplify）",
      <|"Denominator"->den,"LoopMomentum"->k,"Pole"->r|>];
    If[TrueQ[FullSimplify[den /. var->r] === 0],
      order = 1;
      While[order < degree &&
        (RGZMXSplitStep["CheckRepeatedPole: derivative CountOrderCount "<>ToString[order]<>"（FullSimplify）",
          <|"Denominator"->den,"LoopMomentum"->k,"Pole"->r|>];
         TrueQ[FullSimplify[D[den,{var,order}] /. var->r] === 0]),order++];
      RGZMXSplitStep["Compute pole cofactors (PolynomialQuotient / FullSimplify)",
        <|"Denominator"->den,"LoopMomentum"->k,"Pole"->r,"RepeatedPoleOrder"->order|>];
      quot = FullSimplify[PolynomialQuotient[den,(var-r)^order,var]];
      regular = 1/quot;
      Do[
        RGZMXSplitStep["ComputefractionCoefficient "<>ToString[j]<>" / "<>ToString[order]<>"（FullSimplify）",
          <|"Denominator"->den,"LoopMomentum"->k,"Pole"->r,"RepeatedPoleOrder"->order|>];
        coeff = FullSimplify[
          (D[regular,{var,order-j}] /. var->r)/Factorial[order-j]];
        back += coeff/(var-r)^j;
        out += coeff ChangeDimension[FCI[FAD @@ ConstantArray[{k,mass},j]],D],
        {j,1,order}]
    ], {pole,roots}];
  RGZMXSplitStep["Verify decomposition identity (Together / FullSimplify)",<|"Denominator"->den,"LoopMomentum"->k|>];
  If[!TrueQ[FullSimplify[Together[1/den-back]] === 0],
    Return[Failure["UnknownPole",<|"Denominator"->den,
      "Remainder"->Together[1/den-back]|>]]];
  (* Check the actual FAD output, including the general pole-label map.
     Checking only back would miss inconsistent emitted mass labels. *)
  (* Rename the single route BEFORE expansion: k=q+-p otherwise expands
     into several scalar products and no longer forms one replaceable node. *)
  With[{route=MomentumExpand[FCI[Momentum[k,D]]]},
    emitted=out /. HoldPattern[PropagatorDenominator[route,label_]] :>
      PropagatorDenominator[Momentum[probe,D],label]];
  emitted=FeynAmpDenominatorExplicit[RGZLoopPoleMasses[emitted]] /.
    Pair[Momentum[probe,D],Momentum[probe,D]]->var;
  If[!TrueQ[FullSimplify[Together[1/den-emitted]]===0],
    Return[Failure["PoleLabelReconstruction",<|"Denominator"->den,
      "Remainder"->Together[1/den-emitted]|>]]];
  out
]];

RGZMXSplitTerm[expr_, loop_:q, ext_:p] := Module[
  {x,z,u,v,r,num,den,factors,out,f,n,poly,k,split,backRules,route,
    groups={1,1,1},routing={loop,loop-ext,loop+ext},j,stop},Catch[
  RGZMXSplitStep["Normalize parameters, make propagators explicit and expand scalar products"];
  r = ExpandScalarProduct[
    FeynAmpDenominatorExplicit[ChangeDimension[FCI[RGZLoopNormalize[expr]],D]]];
  RGZMXSplitStep["Replace scalar products with q^2, p.q and p^2"];
  r = r /. {
    HoldPattern[Pair[Momentum[loop,D],Momentum[loop,D]]] -> x,
    HoldPattern[Pair[Momentum[ext,D],Momentum[loop,D]]] -> z,
    HoldPattern[Pair[Momentum[loop,D],Momentum[ext,D]]] -> z,
    HoldPattern[Pair[Momentum[ext,D],Momentum[ext,D]]] -> u};
  RGZMXSplitStep["Combine the rational term (Together)"];
  r = Together[r]; num = Numerator[r]; den = Denominator[r];
  If[!PolynomialQ[den,{x,z}],
    Throw[Failure["NonRationalDenominator",<|"Expression"->r|>],stop]];
  RGZMXSplitStep["Factor the denominator polynomial (FactorList)",<|"Denominator"->den|>];
  factors = FactorList[den]; out = num/factors[[1,1]];
  RGZMXSplitStep["identify and Combine q, q-p, q+p OfDenominator",<|"Denominator"->den|>];
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
    rgzMXSplitRoute=j;k=routing[[j]];
    RGZMXSplitStep["Expand denominators with the same momentum routing",<|"LoopMomentum"->k|>];
    split=RGZMXSplitFactor[Expand[groups[[j]]],v,k];
    If[FailureQ[split],Throw[split,stop]];
    out *= split],{j,3}];
  rgzMXSplitRoute=0;RGZMXSplitStep["Restore FeynCalc ScalarProductAndPropagatorFormat"];
  backRules = {x->SPD[loop,loop],z->SPD[ext,loop],u->SPD[ext,ext]};
  FCI[out /. backRules],stop]
];

RGZMXSplit[expr_,loop_:q,ext_:p] := Module[{terms,parts,bad,i},
  Block[{rgzMXSplitRoute=0},
    RGZMXProgressUpdate["TermID"->0,"TotalTermCount"->0,"CompleteTermCount"->0];
    RGZMXSplitStep["Expand projected expressions and count terms to decompose (Expand / FCI)"];
    terms=RGZLoopTerms[Expand[FCI[expr]]];
    RGZMXProgressUpdate["TotalTermCount"->Length[terms]];
    parts=Table[
      rgzMXSplitRoute=0;
      RGZMXProgressUpdate["TermID"->i,"CompleteTermCount"->i-1];
      RGZMXSplitStep["Prepare propagator decomposition for the current term"];
      With[{z=RGZMXSplitTerm[terms[[i]],loop,ext]},
        RGZMXProgressUpdate["CompleteTermCount"->i];z],{i,Length[terms]}];
    bad=Select[parts,FailureQ];If[bad=!={},Return[First[bad]]];
    rgzMXSplitRoute=0;
    RGZMXSplitStep["Combine decomposed propagators (FeynAmpDenominatorCombine)"];
    FeynAmpDenominatorCombine[Total[parts]]]
];
