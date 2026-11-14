(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Role:
   Provide shared AA external-leg weights, SU(N) normalization and scalar PV grouping.

   Inputs:
   Ordered 5x5/8x8 self-energy matrices or a linear scalar-PV expression.

   Dependencies:
   OneLoop/MixedSelfEnergy.wl supplies the field basis and scalar-PV predicates; LoopPrimitives.wl supplies normalization. Shared by QQ and PT.
*)


(* Gauge symbols are reserved parameters, not arbitrary scalar constants.
   Reject foreign contexts explicitly rather than silently identifying two
   distinct symbols. Check values before any equal-branch cancellation. *)
RGZGaugeValidate[x_] := Module[{foreign},
  If[OwnValues[\[Alpha]]=!={},Return[Failure["AssignedGaugeParameter",<||>]]];
  If[!RGZMXValidValueQ[x],Return[Failure["InvalidGaugeExpression",<|
    "Description"->"A gauge check requires finite, fully evaluated expressions, including every nested value."|>]]];
  foreign=DeleteDuplicates[Cases[x,s_Symbol /;
    MemberQ[{"\[Alpha]","alpha"},SymbolName[Unevaluated[s]]] &&
    !SameQ[Unevaluated[s],\[Alpha]],{0,Infinity},Heads->True]];
  foreign=Join[foreign,Cases[x,h_[name_String] /;
    RGZLoopName[h]==="FCGV" && MemberQ[{"\[Alpha]","alpha"},name],{0,Infinity}]];
  If[foreign=!={},Return[Failure["GaugeSymbolContext",<|"Symbols"->foreign,
    "ExpectedContext"->Context[\[Alpha]],
    "Action"->"Normalize the gauge parameter to the unassigned Engine alpha before comparing."|>]]];True];

(* The external A row/column of the tree propagators. In the T basis,
   DT[A,i]=dAA u_i. In the L basis the left/right signs differ: no
   transpose/Hermitian-conjugation shortcut is used. *)
RGZGaugeStart[data_Association] := Module[
  {piT,piL,s0,k,h,delta,den,u,dAA,rowL,colL,termsT,termsL},
  If[!And@@(KeyExistsQ[data,#]&/@{"T","L","TFieldOrder","LFieldOrder"}),
    Return[Failure["MissingMixedMatrices",<||>]]];
  If[data["TFieldOrder"]=!=RGZMixBasis["T"] ||
      data["LFieldOrder"]=!=RGZMixBasis["L"],
    Return[Failure["MixedBasisMismatch",<|
      "Description"->"Use the checked kernel field order and normalized adjoint color basis."|>]]];
  piT=data["T"];piL=data["L"];
  With[{check=RGZGaugeValidate[{piT,piL}]},If[FailureQ[check],Return[check]]];
  If[Dimensions[piT]=!={5,5} || Dimensions[piL]=!={8,8} ||
      !FreeQ[{piT,piL},_Failure|_Missing|$Failed|$Aborted],
    Return[Failure["IncompleteMixedMatrices",<||>]]];
  If[OwnValues[\[Alpha]]=!={},Return[Failure["AssignedGaugeParameter",
    <|"Description"->"Keep Alpha symbolic; a fixed-gauge result cannot establish gauge independence."|>]]];
  s0=SPD[p,p];k=s0-\[Mu]^2;h=mG^2/2;
  delta=(s0-m^2)(s0-\[Mu]^2)+mG^4;
  u={1,-h/k,-h/k,h/k,h/k};dAA=-I k/delta;
  den=s0^2+\[Alpha] M^4;
  rowL={-I \[Alpha] s0/den,-s0 Sqrt[s0]/den,
    -\[Alpha] M^4/(Sqrt[s0] den),-\[Alpha] Sqrt[s0]/den,0,0,0,0};
  colL={-I \[Alpha] s0/den,s0 Sqrt[s0]/den,
    \[Alpha] M^4/(Sqrt[s0] den),\[Alpha] Sqrt[s0]/den,0,0,0,0};
  (* Use every ordered pair once; graph symmetry factors are already in Pi. *)
  termsT=Flatten[Table[u[[i]] piT[[i,j]] u[[j]],{i,5},{j,5}]];
  termsL=Flatten[Table[rowL[[i]] piL[[i,j]] colL[[j]],{i,4},{j,4}]];
  <|"TFieldOrder"->data["TFieldOrder"],"LFieldOrder"->data["LFieldOrder"],
    "TreeTransverseAA"->dAA,"TransverseExternalLegsweight"->u,
    "LongitudinalLeftExternalLegs"->rowL,"LongitudinalRightExternalLegs"->colL,
    "SelfEnergyTParts"->termsT,"SelfEnergyT"->Total[termsT],
    "AttachedT"->dAA^2 Total[termsT],"AttachedL"->Total[termsL],
    "Description"->"SelfEnergyT=u.PiT.u; attached T/L=(D0.Pi.D0)AA; pinch terms are excluded."|>
];
RGZGaugeStart[x_] := Failure["RestoreMixedResultFirst",<||>];

(* Candidate kernels contain colour Casimirs rather than external deltas.
   Mixed matrices already have their external adjoint delta stripped.
   Use the same SU(N) invariants, not a new numeric Nc substitution. *)
(* The model's bare N in mG^2/Sqrt[N] is the same colour
   dimension as SUNN. Do not replace the head of numerical N[...]. *)
RGZLoopColour[x_] := Replace[RGZLoopNormalize[x],
  HoldPattern[System`N]->SUNN,{0,Infinity},Heads->False] /.
  {CA->SUNN,CF->(SUNN^2-1)/(2 SUNN)};

(* Scalar PV objects are opaque while coefficients are manipulated.
   Bubble symmetry is exact; general C0/D0 permutations are NOT guessed. *)
RGZGaugePVFreeQ[x_] := FreeQ[x,
  _PaVe|_GenPaVe|_A0|_A00|_B0|_B1|_B00|_B11|_C0|_D0];
RGZGaugeCanonicalPV[x_] := (x /. {
  HoldPattern[PaVe[0,{}, {a_},opts___Rule]] :> A0[a],
  HoldPattern[PaVe[0,{v_},{a_,b_},opts___Rule]] :> B0[v,a,b]
}) /. {
  HoldPattern[B0[v_,a_,b_]] :> With[{ab=Sort[{a,b}]},B0[v,ab[[1]],ab[[2]]]]
};

RGZGaugePVPairs[expr_] := Module[{walk,group,multiply,pairs,stop},Catch[
  With[{check=RGZGaugeValidate[expr]},If[FailureQ[check],Throw[check,stop]]];
  group[ps_List] := Select[
    ({#[[1,1]],Total[#[[All,2]]]}& /@ GatherBy[ps,First]),
    !TrueQ[#[[2]]===0]&];
  multiply[left_List,right_List] := Flatten[Table[
    If[a[[1]]=!=1 && b[[1]]=!=1,
      Throw[Failure["NonlinearOneLoopPV",<||>],stop]];
    {a[[1]] b[[1]],a[[2]] b[[2]]},{a,left},{b,right}],1];
  walk[x_] := Which[
    RGZGaugePVFreeQ[x],{{1,x}},
    RGZMXPVObjectQ[x],
      If[!RGZMXPVScalarQ[x],Throw[Failure["TensorPVRemaining",<||>],stop]];
      {{RGZGaugeCanonicalPV[x],1}},
    Head[x]===Plus,Flatten[walk/@(List@@x),1],
    Head[x]===Times,Fold[multiply,{{1,1}},walk/@(List@@x)],
    True,Throw[Failure["UnsupportedPVStructure",
      <|"head"->RGZLoopName[Head[x]]|>],stop]];
  pairs=walk[expr];group[pairs],stop]
];



(* Index/color audits and projectors used by the mixed graph engine. *)
ClearAll[RGZMXAdjointDimension, RGZMXAuditSums, RGZMXBoundColourCount, RGZMXColourQuartet, RGZMXColourReduce, RGZMXColourSign, RGZMXColourTensorQ, RGZMXExternalSumQ, RGZMXFourColourQ, RGZMXIndexKind, RGZMXIndexOccurrences, RGZMXNormalizeColourSums, RGZMXObjects, RGZMXPrepareIndices, RGZMXProject, RGZMXResolveSpectatorSums, RGZMXSpectatorDeltaQ, RGZMXSpectatorIndexQ, RGZMXSumDimension, RGZMXTerms, RGZMixAdjoint, RGZMixAdjointColour, RGZMixColourFreeQ, RGZMixColourPairs, RGZMixProjectPair];

RGZMXObjects[x_,name_] := DeleteDuplicates@Cases[x,
  h_[___] /; RGZLoopName[h]===name,{0,Infinity}];

RGZMXTerms[x_] := If[Head[x]===Plus,List@@x,{x}];

(* External sum annotations describe external-state sums for squared
   amplitudes. Here external colour indices remain OPEN, so removing
   this annotation neither sums the index nor multiplies its range. *)
RGZMXExternalSumQ[sum_] := Length[sum]>=3 &&
  AnyTrue[Drop[List@@sum,2],RGZLoopName[#]==="External"&];

RGZMXIndexOccurrences[expr_,idx_] := Which[
  SameQ[expr,idx],1,
  AtomQ[expr],0,
  Head[expr]===Power && IntegerQ[expr[[2]]] && expr[[2]]>0,
    expr[[2]] RGZMXIndexOccurrences[expr[[1]],idx],
  True,Total[RGZMXIndexOccurrences[#,idx]&/@(List@@expr)]
];

RGZMXIndexKind[idx_] := Which[
  MemberQ[{"LorentzIndex","ExplicitLorentzIndex"},
    RGZLoopName[Head[idx]]],"Lorentz",
  RGZLoopName[Head[idx]]==="Index" && Length[idx]>0,
    RGZLoopName[idx[[1]]],
  RGZLoopName[Head[idx]]==="SUNIndex","Adjoint",
  True,"Unknown"];

(* The second adjoint label is a spectator at A-auxiliary vertices.
   FeynArts can leave either delta_ij Sum_i Sum_j or just Sum_i after
   its delta contractions. Evaluate those explicit sums using THEIR
   declared ranges; never drop the resulting colour multiplicity.
   Restrict this step to identifiable Gribov labels, not Lorentz labels. *)
RGZMXSpectatorIndexQ[idx_] := Which[
  RGZLoopName[Head[idx]]==="SUNIndex" && Length[idx]===1,
    StringMatchQ[RGZLoopName[idx[[1]]],"rgzGribov"~~__],
  RGZLoopName[Head[idx]]==="Index" && Length[idx]===2,
    RGZLoopName[idx[[1]]]==="Gribov",
  True,False];

RGZMXSpectatorDeltaQ[d_] :=
  MemberQ[{"IndexDelta","SUNDelta"},RGZLoopName[Head[d]]] &&
  Length[d]===2 && And@@(RGZMXSpectatorIndexQ/@(List@@d));

RGZMXSumDimension[r_] := Which[
  IntegerQ[r] && r>0,r,
  ListQ[r] && r=!={},Length[DeleteDuplicates[r]],
  True,Failure["UnknownExplicitSumRange",<|"Scope"->r|>]];

RGZMXResolveSpectatorSums[input_,inputSums_List] := Module[
  {body=input,sums=inputSums,audit={},progress=True,factors,deltas,delta,
    i,j,si,sj,di,dj,internal},
  (* A Kronecker delta is idempotent; this also handles delta_ij^2. *)
  While[progress,
    progress=False;
    body=body /. (HoldPattern[Power[d_,n_Integer]] /;
      n>1 && RGZMXSpectatorDeltaQ[d] :> d);
    factors=If[Head[body]===Times,List@@body,{body}];
    deltas=Select[factors,RGZMXSpectatorDeltaQ];
    Do[
      {i,j}=List@@delta;
      si=Select[sums,SameQ[#[[1]],i]&];
      sj=Select[sums,SameQ[#[[1]],j]&];
      If[si==={} || sj==={},Continue[]];
      di=RGZMXSumDimension[si[[1,2]]];
      dj=RGZMXSumDimension[sj[[1,2]]];
      If[FailureQ[di] || FailureQ[dj] || si[[1,2]]=!=sj[[1,2]],Continue[]];
      If[SameQ[i,j],
        body=Times@@Select[factors,!SameQ[#,delta]&];
        AppendTo[audit,<|"Tensor"->delta,"process "->"Kronecker diagonal entries are 1; retain explicit sums"|>];
        progress=True;Break[]];
      internal=Select[{i,j},Function[idx,
        AnyTrue[sums,SameQ[#[[1]],idx] && !RGZMXExternalSumQ[#]&]]];
      If[internal==={},Continue[]];
      If[SameQ[First[internal],j],{i,j}={j,i}];
      body=(Times@@Select[factors,!SameQ[#,delta]&]) /. (i->j);
      sums=Select[sums,!SameQ[#[[1]],i]&];
      AppendTo[audit,<|"Tensor"->delta,"process "->"Explicit Kronecker sum: eliminate one index and its SumOver",
        "eliminate Index"->i,"RetainIndex"->j,"Scope"->si[[1,2]]|>];
      progress=True;Break[],{delta,deltas}]
  ];
  <|"Expression"->body,"Sum"->sums,"SumAudit"->audit|>
];

(* FeynArts SUNF[a,b,c,d] means SUM_e f[a,b,e] f[e,c,d].
   Expand this shorthand with a fresh typed dummy, then inspect explicit
   IndexSum binders from inside out. Only a full declared adjoint range
   with exactly two colour occurrences per additive term is converted
   to Einstein notation. Partial/unknown ranges or non-colour dependence
   remain failures. No external indices or group factors are discarded. *)
RGZMXFourColourQ[h_,args_List] := Length[args]===4 &&
  FreeQ[args,_Rule|_RuleDelayed] &&
  (RGZLoopName[h]==="FASUNF" ||
    (RGZLoopName[h]==="SUNF" && StringStartsQ[Context[h],"FeynArts`"]));

RGZMXColourTensorQ[h_,args_List] :=
  FreeQ[args,_Rule|_RuleDelayed] &&
  ((MemberQ[{"FASUNF","SUNF","FASUND","SUND"},RGZLoopName[h]] &&
      Length[args]===3) ||
   (MemberQ[{"IndexDelta","SUNDelta"},RGZLoopName[h]] && Length[args]===2));

(* Result {colour occurrences, other occurrences}; count integer powers
   with their multiplicity rather than counting distinct subexpressions. *)
RGZMXBoundColourCount[x_,v_] := Which[
  SameQ[x,v],{0,1},
  AtomQ[x],{0,0},
  Head[x]===Power && IntegerQ[x[[2]]] && x[[2]]>0,
    x[[2]] RGZMXBoundColourCount[x[[1]],v],
  RGZMXColourTensorQ[Head[x],List@@x],
    Total[If[SameQ[#,v],{1,0},RGZMXBoundColourCount[#,v]]& /@ (List@@x)],
  MemberQ[{Times,Plus},Head[x]],
    Total[RGZMXBoundColourCount[#,v]& /@ (List@@x)],
  True,{0,RGZMXIndexOccurrences[x,v]}
];

(* Read the loaded model, not the symbolic colour factor N. In W2.6
   IndexRange[Index[Gluon]]=NoUnfold[Range[8]]. Never assume SU(3) here. *)
RGZMXAdjointDimension[] := Module[{heads,defs,ranges,dims},
  (* Construct qualified names: Names can shorten symbols on ContextPath. *)
  heads=ToExpression /@ Flatten[Function[c,
    If[Names[c<>"IndexRange"]==={}, {}, {c<>"IndexRange"}]] /@ Contexts[]];
  defs=Flatten[DownValues /@ heads];
  ranges=Cases[defs,HoldPattern[RuleDelayed[l_,r_]] /;
    !FreeQ[l,h_[t_] /; RGZLoopName[h]==="Index" &&
      RGZLoopName[t]==="Gluon"] :> r,{1}];
  ranges=ranges /. (h_[lst_List] /; RGZLoopName[h]==="NoUnfold" :> lst);
  dims=DeleteDuplicates@Cases[ranges,lst_List /;
    Length[lst]>0 && SameQ[lst,Range[Length[lst]]] :> Length[lst],{1}];
  If[Length[dims]===1,First[dims],Failure["UnknownAdjointRange",<|
    "Description"->"Cannot uniquely determine IndexRange[Index[Gluon]] for the current model.",
    "candidateScope"->ranges|>]]
];

RGZMXNormalizeColourSums[input_] := Module[
  {walk,resolve,stop=Unique["rgzMixColourStop$"],audit={},result,
    adjointDim=Missing["NotRead"]},
  resolve[args_List] := Module[{body,it,v,dim,dummy,terms,counts,out},
    If[Length[args]=!=2,Throw[Failure["ExplicitIndexSum",<|
      "Description"->"This IndexSum iterator structure is unsupported.","Objects"->{args}|>],stop]];
    body=args[[1]];it=args[[2]];
    dim=Which[
      MatchQ[it,{_Symbol,1,_Integer}] && it[[3]]>0,it[[3]],
      MatchQ[it,{_Symbol,_List}] && Length[it[[2]]]>0 &&
        SameQ[it[[2]],Range[Length[it[[2]]]]],Length[it[[2]]],
      True,Throw[Failure["ExplicitIndexSum",<|
        "Description"->"The complete adjoint color-sum range is unconfirmed.","Objects"->{args}|>],stop]];
    v=First[it];terms=RGZMXTerms[Expand[body]];
    counts=RGZMXBoundColourCount[#,v]& /@ terms;
    If[!And@@(MemberQ[{{2,0},{0,0}},#]& /@ counts),
      Throw[Failure["ExplicitIndexSum",<|
        "Description"->"Summed indices do not occur in pairs exclusively within color tensors in every term.",
        "Scope"->it,"Occurrences (Color, other)"->counts,
        "Objects"->{args}|>],stop]];
    (* A completely index-independent finite sum is literal multiplicity. *)
    If[MemberQ[counts,{2,0}],
      If[Head[adjointDim]===Missing,adjointDim=RGZMXAdjointDimension[]];
      If[FailureQ[adjointDim] || dim=!=adjointDim,
        Throw[Failure["ExplicitIndexSum",<|
          "Description"->"The sum range differs from the complete adjoint color range of the current model.",
          "Scope"->it,"ModelDimension"->adjointDim,"Objects"->{args}|>],stop]]];
    dummy=SUNIndex[Unique["rgzMixSum$"]];
    out=MapThread[If[#2==={0,0},dim #1,#1 /. (v->dummy)]&,{terms,counts}];
    AppendTo[audit,<|"Sum"->it,
      "process "->"Convert complete color sums to paired dummy indices; retain range multipliers for independent terms",
      "ScopeDimension"->dim,"DummyIndex"->dummy|>];
    Total[out]
  ];
  walk[x_?AtomQ] := x;
  walk[x_] := Module[{h=Head[x],args=walk /@ (List@@x),d},
    Which[
      RGZLoopName[h]==="IndexSum",resolve[args],
      RGZMXFourColourQ[h,args],
        d=SUNIndex[Unique["rgzMixF4$"]];
        AppendTo[audit,<|"Colorabbreviation"->Apply[h,args],
          "process "->"Expand four-argument SUNF into two three-argument structure constants","DummyIndex"->d|>];
        h[args[[1]],args[[2]],d] h[d,args[[3]],args[[4]]],
      True,Apply[h,args]]
  ];
  result=Catch[walk[input],stop];
  If[FailureQ[result],result,<|"Expression"->result,"SumAudit"->audit|>]
];

(* Inspect sums BEFORE Contract/SUNSimplify can erase their indices.
   Drop only sums represented by Einstein contractions. The special
   scalar c/eta case has unused Lorentz labels because this custom generic
   model assigns KinematicIndices[U]={Lorentz} to all U classes.
   No factor D is assigned to an artificial scalar-ghost label.
   An index-independent spectator sum multiplies its explicit range.
   In the supplied SU(3) model this range is 8. Other missing sums
   still fail the audit; no general-N replacement is guessed. *)
RGZMXAuditSums[expr_,scalarC_:False] := Module[
  {terms,out={},audit={},sums,body,idx,kind,n,bad={},t,record,dim,multiplicity,
    normalized},
  normalized=RGZMXNormalizeColourSums[expr];
  If[FailureQ[normalized],Return[normalized]];
  audit=normalized["SumAudit"];
  terms=RGZMXTerms[Expand[normalized["Expression"]]];
  Do[
    sums=RGZMXObjects[t,"SumOver"];
    body=t /. (h_[___] /; RGZLoopName[h]==="SumOver" :> 1);
    record=RGZMXResolveSpectatorSums[body,sums];
    body=record["Expression"];sums=record["Sum"];
    audit=Join[audit,record["SumAudit"]];multiplicity=1;
    Do[
      idx=sum[[1]];kind=RGZMXIndexKind[idx];
      n=RGZMXIndexOccurrences[body,idx];
      Which[
        RGZMXExternalSumQ[sum],
          AppendTo[audit,<|"Sum"->sum,
            "process "->"Remove external-leg sum markers; keep open indices without multiplying by the sum range"|>],
        MemberQ[{"Lorentz","Adjoint","Gluon","Gribov","Colour"},kind] && n>=2,
          AppendTo[audit,<|"Sum"->sum,"process "->"Einstein summation in explicit tensors"|>],
        scalarC && kind==="Lorentz" && n===0,
          AppendTo[audit,<|"Sum"->sum,"process "->"Scalar c Ofmanual Lorentz Tag"|>],
        RGZMXSpectatorIndexQ[idx] && n===0,
          dim=RGZMXSumDimension[sum[[2]]];
          If[FailureQ[dim],AppendTo[bad,<|"Sum"->sum,"Cause"->dim|>],
            multiplicity=multiplicity dim;
            AppendTo[audit,<|"Sum"->sum,"process "->"Explicit color sum independent of the index",
              "Multiplier"->dim|>]],
        True,AppendTo[bad,<|"Sum"->sum,"Type"->kind,
          "TensorInOccurrences"->n,"ContainingTerm"->t|>]
      ],{sum,sums}];
    AppendTo[out,multiplicity body],{t,terms}];
  If[bad=!={},Return[Failure["UnresolvedSum",<|
    "Description"->"Cannot determine the sum multiplier. Inspect Objects before integrating.",
    "Objects"->DeleteDuplicates[bad]|>]]];
  If[RGZMXObjects[normalized["Expression"],"IndexSum"]=!={},
    Return[Failure["ExplicitIndexSum",<|"Objects"->
      RGZMXObjects[normalized["Expression"],"IndexSum"]|>]]];
  <|"Expression"->Total[out],"SumAudit"->DeleteDuplicates[audit]|>
];

(* Some FA versions use Ext[1] or a1 instead of an integer index label. *)
RGZMXPrepareIndices[expr_] := expr /. (HoldPattern[h_[type_,label_]] /;
  RGZLoopName[h]==="Index" && MemberQ[{"Gluon","Gribov"},RGZLoopName[type]] :>
  Module[{y=label,n},
    If[!AtomQ[y] && RGZLoopName[Head[y]]==="Ext",y=First[y]];
    If[Head[y]===Symbol && StringMatchQ[RGZLoopName[y],"a"~~DigitCharacter..],
      y=FromDigits[StringDrop[RGZLoopName[y],1]]];
    n=If[IntegerQ[y],Symbol["RGZWorkflow`Engine`rgz"<>RGZLoopName[type]<>ToString[y]],y];
    SUNIndex[n]]);

(* Project while the denominator is still rational. At this stage
   q is NOT integrated: scalar coefficients can contain p.q and q^2.
   Extra q_mu tensors are included by the projectors; reconstruction
   into PT/PL is valid only AFTER loop integration. *)
(* A colour diamond can survive SUNSimplify in some FC versions:
   f[a,i,j] f[b,k,l] f[i,l,r] f[j,k,r] = -N^2 delta[a,b]/2.
   Use the identity only for this isolated, fully paired network.
   All argument permutations contribute their antisymmetry signs.
   An unpaired index or an index shared with another tensor is untouched. *)
RGZMXColourSign[actual_List,target_List] := Module[{order},
  order=Map[Function[idx,First@FirstPosition[target,idx]],actual];
  (order[[2]]-order[[1]]) (order[[3]]-order[[1]]) (order[[3]]-order[[2]])/2
];

RGZMXColourQuartet[term_] := Module[
  {factors,expanded,fs,rest,vertices,ids,open,inside,external,internal,
    fa,fb,fc,fd,a,b,i,j,k,l,r,partners,common,sign},
  factors=If[Head[term]===Times,List@@term,{term}];
  expanded=Flatten[Map[Function[x,If[Head[x]===Power &&
    Head[x[[1]]]===SUNF && IntegerQ[x[[2]]] && 1<x[[2]]<=4,
      ConstantArray[x[[1]],x[[2]]],{x}]],factors],1];
  fs=Select[expanded,Head[#]===SUNF&];
  If[Length[fs]=!=4 || !And@@(Length[#]===3&/@fs),Return[term]];
  rest=Times@@Select[expanded,Head[#]=!=SUNF&];
  vertices=List@@#&/@fs;ids=DeleteDuplicates[Flatten[vertices]];
  If[Length[ids]=!=7 || !And@@(MatchQ[#,_SUNIndex]&/@ids) ||
      AnyTrue[ids,IntegerQ[#[[1]]]&],Return[term]];
  open=Select[ids,Count[Flatten[vertices],#]===1&];
  inside=Select[ids,Count[Flatten[vertices],#]===2&];
  If[Length[open]=!=2 || Length[inside]=!=5 ||
      !And@@(FreeQ[rest,#]&/@inside),Return[term]];
  (* AA open indices must not also occur in the non-colour coefficient. *)
  If[!And@@(FreeQ[rest,#]&/@open),Return[term]];
  external=Select[vertices,Length[Intersection[#,open]]===1&];
  internal=Select[vertices,Intersection[#,open]==={}&];
  If[Length[external]=!=2 || Length[internal]=!=2 ||
      !And@@(Length[DeleteDuplicates[#]]===3&/@vertices),Return[term]];
  fa=external[[1]];fb=external[[2]];
  a=First[Intersection[fa,open]];b=First[Intersection[fb,open]];
  {i,j}=Select[fa,!MemberQ[open,#]&];
  partners=Select[internal,MemberQ[#,i]&];
  If[Length[partners]=!=1,Return[term]];
  fc=First[partners];fd=First[Select[internal,!SameQ[#,fc]&]];
  common=Intersection[fc,fd];If[Length[common]=!=1,Return[term]];
  r=First[common];partners=Intersection[fc,fb];
  If[Length[partners]=!=1,Return[term]];l=First[partners];
  partners=Select[fb,!MemberQ[{b,l},#]&];
  If[Length[partners]=!=1,Return[term]];k=First[partners];
  If[Sort[fc]=!=Sort[{i,l,r}] || Sort[fd]=!=Sort[{j,k,r}],Return[term]];
  sign=Times@@MapThread[RGZMXColourSign,
    {{fa,fb,fc,fd},{{a,i,j},{b,k,l},{i,l,r},{j,k,r}}}];
  -sign SUNN^2 SUNDelta[a,b] rest/2
];

RGZMXColourReduce[expr_] := Module[{z},
  z=SUNSimplify[expr,SUNNToCACF->False];
  If[!FreeQ[z,_SUNF],
    z=Total[RGZMXColourQuartet/@RGZMXTerms[Expand[z,_SUNF]]];
    z=SUNSimplify[z,SUNNToCACF->False]];
  z
];

(* First reduce Lorentz rank, but retain ALL external colour structures.
   VV: T PT + L PL; VS/SV: L p_mu; SS: L.
   Only the final adjoint-channel matrix uses p_mu/Sqrt[p^2]. *)
RGZMXProject[amp_,i_Integer,j_Integer] := Module[{z,t=0,l,bad},
  RGZMXProgressUpdate["Substep"->"Normalize parameters and project external tensors"];
  z=RGZLoopNormalize[amp];
  z=z /. (HoldPattern[h_[a_,b_]] /;
    RGZLoopName[h]==="IndexDelta" :> SUNDelta[a,b]);
  Which[
    RGZMXVectorQ[i] && RGZMXVectorQ[j],
      t=Contract[FCI[RGZLoopPT[p,\[Mu],\[Nu]] z]]/(D-1);
      l=Contract[FCI[FVD[p,\[Mu]] FVD[p,\[Nu]] z/SPD[p,p]]],
    RGZMXVectorQ[i] || RGZMXVectorQ[j],
      l=Contract[FCI[FVD[p,\[Mu]] z/SPD[p,p]]],
    True,l=Contract[FCI[z]]];
  RGZMXProgressUpdate["Substep"->"Contract internal colors and retain external color tensors"];
  t=RGZMXColourReduce[ExpandScalarProduct[t]];
  l=RGZMXColourReduce[ExpandScalarProduct[l]];
  bad=DeleteDuplicates@Cases[{t,l},_LorentzIndex|_ExplicitLorentzIndex,Infinity];
  If[bad=!={},Return[Failure["ProjectionNotScalar",<|"FieldPair"->{i,j},"Index"->bad|>]]];
  (* SUNF/SUND with external auxiliary colour indices are legitimate
     tensor structures, not failed AA colour reduction. Their coefficients
     factor out of integration. Never replace these structures by a delta. *)
  <|"T"->t,"L"->l|>
];

(* Colour projection onto the normalized antisymmetric adjoint channel.
   X_mu^a = f^{abc} X_mu^{bc}/Sqrt[N]. This is a channel projection;
   the orthogonal colour channels remain in the full tensor result. *)
(* Keep colour-independent subexpressions intact, including long PV sums
   and rational mass coefficients. Only sums/products OF colour structures
   are distributed. No Expand, Together or SUNSimplify acts on these scalar
   coefficients. The result is an exact list {colour, scalar coefficient}. *)
RGZMixColourFreeQ[x_] := FreeQ[x,
  _SUNIndex|_SUNFIndex|_SUNF|_SUND|_SUNDelta|_SUNFDelta|_SUNTrace|_SUNT|_SUNTF];

RGZMixColourPairs[expr_] := Module[{walk,multiply,group,pairs,stop},Catch[
  group[ps_List] := Select[
    ({#[[1,1]],Total[#[[All,2]]]}& /@ GatherBy[ps,First]),
    !TrueQ[#[[2]]===0]&];
  multiply[left_List,right_List] := group[Flatten[
    Table[{a[[1]] b[[1]],a[[2]] b[[2]]},{a,left},{b,right}],1]];
  walk[x_] := Which[
    RGZMixColourFreeQ[x],{{1,x}},
    MemberQ[{"SUNF","SUND","SUNDelta","SUNFDelta","SUNTrace","SUNT","SUNTF"},
      RGZLoopName[Head[x]]],{{x,1}},
    Head[x]===Plus,group[Flatten[walk/@(List@@x),1]],
    Head[x]===Times,Fold[multiply,{{1,1}},walk/@(List@@x)],
    Head[x]===Power && IntegerQ[x[[2]]] && x[[2]]>=0,
      With[{base=walk[x[[1]]]},Nest[multiply[#,base]&,{{1,1}},x[[2]]]],
    True,Throw[Failure["ColourSeparationHead",
      <|"head"->RGZLoopName[Head[x]],
        "Description"->"Color indices occur in an unsupported algebraic structure; projection stopped."|>],stop]];
  pairs=walk[expr];group[pairs],stop]
];

(* Reduce ONE small colour kernel. The external-index map and the
   f/Sqrt[N] normalization are identical to the previous implementation. *)
RGZMixAdjointColour[colour_,i_,j_] := Module[
  {z=colour,factors=1,a1=SUNIndex[rgzMixAdj1],a2=SUNIndex[rgzMixAdj2],
    g1=SUNIndex[rgzGluon1],g2=SUNIndex[rgzGluon2],
    h1=SUNIndex[rgzGribov1],h2=SUNIndex[rgzGribov2],delta,c,bad},
  If[TrueQ[z===0],Return[0]];
  If[RGZMXAuxQ[i],factors*=SUNF[a1,g1,h1]/Sqrt[SUNN],z=z/.g1->a1];
  If[RGZMXAuxQ[j],factors*=SUNF[a2,g2,h2]/Sqrt[SUNN],z=z/.g2->a2];
  z=RGZMXColourReduce[z factors];
  bad=DeleteDuplicates@Join[RGZMXObjects[z,"SUNF"],
    RGZMXObjects[z,"SUND"],RGZMXObjects[z,"SUNTrace"],
    RGZMXObjects[z,"SUNT"],RGZMXObjects[z,"SUNTF"]];
  If[bad=!={},Return[Failure["AdjointColourNotReduced",
    <|"FieldPair"->{i,j},"Objects"->bad|>]]];
  delta=SUNDelta[a1,a2];z=z/.SUNDelta[a2,a1]->delta;
  (* Expand only this colour kernel, never the original integral coefficient.
     Remove the common delta only after checking exact reconstruction. *)
  c=Expand[z]/.delta->1;
  If[!TrueQ[Expand[z-delta c]===0],Return[Failure["AdjointDeltaMissing",
    <|"FieldPair"->{i,j},"ColorKernel"->z|>]]];
  If[!RGZMixColourFreeQ[c],Return[Failure["AdjointIndexRemaining",
    <|"FieldPair"->{i,j},"ColorKernel"->c|>]]];c
];

RGZMixAdjoint[expr_,i_,j_,seconds_:60] := Module[{r},
  r=CheckAbort[TimeConstrained[Module[{pairs,coefficients,c,n,stop},Catch[
    If[TrueQ[expr===0],Return[0]];
    RGZMixResultStep["Separate color structures from scalar integral coefficients",{i,j}];
    pairs=RGZMixColourPairs[expr];If[FailureQ[pairs],Throw[pairs,stop]];
    coefficients=Table[
      RGZMixResultStep["ColorStructure "<>ToString[n]<>" / "<>
        ToString[Length[pairs]]<>": SUNSimplify (only ColorKernel)",{i,j}];
      c=RGZMixAdjointColour[pairs[[n,1]],i,j];
      If[FailureQ[c],Throw[c,stop]];c,{n,Length[pairs]}];
    RGZMixResultStep["Restore scalar coefficients without expanding integral expressions",{i,j}];
    Total[MapThread[Times,{coefficients,pairs[[All,2]]}]],stop]],
    seconds,$Aborted],$Aborted];
  If[r===$Aborted,Failure["AdjointProjectionTimeout",
    <|"FieldPair"->{i,j},"TimeLimit"->seconds|>],r]
];

RGZMixProjectPair[data_,pair_List,seconds_:60] := Module[{t,l},
  RGZMixResultStep["Project T coefficients for the current field pair",pair];
  t=Block[{rgzMixProjectionComponent="T"},
    RGZMixAdjoint[data["T"],Sequence@@pair,seconds]];If[FailureQ[t],Return[t]];
  RGZMixResultStep["Project L coefficients for the current field pair",pair];
  l=Block[{rgzMixProjectionComponent="L"},
    RGZMixAdjoint[data["L"],Sequence@@pair,seconds]];If[FailureQ[l],Return[l]];
  If[Xor@@(RGZMXVectorQ/@pair),l=Sqrt[SPD[p,p]] l];
  <|"T"->t,"L"->l|>
];
