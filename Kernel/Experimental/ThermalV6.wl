(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Optional original V6 route-restricted thermal definitions. No examples run on load. *)
(* =========================================================*)
(*1. RGZ pole labels*)
(* =========================================================*)
ClearAll[massSqByLabel,poleLabelFromMass];

massSqByLabel["0"]=0;
massSqByLabel["L1"]=m1^2;
massSqByLabel["L2"]=m2^2;
massSqByLabel["T3"]=m3^2;
massSqByLabel["T4"]=m4^2;

poleLabelFromMass[mass_]:=Which[mass===0,"0",mass===m1,"L1",mass===m2,"L2",mass===m3,"T3",mass===m4,"T4",True,Print["UnknownMassParameter: ",mass];
Abort[]];
(* =========================================================*)
(*V6:route-restricted numerator matching*)
(* =========================================================*)ClearAll[toTerms,ExpandMomentumHead,NormalizeFADInputV6,poleLabelFromMass,RouteKeyV6,MomentumRouteV6,PropRouteV6,LoopPropDataV6,LoopDenInfoV6,RemoveLoopDenominatorsV6,PrepareForThermalV6];

toTerms[expr_]:=If[Head[expr]===Plus,List@@expr,{expr}];

ExpandMomentumHead[expr_]:=FixedPoint[Expand[#]/. {Momentum[a_+b_,D]:>Momentum[a,D]+Momentum[b,D],Momentum[a_-b_,D]:>Momentum[a,D]-Momentum[b,D],Momentum[-a_,D]:>-Momentum[a,D]}&,expr];

NormalizeFADInputV6[expr_,loop_:q,ext_:p]:=Module[{l=loop,e=ext},expr//.{FAD[{Momentum[k_,D],mass_}]:>FAD[{k,mass}],FAD[{Momentum[k_],mass_}]:>FAD[{k,mass}],FAD[{-l,mass_}]:>FAD[{l,mass}],FAD[{l-e,mass_}]:>FAD[{e-l,mass}],FAD[{-e+l,mass_}]:>FAD[{e-l,mass}],FAD[{Momentum[l-e,D],mass_}]:>FAD[{e-l,mass}],FAD[{Momentum[-e+l,D],mass_}]:>FAD[{e-l,mass}],FAD[{Momentum[e-l,D],mass_}]:>FAD[{e-l,mass}],FAD[{Momentum[l,D],mass_}]:>FAD[{l,mass}]}];

poleLabelFromMass[mass_]:=Which[mass===0,"0",mass===m1,"L1",mass===m2,"L2",mass===m3,"T3",mass===m4,"T4",True,Print["UnknownMassParameter: ",mass];
Abort[]];

RouteKeyV6[route_]:=Switch[route,"q",1,"p-q",2,"external",3,_,99];

MomentumRouteV6[mom_,loop_,ext_]:=Module[{mm},mm=ExpandMomentumHead[mom];
Which[mm===Momentum[loop,D]||mm===-Momentum[loop,D],"q",mm===Momentum[ext,D]-Momentum[loop,D]||mm===-Momentum[loop,D]+Momentum[ext,D]||mm===Momentum[loop,D]-Momentum[ext,D]||mm===-Momentum[ext,D]+Momentum[loop,D],"p-q",FreeQ[mm,loop],"external",True,"other"]];

PropRouteV6[PropagatorDenominator[mom_,mass_],loop_,ext_]:=MomentumRouteV6[mom,loop,ext];

LoopPropDataV6[term_,loop_,ext_]:=Module[{props,data},props=Cases[term,FeynAmpDenominator[ps__PropagatorDenominator]:>{ps},Infinity]//Flatten;
data=MapIndexed[Function[{prop,idx},prop/. PropagatorDenominator[mom_,mass_]:><|"Index"->idx[[1]],"Propagator"->prop,"Momentum"->ExpandMomentumHead[mom],"Mass"->mass,"Label"->poleLabelFromMass[mass],"Route"->MomentumRouteV6[mom,loop,ext]|>],props];
SortBy[Select[data,#["Route"]=!="external"&],{RouteKeyV6[#["Route"]],#["Index"]}&]];

LoopDenInfoV6[term_,loop_,ext_]:=Module[{data,routes,labels,topology},data=LoopPropDataV6[term,loop,ext];
routes=If[data==={},{},data[[All,"Route"]]];
labels=If[data==={},{},data[[All,"Label"]]];
topology=Which[routes==={},"NoLoopDenominator",routes==={"q"}||routes==={"p-q"},"Tadpole1",routes==={"q","p-q"},"Bubble2",routes==={"q","q","p-q"},"Bubble3QQP",routes==={"q","p-q","p-q"},"Bubble3QPP",routes==={"q","q","p-q","p-q"},"Bubble4QQPP",True,"Unsupported"];
<|"Topology"->topology,"NLoopPropagators"->Length[data],"Routes"->routes,"Labels"->labels,"Data"->data|>];

RemoveLoopDenominatorsV6[term_,loop_,ext_]:=term/. FeynAmpDenominator[ps__PropagatorDenominator]:>Module[{props,keep},props={ps};
keep=Select[props,PropRouteV6[#,loop,ext]==="external"&];
If[keep==={},1,FeynAmpDenominator@@keep]];

ClearAll[PrepareForThermalV6];

PrepareForThermalV6[expr_,loop_,ext_]:=Module[{tmp},tmp=If[ListQ[expr],Total[expr],expr];
tmp=NormalizeFADInputV6[tmp,loop,ext];
tmp=FCI[tmp];

tmp=Contract[tmp];
tmp=ExpandScalarProduct[tmp];
tmp=ExpandMomentumHead[tmp];
tmp=Contract[tmp];
tmp=ExpandScalarProduct[tmp];
Expand[tmp]];
ClearAll[thermalMomentV6,MomentPolyV6];



thermalMomentV6[topology_String,labels_List,a_Integer,b_Integer,c_Integer]:=LoopM[topology,labels,a,b,c];



MomentPolyV6[topology_String,labels_List,poly_]:=Module[{vars,expr,terms},vars={zq,xq,rq};
expr=Expand[poly];
If[TrueQ[expr===0],Return[0]];
If[!PolynomialQ[expr,vars],Print["MomentPolyV6: not a polynomial in zq, xq, rq: ",expr];
Abort[]];
terms=If[Head[expr]===Plus,List@@expr,{expr}];
Total[Map[Function[t,Module[{aa,bb,cc,coeff},If[TrueQ[t===0],0,aa=Exponent[t,zq];
bb=Exponent[t,xq];
cc=Exponent[t,rq];
If[!And@@(IntegerQ/@{aa,bb,cc}),Print["MomentPolyV6: non-integer exponent: ",{aa,bb,cc}];
Print["term = ",t];
Abort[]];
coeff=Simplify[t/(zq^aa*xq^bb*rq^cc)];
coeff*thermalMomentV6[topology,labels,aa,bb,cc]]]],terms]]];
(* =========================================================*)
(*Scalar products->thermal variables*)
(* =========================================================*)
ClearAll[ScalarToMomentPolyV6];

ScalarToMomentPolyV6[weight_,ext_,loop_,p0_,pv_]:=Module[{tmp},tmp=ExpandMomentumHead[ExpandScalarProduct[Contract[weight]]];
tmp=tmp/. {Pair[Momentum[ext,D],Momentum[ext,D]]:>p0^2-pv^2,Pair[Momentum[ext,D],Momentum[loop,D]]:>p0*zq-xq,Pair[Momentum[loop,D],Momentum[ext,D]]:>p0*zq-xq,Pair[Momentum[loop,D],Momentum[loop,D]]:>zq^2-rq};
Expand[tmp]];
(* ========================================================= *)
(* F1,F2,F3,F4 algebra                                      *)
(* ========================================================= *)

ClearAll[
  zeroCoeff,
  addCoeff,
  scaleCoeff
];

zeroCoeff[] :=
  <|
    "F1" -> 0,
    "F2" -> 0,
    "F3" -> 0,
    "F4" -> 0
  |>;

addCoeff[a_Association, b_Association] :=
  AssociationThread[
    Keys[a],
    Simplify /@ (Values[a] + Values[b])
  ];

scaleCoeff[c_, a_Association] :=
  AssociationThread[
    Keys[a],
    Simplify[c #] & /@ Values[a]
  ];
(* ========================================================= *)

(* ========================================================= *)

ClearAll[TensorCoeffV6];

TensorCoeffV6[
   topology_String,
   labels_List,
   tensorType_String,
   scalarWeight_,
   ext_,
   loop_,
   p0_,
   pv_
] :=
 Module[{W, Iw, F1, F2, F3, F4},

   W =
     ScalarToMomentPolyV6[
       scalarWeight,
       ext,
       loop,
       p0,
       pv
     ];

   Iw[poly_] :=
     MomentPolyV6[
       topology,
       labels,
       Expand[W*poly]
     ];

   Switch[
     tensorType,

     "g",
       F1 = Iw[1];
       F2 = Iw[1];
       F3 = 0;
       F4 = 0,

     "pp",
       F1 = p0^2*Iw[1];
       F2 = 0;
       F3 = p0*Iw[1];
       F4 = Iw[1],

     "pq" | "qp",
       F1 = p0*Iw[zq];
       F2 = 0;
       F3 = 1/2*(Iw[zq] + p0*Iw[xq]/pv^2);
       F4 = Iw[xq]/pv^2,

     "qq",
       F1 = Iw[zq^2];
       F2 = -1/2*(Iw[rq] - Iw[xq^2]/pv^2);
       F3 = Iw[zq*xq]/pv^2;
       F4 = 1/2*(3*Iw[xq^2]/pv^4 - Iw[rq]/pv^2),

     _,
       Print["Unknown tensorType = ", tensorType];
       Abort[]
   ];

   <|
     "F1" -> F1,
     "F2" -> F2,
     "F3" -> F3,
     "F4" -> F4
   |>
 ];
(* =========================================================*)
(*Restricted numerator whitelist*)
(* =========================================================*)
ClearAll[AllowedNumeratorSpecsTableV6,AllowedNumeratorSpecsV6];

AllowedNumeratorSpecsTableV6=<|"Bubble2"->{{"pp",{0,0,0}},{"pq",{0,0,0}},{"qp",{0,0,0}},{"qq",{0,0,0}},{"g",{1,0,0}},{"g",{0,1,0}},{"g",{0,0,1}}},"Bubble3QQP"->{{"qq",{1,0,0}},{"qp",{0,1,0}},{"pq",{0,1,0}},{"qq",{0,1,0}},{"g",{0,2,0}},{"pp",{0,0,1}},{"qp",{0,0,1}},{"pq",{0,0,1}},{"qq",{0,0,1}},{"g",{0,1,1}},{"g",{0,0,2}}},"Bubble3QPP"->{{"pp",{1,0,0}},{"qq",{1,0,0}},{"g",{2,0,0}},{"qp",{0,1,0}},{"pq",{0,1,0}},{"pp",{0,0,1}},{"qq",{0,0,1}},{"g",{1,0,1}},{"g",{0,0,2}}},"Bubble4QQPP"->{{"qq",{2,0,0}},{"qp",{1,1,0}},{"pq",{1,1,0}},{"pp",{0,2,0}}},"Tadpole1"->{}|>;

AllowedNumeratorSpecsV6[topology_String]:=Lookup[AllowedNumeratorSpecsTableV6,topology,{}];
(* =========================================================*)
(*Building restricted numerator patterns*)
(* =========================================================*)
ClearAll[gD,vD,spD,RankTensorV6,ScalarPartV6,PatternNameV6];

gD[mu_,nu_]:=Pair[LorentzIndex[mu,D],LorentzIndex[nu,D]];

vD[k_,mu_]:=Pair[LorentzIndex[mu,D],Momentum[k,D]];

spD[a_,b_]:=Pair[Momentum[a,D],Momentum[b,D]];

RankTensorV6["pp",ext_,loop_,mu_,nu_]:=vD[ext,mu]*vD[ext,nu];

RankTensorV6["pq",ext_,loop_,mu_,nu_]:=vD[ext,mu]*vD[loop,nu];

RankTensorV6["qp",ext_,loop_,mu_,nu_]:=vD[loop,mu]*vD[ext,nu];

RankTensorV6["qq",ext_,loop_,mu_,nu_]:=vD[loop,mu]*vD[loop,nu];

RankTensorV6["g",ext_,loop_,mu_,nu_]:=gD[mu,nu];

ScalarPartV6[{a_Integer,b_Integer,c_Integer},ext_,loop_]:=spD[ext,ext]^a*spD[ext,loop]^b*spD[loop,loop]^c;

PatternNameV6[topology_String,tensor_String,{a_,b_,c_}]:=topology<>"_"<>tensor<>"_p2"<>ToString[a]<>"_pq"<>ToString[b]<>"_q2"<>ToString[c];
(* =========================================================*)
(*Canonical scalar variables for exact matching*)
(* =========================================================*)
ClearAll[P2X,PQX,Q2X,ScalarRulesV6,CanonicalScalarizeV6];

ScalarRulesV6[ext_,loop_]:={Pair[Momentum[ext,D],Momentum[ext,D]]:>P2X,Pair[Momentum[ext,D],Momentum[loop,D]]:>PQX,Pair[Momentum[loop,D],Momentum[ext,D]]:>PQX,Pair[Momentum[loop,D],Momentum[loop,D]]:>Q2X};

CanonicalScalarizeV6[expr_,ext_,loop_]:=Module[{tmp},tmp=ExpandMomentumHead[ExpandScalarProduct[Contract[expr]]];
tmp=tmp/. ScalarRulesV6[ext,loop];
Expand[tmp]];
(* =========================================================*)
(*Corrected restricted numerator matching*)
(* =========================================================*)
ClearAll[CoefficientFreeQV6,BuildPatternV6,MatchRestrictedNumeratorV6];

CoefficientFreeQV6[coef_,ext_,loop_,mu_,nu_]:=Module[{bad},bad=!FreeQ[coef,P2X]||!FreeQ[coef,PQX]||!FreeQ[coef,Q2X]||!FreeQ[coef,LorentzIndex[mu,D]]||!FreeQ[coef,LorentzIndex[nu,D]]||!FreeQ[coef,Momentum[loop,D]]||!FreeQ[coef,Momentum[loop]]||!FreeQ[coef,Pair[LorentzIndex[mu,D],_]]||!FreeQ[coef,Pair[LorentzIndex[nu,D],_]]||!FreeQ[coef,Pair[_,LorentzIndex[mu,D]]]||!FreeQ[coef,Pair[_,LorentzIndex[nu,D]]]||!FreeQ[coef,Pair[Momentum[loop,D],_]]||!FreeQ[coef,Pair[_,Momentum[loop,D]]];
TrueQ[!bad]];

BuildPatternV6[topology_String,{tensor_String,exps_List},ext_,loop_,mu_,nu_]:=Module[{scalar,rank2,pattern,name},scalar=ScalarPartV6[exps,ext,loop];
rank2=RankTensorV6[tensor,ext,loop,mu,nu];
pattern=scalar*rank2;
name=PatternNameV6[topology,tensor,exps];
<|"Name"->name,"TensorType"->tensor,"Exponents"->exps,"ScalarPart"->scalar,"RankTensor"->rank2,"Pattern"->pattern|>];

MatchRestrictedNumeratorV6[numerator_,topology_String,ext_,loop_,mu_,nu_]:=Module[{specs,patterns,numC,hits},specs=AllowedNumeratorSpecsV6[topology];
If[specs==={},Print["No supported numerator structure is defined for this topology: ",topology];
Print["numerator = "];
Print[numerator];
Abort[]];
patterns=BuildPatternV6[topology,#,ext,loop,mu,nu]&/@specs;
numC=CanonicalScalarizeV6[numerator,ext,loop];
hits=Cases[patterns,pat_Association:>Module[{patC,coef,ok},patC=CanonicalScalarizeV6[pat["Pattern"],ext,loop];
coef=Quiet@Simplify[Together[numC/patC]];
ok=TrueQ[CoefficientFreeQV6[coef,ext,loop,mu,nu]]&&TrueQ[Simplify[numC-coef*patC]===0];
If[ok,Join[pat,<|"Coefficient"->coef|>],Nothing]]];
If[hits==={},Print["No matching supported structure."];
Print["Topology = ",topology];
Print["Allowed specs = ",specs];
Print["numerator = "];
Print[numerator];
Print["canonical numerator = "];
Print[numC];
Abort[],First[hits]]];
(* =========================================================*)(*Automatically infer whitelist from an expression*)(* =========================================================*)ClearAll[TensorTypesV6,SpecFromNumeratorTermV6,BuildAllowedNumeratorSpecsFromExprV6];

TensorTypesV6={"pp","pq","qp","qq","g"};

SpecFromNumeratorTermV6[numerator_,ext_,loop_,mu_,nu_]:=Module[{numC,candidates,rank,rankC,w,a,b,c,coeff,ok},numC=CanonicalScalarizeV6[numerator,ext,loop];
candidates=Reap[Do[rank=RankTensorV6[tensor,ext,loop,mu,nu];
rankC=CanonicalScalarizeV6[rank,ext,loop];
w=Quiet@Simplify[Together[numC/rankC]];
If[TrueQ[Simplify[numC-w*rankC]==0],a=Exponent[w,P2X];
b=Exponent[w,PQX];
c=Exponent[w,Q2X];
coeff=Quiet@Simplify[Together[w/(P2X^a*PQX^b*Q2X^c)]];
ok=And@@(IntegerQ[#]&&#>=0&/@{a,b,c})&&TrueQ[CoefficientFreeQV6[coeff,ext,loop,mu,nu]]&&TrueQ[Simplify[w-coeff*P2X^a*PQX^b*Q2X^c]==0];
If[ok,Sow[{tensor,{a,b,c}}]]],{tensor,TensorTypesV6}]][[2]];
If[candidates==={},Missing["NoSpec",numerator],First[First[candidates]]]];

BuildAllowedNumeratorSpecsFromExprV6[expr_,loop_,ext_,mu_,nu_]:=Module[{prepared,terms,rows,goodRows,badRows,table,topologies},prepared=PrepareForThermalV6[expr,loop,ext];
terms=toTerms[Expand[prepared]];
rows=Table[Module[{info,topology,numerator,spec},info=LoopDenInfoV6[term,loop,ext];
topology=info["Topology"];
If[topology==="NoLoopDenominator"||topology==="Unsupported",<|"Status"->"BadTopology","Topology"->topology,"Routes"->info["Routes"],"Labels"->info["Labels"],"Term"->term|>,numerator=Simplify[RemoveLoopDenominatorsV6[term,loop,ext]];
spec=SpecFromNumeratorTermV6[numerator,ext,loop,mu,nu];
If[Head[spec]===Missing,<|"Status"->"BadNumerator","Topology"->topology,"Routes"->info["Routes"],"Labels"->info["Labels"],"Numerator"->numerator,"Term"->term|>,<|"Status"->"Good","Topology"->topology,"Routes"->info["Routes"],"Labels"->info["Labels"],"Spec"->spec,"Term"->term|>]]],{term,terms}];
badRows=Select[rows,#["Status"]=!="Good"&];
If[badRows=!={},Print["some Termcannot automatically identify
. first Termasunder : "];
Print[First[badRows]];
Abort[]];
goodRows=Select[rows,#["Status"]==="Good"&];
table=GroupBy[goodRows,#["Topology"]&,DeleteDuplicates[#["Spec"]&/@#]&];
topologies={"Bubble2","Bubble3QQP","Bubble3QPP","Bubble4QQPP","Tadpole1"};
AssociationMap[Lookup[table,#,{}]&,topologies]];
(* =========================================================*)
(*Process one term*)
(* =========================================================*)
ClearAll[ProcessThermalTermV6];

ProcessThermalTermV6[term_,loop_,ext_,mu_,nu_,p0_,pv_]:=Module[{info,topology,labels,numerator,hit,basis,basisContribution},info=LoopDenInfoV6[term,loop,ext];
topology=info["Topology"];
labels=info["Labels"];
If[topology==="NoLoopDenominator",Print["This term has no loop-dependent denominator: "];
Print[term];
Abort[]];
If[topology==="Unsupported",Print["Unsupported FAD topology."];
Print["Routes = ",info["Routes"]];
Print["Labels = ",labels];
Print["term = ",term];
Abort[]];
numerator=Simplify[RemoveLoopDenominatorsV6[term,loop,ext]];
hit=MatchRestrictedNumeratorV6[numerator,topology,ext,loop,mu,nu];
basis=TensorCoeffV6[topology,labels,hit["TensorType"],hit["ScalarPart"],ext,loop,p0,pv];
basisContribution=scaleCoeff[hit["Coefficient"],basis];
<|"Topology"->topology,"NLoopPropagators"->info["NLoopPropagators"],"Routes"->info["Routes"],"PoleLabels"->labels,"TensorType"->hit["TensorType"],"ScalarExponents"->hit["Exponents"],"NumeratorPattern"->hit["Name"],"Coefficient"->hit["Coefficient"],"ScalarPart"->hit["ScalarPart"],"MatchedPattern"->hit["Pattern"],"BasisBeforeCoefficient"->basis,"BasisContribution"->basisContribution,"OriginalTerm"->term|>];
(* =========================================================*)
(*Main reducer V6*)
(* =========================================================*)
ClearAll[ThermalReduceRGZV6];

ThermalReduceRGZV6[expr_,loop_,ext_,mu_,nu_,p0Sym_:p0,pvSym_:pv]:=Module[{prepared,terms,processed,total},prepared=PrepareForThermalV6[expr,loop,ext];
terms=toTerms[prepared];
processed=ProcessThermalTermV6[#,loop,ext,mu,nu,p0Sym,pvSym]&/@terms;
total=Fold[addCoeff,zeroCoeff[],processed[[All,"BasisContribution"]]];
<|"PreparedExpression"->prepared,"NumberOfTerms"->Length[terms],"TopologyTally"->Tally[processed[[All,"Topology"]]],"FADNumberTally"->Tally[processed[[All,"NLoopPropagators"]]],"RouteTally"->Tally[processed[[All,"Routes"]]],"PoleLabelTally"->Tally[processed[[All,"PoleLabels"]]],"TensorTally"->Tally[processed[[All,"TensorType"]]],"NumeratorPatternTally"->Tally[processed[[All,"NumeratorPattern"]]],"TermData"->processed,"Coefficients"->total|>];
(* =========================================================*)
(*Finite-temperature numerical backend with T=0 subtraction*)
(*Robust option parser:accepts T->and "T"->forms*)
(* =========================================================*)ClearAll[IdStringV6,NormalizeLabelsV6,NormalizeMassAssocV6,NormalizeOptionsAssocV6,GetOptV6,RoutesFromTopologyV6,LabelMass2V6,BoseV6,EnergyExprV6,ShiftExprV6,LoopMCacheV6,ClearLoopMCacheV6,LoopMThermalFiniteV6,EvaluateThermalCoefficientsV6];

(*----------identifier normalization----------*)

IdStringV6[x_String]:=x;

IdStringV6[0]:="0";
IdStringV6[0.]:="0";

IdStringV6[x_Integer]:=ToString[x,InputForm];

IdStringV6[x_Real]:=If[TrueQ[Chop[x]==0],"0",ToString[x,InputForm]];

IdStringV6[x_Symbol]:=SymbolName[Unevaluated[x]];

IdStringV6[x_]:=StringReplace[ToString[Unevaluated[x],InputForm],"\""->""];

NormalizeLabelsV6[labels_List]:=IdStringV6/@labels;

NormalizeMassAssocV6[masses_Association]:=Association[KeyValueMap[IdStringV6[#1]->#2&,masses]];

(*----------option normalization----------*)

NormalizeOptionsAssocV6[opts_List]:=Module[{rules},rules=opts/. {Rule[k_,v_]:>Rule[IdStringV6[k],v],RuleDelayed[k_,v_]:>RuleDelayed[IdStringV6[k],v]};
Association[rules]];

GetOptV6[assoc_Association,key_String,default_]:=If[KeyExistsQ[assoc,key],assoc[key],default];

(*----------topology routes----------*)

RoutesFromTopologyV6[topology_,n_Integer]:=Module[{top},top=IdStringV6[topology];
Switch[top,"Tadpole1",If[n===1,{"q"},Print["Tadpole1 labels Invalid length: ",n];
Abort[]],"Bubble2",If[n===2,{"q","p-q"},Print["Bubble2 labels Invalid length: ",n];
Abort[]],"Bubble3QQP",If[n===3,{"q","q","p-q"},Print["Bubble3QQP labels Invalid length: ",n];
Abort[]],"Bubble3QPP",If[n===3,{"q","p-q","p-q"},Print["Bubble3QPP labels Invalid length: ",n];
Abort[]],"Bubble4QQPP",If[n===4,{"q","q","p-q","p-q"},Print["Bubble4QQPP labels Invalid length: ",n];
Abort[]],_,Print["Unknown topology: ",top];
Abort[]]];

(*----------mass lookup----------*)

LabelMass2V6[label_,masses_Association]:=Module[{key,massesS},key=IdStringV6[label];
massesS=NormalizeMassAssocV6[masses];
If[KeyExistsQ[massesS,key],massesS[key],Print["Label missing from Mass2 association: ",key];
Print["Current keys = ",Keys[massesS]];
Abort[]]];

(*----------Bose factor and kinematics----------*)

BoseV6[E_,temp_]:=1/(Exp[E/temp]-1);

EnergyExprV6[route_,label_,qabs_,costh_,pvVal_,masses_Association]:=Module[{routeS,m2},routeS=IdStringV6[route];
m2=LabelMass2V6[label,masses];
Switch[routeS,"q",Sqrt[qabs^2+m2],"p-q",Sqrt[qabs^2+pvVal^2-2 pvVal qabs costh+m2],_,Print["Unknown route: ",routeS];
Abort[]]];

ShiftExprV6[route_,p0M_]:=Module[{routeS},routeS=IdStringV6[route];
Switch[routeS,"q",0,"p-q",p0M,_,Print["Unknown route: ",routeS];
Abort[]]];

(*----------cache----------*)

LoopMCacheV6=<||>;

ClearLoopMCacheV6[]:=(LoopMCacheV6=<||>;);

(*----------main numerical moment----------*)

LoopMThermalFiniteV6[topology_,labels_List,a_Integer,b_Integer,c_Integer,opts___Rule]:=Module[{optAssoc,topologyS,labelsS,temp,pvVal,p0M,qmin,qmax,masses,useCache,wp,acc,prec,maxrec,method,qabs,costh,z,routes,lineData,familyData,den,num,F,thermalResidueExpr,measureExpr,cacheKey,result},optAssoc=NormalizeOptionsAssocV6[{opts}];
topologyS=IdStringV6[topology];
labelsS=NormalizeLabelsV6[labels];
temp=GetOptV6[optAssoc,"T",0.15];
pvVal=GetOptV6[optAssoc,"PV",1.0];
p0M=GetOptV6[optAssoc,"P0M",0.0];
qmin=GetOptV6[optAssoc,"QMin",10^-6];
qmax=GetOptV6[optAssoc,"QMax",10.0];
masses=NormalizeMassAssocV6[GetOptV6[optAssoc,"Mass2",<|"0"->0.0,"L1"->0.4^2,"L2"->0.5^2,"T3"->0.6^2,"T4"->0.8^2|>]];
useCache=TrueQ[GetOptV6[optAssoc,"UseCache",True]];
wp=GetOptV6[optAssoc,"WorkingPrecision",MachinePrecision];
acc=GetOptV6[optAssoc,"AccuracyGoal",5];
prec=GetOptV6[optAssoc,"PrecisionGoal",5];
maxrec=GetOptV6[optAssoc,"MaxRecursion",8];
method=GetOptV6[optAssoc,"Method",Automatic];
cacheKey={topologyS,labelsS,a,b,c,temp,pvVal,p0M,qmin,qmax,Normal[masses],wp,acc,prec,maxrec,method};
If[useCache&&KeyExistsQ[LoopMCacheV6,cacheKey],
 result=LoopMCacheV6[cacheKey];If[NumberQ[result]&&!RGZWorkflow`Private`qqFFInvalidQ[result],Return[result]];
 KeyDropFrom[LoopMCacheV6,cacheKey]];
routes=RoutesFromTopologyV6[topologyS,Length[labelsS]];
lineData=MapThread[<|"Route"->#1,"Label"->#2,"Shift"->ShiftExprV6[#1,p0M],"Energy"->EnergyExprV6[#1,#2,qabs,costh,pvVal,masses]|>&,{routes,labelsS}];
familyData=DeleteDuplicatesBy[lineData,{#["Route"],#["Label"]}&];
den=Times@@(((z-#["Shift"])^2-#["Energy"]^2)&/@lineData);
num=z^a*(pvVal*qabs*costh)^b*(qabs^2)^c;
F=num/den;
thermalResidueExpr=Total[(Residue[BoseV6[-(z-#["Shift"]),temp]*F,{z,#["Shift"]-#["Energy"]}]+Residue[-BoseV6[z-#["Shift"],temp]*F,{z,#["Shift"]+#["Energy"]}])&/@familyData];
measureExpr=Cancel[Together[qabs^2/(4 Pi^2)*thermalResidueExpr]];
result=Check[With[{qlo=qmin,qhi=qmax,wp0=wp,acc0=acc,prec0=prec,maxrec0=maxrec,method0=method,integrand=measureExpr},NIntegrate[Evaluate[integrand],{qabs,qlo,qhi},{costh,-1,1},WorkingPrecision->wp0,AccuracyGoal->acc0,PrecisionGoal->prec0,MaxRecursion->maxrec0,Method->method0]],$Failed];
If[!NumberQ[result]||RGZWorkflow`Private`qqFFInvalidQ[result],Return[Failure["ThermalMomentIncomplete",<||>]]];
If[useCache,LoopMCacheV6[cacheKey]=result];
result];

(*----------replace LoopM by numerical moments----------*)

EvaluateThermalCoefficientsV6[coeff_Association,opts___Rule]:=Module[{optAssoc,p0M,pvVal,replaced},optAssoc=NormalizeOptionsAssocV6[{opts}];
p0M=GetOptV6[optAssoc,"P0M",0.0];
pvVal=GetOptV6[optAssoc,"PV",1.0];
replaced=coeff/. {p0->p0M,pv->pvVal};
replaced/.
LoopM[topology_,labels_List,a_Integer,b_Integer,c_Integer]:>LoopMThermalFiniteV6[topology,labels,a,b,c,opts]];
