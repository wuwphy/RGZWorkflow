(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Run after loading FeynCalc/FeynArts and extracting the raw KT and KL.
   Put RGZ_W2.6.gen and RGZ_W2.6.mod beside the notebook.
   Fourier convention: del -> -I p, D = I Inverse[K].
   rho is written on the left. mG^4 = 2 N gs^2 gamma^4. *)

ClearAll[rgzS, rgzName, rgzParameters, rgzNormalizeParameters,
  rgzNormalizeKernel, rgzIndexLabel, rgzIndexName, rgzPrepareFA, rgzLorentzNames,
  RGZTwoPoint, rgzReduce, rgzRational, rgzDifference,
  rgzIsVector, rgzIsAux, rgzAuxSign, rgzColor, rgzPP,
  rgzPT, rgzPL, rgzT, rgzQ, rgzDelta, rgzAnalytic,
  rgzInsertionToCovariance, rgzConnectedAh, rgzConnectionChecks,
  rgzKernelReference, rgzGhostK, rgzGhostSource, rgzKF, rgzDF,
  rgzGhostKernelReference, rgzMakeRow, rgzState, rgzPane,
  rgzCountTable, rgzShowPropagator, rgzOldPut, RGZSplitPropagator,
  RGZInsertPoleMasses, m1, m2, m3, m4];

rgzName[x_Symbol] := SymbolName[Unevaluated[x]];
rgzName[x_String] := x;
rgzName[_] := "";
rgzParameters = <|
  "m" -> m, "mG" -> mG, "M" -> M, "N" -> N, "Nc" -> N,
  "\[Mu]" -> \[Mu], "\[Alpha]" -> \[Alpha], "\[Rho]" -> \[Rho],
  "m02" -> m^2, "mu2" -> \[Mu]^2,
  "mg2" -> mG^2, "mG2" -> mG^2, "M4" -> M^4,
  "rho" -> \[Rho], "s" -> rgzS,
  "0" -> 0, "1" -> 1, "2" -> 2, "3" -> 3, "4" -> 4, "I" -> I|>;

(* Copied TraditionalForm output can contain Superscript instead of Power,
   and TeX strings such as "\[Alpha]" instead of the corresponding symbol.
   Convert known parameter strings only; never evaluate arbitrary strings. *)
rgzNormalizeParameters[x_] := FixedPoint[
  Function[z, z /. {
    HoldPattern[Superscript[a_, n_Integer]] :> a^n,
    HoldPattern[Subsuperscript[a_, b_, n_Integer]] :> Subscript[a, b]^n,
    HoldPattern[Subscript[a_, b_]] /;
      rgzName[a] == "m" && rgzName[b] == "G" :> mG,
    HoldPattern[FCGV[a_String]] /;
      MemberQ[Keys[rgzParameters], a] :> rgzParameters[a],
    a_String /; MemberQ[Keys[rgzParameters], a] :> rgzParameters[a],
    a_Symbol /; MemberQ[Keys[rgzParameters], rgzName[a]] :>
      rgzParameters[rgzName[a]]
  }], x, 8
];

If[!MatrixQ[KT] || Dimensions[KT] =!= {5, 5} ||
   !MatrixQ[KL] || Dimensions[KL] =!= {8, 8},
  Print["\:8bf7\:5148\:8fd0\:884c\:4e8c\:6b21\:9879\:63d0\:53d6\:4ee3\:7801\:ff0c\:5f97\:5230\:539f\:59cb List \:77e9\:9635 KT\:3001KL\:ff1b\:4e0d\:8981\:4fdd\:5b58 MatrixForm \:663e\:793a\:5bf9\:8c61\:3002"];
  Abort[]
];

(* m2 can be a kernel mass parameter, but is also a pole label below.
   Resolve this alias only while reading the original quadratic kernels. *)
rgzNormalizeKernel[x_] := rgzNormalizeParameters[x] /. {
  HoldPattern[FCGV["m2"]] :> m^2,
  "m2" :> m^2,
  a_Symbol /; rgzName[a] == "m2" :> m^2
};
rgzKernelNotation = DeleteDuplicates[Cases[{KT, KL},
  _Superscript | _Subsuperscript | _String, Infinity]];
If[Length[rgzKernelNotation] > 0,
  Print["\:4e8c\:6b21\:6838\:4e2d\:7684\:663e\:793a\:5bf9\:8c61\:5df2\:8f6c\:4e3a\:8ba1\:7b97\:8868\:8fbe\:5f0f\:ff1a", InputForm[rgzKernelNotation]]];
rgzKT = rgzNormalizeKernel[KT];
rgzKL = rgzNormalizeKernel[KL];
rgzDT = FullSimplify[I Inverse[rgzKT]];
rgzDL = FullSimplify[I Inverse[rgzKL]];

Print["\:6a2a\:5411\:53cd\:6f14\:68c0\:67e5\:ff1a", FullSimplify[rgzKT.rgzDT-I IdentityMatrix[5]]];
Print["\:7eb5\:5411\:53cd\:6f14\:68c0\:67e5\:ff1a", FullSimplify[rgzKL.rgzDL-I IdentityMatrix[8]]];

modelRGZ = RGZWorkflow`Private`modelBase[];
If[!And @@ (FileExistsQ[modelRGZ <> #] & /@ {".gen", ".mod"}),
  Print["\:6a21\:578b\:6587\:4ef6\:672a\:627e\:5230\:ff1a", modelRGZ, ".gen / .mod"]; Abort[]
];
topRGZ = CreateTopologies[0, 1 -> 1, CTOrder -> 1,
  ExcludeTopologies -> TadpoleCTs];

rgzIsVector[i_] := MemberQ[{1, 5, 6, 7, 8}, i];
rgzIsAux[i_] := MemberQ[{5, 6, 7, 8}, i];
rgzAuxSign[i_] := If[MemberQ[{5, 6}, i], 1, -1];

rgzIndexLabel[x_] := Module[{y = x},
  If[rgzName[Head[y]] == "Ext", y = First[y]];
  If[Head[y] === Symbol &&
    StringMatchQ[rgzName[y], "a" ~~ DigitCharacter ..],
    y = ToExpression[StringDrop[rgzName[y], 1]]]; y
];
rgzIndexName[type_, x_] := Module[{y = rgzIndexLabel[x]},
  If[IntegerQ[y],
    Symbol["RGZWorkflow`Engine`" <> If[type == "Gluon", "Glu", "Gri"] <>
      ToString[y]], y]
];
rgzPrepareFA[x_] := x /. {
  HoldPattern[h_[type_, a_]] /;
    rgzName[h] == "Index" && MemberQ[{"Gluon", "Gribov"}, rgzName[type]] :>
    SUNIndex[rgzIndexName[rgzName[type], a]]
};

rgzLorentzNames[i_, j_] := Module[{n = 0},
  DeleteCases[MapIndexed[Function[{leg, pos},
    Which[rgzIsVector[leg], n++; If[n == 1, \[Mu], \[Nu]],
      MemberQ[{9, 10}, leg],
        Symbol["RGZWorkflow`Engine`rgzGhostLor" <> ToString[First[pos]]],
      True, Nothing]], {i, j}], Nothing]
];

rgzReduce[x_] := Module[{z = rgzNormalizeParameters[x]},
  z = z /. (HoldPattern[h_[a_, b_]] /; rgzName[h] == "IndexDelta" :>
    SUNDelta[a, b]);
  z = Expand[ExpandScalarProduct[FCI[z]]];
  z = FCCanonicalizeDummyIndices[z,
    SUNIndexNames -> Table[Symbol["RGZWorkflow`Engine`rgzColorD" <> ToString[k]], {k, 24}]];
  z = SUNSimplify[z, SUNNToCACF -> False] /. SUNN -> N;
  FullSimplify[Cancel[Together[Expand[z]]]]
];

rgzRational[x_] := Module[{z = If[ListQ[x], Total[x], x]},
  z = ExpandScalarProduct[FeynAmpDenominatorExplicit[FCI[z]]];
  z = z /. {
    HoldPattern[Pair[Momentum[p, dim1___], Momentum[p, dim2___]]] /;
      {dim1} === {dim2} :> rgzS
  };
  rgzReduce[z]
];
rgzDifference[x_, y_] := rgzReduce[x-y];

(* FA's p enters a two-point insertion. The physical field momentum at
   the adjacent vertex is -p. Compare the covariance D_ij(p), not the
   insertion D_ij(-p); even-momentum entries are unchanged. *)
rgzInsertionToCovariance[x_] := x /. p -> -p;

RGZTwoPoint[fin_, fout_, i_, j_] := Module[{ins, fa, fc},
  ins = InsertFields[topRGZ, {fin} -> {fout},
    InsertionLevel -> {Particles}, Model -> modelRGZ,
    GenericModel -> modelRGZ];
  If[Length[ins] == 0,
    Return[<|"FeynArts\:632f\:5e45" -> 0, "FeynCalc\:632f\:5e45" -> 0,
      "\:751f\:6210\:72b6\:6001" -> "\:65e0\:63d2\:5165\:56fe"|>]];
  fa = Check[CreateFeynAmp[ins, Truncated -> True, GaugeRules -> {},
    PreFactor -> 1], $Failed, {CreateFeynAmp::nocoupl, CreateFeynAmp::noprop}];
  If[fa === $Failed || Length[fa] == 0,
    Return[Failure["NoAmplitude", <|"\:63d2\:5165\:56fe" -> ins|>]]];
  fc = FCFAConvert[rgzPrepareFA[fa], IncomingMomenta -> {p},
    OutgoingMomenta -> {p}, LorentzIndexNames -> rgzLorentzNames[i, j],
    UndoChiralSplittings -> True, ChangeDimension -> D,
    List -> False, DropSumOver -> True];
  If[!FreeQ[fc, a_ /; MemberQ[
      {"G", "C", "AnalyticalCoupling", "FAFourVector", "FAMetricTensor",
       "FAScalarProduct", "FASUNF", "RGZUPropagator"}, rgzName[Head[a]]]],
    Return[Failure["Unresolved", <|"\:632f\:5e45" -> fc|>]]];
  <|"FeynArts\:632f\:5e45" -> fa, "FeynCalc\:632f\:5e45" -> fc,
    "\:751f\:6210\:72b6\:6001" -> "\:5df2\:751f\:6210"|>
];

rgzT = rgzS-\[Mu]^2;
rgzQ = rgzS^2+\[Alpha] M^4;
rgzDelta = (rgzS-m^2)(rgzS-\[Mu]^2)+mG^4;
rgzPP = FCI[FVD[p, \[Mu]] FVD[p, \[Nu]]];
rgzPT = FCI[MTD[\[Mu], \[Nu]]]-rgzPP/rgzS;
rgzPL = rgzPP/rgzS;

rgzColor[i_, j_] := Which[
  rgzIsAux[i] && rgzIsAux[j],
    SUNF[Glu1, Gri1, rgzColorD0] SUNF[Glu2, Gri2, rgzColorD0]/N,
  rgzIsAux[i], SUNF[Glu2, Glu1, Gri1]/Sqrt[N],
  rgzIsAux[j], SUNF[Glu1, Glu2, Gri2]/Sqrt[N],
  True, SUNDelta[Glu1, Glu2]
];

(* \:72ec\:7acb\:5199\:51fa\:7684\:5f53\:524d\:95f5\:6c0f\:53c2\:8003\:ff0c\:4fdd\:7559\:6240\:6709\:989c\:8272\:6307\:6807\:3002 *)
rgzAnalytic[i_, j_] := Module[{d = SUNDelta[Glu1, Glu2],
    v = FCI[FVD[p, \[Mu]]], free, core},
  Which[
    {i,j} === {9,10}, -I d/rgzS,
    i === 9 && MemberQ[{1,2,3,4},j],
      -\[Rho]/rgzS rgzAnalytic[4,j],
    rgzIsAux[i] && rgzIsAux[j],
      free = If[MemberQ[{{5,6},{6,5},{7,8},{8,7}}, {i,j}],
        I SUNDelta[Glu1,Glu2] SUNDelta[Gri1,Gri2] FCI[MTD[\[Mu],\[Nu]]]/rgzT, 0];
      free-I rgzAuxSign[i] rgzAuxSign[j] mG^4/(4 rgzT rgzDelta)
        rgzColor[i,j] rgzPT,
    i === 1 && rgzIsAux[j],
      I rgzAuxSign[j] mG^2/(2 rgzDelta) rgzColor[i,j] rgzPT,
    j === 1 && rgzIsAux[i],
      I rgzAuxSign[i] mG^2/(2 rgzDelta) rgzColor[i,j] rgzPT,
    i === 3 && rgzIsAux[j],
      -rgzAuxSign[j] mG^2/(2 rgzS rgzT) rgzColor[i,j] v,
    j === 3 && rgzIsAux[i],
      rgzAuxSign[i] mG^2/(2 rgzS rgzT) rgzColor[i,j] v,
    rgzIsAux[i] || rgzIsAux[j], 0,
    True,
      core = Switch[{i,j},
        {1,1}, -I (rgzT/rgzDelta rgzPT+\[Alpha] rgzS/rgzQ rgzPL),
        {1,2}, -rgzS v/rgzQ, {2,1}, rgzS v/rgzQ,
        {1,3}, -\[Alpha] M^4 v/(rgzS rgzQ),
        {3,1}, \[Alpha] M^4 v/(rgzS rgzQ),
        {1,4}, -\[Alpha] v/rgzQ, {4,1}, \[Alpha] v/rgzQ,
        {2,2}, I M^4/rgzQ,
        {2,3}|{3,2}, -I M^4/rgzQ,
        {2,4}|{4,2}, -I rgzS/rgzQ,
        {3,3}, I (-m^2/rgzS+M^4/rgzQ+mG^4/(rgzS rgzT)),
        {3,4}|{4,3}, I rgzS/rgzQ,
        {4,4}, -I \[Alpha]/rgzQ, _, 0]; d core
  ]
];

(* Ghost quadratic sector, with the given positive kinetic term and rho xi c. *)
rgzGhostK = rgzS;
rgzGhostSource = \[Rho];

(* Antisymmetric ghost kernel: its bilinear is s bar c' c. *)
rgzKF = {{0, rgzGhostK}, {-rgzGhostK, 0}};
rgzDF = FullSimplify[I Inverse[rgzKF]];
Print["\:9b3c\:573a\:6838\:53cd\:6f14\:68c0\:67e5\:ff1a",
  FullSimplify[rgzKF.rgzDF-I IdentityMatrix[2]]];

rgzGhostKernelReference[j_Integer] := Module[
  {color = SUNDelta[Glu1, Glu2], xiX},
  Which[
    j === 10, rgzReduce[color rgzDF[[1, 2]]],
    MemberQ[{1, 2, 3, 4}, j],
      xiX = color rgzDL[[4, j]];
      If[j === 1,
        xiX *= FCI[FVD[p, \[Mu]]]/Sqrt[rgzS]];
      rgzReduce[-rgzGhostSource/rgzGhostK xiX],
    True, Missing["\:672a\:7eb3\:5165\:9b3c\:573a\:6bd4\:8f83"]
  ]
];

rgzKernelReference[i_, j_] := Module[{ti, tj, free, core},
  If[i === 9 && MemberQ[{1, 2, 3, 4, 10}, j],
    Return[rgzGhostKernelReference[j]]];
  If[i > 8 || j > 8, Return[Missing["\:4e0d\:5728\:5f53\:524d\:4e8c\:6b21\:90e8\:95e8\:4e2d"]]];
  If[rgzIsAux[i] && rgzIsAux[j],
    ti=i-3; tj=j-3;
    free=If[MemberQ[{{5,6},{6,5},{7,8},{8,7}}, {i,j}], I/rgzT, 0];
    Return[rgzReduce[
      free SUNDelta[Glu1,Glu2] SUNDelta[Gri1,Gri2] FCI[MTD[\[Mu],\[Nu]]] +
      rgzColor[i,j] ((rgzDT[[ti,tj]]-free) rgzPT+
        (rgzDL[[i,j]]-free) rgzPL)]]];
  core=Which[
    rgzIsVector[i] && rgzIsVector[j],
      ti=If[i==1,1,i-3]; tj=If[j==1,1,j-3];
      rgzDT[[ti,tj]] rgzPT+rgzDL[[i,j]] rgzPL,
    rgzIsVector[i] || rgzIsVector[j],
      rgzDL[[i,j]] FCI[FVD[p,\[Mu]]]/Sqrt[rgzS],
    True, rgzDL[[i,j]]];
  rgzReduce[rgzColor[i,j] core]
];

(* \:4fdd\:7559\:65e7\:4ee3\:7801\:4e2d\:7684 PDF \:8f6c\:5f55\:503c\:ff1b\:4e0d\:628a\:5b83\:4eec\:5f53\:4f5c\:5df2\:7edf\:4e00\:7684\:95f5\:6c0f\:516c\:5f0f\:3002 *)
rgzOldPDF=<||>; rgzEquation=<||>;
rgzOldPut[key_, eq_, val_] := (
  AssociateTo[rgzOldPDF,key->val]; AssociateTo[rgzEquation,key->eq]);
Do[With[{i=spec[[1]],j=spec[[2]],eq=spec[[3]]},
  rgzOldPut[ToString[i]<>":"<>ToString[j],eq,rgzAnalytic[i,j]]],
  {spec,{{1,1,"(35),(57),(62)"},{1,2,"(36)"},{1,3,"(38)"},{1,4,"(37)"},
    {2,2,"(39)"},{2,3,"(41)"},{2,4,"(40)"},{3,3,"(44)"},{3,4,"(43)"},{4,4,"(42)"}}}];
rgzOldPDF["1:3"]=-rgzAnalytic[1,3];
rgzOldPDF["3:4"]=-rgzAnalytic[3,4];
Do[
  rgzOldPut["1:"<>ToString[j],"(58),(63)",rgzAnalytic[1,j]];
  rgzOldPut[ToString[j]<>":3","(46)--(48)",rgzAnalytic[j,3]];
  rgzOldPut[ToString[j]<>":"<>ToString[j],"(59),(64)",rgzAnalytic[j,j]], {j,5,8}];
Do[rgzOldPut[ToString[a[[1]]]<>":"<>ToString[a[[2]]],"(60),(65)",
  rgzAnalytic@@a],{a,{{5,6},{6,5},{7,8},{8,7}}}];
Do[rgzOldPut[ToString[i]<>":"<>ToString[j],"(61),(66)",rgzAnalytic[i,j]],
  {i,5,6},{j,7,8}];
rgzOldPut["9:10","(49)",-SUNDelta[Glu1,Glu2]/rgzS];
rgzOldPut["9:4","(50)",-\[Rho] \[Alpha] SUNDelta[Glu1,Glu2]/(rgzS rgzQ)];
rgzOldPut["9:1","(51)",-I \[Rho] SUNDelta[Glu1,Glu2] FCI[FVD[p,\[Mu]]]/rgzQ];
rgzOldPut["9:2","(52)",-\[Rho] SUNDelta[Glu1,Glu2]/rgzQ];
rgzOldPut["9:3","(53)",\[Rho] SUNDelta[Glu1,Glu2]/rgzQ];

rgzNames={"A","b","\[Tau]","\[Xi]","\[Phi]",Overscript[\[Phi],"_"],
  "\[Beta]",Overscript[\[Beta],"_"],Overscript[c,"_"],"c"};
rgzIncoming={V[1],S[3],S[2],S[1],V[2],-V[2],V[3],-V[3]};
rgzOutgoing={V[1],S[3],S[2],S[1],-V[2],V[2],-V[3],V[3]};
rgzSpecs=Join[Flatten[Table[{i,j,rgzIncoming[[i]],rgzOutgoing[[j]]},
  {i,8},{j,8}],1],{{9,10,-U[1],-U[1]},{9,1,-U[1],V[1]},
  {9,2,-U[1],S[3]},{9,3,-U[1],S[2]},{9,4,-U[1],S[1]}}];

rgzMakeRow[{i_,j_,fin_,fout_}] := Module[{key,result,raw,model,ref,kernel,old},
  key=ToString[i]<>":"<>ToString[j];
  Print["\:6b63\:5728\:68c0\:67e5 ",key,"\:ff1a",Row[{rgzNames[[i]]," -> ",rgzNames[[j]]}]];
  ref=rgzReduce[rgzAnalytic[i,j]]; kernel=rgzKernelReference[i,j];
  old=Lookup[rgzOldPDF,key,Missing["\:65e7\:4ee3\:7801\:672a\:9010\:9879\:5217\:51fa"]];
  result=CheckAbort[RGZTwoPoint[fin,fout,i,j],$Failed];
  raw=If[AssociationQ[result],result["FeynCalc\:632f\:5e45"],$Failed];
  model=If[raw===$Failed,$Failed,rgzRational[rgzInsertionToCovariance[raw]]];
  <|"\:7f16\:53f7"->key,"\:573a"->Row[{rgzNames[[i]]," -> ",rgzNames[[j]]}],
    "PDF\:5f0f\:53f7"->Lookup[rgzEquation,key,"\:672a\:5217\:51fa"],
    "\:751f\:6210\:72b6\:6001"->If[AssociationQ[result],result["\:751f\:6210\:72b6\:6001"],"\:8ba1\:7b97\:5931\:8d25"],
    "InsertionMomentumConvention"->"Incoming insertion momentum is minus covariance momentum",
    "FeynCalc\:632f\:5e45"->raw,"\:6709\:7406\:5f0f"->model,"\:95f5\:6c0f\:53c2\:8003"->ref,"\:4e8c\:6b21\:6838\:516c\:5f0f"->kernel,
    "\:65e7PDF\:53c2\:8003"->old,
    "\:6a21\:578b\:51cf\:95f5\:6c0f"->If[model===$Failed,$Failed,rgzDifference[model,ref]],
    "\:6a21\:578b\:51cf\:4e8c\:6b21\:6838"->If[Head[kernel]===Missing,kernel,
      If[model===$Failed,$Failed,rgzDifference[model,kernel]]],
    "\:6a21\:578b\:51cf\:65e7PDF"->If[Head[old]===Missing,old,
      If[model===$Failed,$Failed,rgzDifference[model,old]]]|>
];
rgzRows=rgzMakeRow /@ rgzSpecs;

(* Test the generated FA covariances after contraction with the derivative
   vertex, independently of the entry-by-entry analytic references.
   p.A + I p^2 xi vanishes against every field except tau: the multiplier
   supplies the -delta contact term for the engineering tau convention. *)
rgzConnectedAh[j_Integer] := Module[{a,xi,expected=0},
  a=rgzRows[[j]]["\:6709\:7406\:5f0f"];
  xi=rgzRows[[24+j]]["\:6709\:7406\:5f0f"];
  If[MemberQ[{1,5,6,7,8},j],xi=xi /. \[Mu]->\[Nu]];
  If[j===3,expected=-SUNDelta[Glu1,Glu2]];
  rgzRational[Contract[FCI[FVD[p,\[Mu]]] a+I rgzS xi]-expected]
];
rgzConnectionChecks=Association@Table["Ah:"<>ToString[j]->rgzConnectedAh[j],{j,8}];


(* ============================================================ *)
(* \:6781\:70b9\:5206\:89e3\:ff1a\:4ece\:5b9e\:9645\:6709\:7406\:5f0f\:8ba1\:7b97\:7559\:6570\:ff0c\:652f\:6301\:91cd\:6781\:70b9\:3002                 *)
(* \:8fd4\:56de\:503c\:4fdd\:7559 m1,m2,m3,m4\:ff0c\:4f7f\:7528\:524d\:4e0d\:8981\:7ed9\:8fd9\:56db\:4e2a\:7b26\:53f7\:8d4b\:503c\:3002          *)
(* ============================================================ *)

RGZInsertPoleMasses[x_] := Module[{avg=(m^2+\[Mu]^2)/2,
    gap=Sqrt[mG^4-(m^2-\[Mu]^2)^2/4]},
  x /. {m1->Sqrt[-I Sqrt[\[Alpha] M^4]],m2->Sqrt[I Sqrt[\[Alpha] M^4]],
    m3->Sqrt[avg+I gap],m4->Sqrt[avg-I gap]}
];

RGZSplitPropagator[x_,q_:p] := Module[
  {z=Unique["rgzPoleS"],r,num,den,poly,poles,part,out,root,mass,order,quot,f,k,c,check},
  r=FeynAmpDenominatorExplicit[FCI[If[ListQ[x],Total[x],x]]];
  r=ExpandScalarProduct[rgzNormalizeParameters[r]];
  r=r /. (HoldPattern[Pair[Momentum[q,dim1___],Momentum[q,dim2___]]] /;
    {dim1} === {dim2} :> z);
  r=Cancel[Together[r]]; num=Numerator[r]; den=Denominator[r];
  If[!PolynomialQ[num,z] || !PolynomialQ[den,z],
    Return[Failure["NonRational",<|"\:8868\:8fbe\:5f0f"->r|>]]];
  poly=PolynomialQuotient[num,den,z];
  poles={{0,0},{\[Mu]^2,\[Mu]},
    {-I Sqrt[\[Alpha] M^4],m1},{I Sqrt[\[Alpha] M^4],m2},
    {(m^2+\[Mu]^2)/2+I Sqrt[mG^4-(m^2-\[Mu]^2)^2/4],m3},
    {(m^2+\[Mu]^2)/2-I Sqrt[mG^4-(m^2-\[Mu]^2)^2/4],m4}};
  poles=DeleteDuplicates[poles,TrueQ[FullSimplify[#1[[1]]-#2[[1]]]==0]&];
  part=0;out=poly /. z->SPD[q,q];
  Do[
    root=FullSimplify[pole[[1]]]; mass=pole[[2]];
    If[TrueQ[FullSimplify[den /. z->root]==0],
      order=1;
      While[order<=Exponent[den,z] &&
        TrueQ[FullSimplify[D[den,{z,order}] /. z->root]==0],order++];
      quot=PolynomialQuotient[den,(z-root)^order,z] // FullSimplify;
      f=num/quot;
      Do[
        c=FullSimplify[(D[f,{z,order-k}] /. z->root)/Factorial[order-k]];
        part+=c/(z-root)^k;
        out+=c ChangeDimension[FCI[FAD@@ConstantArray[{q,mass},k]],D],
        {k,order}]
    ],{pole,poles}];
  check=FullSimplify[Cancel[Together[r-poly-part]]];
  If[!TrueQ[check===0],Return[Failure["IncompleteSplit",<|"\:4f59\:9879"->check|>]]];
  FCI[out]
];

(* ============================================================ *)
(* \:663e\:793a\:4e0e\:9010\:9879\:68c0\:67e5                                               *)
(* ============================================================ *)

rgzState[x_] := Which[Head[x]===Missing,"\:672a\:6bd4\:8f83",x===$Failed||FailureQ[x],
  "\:8ba1\:7b97\:5931\:8d25",TrueQ[x===0],"\:901a\:8fc7",True,"\:5dee\:503c\:672a\:5f52\:96f6"];
rgzPane[x_] := Which[Head[x]===Missing,"\:672a\:6bd4\:8f83",x===$Failed||FailureQ[x],x,
  True,Pane[TraditionalForm[x],ImageSize->{950,Automatic},Scrollbars->{True,False}]];
rgzCountTable=Grid[Prepend[Map[Function[spec,With[
  {states=rgzState /@ Lookup[rgzRows,spec[[2]]]},
  {spec[[1]],Count[states,"\:901a\:8fc7"],Count[states,"\:5dee\:503c\:672a\:5f52\:96f6"],
   Count[states,"\:672a\:6bd4\:8f83"],Count[states,"\:8ba1\:7b97\:5931\:8d25"]}]],
  {{"\:6a21\:578b\:4e0e\:5f53\:524d\:95f5\:6c0f\:516c\:5f0f","\:6a21\:578b\:51cf\:95f5\:6c0f"},{"\:6a21\:578b\:4e0e\:4e8c\:6b21\:6838\:ff08\:542b\:9b3c\:573a\:ff09","\:6a21\:578b\:51cf\:4e8c\:6b21\:6838"}}],
  {"\:6bd4\:8f83\:9879\:76ee","\:901a\:8fc7","\:5dee\:503c\:672a\:5f52\:96f6","\:672a\:6bd4\:8f83","\:8ba1\:7b97\:5931\:8d25"}],Frame->All,Alignment->Left];
Print[rgzCountTable];
Print[Grid[Prepend[MapIndexed[Function[{row,n},
  {First[n],row["\:573a"],row["\:751f\:6210\:72b6\:6001"],rgzState[row["\:6a21\:578b\:51cf\:95f5\:6c0f"]],
   rgzState[row["\:6a21\:578b\:51cf\:4e8c\:6b21\:6838"]]}],rgzRows],
  {"\:5e8f\:53f7","\:573a\:5bf9","\:751f\:6210\:60c5\:51b5","\:5f53\:524d\:95f5\:6c0f\:516c\:5f0f","\:4e8c\:6b21\:6838"}],Frame->All,Alignment->Left]];

rgzShowPropagator[n_Integer] := Module[{row,split,exact,diff,state},
  If[n<1||n>Length[rgzRows],Return[$Failed]];row=rgzRows[[n]];
  split=If[row["\:6709\:7406\:5f0f"]===$Failed,$Failed,RGZSplitPropagator[rgzInsertionToCovariance[row["FeynCalc\:632f\:5e45"]]]];
  exact=If[split===$Failed||FailureQ[split],$Failed,
    rgzRational[RGZInsertPoleMasses[split]]];
  diff=If[exact===$Failed,$Failed,rgzDifference[exact,row["\:6709\:7406\:5f0f"]]];
  Print[Style[Row[{"\:7b2c ",n," \:9879\:ff1a",row["\:573a"]}],Bold,14]];
  Print[Grid[{
    {"\:9879\:76ee","\:8868\:8fbe\:5f0f"},{"\:539f\:59cb FeynCalc \:632f\:5e45",rgzPane[row["FeynCalc\:632f\:5e45"]]},
    {"\:6a21\:578b\:6709\:7406\:5f0f",rgzPane[row["\:6709\:7406\:5f0f"]]},{"\:5f53\:524d\:95f5\:6c0f\:53c2\:8003",rgzPane[row["\:95f5\:6c0f\:53c2\:8003"]]},
    {"\:4e8c\:6b21\:6838\:53c2\:8003",rgzPane[row["\:4e8c\:6b21\:6838\:516c\:5f0f"]]},
    {"\:6a21\:578b\:51cf\:95f5\:6c0f",rgzPane[row["\:6a21\:578b\:51cf\:95f5\:6c0f"]]},
    {"\:6a21\:578b\:51cf\:4e8c\:6b21\:6838",rgzPane[row["\:6a21\:578b\:51cf\:4e8c\:6b21\:6838"]]},
    {"\:6781\:70b9\:5206\:89e3\:ff08\:4fdd\:7559 m1--m4\:ff09",rgzPane[split]},
    {"\:4ee3\:5165\:6781\:70b9\:8d28\:91cf",rgzPane[exact]},{"\:6781\:70b9\:5206\:89e3\:5dee\:503c",rgzPane[diff]},
    {"\:6781\:70b9\:5206\:89e3\:68c0\:67e5",rgzState[diff]},
    {"\:65e7\:4ee3\:7801 PDF \:8f6c\:5f55\:5f0f\:53f7",row["PDF\:5f0f\:53f7"]},
    {"\:65e7\:4ee3\:7801 PDF \:8f6c\:5f55\:503c\:ff08\:7ea6\:5b9a\:672a\:7edf\:4e00\:ff09",rgzPane[row["\:65e7PDF\:53c2\:8003"]]},
    {"\:6a21\:578b\:51cf\:65e7 PDF \:8f6c\:5f55\:503c",rgzPane[row["\:6a21\:578b\:51cf\:65e7PDF"]]}
    },Frame->All,Alignment->{Left,Top}]]
];
Null;

(* \:5176\:4ed6\:9879\:76ee\:ff1argzShowPropagator[4]; rgzShowPropagator[12]; *)
