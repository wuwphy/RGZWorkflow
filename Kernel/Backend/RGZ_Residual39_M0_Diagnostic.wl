(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* RGZ residual-39 diagnostic, v1.0

   Input: RGZ_Gauge_Residual_39.wxf exported from the completed comparison.
   ParameterPoint -> Automatic uses the point below; exact partial overrides
   are accepted for spacelike, positive-mass, nondegenerate diagnostic points.
   No diagrams, TID, mixed matrices or original coefficient caches are changed.

   This is an exact diagnostic point, NOT a general gauge-independence proof:
     p^2=-11, m^2=9, mu^2=1, mG^4=25, N=SUNN=3,
     pole3^2=5+3 I, pole4^2=5-3 I, reference alpha=1.
   Units can be restored with a common mass scale Lambda.
   mG^4=2 N gs^2 gamma^4 is satisfied by gamma^4=25/(6 gs^2).

   The overall Pi^2 SMP["g_s"]^2 is stripped from coefficients.
   Put e=M^2>0 and h=Sqrt[alpha]>0, keeping D symbolic throughout.

   M->0 prescription: take the massless limit at generic dimension where the
   endpoint derivatives converge (Re[D]>6 suffices here), and analytically
   continue the resulting master integrals to D=4-2 epsilon. Small-mass A0
   hard-region terms are scaleless and vanish in dimensional regularization.
   This is NOT a direct substitution M->0 into finite-M coefficients, and is
   NOT an assertion that the epsilon and infrared-regulator limits commute.

   Exact mass derivatives used, with Z a nonzero mass SQUARED:
     d_a B0(s,0,0)=-(D-3) B0(s,0,0)/s;
     d_aa B0(s,0,0)=(D-4)(D-3) B0(s,0,0)/s^2;
     d_ab B0(s,0,0)=(D-6)(D-3) B0(s,0,0)/s^2;
     d_a B0(s,0,Z)=((D-2) A0(Z)-(D-3)(s+Z) B0(s,0,Z))/(s-Z)^2.

   UV convention, D=4-2 epsilon:
     A0(Z)|UV=Z/epsilon, B0|UV=1/epsilon.
   https://feyncalc.github.io/FeynCalcBook/PaVeUVPart.html

   Usage (save this file beside the exported WXF):
     Get[FileNameJoin[{RGZWorkflow`Private`cacheDirectory[],"RGZ_Residual39_M0_Diagnostic.wl"}]];
     rgz39Check = RGZ39M0Check[Automatic,60];
     RGZ39M0Summary[rgz39Check];
     RGZ39M0Show[rgz39Check];  (* only seven compact master coefficients *)

   Algebra and integral identities were independently checked using exact
   Python symbolic algebra and high-precision Feynman-parameter integrals.
   Native Mathematica/FeynCalc execution has not been tested here.
*)

BeginPackage["RGZResidual39Diagnostic`"];
RGZ39M0Check::usage = "RGZ39M0Check[file,seconds] reads the 39 residuals and checks an exact M->0 diagnostic point. Automatic uses the directory of this .wl file.";
RGZ39M0Summary::usage = "RGZ39M0Summary[result] prints a small diagnostic table.";
RGZ39M0Show::usage = "RGZ39M0Show[result] displays the compact M->0 master coefficients, with D retained.";
Begin["`Private`"];

$sourceDirectory = If[StringQ[$InputFileName] && StringLength[$InputFileName]>0,
  DirectoryName[ExpandFileName[$InputFileName]], Directory[]];
Clear[e,h,dim,a0,b0];
$progress = "Prepare";
$stopTag = "RGZ39DiagnosticStopped";

Clear[bounded];
SetAttributes[bounded, HoldRest];
bounded[label_, body_, seconds_] := Module[{answer},
  $progress=label;
  answer=TimeConstrained[body, seconds,
    Failure["DiagnosticTimeout",<|"Operation"->label,"TimeLimit"->seconds|>]];
  If[FailureQ[answer],Throw[answer,$stopTag]];
  If[answer===$Aborted || answer===$Failed,
    Throw[Failure["DiagnosticFailed",<|"Operation"->label|>],$stopTag]];
  answer
];

$defaultPoint=<|"p^2"->-11,"m^2"->9,"mu^2"->1,"mG^4"->25,"N"->3|>;
normalizePoint[value_] := Module[{point,disc},
 If[value===Automatic,Return[$defaultPoint]];
 If[!AssociationQ[value] || !SubsetQ[Keys[$defaultPoint],Keys[value]],Return[Failure["InvalidParameterPoint",<|"Explanation"->"Use an association with keys p^2, m^2, mu^2, mG^4, N."|>]]];
 point=Join[$defaultPoint,value];
 If[!AllTrue[Values[point],MatchQ[#,_Integer|_Rational]&] ||
 !TrueQ[point["p^2"]<0] || !AllTrue[Lookup[point,{"m^2","mu^2","mG^4"}],#>0&] || !IntegerQ[point["N"]] || point["N"]<2,
 Return[Failure["InvalidParameterPoint",<|"Explanation"->"Require exact rational spacelike p^2<0, positive m^2, mu^2, mG^4 and integer N>=2; use 1/2 rather than 0.5."|>]]];
 disc=(point["m^2"]-point["mu^2"])^2-4 point["mG^4"];
 If[disc===0,Return[Failure["DegenerateParameterPoint",<|"Explanation"->"Repeated transverse poles require a separate limit prescription."|>]]];
 point];

Clear[pointExpression];
pointExpression[input_] := Module[{expr},
  (* Inert PV heads prevent incidental simplifications while forming Taylor terms. *)
  expr=input /. {
    HoldPattern[FeynCalc`A0[v_]] :> a0[v],
    HoldPattern[FeynCalc`B0[v_,u_,w_]] :> b0[v,u,w]
  };
  expr=expr /. {
    HoldPattern[FeynCalc`Pair[FeynCalc`Momentum[RGZWorkflow`Engine`p,___],FeynCalc`Momentum[RGZWorkflow`Engine`p,___]]] -> $parameterPoint["p^2"],
    HoldPattern[FeynCalc`SPD[RGZWorkflow`Engine`p,RGZWorkflow`Engine`p]] -> $parameterPoint["p^2"],
    HoldPattern[FeynCalc`SMP["g_s"]] -> 1,
    HoldPattern[System`N] -> $parameterPoint["N"],
    HoldPattern[FeynCalc`SUNN] -> $parameterPoint["N"],
    HoldPattern[RGZWorkflow`Engine`m] -> Sqrt[$parameterPoint["m^2"]],
    HoldPattern[RGZWorkflow`Engine`μ] -> Sqrt[$parameterPoint["mu^2"]],
    HoldPattern[RGZWorkflow`Engine`mG] -> $parameterPoint["mG^4"]^(1/4),
    HoldPattern[RGZWorkflow`Engine`M] -> Sqrt[e],
    HoldPattern[RGZWorkflow`Engine`α] -> h^2,
    HoldPattern[System`D] -> dim
  };
  (* These specific root identities use e>0,h>0; PowerExpand is never used. *)
  expr=expr /. {
    HoldPattern[(e^2 h^2)^k_Rational] :> (e h)^(2 k),
    HoldPattern[(e^2)^k_Rational] :> e^(2 k),
    HoldPattern[(h^2)^k_Rational] :> h^(2 k)
  };
  expr
];

Clear[bubbleSmall,bubbleMixed,pvTaylor];
bubbleSmall[sp_,i_,j_] := b0[sp,0,0] (1-e (dim-3)(i+j)/sp+
  e^2 ((i^2+j^2)(dim-4)(dim-3)/2+i j(dim-6)(dim-3))/sp^2);
bubbleMixed[sp_,k_,zz_] := b0[sp,0,zz]+
  e k ((dim-2)a0[zz]-(dim-3)(sp+zz)b0[sp,0,zz])/(sp-zz)^2;
pvTaylor[a0[zz_]] := If[!FreeQ[zz,e] || TrueQ[zz===0],0,a0[zz]];
pvTaylor[b0[sp_,aa_,bb_]] := Module[{sa,sb},
  sa=TrueQ[Cancel[aa /. e->0]===0];
  sb=TrueQ[Cancel[bb /. e->0]===0];
  Which[
    sa && sb, bubbleSmall[sp,Cancel[aa/e],Cancel[bb/e]],
    sa, bubbleMixed[sp,Cancel[aa/e],bb],
    sb, bubbleMixed[sp,Cancel[bb/e],aa],
    True, b0[sp,aa,bb]
  ]
];

Clear[prettyCoefficient,publicMaster];
prettyCoefficient[expr_] := Module[{q},
  q=Expand[expr] /. {HoldPattern[h^n_Integer] /; EvenQ[n] :>
    RGZWorkflow`Engine`α^(n/2)};
  Factor[Cancel[q /. {h->Sqrt[RGZWorkflow`Engine`α],dim->System`D}]]
];
publicMaster[expr_] := expr /. {a0->FeynCalc`A0,b0->FeynCalc`B0};

Clear[reducePV];
reducePV[expr_] := Module[{expanded,ml,constant},
  expanded=Expand[expr];
  ml=DeleteDuplicates@Cases[expanded,_a0|_b0,Infinity];
  constant=expanded /. Thread[ml->ConstantArray[0,Length[ml]]];
  Factor[Cancel[constant]]+
    Total[(Factor[Cancel[Coefficient[expanded,#]]] #)& /@ ml]
];

Clear[rationalLaurent];
rationalLaurent[co_] := Module[{nn,dd,nmin,dmin,low,d0,values,k,j,next},
  If[co===0,Return[0]];
  nn=Numerator[co];dd=Denominator[co];
  nmin=Exponent[nn,e,Min];dmin=Exponent[dd,e,Min];low=nmin-dmin;
  If[low>0,Return[0]];
  d0=Coefficient[dd,e,dmin];values=ConstantArray[0,1-low];
  Do[
    next=Coefficient[nn,e,nmin+k]-
      Sum[Coefficient[dd,e,dmin+j] values[[k-j+1]],{j,1,k}];
    values[[k+1]]=Factor[Cancel[next/d0]],
    {k,0,-low}
  ];
  Total[Table[values[[k+1]] e^(low+k),{k,0,-low}]]
];

Clear[checkCore];
checkCore[file_,seconds_] := Block[
  {RGZWorkflow`Engine`m,RGZWorkflow`Engine`mG,RGZWorkflow`Engine`M,RGZWorkflow`Engine`α,RGZWorkflow`Engine`μ,RGZWorkflow`Engine`p,FeynCalc`SUNN},
  Module[{packet,rows,done={},contributions={},uvTerms={},r,i,master,co,nn,dd,
    low,laurent,whole,negative4,negative2,finite,masters,coeffs,uv,uv4,
    uvFiniteM,uvFiniteM0,zeros,expected,table,unknown,allLaurent,hash},
    packet=bounded["Read 39 Term WXF",Quiet[Check[Import[file,"WXF"],$Failed]],seconds];
    If[!AssociationQ[packet],Throw[Failure["InvalidPacket",<||>],$stopTag]];
    rows=Lookup[packet,"ResidualParts",Missing["NoRows"]];
    If[!ListQ[rows] || Length[rows]=!=39 || !AllTrue[rows,AssociationQ],
      Throw[Failure["InvalidRows",<|"Description"->"Provide a residual-term list containing 39 Associations."|>],$stopTag]];
    If[!TrueQ[Lookup[packet,"ReferenceGauge",Missing[]]===1],
      Throw[Failure["DifferentReferenceGauge",<|"Description"->"This diagnostic uses reference gauge 1."|>],$stopTag]];
    Do[
      r=rows[[i]];
      If[!And@@(KeyExistsQ[r,#]& /@ {"ID","MasterIntegral","ResidualCoefficient"}),
        Throw[Failure["MissingRowKey",<|"ID"->i|>],$stopTag]];
      master=pointExpression[r["MasterIntegral"]];
      If[!MatchQ[master,_a0|_b0],
        Throw[Failure["UnsupportedMaster",<|"ID"->i|>],$stopTag]];
      co=bounded["ExactParameterPoint: Coefficient "<>ToString[i]<>" / 39",
        Factor[Cancel[pointExpression[r["ResidualCoefficient"]]/Pi^2]],seconds];
      unknown=DeleteDuplicates@Cases[co,sym_Symbol /;
        !MemberQ[{e,h,dim,System`I},sym] && Context[sym]=!="System`",Infinity];
      If[unknown=!={},Throw[Failure["UnmappedSymbols",<|"ID"->i,"Symbol"->unknown|>],$stopTag]];
      If[co===0,
        laurent=0,
        nn=Numerator[co];dd=Denominator[co];
        If[!PolynomialQ[nn,e] || !PolynomialQ[dd,e],
          Throw[Failure["NonRationalSmallMass",<|"ID"->i|>],$stopTag]];
        low=Exponent[nn,e,Min]-Exponent[dd,e,Min];
        If[low < -2,Throw[Failure["HigherMassPole",<|"ID"->i,"MinPower"->low|>],$stopTag]];
        laurent=bounded["SmallMass Laurent Expand: "<>ToString[i]<>" / 39",
          rationalLaurent[co],seconds]
      ];
      AppendTo[done,<|"ID"->r["ID"],"MasterIntegral"->master,"Coefficient"->co,
        "Laurent"->laurent,"Iswhether Zero"->TrueQ[co===0]|>];
      If[co=!=0,
        AppendTo[contributions,Expand[laurent pvTaylor[master]]];
        AppendTo[uvTerms,laurent If[Head[master]===a0,master[[1]],1]]
      ],
      {i,Length[rows]}
    ];
    allLaurent=bounded["CombineSmallMassPoleAndFiniteTerm",Expand[e^2 Total[contributions]],seconds];
    negative4=bounded["Verify M^-4 Coefficient",reducePV[Coefficient[allLaurent,e,0]],seconds];
    negative2=bounded["Verify M^-2 Coefficient",reducePV[Coefficient[allLaurent,e,1]],seconds];
    finite=Coefficient[allLaurent,e,2];
    masters=DeleteDuplicates@Cases[finite,_a0|_b0,Infinity];
    coeffs=Table[bounded["FiniteMasterIntegralCoefficient: "<>ToString[i]<>" / "<>ToString[Length[masters]],
      Factor[Cancel[Coefficient[finite,masters[[i]]]]],seconds],{i,Length[masters]}];
    finite=Total[MapThread[Times,{coeffs,masters}]];
    uv=bounded["M0 UV pole coefficient",
      Factor[Cancel[Total[MapThread[#1 If[Head[#2]===a0,#2[[1]],1]&,{coeffs,masters}]]]],seconds];
    uv4=Factor[Cancel[uv /. dim->4]];
    uvFiniteM=bounded["Comparison: FiniteMfirst extract UVthen take M0",
      Expand[e^2 Total[uvTerms]],seconds];
    uvFiniteM0=Factor[Cancel[Coefficient[uvFiniteM,e,2]]];
    zeros=Lookup[Select[done,TrueQ[#["Iswhether Zero"]]&],"ID",{}];
    expected=8751 I(h^2-1)/70225;
    table=MapThread[<|"MasterIntegral"->publicMaster[#1],"Coefficient"->prettyCoefficient[#2]|>&,
      {masters,coeffs}];
    hash=Quiet[Check[IntegerString[FileHash[file,"SHA256"],16,64],Missing["Unavailable"]]];
    $progress="Complete";
    <|"Status"->m0DiagnosticStatus[negative4,negative2,uv4,finite],
      "InputFile"->file,"SHA256"->hash,"ReferenceGauge"->1,
      "ParameterPoint"-><|"p^2"->$parameterPoint["p^2"],"m^2"->$parameterPoint["m^2"],"mu^2"->$parameterPoint["mu^2"],"mG^4"->$parameterPoint["mG^4"],"N=SUNN"->$parameterPoint["N"]|>,
      "Strip the overall factor"->"Pi^2 SMP[\"g_s\"]^2",
      "FiniteMZeroID"->zeros,"FiniteMNonzeroTermCount"->39-Length[zeros],
      "M^-4Coefficient"->publicMaster[prettyCoefficient[negative4]],
      "M^-2Coefficient"->publicMaster[prettyCoefficient[negative2]],
      "M0MasterIntegralCoefficient"->table,
      "M0Result"->publicMaster[prettyCoefficient[finite]],
      "M0UVCoefficient"->prettyCoefficient[uv],
      "M0EpsilonPoleCoefficient"->prettyCoefficient[uv4],
      "IndependentExactResultComparison"->If[$parameterPoint===$defaultPoint,TrueQ[Cancel[uv4-expected]===0],Missing["NotApplicableToCustomPoint"]],
      "UVBeforeM0UVCoefficient"->prettyCoefficient[uvFiniteM0],
      "UVBeforeM0EpsilonPoleCoefficient"->prettyCoefficient[uvFiniteM0 /. dim->4],
      "Convention"->"Take M0 at Re[D]>6 before dimensional analytic continuation; this does not interchange the epsilon and M limits.",
      "Scope"->"Only the supplied candidate combination is checked; this cannot refute BRST gauge independence of the full S matrix."|>
  ]
];

(* Classify the complete limit, not only its UV pole. *)
m0DiagnosticStatus[n4_,n2_,uv_,finite_] := Which[
 !TrueQ[n4===0] || !TrueQ[n2===0], "M0 diagnostic: small-mass divergence",
 !TrueQ[uv===0], "M0 diagnostic: UV residual",
 !TrueQ[finite===0], "M0 diagnostic: finite residual",
 True, "M0 diagnostic passed: residual cancels at this parameter point"];

Options[RGZ39M0Check]={"ShowProgress"->True,"ParameterPoint"->Automatic};
RGZ39M0Check[file_:Automatic,seconds_:60,OptionsPattern[]] := Module[{path,run,point},
  point=normalizePoint[OptionValue["ParameterPoint"]];If[FailureQ[point],Return[point]];
  If[!NumericQ[seconds] || !TrueQ[seconds>0],Return[Failure["InvalidTimeLimit",<||>]]];
  If[OwnValues[RGZWorkflow`Engine`α]=!={},Return[Failure["AssignedGaugeParameter",
    <|"Description"->"Clear alpha first. This diagnostic requires symbolic alpha to prevent assigned reference-gauge values from removing the residual."|>]]];
  path=If[file===Automatic,FileNameJoin[{$sourceDirectory,"RGZ_Gauge_Residual_39.wxf"}],file];
  If[!StringQ[path] || !FileExistsQ[path],Return[Failure["InputFileNotFound",<|"File"->path|>]]];
  $progress="Prepare";
  If[TrueQ[OptionValue["ShowProgress"]],
    Monitor[Block[{$parameterPoint=point},Catch[checkCore[path,seconds],$stopTag]],Style[$progress,11]],
    Block[{$parameterPoint=point},Catch[checkCore[path,seconds],$stopTag]]
  ]
];

RGZ39M0Summary[result_] := If[AssociationQ[result],
  Grid[{
    {"CheckParts","ResultWithPiSquaredGsSquaredStripped"},
    {"ExactParameterPoint",result["ParameterPoint"]},
    {"FiniteMZeroID",result["FiniteMZeroID"]},
    {"FiniteMNonzeroTermCount",result["FiniteMNonzeroTermCount"]},
    {"M^-4Coefficient",result["M^-4Coefficient"]},
    {"M^-2Coefficient",result["M^-2Coefficient"]},
    {"M0MasterIntegralCount",Length[result["M0MasterIntegralCoefficient"]]},
    {"M0EpsilonPoleCoefficient",TraditionalForm[result["M0EpsilonPoleCoefficient"]]},
    {"IndependentExactResultComparison",result["IndependentExactResultComparison"]},
    {"Status",result["Status"]}
  },Frame->All,Alignment->Left,Spacings->{1.5,0.8}],
  If[FailureQ[result],Grid[{{"StoppedTag",result[[1]]},{"Information",result[[2]]}},Frame->All],
    "No valid diagnostic result."]
];
RGZ39M0Show[result_Association] := Grid[
  Prepend[({TraditionalForm[#["MasterIntegral"]],TraditionalForm[#["Coefficient"]]}& /@
    result["M0MasterIntegralCoefficient"]),{"MasterIntegral","CoefficientWithPiSquaredGsSquaredStrippedKeepingD"}],
  Frame->All,Alignment->Left,Spacings->{1.5,0.8}];

End[];
EndPackage[];
Print["39-term M0 check loaded: rgz39Check=RGZ39M0Check[Automatic,60]; RGZ39M0Summary[rgz39Check]. Diagrams are not recomputed."];
