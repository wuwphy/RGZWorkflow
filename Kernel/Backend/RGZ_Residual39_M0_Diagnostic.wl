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
$progress = "\:51c6\:5907";
$stopTag = "RGZ39DiagnosticStopped";

Clear[bounded];
SetAttributes[bounded, HoldRest];
bounded[label_, body_, seconds_] := Module[{answer},
  $progress=label;
  answer=TimeConstrained[body, seconds,
    Failure["DiagnosticTimeout",<|"\:64cd\:4f5c"->label,"\:65f6\:9650"->seconds|>]];
  If[FailureQ[answer],Throw[answer,$stopTag]];
  If[answer===$Aborted || answer===$Failed,
    Throw[Failure["DiagnosticFailed",<|"\:64cd\:4f5c"->label|>],$stopTag]];
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
    HoldPattern[RGZWorkflow`Engine`\:03bc] -> Sqrt[$parameterPoint["mu^2"]],
    HoldPattern[RGZWorkflow`Engine`mG] -> $parameterPoint["mG^4"]^(1/4),
    HoldPattern[RGZWorkflow`Engine`M] -> Sqrt[e],
    HoldPattern[RGZWorkflow`Engine`\:03b1] -> h^2,
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
    RGZWorkflow`Engine`\:03b1^(n/2)};
  Factor[Cancel[q /. {h->Sqrt[RGZWorkflow`Engine`\:03b1],dim->System`D}]]
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
  {RGZWorkflow`Engine`m,RGZWorkflow`Engine`mG,RGZWorkflow`Engine`M,RGZWorkflow`Engine`\:03b1,RGZWorkflow`Engine`\:03bc,RGZWorkflow`Engine`p,FeynCalc`SUNN},
  Module[{packet,rows,done={},contributions={},uvTerms={},r,i,master,co,nn,dd,
    low,laurent,whole,negative4,negative2,finite,masters,coeffs,uv,uv4,
    uvFiniteM,uvFiniteM0,zeros,expected,table,unknown,allLaurent,hash},
    packet=bounded["\:8bfb\:53d6 39 \:9879 WXF",Quiet[Check[Import[file,"WXF"],$Failed]],seconds];
    If[!AssociationQ[packet],Throw[Failure["InvalidPacket",<||>],$stopTag]];
    rows=Lookup[packet,"\:6b8b\:5dee\:5206\:9879",Missing["NoRows"]];
    If[!ListQ[rows] || Length[rows]=!=39 || !AllTrue[rows,AssociationQ],
      Throw[Failure["InvalidRows",<|"\:8bf4\:660e"->"\:9700\:8981\:542b39\:4e2aAssociation\:7684\:6b8b\:5dee\:5206\:9879\:5217\:8868\:3002"|>],$stopTag]];
    If[!TrueQ[Lookup[packet,"\:53c2\:8003\:89c4\:8303",Missing[]]===1],
      Throw[Failure["DifferentReferenceGauge",<|"\:8bf4\:660e"->"\:6b64\:8bca\:65ad\:7684\:5bf9\:7167\:503c\:91c7\:7528\:53c2\:8003\:89c4\:83031\:3002"|>],$stopTag]];
    Do[
      r=rows[[i]];
      If[!And@@(KeyExistsQ[r,#]& /@ {"\:7f16\:53f7","\:4e3b\:79ef\:5206","\:6b8b\:5dee\:7cfb\:6570"}),
        Throw[Failure["MissingRowKey",<|"\:7f16\:53f7"->i|>],$stopTag]];
      master=pointExpression[r["\:4e3b\:79ef\:5206"]];
      If[!MatchQ[master,_a0|_b0],
        Throw[Failure["UnsupportedMaster",<|"\:7f16\:53f7"->i|>],$stopTag]];
      co=bounded["\:7cbe\:786e\:53c2\:6570\:70b9\:ff1a\:7cfb\:6570 "<>ToString[i]<>" / 39",
        Factor[Cancel[pointExpression[r["\:6b8b\:5dee\:7cfb\:6570"]]/Pi^2]],seconds];
      unknown=DeleteDuplicates@Cases[co,sym_Symbol /;
        !MemberQ[{e,h,dim,System`I},sym] && Context[sym]=!="System`",Infinity];
      If[unknown=!={},Throw[Failure["UnmappedSymbols",<|"\:7f16\:53f7"->i,"\:7b26\:53f7"->unknown|>],$stopTag]];
      If[co===0,
        laurent=0,
        nn=Numerator[co];dd=Denominator[co];
        If[!PolynomialQ[nn,e] || !PolynomialQ[dd,e],
          Throw[Failure["NonRationalSmallMass",<|"\:7f16\:53f7"->i|>],$stopTag]];
        low=Exponent[nn,e,Min]-Exponent[dd,e,Min];
        If[low < -2,Throw[Failure["HigherMassPole",<|"\:7f16\:53f7"->i,"\:6700\:4f4e\:5e42"->low|>],$stopTag]];
        laurent=bounded["\:5c0f\:8d28\:91cf Laurent \:5c55\:5f00\:ff1a"<>ToString[i]<>" / 39",
          rationalLaurent[co],seconds]
      ];
      AppendTo[done,<|"\:7f16\:53f7"->r["\:7f16\:53f7"],"\:4e3b\:79ef\:5206"->master,"\:7cfb\:6570"->co,
        "Laurent"->laurent,"\:662f\:5426\:5f52\:96f6"->TrueQ[co===0]|>];
      If[co=!=0,
        AppendTo[contributions,Expand[laurent pvTaylor[master]]];
        AppendTo[uvTerms,laurent If[Head[master]===a0,master[[1]],1]]
      ],
      {i,Length[rows]}
    ];
    allLaurent=bounded["\:5408\:5e76\:5c0f\:8d28\:91cf\:6781\:70b9\:4e0e\:6709\:9650\:9879",Expand[e^2 Total[contributions]],seconds];
    negative4=bounded["\:6838\:5bf9 M^-4 \:7cfb\:6570",reducePV[Coefficient[allLaurent,e,0]],seconds];
    negative2=bounded["\:6838\:5bf9 M^-2 \:7cfb\:6570",reducePV[Coefficient[allLaurent,e,1]],seconds];
    finite=Coefficient[allLaurent,e,2];
    masters=DeleteDuplicates@Cases[finite,_a0|_b0,Infinity];
    coeffs=Table[bounded["\:6709\:9650\:4e3b\:79ef\:5206\:7cfb\:6570\:ff1a"<>ToString[i]<>" / "<>ToString[Length[masters]],
      Factor[Cancel[Coefficient[finite,masters[[i]]]]],seconds],{i,Length[masters]}];
    finite=Total[MapThread[Times,{coeffs,masters}]];
    uv=bounded["M0 \:7ed3\:679c\:7684\:7d2b\:5916\:6781\:70b9\:7cfb\:6570",
      Factor[Cancel[Total[MapThread[#1 If[Head[#2]===a0,#2[[1]],1]&,{coeffs,masters}]]]],seconds];
    uv4=Factor[Cancel[uv /. dim->4]];
    uvFiniteM=bounded["\:5bf9\:7167\:ff1a\:6709\:9650M\:5148\:63d0\:53d6UV\:518d\:53d6M0",
      Expand[e^2 Total[uvTerms]],seconds];
    uvFiniteM0=Factor[Cancel[Coefficient[uvFiniteM,e,2]]];
    zeros=Lookup[Select[done,TrueQ[#["\:662f\:5426\:5f52\:96f6"]]&],"\:7f16\:53f7",{}];
    expected=8751 I(h^2-1)/70225;
    table=MapThread[<|"\:4e3b\:79ef\:5206"->publicMaster[#1],"\:7cfb\:6570"->prettyCoefficient[#2]|>&,
      {masters,coeffs}];
    hash=Quiet[Check[IntegerString[FileHash[file,"SHA256"],16,64],Missing["Unavailable"]]];
    $progress="\:5b8c\:6210";
    <|"\:72b6\:6001"->m0DiagnosticStatus[negative4,negative2,uv4,finite],
      "\:8f93\:5165\:6587\:4ef6"->file,"SHA256"->hash,"\:53c2\:8003\:89c4\:8303"->1,
      "\:53c2\:6570\:70b9"-><|"p^2"->$parameterPoint["p^2"],"m^2"->$parameterPoint["m^2"],"mu^2"->$parameterPoint["mu^2"],"mG^4"->$parameterPoint["mG^4"],"N=SUNN"->$parameterPoint["N"]|>,
      "\:5265\:79bb\:6574\:4f53\:56e0\:5b50"->"Pi^2 SMP[\"g_s\"]^2",
      "\:6709\:9650M\:5f52\:96f6\:7f16\:53f7"->zeros,"\:6709\:9650M\:975e\:96f6\:9879\:6570"->39-Length[zeros],
      "M^-4\:7cfb\:6570"->publicMaster[prettyCoefficient[negative4]],
      "M^-2\:7cfb\:6570"->publicMaster[prettyCoefficient[negative2]],
      "M0\:4e3b\:79ef\:5206\:7cfb\:6570"->table,
      "M0\:7ed3\:679c"->publicMaster[prettyCoefficient[finite]],
      "M0\:7684UV\:7cfb\:6570"->prettyCoefficient[uv],
      "M0\:76841/epsilon\:7cfb\:6570"->prettyCoefficient[uv4],
      "\:72ec\:7acb\:7cbe\:786e\:7ed3\:679c\:5bf9\:7167"->If[$parameterPoint===$defaultPoint,TrueQ[Cancel[uv4-expected]===0],Missing["NotApplicableToCustomPoint"]],
      "\:5148UV\:540eM0\:7684UV\:7cfb\:6570"->prettyCoefficient[uvFiniteM0],
      "\:5148UV\:540eM0\:76841/epsilon\:7cfb\:6570"->prettyCoefficient[uvFiniteM0 /. dim->4],
      "\:7ea6\:5b9a"->"\:5148\:5728Re[D]>6\:53d6M0\:ff0c\:518d\:4f5c\:7ef4\:6570\:89e3\:6790\:5ef6\:62d3\:ff1b\:4e0d\:662fepsilon\:4e0eM\:6781\:9650\:4ea4\:6362\:3002",
      "\:8303\:56f4"->"\:53ea\:68c0\:67e5\:4e0a\:4f20\:7684\:5019\:9009\:7ec4\:5408\:ff0c\:4e0d\:80fd\:636e\:6b64\:5426\:5b9a\:5b8c\:6574S\:77e9\:9635\:7684BRST\:89c4\:8303\:65e0\:5173\:6027\:3002"|>
  ]
];

(* Classify the complete limit, not only its UV pole. *)
m0DiagnosticStatus[n4_,n2_,uv_,finite_] := Which[
 !TrueQ[n4===0] || !TrueQ[n2===0], "M0\:8bca\:65ad\:ff1a\:5b58\:5728\:5c0f\:8d28\:91cf\:53d1\:6563",
 !TrueQ[uv===0], "M0\:8bca\:65ad\:ff1a\:5b58\:5728\:7d2b\:5916\:6b8b\:5dee",
 !TrueQ[finite===0], "M0\:8bca\:65ad\:ff1a\:5b58\:5728\:6709\:9650\:6b8b\:5dee",
 True, "M0\:8bca\:65ad\:901a\:8fc7\:ff1a\:6b64\:53c2\:6570\:70b9\:6b8b\:5dee\:5b8c\:5168\:62b5\:6d88"];

Options[RGZ39M0Check]={"\:663e\:793a\:8fdb\:5ea6"->True,"ParameterPoint"->Automatic};
RGZ39M0Check[file_:Automatic,seconds_:60,OptionsPattern[]] := Module[{path,run,point},
  point=normalizePoint[OptionValue["ParameterPoint"]];If[FailureQ[point],Return[point]];
  If[!NumericQ[seconds] || !TrueQ[seconds>0],Return[Failure["InvalidTimeLimit",<||>]]];
  If[OwnValues[RGZWorkflow`Engine`\:03b1]=!={},Return[Failure["AssignedGaugeParameter",
    <|"\:8bf4\:660e"->"\:5148 Clear[\:03b1]\:ff1b\:6b64\:8bca\:65ad\:4fdd\:7559\:03b1\:4e3a\:7b26\:53f7\:ff0c\:907f\:514d\:53c2\:8003\:89c4\:8303\:7684\:8d4b\:503c\:4f7f\:6b8b\:5dee\:6d88\:5931\:3002"|>]]];
  path=If[file===Automatic,FileNameJoin[{$sourceDirectory,"RGZ_Gauge_Residual_39.wxf"}],file];
  If[!StringQ[path] || !FileExistsQ[path],Return[Failure["InputFileNotFound",<|"\:6587\:4ef6"->path|>]]];
  $progress="\:51c6\:5907";
  If[TrueQ[OptionValue["\:663e\:793a\:8fdb\:5ea6"]],
    Monitor[Block[{$parameterPoint=point},Catch[checkCore[path,seconds],$stopTag]],Style[$progress,11]],
    Block[{$parameterPoint=point},Catch[checkCore[path,seconds],$stopTag]]
  ]
];

RGZ39M0Summary[result_] := If[AssociationQ[result],
  Grid[{
    {"\:68c0\:67e5\:9879\:76ee","\:7ed3\:679c\:ff08\:5df2\:5265\:79bb Pi^2 g_s^2\:ff09"},
    {"\:7cbe\:786e\:53c2\:6570\:70b9",result["\:53c2\:6570\:70b9"]},
    {"\:6709\:9650M\:5f52\:96f6\:7f16\:53f7",result["\:6709\:9650M\:5f52\:96f6\:7f16\:53f7"]},
    {"\:6709\:9650M\:975e\:96f6\:9879\:6570",result["\:6709\:9650M\:975e\:96f6\:9879\:6570"]},
    {"M^-4\:7cfb\:6570",result["M^-4\:7cfb\:6570"]},
    {"M^-2\:7cfb\:6570",result["M^-2\:7cfb\:6570"]},
    {"M0\:4e3b\:79ef\:5206\:6570",Length[result["M0\:4e3b\:79ef\:5206\:7cfb\:6570"]]},
    {"M0\:76841/epsilon\:7cfb\:6570",TraditionalForm[result["M0\:76841/epsilon\:7cfb\:6570"]]},
    {"\:72ec\:7acb\:7cbe\:786e\:7ed3\:679c\:5bf9\:7167",result["\:72ec\:7acb\:7cbe\:786e\:7ed3\:679c\:5bf9\:7167"]},
    {"\:72b6\:6001",result["\:72b6\:6001"]}
  },Frame->All,Alignment->Left,Spacings->{1.5,0.8}],
  If[FailureQ[result],Grid[{{"\:505c\:6b62\:6807\:7b7e",result[[1]]},{"\:4fe1\:606f",result[[2]]}},Frame->All],
    "\:672a\:5f97\:5230\:6709\:6548\:8bca\:65ad\:7ed3\:679c\:3002"]
];
RGZ39M0Show[result_Association] := Grid[
  Prepend[({TraditionalForm[#["\:4e3b\:79ef\:5206"]],TraditionalForm[#["\:7cfb\:6570"]]}& /@
    result["M0\:4e3b\:79ef\:5206\:7cfb\:6570"]),{"\:4e3b\:79ef\:5206","\:7cfb\:6570\:ff08\:5df2\:5265\:79bb Pi^2 g_s^2\:ff1bD\:4ecd\:4fdd\:7559\:ff09"}],
  Frame->All,Alignment->Left,Spacings->{1.5,0.8}];

End[];
EndPackage[];
Print["\:5df2\:52a0\:8f7d39\:9879M0\:590d\:6838\:ff1argz39Check=RGZ39M0Check[Automatic,60]; RGZ39M0Summary[rgz39Check]\:3002\:4e0d\:91cd\:7b97\:56fe\:3002"];
