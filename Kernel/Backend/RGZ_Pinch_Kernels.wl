(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* One source for the four kernels. Internal convention: D=i[a g+b kk],
   a=-A and b=-B in the reviewed PDF. External propagators stay rational.
   FourPinch includes the AA GammaP contribution in the two NAV diagrams;
   GaugeDifference subtracts an arbitrary fixed reference gauge. *)
ClearAll[RGZBuildPinches];
Options[RGZBuildPinches]={"Mode"->"FourPinch","ReferenceGauge"->0,
  "Representation"->"FAD"};
RGZBuildPinches[OptionsPattern[]] := Module[
  {mode=OptionValue["Mode"],ref=OptionValue["ReferenceGauge"],
   rep=OptionValue["Representation"],a,bt,bl,b,dk,ck,ap,pr,rr,
   gap,delta,r3=rgzLoopM3^2,r4=rgzLoopM4^2,g2=SMP["g_s"]^2,
   s0=SPD[p,p],box,nav,av,qse,gp,total,parts},
  If[!MemberQ[{"FourPinch","GaugeDifference"},mode] ||
      !MemberQ[{"FAD","Rational"},rep],
    Return[Failure["PinchOptions",<|"Mode"->mode,"Representation"->rep|>]]];
  If[mode==="GaugeDifference" && (!FreeQ[ref,\[Alpha]] || !FreeQ[ref,_FeynAmpDenominator|_LorentzIndex]),
    Return[Failure["FixedReferenceGaugeRequired",<||>]]];
  If[rep==="Rational",
    a[k_] := -(SPD[k,k]-\[Mu]^2)/RGZLoopDelta[SPD[k,k]];
    bt[k_] := -a[k]/SPD[k,k];
    bl[k_,g_] := If[TrueQ[g===0],0,-g/(SPD[k,k]^2+g M^4)],
    gap=Sqrt[mG^4-(m^2-\[Mu]^2)^2/4];delta=(m^2-\[Mu]^2)/2;
    a[k_] := -((1+delta/(I gap))FAD[{k,rgzLoopM3}]+
      (1-delta/(I gap))FAD[{k,rgzLoopM4}])/2;
    bt[k_] := -\[Mu]^2/(r3 r4)FAD[k]+
      (r3-\[Mu]^2)/(r3(r3-r4))FAD[{k,rgzLoopM3}]+
      (r4-\[Mu]^2)/(r4(r4-r3))FAD[{k,rgzLoopM4}];
    bl[k_,g_] := Which[TrueQ[g===0],0,
      TrueQ[g===\[Alpha]],\[Alpha]/(2 I Sqrt[\[Alpha] M^4])
        (FAD[{k,rgzLoopM1}]-FAD[{k,rgzLoopM2}]),
      True,g/(2 I Sqrt[g M^4])
        (FAD[{k,Sqrt[-I Sqrt[g M^4]]}]-FAD[{k,Sqrt[I Sqrt[g M^4]]}])]
  ];
  b[k_,g_] := bt[k]+bl[k,g];
  dk[k_] := If[mode==="FourPinch",b[k,\[Alpha]],
    bl[k,\[Alpha]]-bl[k,ref]];
  ck[k_,l_] := If[mode==="FourPinch",b[k,\[Alpha]]b[l,\[Alpha]],
    (* Keep the reference cross terms; B(reference) generally is nonzero. *)
    b[k,ref]dk[l]+dk[k]b[l,ref]+dk[k]dk[l]];
  ap=-(s0-\[Mu]^2)/RGZLoopDelta[s0];
  pr=RGZLoopPT[p,\[Mu],\[Nu]];
  rr=RGZLoopPT[p,\[Mu],\[Rho]]RGZLoopPT[p,\[Nu],\[Sigma]]
    FVD[q,\[Rho]]FVD[q,\[Sigma]];
  box=g2 CA(pr a[p-q]dk[q]+rr ck[q,p-q]/2);
  nav=g2 CA ap(s0 rr ck[q,p-q]/2+
    (s0-SPD[p-q,p-q])pr a[p-q]dk[q]+rr a[p-q]dk[q]);
  av=-g2(CA/2-CF)ap pr dk[q];
  qse=-g2 CF ap pr dk[q]/2;
  gp=If[mode==="FourPinch",-2g2 CA ap pr a[q]a[p-q],0];
  parts=Map[ChangeDimension[FCI[#],D]&,
    <|"box"->box,"nav全部"->2nav+gp,"av全部"->2av,"qse全部"->4qse|>];
  total=Total[Values[parts]];
  Join[parts,<|"NAVMomentum"->ChangeDimension[FCI[2nav],D],
    "GammaP"->ChangeDimension[FCI[gp],D],"Total"->total,
    "候选总和"->total,"A外部标量"->ap,"Mode"->mode,
    "ReferenceGauge"->If[mode==="GaugeDifference",ref,None],
    "KernelVersion"->"RGZFourPinch/2",
    "归一化说明"->"守恒外电流之间的四图传播子替换核；共同约定为 -(D0.Pi.D0)AA。FourPinch 包含两侧 AA GammaP；GaugeDifference 为固定参数的参考规范差。"|>]
];
