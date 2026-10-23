(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
project=DirectoryName[DirectoryName[$InputFileName]];Get[FileNameJoin[{project,"RGZWorkflow.wl"}]];
RGZInitialize[FileNameJoin[{$TemporaryDirectory,"qq-branch-comparison-test-"<>CreateUUID[]}],"Display"->"Quiet"];
Quiet[RGZLoadFeynCalc[],FrontEndObject::notavail];
delta=RGZWorkflow`Engine`mG^4-(RGZWorkflow`Engine`m^2-RGZWorkflow`Engine`\[Mu]^2)^2/4;
u=(-RGZWorkflow`Engine`m^2-RGZWorkflow`Engine`\[Mu]^2)/2+(RGZWorkflow`Engine`m^2+RGZWorkflow`Engine`\[Mu]^2)/2;
co=(RGZWorkflow`Private`qqGaugeAlpha-1)(Sqrt[-(u-2 I Sqrt[delta])^2]-2 Sqrt[delta]);
amp=First[RGZWorkflow`Private`qqFFColors[]] co;
Block[{RGZWorkflow`Private`qqBranchCanonicalCoefficient,RGZWorkflow`Private`qqCoefficientRational},
 Clear[RGZWorkflow`Private`qqBranchCanonicalCoefficient,RGZWorkflow`Private`qqCoefficientRational];
 RGZWorkflow`Private`qqBranchCanonicalCoefficient[z_]:=z;
 RGZWorkflow`Private`qqCoefficientRational[z_]:=z;
 legacy=RGZWorkflow`Private`qqGaugeCompare[amp,1,10,5,"BranchCacheTest"]];
current=RGZWorkflow`Private`qqGaugeCompare[amp,1,10,5,"BranchCacheTest"];
checks=<|"LegacyBranchExpressionNotClaimedZero"->(AssociationQ[legacy]&&legacy["AllCoefficientsZero"]===False),
 "CachedLegacyCoefficientNowExactlyZero"->(AssociationQ[current]&&current["AllCoefficientsZero"]===True),
 "NoPhysicalInputChange"->(legacy["FormFactors"]["InputSHA256"]===current["FormFactors"]["InputSHA256"]),
 "GeneralGaugeNotPointTest"->(current["PointTest"]===False)|>;
Print[checks];Print[<|"Passed"->Count[Values[checks],True],"Failed"->Count[Values[checks],False]|>];Exit[If[And@@Values[checks],0,1]];
