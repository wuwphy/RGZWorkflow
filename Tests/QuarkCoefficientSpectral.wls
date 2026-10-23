(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
project=DirectoryName[DirectoryName[$InputFileName]];Get[FileNameJoin[{project,"RGZWorkflow.wl"}]];
RGZWorkflow`Private`qqProgressUpdate[<||>];
$MaxExtraPrecision=2000;
checks=<||>;record[n_,v_]:=(AssociateTo[checks,n->TrueQ[v]];Print[n," : ",TrueQ[v]]);
m=RGZWorkflow`Engine`m;mu=RGZWorkflow`Engine`\[Mu];mg=RGZWorkflow`Engine`mG;
r=Sqrt[mg^4-(m^2-mu^2)^2/4];lo=Sqrt[(m^2+mu^2)/2-I r];hi=Sqrt[(m^2+mu^2)/2+I r];
e=(m^2+mu^2+2 lo hi)/(lo+hi)^3;
out=RGZWorkflow`Private`qqCoefficientSpectral[e];
record["PoleSumIdentity",Cancel[Together[out-1/(lo+hi)]]===0];
e2=(lo+hi)/(m^2+mu^2)+Sqrt[4 mg^4-(m^2-mu^2)^2]/((lo-hi) (m^2+mu^2));
out2=RGZWorkflow`Private`qqCoefficientSpectral[e2];
record["NoSpectralVariableLeak",FreeQ[out2,s_Symbol/;StringMatchQ[SymbolName[Unevaluated[s]],"spectralMinus*"|"spectralPlus*"]]];
record["RealDiscriminantBranch",Abs[N[(out2-e2)/.{m->1,mu->2,mg->2},40]]<10^-30];
record["NegativeDiscriminantBranch",Abs[N[(out2-e2)/.{m->3,mu->1,mg->1},40]]<10^-30];
record["UnrelatedRadicalUsesFallback",RGZWorkflow`Private`qqCoefficientSpectral[1/(x+Sqrt[y])] ===$Failed];
record["DoesNotForceNonzeroToZero",out2=!=0];
rad4=Sqrt[Expand[4 (mg^4-(m^2-mu^2)^2/4)]];
e3=1/(m^2+mu^2+I r)+1/(m^2+mu^2+I rad4/2);
out3=RGZWorkflow`Private`qqCoefficientSpectral[e3];
record["DiscriminantOnlySectorDoesNotFallback",out3=!=$Failed];
record["ProportionalDiscriminantExactIdentity",Cancel[Together[out3-(e3/.rad4->2 r)]]===0];
record["DiscriminantOnlyComplexBranch",Abs[N[(out3-e3)/.{m->3,mu->1,mg->1},40]]<10^-30];
grouped3=RGZWorkflow`Private`qqCoefficientSpectral[e3,"Grouped"];
record["ExplicitGroupedModeIsExact",Cancel[Together[grouped3-(e3/.rad4->2 r)]]===0];
record["ActualAlgorithmRecorded",Lookup[RGZResult["QQProgress"],"SpectralMethod",None]==="Grouped"];
record["ImplementationVersionRecorded",Lookup[RGZResult["QQProgress"],"SpectralImplementation",None]===RGZWorkflow`Private`$qqCoefficientSpectralVersion];
Block[{RGZWorkflow`Private`qqCoefficientSpectralNativeFactor},
 Clear[RGZWorkflow`Private`qqCoefficientSpectralNativeFactor];
 RGZWorkflow`Private`qqCoefficientSpectralNativeFactor[_]:=$TimedOut;
 probeFallback=RGZWorkflow`Private`qqCoefficientSpectral[e];
 record["AutomaticFactorTimeoutUsesExactGroupedPath",Cancel[Together[probeFallback-1/(lo+hi)]]===0];
 record["FactorFallbackRecorded",Lookup[RGZResult["QQProgress"],"SpectralMethod",None]==="GroupedAfterFactorProbe"]];
Print[<|"Passed"->Count[Values[checks],True],"Failed"->Count[Values[checks],False]|>];Exit[If[And@@Values[checks],0,1]];
