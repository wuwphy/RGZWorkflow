(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
project=DirectoryName[DirectoryName[$InputFileName]];Get[FileNameJoin[{project,"RGZWorkflow.wl"}]];RGZWorkflow`Private`qqProgressUpdate[<||>];
checks=<||>;record[n_,v_]:=(AssociateTo[checks,n->TrueQ[v]];Print[n," : ",TrueQ[v]]);
z=(u^2-h^2)(v^2-h^2);group[e_]:=RGZWorkflow`Private`qqCoefficientSpectralGrouped[e,u,v,h,g];
r=group[1/((u^2+v^2-2 h^2-2 g^2)(u^2+v^2-2 h^2+2 g^2))];
record["PairedMassFactorsEliminateRedundantMass",Cancel[Together[r-1/(u^2-v^2)^2]]===0&&FreeQ[r,g]];
record["MassPolynomialRelationIsExactZero",group[(g^4-z)/x]===0];
record["OddMassSquareIsRetained",group[g^2]===g^2];
r=group[(1/x+1/y)/(g^4+t)];record["NestedRationalCoefficient",Cancel[Together[r-(x+y)/(x y (z+t))]]===0];
e=Sum[(u+v)^i/(x+i),{i,1,12}];r=group[e];record["SharedDenominatorCollectionIsExact",Cancel[Together[r-e]]===0];
record["DegenerateCoordinateDenominatorStops",group[1/(g^4-z)]===$Failed];
record["ZeroOverZeroRelationIsNotCertifiedAsZero",group[(g^4-z)/(g^8-z^2)]===$Failed];
record["NonPolynomialMassDependenceNotDiscarded",group[Sin[g]/(x+g)]===$Failed];
record["GenericDimensionKept",!FreeQ[group[(D-4)/(x+g^4)],D]];
record["NoForcedCancellation",group[1/(x+g^4)]=!=0];
record["RationalDenominatorUnit",Cancel[Together[group[1/(u^2+v^2+(v^2-u^2)/2)]-2/(u^2+3v^2)]]===0];
record["NegativeNumericFactorExponent",Cancel[Together[group[1/(u^2/4+3 v^2/4)]-4/(u^2+3 v^2)]]===0];
Print[<|"Passed"->Count[Values[checks],True],"Failed"->Count[Values[checks],False]|>];Exit[If[And@@Values[checks],0,1]];
