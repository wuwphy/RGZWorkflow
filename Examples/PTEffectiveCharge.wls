(* SPDX-License-Identifier: GPL-3.0-or-later *)
(* A real, pinch-free sector of the PT two-point function, followed by a
   finite MOM shape and the independent MSbar RG solution. No full-PT claim. *)
If[$Context =!= "RGZWorkflow`Engine`",With[{script=$InputFileName,root=DirectoryName[DirectoryName[$InputFileName]]},
 Get[FileNameJoin[{root,"RGZWorkflow.wl"}]];
 RGZWorkflow`RGZInitialize[CreateDirectory[],"Display"->"Quiet"];
 RGZWorkflow`RGZLoadFeynCalc[];RGZWorkflow`Private`loadQuarkEngine[];
 Get[FileNameJoin[{root,"Tools","PTEffectiveCharge.wl"}]];
 RGZWorkflow`Private`engineEvaluation[Get[script]]];Exit[1]];
requireReview[x_]:=If[FailureQ[x],Print[x];Exit[1],x];
checksReview=<||>;recReview[n_,v_]:=(AssociateTo[checksReview,n->TrueQ[v]];Print[n," ",TrueQ[v]]);
requireReview[RGZMXStart["FieldPair"->{1,1},"Category"->{"QuarkBubble"},"reuseAACache"->False]];
recReview["TwoDynamicalQuarkGraphs",Length[rgzMXJob["PerDiagram"]]===2];
Do[Do[requireReview[RGZMXRun[i,stage,60]],{stage,{"Convert","Project","DecomposeT","IntegrateT","ScalarT","DecomposeL","IntegrateL","ScalarL"}}],{i,2}];
sectorReview=requireReview[RGZMXTotal[]];matrixReview=requireReview[RGZMixProjectPair[sectorReview,{1,1},60]];
sReview=FCI[SPD[p,p]];aReview=(sReview-\[Mu]^2)/((sReview-m^2)(sReview-\[Mu]^2)+mG^4);
packetReview=<|"Mode"->"FourPinch","CommonConventionSelfEnergyT"->aReview^2 matrixReview["T"],
 "pinchParts"-><|"box"->0,"navAll"->0,"avAll"->0,"qseAll"->0|>,"NAVMode"->"QuarkSectorNoPinchRequired"|>;
twoPointReview=requireReview[RGZPTResearch`RGZPTSelfEnergy[packetReview,sReview]];
expectedSigmaReview=-SMP["g_s"]^2/(8Pi^2) (D-2)/(D-1) sReview B0[sReview,0,0];
recReview["ActualQuarkSelfEnergyAmputation",Cancel[Together[FCI[twoPointReview["SelfEnergyM"]-expectedSigmaReview]]]===0];
recReview["TransverseQuarkLoop",matrixReview["L"]===0];
recReview["AmputationReconstruction",Cancel[Together[twoPointReview["AmputationReconstruction"]]]===0];
euclideanReview=RGZPTResearch`RGZPTEuclidean[twoPointReview,reviewQ2];
pairsReview=RGZGaugePVPairs[euclideanReview["SelfEnergyE"]];
mastersReview=First/@pairsReview;
polesReview=Association[Table[master->RGZWorkflow`Private`qqUVIRMaster[master,False],{master,mastersReview}]];
finiteReview=requireReview[RGZWorkflow`Private`qqMSbarFiniteRow[pairsReview,polesReview,reviewMuR2,60]];
expectedFiniteReview=SMP["g_s"]^2/(8Pi^2) reviewQ2 (10/9-2Log[reviewQ2/reviewMuR2]/3);
recReview["FiniteQuarkSector",FullSimplify[finiteReview["FiniteFormFactor"]-expectedFiniteReview]===0];
shapeReview=RGZPTResearch`RGZPTCharge[finiteReview["FiniteFormFactor"]/.SMP["g_s"]^2->4Pi reviewAlpha,reviewQ2,euclideanReview["TreeInverseE"],reviewMuR2,reviewAlpha];
recReview["MOMReferenceNormalization",FullSimplify[(shapeReview["OneLoopEffectiveCoupling"]/.reviewQ2->reviewMuR2)-reviewAlpha]===0];
qcdShapeReview=shapeReview["OneLoopEffectiveCoupling"]/.{m->0,\[Mu]->0,mG->0};
recReview["FermionScreeningDirection",FullSimplify[reviewQ2 D[qcdShapeReview,reviewQ2]-reviewAlpha^2/(3Pi)]===0];
(* Check local quark-sector UV closure in the same external ordering.
   C=-D0 Pi D0, and the QQ code connects it as -gs^2 C. *)
ct2Review=RGZWorkflow`RGZQQMSbarCounterterms["ClosedQuarkFlavors"->2]["ParameterCounterterms"];
ct0Review=RGZWorkflow`RGZQQMSbarCounterterms["ClosedQuarkFlavors"->0]["ParameterCounterterms"];
nfCTReview=AssociationThread[Keys[ct2Review],MapThread[Subtract,{Values[ct2Review],Values[ct0Review]}]];
bornReview=-SMP["g_s"]^2 aReview;
localCTReview=RGZWorkflow`Private`qqLocalParameterCT[bornReview,nfCTReview]/.SMP["Delta_UV"]->1/EpsilonUV;
rawSelfReview=-SMP["g_s"]^2 twoPointReview["OrdinaryCommonKernel"]/I;
uvSelfReview=RGZWorkflow`Private`qqUVIRPole[PaVeUVPart[rawSelfReview]/.D->4-2EpsilonUV,EpsilonUV];
recReview["QuarkSectorLocalUVCancellation",FullSimplify[ChangeDimension[uvSelfReview,D]+localCTReview]===0];
runningReview=RGZPTResearch`RGZMSbarRunning[reviewQ2,reviewMuR2,reviewAlpha];
recReview["TwoFlavourBeta0",runningReview["Beta0"]===29/3];
recReview["MSbarBetaEquation",FullSimplify[reviewQ2 D[runningReview["Alpha"],reviewQ2]+(29/3)runningReview["Alpha"]^2/(4Pi)]===0];
recReview["MinkowskiEuclideanDysonSigns",FullSimplify[euclideanReview["OneLoopScalarE"]+(twoPointReview["OneLoopScalarM"]/.sReview->-reviewQ2)]===0];
(* A negative test: raw masters are not accepted as a finite running charge. *)
recReview["UnsubtractedBubbleNotUsedAsCoupling",FailureQ[RGZPTResearch`RGZPTCharge[B0[-reviewQ2,0,0],reviewQ2,reviewQ2,reviewMuR2,reviewAlpha]]];
opaqueReview=RGZPTResearch`RGZPTCharge[RGZWorkflow`RGZPVFinite[B0[-reviewQ2,reviewMass1,reviewMass2],reviewMuR2],reviewQ2,reviewQ2,reviewMuR2,reviewAlpha];
recReview["FiniteMasterRepresentationPreserved",AssociationQ[opaqueReview]&&!FreeQ[opaqueReview,RGZWorkflow`RGZPVFinite]];
externalFormReview=<|"SquaredMomentum"->sReview,"TreeScalarM"->aReview,"SelfEnergyM"->B0[SPD[p,p],reviewMass1,reviewMass2]|>;
normalizedReview=RGZPTResearch`RGZPTEuclidean[externalFormReview,reviewQ2];
recReview["ExternalAndInternalInvariantFormsAgree",normalizedReview["SelfEnergyE"]===B0[-reviewQ2,reviewMass1,reviewMass2]];
resultReview=<|"Scope"->"Actual two-flavour quark sector only; full gluonic/auxiliary PT sum unproven","Checks"->checksReview,
 "TwoPoint"->twoPointReview,"Euclidean"->euclideanReview,"FiniteQuarkSelfEnergyE"->finiteReview,
 "FermionMOMShape"->shapeReview,"MSbarRGReference"->runningReview|>;
fileReview=FileNameJoin[{RGZWorkflow`Private`dataDirectory[],"pt-effective-charge-example.wxf"}];Export[fileReview,resultReview,"WXF"];
Print["Result: ",fileReview];Print["SUMMARY ",<|"Passed"->Count[Values[checksReview],True],"Failed"->Keys[Select[checksReview,Not]]|>];
Exit[If[And@@Values[checksReview],0,1]];
