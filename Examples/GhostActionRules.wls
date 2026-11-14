(* SPDX-License-Identifier: GPL-3.0-or-later *)
(* Fresh FeynRules kernel. Follow with GhostModelRules.wls in another kernel.
   Set RGZ_GHOST_RULE_WORK to a new temporary directory shared by both runs. *)
If[$Context=!="RGZWorkflow`Engine`",With[{script=$InputFileName,project=DirectoryName[DirectoryName[$InputFileName]]},
 Get[FileNameJoin[{project,"RGZWorkflow.wl"}]];
 If[!StringQ[Environment["RGZ_GHOST_RULE_WORK"]],Print["Set RGZ_GHOST_RULE_WORK to an isolated temporary directory."];Exit[1]];
 RGZWorkflow`RGZInitialize[Environment["RGZ_GHOST_RULE_WORK"],"Display"->"Quiet"];
 Global`FR$Parallel=False;Global`FR$Parallelize=False;
 If[!AssociationQ[RGZWorkflow`RGZLoadFeynRules[]],Exit[1]];
 If[!AssociationQ[RGZWorkflow`RGZExtraTreeAudit[120]],Exit[1]];
 RGZWorkflow`Private`engineEvaluation[Get[script]]];Exit[1]];
 rgzGhostQuadratic=Append[Select[rgzExtraRows,MemberQ[{"eta","omega","zeta"},#["Field"]]&],
  Block[{rho=0},rgzExtraGhostRow[c,cbar,"c",1,0,"delta_adjoint"]]];
 rgzGhostCubic=Flatten[Table[FeynmanRules[Evaluate[Symbol["RGZWorkflow`Engine`"<>rgzSector]],
  FlavorExpand->False,MinParticles->3,MaxParticles->3,ScreenOutput->False],
  {rgzSector,{"Lghost","Lomega","Lzeta","Leta"}}],1];
 rgzGhostCubic=Select[rgzGhostCubic,MemberQ[#[[1,All,1]],A]&];
 Export[FileNameJoin[{RGZWorkflow`Private`dataDirectory[],"RGZ_all_raw.wxf"}],rgzGhostCubic,"WXF"];
 rgzGhostPassed=Length[rgzGhostCubic]===4&&And@@Flatten[Values[#["Checks"]]&/@rgzGhostQuadratic];
 Export[FileNameJoin[{Environment["RGZ_GHOST_RULE_WORK"],"ghost-action.wxf"}],<|"QuadraticRows"->rgzGhostQuadratic,
 "CubicRules"->rgzGhostCubic,"FeynRulesVersion"->FR$VersionNumber,"Passed"->rgzGhostPassed|>,"WXF"];
 Print["Ghost action kernels and four A-ghost vertices: ",rgzGhostPassed];
 Exit[If[TrueQ[rgzGhostPassed],0,1]];
