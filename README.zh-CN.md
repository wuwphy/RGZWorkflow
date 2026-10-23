# RGZWorkflow 独立开发版

版本 **0.3.0-dev.1**，许可证 **GPL-3.0-or-later**。本目录是独立副本，不修改原工程。

默认模型包含约束鬼场 eta 与两味动力学夸克。完整 QQ 外线流程仍限无质量情形；非零夸克质量仅支持基础模型和闭合夸克圈。费米子 horizon 模型放在 `Models/Experimental`，不参与默认计算。

打开 [Start Here](<Examples/RGZWorkflow — Start Here.nb>) 按节运行。依赖外部安装 Mathematica、FeynRules、FeynCalc/FeynArts；不捆绑第三方源码。FeynRules 与 FeynCalc 必须用不同的新内核。安装和许可证见 [安装说明](Documentation/Installation.md) 与 [第三方说明](THIRD_PARTY.md)。

当前发布验证只覆盖验证记录中明确列出的检查；没有把旧探针模型的规范抵消和有限振幅结果移植为新两味模型的结果。新模型完整混合矩阵、振幅、一般规范差以及 UV/IR 闭合仍需按冻结版本重新运行。

此包可以作为公开开发版本；随论文发布前还需补齐 [论文复现材料](Documentation/Reproducibility.md)。它不是已经完成全部物理验证的最终论文结果包。尚未上传或发布到远端。
