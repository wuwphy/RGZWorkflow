# 独立费米子 horizon 扩展

文件组：`RGZ_FermionicHorizon.fr`、`RGZ_FermionicHorizon.mod`、`RGZ_FermionicHorizon.gen`。
基于 arXiv:2107.08569v2 的物理作用量（28）、（55），在现有 RGZ 基础模型上为两味夸克各加入一组辅助场。
这是独立模型，不修改或替换 `RGZ_W2.6.*`，也没有接入现有 `RGZMixedComputeAll`、QQ、缓存与重整化工作流。
请在新内核加载；不要与原模型同时加载。

## 场和参数

| 场 | FR / FA 类别 | 统计性质及指标 |
|---|---|---|
| `Q1,Q2` | `F[1],F[2]` | 原有 Dirac 夸克，基本表示 |
| `LamF1,LamF2` | `F[3],F[4]` | 反对易辅助自旋场，伴随 × 基本表示；工程维数为 1，二阶动力学 |
| `BosF1,BosF2` | `S[4],S[5]` | 对易辅助自旋场的分量表示，`FHSpin × Gluon × Colour` |

共轭场使用相应的 `...bar` 名字。`BosF` 对应论文新增的对易 ζ，不能与基础模型已有的 ζ 鬼场混用。
它的自旋指标是旁观指标，动力学和相互作用都对该指标对角；闭合分量求和为 4。
不得把它改成 F 类或人为加入费米圈负号。F 类辅助场的 Grassmann 负号由 FeynArts 产生。
`LamF` 不是通常的一阶 Dirac 场，不能套用标准 Dirac 动能或质量谱自动检查。

- `mQ1,mQ2`：普通夸克质量，默认数值 0。
- `hF1,hF2`：实的 horizon 混合系数，质量维数 3/2，默认数值 0；计算中保留符号。
- `wF1,wF2`：辅助部门 refinement 质量，默认数值 1；独立于原模型的 `M` 红外调节参数和 `mu`。

### 原文归一化需要显式区分

PDF 第 6 页式（28）及式（45）给出的混合系数为 `sigma^(3/2)`，但第 29 页式（174）的质量函数写成
`m + gs^2 CF sigma^3/(pE^2+w^2)`。按式（28）直接做 Gaussian 消元只得到 `CF sigma^3`。
本实现不把这一差异隐藏在代码中，统一使用实际混合系数 `hF`：

- 按式（28）的局域混合归一化：`hF = sigma^(3/2)`。
- 按式（174）的质量函数归一化：`hF = gs sigma^(3/2)`。

两者是不同的 sigma 参数定义。所有传播子和混合顶点都使用同一 `hF`，不可只修改传播子分子。

## 作用量与展开范围

FR 中的新部门命名为 `LFermionicHorizon`，总作用量 `LTotal` 已包含它。
夸克 dressing 采用 `psiH = exp[-I gs xi.T] Q`、`barPsiH = Qbar exp[I gs xi.T]` 的 ξ² 展开；
`Ah` 沿用基础模型的有限展开。FA 文件一致保留所有三、四粒子相互作用：基础 48 项加新增 24 项。
高粒子数项和论文用于全阶重整化的外源作用量 Σ 没有在 FA 文件中实现。
因此这组文件不是任意阶数 BRST/ST 证明，也不宣称完成该扩展的一圈重整化。

Minkowski 新二次核（省略颜色和自旋单位阵）为

```text
K = [ slash(k)-mq,  hF T^a       ]
    [ hF T^b,      -(k^2-wF^2) ]
```

对易辅助场使用相同的二阶核 `-(k^2-wF^2)`。
积分掉反对易辅助场给出的 Euclidean 质量函数为

```text
A_E(pE^2) = mq + CF hF^2/(pE^2+wF^2),  CF=(Nc^2-1)/(2 Nc).
```

其 Minkowski 表示是 `A_M(s)=mq-CF hF^2/(s-wF^2)`。
完整 λ–λ 协方差保留 `delta_ab delta_ij` 自由项和 `T^a S_Q T^b` 混合修正，
没有只保留与夸克耦合的颜色投影。
在 `hF=0` 时，λ 与 Bos 的相同算符行列式因相反统计相消。

## FA 内部线约定：不可重复乘传播子

沿用基础模型的玻色传播子插入方式，并扩展到整个 Q–λ 矩阵：

- 内部 V、S、F 线是 carrier；每条物理内部线必须恰好有一个 `Vertex[2,1]`，提供完整协方差。
- U 鬼线本身已包含完整传播子，不再插入一个协方差。
- 新 FF 两点项已重求和全部 Q–λ 二次混合；不可再插入原始 `hF barLambda T Q` 两点顶点。
- 插入节点使用第二个系数槽（CTOrder 1），仅是传播子编码，不是物理重整化反项。
- F 的外部波函数设置为 1，只生成截肢 Green 函数。非零 horizon 时不能直接使用质量为 mq 的自由外线旋量或原 QQ 的 LSZ 留数。

例如纯 λ 或 Bos 泡图也需要每条环线各一个完整两点节点；不能直接沿用基础模型没有节点的纯 U 鬼泡拓扑。
`CreateTopologies` 默认拓扑不会自动添加这些节点。

## 加载

FeynRules 新内核，先按本机安装方式加载 FeynRules，再执行：

```wl
LoadModel[FileNameJoin[{project,"Models","Experimental","FermionicHorizon","RGZ_FermionicHorizon.fr"}]];
FeynmanRules[RGZWorkflow`Engine`LFermionicHorizon,
  MinParticles->3,MaxParticles->4,FlavorExpand->True,
  ConservedQuantumNumbers->False];
```

FeynArts 新内核，加载 FeynArts 后执行：

```wl
fhModel = FileNameJoin[{project,"Models","Experimental","FermionicHorizon","RGZ_FermionicHorizon"}];
InitializeModel[fhModel,GenericModel->fhModel];
```

这一步只是加载模型；计算时还需使用上述专用拓扑。
现有 `RGZInitialize[... "ModelBase"->...]` 工作流的元数据、场枚举、张量约化和 LSZ 仍针对原模型，不能用于此扩展。

## 已执行检查

独立检查脚本在 `Tests/Experimental/FermionicHorizon/`：

- FeynRules 生成新增 24 个三、四粒子顶点，覆盖两味。
- FeynArts 实际生成全部 24 项顶点及 10 项新协方差块，无缺失耦合/传播子消息。
- 基础与新增张量系数在合并 generic basis 后的精确重构。
- 八项独立二次核检查，包括完整 27×27 SU(3) 颜色块的符号反演、Euclidean 质量函数和零 horizon 极限。

这些检查不等于完整圈图计算或规范无关性证明。
