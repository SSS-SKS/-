# 动力电池回收匿名研究网页 V2

[打开匿名研究网页](https://sss-sks.github.io/-/) · [直接下载集成式HTML](https://github.com/SSS-SKS/-/releases/download/offline-html/battery-recycling-research.html)

网页按概览、正文、复算、证据四个入口组织。保留6章短读、4类可输入计算、18幅图表、84条来源及76条报告引用映射。详细推导与实施建议集中在可展开附录，来源每页20条，模型数据分6组并支持完整ZIP下载。

点击“保存HTML”可离线使用；“复制HTML代码”可在浏览器允许时复制完整源代码，受限时自动保存文件。正文和附录分别提供打印入口。网页与正式报告独立保存，不提供Word下载。

## 输入计算

- 信息窗口：运输间隔、提前报送系数和观察阈值；显示均值、中位数、分位、零值概率及尾部概率。
- 贮存周转：分类期限、容量、日入库、预留量、准备时间及实际转运周期；同时检验期限和容量。
- 材料回收：进入率、有效工序进入率和目标元素工序率。
- 企业碳账：单位用电、电力因子、其他净排放，或燃烧、过程、运输及扣减分项。

全部输入即时更新，支持参考情景、重置、下载参数与结果。点击“保存单文件网页”后可以离线使用，当前计算参数随文件保存；重置会恢复研究参考值。

## 图表与资料

正文图1—8、专题图A1—A10与V2研究图号对应。图表目录见 [figure_catalog.json](research/figure_catalog.json)，完整来源见 [evidence.json](research/evidence.json)，76条编号见 [citation_map.json](research/citation_map.json)。

[基础Excel](data/evidence-database.xlsx)包含82条来源和22个工作表；[补充来源](data/source-supplement.csv)包含GS1及绿色转型政策；[V2文献编号对照](data/reference-map-v2.csv)列出76条题录与网址。完整84条来源JSON与模型CSV可从网页下载或在仓库中查阅。

## 复算与验证

Python 3.10或更新版本，无额外依赖：

```sh
python check_public.py
```

核心参考值为待匹配窗口10.31日、超过15日概率31.67%、周转上限4日及缩短准备后的4.5日、全链条回收64.60%、企业排放强度0.44吨CO₂/吨。全部为给定假设下的条件结果，不表示现实违规率、全国回收率或生命周期净减排。

绘图脚本和模型数据保存在 `research/`；网页插图PNG按图表目录提供。计算、桌面/手机交互及离线验证摘要见 [validation-summary.json](validation-summary.json)。

## 署名和许可

- IEA数据按CC BY 4.0署名，参见 [IEA图表](https://www.iea.org/data-and-statistics/charts/electric-vehicle-battery-sales-share-by-chemistry-and-region-2023-2025)。
- 图A1：Chacana-Olivares等（2025）Fig.1，完整文献原图，非商业学习用途；[原文](https://www.nature.com/articles/s44296-025-00083-7)，CC BY-NC-ND 4.0。
- 图A2：Tembo等（2024）Fig.7；[原文](https://doi.org/10.1038/s41427-024-00562-8)，CC BY 4.0。
- 图A3：Anticoi等（2025）Fig.9；[原文](https://doi.org/10.1007/s10163-025-02159-x)，CC BY 4.0。
- 图1为工信部官网原页面的条款截图，来源与截取日期随图保留。

公开目录不包含正式报告文件、学校信息或个人附录。第三方资料遵循各自署名和许可。
