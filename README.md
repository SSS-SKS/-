# 动力电池回收研究

[在线打开研究资料站](https://sss-sks.github.io/-/) · [下载集成式HTML](https://github.com/SSS-SKS/-/releases/download/offline-html/battery-recycling-research.html)

这是独立研究网站，按研究总览、政策脉络、文献评述、责任与溯源、数理分析、标准与模型、专利、图表、资料检索及下载中心组织。包含18幅图表、83条来源、四类情景计算和匿名数据下载。

网页中的“下载HTML”按钮点击即可保存，下载后可用浏览器离线打开。图像、样式、脚本和资料均已内嵌，无需安装软件。网站不包含报告全文或Word下载入口，研究报告独立维护。

## 资料

- [Excel证据库](data/evidence-database.xlsx)：22个工作表，82条来源。
- [GS1补充记录](data/source-supplement.csv)；[整合83条来源](research/evidence.json)。
- [图表与图号](research/figure_catalog.json)；[模型数据](research/model/)。
- [计算与文件核验结果](validation-summary.json)。

## 核心结果复算

Python 3.10或更新版本，无额外依赖：

```sh
python check_core.py
```

复算待匹配窗口10.31日、超过15日概率31.67%、准备时间缩短后的转运上界4.5日、全链条锂回收64.60%和企业情景强度0.44吨CO₂/吨。脚本另以数值积分核对时滞解析式。

## 绘图

已有研究图使用 R 4.3+，所需包为 ggplot2、patchwork、jsonlite、ragg、svglite、scales、systemfonts。依次在 `research/` 目录运行 `plot_research.R`、`plot_standards.R`、`plot_revisions.R`。字体使用 Noto Sans CJK SC。几何核验脚本保存在 `research/qa_tools/`。

资料入口、政策表头及综合机制图使用 R 4.6.1 与 svglite 2.2.2；中文字体仿宋，英文 Times New Roman。将 `qr-matrix.csv`、`official-mark.csv` 与输出目录放在同一级目录后执行：

```sh
Rscript plot_integration.R new-figures
```

## 图像署名与许可

- IEA公开数据按 CC BY 4.0 署名，详见[IEA原始数据](https://www.iea.org/data-and-statistics/charts/electric-vehicle-battery-sales-share-by-chemistry-and-region-2023-2025)。
- 图5：Chacana-Olivares等（2025）Fig.1，完整原图，仅等比例显示，非商业学习用途；[原文](https://www.nature.com/articles/s44296-025-00083-7)，[CC BY-NC-ND 4.0](https://creativecommons.org/licenses/by-nc-nd/4.0/)。
- 图6：Tembo等（2024）Fig.7，完整原图；[原文](https://doi.org/10.1038/s41427-024-00562-8)，[CC BY 4.0](https://creativecommons.org/licenses/by/4.0/)。
- 图16：Anticoi等（2025）Fig.9，完整原图；[原文](https://doi.org/10.1007/s10163-025-02159-x)，CC BY 4.0。
- 政策表头中的标识采用工信部官方原图，文号和日期据原文重排，不是官网截图。

正式报告文档、学校及个人信息不在本仓库发布。第三方资料遵循各自署名和许可，不对其统一授予新许可。
