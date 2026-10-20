# Reproducible R figures. Rules are descriptive; waiting-time distributions are analytic scenarios.
suppressPackageStartupMessages({library(ggplot2);library(patchwork);library(jsonlite);library(grid)})
args<-commandArgs(trailingOnly=TRUE);root<-if(length(args))args[1] else '.'
out<-file.path(root,'figures');dir.create(out,showWarnings=FALSE,recursive=TRUE)
m<-fromJSON(file.path(root,'model/analysis.json'));ev<-fromJSON(file.path(root,'evidence.json'))
font<-'Noto Sans CJK SC'
ink<-'#243D50';teal<-'#278A83';blue<-'#6088AF';gold<-'#C39D5D';gray<-'#84949D';light<-'#EFF3F4'
cols<-c('0'=gray,'2'=teal,'5'=blue,'10'=gold)
theme_set(theme_classic(base_size=8,base_family='Noto Sans CJK SC')+theme(
 axis.line=element_line(linewidth=.3,colour='#8A969D'),axis.ticks=element_line(linewidth=.25,colour='#8A969D'),
 axis.text=element_text(size=7,colour=ink),axis.title=element_text(size=8,colour=ink),
 axis.title.x=element_text(margin=margin(t=7)),axis.title.y=element_text(margin=margin(r=6)),
 plot.title=element_text(size=8.5,face='bold',colour=ink,margin=margin(b=10)),
 plot.subtitle=element_text(size=7,colour=gray,margin=margin(b=8)),
 plot.tag=element_text(size=10,face='bold',colour=ink),plot.tag.position=c(0,1),
 plot.margin=margin(9,10,9,9),legend.text=element_text(size=7),legend.title=element_blank(),
 panel.grid.major.y=element_line(linewidth=.2,colour='#E6ECEF')))
sk<-Sys.getenv('NATURE_FIGURE_SKILL_ROOT')
if(!nzchar(sk))stop('Set NATURE_FIGURE_SKILL_ROOT for mandatory rendered geometry QA.')
source(file.path(sk,'scripts/panel_alignment.R'))
# Measure final panel viewports on the Cairo device. This resolves gtable null units
# in the actual drawn layout and retains the standard backend-neutral auditor.
write_patchwork_panel_layout<-function(plot,manifest_path,width_in,height_in,panel_ids=NULL,row_groups=NULL,column_groups=NULL,exemptions=list()){
 probe<-tempfile(fileext='.pdf');cairo_pdf(probe,width=width_in,height=height_in,family=font)
 gr<-patchwork::patchworkGrob(plot);rows<-.nature_alignment_panel_rows(gr)
 if(is.null(panel_ids))panel_ids<-letters[seq_len(nrow(rows))]
 on.exit({dev.off();unlink(probe)},add=TRUE);grid.newpage();grid.draw(gr);grid.force()
 panels<-lapply(seq_len(nrow(rows)),function(i){
  z<-rows[i,];vp<-paste0(z$name,'.',z$t,'-',z$l,'-',z$b,'-',z$r)
  seekViewport(vp);lo<-deviceLoc(unit(0,'npc'),unit(0,'npc'),valueOnly=TRUE);hi<-deviceLoc(unit(1,'npc'),unit(1,'npc'),valueOnly=TRUE);upViewport(0)
  list(id=panel_ids[i],bbox_pt=c(lo$x,lo$y,hi$x,hi$y)*72,grid_id='patchwork-grid-1',row_start=as.integer(z$t-1),row_stop=as.integer(z$b),col_start=as.integer(z$l-1),col_stop=as.integer(z$r))
 })
 manifest<-list(schema_version=1L,backend='r-patchwork-cairo-viewports',figure=list(width_pt=width_in*72,height_pt=height_in*72),panels=panels,exemptions=exemptions)
 if(!is.null(row_groups))manifest$row_groups<-row_groups
 if(!is.null(column_groups))manifest$column_groups<-column_groups
 write_json(manifest,manifest_path,auto_unbox=TRUE,pretty=TRUE,digits=NA);invisible(manifest)
}
contracts<-list()
save_pub<-function(p,name,height_mm,claim,panels='single panel'){
 width_mm<-145;w<-width_mm/25.4;h<-height_mm/25.4;f<-file.path(out,name)
 if(inherits(p,'patchwork'))require_patchwork_panel_alignment(p,paste0(f,'.alignment-layout.json'),paste0(f,'.alignment.json'),width_in=w,height_in=h,
  audit_script=file.path(sk,'scripts/audit_panel_alignment.py'),python=Sys.getenv('CODEX_PRIMARY_RUNTIME_PYTHON'),overlay_svg=paste0(f,'.alignment.svg'),tolerance_pt=1.5,gutter_tolerance_pt=1.5,strict=TRUE)
 svglite::svglite(paste0(f,'.svg'),width=w,height=h);print(p);dev.off()
 cairo_pdf(paste0(f,'.pdf'),width=w,height=h,family=font);print(p);dev.off()
 ragg::agg_png(paste0(f,'.png'),width=w,height=h,units='in',res=600,background='white');print(p);dev.off()
 cat('FILE',name,file.info(paste0(f,'.png'))$size,'DEV',dev.list(),'\n')
 contracts[[name]]<<-list(claim=claim,panels=panels,backend='R ggplot2/patchwork',width_mm=145,height_mm=height_mm,dpi=600,
  uncertainty='法定范围、解析模型分位或技术披露类别；无企业样本、标准误或置信区间')
}
tailW<-function(T,tau=2,a=15,b=30){x<-T-tau;pmax(0,pmin(1,(pmax(b-x,0)^2-pmax(b-x-a,0)^2-pmax(-x,0)^2+pmax(-x-a,0)^2)/(2*a*b)))}
meanW<-function(tau=2,a=15,b=30)(pmax(tau+b,0)^3-pmax(tau+b-a,0)^3-pmax(tau,0)^3+pmax(tau-a,0)^3)/(6*a*b)
quantW<-function(p,tau=2,a=15,b=30){c<-min(a,b)/(2*max(a,b));z<-if(p<c)-a+sqrt(2*a*b*p) else if(p>1-c)b-sqrt(2*a*b*(1-p)) else (b-a)/2+max(a,b)*(p-.5);max(0,tau+z)}
for(s in seq_len(nrow(m$scenarios))){r<-m$scenarios[s,];stopifnot(abs(meanW(r$tau)-r$mean)<1e-9,abs(quantW(.95,r$tau)-r$q95)<1e-9,abs(tailW(15,r$tau)-r$p_over_T)<1e-9)}
write_json(list(status='PASS',baseline=list(mean=meanW(),median=quantW(.5),q95=quantW(.95),p_over_15=tailW(15)),comparison='R closed forms agree with independently integrated source model'),file.path(root,'qa/r_numeric_check.json'),pretty=TRUE,auto_unbox=TRUE)
# 1 Industry context. Direct labels retain the official units and all years.
sales<-ev$sales
p<-ggplot(sales,aes(year,sales))+geom_area(fill='#E4EEEF',alpha=.9)+geom_line(colour=teal,linewidth=.9)+geom_point(colour=teal,size=2.5)+
 geom_text(aes(label=format(sales,big.mark=',',trim=TRUE)),nudge_y=180,size=2.9,family=font,colour=ink)+
 scale_x_continuous(breaks=sales$year,expand=expansion(mult=c(.12,.12)))+scale_y_continuous(limits=c(0,1900),breaks=c(0,500,1000,1500),labels=scales::comma)+
 labs(x=NULL,y='新能源汽车销量（万辆）')+theme(panel.grid.major.y=element_blank())
save_pub(p,'fig1_sales',72,'2023—2025年新能源汽车行业销量上升；销量不直接换算退役量。')
# 3 Legal handover relationships. Boxes describe roles, arrows do not encode flows.
nodes<-data.frame(x=c(1.05,3.7,6.35),y=c(1,1,1),label=c('换电服务、维修、\n报废机动车回收拆解等','生产企业依法设立的\n回收服务网点','依法设立的\n综合利用企业'))
p<-ggplot()+geom_rect(data=nodes,aes(xmin=x-1,xmax=x+1,ymin=.66,ymax=1.34),fill=c('#EFF3F4','#E4F0EC','#E6EDF4'),colour='#B9C8CB',linewidth=.35)+
 geom_segment(aes(x=2.08,xend=2.65,y=1,yend=1),arrow=arrow(length=unit(1.8,'mm')),linewidth=.45,colour=teal)+
 geom_segment(aes(x=4.75,xend=5.3,y=1,yend=1),arrow=arrow(length=unit(1.8,'mm')),linewidth=.45,colour=teal)+
 geom_segment(aes(x=1.05,xend=1.05,y=.64,yend=.28),linewidth=.4,colour=blue)+geom_segment(aes(x=1.05,xend=6.35,y=.28,yend=.28),linewidth=.4,colour=blue)+
 geom_segment(aes(x=6.35,xend=6.35,y=.28,yend=.64),arrow=arrow(length=unit(1.8,'mm')),linewidth=.4,colour=blue)+
 geom_text(data=nodes,aes(x,y,label=label),size=2.9,lineheight=1.4,family=font,colour=ink)+
 annotate('text',x=3.7,y=.1,label='来源主体亦可依法直接交付综合利用企业',size=2.6,family=font,colour=blue)+
 coord_cartesian(xlim=c(-.05,7.45),ylim=c(-.03,1.55),clip='off')+theme_void(base_family=font)+theme(plot.margin=margin(7,7,7,7))
save_pub(p,'fig3_responsibility',63,'主要来源主体可按法定路径交付，网点与综合利用须衔接。')
# 4 Event-specific deadline contrast; zero is each event's own trigger.
dd<-data.frame(event=factor(c('生产者回收入库','生产者移交出库','综合利用企业接收入库','综合利用产品移交出库'),levels=rev(c('生产者回收入库','生产者移交出库','综合利用企业接收入库','综合利用产品移交出库'))),days=c(15,15,30,30),group=c('生产者','生产者','综合利用','综合利用'))
p<-ggplot(dd,aes(y=event,x=days,colour=group))+geom_segment(aes(x=0,xend=days,yend=event),linewidth=1.1)+geom_point(size=2.7)+geom_text(aes(label=paste0(days,'日')),nudge_x=2,size=2.9,family=font,hjust=0)+
 scale_colour_manual(values=c('生产者'=teal,'综合利用'=blue),guide='none')+scale_x_continuous(limits=c(0,38),breaks=c(0,15,30),expand=expansion(mult=0))+
 labs(x='自各自触发事件起的法定报送期限（自然日）',y=NULL)+theme(panel.grid.major.y=element_blank(),axis.line.y=element_blank(),axis.ticks.y=element_blank())
save_pub(p,'fig4_deadlines',67,'出入库期限分别起算，法定报送窗口不等同实物运输时间。')
# 5 Code-level evidence and dimension-level totals are complementary roles.
codes<-do.call(rbind,lapply(seq_len(nrow(m$coding)),function(i)data.frame(record=m$coding$id[i],dimension=seq_len(6),value=unlist(m$coding$codes[[i]]))))
codes$record<-factor(codes$record,levels=rev(m$coding$id));dlab<-c('对象\n识别','回收\n责任','交接\n流向','信息\n规则','工程\n安环','监督\n衔接')
a<-ggplot(codes,aes(factor(dimension),record,fill=factor(value)))+geom_tile(colour='white',linewidth=.5)+scale_fill_manual(values=c('0'='#F0F3F4','1'=teal),guide='none')+scale_x_discrete(labels=dlab,expand=c(0,0))+scale_y_discrete(expand=c(0,0))+
 labs(x=NULL,y=NULL,title='逐条编码',subtitle='20条新办法分析记录')+theme(axis.line=element_blank(),axis.ticks=element_blank(),panel.grid=element_blank(),axis.text.y=element_text(size=6.8),axis.text.x=element_text(size=6.8))
cnt<-data.frame(dimension=factor(m$dimensions,levels=rev(m$dimensions)),n=m$dimension_counts)
b<-ggplot(cnt,aes(n,dimension))+geom_col(width=.55,fill=teal,alpha=.85)+geom_text(aes(label=paste0(n,'/20  ',n*5,'%')),hjust=0,nudge_x=.25,size=2.6,family=font,colour=ink)+
 scale_x_continuous(limits=c(0,13),breaks=c(0,5,10),expand=expansion(mult=0))+labs(x='涉及该维度的记录数',y=NULL,title='维度覆盖',subtitle='非互斥编码，各条记录等权')+theme(panel.grid.major.y=element_blank(),axis.line.y=element_blank(),axis.ticks.y=element_blank(),axis.text.y=element_text(size=6.8))
save_pub(a+b+plot_layout(ncol=2)+plot_annotation(tag_levels='a'),'fig5_policy_coding',119,'信息规则在所选20条记录中涉及9条，应与实体义务共同阅读。',c('逐条判定','维度计数'))
# 6 Complete natural-day record set and its exact discrete ECDF.
e<-m$events[m$events$include,];short<-c('汽车编码','汽车销售','合作维修','车企更换','独立维修','网点变更','回收入库','回收出库','综合入库','产品出库','车企废料','电池废料','换电废料')
e$display<-factor(paste(e$id,short),levels=rev(paste(e$id,short)))
a<-ggplot(e,aes(value,display))+geom_segment(aes(x=0,xend=value,yend=display),colour='#DEE7EB',linewidth=.45)+geom_point(colour=teal,size=2)+
 scale_x_continuous(limits=c(0,44),breaks=c(0,15,30,40),expand=expansion(mult=0))+labs(x='法定期限（自然日）',y=NULL,title='同单位期限',subtitle='13项自然日报送事件')+theme(axis.text.y=element_text(size=6.5),panel.grid.major.y=element_blank(),axis.ticks.y=element_blank())
cd<-data.frame(x=c(0,15,20,30,40,45),y=c(0,6,7,10,13,13)/13)
ann<-data.frame(x=c(8,24,29,39),y=c(.405,.59,.83,1.055),lab=c('46.2%','53.8%','76.9%','100%'))
b<-ggplot(cd,aes(x,y))+geom_step(colour=teal,linewidth=.75,direction='hv')+geom_point(data=cd[2:5,],colour=teal,size=1.8)+geom_text(data=ann,aes(x,y,label=lab),size=2.5,family=font,colour=ink)+
 scale_x_continuous(limits=c(0,45),breaks=c(0,15,30,40),expand=expansion(mult=0))+scale_y_continuous(limits=c(0,1.1),breaks=seq(0,1,.25),labels=scales::percent_format(accuracy=1),expand=expansion(mult=0))+
 labs(x='法定期限（自然日）',y='规则记录的累计比例',title='离散经验分布',subtitle='15、20、30、40日四类')
save_pub(a+b+plot_layout(ncol=2)+plot_annotation(tag_levels='a'),'fig6_deadline_distribution',105,'13项自然日期限中位数20日，单位不同的4项保留并排除混算。',c('全部事件','经验分布'))
# 7 Distribution-free support and analytic mixed-distribution shape.
s<-m$scenarios;s$index<-seq_len(nrow(s));s$group<-factor(s$tau,levels=c(0,2,5,10))
a<-ggplot(s,aes(y=index,colour=group))+geom_segment(aes(x=lower,xend=upper,yend=index),linewidth=1)+geom_point(aes(x=lower),shape=124,size=4)+geom_point(aes(x=upper),shape=124,size=4)+geom_text(aes(x=upper+1.3,label=paste0('0–',upper)),size=2.6,hjust=0,family=font)+
 scale_colour_manual(values=cols,guide='none')+scale_y_reverse(breaks=1:4,labels=paste0('τ = ',s$tau,'日'),limits=c(4.5,.5))+scale_x_continuous(limits=c(-1,48),breaks=c(0,15,30,45),expand=expansion(mult=0))+
 labs(x='可行待匹配时长 W（日）',y=NULL,title='法定窗口约束',subtitle='可行范围，无报送分布假设')+theme(panel.grid.major.y=element_blank(),axis.line.y=element_blank(),axis.ticks.y=element_blank())
poly<-do.call(rbind,lapply(seq_len(nrow(s)),function(i){r<-s[i,];w<-seq(.000001,r$upper,length.out=601);z<-w-r$tau;density<-pmax(pmin(15,30-z)-pmax(0,-z),0)/(15*30);wd<-density*.34/(1/30);data.frame(x=c(i-wd,rev(i+wd)),w=c(w,rev(w)),group=factor(r$tau,levels=c(0,2,5,10)))}))
b<-ggplot()+geom_polygon(data=poly,aes(x,w,group=group,fill=group,colour=group),alpha=.35,linewidth=.35)+geom_linerange(data=s,aes(x=index,ymin=q05,ymax=q95,colour=group),linewidth=.5)+geom_point(data=s,aes(index,median,colour=group),fill='white',shape=21,size=2,stroke=.7)+
 geom_text(data=s,aes(index,44,label=paste0('零值\n',sprintf('%.1f%%',p_zero*100)),colour=group),size=2.35,lineheight=1.2,family=font,vjust=1)+scale_colour_manual(values=cols,guide='none')+scale_fill_manual(values=cols,guide='none')+
 scale_x_continuous(breaks=1:4,labels=s$tau,limits=c(.45,4.55),expand=expansion(mult=0))+scale_y_continuous(breaks=c(0,15,30,45),limits=c(-1,46),expand=expansion(mult=0))+
 labs(x='假设运输/交接间隔 τ（日）',y='模型待匹配时长 W（日）',title='解析琴形分布',subtitle='点：中位数；线：5%—95%分位')
save_pub(a+b+plot_layout(ncol=2)+plot_annotation(tag_levels='a'),'fig7_lag_intervals_violin',106,'守时仍可出现单向待匹配；可行范围与假设分布下的分位有不同性质。',c('确定性范围','含零值概率的解析分布'))
# 8 Four stress tests of the same mechanism.
curve<-do.call(rbind,lapply(c(0,2,5,10),function(t)data.frame(T=seq(0,45,.25),p=tailW(seq(0,45,.25),tau=t),group=factor(t,levels=c(0,2,5,10)))))
leg<-data.frame(T=32,p=c(.96,.86,.76,.66),label=paste0('τ = ',c(0,2,5,10),'日'),group=factor(c(0,2,5,10),levels=c(0,2,5,10)))
a<-ggplot(curve,aes(T,p,colour=group))+geom_line(linewidth=.65)+geom_text(data=leg,aes(label=label),size=2.4,hjust=0,family=font)+scale_colour_manual(values=cols,guide='none')+
 scale_x_continuous(limits=c(0,45),breaks=c(0,15,30,45),expand=expansion(mult=0))+scale_y_continuous(limits=c(0,1.02),breaks=c(0,.5,1),labels=scales::percent_format(accuracy=1),expand=expansion(mult=0))+labs(x='示例观察阈值 T（日）',y='P(W > T)',title='交接间隔变化')
er<-m$early_reporting
b<-ggplot(er,aes(eta,median))+geom_linerange(aes(ymin=q05,ymax=q95),colour=teal,linewidth=.65)+geom_errorbar(aes(ymin=q05,ymax=q95),width=.035,colour=teal,linewidth=.45)+geom_point(shape=21,fill='white',colour=teal,size=2,stroke=.6)+
 scale_x_continuous(breaks=er$eta,labels=sprintf('%.2f',er$eta))+scale_y_continuous(limits=c(-1,29),breaks=c(0,10,20),expand=expansion(mult=0))+labs(x='下游自愿报送系数 η',y='窗口 W（日）',title='提前报送：τ = 2日')
ms<-m$marginal_sensitivity;ms$up<-factor(ms$up,levels=rev(c('前置三角','均匀','后置三角')));ms$down<-factor(ms$down,levels=c('前置三角','均匀','后置三角'))
c<-ggplot(ms,aes(down,up,fill=p_over_15))+geom_tile(colour='white',linewidth=.6)+geom_text(aes(label=sprintf('%.1f%%',p_over_15*100),colour=p_over_15>.4),size=2.7,family=font)+scale_colour_manual(values=c('FALSE'=ink,'TRUE'='white'),guide='none')+scale_fill_gradient(low='#EDF4F0',high='#2C747C',limits=c(0,.7),guide='none')+
 scale_x_discrete(labels=c('前置','均匀','后置'),expand=c(0,0))+scale_y_discrete(labels=c('后置','均匀','前置'),expand=c(0,0))+labs(x='下游V边际分布',y='上游U边际分布',title='边际假设：τ=2，T=15日')+theme(axis.line=element_blank(),axis.ticks=element_blank(),panel.grid=element_blank())
dp<-m$dependence;dp$case<-factor(dp$case,levels=rev(dp$case))
dd<-ggplot(dp,aes(p_over_15,case))+geom_segment(aes(x=0,xend=p_over_15,yend=case),colour='#DCE5E8',linewidth=.5)+geom_point(colour=teal,size=2.1)+geom_text(aes(label=sprintf('%.1f%%',p_over_15*100)),nudge_x=.025,hjust=0,size=2.6,family=font,colour=ink)+
 scale_x_continuous(limits=c(0,.55),breaks=c(0,.2,.4),labels=scales::percent_format(accuracy=1),expand=expansion(mult=0))+labs(x='P(W > 15日)',y=NULL,title='依赖结构：相同均匀边际')+theme(panel.grid.major.y=element_blank(),axis.line.y=element_blank(),axis.ticks.y=element_blank())
save_pub(a+b+c+dd+plot_layout(ncol=2)+plot_annotation(tag_levels='a'),'fig8_sensitivity',153,'提前报送的情景收益与概率大小受参数、边际分布及依赖结构影响。',c('参数','自愿提前报送','边际假设','依赖结构'))
# 9 A qualitative patent-to-task map, not a score, ranking or patent landscape.
pc<-read.csv(file.path(root,'model/patent_capabilities.csv'),check.names=FALSE)
names(pc)<-c('id','publication','function_name','disclosure','location');pc$row<-factor(paste(pc$id,pc$publication),levels=rev(unique(paste(pc$id,pc$publication))));pc$function_name<-factor(pc$function_name,levels=c('对象关联','事件与版本','受控访问','元素料占比','法定期限'))
p<-ggplot(pc,aes(function_name,row,fill=disclosure))+geom_tile(colour='white',linewidth=1)+geom_text(aes(label=disclosure,colour=disclosure=='权'),size=3,family=font)+scale_fill_manual(values=c('权'=teal,'说'='#E8D4B3','—'='#F0F3F4'),guide='none')+scale_colour_manual(values=c('TRUE'='white','FALSE'=ink),guide='none')+
 scale_x_discrete(position='top',labels=c('实物对象\n关联','事件与\n版本','受控\n访问','元素再生\n料核算','法定报送\n期限核查'),expand=c(0,0))+scale_y_discrete(expand=c(0,0))+labs(x=NULL,y=NULL)+theme(axis.line=element_blank(),axis.ticks=element_blank(),panel.grid=element_blank(),axis.text.y=element_text(size=6.6),axis.text.x=element_text(size=7.2),plot.margin=margin(8,8,8,7))
save_pub(p,'fig9_patent_policy',75,'公开专利覆盖身份、事件、访问及物料核算等不同任务，政策履行仍需配套规则和验证。')
write_json(contracts,file.path(root,'figure_contracts.json'),auto_unbox=TRUE,pretty=TRUE)
capture.output(sessionInfo(),file=file.path(root,'qa/R_sessionInfo.txt'))
while(!is.null(dev.list()))dev.off()
cat('EXPORTED',length(contracts),'R FIGURES\n')
