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
 for(ext in c('svg','pdf','png')){
  tf<-tempfile(fileext=paste0('.',ext))
  if(ext=='svg')svglite::svglite(tf,width=w,height=h)
  if(ext=='pdf')cairo_pdf(tf,width=w,height=h,family=font)
  if(ext=='png')ragg::agg_png(tf,width=w,height=h,units='in',res=600,background='white')
  print(p);dev.off();bytes<-readBin(tf,'raw',n=file.info(tf)$size);stopifnot(length(bytes)>1000)
  writeBin(bytes,paste0(f,'.',ext));writeLines(jsonlite::base64_enc(bytes),paste0(f,'.',ext,'.b64'));unlink(tf)
 }
 cat('FILE',name,file.info(paste0(f,'.png'))$size,'DEV',dev.list(),'\n')
 contracts[[name]]<<-list(claim=claim,panels=panels,backend='R ggplot2/patchwork',width_mm=145,height_mm=height_mm,dpi=600,
  uncertainty='法定范围、解析模型分位或技术披露类别；无企业样本、标准误或置信区间')
}
tailW<-function(T,tau=2,a=15,b=30){x<-T-tau;pmax(0,pmin(1,(pmax(b-x,0)^2-pmax(b-x-a,0)^2-pmax(-x,0)^2+pmax(-x-a,0)^2)/(2*a*b)))}
meanW<-function(tau=2,a=15,b=30)(pmax(tau+b,0)^3-pmax(tau+b-a,0)^3-pmax(tau,0)^3+pmax(tau-a,0)^3)/(6*a*b)
quantW<-function(p,tau=2,a=15,b=30){c<-min(a,b)/(2*max(a,b));z<-if(p<c)-a+sqrt(2*a*b*p) else if(p>1-c)b-sqrt(2*a*b*(1-p)) else (b-a)/2+max(a,b)*(p-.5);max(0,tau+z)}
for(s in seq_len(nrow(m$scenarios))){r<-m$scenarios[s,];stopifnot(abs(meanW(r$tau)-r$mean)<1e-9,abs(quantW(.95,r$tau)-r$q95)<1e-9,abs(tailW(15,r$tau)-r$p_over_T)<1e-9)}
write_json(list(status='PASS',baseline=list(mean=meanW(),median=quantW(.5),q95=quantW(.95),p_over_15=tailW(15)),comparison='R closed forms agree with independently integrated source model'),file.path(root,'qa/r_numeric_check.json'),pretty=TRUE,auto_unbox=TRUE)
# Figure 1: complete annual scale and market composition.
sales<-ev$sales
vol<-rbind(data.frame(year=sales$year,value=sales$production,metric="产量"),data.frame(year=sales$year,value=sales$sales,metric="销量"))
vol$metric<-factor(vol$metric,levels=c("产量","销量"))
a<-ggplot(vol,aes(factor(year),value,fill=metric))+geom_col(position=position_dodge(width=.74),width=.64)+scale_fill_manual(values=c("产量"="#C4D2D8","销量"=teal),name=NULL)+geom_text(data=data.frame(year=sales$year,value=sales$sales,metric=factor("销量",levels=c("产量","销量"))),aes(label=scales::comma(value,accuracy=.1)),nudge_x=.18,vjust=-.7,size=2.4,family=font,colour=ink)+scale_x_discrete(expand=expansion(add=c(.7,1.0)))+scale_y_continuous(limits=c(0,1900),breaks=c(0,500,1000,1500),labels=scales::comma,expand=c(0,0))+labs(x=NULL,y="万辆",title="产销规模持续扩大",subtitle="完整年度；行业统计口径")+theme(legend.position="bottom",legend.text=element_text(size=7),panel.grid.major.y=element_blank())
b<-ggplot(sales,aes(year,share))+geom_line(linewidth=.7,colour=blue)+geom_point(size=2.1,colour=blue)+geom_text(aes(label=scales::percent(share,accuracy=.1)),nudge_y=.035,hjust=1,size=2.6,family=font,colour=ink)+scale_x_continuous(breaks=2020:2025,expand=expansion(mult=c(.16,.09)))+scale_y_continuous(limits=c(0,.57),breaks=c(0,.15,.30,.45),labels=scales::percent_format(accuracy=0.1),expand=c(0,0))+labs(x=NULL,y="新能源汽车新车销量占比",title="市场结构同步变化",subtitle="新能源汽车销量 / 汽车新车总销量")+theme(panel.grid.major.y=element_blank())
save_pub(a+b+plot_layout(ncol=2,guides="collect")+plot_annotation(tag_levels="a")&theme(legend.position="bottom"),"fig1_sales",86,"绝对产销规模与新车市场占比共同上升，不能直接推为同期退役量。")
# Figure 9: exact mixed distribution, without artificial company observations.
summ<-data.frame(tau=c(0,2,5,10));summ$p0<-sapply(summ$tau,function(t)1-tailW(0,t));summ$mean<-sapply(summ$tau,meanW);summ$q95<-sapply(summ$tau,function(t)quantW(.95,t));summ$max<-summ$tau+30
summ$group<-factor(summ$tau,levels=rev(c(0,2,5,10)));summ$colour<-factor(summ$tau,levels=c(0,2,5,10))
den<-do.call(rbind,lapply(summ$tau,function(t){w<-seq(.001,40,length.out=1000);z<-w-t;data.frame(tau=t,w=w,density=pmax(0,pmin(30,z+15)-pmax(0,z))/(15*30))}));den$colour<-factor(den$tau,levels=c(0,2,5,10))
# Density integrates to the positive probability mass for each exact distribution.
for(t in summ$tau){v<-integrate(function(w)pmax(0,pmin(30,w-t+15)-pmax(0,w-t))/(15*30),0,t+30,rel.tol=1e-9)$value;stopifnot(abs(v+summ$p0[summ$tau==t]-1)<1e-6)}
a<-ggplot(summ,aes(y=group,colour=colour))+geom_segment(aes(x=0,xend=max,yend=group),linewidth=1.1)+geom_point(aes(x=0),shape=124,size=4)+geom_point(aes(x=max),shape=124,size=4)+geom_text(aes(x=max+1.2,label=paste0("0–",max)),hjust=0,size=2.5,family=font)+scale_x_continuous(limits=c(0,47),breaks=c(0,15,30,45),expand=c(0,0))+scale_y_discrete(labels=function(x)paste0("τ=",x,"日"))+scale_colour_manual(values=cols,guide="none")+labs(x="可行待匹配时长 W（日）",y=NULL,title="确定性可行范围",subtitle="a=15日，b=30日；不设报送分布")+theme(panel.grid.major.y=element_blank())
b<-ggplot(den,aes(w,density,colour=colour))+geom_line(linewidth=.65)+scale_colour_manual(values=cols,labels=paste0("τ=",c(0,2,5,10),"日"),name=NULL)+scale_x_continuous(limits=c(0,42),breaks=c(0,15,30,40),expand=c(0,0))+scale_y_continuous(limits=c(0,.041),breaks=c(0,.01,.02,.03,.04),labels=scales::number_format(accuracy=.01),expand=c(0,0))+labs(x="正值待匹配时长 W（日）",y="连续概率密度（日⁻¹）",title="正值部分的解析密度",subtitle="曲线面积为1−P(W=0)")+theme(panel.grid.major.y=element_line(linewidth=.2,colour="#E6ECEF"))
c<-ggplot(summ,aes(group,p0,fill=colour))+geom_col(width=.56)+geom_text(aes(label=scales::percent(p0,accuracy=.1)),vjust=-.65,size=2.7,family=font,colour=ink)+scale_fill_manual(values=cols,guide="none")+scale_x_discrete(limits=as.character(c(0,2,5,10)),labels=paste0("τ=",c(0,2,5,10)))+scale_y_continuous(limits=c(0,.32),breaks=c(0,.1,.2,.3),labels=scales::percent_format(accuracy=1),expand=c(0,0))+labs(x="假设运输 / 交接间隔（日）",y="零值概率 P(W=0)",title="零值离散质量单列",subtitle="接收记录不晚于交出记录的情景概率")+theme(panel.grid.major.y=element_blank())
d<-ggplot(summ,aes(y=group,colour=colour))+geom_segment(aes(x=mean,xend=q95,yend=group),linewidth=.55)+geom_point(aes(x=mean),size=2,shape=16)+geom_point(aes(x=q95),size=2.4,shape=2,stroke=.7)+geom_text(aes(x=mean,label=sprintf("%.1f",mean)),nudge_y=-.24,vjust=1,size=2.5,family=font)+geom_text(aes(x=q95,label=sprintf("%.1f",q95)),nudge_y=.28,vjust=0,size=2.5,family=font)+scale_colour_manual(values=cols,guide="none")+scale_y_discrete(labels=function(x)paste0("τ=",x,"日"))+scale_x_continuous(limits=c(0,40),breaks=c(0,15,30,40),expand=c(0,0))+labs(x="模型待匹配时长（日）",y=NULL,title="中心时长与右尾比较",subtitle="圆点：均值；三角：95%模型分位")+theme(panel.grid.major.y=element_blank())
save_pub(wrap_plots(a,b,c,d,ncol=2,guides="collect")+plot_annotation(tag_levels="a")&theme(legend.position="bottom"),"fig7_lag_intervals_violin",145,"区分确定性边界、连续密度与零值概率质量，并以模型均值及95%分位表达情景时长。")
write.csv(summ,file.path(root,"model/lag_panel_summary.csv"),row.names=FALSE)
write.csv(den[,c("tau","w","density")],file.path(root,"model/lag_positive_density.csv"),row.names=FALSE)
# Figure 12: distinct roles for bound, operating envelope, checks and interventions.
lambda<-seq(.2,2.5,length.out=240);cur<-rbind(data.frame(lambda=lambda,h=pmin(20,7/lambda)-1,kind="A类：20日"),data.frame(lambda=lambda,h=pmin(5,7/lambda)-1,kind="B/C类：5日"))
a<-ggplot(cur,aes(lambda,h,colour=kind))+geom_line(linewidth=.7)+scale_colour_manual(values=c("A类：20日"=teal,"B/C类：5日"=gold),name=NULL)+geom_vline(xintercept=c(.35,1.4),linetype=3,colour="#B5C0C5",linewidth=.3)+scale_x_continuous(breaks=c(.5,1,1.5,2,2.5))+scale_y_continuous(limits=c(0,20),breaks=c(0,5,10,15,20),expand=c(0,0))+labs(x="假设日入库量 λ（吨/日）",y="最大允许转运间隔 h（日）",title="期限与容量的上界切换",subtitle="K=8吨，Q₀=1吨，δ=1日")+theme(legend.position="bottom",panel.grid.major.y=element_blank())
grid<-expand.grid(lambda=seq(.2,2.5,length.out=100),K=seq(2,16,length.out=100));grid$hmax<-pmax(0,pmin(5,(grid$K-1)/grid$lambda)-1)
b<-ggplot(grid,aes(lambda,K))+geom_raster(aes(fill=hmax))+geom_contour(aes(z=hmax),breaks=c(1,2,3),colour="white",linewidth=.25)+geom_line(data=data.frame(lambda=lambda,K=1+5*lambda),aes(lambda,K),inherit.aes=FALSE,colour=ink,linetype=2,linewidth=.45)+geom_point(data=data.frame(lambda=1.2,K=8),colour=gold,shape=21,fill="white",size=2,stroke=.7)+scale_fill_gradient(low="#E9F0EE",high="#277F81",limits=c(0,4),name="最大h（日）")+scale_x_continuous(breaks=c(.5,1,1.5,2,2.5),expand=c(0,0))+scale_y_continuous(breaks=c(2,6,10,14),expand=c(0,0))+labs(x="假设日入库量 λ（吨/日）",y="假设有效容量 K（吨）",title="B/C类的参数可行域",subtitle="虚线：h上界达到4日的容量边界")+theme(legend.position="bottom",legend.key.width=unit(8,"mm"),panel.grid.major.y=element_blank())
check<-data.frame(h=rep(c(3,4,5,7),2),ratio=c((1.2*(c(3,4,5,7)+1)+1)/8,(c(3,4,5,7)+1)/5),what=rep(c("库存 / 容量","贮存时间 / 期限"),each=4));check$what<-factor(check$what,levels=c("库存 / 容量","贮存时间 / 期限"))
c<-ggplot(check,aes(factor(h),ratio,fill=what))+geom_col(position=position_dodge(width=.7),width=.6)+geom_hline(yintercept=1,linetype=2,colour=ink,linewidth=.35)+scale_fill_manual(values=c(teal,gold),name=NULL)+scale_y_continuous(limits=c(0,1.8),breaks=c(0,.5,1,1.5),expand=c(0,0))+labs(x="假设转运间隔 h（日）",y="必要约束使用比例",title="同一条件的双约束核对",subtitle="λ=1.2，K=8；虚线=约束上限")+theme(legend.position="bottom",panel.grid.major.y=element_blank())
pert<-data.frame(scenario=c("参考条件","容量增至10吨","入库降至0.8吨/日","准备缩至0.5日","入库增至2吨/日"),lambda=c(1.2,1.2,.8,1.2,2),K=c(8,10,8,8,8),delta=c(1,1,1,.5,1));pert$hmax<-pmin(5,(pert$K-1)/pert$lambda)-pert$delta;pert$scenario<-factor(pert$scenario,levels=rev(pert$scenario))
d<-ggplot(pert,aes(hmax,scenario))+geom_vline(xintercept=4,linetype=2,colour="#B5C0C5",linewidth=.35)+geom_segment(aes(x=0,xend=hmax,yend=scenario),colour="#D5DFE1",linewidth=.65)+geom_point(size=2.3,colour=teal)+geom_text(aes(label=sprintf("%.1f",hmax)),nudge_x=.18,hjust=0,size=2.7,family=font,colour=ink)+scale_x_continuous(limits=c(0,5.25),breaks=c(0,2,4,5),expand=c(0,0))+labs(x="B/C类最大转运间隔（日）",y=NULL,title="明确参数扰动的执行情景",subtitle="参考：λ=1.2，K=8，δ=1")+theme(axis.text.y=element_text(size=6.8),panel.grid.major.y=element_blank())
# Different legend types remain local; plot-area alignment is measured after guide layout.
save_pub(wrap_plots(a,b,c,d,ncol=2,guides="collect")+plot_annotation(tag_levels="a")&theme(legend.position="bottom",legend.box="vertical",legend.title=element_text(size=7),legend.spacing.y=unit(1,"pt")),"fig12_dispatch",154,"提高容量、缩短准备或改变日入库量对最大转运周期的作用取决于期限和容量哪个先约束。")
write.csv(grid,file.path(root,"model/dispatch_capacity_surface.csv"),row.names=FALSE)
write.csv(pert,file.path(root,"model/dispatch_interventions.csv"),row.names=FALSE)
write_json(contracts,file.path(root,"qa/revised_figure_exports.json"),pretty=TRUE,auto_unbox=TRUE)
capture.output(sessionInfo(),file=file.path(root,"qa/R_sessionInfo_revisions.txt"))
