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

sm<-fromJSON(file.path(root,'model/standards_model.json'))
# Chart 2 uses all official data and equal axes across four regional panels.
cdata<-ev$chemistry;cdata$chemistry<-factor(cdata$chemistry,levels=c('低镍','中高镍','磷酸铁锂'))
region_order<-c('中国','其他新兴市场和发展中经济体','欧盟','美国')
pp<-lapply(region_order,function(reg){z<-cdata[cdata$region==reg,];ggplot(z,aes(factor(year),share,fill=chemistry))+geom_col(width=.65)+scale_fill_manual(values=c('低镍'=gold,'中高镍'=blue,'磷酸铁锂'=teal),drop=FALSE)+scale_y_continuous(limits=c(0,100),breaks=c(0,50,100),labels=function(x)paste0(x,'%'),expand=c(0,0))+labs(title=if(reg==region_order[2])'其他新兴市场/发展中经济体' else reg,x=NULL,y='容量份额')+theme(panel.grid=element_blank(),panel.grid.major.y=element_blank(),plot.title=element_text(size=8.2),legend.position='bottom')})
save_pub(wrap_plots(pp,ncol=2,guides='collect')+plot_annotation(tag_levels='a')&theme(legend.position='bottom'),'fig2_chemistry',128,'IEA新注册车辆电池容量份额显示不同地区化学体系差异；不是退役结构。')
# 12: Binding regimes vs explicit inventory/age checks.
dc<-sm$dispatch_curve;dc$kind<-factor(dc$kind,levels=c('A类','B/C类期限'))
a<-ggplot(dc,aes(lambda_,h_max,colour=kind))+geom_line(linewidth=.7)+scale_colour_manual(values=c(teal,gold),guide='none')+geom_vline(xintercept=1.2,linetype=3,colour=gray,linewidth=.3)+annotate('text',x=1.4,y=17,label='A类：D=20日',hjust=0,size=2.7,family=font,colour=teal)+annotate('text',x=1.55,y=6.8,label='B/C类：D=5日',hjust=0,size=2.7,family=font,colour=gold)+scale_x_continuous(breaks=c(.5,1,1.5,2,2.5))+scale_y_continuous(limits=c(0,20),breaks=c(0,5,10,15,20),expand=expansion(mult=c(0,.03)))+labs(x='假设日入库量 λ（吨/日）',y='最大允许转运间隔 h（日）',title='期限与容量的约束切换',subtitle='K=8吨，Q₀=1吨，δ=1日')+theme(panel.grid.major.y=element_blank())
ck<-sm$dispatch_checks
long<-rbind(data.frame(h=ck$h,ratio=ck$peak/8,what='库存 / 容量'),data.frame(h=ck$h,ratio=ck$age/5,what='贮存 / B/C期限'))
long$what<-factor(long$what,levels=c('库存 / 容量','贮存 / B/C期限'))
b<-ggplot(long,aes(factor(h),ratio,fill=what))+geom_col(position=position_dodge(.75),width=.65)+geom_hline(yintercept=1,linetype=2,colour=ink,linewidth=.35)+scale_fill_manual(values=c(teal,gold))+scale_y_continuous(limits=c(0,1.85),breaks=c(0,.5,1,1.5),expand=c(0,0))+labs(x='假设转运间隔 h（日）',y='约束使用比例',title='同一情景的两项核对',subtitle='λ=1.2吨/日；虚线为约束上限')+theme(legend.position='bottom',legend.direction='vertical',panel.grid.major.y=element_blank())
save_pub(a+b+plot_layout(ncol=2)+plot_annotation(tag_levels='a'),'fig12_dispatch',115,'实体周转同时受期限和库存约束，不能用报送期限代替贮存条件。')
# 13: Full-chain mass balance, with separate interventions.
sf<-sm$recovery_surface
a<-ggplot(sf,aes(c,gamma,fill=R))+geom_raster()+geom_contour(aes(z=R),breaks=c(.3,.4,.5,.6,.7,.8),colour='white',linewidth=.25)+scale_fill_gradient(low='#EFF5EF',high='#277E82',limits=c(0,.85),labels=scales::percent_format(accuracy=1),name='链条回收')+scale_x_continuous(breaks=c(.4,.6,.8,1),labels=scales::percent_format(accuracy=1),expand=c(0,0))+scale_y_continuous(breaks=c(.7,.8,.9,1),labels=scales::percent_format(accuracy=1),expand=c(0,0))+labs(x='链条进入比例 c',y='有效工序进入比例 γ',title='同一工序率下的系统差异',subtitle='rLi=0.85；全部为条件网格')+theme(legend.position='bottom',legend.key.width=unit(10,'mm'),panel.grid.major.y=element_blank())
rr<-sm$recovery;rr$case<-factor(rr$case,levels=rr$case)
b<-ggplot(rr,aes(case,R))+geom_col(width=.55,fill=c(teal,blue,gold))+geom_text(aes(label=sprintf('%.2f%%',round(R*100+1e-9,2))),vjust=-.7,size=2.8,family=font,colour=ink)+scale_x_discrete(labels=c('参考','工序率\n0.90','进入率\n0.90'))+scale_y_continuous(limits=c(0,.9),breaks=c(0,.3,.6,.9),labels=scales::percent_format(accuracy=1),expand=c(0,0))+labs(x=NULL,y='全链条锂回收比例',title='不同条件变化的结果',subtitle='γ=0.95；各组变化见表8')+theme(panel.grid.major.y=element_blank())
save_pub(a+b+plot_layout(ncol=2)+plot_annotation(tag_levels='a'),'fig13_recovery',110,'85%的工序回收条件不能直接推为社会废电池回收率；前端进入和识别共同影响结果。')
# 14: Inventory decomposition and sensitivity share a single enterprise boundary.
cs<-sm$carbon_surface
a<-ggplot(cs,aes(f,e,fill=intensity))+geom_raster()+geom_contour(aes(z=intensity),breaks=c(.2,.4,.6,.8),colour='white',linewidth=.25)+scale_fill_gradient(low='#EDF4EF',high='#366B81',name='吨CO₂/吨')+scale_x_continuous(breaks=c(.2,.4,.6,.8),expand=c(0,0))+scale_y_continuous(breaks=c(.2,.4,.6,.8,1),expand=c(0,0))+labs(x='电力因子 f（吨CO₂/MWh）',y='单位处理用电 e（MWh/吨）',title='用电量与因子的联合影响',subtitle='其他净排放固定0.14吨CO₂/吨')+theme(legend.position='bottom',legend.key.width=unit(10,'mm'),panel.grid.major.y=element_blank())
cp<-sm$carbon_parts;cp$part<-factor(cp$part,levels=cp$part)
b<-ggplot(cp,aes(part,value))+geom_col(width=.55,fill=c(gray,gold,blue,teal))+geom_text(aes(label=sprintf('%.2f',value)),vjust=-.7,size=2.8,family=font,colour=ink)+scale_y_continuous(limits=c(0,.36),breaks=c(0,.1,.2,.3),expand=c(0,0))+labs(x=NULL,y='吨CO₂/吨处理量',title='参考情景的碳账分项',subtitle='e=0.60，f=0.50；合计0.44')+theme(panel.grid.major.y=element_blank())
save_pub(a+b+plot_layout(ncol=2)+plot_annotation(tag_levels='a'),'fig14_carbon',110,'企业碳核算需要一致边界与活动数据；电力因子敏感性不等同生命周期减排。')
write_json(contracts,file.path(root,'qa/new_figure_exports.json'),auto_unbox=TRUE,pretty=TRUE)
capture.output(sessionInfo(),file=file.path(root,'qa/R_sessionInfo.txt'))
while(!is.null(dev.list()))dev.off()
cat('DONE 4 NEW R FIGURES\n')
