# Core claim: event-linked physical and information records require separate clocks.
# Conceptual synthesis and labelled scenarios; no empirical observations are invented.
# Export contract: 183 mm, editable PDF/SVG text, PNG 300 dpi, glyphs >= 8 pt.
args <- commandArgs(trailingOnly = TRUE)
out <- normalizePath(args[1], mustWork = FALSE)
.libPaths(c(file.path(dirname(out),'runtime','Rlib'),.libPaths()))
dir.create(out, recursive = TRUE, showWarnings = FALSE)
ink <- '#172D3E'; green <- '#276653'; muted <- '#536372'; rule <- '#BECBD0'
family <- 'FangSong'
setup <- function() {
  par(mar=c(0,0,0,0),xaxs='i',yaxs='i',family=family,cex=1)
  plot.new();plot.window(c(0,1),c(0,1))
}
txt <- function(x,y,s,size=9,col=ink,adj=.5,font=1) {
  chosen_family <- if(grepl('^[ -~]+$',s)) 'Times New Roman' else family
  text(x,y,s,cex=size/12,col=col,adj=adj,font=font,family=chosen_family)
}
box <- function(x,y,w,h,label,fill='#F2F6F5',size=9) {
  rect(x,y,x+w,y+h,col=fill,border=rule,lwd=.6)
  lines <- strsplit(label,'\n',fixed=TRUE)[[1]]
  for(i in seq_along(lines))txt(x+w/2,y+h/2+(length(lines)+1-2*i)*.016,lines[i],size=if(i==1)size else size-.5,font=if(i==1)2 else 1)
}
arrow <- function(x0,y0,x1,y1,dash=FALSE) {
  arrows(x0,y0,x1,y1,length=.055,angle=25,col=if(dash)muted else green,lwd=1,lty=if(dash)2 else 1)
}
save_fig <- function(name,w,h,draw) {
  grDevices::cairo_pdf(file.path(out,paste0(name,'.pdf')),width=w/25.4,height=h/25.4,family=family,pointsize=12)
  setup();draw();dev.off()
  svglite::svglite(file.path(out,paste0(name,'.svg')),width=w/25.4,height=h/25.4,pointsize=12)
  setup();draw();dev.off()
  grDevices::png(file.path(out,paste0(name,'.png')),width=w,height=h,units='mm',res=600,type='cairo',family=family,pointsize=12)
  setup();draw();dev.off()
}
draw_mechanism <- function() {
  txt(.045,.955,'a  实物流转与信息记录',11,adj=0,font=2)
  xs <- c(.045,.29,.535,.78);w <- .175
  labels <- c('来源主体\n编码与来源凭证','收集型网点\n分类、暂存、出库','运输与交接\n运单与签收','综合利用企业\n入库与处理')
  fields <- c('身份关联\n对象、主体、批次','出库事件\n发生、提交、回执','交接凭证\n实际到货与更正','入库事件\n发生、提交、回执')
  for(i in seq_along(xs)) {
    box(xs[i],.785,w,.11,labels[i]);box(xs[i],.61,w,.11,fields[i],fill='#F7F8FA')
    arrow(xs[i]+w/2,.78,xs[i]+w/2,.728,TRUE)
    if(i<4){arrow(xs[i]+w+.008,.84,xs[i+1]-.008,.84);arrow(xs[i]+w+.008,.665,xs[i+1]-.008,.665,TRUE)}
  }
  txt(.5,.565,'实线：实物流转     虚线：信息关联；字段为研究设计建议',8.5,col=muted)
  segments(.045,.525,.955,.525,col=rule,lwd=.6)
  txt(.045,.48,'b  信息时钟：交接假设案例',10,adj=0,font=2)
  tx <- c(.065,.33,.595,.86)
  segments(tx[1],.36,tx[4],.36,col=green,lwd=1)
  points(tx,rep(.36,4),pch=21,bg='white',col=green,cex=.7)
  for(i in 1:4)txt(tx[i],.405,c('第0日出库','第2日入库','第10日报出库','第22日报入库')[i],8.4)
  txt(.195,.31,'实际运输 2 日',8.5)
  txt(.725,.31,'单向待匹配 W = 12 日',8.5)
  txt(.5,.255,'U = 10；V = 20；τ = 2；W = max(0, τ + V - U)',9)
  txt(.5,.21,'两方均按各自期限报送；此时间轴按事件顺序示意，不按长度比例',8.2,col=muted)
  segments(.045,.17,.955,.17,col=rule,lwd=.6)
  txt(.045,.125,'c  实物时钟：分类贮存与容量',10,adj=0,font=2)
  txt(.5,.075,'上海收集型网点 B/C 类情景：h + δ ≤ 5 日；峰值库存 ≤ K',9)
  txt(.5,.035,'用同一批次关联核验；信息期限、实物期限与系统等待时间分别计算',8.5,col=green)
}
save_fig('fig18_event_integration',183,150,draw_mechanism)

qr <- as.matrix(read.csv(file.path(dirname(out),'qr-matrix.csv'),header=FALSE))
draw_guide <- function() {
  txt(.055,.86,'研究资料入口',15,adj=0,font=2)
  txt(.055,.68,'图表  /  公开来源  /  模型计算  /  数据下载',10,adj=0)
  txt(.055,.50,'https://sss-sks.github.io/-/',10,adj=0)
  txt(.055,.30,'GitHub 备用入口',9,adj=0,col=muted)
  txt(.055,.18,'https://github.com/SSS-SKS/-',9,adj=0)
  n <- nrow(qr);side <- .28;left <- .68;bottom <- .17
  rect(left-.022,bottom-.05,left+side+.022,bottom+.64,col='white',border=NA)
  for(i in 1:n)for(j in 1:n)if(qr[i,j]==1) {
    x <- left+(j-1)/n*side;y <- bottom+(n-i)/n*.60
    rect(x,y,x+side/n,y+.60/n,col=ink,border=NA)
  }
}
save_fig('reader_guide',183,85,draw_guide)

draw_header <- function() {
  mark <- as.matrix(read.csv(file.path(dirname(out),'official-mark.csv'),header=FALSE))
  rasterImage(as.raster(mark),.880,.86,.928,.988,interpolate=FALSE)
  txt(.055,.73,'工业和信息化部等六部门令',13,adj=0,font=2)
  txt(.055,.55,'第 73 号',18,adj=0,font=2,col=green)
  txt(.055,.37,'新能源汽车废旧动力电池回收和综合利用管理暂行办法',11,adj=0)
  txt(.055,.18,'2026 年 4 月 1 日起施行',14,adj=0,font=2)
  txt(.055,.045,'标识采用官方原图；文号、日期据原文重排，非官网截图',8.5,adj=0,col=muted)
}
save_fig('policy_header',183,75,draw_header)
cat('Exported 3 figures in PDF/SVG/PNG.\n')
