# Reader guide and a policy-title extract. No empirical observations are generated.
suppressPackageStartupMessages({library(grid);library(jsonlite)})
a<-commandArgs(trailingOnly=TRUE);root<-a[1];out<-if(length(a)>1)a[2] else file.path(root,'figures');dir.create(out,showWarnings=FALSE,recursive=TRUE)
font<-if('Noto Sans CJK SC' %in% systemfonts::system_fonts()$family) 'Noto Sans CJK SC' else 'sans-serif';ink<-'#173F4A';teal<-'#278A83';muted<-'#667D84';gold<-'#BD965E'
qr<-fromJSON(file.path(root,'qr-matrix.json'));n<-nrow(qr$matrix);quiet<-qr$quiet_zone
text<-function(label,x,y,size=8,col=ink,face='plain',just='centre')grid.text(label,x,y,just=just,gp=gpar(fontfamily=font,fontsize=size,col=col,fontface=face))
line<-function(x,y,col=teal,lwd=1)grid.lines(x,y,gp=gpar(col=col,lwd=lwd))
circle<-function(x,y,r=.03,col=teal,fill='white')grid.circle(x,y,r,gp=gpar(col=col,fill=fill,lwd=1))
export<-function(draw,name,h){for(ext in c('png','pdf','svg')){
 f<-tempfile(fileext=paste0('.',ext));w<-145/25.4;hh<-h/25.4
 if(ext=='png')ragg::agg_png(f,width=w,height=hh,units='in',res=600,background='white')
 if(ext=='pdf')cairo_pdf(f,width=w,height=hh,family=font)
 if(ext=='svg')svglite::svglite(f,width=w,height=hh)
 grid.newpage();pushViewport(viewport(gp=gpar(fontsize=8,fontfamily=font)));draw();popViewport();dev.off();b<-readBin(f,'raw',n=file.info(f)$size);writeBin(b,file.path(out,paste0(name,'.',ext)));writeLines(base64_enc(b),file.path(out,paste0(name,'.',ext,'.b64')));unlink(f)
}}
# This is one composed schematic; boxes represent steps, not comparable data axes.
guide<-function(){
 text('动力电池回收研究资料库',.03,.92,12,face='bold',just='left')
 text('政策文本与公开证据  2018—2026',.03,.825,7.5,col=muted,just='left')
 xs<-c(.10,.33,.56);ys<-.60
 line(c(.10,.56),c(ys,ys),col='#A9C5C5',lwd=1.5)
 for(i in seq_along(xs))circle(xs[i],ys,.042,fill='white')
 # Exact icon geometry: original document, linked event, plotted result.
 grid.rect(xs[1],ys,.036,.067,gp=gpar(col=teal,fill='white',lwd=.8));line(xs[1]+c(-.010,.010),c(ys+.011,ys+.011),lwd=.6);line(xs[1]+c(-.010,.010),c(ys-.010,ys-.010),lwd=.6)
 circle(xs[2]-.012,ys+.007,.010);circle(xs[2]+.012,ys-.007,.010);line(xs[2]+c(-.003,.003),ys+c(.003,-.003),lwd=.8)
 line(xs[3]+c(-.018,-.018,.019),ys+c(.026,-.025,-.025),lwd=.7);line(xs[3]+c(-.012,-.001,.011,.018),ys+c(-.012,.010,.003,.022),lwd=1)
 labels<-c('核对原文','分析机制','复查结论');sub<-c('政策  标准  文献  专利','责任事件  时间  工程约束','原文链接  数据  R代码')
 for(i in seq_along(xs)){text(labels[i],xs[i],.438,8.6,face='bold');text(sub[i],xs[i],.338,6.8,col=muted)}
 # Counts refer to the whole current evidence database, not a systematic-review sample.
 text('82条资料',.10,.19,8.7,face='bold');text('22项文献评述',.33,.19,8.7,face='bold');text('10项标准',.56,.19,8.7,face='bold')
 line(c(.705,.705),c(.15,.79),col='#DCE6E6',lwd=.7)
 # 37 modules plus four-module quiet zone, rendered without logos or stylisation.
 size<-33/145;x0<-.84-size/2;y0<-.54-size*145/76/2;cell<-size/(n+2*quiet)
 for(i in seq_len(n))for(j in seq_len(n))if(qr$matrix[i,j])grid.rect(x0+(j+quiet-.5)*cell,y0+(n-i+quiet+.5)*cell*145/76,width=cell,height=cell*145/76,gp=gpar(col=NA,fill=ink))
 text('扫码访问公开资料库',.84,.17,7.2,face='bold')
 text('资料与稿件截至2026年10月20日',.03,.055,6.7,col=muted,just='left')
}
policy<-function(){
 # Genuine official image, kept at its native aspect ratio; text below is an explicitly labelled re-typeset extract.
 mark<-as.raster(fromJSON(file.path(root,'mark-raster.json')))
 grid.raster(mark,.5,.86,width=unit(8,'mm'),height=unit(8*61/56,'mm'),interpolate=FALSE)
 text('工业和信息化部规章',.5,.67,9.5,face='bold')
 text('新能源汽车废旧动力电池回收',.5,.48,11,face='bold')
 text('和综合利用管理暂行办法',.5,.34,11,face='bold')
 text('六部门令第73号',.5,.18,8.5,col=muted)
 text('2025年12月31日公布   2026年4月1日起施行',.5,.065,7.3,col=muted)
}
export(guide,'reader_guide',76);export(policy,'policy_header',60)
write_json(list(backend='R grid',width_mm=145,dpi=600,figures=list(reader_guide=list(height_mm=76,role='research-library navigation',panels='single schematic, no comparable axes',source='82-record evidence database and verified public site',qr_url=qr$url,qr_quiet_zone_modules=4),policy_header=list(height_mm=60,role='official-source identification',original_image='miit-mark.png',image_url='https://wap.miit.gov.cn/cms_files/filemanager/1226211233/picture/202112/S37e0b1a045ab4834b76b88fc89acb415-56.png',treatment='Original mark preserved; policy title and dates re-typeset, not a screenshot or fabricated red-head document'))),file.path(root,'qa/figure_contract.json'),auto_unbox=TRUE,pretty=TRUE)
