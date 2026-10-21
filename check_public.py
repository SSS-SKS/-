"""Validate the portable site, public file scope, and published data counts."""
from pathlib import Path
from html.parser import HTMLParser
import json,re
from check_core import verify
ROOT=Path(__file__).parent
class Page(HTMLParser):
 def __init__(self):super().__init__();self.ids=[];self.links=[];self.runtime=[]
 def handle_starttag(self,tag,attrs):
  a=dict(attrs)
  if 'id' in a:self.ids.append(a['id'])
  if tag=='a' and a.get('href','').startswith('#'):self.links.append(a['href'][1:])
  if tag=='script' and a.get('src'):self.runtime.append(a['src'])
  if tag=='link' and a.get('rel')=='stylesheet':self.runtime.append(a.get('href'))
def main():
 text=(ROOT/'index.html').read_text('utf-8');p=Page();p.feed(text)
 assert len(p.ids)==len(set(p.ids)),'Duplicate HTML ID'
 routes={'overview','policy','literature','analysis','evidence','mechanism','standards','patents','outlook','figures','downloads'}
 assert all(x in p.ids or x.split('/')[0] in routes for x in p.links),'Broken internal route'
 assert not p.runtime,'External runtime dependency'
 assert not list(ROOT.rglob('*.docx')),'Private document in public directory'
 assert not list(ROOT.rglob('*.crdownload')),'Incomplete download in public directory'
 assert not any(x in text for x in ['江苏科技大学','research-report.docx','DATA.report','function report()']),'Report or institutional information in website'
 data=json.loads(re.search(r'<script id="research-data" type="application/json">(.*?)</script>',text,re.S)[1])
 assert 'report' not in data and 'search' not in data,'Report text embedded in website'
 assert len(data['sources'])==83 and len(data['figures'])==18
 sources=json.loads((ROOT/'research/evidence.json').read_text('utf-8'))['sources']
 assert len(sources)==len({s['id'] for s in sources})==83
 figs=json.loads((ROOT/'research/figure_catalog.json').read_text('utf-8'))
 assert sorted(map(int,figs))==list(range(1,19))
 citations=json.loads((ROOT/'research/citation_map.json').read_text('utf-8'))
 assert sorted(citations.values())==list(range(1,75))
 assert '4.1.5日' not in text
 assert 'data:image/' in text and '<math' in text
 verify()
 print('PASS: independent website; 18 figures, 83 sources; no report text; self-contained HTML; core models verified.')
if __name__=='__main__':main()
