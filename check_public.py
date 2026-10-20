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
 assert set(p.links)<=set(p.ids),'Broken internal anchor'
 assert not p.runtime,'External runtime dependency'
 assert not list(ROOT.rglob('*.docx')),'Private document in public directory'
 assert not list(ROOT.rglob('*.crdownload')),'Incomplete download in public directory'
 sources=json.loads((ROOT/'research/evidence.json').read_text('utf-8'))['sources']
 assert len(sources)==len({s['id'] for s in sources})==83
 figs=json.loads((ROOT/'research/figure_catalog.json').read_text('utf-8'))
 assert sorted(map(int,figs))==list(range(1,19))
 citations=json.loads((ROOT/'research/citation_map.json').read_text('utf-8'))
 assert sorted(citations.values())==list(range(1,75))
 assert '4.1.5日' not in text
 assert 'data:image/' in text and '<math' in text
 verify()
 print('PASS: 18 figures, 83 sources, 74 references; self-contained HTML; core models verified.')
if __name__=='__main__':main()
