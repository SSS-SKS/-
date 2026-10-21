"""Validate the anonymous V2 portable website and model material."""
from pathlib import Path
from html.parser import HTMLParser
import json,re,base64,hashlib
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
 assert not list(ROOT.rglob('*.docx')) and not list(ROOT.rglob('*.crdownload'))
 data=json.loads(re.search(r'<script id="research-data" type="application/json">(.*?)</script>',text,re.S)[1])
 assert data['version']=='V2' and len(data['sources'])==84
 assert len(data['figures'])==18 and len(data['assets'])==18
 expected={str(i) for i in range(1,9)}|{'A'+str(i) for i in range(1,11)}
 assert {f['number'] for f in data['figures']}==expected
 assert set(data['citations'].values())==set(range(1,77))
 assert len({s['id'] for s in data['sources']})==84
 assert set(data['citations'])<={s['id'] for s in data['sources']}
 assert all('ref-'+str(i) in p.ids for i in range(1,77))
 assert len(json.loads((ROOT/'research/evidence.json').read_text('utf-8'))['sources'])==84
 assert 'data:image/' in text and '<math' in text
 for f in data['figures']:
  raw=base64.b64decode(data['assets'][f['asset']].split(',',1)[1])
  assert hashlib.sha256(raw).hexdigest()==f['asset']
  path=ROOT/'research'/('source' if f['original'] else 'figures')/f['file']
  assert path.read_bytes()==raw
 verify()
 print('PASS: V2 anonymous site, 18 figures, 84 sources, 76 references, offline assets and core models.')
if __name__=='__main__':main()
