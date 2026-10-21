from pathlib import Path
from html.parser import HTMLParser
import json,re,hashlib,base64
from check_core import verify
P=Path(__file__).parent
class Page(HTMLParser):
 def __init__(self):super().__init__();self.ids=[];self.external=[]
 def handle_starttag(self,t,attrs):
  a=dict(attrs)
  if 'id' in a:self.ids.append(a['id'])
  if t=='script' and a.get('src'):self.external.append(a['src'])
  if t=='link' and a.get('rel')=='stylesheet':self.external.append(a.get('href'))
def main():
 h=(P/'index.html').read_text('utf-8');p=Page();p.feed(h)
 assert len(p.ids)==len(set(p.ids)) and not p.external
 d=json.loads(re.search(r'<script id="research-data" type="application/json">(.*?)</script>',h,re.S)[1])
 assert len(d['sources'])==84 and len({s['id'] for s in d['sources']})==84
 assert len(d['chapters'])==6 and len(d['appendices'])==8
 assert set(d['citations'].values())==set(range(1,77))
 assert len(d['figures'])==18 and len(d['assets'])==18
 assert {f['number'] for f in d['figures']}=={str(i) for i in range(1,9)}|{'A'+str(i) for i in range(1,11)}
 assert not list(P.rglob('*.docx')) and not list(P.rglob('*.crdownload'))
 for f in d['figures']:
  raw=base64.b64decode(d['assets'][f['asset']].split(',',1)[1]);assert hashlib.sha256(raw).hexdigest()==f['asset']
 for c in d['chapters']:
  for section in c['sections']:assert {int(n) for n in re.findall(r'\[(\d+)\]',section['text'])}<=set(range(1,77))
 verify()
 print('PASS: portable four-view site; 6 chapters, 4 models, 84 sources, 76 references, 18 figures.')
if __name__=='__main__':main()
