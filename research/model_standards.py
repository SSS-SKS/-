from pathlib import Path
import json,csv,math
r=Path(__file__).parent
m=dict(parameters={'lambda':1.2,'K':8,'Q0':1,'delta':1,'h':4,'D_A':20,'D_BC':5,'c':.8,'gamma':.95,'r_Li':.85,'r_NCM':.98,'electricity':.6,'factor':.5,'combustion':.08,'process':.04,'transport':.02,'export':0,'CCU':0})
p=m['parameters']
m['dispatch']=[dict(kind=k,lambda_=lam,D=D,K=8,Q0=1,delta=1,h_max=max(0,min(D,7/lam)-1),binding='期限' if D<7/lam else '容量') for k,D in [('A类',20),('B/C类期限',5)] for lam in [.5,1.2,2]]
m['dispatch_curve']=[dict(kind=k,lambda_=i/100,D=D,h_max=max(0,min(D,7/(i/100))-1)) for k,D in [('A类',20),('B/C类期限',5)] for i in range(20,251)]
m['dispatch_checks']=[dict(h=h,age=h+1,peak=1.2*(h+1)+1,capacity=1.2*(h+1)+1<=8,A=h+1<=20,BC=h+1<=5) for h in [3,4,5,7]]
m['recovery']=[dict(case='参考',c=.8,gamma=.95,r=.85,R=.8*.95*.85),dict(case='工序率0.90',c=.8,gamma=.95,r=.9,R=.8*.95*.9),dict(case='进入率0.90',c=.9,gamma=.95,r=.85,R=.9*.95*.85)]
m['recovery_surface']=[dict(c=c/100,gamma=g/100,R=c/100*g/100*.85) for c in range(40,101,2) for g in range(70,101,2)]
m['carbon']=[dict(factor=f,electricity=.6,other=.14,intensity=.6*f+.14) for f in [.2,.5,.8]]
m['carbon_surface']=[dict(e=e/100,f=f/100,intensity=e/100*f/100+.14) for e in range(20,101,2) for f in range(10,91,2)]
m['carbon_parts']=[dict(part=k,value=v) for k,v in [('燃烧',.08),('工业过程',.04),('运输',.02),('购入电力',.30)]]
# Independent explicit physical checks and mass/energy bookkeeping.
assert math.isclose(p['lambda']*(p['h']+p['delta'])+p['Q0'],7)
assert math.isclose(sum(x['value'] for x in m['carbon_parts']),m['carbon'][1]['intensity'])
assert all(0<=x['R']<=x['c']<=1 for x in m['recovery'])
for x in m['dispatch']:
 age=x['h_max']+x['delta'];peak=x['lambda_']*age+x['Q0'];assert age<=x['D']+1e-10 and peak<=x['K']+1e-10
for key in ['dispatch','dispatch_checks','recovery','recovery_surface','carbon','carbon_surface','carbon_parts']:
 with (r/'model'/('standard_'+key+'.csv')).open('w') as f:
  w=csv.DictWriter(f,fieldnames=list(m[key][0]));w.writeheader();w.writerows(m[key])
(r/'model/standards_model.json').write_text(json.dumps(m,ensure_ascii=False,indent=2))
print('PASS',m['dispatch'],m['recovery'],m['carbon'])
