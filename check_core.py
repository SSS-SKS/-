"""Recompute conditional results with the Python standard library only."""
from pathlib import Path
import json, math, csv

def positive(x): return max(0.0,x)
def expected_window(tau=2,a=15,b=30):
    return (positive(tau+b)**3-positive(tau+b-a)**3-positive(tau)**3+positive(tau-a)**3)/(6*a*b)
def tail_probability(threshold=15,tau=2,a=15,b=30):
    return (positive(tau+b-threshold)**2-positive(tau+b-a-threshold)**2-positive(tau-threshold)**2+positive(tau-a-threshold)**2)/(2*a*b)
def turnover(D=5,K=8,Q0=1,arrival=1.2,preparation=0.5):
    if D<=0 or K<0 or Q0<0 or arrival<=0 or preparation<0:raise ValueError('Invalid scenario input')
    return min(D-preparation,(K-Q0)/arrival-preparation)
def verify():
    values={'expected_window_days':expected_window(),'probability_over_15_days':tail_probability(),
      'improved_turnover_days':turnover(),'baseline_turnover_days':turnover(preparation=1),
      'lithium_chain_recovery':.8*.95*.85,'enterprise_emission_intensity':.6*.5+.08+.04+.02,
      'example_window_days':max(0,2+20-10)}
    expected={'expected_window_days':10.313703703703704,'probability_over_15_days':19/60,
      'improved_turnover_days':4.5,'baseline_turnover_days':4,'lithium_chain_recovery':.646,
      'enterprise_emission_intensity':.44,'example_window_days':12}
    for k,v in values.items(): assert math.isclose(v,expected[k],rel_tol=1e-12,abs_tol=1e-12),(k,v)
    # Independent midpoint quadrature checks the two analytic window formulas.
    n=1200;a=15;b=30;total=0;tail=0
    for i in range(n):
      u=(i+.5)*a/n
      # Integrate the uniform V distribution exactly at each U midpoint.
      lo=max(0,u-2)
      total+=max(0,((2-u)*(b-lo)+(b*b-lo*lo)/2))/b
      tail+=min(1,max(0,(b-(15-2+u))/b))
    assert abs(total/n-values['expected_window_days'])<1e-5
    assert abs(tail/n-values['probability_over_15_days'])<1e-6
    return values
if __name__=='__main__':
    values=verify()
    print(json.dumps({'passed':True,'results':values},ensure_ascii=False,indent=2))
