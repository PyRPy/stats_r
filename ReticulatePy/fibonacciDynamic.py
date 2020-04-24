from typing import Dict
memo: Dict[int, int] = {0:0, 1:1}

def fibPyD(n):
    if n not in memo:
        memo[n] = fibPyD(n-1) + fibPyD(n - 2)
    return memo[n]
    
