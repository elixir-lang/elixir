ns Orddict

def fetch: [ [{k,_}|_], key, default ] | key < k, do: default
def fetch: [ [{k,_}|d], key, default ] | key > k, do: fetch(d, key, default)
def fetch: [ [{_k,value}|_], _key, _default ],    do: value
def fetch: [ [], _, default ],                    do: default