module Orddict

def fetch([{k,_}|_], key, default) when key < k, do: default
def fetch([{k,_}|d], key, default) when key > k, do: fetch(d, key, default)
def fetch([{_k,value}|_], _key, _default),    do: value
def fetch([], _, default),                    do: default

def values([{_,v}|t]), do: [v|values(t)]
def values([]),        do: []

def delete([{k,_}=e|dict], key) when key < k, do: [e|dict]
def delete([{k,_}=e|dict], key) when key > k, do: [e|delete(dict, key)]
def delete([{_k,_v}|dict], _key), do: dict
def delete([], _), do: []