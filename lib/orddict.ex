module Orddict do
  def from_list(pairs) do
    List.foldl pairs, [], fn({k,v}, dict){ store(dict, k, v) }
  end

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

  def store([{k,_}=e|dict], key, new) when key < k, do: [{key,new},e|dict]
  def store([{k,_}=e|dict], key, new) when key > k, do: [e|store(dict, key, new)]
  def store([{_,_}|dict], key, new), do: [{key,new}|dict]
  def store([], key, new), do: [{key,new}]

  def merge([{k1,_}=e1|d1], [{k2,_}=e2|d2]) when k1 < k2, do: [e1|merge(d1, [e2|d2])];
  def merge([{k1,_}=e1|d1], [{k2,_}=e2|d2]) when k1 > k2, do: [e2|merge([e1|d1], d2)];
  def merge([{k1,_v1}|d1], [{_k2,v2}|d2]), do: [{k1,v2}|merge(d1, d2)];
  def merge([], d2), do: d2
  def merge(d1, []), do: d1
end