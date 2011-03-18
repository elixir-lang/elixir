% elixir: cache


% Most of functions use native erlang calls. in some of them added error catches
object ETS
  module Mixin
    % Creates new ets table. it's many args, so it will
    % be great with named args =\
    %
    def create(type := 'set, keypos := 1, visibility := 'protected, name := 'elixir_ets, heir := 'none, write_concurrency := false, read_concurrency := false)
      init_ets Erlang.ets.new(name, [type, visibility, {'keypos, keypos}, {'heir, heir}, {'write_concurrency, write_concurrency}, {'read_concurrency, read_concurrency}])
    end

    % Returns a list of all tables at the node. Named tables are
    % given by their names, unnamed tables are given by their table identifiers.
    %
    def all()
      Erlang.ets.all()
    end

%    def test_ms(tup, mat)
%      Erlang.ets.test_ms(tup, mat)
%    end

%    def tabfile_info(f)
%      Erlang.ets.tabfile_info(f)
%    end

%    def match_continuation(con)
%      Erlang.ets.match(con)
%    end

%    def select_reverse_continuation(con)
%      Erlang.ets.select_reverse(con)
%    end

%    def match_spec_compile(con)
%      Erlang.ets.match_spec_compile(con)
%    end

%    def repair_continuation(con, mat)
%      Erlang.ets.repair_continuation(con, mat)
%    end

%    def select_continuation(con)
%      Erlang.ets.select(con)
%    end

%    def match_object_continuation(con)
%      Erlang.ets.match_object(con)
%    end

%    def match_spec_run(list, cms)
%      Erlang.ets.match_spec_run(list, cms)
%    end

%    def is_compiled_ms(term)
%      Erlang.ets.is_compiled_ms(term)
%    end

    % Displays information about all ETS tables.
    %
    def summary()
      Erlang.ets.i()
    end

%    def fun2ms(fun)
%      Erlang.ets.fun2ms(fun)
%    end

    % Reads a file produced by tab2file and creates the corresponding table.
%    def file2tab(file, verify := false)
%      case Erlang.ets.file2tab(file, [{'verify, verify}])
%        match {'ok, tab}
%          init_ets tab
%        match {'error, reason}
%          reason
%      end
%    end

    private

    % Erlang native call to create ets table
    %
    def init_ets(result)
      ETS.new(result)
    end
  end

  def constructor(table)
    {'table: table}
  end

  % Deletes the entire table.
  %
  def delete()
    Erlang.ets.delete(@table)
  end

  % Deletes all objects with the key Key from the table.
  %
  def delete(key)
    Erlang.ets.delete(@table, key)
  end

  % Delete all objects in the table. The operation is guaranteed to be atomic and isolated.
  %
  def delete_all_objects()
    Erlang.ets.delete_all_objects(@table)
  end

  % Delete the exact object Object from the ETS table, leaving objects with
  % the same key but other differences (useful for type bag). In a duplicate_bag,
  % all instances of the object will be deleted.
  %
  def delete_object(obj)
    Erlang.ets.delete_object(@table, obj)
  end

  % Returns the first key Key in the table Tab. If the table is of the
  % ordered_set type, the first key in Erlang term order will be returned.
  % If the table is of any other type, the first key according to the table's
  % internal order will be returned. If the table is empty, 'eof will be returned.
  % Use next to find subsequent keys in the table.
  %
  def first()
    case Erlang.ets.first(@table)
      match '"$end_of_table"
        'eof
      match other
        other
    end
  end

%  def fold_left(fun, acc)
%    Erlang.ets.foldl(fun, acc, @table)
%  end

%  def fold_right(fun, acc)
%    Erlang.ets.foldr(fun, acc, @table)
%  end

%  def from_dets(dets)
%    Erlang.ets.from_dets(@table, dets)
%  end

%  def give_away(pid, gift)
%    Erlang.ets.give_away(@table, pid, gift)
%  end

  % Browses the table.
  %
  def i()
    Erlang.ets.i(@table)
  end

  % Returns information about the table Tab as a list of {item, value} tuples.
  % Item=memory, Value=int() : The number of words allocated to the table.
  % Item=owner, Value=pid() : The pid of the owner of the table.
  % Item=heir, Value=pid() | none : The pid of the heir of the table, or none if no heir is set.
  % Item=name, Value=atom() : The name of the table.
  % Item=size, Value=int() : The number of objects inserted in the table.
  % Item=node, Value=atom() : The node where the table is stored. This field is no
  %                           longer meaningful as tables cannot be accessed from other nodes.
  % Item=named_table, Value=true | false : Indicates if the table is named or not.
  % Item=type, Value = 'set | 'ordered_set | 'bag | 'duplicate_bag : The table type.
  % Item=keypos, Value=int() : The key position.
  % Item=protection, Value = 'public | 'protected | 'private : The table access rights.
  % Item=compressed, Value=true | false : Indicates if the table is compressed or not.
  %
  def info()
    Erlang.ets.info(@table)
  end

  % Returns the information associated with Item for the table, or returns 'undefined
  % if table does not refer an existing ETS table.
  %
  def info(item)
    Erlang.ets.info(@table, item)
  end

%  def init_table(fun)
%    Erlang.ets.init_table(@table, fun)
%  end

  % Inserts the object or all of the objects in the list ObjectOrObjects into the table.
  % If the table is a set and the key of the inserted objects matches the key of any object in
  % the table, the old object will be replaced. If the table is an ordered_set and the key of the
  % inserted object compares equal to the key of any object in the table, the old object is also
  % replaced. If the list contains more than one object with matching keys and the table is a set,
  % one will be inserted, which one is not defined. The same thing holds for ordered_set, but will
  % also happen if the keys compare equal.
  % The entire operation is guaranteed to be atomic and isolated, even when a list of objects is inserted.
  %
  def insert(obj)
    Erlang.ets.insert(@table, obj)
  end

  % This function works exactly like insert, with the exception that instead of overwriting objects
  % with the same key (in the case of set or ordered_set) or adding more objects with keys already existing
  % in the table (in the case of bag and duplicate_bag), it simply returns false. If obj is
  % a list, the function checks every key prior to inserting anything. Nothing will be inserted if
  % not all keys present in the list are absent from the table. Like insert/2, the entire operation is
  % guaranteed to be atomic and isolated.
  %
  def insert_new(obj)
    Erlang.ets.insert_new(@table, obj)
  end

  % Returns the last key Key according to Erlang term order in the table Tab of the ordered_set
  % type. If the table is of any other type, the function is synonymous to first. If the table
  % is empty, 'eof is returned.
  % Use prev to find preceding keys in the table.
  %
  def last()
    case Erlang.ets.last(@table)
      match '"$end_of_table"
        'eof
      match other
        other
    end
  end

  % Returns a list of all objects with the key Key in the table Tab. In the case of set,
  % bag and duplicate_bag, an object is returned only if the given key matches the key of
  % the object in the table. If the table is an ordered_set however, an object is returned
  % if the key given compares equal to the key of an object in the table. The difference
  % being the same as between =:= and ==. As an example, one might insert an object with
  % the integer()1 as a key in an ordered_set and get the object returned as a result of
  % doing a lookup/2 with the float()1.0 as the key to search for.
  % If the table is of type set or ordered_set, the function returns either the empty
  % list or a list with one element, as there cannot be more than one object with the
  % same key. If the table is of type bag or duplicate_bag, the function returns a list of
  % arbitrary length.
  % Note that the time order of object insertions is preserved; The first object inserted
  % with the given key will be first in the resulting list, and so on.
  % Insert and look-up times in tables of type set, bag and duplicate_bag are constant,
  % regardless of the size of the table. For the ordered_set data-type, time is
  % proportional to the (binary) logarithm of the number of objects.
  %
  def lookup(key)
    Erlang.ets.lookup(@table, key)
  end

  % If the table Tab is of type set or ordered_set, the function returns the Pos:th
  % element of the object with the key Key.
  % If the table is of type bag or duplicate_bag, the functions returns a list with the
  % Pos:th element of every object with the key Key.
  % If no object with the key Key exists, the function will exit with reason badarg.
  % The difference between set, bag and duplicate_bag on one hand, and ordered_set
  % on the other, regarding the fact that ordered_set's view keys as equal when they
  % compare equal whereas the other table types only regard them equal when they match,
  % naturally holds for lookup_element as well as for lookup.
  %
  def lookup_element(key, pos)
    try
      Erlang.ets.lookup_element(@table, key, pos)
    catch 'error: reason
      reason
    end
  end

%  def match_for(pattern)
%    Erlang.ets.match(@table, pattern)
%  end

%  def match_for(pattern, limit)
%    Erlang.apply('ets, 'match, [@table, pattern, limit])
%  end

%  def match_delete(pattern)
%    Erlang.ets.match_delete(@table, pattern)
%  end

%  def match_object(pattern)
%    Erlang.ets.match_object(@table, pattern)
%  end

%  def match_object(pattern, limit)
%    Erlang.ets.match_object(@table, pattern, limit)
%  end

  % Works like lookup/2, but does not return the objects. The function
  % returns true if one or more elements in the table has the key Key, false otherwise.
  %
  def member(key)
    Erlang.ets.member(@table, key)
  end

  % Returns the next key Key2, following the key Key1 in the table Tab.
  % If the table is of the ordered_set type, the next key in Erlang term order is
  % returned. If the table is of any other type, the next key according to the table's
  % internal order is returned. If there is no next key, 'eof is returned.
  % Use first/1 to find the first key in the table.
  %
  def next(key)
    case Erlang.ets.next(@table, key)
      match '"$end_of_table"
        'eof
      match other
        other
    end
  end

  % Returns the previous key Key2, preceding the key Key1 according the Erlang term order
  % in the table Tab of the ordered_set type. If the table is of any other type, the function
  % is synonymous to next/2. If there is no previous key, '$end_of_table' is returned.
  % Use last/1 to find the last key in the table.
  %
  def prev(key)
    case Erlang.ets.prev(@table, key)
      match '"$end_of_table"
        'eof
      match other
        other
    end
  end

  % Renames the named table Tab to the new name Name. Afterwards, the
  % old name can not be used to access the table. Renaming an unnamed table has no effect.
  %
  def rename(key)
    Erlang.ets.rename(@table, key)
  end

%  def select(spec)
%    Erlang.ets.select(@table, spec)
%  end

%  def select(spec, limit)
%    Erlang.ets.select(@table, spec, limit)
%  end

%  def safe_fixtable(key)
%    Erlang.ets.safe_fixtable(@table, key)
%  end

%  def select_count(key)
%    Erlang.ets.select_count(@table, key)
%  end

%  def select_delete(key)
%    Erlang.ets.select_delete(@table, key)
%  end

%  def select_reverse(key)
%    Erlang.ets.select_reverse(@table, key)
%  end

%  def select_reverse(key, limit)
%    Erlang.ets.select_reverse(@table, key, limit)
%  end

%  def setopts(opts)
%    Erlang.ets.setopts(@table, opts)
%  end

%  def slot(key)
%    Erlang.ets.slot(@table, key)
%  end

%  def tab2file(file)
%    Erlang.ets.tab2file(@table, file)
%  end

  % Returns a list of all objects in the table Tab.
  %
  def tab2list()
    case Erlang.ets.tab2list(@table)
      match {'error, reason}
        reason
      match other
        other
    end
  end

%  def to_dets(key)
%    Erlang.ets.to_dets(@table, key)
%  end

  % This function provides an efficient way to update one or more elements within
  % an object, without the hassle of having to look up, update and write back the entire object.
  % It will destructively update the object with key Key in the table Tab. The element
  % at the Pos:th position will be given the value Value.
  % A list of {Pos,Value} can be supplied to update several elements within the same object.
  % If the same position occurs more than one in the list, the last value in the list
  % will be written. If the list is empty or the function fails, no updates will be done at all.
  % The function is also atomic in the sense that other processes can never see any intermediate results.
  % The function returns true if an object with the key Key was found, false otherwise.
  %
  def update_element(key, pos, value)
    try
      Erlang.ets.update_element(@table, key, {pos, value})
    catch 'error: reason
      reason
    end
  end

%  def update_elements(key, list)
%    try
%      Erlang.ets.update_element(@table, key, list)
%    catch 'error: reason
%      reason
%    end
%  end
end

