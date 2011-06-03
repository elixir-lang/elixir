% Most methods simply map to Erlang ones. Use ETS.new(tid|atom) to wrap an
% existing ETS table. Use ETS.create to create a new one.
module ETS
  def new([table])
    #ETS::Instance(table)
  end

  % Creates new ets table.
  def create(name, options)
    #ETS::Instance(Erlang.ets.new(name, options.to_list))
  end

  % Returns a list of all tables at the node. Named tables are
  % given by their names, unnamed tables are given by their table identifiers.
  def all()
    Erlang.ets.all()
  end

  % Displays information about all ETS tables.
  def summary()
    Erlang.ets.i()
  end

  module Instance
    def __bound__(table)
      @('table: table)
    end

    % Deletes the entire table.
    def delete()
      Erlang.ets.delete(@table)
    end

    % Deletes all objects with the given *key* from the table.
    def delete(key)
      Erlang.ets.delete(@table, key)
    end

    % Returns the first key in the table. If the table is of the
    % ordered_set type, the first key in Erlang term order will be returned.
    % If the table is of any other type, the first key according to the table's
    % internal order will be returned. If the table is empty, '"$end_of_table" will be returned.
    def first()
      Erlang.ets.first(@table)
    end

    % Insert the given object in the table.
    %
    % If the table is a set and the key of the inserted objects matches the key of any object in
    % the table, the old object will be replaced. If the table is an ordered_set and the key of the
    % inserted object compares equal to the key of any object in the table, the old object is also
    % replaced. If the list contains more than one object with matching keys and the table is a set,
    % one will be inserted, which one is not defined. The same thing holds for ordered_set, but will
    % also happen if the keys compare equal.
    %
    % The entire operation is guaranteed to be atomic and isolated.
    def insert(obj)
      Erlang.ets.insert(@table, obj)
    end

    % This function works exactly like insert, with the exception that instead of overwriting objects
    % with the same key (in the case of set or ordered_set) or adding more objects with keys already existing
    % in the table (in the case of bag and duplicate_bag), it simply returns false. If obj is
    % a list, the function checks every key prior to inserting anything. Nothing will be inserted if
    % not all keys present in the list are absent from the table. Like `insert`, the entire operation is
    % guaranteed to be atomic and isolated.
    def insert_new(obj)
      Erlang.ets.insert_new(@table, obj)
    end

    % Returns the last key according to Erlang term order in the table Tab of the ordered_set
    % type. If the table is of any other type, the function is synonymous to first. If the table
    % is empty, '"$end_of_table" is returned.
    def last()
      Erlang.ets.last(@table)
    end

    % Returns a list of all objects with the key in the table.
    def lookup(key)
      Erlang.ets.lookup(@table, key)
    end

    % If the table is of type set or ordered_set, the function returns the pos:th
    % element of the object with the *key*.
    %
    % If the table is of type bag or duplicate_bag, the functions returns a list with the
    % pos:th element of every object with the key.
    def lookup_element(key, pos)
      Erlang.ets.lookup_element(@table, key, pos)
    end

    % Works like lookup/2, but does not return the objects. The function returns
    % true if one or more elements in the table has the *key*, false otherwise.
    def member?(key)
      Erlang.ets.member(@table, key)
    end
    alias_local 'member?, 'include?, 1

    % Returns a list of all objects in the table.
    def to_list()
      Erlang.ets.tab2list(@table)
    end

    % This function provides an efficient way to update one or more elements within
    % an object, without the hassle of having to look up, update and write back the entire object.
    %
    % It will destructively update the object with key. The element at the pos:th position will be
    % given the value Value.
    def update_element(key, pos, value)
      Erlang.ets.update_element(@table, key, {pos, value})
    end

    % Same as update element, but receives a list of pairs to be updated.
    def update_elements(key, pairs)
      Erlang.ets.update_element(@table, key, pairs)
    end
  end
end