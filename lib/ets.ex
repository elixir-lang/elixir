% elixir: cache

object ETS
  module Mixin
    %if name specified, table would be named
    def create(name, opts)
      init_ets Erlang.ets.new(name, opts.push('named_table))
    end

    def create_named(name)
      init_ets Erlang.ets.new(name, ['named_table])
    end

    def create(opts)
      init_ets Erlang.ets.new('elixir_ets, opts.delete_all('named_table))
    end

    def create()
      init_ets Erlang.ets.new('elixir_ets, [])
    end

    def all()
      Erlang.ets.all()
    end

    def test_ms(tup, mat)
      Erlang.ets.test_ms(tup, mat)
    end

    def tabfile_info(f)
      Erlang.ets.tabfile_info(f)
    end

    def match_continuation(con)
      Erlang.ets.match(con)
    end

    def select_reverse_continuation(con)
      Erlang.ets.select_reverse(con)
    end

    def match_spec_compile(con)
      Erlang.ets.match_spec_compile(con)
    end

    def repair_continuation(con, mat)
      Erlang.ets.repair_continuation(con, mat)
    end

    def select_continuation(con)
      Erlang.ets.select(con)
    end

    def match_object_continuation(con)
      Erlang.ets.match_object(con)
    end

    def match_spec_run(list, cms)
      Erlang.ets.match_spec_run(list, cms)
    end

    def is_compiled_ms(term)
      Erlang.ets.is_compiled_ms(term)
    end

    def summary()
      Erlang.ets.i()
    end

    def fun2ms(fun)
      Erlang.ets.fun2ms(fun)
    end

    def file2tab(file)
      case Erlang.ets.file2tab(file)
        match {'ok, tab}
          init_ets tab
        match other
          other
      end
    end

    def file2tab(file, opts)
      case Erlang.ets.file2tab(file, opts)
        match {'ok, tab}
          init_ets tab
        match other
          other
      end
    end

    private

    def init_ets(result)
      ETS.new(result)
    end
  end

  def constructor(table)
    {'table: table}
  end

  def delete()
    Erlang.ets.delete(@table)
  end

  def delete(key)
    Erlang.ets.delete(@table, key)
  end

  def delete_all_objects()
    Erlang.ets.delete_all_objects(@table)
  end

  def delete_object(obj)
    Erlang.ets.delete_object(@table, obj)
  end

  def first()
    Erlang.ets.first(@table)
  end

  def fold_left(fun, acc)
    Erlang.ets.foldl(fun, acc, @table)
  end

  def fold_right(fun, acc)
    Erlang.ets.foldr(fun, acc, @table)
  end

  def from_dets(dets)
    Erlang.ets.from_dets(@table, dets)
  end

  def give_away(pid, gift)
    Erlang.ets.give_away(@table, pid, gift)
  end

  def i()
    Erlang.ets.i(@table)
  end

  def info()
    Erlang.ets.info(@table)
  end

  def info(item)
    Erlang.ets.info(@table, item)
  end

  def init_table(fun)
    Erlang.ets.init_table(@table, fun)
  end

  def insert(obj)
    Erlang.ets.insert(@table, obj)
  end

  def insert_new(obj)
    Erlang.ets.insert_new(@table, obj)
  end

  def last()
    Erlang.ets.last(@table)
  end

  def lookup(key)
    Erlang.ets.lookup(@table, key)
  end

  def lookup_element(key, pos)
    Erlang.ets.lookup_element(@table, key, pos)
  end

  def match_for(pattern)
    Erlang.ets.match(@table, pattern)
  end

  def match_for(pattern, limit)
    Erlang.apply('ets, 'match, [@table, pattern, limit])
  end

  def match_delete(pattern)
    Erlang.ets.match_delete(@table, pattern)
  end

  def match_object(pattern)
    Erlang.ets.match_object(@table, pattern)
  end

  def match_object(pattern, limit)
    Erlang.ets.match_object(@table, pattern, limit)
  end

  def member(key)
    Erlang.ets.member(@table, key)
  end

  def next(key)
    Erlang.ets.next(@table, key)
  end

  def prev(key)
    Erlang.ets.prev(@table, key)
  end

  def rename(key)
    Erlang.ets.rename(@table, key)
  end

  def select(spec)
    Erlang.ets.select(@table, spec)
  end

  def select(spec, limit)
    Erlang.ets.select(@table, spec, limit)
  end

  def safe_fixtable(key)
    Erlang.ets.safe_fixtable(@table, key)
  end

  def select_count(key)
    Erlang.ets.select_count(@table, key)
  end

  def select_delete(key)
    Erlang.ets.select_delete(@table, key)
  end

  def select_reverse(key)
    Erlang.ets.select_reverse(@table, key)
  end

  def select_reverse(key, limit)
    Erlang.ets.select_reverse(@table, key, limit)
  end

  def setopts(key)
    Erlang.ets.setopts(@table, key)
  end

  def slot(key)
    Erlang.ets.slot(@table, key)
  end

  def tab2file(key)
    Erlang.ets.tab2file(@table, key)
  end

  def tab2list()
    Erlang.ets.tab2list(@table)
  end

  def to_dets(key)
    Erlang.ets.to_dets(@table, key)
  end

  def update_element(key, obj)
    Erlang.ets.update_element(@table, key, obj)
  end

  def update_elements(key, list)
    Erlang.ets.update_element(@table, key, list)
  end
end

