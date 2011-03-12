% elixir: cache

module Record
  module Mixin
    def __added_as_proto__(base)
      base.mixin Record::Definition
    end

    def retrieve(name, 'from: string)
      file = string.to_char_list

      case Erlang.code.where_is_file(file)
      match 'non_existing
        realfile = file
      match realfile
      end

      retrieve_record(name, realfile)
    end

    def retrieve(name, 'from_lib: file)
      [app|path] = File.split(file)
      case Erlang.code.lib_dir(app.to_char_list)
      match {'error, _}
        self.error {'norecord, {name, file}}
      match libpath
        retrieve_record name, File.join([libpath|path])
      end
    end

    private

    def retrieve_record(name, file)
      case retrieve_from_file(file).keyfind(name, 0)
      match false
        self.error {'norecord, {name, file}}
      match {name, pairs}
        pairs
      end
    end

    def retrieve_from_file(file)
      [parse_record(record) for {'attribute, _, 'record, record} in read_file(file)]
    end

    def read_file(file)
      case Erlang.epp_dodger.quick_parse_file(file.to_bin)
      match {'ok, form}
        form
      match error
        self.error(error)
      end
    end

    def parse_record({name, fields})
      tuples = fields.map -> (f) parse_field(f)
      cons = tuples.foldr { 'nil, 0 }, -> (tuple, acc) { 'cons, 0, tuple, acc }
      { 'value, list, [] } = Erlang.erl_eval.expr(cons, [])
      { name,  list }
    end

    def parse_field({'typed_record_field, record_field, _type})
      parse_field(record_field)
    end

    def parse_field({'record_field, _, key })
      {'tuple, 0, [key, {'nil,0}]}
    end

    def parse_field({'record_field, _, key, value })
      {'tuple, 0, [key, value]}
    end
  end

  module Definition
    def record(name, options)
      pairs = Record.retrieve(name, options)
      { keys, values } = pairs.unzip

      self.attr_accessor keys

      self.module_eval __FILE__, __LINE__ + 1, ~~ELIXIR
  def record_name
    '#{name}
  end

  def record_keys
    #{keys.inspect}
  end

  def record_defaults
    #{values.inspect}
  end

  def record_size
    #{keys.size}
  end
~~
    end
  end

  def constructor(tuple)
    if tuple.__parent_name__ == 'Tuple && tuple[0] == self.record_name
      [_|pairs] = tuple.to_list
      OrderedDict.from_list self.record_keys.zip(pairs)
    else
      self.error { 'badrecord, tuple }
    end
  end

  def constructor()
    OrderedDict.from_list self.record_keys.zip(self.record_defaults)
  end

  def update(values)
    values.fold self, do (key, value, acc)
      acc.set_ivar(key, value)
    end
  end
end