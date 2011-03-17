% elixir: cache

% Allow Erlang records to be imported into Elixir. For example,
% we can retrieve the `file_info` record from Erlang as follow:
%
%     Code.require "record"
% 
%     object FileInfo
%       proto Record
%       record 'file_info, 'from_lib: "kernel/include/file.hrl"
%     end
% 
%     % Manually access the Erlang file:read_file_info method
%     % passing the current file as a char list.
%     { 'ok, info } = Erlang.file.read_file_info(__FILE__.to_char_list)
% 
%     % Create a new FileInfo object based on the tuple returned above
%     record = FileInfo.new info
% 
%     % Profit by accessing the record info
%     record.access % => 'read_write
%
% Besides defining an `attr_accessor` for each field in the record, that
% allows you to read and update each attribute, you also have the following
% methods available to manipulate the record:
%
% * constructor() - creates an instance for this record using the default
%   values given in the record declaration.
%
% * constructor(tuple) - receives a tuple that matches the record. If the first
%   element of the tuple does not match the record name and the tuple size does
%   match the amount of fields in the record, an error is raised.
%
% * constructor(ordered_dict) - receives an ordered dict and creates a new record
%   using the values for the given keys in the dict and using the default values
%   for the keys that were not supplied.
%
% * update(ordered_dict) - receives an ordered dict that updates each element
%   in the record.
% 
% Besides the methods above, calling `record` adds the following methods for reflection:
%
% * record_name
% * record_keys
% * record_defaults
% * record_size
%
module Record
  % Mixin methods for the Record module. They are basically
  % method that handles retrieving record definitions from
  % Erlang files.
  module Mixin
    def __added_as_proto__(base)
      base.mixin Record::Definition
    end

    % Retrieve a record definition from an Erlang file using
    % the same lookup as the *include* attribute from Erlang modules.
    def retrieve(name, 'from: string)
      file = string.to_char_list

      case Erlang.code.where_is_file(file)
      match 'non_existing
        realfile = file
      match realfile
      end

      retrieve_record(name, realfile)
    end

    % Retrieve a record definition from an Erlang file using
    % the same lookup as the *include_lib* attribute from Erlang modules.
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

    % Retrieve the record with the given name from the given file
    % Raises {'norecord, { name, file } } if the record does not exist.
    def retrieve_record(name, file)
      case retrieve_from_file(file).keyfind(name, 0)
      match false
        self.error {'norecord, {name, file}}
      match record
        parse_record(record)
      end
    end

    % Parse the given file and retrieve all existent records.
    def retrieve_from_file(file)
      [record for {'attribute, _, 'record, record} in read_file(file)]
    end

    % Read a file and return its abstract syntax form that also
    % includes record and other preprocessor modules. This is done
    % by using Erlang's epp_dodger.
    def read_file(file)
      case Erlang.epp_dodger.quick_parse_file(file.to_bin)
      match {'ok, form}
        form
      match error
        self.error(error)
      end
    end

    % Parse a tuple with name and fields and returns a list of second order tuples
    % where the first element is the field and the second is its default value.
    def parse_record({_name, fields})
      tuples = fields.map -> (f) parse_field(f)
      cons = tuples.foldr { 'nil, 0 }, -> (tuple, acc) { 'cons, 0, tuple, acc }
      { 'value, list, [] } = Erlang.erl_eval.expr(cons, [])
      list
    end

    def parse_field({'typed_record_field, record_field, _type})
      parse_field(record_field)
    end

    def parse_field({'record_field, _, key })
      {'tuple, 0, [key, {'atom, 0, 'nil}]}
    end

    def parse_field({'record_field, _, key, value })
      {'tuple, 0, [key, value]}
    end
  end

  module Definition
    % Method to be used in objects that adds Record as proto.
    % It accepts a name and 'from or 'from_lib as option as
    % described in `Record#retrieve`.
    %
    % For each record field, this method defines an `attr_accessor`
    % and also defines the following methods:
    %
    % * record_name
    % * record_keys
    % * record_defaults
    % * record_size
    %
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

  % If it receives a tuple as argument, it checks if the tuple matches the record.
  % If the first element of the tuple does not match the record name and the tuple
  % size does match the amount of fields in the record, a `'badrecord` error is raised.
  %
  % If the argument is an ordered dict, it creates a new record using the values for
  % the given keys in the dict and using the default values for the keys that were
  % not supplied.
  %
  % If the given object is none of the above, a `badarg error is raised.
  def constructor(object)
    case object.__parent_name__
    match 'Tuple
      if object[0] == self.record_name && object.size == self.record_keys.size + 1
        [_|pairs] = object.to_list
        OrderedDict.from_list self.record_keys.zip(pairs)
      else
        self.error { 'badrecord, object }
      end
    match 'OrderedDict
      constructor().merge(object)
    else
      self.error { 'badarg, object }
    end
  end

  % Creates a new record using the default values as defaults.
  def constructor()
    OrderedDict.from_list self.record_keys.zip(self.record_defaults)
  end

  % Update the record using the given ordered dict *values*.
  def update(values)
    @(values)
  end
end