defmodule IEx.Pager do

  defrecord State, screen_height: 24, screen_width: 80, line: 0, col: 0, stash: nil, gl: nil

  def start_link(old_group_leader) do
    spawn_link(__MODULE__, :init, [State.new(get_screen_size(gl: old_group_leader))])
  end

  def init(state) do
    __MODULE__.loop(state)
    
  end

  def loop(state) do
    receive do
      req = {:io_request, from, reply_as, { :put_chars, :unicode, chars }} ->
        state = move_cursor(state, chars)
        case state.stash do
        nil ->    
          state.gl <- req
          loop(state)
        _ ->
          stash_length = String.length(state.stash)
          to_write = String.slice(chars, 0, String.length(chars) - stash_length)
          state.gl <- {:io_request, from, reply_as, { :put_chars, :unicode, to_write }}
          IO.write "paused: "
          pause(state, req)
        end

      (req = {:io_request, _from, _reply_as, request})
      when elem(request, 0) in [ :get_until, :get_chars, :get_line ] ->
        state.gl <- req
        loop(state.update(get_screen_size(line: 0, col: 0)))

      req = {:io_request, _from, _reply_as, _request} ->
        state.gl <- req
        loop(state)
    end
  end

  def pause(state, {:io_request, from, reply_as, { :put_chars, :unicode, _chars }}) do
    state.gl <- { :io_request, self, :pause, {:get_chars, :unicode, '', 1}}
    receive do
      { :io_reply, :pause, _char } ->
        nil
    end
    self <- {:io_request, from, reply_as, {:put_chars, :unicode, state.stash}}
    loop(state.update(stash: nil, line: 0, col: 0))
  end

  def move_cursor(state, "") do
    state
  end

  def move_cursor(state, << "\r"::utf8, chars::binary >>) do
    update_cursor(state, "", chars, col: 0)
  end

  def move_cursor(state, << "\n"::utf8, chars::binary >>) do
    update_cursor(state, "", chars, col: 0, line: state.line+1)
  end

  # for now, ignore most escape sequences
  def move_cursor(state, << "\e["::utf8, chars::binary>>) do
    move_cursor(state, skip_until_ansii_teminator(chars))
  end


  def move_cursor(state, << "\e"::utf8, char::utf8, chars::binary>>)
  when char in ?@..?_ do
    move_cursor(state, chars)
  end

  def move_cursor(state, << char::utf8, chars::binary >>) do
    update_cursor(state, char, chars, col: state.col + 1)
  end

  # This is where the magic happens. If moving the cursor would cause us to 
  # hit the bottom of the screen, we defer the rest of the string
  # and stash the string, setting the `wait` flag
  def update_cursor(state, char, chars, changes) do
    try do

      line = Dict.get(changes, :line, state.line)
      if line >= state.screen_height do
        throw {:pause, state, char <> chars}
      end

      col = Dict.get(changes, :col, state.col)
      if col >= state.screen_width do
        col = 0
        line = line + 1
        if line >= state.screen_height do
          throw {:pause, state, char <> chars}
        end
      end
      move_cursor(state.update(col: col, line: line), chars)

   catch :throw, { :pause, state, stash } ->
     state.update(stash: stash)
    end
  end

  def skip_until_ansii_teminator(<< ch::utf8, chars::binary >>) 
  when ch in ?@..?~ do
    chars
  end

  def skip_until_ansii_teminator(<< _ch::utf8, chars::binary >>) do
    skip_until_ansii_teminator(chars)
  end

  def get_screen_size(opts) do
    { :ok, height } = :io.rows()
    { :ok, width  } = :io.columns()
    Dict.merge([ screen_width: width, screen_height: height-1 ], opts)
  end
end

