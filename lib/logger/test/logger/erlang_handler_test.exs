defmodule Logger.ErlangHandlerTest do
  use Logger.Case
  @moduletag :logger

  defmodule CustomTranslator do
    def t(:debug, :info, :format, {'hello: ~p', [:ok]}) do
      :skip
    end

    def t(:debug, :info, :format, {'world: ~p', [:ok]}) do
      {:ok, "rewritten"}
    end

    def t(_, _, _, _) do
      :none
    end
  end

  test "add_translator/1 and remove_translator/1 for error_logger" do
    assert Logger.add_translator({CustomTranslator, :t})

    assert capture_log(fn ->
             :error_logger.info_msg('hello: ~p', [:ok])
           end) == ""

    assert capture_log(fn ->
             :error_logger.info_msg('world: ~p', [:ok])
           end) =~ "\[info\]  rewritten"
  after
    assert Logger.remove_translator({CustomTranslator, :t})
  end

  test "add_translator/1 and remove_translator/1 for logger" do
    assert Logger.add_translator({CustomTranslator, :t})
    # TODO: Add this
  after
    assert Logger.remove_translator({CustomTranslator, :t})
  end
end
