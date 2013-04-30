defmodule Mix.OpenPath do
	@moduledoc """
	Open and read content from either a URL or a local filesystem path.

	Used by local.install and local.rebar.
	"""

	def read_path(path) do
    cond do
      is_url?(path)  -> read_url(path)
      is_file?(path) -> read_file(path)
      :else          -> raise Mix.Error, message: "expected #{path} to be a url or a local file path"
    end
  end



  defp read_file(path) do
    File.read!(path)
  end

  defp read_url(path) do
    if URI.parse(path).scheme == "https" do
      :ssl.start
    end

    :inets.start

    case :httpc.request(binary_to_list(path)) do
      { :ok, { { _, status, _ }, _, body } } when status in 200..299 ->
        iolist_to_binary(body)
      { :ok, { { _, status, _ }, _, _ } } ->
        raise Mix.Error, message: "could not access url #{path}, got status: #{status}"
      { :error, reason } ->
        raise Mix.Error, message: "could not access url #{path}, error: #{inspect reason}"
    end
  end

  defp is_file?(path) do
    File.regular?(path)
  end

  defp is_url?(path) do
    URI.parse(path).scheme in ["http", "https"]
  end
end
