Q := @

clean:
	rm -f elixir.1
	rm -f iex.1

build:
	$(Q) cp iex.1.in iex.1
	$(Q) sed -i"" "/{COMMON}/r common" iex.1
	$(Q) sed -i"" "/{COMMON}/d" iex.1
	$(Q) cp elixir.1.in elixir.1
	$(Q) sed -i"" "/{COMMON}/r common" elixir.1
	$(Q) sed -i"" "/{COMMON}/d" elixir.1
