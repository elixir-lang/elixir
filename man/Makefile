Q := @

build:
	$(Q) cp iex.1.in iex.1
	$(Q) sed -i.bak "/{COMMON}/r common" iex.1
	$(Q) sed -i.bak "/{COMMON}/d" iex.1
	$(Q) cp elixir.1.in elixir.1
	$(Q) sed -i.bak "/{COMMON}/r common" elixir.1
	$(Q) sed -i.bak "/{COMMON}/d" elixir.1
	$(Q) rm *.bak

clean:
	rm -f elixir.1
	rm -f iex.1
