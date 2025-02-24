<!--
  SPDX-License-Identifier: Apache-2.0
  SPDX-FileCopyrightText: 2021 The Elixir Team
  SPDX-FileCopyrightText: 2012 Plataformatec
-->

# Contributing to Elixir

We invite contributions to Elixir. To contribute, there are a few
things you need to know about the code. First, Elixir code is divided
by each application inside the `lib` folder:

  * `elixir` - Elixir's kernel and standard library

  * `eex` - EEx is the template engine that allows you to embed Elixir

  * `ex_unit` - ExUnit is a simple test framework that ships with Elixir

  * `iex` - IEx stands for Interactive Elixir: Elixir's interactive shell

  * `logger` - Logger is the built-in logger

  * `mix` - Mix is Elixir's build tool

You can run all tests in the root directory with `make test`. You can
also run tests for a specific framework with `make test_#{APPLICATION}`, for example,
`make test_ex_unit`. If you just changed something in Elixir's standard
library, you can run only that portion through `make test_stdlib`.

If you are only changing one file, you can choose to compile and run tests
for that specific file for faster development cycles. For example, if you
are changing the String module, you can compile it and run its tests as:

```sh
bin/elixirc lib/elixir/lib/string.ex -o lib/elixir/ebin
bin/elixir lib/elixir/test/elixir/string_test.exs
```

Some test files need their `test_helper.exs` to be explicitly required
before, such as:

```sh
bin/elixir -r lib/logger/test/test_helper.exs lib/logger/test/logger_test.exs
```

You can also use the `LINE` env var to run a single test:

```sh
LINE=123 bin/elixir lib/elixir/test/elixir/string_test.exs
````

To recompile all (including Erlang modules):

```sh
make compile
```

After your changes are done, please remember to run `make format` to guarantee
all files are properly formatted, then run the full suite with
`make test`.

If your contribution fails during the bootstrapping of the language,
you can rebuild the language from scratch with:

```sh
make clean_elixir compile
```

Similarly, if you can not get Elixir to compile or the tests to pass after
updating an existing checkout, run `make clean compile`. You can check
[the official build status](https://github.com/elixir-lang/elixir/actions/workflows/ci.yml).
More tasks can be found by reading the [Makefile](Makefile).

With tests running and passing, you are ready to contribute to Elixir and
[send a pull request](https://help.github.com/articles/using-pull-requests/).
We have saved some excellent pull requests we have received in the past in
case you are looking for some examples:

  * [Implement Enum.member? - Pull request](https://github.com/elixir-lang/elixir/pull/992)
  * [Add String.valid? - Pull request](https://github.com/elixir-lang/elixir/pull/1058)
  * [Implement capture_io for ExUnit - Pull request](https://github.com/elixir-lang/elixir/pull/1059)

## Reviewing changes

Once a pull request is sent, the Elixir team will review your changes.
We outline our process below to clarify the roles of everyone involved.

All pull requests must be approved by two committers before being merged into
the repository. If changes are necessary, the team will leave appropriate
comments requesting changes to the code. Unfortunately, we cannot guarantee a
pull request will be merged, even when modifications are requested, as the Elixir
team will re-evaluate the contribution as it changes.

Committers may also push style changes directly to your branch. If you would
rather manage all changes yourself, you can disable the "Allow edits from maintainers"
feature when submitting your pull request.

The Elixir team may optionally assign someone to review a pull request.
If someone is assigned, they must explicitly approve the code before
another team member can merge it.

When the review finishes, your pull request will be squashed and merged
into the repository. If you have carefully organized your commits and
believe they should be merged without squashing, please mention it in
a comment.

## Licensing and Compliance Requirements

Please review our [Open Source Policy](OPEN_SOURCE_POLICY.md) for complete
guidelines on licensing and compliance. Below is a summary of the key points
affecting **all external contributors**:

- Accepted Licenses: Any code contributed must be licensed under the
  `Apache-2.0` license.
- SPDX License Headers: With the exception of approved test fixture files,
  all new or modified files in a pull request must include correct SPDX
  headers. If you are creating a new file under the `Apache-2.0` license, for
  instance, please use:
    
    ```elixir
    # SPDX-License-Identifier: Apache-2.0
    # SPDX-FileCopyrightText: 2021 The Elixir Team
    ```
    
- No Executable Binaries: Contributions must **not** include any executable
  binary files. If you require an exception (for example, certain test artifacts),
  please see the policy on how to request approval and document exceptions.
- Preserving Copyright and License Info: If you copy code from elsewhere,
  ensure that **all original copyright and license notices remain intact**. If
  they are missing or incomplete, you must add them.
- Failure to Comply: Pull requests that do not meet these licensing and
  compliance standards will be rejected or require modifications before merging.
- Developer Certificate of Origin: All contributions are subject to the
  Developer Certificate of Origin.

    ```
    By making a contribution to this project, I certify that:

    (a) The contribution was created in whole or in part by me and I
        have the right to submit it under the open source license
        indicated in the file; or

    (b) The contribution is based upon previous work that, to the 
        best of my knowledge, is covered under an appropriate open 
        source license and I have the right under that license to   
        submit that work with modifications, whether created in whole
        or in part by me, under the same open source license (unless
        I am permitted to submit under a different license), as 
        Indicated in the file; or

    (c) The contribution was provided directly to me by some other
        person who certified (a), (b) or (c) and I have not modified
        it.

    (d) I understand and agree that this project and the contribution
        are public and that a record of the contribution (including 
        all personal information I submit with it, including my
        sign-off) is maintained indefinitely and may be redistributed
        consistent with this project or the open source license(s)
        involved.
    ```

    See http://developercertificate.org/ for a copy of the Developer Certificate
    of Origin license.

## Building documentation

Building the documentation requires that [ExDoc](https://github.com/elixir-lang/ex_doc)
is installed and built alongside Elixir:

```sh
# After cloning and compiling Elixir, in its parent directory:
git clone https://github.com/elixir-lang/ex_doc.git
cd ex_doc && ../elixir/bin/elixir ../elixir/bin/mix do deps.get + compile
```

Now go back to Elixir's root directory and run:

```sh
make docs                  # to generate HTML pages
make docs DOCS_FORMAT=epub # to generate EPUB documents
```

This will produce documentation sets for `elixir`, `eex`, `ex_unit`, `iex`, `logger`,
and `mix` under the `doc` directory. If you are planning to contribute documentation,
[please check our best practices for writing documentation](https://hexdocs.pm/elixir/writing-documentation.html).
