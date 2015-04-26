# Contributing to Elixir

Please take a moment to review this document in order to make the contribution
process easy and effective for everyone involved!

## Using the issue tracker

Use the issues tracker for:

* [bug reports](#bug-reports)
* [submitting pull requests](#pull-requests)

Please **do not** use the issue tracker for personal support requests nor feature requests. Support requests should be sent to:

* [the elixir-talk mailing list](http://groups.google.com/group/elixir-lang-talk)
* [Stack Overflow](http://stackoverflow.com/questions/ask?tags=elixir)
* **[#elixir-lang](irc://chat.freenode.net/elixir-lang)** IRC channel on [chat.freenode.net](http://www.freenode.net/)

Feature requests can be discussed on [the elixir-core mailing list](http://groups.google.com/group/elixir-lang-core).

We do our best to keep the issue tracker tidy and organized, making it useful
for everyone. For example, we classify open issues per application and perceived
difficulty of the issue, making it easier for developers to
[contribute to Elixir](#contributing).

## Bug reports

A bug is a _demonstrable problem_ that is caused by the code in the repository.
Good bug reports are extremely helpful - thank you!

Guidelines for bug reports:

1. **Use the GitHub issue search** &mdash; check if the issue has already been
   reported.

2. **Check if the issue has been fixed** &mdash; try to reproduce it using the
   `master` branch in the repository.

3. **Isolate and report the problem** &mdash; ideally create a reduced test
   case.

Please try to be as detailed as possible in your report. Include information about
your Operating System, your Erlang and Elixir versions. Please provide steps to
reproduce the issue as well as the outcome you were expecting! All these details
will help developers to fix any potential bugs.

Example:

> Short and descriptive example bug report title
>
> A summary of the issue and the environment in which it occurs. If suitable,
> include the steps required to reproduce the bug.
>
> 1. This is the first step
> 2. This is the second step
> 3. Further steps, etc.
>
> `<url>` - a link to the reduced test case (e.g. a GitHub Gist)
>
> Any other information you want to share that is relevant to the issue being
> reported. This might include the lines of code that you have identified as
> causing the bug, and potential solutions (and your opinions on their
> merits).

## Feature requests

Feature requests are welcome and should be discussed on [the elixir-core mailing list](http://groups.google.com/group/elixir-lang-core). But take a moment to find
out whether your idea fits with the scope and aims of the project. It's up to *you*
to make a strong case to convince the community of the merits of this feature.
Please provide as much detail and context as possible.

## Contributing

We incentivize everyone to contribute to Elixir and help us tackle
existing issues! To do so, there are a few things you need to know
about the code. First, Elixir code is divided in applications inside
the `lib` folder:

* `elixir` - Contains Elixir's kernel and stdlib

* `eex` - Template engine that allows you to embed Elixir

* `ex_unit` - Simple test framework that ships with Elixir

* `iex` — IEx, Elixir's interactive shell

* `mix` — Elixir's build tool

You can run all tests in the root directory with `make test` and you can
also run tests for a specific framework `make test_#{NAME}`, for example,
`make test_ex_unit`.

In case you are changing a single file, you can compile and run tests only
for that particular file for fast development cycles. For example, if you
are changing the String module, you can compile it and run its tests as:

    $ bin/elixirc lib/elixir/lib/string.ex -o lib/elixir/ebin
    $ bin/elixir lib/elixir/test/elixir/string_test.exs

After your changes are done, please remember to run the full suite with
`make test`.

From time to time, your tests may fail in an existing Elixir checkout and
may require a clean start by running `make clean compile`. You can always
check [the official build status on Travis-CI](https://travis-ci.org/elixir-lang/elixir).

With tests running and passing, you are ready to contribute to Elixir and
send your pull requests.

## Contributing Documentation

Code documentation (`@doc`, `@moduledoc`, `@typedoc`) has a special convention:
the first paragraph is considered to be a short summary.

For functions, macros and callbacks say what it will do. For example write
something like:

```elixir
@doc """
Returns only those elements for which `fun` is `true`.

...
"""
def filter(collection, fun) ...
```

For modules, protocols and types say what it is. For example write
something like:

```elixir
defmodule File.Stat do
  @moduledoc """
  Information about a file.

  ...
  """

  defstruct [...]
end
```

Keep in mind that the first paragraph might show up in a summary somewhere, long
texts in the first paragraph create very ugly summaries. As a rule of thumb
anything longer than 80 characters is too long.

Try to keep unnecessary details out of the first paragraph, it's only there to
give a user a quick idea of what the documented "thing" does/is. The rest of the
documentation string can contain the details, for example when a value and when
`nil` is returned.

If possible include examples, preferably in a form that works with doctests. For
example:

```elixir
@doc """
Return only those elements for which `fun` is `true`.

## Examples

    iex> Enum.filter([1, 2, 3], fn(x) -> rem(x, 2) == 0 end)
    [2]

"""
def filter(collection, fun) ...
```

This makes it easy to test the examples so that they don't go stale and examples
are often a great help in explaining what a function does.

## Pull requests

Good pull requests - patches, improvements, new features - are a fantastic
help. They should remain focused in scope and avoid containing unrelated
commits.

**IMPORTANT**: By submitting a patch, you agree that your work will be
licensed under the license used by the project.

If you have any large pull request in mind (e.g. implementing features,
refactoring code, etc), **please ask first** otherwise you risk spending
a lot of time working on something that the project's developers might
not want to merge into the project.

Please adhere to the coding conventions in the project (indentation,
accurate comments, etc.) and don't forget to add your own tests and
documentation. When working with git, we recommend the following process
in order to craft an excellent pull request:

1. [Fork](http://help.github.com/fork-a-repo/) the project, clone your fork,
   and configure the remotes:

   ```bash
   # Clone your fork of the repo into the current directory
   git clone https://github.com/<your-username>/elixir
   # Navigate to the newly cloned directory
   cd elixir
   # Assign the original repo to a remote called "upstream"
   git remote add upstream https://github.com/elixir-lang/elixir
   ```

2. If you cloned a while ago, get the latest changes from upstream:

   ```bash
   git checkout master
   git pull upstream master
   ```

3. Create a new topic branch (off of `master`) to contain your feature, change,
   or fix.

   **IMPORTANT**: Making changes in `master` is discouraged. You should always
   keep your local `master` in sync with upstream `master` and make your
   changes in topic branches.

   ```bash
   git checkout -b <topic-branch-name>
   ```

4. Commit your changes in logical chunks. Keep your commit messages organized,
   with a short description in the first line and more detailed information on
   the following lines. Feel free to use Git's
   [interactive rebase](https://help.github.com/articles/interactive-rebase)
   feature to tidy up your commits before making them public.

5. Make sure all the tests are still passing.

   ```bash
   make test
   ```

   This command will compile the code in your branch and use that
   version of Elixir to run the tests. This is needed to ensure your changes can
   pass all the tests.

6. Push your topic branch up to your fork:

   ```bash
   git push origin <topic-branch-name>
   ```

7. [Open a Pull Request](https://help.github.com/articles/using-pull-requests/)
    with a clear title and description.

8. If you haven't updated your pull request for a while, you should consider
   rebasing on master and resolving any conflicts.

   **IMPORTANT**: _Never ever_ merge upstream `master` into your branches. You
   should always `git rebase` on `master` to bring your changes up to date when
   necessary.

   ```bash
   git checkout master
   git pull upstream master
   git checkout <your-topic-branch>
   git rebase master
   ```

We have saved some excellent pull requests we have received in the past in case
you are looking for some examples:

* https://github.com/elixir-lang/elixir/pull/992
* https://github.com/elixir-lang/elixir/pull/1058
* https://github.com/elixir-lang/elixir/pull/1059

Thank you for your contributions!
