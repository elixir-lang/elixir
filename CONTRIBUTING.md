# Contributing to Elixir

Please take a moment to review this document in order to make the contribution
process easy and effective for everyone involved!

## Using the issue tracker

The issue tracker is the preferred channel for [bug reports](#bugs-reports),
[features requests](#feature-requests) and [submitting pull
requests](#pull-requests), but please **do not** use the issue tracker for
personal support requests. Instead, use
[the mailing list](http://groups.google.com/group/elixir-lang-core),
[Stack Overflow](http://stackoverflow.com/questions/ask?tags=elixir), or
[#elixir-lang](irc://chat.freenode.net/elixir-lang) on Freenode).

We do our best to keep the issues tracker tidy and organized, making it useful
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
   latest `master` or development branch in the repository.

3. **Isolate the problem** &mdash; ideally create a reduced test
   case.

A good bug report shouldn't leave others needing to chase you up for more
information. Please try to be as detailed as possible in your report. What is
your environment? What steps will reproduce the issue? What version of Erlang
and Elixir experience the problem? What would you expect to be the outcome?
All these details will help people to fix any potential bugs.

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

Feature requests are welcome. But take a moment to find out whether your idea
fits with the scope and aims of the project. It's up to *you* to make a strong
case to convince the project's developers of the merits of this feature. Please
provide as much detail and context as possible.

## Contributing

We incentivate everyone to contribute to Elixir and help us tackle
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

With tests running and passing, you are ready to contribute to Elixir and
send your pull requests.

## Pull requests

Good pull requests - patches, improvements, new features - are a fantastic
help. They should remain focused in scope and avoid containing unrelated
commits.

**IMPORTANT**: By submitting a patch, you agree to allow the project owner to
license your work under the same license as that used by the project.

If you have any significant pull request in mind (e.g. implementing features,
refactoring code, porting to a different language), **please ask first**
otherwise you risk spending a lot of time working on something that the
project's developers might not want to merge into the project.

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
   git checkout <dev-branch>
   git pull upstream <dev-branch>
   ```

3. Create a new topic branch (off master) to contain your feature, change, or fix:

   ```bash
   git checkout -b <topic-branch-name>
   ```

4. Commit your changes in logical chunks. Keep your commit messages organized,
   with a short description in the first line and more detailed information on
   the following lines. Feel free to use Git's
   [interactive rebase](https://help.github.com/articles/interactive-rebase)
   feature to tidy up your commits before making them public.

5. Locally merge (or rebase) the upstream development branch into your topic branch:

   ```bash
   git pull [--rebase] upstream <dev-branch>
   ```

6. Push your topic branch up to your fork:

   ```bash
   git push origin <topic-branch-name>
   ```

7. [Open a Pull Request](https://help.github.com/articles/using-pull-requests/)
    with a clear title and description.

We have saved some excellent pull requests we have received in the past in case
you are looking for some examples:

* https://github.com/elixir-lang/elixir/pull/992
* https://github.com/elixir-lang/elixir/pull/1041
* https://github.com/elixir-lang/elixir/pull/1058
* https://github.com/elixir-lang/elixir/pull/1059
