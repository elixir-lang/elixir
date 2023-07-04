# What are anti-patterns?

Anti-patterns describe common mistakes or indicators of potential problems in code.
They are also known as "code smells".

The goal of these guides is to document known anti-patterns found in Elixir software
and teach developers how to identify and correct them. If an existing piece of code
matches an anti-pattern, it does not mean your code must be rewritten. However, you
should take its potential pitfalls and alternatives into consideration.

The anti-patterns in these guides are broken into 4 main categories:

  * **Code-related anti-patterns:** related to your code and particular
    language idioms and features;

  * **Design-related anti-patterns:** related to your modules, functions,
    and the role they play within a codebase;

  * **Process-related anti-patterns:** related to processes and process-based
    abstractions;

  * **Meta-programming anti-patterns:** related to meta-programming.

Each anti-pattern is documented using the following structure:

  * **Name:** Unique identifier of the anti-pattern. This name is important to facilitate
    communication between developers;

  * **Problem:** How the anti-pattern can harm code quality and what impacts this can have
    for developers;

  * **Example:** Code and textual descriptions to illustrate the occurrence of the anti-pattern;

  * **Refactoring:** Ways to change your code to improve its qualities. Examples of refactored
    code are presented to illustrate these changes.

The initial catalog of anti-patterns was proposed by Lucas Vegi and Marco Tulio Valente, from [ASERG/DCC/UFMG](http://aserg.labsoft.dcc.ufmg.br/). For more info, see [Understanding Code Smells in Elixir Functional Language](https://github.com/lucasvegi/Elixir-Code-Smells/blob/main/etc/2023-emse-code-smells-elixir.pdf)
and [the associated code repository](https://github.com/lucasvegi/Elixir-Code-Smells).
