defmodule Mix.Templates do

  @moduledoc """

  Provide a way to customize the templates used by "mix.new".

  When you build a new project, you can use the --template option to
  override the default, Mix-supplied template.

  If you use --template, you'll give it the name of a module that
  defines the template(s) to be used. For example, you can use

      mix new wonder_code --template lean_template

  to use the LeanTemplate module. Of course, before dong this, you'd you
  to install the new template. 

      mix archive.install http://github.com/pragdave/lean_template/lean_template.ez

  This is a one-off installation, and the template will be available to all
  your projects going forward.


  ### Creating a Template

  Templates are simply a module that look like this:

      defmodule MyTemplate do

        use Mix.Templates


        embed_template :readme, \"\"\"
        « text for the README.md template...
        \"\"\"

       embed_text :gitignore, \"\"\"
        « text for the README.md template...
       \"\"\"

      end

  By default, your template inherits each of the standard templates. You can look at
  Mix.Templates.Standard so see what these look like.

  You use `embed_template` for those templates that need to be run through EEx 
  because they use @assigned_variabled. You use `embed_text` for templates that 
  are simply copied.

  The available templates are:

  * `embed_template  :config, ...`

     config/config.exs for regular applications

  * `embed_template  :config_umbrella, ...`

     config/config_umbrella.exs for umbrella applications

  * `embed_text      :gitignore, ...`

     the top-level .gitignore file

  * `embed_template  :lib, ...`

     the file lib/«appname».ex without a top-level supervisor

  * `embed_template  :lib_app, ...`

     the file lib/«appname».ex when the --sup flag has been specified

  * `embed_template  :mixfile, ...`
        
     mix.exs

  * `embed_template  :mixfile_apps, ...`

     mix.exs for apps inside an umbrella project

  * `embed_template  :mixfile_umbrella, ...`

     mix.exs for a top-level umbrella project

  * `embed_template  :readme, ...`
        
     README.md

  * `embed_template  :test, ...`

     The test/«appname»_test.exs file

  * `embed_template  :test_helper, ...`

     And test/test_helper.exs

  Have a look a Mix.Templates.Standard for the available assigned variables.

  """
  

  @default Mix.Templates.Standard
  defmacro __using__(_opts) do
    quote do

      require Mix.Generator
      import Mix.Generator, only: [ embed_template: 2, embed_text: 2]

      
      def config(assigns),           do: unquote(@default).config(assigns)
      def config_umbrella(assigns),  do: unquote(@default).config_umbrella(assigns)
      def gitignore(),               do: unquote(@default).gitignore()
      def lib(assigns),              do: unquote(@default).lib(assigns)
      def lib_app(assigns),          do: unquote(@default).lib_app(assigns)
      def mixfile(assigns),          do: unquote(@default).mixfile(assigns)
      def mixfile_apps(assigns),     do: unquote(@default).mixfile_apps(assigns)
      def mixfile_umbrella(assigns), do: unquote(@default).mixfile_umbrella(assigns)
      def readme(assigns),           do: unquote(@default).readme(assigns)
      def test(assigns),             do: unquote(@default).test(assigns)
      def test_helper(assigns),      do: unquote(@default).test_helper(assigns)

      defoverridable [
        config:           1,
        config_umbrella:  1,
        gitignore:        0,
        
        lib:              1,
        lib_app:          1,
        mixfile:          1,
        
        mixfile_apps:     1,
        mixfile_umbrella: 1,
        readme:           1,
        
        test:             1,
        test_helper:      1,
      ]
      
    end

  end


end
