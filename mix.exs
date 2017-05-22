defmodule Proper.Mixfile do
  use Mix.Project

  def project do
    [app: :proper,
     version: "1.2.0",
     description: description(),
     package: package(),
     deps: deps()]
  end

  defp deps do
    [
      {:ex_doc, "~> 0.14", only: :dev},
    ]
  end

  defp description do
    """
    QuickCheck-inspired property-based testing tool for Erlang.
    """
  end

  defp package do
     [files: ~w(src include rebar.config configure Makefile COPYING README.md THANKS check_escripts.sh clean_doc.sh clean_temp.sh write_compile_flags mix.exs),
     maintainers: ["Manolis Papadakis", "Eirini Arvaniti", "Kostis Sagonas"],
     licenses: ["GPL"],
     links: %{"GitHub" => "https://github.com/manopapad/proper"}]
   end
end
