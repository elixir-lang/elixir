<!--
  SPDX-License-Identifier: Apache-2.0
  SPDX-FileCopyrightText: 2021 The Elixir Team
-->

# Software Bill of Materials

A Software Bill of Materials (SBoM) is a structured inventory of the components that make up a software system. This guide explains what SBoMs are, why they matter, and how to generate them for Elixir projects.

## What is an SBoM?

An SBoM is a formal, machine-readable record of all components in a piece of software. Think of it as a detailed ingredient list for your application. A typical SBoM includes:

  * **Dependencies**: all libraries and packages your project uses
  * **Versions**: the exact version of each component
  * **Source locations**: where each component was obtained (Hex, GitHub, etc.)
  * **Checksums**: cryptographic hashes to verify integrity
  * **Licensing information**: the license under which each component is distributed

Two widely adopted standards exist for SBoM formats:

  * [CycloneDX](https://cyclonedx.org/): a lightweight standard designed for security contexts
  * [SPDX](https://spdx.dev/): a more comprehensive standard originally focused on licensing

Both formats are machine-readable (JSON, XML) and designed to be consumed by automated tooling.

> #### SBoM is an inventory, not a certification {: .info}
>
> An SBoM does not claim that software is secure, compliant, or free of vulnerabilities.
> It simply provides a detailed inventory that enables further analysis. Security and
> compliance assessments are performed by separate tools that consume the SBoM.

## Why generate SBoMs?

There are three main reasons to generate SBoMs for your projects.

### Vulnerability analysis

When a security vulnerability (CVE) is discovered in a library, you need to quickly determine if your projects are affected. SBoMs make this possible by providing a complete inventory of your dependencies.

Tools like [OWASP Dependency-Track](https://dependencytrack.org/) continuously monitor your SBoMs against vulnerability databases. When a new CVE is published, you get notified if any of your projects use the affected component.

Without an SBoM, answering "are we affected by CVE-2024-XXXXX?" means manually checking each project, which is time-consuming and error-prone.

### Regulatory requirements

SBoMs are required by some regulations and procurement policies:

  * **US Executive Order 14028** (2021) requires SBoMs for certain software supplied to the U.S. federal government, particularly for designated critical software
  * **EU Cyber Resilience Act** introduces requirements for component inventories (commonly fulfilled using SBoMs) for products with digital elements placed on the EU market
  * **Safety-critical industries** (medical devices, automotive, aerospace) often require detailed component inventories as part of certification

Customers and partners may also request SBoMs as part of their own compliance efforts.

### License compliance

Every dependency in your project comes with a license. An SBoM provides a starting point for license review by listing the declared license for each package. This helps you:

  * Get an overview of licenses in your dependency tree
  * Flag packages that may need closer review
  * Support due diligence for acquisitions, audits, or legal review

Note that package-level license information (as provided by mix_sbom) reflects what the package declares, not necessarily all licenses present in its source files. For thorough license compliance, file-level scanning tools like ORT provide deeper analysis.

## Generating SBoMs with mix_sbom

[mix_sbom](https://github.com/erlef/mix_sbom) is an EEF project that generates CycloneDX SBoMs for Elixir projects.

### Installation

There are several ways to install mix_sbom:

  * **As a project dependency**: add `sbom` to your `mix.exs`
  * **As a global escript**: run `mix escript.install hex sbom` (requires Elixir 1.19.4+)
  * **As a standalone binary**: download from the [releases page](https://github.com/erlef/mix_sbom/releases)

The standalone binary is useful for CI environments or when you don't want to modify your project's dependencies. It requires no local Elixir or Erlang installation.

### Basic usage

To generate an SBoM for your project using the standalone binary:

```console
$ mix_sbom cyclonedx /path/to/your/project
```

This creates a `bom.cdx.json` file in CycloneDX format containing your project's complete dependency tree.

### Common options

The most useful options are:

  * `-o, --output PATH`: specify the output file (default: `bom.cdx.json`)
  * `-t, --format FORMAT`: output format (`json`, `xml`, or `protobuf`)
  * `-s, --schema VERSION`: CycloneDX schema version

For example, to generate an XML format SBoM:

```console
$ mix_sbom cyclonedx --format xml --output sbom.xml /path/to/project
```

## CI integration

For automated SBoM generation, mix_sbom provides a GitHub Action:

```yaml
name: Generate SBoM
on:
  release:
    types: [published]

jobs:
  sbom:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: erlef/mix_sbom@v0
        with:
          path: "."
          format: "json"
      - uses: actions/upload-artifact@v4
        with:
          name: sbom
          path: bom.cdx.json
```

This workflow generates an SBoM whenever you publish a release and uploads it as a build artifact. See the [action's documentation](https://github.com/erlef/mix_sbom) for additional options.

## Deeper analysis with ORT

mix_sbom provides package-level license information based on what each dependency declares. However, some compliance workflows require file-level scanning. A package might declare an MIT license but contain individual files under different licenses, or include vendored code with its own licensing terms.

The [OSS Review Toolkit (ORT)](https://oss-review-toolkit.org/) addresses this by scanning actual source files for license headers and copyright notices. ORT supports Mix projects and provides:

  * **File-level license detection**: scans source code for license texts and SPDX identifiers
  * **Copyright holder identification**: extracts copyright notices from files
  * **Policy enforcement**: define rules for allowed and denied licenses
  * **Multi-ecosystem support**: analyze projects that span multiple package managers

For organizations with strict compliance requirements, ORT complements mix_sbom by providing the deeper analysis needed for thorough license audits.

See the [ORT Mix plugin documentation](https://oss-review-toolkit.org/ort/docs/plugins/package-managers/Mix) for details.

## Next steps

  * [CycloneDX Specification](https://cyclonedx.org/specification/overview/): learn more about the SBoM format
  * [OWASP Dependency-Track](https://dependencytrack.org/): continuous SBoM analysis platform
  * [mix_sbom documentation](https://hexdocs.pm/sbom): full documentation and advanced options
