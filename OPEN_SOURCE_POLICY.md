<!--
  SPDX-License-Identifier: Apache-2.0
  SPDX-FileCopyrightText: 2021 The Elixir Team
-->

# Open Source Policy

## 1. Introduction

This Open Source Policy outlines the licensing, contribution, and compliance
requirements for all code released under the Elixir project. By adhering to
these guidelines, we ensure that our community, maintainers, and contributors
uphold both legal and ethical standards while fostering a collaborative,
transparent environment.

This policy exists to support and protect the Elixir community. It aims to
balance openness, collaboration, and respect for all contributors’ rights,
ensuring that Elixir remains a trusted and innovative open source project.

## 2. Scope

This policy applies to the Elixir Programming language, located at
https://github.com/elixir-lang/elixir. It covers every file, and contribution
made, including documentation and any associated assets.

## 3. Licensing

All code released by the Elixir team is licensed under the
[Apache-2.0](./LICENSES/Apache-2.0.txt) license. Additionally, the following
licenses are recognized as permissible in this project:

- The Unicode license, as documented at
  [LicenseRef-scancode-unicode](./LICENSES/LicenseRef-scancode-unicode.txt)
- The Elixir Trademark Policy, as documented at
  [LicenseRef-elixir-trademark-policy](./LICENSES/LicenseRef-elixir-trademark-policy.txt)

These licenses are considered acceptable for any files or code that form part of
an Elixir repository. If a contribution requires a different license, it must
either be rejected or prompt an update to this policy.

## 4. Contributing to Elixir Projects

Any code contributed to Elixir repositories must fall under one of the accepted
licenses (Apache-2.0, Unicode, or Elixir Trademark). Contributions under any
other license will be rejected unless this policy is formally revised to include
that license. All files except those specifically exempted (e.g., certain test
fixture files) must contain SPDX license and copyright headers
(`SPDX-License-Identifier` and `SPDX-FileCopyrightText`). If a file qualifies
for an exception, this must be configured in the ORT (Open Source Review Toolkit)
configuration and undergo review.

Contributions must not introduce executable binary files into the codebase.

Every Elixir project within the organization will have an automated GitHub
Action to enforce these rules. This mechanism aids in detecting non-compliant
licenses or files early in the review process.

## 5. Preservation of Copyright and License Information

Any third-party code incorporated into Elixir projects must retain original
copyright and license headers. If no such headers exist in the source, they must
be added. This practice ensures that original authors receive proper credit and
that the licensing lineage is preserved.

## 6. Objectives

The Elixir project aims to promote a culture of responsible open source usage.
Specifically, our objectives include:

### 6.1 Clearly Define and Communicate Licensing & Compliance Policies

We will identify and document all third-party dependencies, ensure that license
information is communicated clearly, and maintain a project-wide license policy
or compliance handbook.

### 6.2 Implement Clear Processes for Reviewing Contributions

We will provide well-defined contribution guidelines. We implement the
Developer Certificate of Origin (DCO) for additional clarity regarding
contributor rights and obligations.

### 6.3 Track and Audit Third-Party Code Usage

All projects will implement a Software Bill of Materials (SBoM) strategy and
regularly verify license compliance for direct and transitive dependencies.

### 6.4 Monitor and Continuously Improve Open Source Compliance

We will conduct periodic internal audits, integrate compliance checks into
continuous integration (CI/CD) pipelines, and regularly review and refine these
objectives to align with best practices.

## 7. Roles and Responsibilities

### 7.1 Core Team Member

Core Team Members are responsible for being familiar with this policy and
ensuring it is consistently enforced. They must demonstrate sufficient
competencies to understand the policy requirements and must reject or request
changes to any pull requests that violate these standards.

### 7.2 Contributor

Contributors are expected to follow this policy when submitting code. If a
contributor submits a pull request that does not comply with the policy
(e.g., introduces a disallowed license), Core Team Members have the authority to
reject it or request changes. No special competencies are required for
contributors beyond awareness and adherence to the policy.

### 7.3 EEF CISO

The CISO designated by the Erlang Ecosystem Foundation (EEF) provides oversight
on queries and guidance regarding open source compliance or legal matters for
Elixir. The CISO is responsible for checking ongoing compliance with the policy,
escalating potential violations to the Core Team, and involving legal counsel if
necessary. This role does not require legal expertise but does involve
initiating legal or community discussions when needed.

## 8. Implications of Failing to Follow the Program Requirements

If a violation of this policy is identified, the Elixir Core Team will undertake
the following actions:

## 8.1 Review the Codebase for Additional Violations

We will investigate the codebase thoroughly to detect any similar instances of
non-compliance.

## 8.2 Review and Update the Process or Policy

In collaboration with the EEF CISO, the Elixir Core Team will assess the policy
and our internal workflows, making any necessary clarifications or amendments to
reduce the likelihood of recurrence.

## 8.3 Notify and Train Core Team Members

We will ensure that all active Core Team Members are informed about any policy
changes and understand how to apply them in everyday development.

## 8.4 Remove or Replace the Offending Code

If required, we will remove or replace the non-compliant code.

## 9. Contact

The project maintains a private mailing list at
[policy@elixir-lang.org](mailto:policy@elixir-lang.org) for handling licensing
and policy-related queries. Email is the preferred communication channel, and
the EEF CISO will be included on this list to provide assistance and ensure
timely responses. While solutions may take longer to implement, the project
commits to acknowledging all queries within five business days.

## 10. External Contributions of Core Team Members

When Core Team Members contribute to repositories outside Elixir, they do so in
a personal capacity or via their employer. They will not act as official
representatives of the Elixir team in those external contexts.

## 11. Policy Review and Amendments

This policy will be revisited annually to address new concerns, accommodate
changes in community standards, or adjust to emerging legal or technical
requirements. Proposed amendments must be reviewed by the Core Team and, if
necessary, by the EEF CISO. Any significant changes will be communicated to
contributors and made publicly available.

*Effective Date: 2025-02-20*  
*Last Reviewed: 2025-02-20*