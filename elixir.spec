Name:           elixir
Version:        1.4.2
Release:        1%{?dist}
Summary:        A modern approach to programming for the Erlang VM

Group:          Development/Languages
License:        ASL 2.0
URL:            http://elixir-lang.org/

Source0:        https://github.com/elixir-lang/elixir/archive/v%{version}.tar.gz
BuildArch:      noarch
BuildRequires:  wxGTK, wxGTK-gl
BuildRequires:  git
BuildRequires:  esl-erlang >= 19
Requires:       wxGTK, wxGTK-gl
Requires:       esl-erlang >= 19

%description
Elixir is a programming language built on top of the Erlang VM.
As Erlang, it is a functional language built to support distributed,
fault-tolerant, non-stop applications with hot code swapping.

%prep
%setup -n elixir-%{version} -q
# update Makefile to install to /usr, rather than /usr/local
sed -i -e "s#/usr/local#/usr#g" Makefile

%build
export LANG="en_US.UTF-8"
make %{?_smp_mflags}

%check
export LANG="en_US.UTF-8"
make test

%install
%make_install

%files
%doc LICENSE CHANGELOG.md CODE_OF_CONDUCT.md NOTICE README.md RELEASE.md VERSION
/usr/bin/*
/usr/lib/elixir/
/usr/share/man/man1/*


%changelog
* Mon May 01 2017 Scott Merrill <smerrill@covermymeds.com> - 1.4.2-1
- New upstream release.
- sed Makefile to use /usr instead of /usr/local.
- Use %make_install macro.

* Wed Jun 29 2016 Nathan Chowning <nchowning@covermymeds.com> - 1.3.0-1
- New upstream release.

* Mon Apr 4 2016 Martin Langhoff <martin@laptop.org> - 1.2.4-1
- New upstream release.

* Wed Feb 24 2016 Martin Langhoff <martin@laptop.org> - 1.2.3-1
- New upstream release.

* Mon Feb 8 2016 Martin Langhoff <martin@laptop.org> - 1.2.2-1
- New upstream release.

* Wed Feb 03 2016 Fedora Release Engineering <releng@fedoraproject.org> - 1.1.1-2
- Rebuilt for https://fedoraproject.org/wiki/Fedora_24_Mass_Rebuild

* Thu Oct 29 2015 Ricky Elrod <relrod@redhat.com> - 1.1.1-1
- Latest upstream release.
- Re-enable test suite to see what breaks.

* Tue Jun 30 2015 Jochen Schmitt <Jochen herr-schmitt de> - 1.0.5-1
- New upstream release
- set a UTF-8 locale to build elixir
- Disable test suite

* Wed Jun 17 2015 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 1.0.3-2
- Rebuilt for https://fedoraproject.org/wiki/Fedora_23_Mass_Rebuild

* Fri Feb 13 2015 Jochen Schmitt <Jochen herr-schmitt de> - 1.0.3-1
- New upstream release

* Wed Oct 22 2014 Jochen Schmitt <Jochen herr-schmitt de> - 1.0.2-1
- New upstream release

* Thu Oct  9 2014 Jochen Schmitt <Jochen herr-schmitt de> - 1.0.1-2
- Fix wrong Erlang release specification in the BRs

* Wed Oct 8 2014 Ricky Elrod <relrod@redhat.com> - 1.0.1-1
- Update to upstream 1.0.1.

* Sat Oct  4 2014 Jochen Schmitt <Jochen herr-schmitt de> - 1.0-1
- New upstream release

* Sat Jun 07 2014 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 0.12.5-2
- Rebuilt for https://fedoraproject.org/wiki/Fedora_21_Mass_Rebuild

* Wed Mar 12 2014 Ricky Elrod <codeblock@fedoraproject.org> - 0.12.5-1
- Update to upstream 0.12.5.

* Thu Feb 13 2014 Ricky Elrod <codeblock@fedoraproject.org> - 0.12.4-1
- Update to upstream 0.12.4.

* Tue Feb 4 2014 Ricky Elrod <codeblock@fedoraproject.org> - 0.12.3-1
- Update to upstream 0.12.3.

* Sun Jan 19 2014 Ricky Elrod <codeblock@fedoraproject.org> - 0.12.2-2
- Remove patch that is no longer needed.

* Fri Jan 17 2014 Patrick Uiterwijk <puiterwijk@redhat.com> - 0.12.2-1
- Update to upstream 0.12.2.

* Sat Jan 11 2014 Ricky Elrod <codeblock@fedoraproject.org> - 0.12.1-1
- Update to upstream 0.12.1.

* Sun Dec 15 2013 Ricky Elrod <codeblock@fedoraproject.org> - 0.12.0-1
- Update to upstream 0.12.0.

* Sun Nov 24 2013 Ricky Elrod <codeblock@fedoraproject.org> - 0.11.2-1
- Update to upstream 0.11.2.

* Sat Nov 2 2013 Ricky Elrod <codeblock@fedoraproject.org> - 0.11.0-1
- Update to upstream 0.11.0.

* Tue Oct 8 2013 Ricky Elrod <codeblock@fedoraproject.org> - 0.10.3-1
- Update to upstream 0.10.3.

* Wed Sep 4 2013 Ricky Elrod <codeblock@fedoraproject.org> - 0.10.2-1
- Update to upstream 0.10.2.

* Sun Aug 4 2013 Ricky Elrod <codeblock@fedoraproject.org> - 0.10.1-2
- Copy mix binary, too.

* Sat Aug 3 2013 Ricky Elrod <codeblock@fedoraproject.org> - 0.10.1-1
- Update to upstream 0.10.1.

* Sat Aug 03 2013 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 0.10.0-2
- Rebuilt for https://fedoraproject.org/wiki/Fedora_20_Mass_Rebuild

* Tue Jul 16 2013 Ricky Elrod <codeblock@fedoraproject.org> - 0.10.0-1
- Update to upstream 0.10.0.

* Wed Jun 12 2013 Ricky Elrod <codeblock@fedoraproject.org> - 0.9.3-2
- Fix patch, doctest.exs was renamed to doc_test.exs

* Wed Jun 12 2013 Ricky Elrod <codeblock@fedoraproject.org> - 0.9.3-1
- Update to upstream 0.9.3.

* Wed Jun 12 2013 Ricky Elrod <codeblock@fedoraproject.org> - 0.9.1-1
- Update to upstream 0.9.1.
- Clean up specfile.

* Sun Feb 17 2013 Ricky Elrod <codeblock@fedoraproject.org> - 0.8.1-1
- Update to upstream 0.8.1.

* Wed Feb 13 2013 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 0.7.2-2
- Rebuilt for https://fedoraproject.org/wiki/Fedora_19_Mass_Rebuild

* Mon Jan 7 2013 Ricky Elrod <codeblock@fedoraproject.org> - 0.7.2-1
- Update to upstream 0.7.2.

* Mon Oct 22 2012 Ricky Elrod <codeblock@fedoraproject.org> - 0.7.0-1.20121022git833e9e9
- Update to upstream 0.7.0.

* Wed Aug 1 2012 Ricky Elrod <codeblock@fedoraproject.org> - 0.6.0-1.20120801git109919c
- Update to upstream 0.6.0.

* Sat May 26 2012 Ricky Elrod <codeblock@fedoraproject.org> - 0.5.0-1.20120526git6052352
- Initial build.
