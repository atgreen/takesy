# SBCL dumps a standalone executable core; stripping/mangling it corrupts the
# embedded image, so disable the debug/strip machinery.
%global debug_package %{nil}
%global __strip /bin/true
%global __brp_strip %{nil}
%global __brp_strip_static_archive %{nil}
%global __brp_strip_comment_note %{nil}

Name:           takesy
Version:        0.0.0
Release:        1%{?dist}
Summary:        A screen recorder for modern Linux desktops (Wayland & X11)

License:        MIT
URL:            https://github.com/atgreen/takesy
Source0:        %{name}-%{version}.tar.gz

BuildRequires:  sbcl
BuildRequires:  gcc
BuildRequires:  make
BuildRequires:  libfixposix-devel

# ffmpeg does the encoding (subprocess); libfixposix, PipeWire and EGL/GL are
# dlopen'd by the binary at runtime, so they can't be auto-detected -- list them.
# iolib dlopens the UNVERSIONED "libfixposix.so", which ships in -devel (the
# runtime libfixposix package only provides libfixposix.so.3), so require -devel.
Requires:       /usr/bin/ffmpeg
Requires:       libfixposix-devel
Requires:       libinput
Requires:       pipewire-libs
Requires:       mesa-libEGL
Requires:       mesa-libGL
Recommends:     xdg-desktop-portal
Recommends:     pipewire-utils

%description
takesy records your screen and turns it into a polished screencast: it
automatically zooms in on wherever you're working, smooths the cursor motion,
and composites the result -- padded background, rounded corners, drop shadow --
into a finished mp4 or GIF. Capture uses xdg-desktop-portal and PipeWire; the
compositor runs on the GPU via EGL/OpenGL.

%prep
%autosetup

%build
make

%install
install -D -m 0755 takesy %{buildroot}%{_bindir}/takesy
install -D -m 0644 OPEN-SOURCE-NOTICES.txt %{buildroot}%{_licensedir}/%{name}/OPEN-SOURCE-NOTICES.txt

%files
%license LICENSE
%license %{_licensedir}/%{name}/OPEN-SOURCE-NOTICES.txt
%doc README.md CHANGELOG.md
%{_bindir}/takesy

%changelog
* Sat Aug 29 2026 Anthony Green <green@moxielogic.com> - 1.0.0-1
- Initial package.
