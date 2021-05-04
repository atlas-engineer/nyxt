FROM fedora:34

RUN dnf groupinstall -y "Development Tools"
RUN dnf install -y ruby ruby-devel rubygems rpm-build sbcl libffi-devel redhat-rpm-config git zlib-devel

# COPY . /nyxt
# RUN cd /nyxt

COPY build-scripts/build-ubuntu-package.sh /usr/local/bin/
ENTRYPOINT ["build-ubuntu-package.sh"]
