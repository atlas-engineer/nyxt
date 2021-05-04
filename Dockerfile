FROM fedora:34

RUN dnf groupinstall -y "Development Tools"
RUN dnf install -y ruby ruby-devel rubygems rpm-build sbcl libffi-devel redhat-rpm-config git zlib-devel

# Copy repo content inside container:
COPY . .

COPY build-scripts/build-fedora-package.sh /usr/local/bin/
ENTRYPOINT ["build-fedora-package.sh"]
