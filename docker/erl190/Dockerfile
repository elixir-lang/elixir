FROM erlang:19.0

# install basic dev tools
RUN apt-get update -qq && apt-get install -y \
      vim \
      make \
      curl

# Install and use UTF-8 locale
RUN DEBIAN_FRONTEND=noninteractive \
  apt-get update -qq && \
	apt-get install -y locales -qq && \
	locale-gen en_US.UTF-8 en_us && \
	dpkg-reconfigure locales && \
	dpkg-reconfigure locales && \
	locale-gen C.UTF-8 && \
	/usr/sbin/update-locale LANG=C.UTF-8

ENV LANG C.UTF-8
ENV LANGUAGE C.UTF-8
ENV LC_ALL C.UTF-8

WORKDIR /elixir
ENTRYPOINT ["bash"]
