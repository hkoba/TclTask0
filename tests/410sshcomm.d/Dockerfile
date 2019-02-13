FROM rastasheep/ubuntu-sshd:latest
MAINTAINER Hiroaki Kobayashi "https://github.com/hkoba"

RUN apt-get update && \
    apt install -y tcl tcllib && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
    
RUN sed -i "s/#PasswordAuthentication.*/PasswordAuthentication no/" /etc/ssh/sshd_config

EXPOSE 22

CMD    ["/usr/sbin/sshd", "-D", "-e"]
