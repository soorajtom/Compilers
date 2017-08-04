# Docker file for the compilers course available at
# https://bitbucket.org/piyush-kurur/compilers
#
# Used also for CI on shippable.

FROM ubuntu:latest
MAINTAINER Piyush P Kurur <ppk@cse.iitk.ac.in>

RUN apt-get update -y
RUN apt-get install sudo locales -y

# Generate the locales.
RUN sudo locale-gen en_US en_US.UTF-8
RUN sudo update-locale LC_ALL=en_US.UTF-8 LANG=en_US.UTF-8

# Install some basic packages required by the course. Mostly standard
# ML compilers and compiler writing tools.

RUN sudo apt-get install smlnj ml-yacc ml-lex ml-burg mlton make -y
