# Docker file for the compilers course available at
# https://bitbucket.org/piyush-kurur/compilers
#
# Used also for CI on shippable.

FROM ubuntu:latest
MAINTAINER Piyush P Kurur <ppk@cse.iitk.ac.in>

# Versions of some haskell packages to pre-install

RUN sudo apt-get update -y
# RUN sudo apt-get install software-properties-common -y

# Generate the locales.

RUN sudo locale-gen en_US en_US.UTF-8
RUN sudo update-locale LC_ALL=en_US.UTF-8 LANG=en_US.UTF-8

# Update apt- packages.
RUN sudo apt-get update -y

# Install some basic packages that are required by the haskell
# environment.
RUN sudo apt-get install smlnj ml-yacc ml-lex ml-burg mlton
