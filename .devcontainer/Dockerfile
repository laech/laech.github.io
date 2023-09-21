FROM mcr.microsoft.com/devcontainers/base:bookworm

RUN apt-get update \
 && apt-get install -y hugo \
 && apt-get autoremove -y \
 && apt-get clean -y \
 && rm -rf /var/lib/apt/lists/*
