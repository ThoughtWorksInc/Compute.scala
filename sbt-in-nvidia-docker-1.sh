#!/bin/bash
nvidia-docker run --tty --interactive --init \
  --volume "$HOME/.ivy2/cache:/root/.ivy2/cache" \
  --volume "$HOME/.sbt/boot:/root/.sbt/boot" \
  --volume "$PWD:/mnt/project-root" \
  --workdir /mnt/project-root \
  popatry/scala-cuda:sbt-openjdk8-cuda9.0-cudnn7-runtime-ubuntu16.04 \
  sbt
