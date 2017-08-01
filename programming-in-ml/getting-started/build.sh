#!/bin/bash

set -e
ml-build tutorial.cm Tutorial.main tutorial-image
sml @SMLload tutorial-image.* foo bar biz
