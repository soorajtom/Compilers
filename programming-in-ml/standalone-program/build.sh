#!/bin/sh


echo ---------------  Building with SML/NJ -----------------------------
ml-build hello.cm Hello.main hello-image
sml @SMLload hello-image.* foo bar biz
echo ----------------- Done --------------------------------------------

echo
echo ---------------- Building with MLTON ------------------------------

mlton hello.mlb
./hello foo bar biz

echo ----------------- Done --------------------------------------------
