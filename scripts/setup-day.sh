#!/usr/bin/env bash

# Sets $header
. scripts/cookie.sh

year=$1
day=$2
day00=$(printf %02d $day)
dir=input/$year/day$day00
mkdir -p $dir
hs_dir=y${year}d$day00
mkdir -p $hs_dir
hs_file=$hs_dir/Main.hs
url="https://adventofcode.com/$year/day/$day"
cat <<END > $hs_file
-- $url
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    let inputFile = head args
    content <- readFile inputFile
    print $ lines content
END
chmod +x $hs_file

cat <<END >> aoc2025.cabal

executable $hs_dir
    import: warnings, common-extensions
    main-is: $hs_file
END

# https://adventofcode.com/2024/day/3/input
curl "https://adventofcode.com/$year/day/$day/input" \
  --silent \
  -H "$header" \
  > $dir/input.txt

echo 'input:'
head $dir/input.txt

# https://adventofcode.com/2024/day/4
curl "https://adventofcode.com/$year/day/$day" \
  --silent \
  -H "$header" \
  > /tmp/advent.html

./scripts/extract-sample.ts /tmp/advent.html $dir

echo ''
echo "./$hs_file $dir/input.txt"
echo ''
echo "https://adventofcode.com/$year/day/$day"
echo ''
