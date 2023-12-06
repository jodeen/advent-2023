# aoc2023
AOC for 2023

When running from the command line you can pass the option `-d/--day DAY` to run a specific day's solutions. If you do this, then you can also pass `-i/--input FILE` to specify an input file; by default, the program will look for it in `input/DayXX.txt`. You can also pass the argument `--all-days` and all days will be run in order, assuming the input files are in their default places.

Additionally, you can specify the level of detail to print out. By default, the program will print only the answers. If you'd like it to print timing information, use the `-t/--timings` option. Alternatively, if you'd like it to print the output of the parser and error messages in full, use the `-v/--verbose` option.

Example usage:
- `stack run -- -d 9`: Runs Day 9's solutions.
- `stack run -- --day 14 --input "wibble.txt"`: Runs Day 14's solutions, using the input file "wibble.txt".
- `stack run -- -d 1 -i "alex.txt" --timings`: Runs Day 1's solutions, using the input file "alex.txt". Also prints timing information for each solution.
- `stack run -- --all-days`: Runs the solutions from all days.

This can be used with `ghcid` to compile and run your code every time you save your files. Consider putting the following in your `.bashrc` (or equivalent):

```bash
function day { ghcid --run="Main.performDay (Options (OneDay $1 Nothing) Timings)" }
```

If sourced in a terminal, running the command `day 9`, for example, will, open a `ghcid` session and run your code every time you save, displaying the answers as if you ran the first example command above.

If you think the structure of the `Day` files needs changing to better suit your needs (before starting the project), then make the appropriate changes in `src/Days/Day01.hs` and run the `apply_changes.zsh` file. This will copy Day01 to all the other days, changing Day01 for DayXX as appropriate.