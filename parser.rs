/*
Module: parser

Convert a list of tokens into a command_line.
*/

use std;
use tokenizer;

export parse;

enum output_sink {
    stdout,
    stderr,
    outfile(str),
}

enum input_source {
    stdin,
    infile(str),
}

type command = {args: [str],
                input: input_source,
                output: output_sink,
                error: output_sink };

enum command_line {
    pipeline([command_line]),
    sequence([command_line]),
    background(@command_line),
    and(@command_line, @command_line),
    or(@command_line, @command_line),
}
