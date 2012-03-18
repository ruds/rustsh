/*
Module: parser

Convert a list of tokens into a command_line.
*/

#[link(name = "parser",
       vers = "0.1")];

use std;
use tokenizer;
import either::either;
import either::right;
import either::left;
import tokenizer::token;
import tokenizer::token_to_string;

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

enum parse_result {
    parsed(command_line),
    continuation_required,
    error(str),
}

/*
Function: parse

Parse a sequence of tokens into a command line.

Parameters:

tokens - A vector of tokens from a command line.

Returns:

A parse_result. If continuation_required is returned, the caller must
harvest another commandline from the user, and call parse again with
the new tokens concatenated to the present tokens.
*/
fn parse(tokens: [token]) -> parse_result {
    if vec::is_empty(tokens) {
        ret parsed(sequence([]));
    } else if vec::last(tokens) == tokenizer::continuation {
        ret continuation_required;
    }
    let idx = 0u;
    ret parse_tokens(tokens, 0u, idx);
}

fn make_command(tokens: [token]) -> either<command, str> {
    assert vec::is_not_empty(tokens);
    let args: [str] = [];
    let i = stdin;
    let o = stdout;
    let e = stderr;
    for t in tokens {
        alt t {
          tokenizer::string(s) { args += [s]; }
          tokenizer::redirect_output(s) { o = outfile(s); }
          tokenizer::redirect_error(s) { e = outfile(s); }
          tokenizer::redirect_error_to_output { e = o; }
          tokenizer::redirect_input(s) { i = infile(s); }
          _ { ret right("Unexpected token: " + token_to_string(t)); }
        }
    }
    ret left({args: args, input: i, output: o, error: e});
}

enum part_parse {
    cmd(command),
    subshell(command_line),
    sep(token),
}

fn finish_parse(parts: [part_parse]) -> parse_result {
    assert vec::is_not_empty(parts);
    fail("not implemented.");
}

fn parse_tokens(tokens: [token], level: uint, &idx: uint) -> parse_result {
    let parts: [part_parse] = [];
    let cur: [token] = [];

    #macro([#make_command[ts, ps],
            if vec::is_not_empty(ts) {
                alt make_command(ts) {
                  left(c) { ps += [cmd(c)]; ts = []; }
                  right(e) { ret error(e); }
                }
            }]);

    while (idx < vec::len(tokens)) {
        let t = tokens[idx];
        alt t {
          tokenizer::error(e) { ret error(e); }
          tokenizer::pipe
          | tokenizer::and
          | tokenizer::or
          | tokenizer::background
          | tokenizer::sequence {
            #make_command[cur, parts];
            parts += [sep(t)];
          }
          tokenizer::open_subshell {
            #make_command[cur, parts];
            idx += 1u;
            alt parse_tokens(tokens, level + 1u, idx) {
              parsed(cl) { parts += [subshell(cl)]; }
              error(e) { ret error(e); }
              continuation_required { fail("Inconceivable!"); }
            }
          }
          tokenizer::close_subshell {
            if level == 0u {
                ret error("Unexpected ')'.");
            }
            #make_command[cur, parts];
            ret finish_parse(parts);
          }
          tokenizer::continuation {  /* ignore me! */ }
          _ { cur += [t]; }
        }
        idx += 1u;
    }
    if level > 0u {
        ret error("Expected ')'");
    }
    #make_command[cur, parts];
    ret finish_parse(parts);
}

#[test]
fn test_make_command() {
    assert make_command([tokenizer::string("foo"),
                         tokenizer::string("bar"),
                         tokenizer::redirect_output("baz"),
                         tokenizer::redirect_error_to_output])
        == left({args: ["foo", "bar"],
                 input: stdin,
                 output: outfile("baz"),
                 error: outfile("baz")});
    assert make_command([tokenizer::string("foo"),
                         tokenizer::string("bar"),
                         tokenizer::redirect_error_to_output,
                         tokenizer::redirect_output("baz")])
        == left({args: ["foo", "bar"],
                 input: stdin,
                 output: outfile("baz"),
                 error: stdout});
    assert make_command([tokenizer::string("foo"),
                         tokenizer::string("bar"),
                         tokenizer::redirect_input("hootenanny"),
                         tokenizer::redirect_output("baz")])
        == left({args: ["foo", "bar"],
                 input: infile("hootenanny"),
                 output: outfile("baz"),
                 error: stderr});
    alt make_command([tokenizer::string("foo"),
                      tokenizer::string("bar"),
                      tokenizer::background]) {
      left(_) { assert false; }
      right(_) { assert true; }
    }
}
